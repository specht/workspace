#!/usr/bin/env ruby
# frozen_string_literal: true

require 'date'
require 'fileutils'
require 'json'
require 'net/http'
require 'rexml/document'
require 'rexml/xpath'
require 'optparse'
require 'set'
require 'time'
require 'uri'
require 'zlib'

class DiscogsDatasetBuilder
  SOURCE_ROOT = 'https://data.discogs.com/'
  DATASET_TYPES = %w[artists masters releases].freeze
  HTTP_RETRY_LIMIT = 8
  HTTP_RETRY_BASE_DELAY = 60
  HTTP_RETRY_MAX_DELAY = 300
  EXCLUDED_ALBUM_DESCRIPTIONS = [
    'Compilation',
    'Unofficial Release',
    'Reissue',
    'Remastered',
    'Tour Recording'
  ].freeze

  def initialize(output_dir:, min_artist_versions: 100, min_album_versions: 10, dump_date: nil, force_download: false)
    @output_dir = File.expand_path(output_dir)
    @download_dir = File.join(@output_dir, '.downloads')
    @min_artist_versions = min_artist_versions
    @min_album_versions = min_album_versions
    @requested_dump_date = dump_date
    @force_download = force_download
  end

  def run
    raise ArgumentError, '--min-artist-versions must be at least 1' if @min_artist_versions < 1
    raise ArgumentError, '--min-album-versions must be at least 1' if @min_album_versions < 1

    FileUtils.mkdir_p(@download_dir)
    @dump_date = resolve_dump_date

    STDERR.puts "Using Discogs dump #{@dump_date}."
    STDERR.puts "Downloading Discogs source data to #{@download_dir}..."
    DATASET_TYPES.each { |type| download(type) }

    canonical_release_path = nil
    begin
      main_release_ids = load_main_release_ids
      version_counts, canonical_release_path = scan_releases(main_release_ids)
      main_release_ids = nil
      masters = load_candidate_album_masters(version_counts)
      albums = load_main_releases(masters, canonical_release_path)
    ensure
      FileUtils.rm_f(canonical_release_path) if canonical_release_path
    end

    selected_artist_ids = select_popular_artists!(albums)
    artists, memberships = load_artists(selected_artist_ids)
    if selected_artist_ids.any? && selected_artist_ids.none? { |artist_id| artists.key?(artist_id) }
      raise "Could not resolve any of #{selected_artist_ids.size} selected artist IDs from the Discogs artist dump"
    end

    albums.delete_if { |_id, album| !artists.key?(album[:artist_id]) }
    genres = normalize_genres(albums)
    tracks = flatten_tracks(albums)

    write_artists(artists)
    write_albums(albums)
    write_tracks(tracks)
    write_genres(genres)
    write_mysql(albums, artists, memberships, genres, tracks)
    write_neo4j(albums, artists, memberships, genres, tracks)
    write_readme(albums.size, artists.size, tracks.size, genres.size, memberships.size)

    STDERR.puts
    STDERR.puts 'Done. Wrote:'
    %w[artists.txt albums.txt tracks.txt genres.txt mysql.sql neo4j.dump README.md].each do |name|
      STDERR.puts "  #{File.join(@output_dir, name)}"
    end
  end

  private

  def resolve_dump_date
    return validate_dump_date(@requested_dump_date) if @requested_dump_date

    STDERR.puts 'Finding the newest complete monthly Discogs dump...'

    dates = available_dump_dates
    return dates.max unless dates.empty?

    raise 'Could not find a complete Discogs dump in the last 18 months; use --dump-date YYYYMMDD'
  end

  def available_dump_dates
    this_month = Date.new(Date.today.year, Date.today.month, 1)
    cutoff = this_month << 17
    types_by_date = Hash.new { |hash, key| hash[key] = Set.new }

    (cutoff.year..this_month.year).each do |year|
      html = request_text(index_uri(year))
      html.scan(/discogs_(\d{8})_(artists|masters|releases)\.xml\.gz/) do |dump_date, type|
        types_by_date[dump_date] << type
      end
    end

    types_by_date.filter_map do |dump_date, types|
      next unless DATASET_TYPES.all? { |type| types.include?(type) }

      begin
        date = Date.strptime(dump_date, '%Y%m%d')
      rescue Date::Error
        next
      end

      dump_date if date >= cutoff && date <= Date.today
    end
  end

  def index_uri(year)
    uri = URI(SOURCE_ROOT)
    uri.query = URI.encode_www_form(prefix: "data/#{year}/")
    uri
  end

  def request_text(uri, redirects_left = 5, retry_attempt = 0)
    raise "Too many redirects while requesting #{uri}" if redirects_left.zero?

    request = Net::HTTP::Get.new(uri)
    request['User-Agent'] = 'workspace-discogs-dataset-builder/1.0'

    response = Net::HTTP.start(
      uri.host,
      uri.port,
      use_ssl: uri.scheme == 'https',
      open_timeout: 30,
      read_timeout: 60
    ) { |http| http.request(request) }

    if response.code == '429'
      retry_rate_limited(response, uri, retry_attempt)
      return request_text(uri, redirects_left, retry_attempt + 1)
    end

    case response
    when Net::HTTPSuccess
      response.body
    when Net::HTTPRedirection
      location = response['location']
      raise "Redirect without Location while requesting #{uri}" unless location

      request_text(URI.join(uri, location), redirects_left - 1, retry_attempt)
    else
      raise "Request failed for #{uri}: #{response.code} #{response.message}"
    end
  end

  def retry_rate_limited(response, uri, retry_attempt)
    if retry_attempt >= HTTP_RETRY_LIMIT
      raise "Discogs still returns 429 Too Many Requests for #{uri} after #{HTTP_RETRY_LIMIT} retries"
    end

    delay = retry_delay(response, retry_attempt)
    STDERR.puts "  Discogs rate limit for #{uri}; retrying in #{delay} seconds..."
    sleep delay
  end

  def retry_delay(response, retry_attempt)
    retry_after = response['retry-after']
    if retry_after
      if retry_after.match?(/\A\d+\z/)
        return [retry_after.to_i, 1].max
      end

      begin
        seconds = (Time.httpdate(retry_after) - Time.now).ceil
        return [seconds, 1].max if seconds.positive?
      rescue ArgumentError
        # Fall back to exponential backoff below.
      end
    end

    [HTTP_RETRY_BASE_DELAY * (2**retry_attempt), HTTP_RETRY_MAX_DELAY].min
  end

  def validate_dump_date(value)
    unless value.match?(/\A\d{8}\z/)
      raise ArgumentError, '--dump-date must be in YYYYMMDD form, for example 20260801'
    end

    Date.strptime(value, '%Y%m%d')
    value
  rescue Date::Error
    raise ArgumentError, '--dump-date is not a valid date'
  end

  def dataset_name(type, dump_date = @dump_date)
    "discogs_#{dump_date}_#{type}.xml.gz"
  end

  def source_uri(type, dump_date = @dump_date)
    year = dump_date[0, 4]
    uri = URI(SOURCE_ROOT)
    uri.query = URI.encode_www_form(
      download: "data/#{year}/#{dataset_name(type, dump_date)}"
    )
    uri
  end

  def dataset_path(type)
    File.join(@download_dir, dataset_name(type))
  end

  def gzip_file?(path)
    File.open(path, 'rb') { |file| file.read(2) == "\x1f\x8b".b }
  rescue Errno::ENOENT
    false
  end

  def download(type)
    target = dataset_path(type)
    if File.exist?(target) && File.size?(target) && !@force_download
      if gzip_file?(target)
        STDERR.puts "  using cached #{File.basename(target)}"
        return
      end

      STDERR.puts "  discarding invalid cached #{File.basename(target)} (not gzip data)"
      FileUtils.rm_f(target)
    end

    uri = source_uri(type)
    tmp = "#{target}.part"
    FileUtils.rm_f(tmp) if @force_download

    if File.exist?(tmp) && File.size?(tmp) && !gzip_file?(tmp)
      STDERR.puts "  discarding invalid partial #{File.basename(tmp)} (not gzip data)"
      FileUtils.rm_f(tmp)
    end

    offset = File.exist?(tmp) ? File.size(tmp) : 0
    if offset.positive?
      STDERR.puts "  resuming #{uri} at byte #{offset}"
    else
      STDERR.puts "  downloading #{uri}"
    end

    mode = offset.positive? ? 'r+b' : 'wb'
    File.open(tmp, mode) do |file|
      file.seek(0, IO::SEEK_END)
      request_to_file(uri, file, offset: offset)
    end

    unless gzip_file?(tmp)
      raise "Downloaded #{uri} is not gzip data; refusing to cache it as #{File.basename(target)}"
    end

    File.rename(tmp, target)
  rescue StandardError
    if tmp && File.exist?(tmp) && File.size?(tmp) && gzip_file?(tmp)
      STDERR.puts "  keeping partial download #{tmp} for the next run"
    else
      FileUtils.rm_f(tmp) if tmp
    end
    raise
  end

  def request_to_file(uri, file, offset: 0, redirects_left: 5, retry_attempt: 0)
    raise "Too many redirects while downloading #{uri}" if redirects_left.zero?

    request = Net::HTTP::Get.new(uri)
    request['User-Agent'] = 'workspace-discogs-dataset-builder/1.0'
    request['Range'] = "bytes=#{offset}-" if offset.positive?

    Net::HTTP.start(
      uri.host,
      uri.port,
      use_ssl: uri.scheme == 'https',
      open_timeout: 30,
      read_timeout: 180
    ) do |http|
      http.request(request) do |response|
        if response.code == '429'
          retry_rate_limited(response, uri, retry_attempt)
          return request_to_file(
            uri,
            file,
            offset: offset,
            redirects_left: redirects_left,
            retry_attempt: retry_attempt + 1
          )
        end

        case response
        when Net::HTTPSuccess
          if offset.positive? && response.is_a?(Net::HTTPPartialContent)
            content_range = response['content-range']
            unless content_range&.start_with?("bytes #{offset}-")
              raise "Server resumed #{uri} at an unexpected range: #{content_range.inspect}"
            end
          elsif offset.positive?
            STDERR.puts '  server did not honor the Range request; restarting this file'
            file.truncate(0)
            file.rewind
            offset = 0
          end

          response.read_body { |chunk| file.write(chunk) }
        when Net::HTTPRedirection
          location = response['location']
          raise "Redirect without Location while downloading #{uri}" unless location

          request_to_file(
            URI.join(uri, location),
            file,
            offset: offset,
            redirects_left: redirects_left - 1,
            retry_attempt: retry_attempt
          )
        else
          raise "Download failed for #{uri}: #{response.code} #{response.message}"
        end
      end
    end
  end

  def load_main_release_ids
    STDERR.puts 'Indexing canonical release IDs from Discogs masters...'
    bitset = ''.b
    count = 0
    main_release_re = /<main_release>(\d+)<\/main_release>/

    Zlib::GzipReader.open(dataset_path('masters')) do |gz|
      gz.each_line do |line|
        match = main_release_re.match(line)
        next unless match

        bitset_add(bitset, match[1].to_i)
        count += 1
      end
    end

    STDERR.puts "  #{count} canonical release IDs indexed in #{format('%.1f', bitset.bytesize / 1_048_576.0)} MiB"
    bitset
  end

  def bitset_add(bitset, id)
    byte_index = id >> 3
    if byte_index >= bitset.bytesize
      bitset << "\0".b * (byte_index - bitset.bytesize + 1)
    end

    bitset.setbyte(byte_index, bitset.getbyte(byte_index) | (1 << (id & 7)))
  end

  def bitset_include?(bitset, id)
    byte_index = id >> 3
    return false if byte_index >= bitset.bytesize

    (bitset.getbyte(byte_index) & (1 << (id & 7))).positive?
  end

  def scan_releases(main_release_ids)
    STDERR.puts 'Scanning releases once for popularity and canonical release records...'
    counts = []
    spool_path = File.join(@download_dir, "discogs_#{@dump_date}_canonical-releases.tmp.gz")
    FileUtils.rm_f(spool_path)

    master_id_re = /<master_id\b[^>]*>(\d+)<\/master_id>/
    release_start_re = /<release\b[^>]*\bid="(\d+)"[^>]*>/
    end_marker = '</release>'
    collecting = false
    selected = false
    release_id = nil
    buffer = +''
    release_count = 0
    canonical_count = 0

    Zlib::GzipWriter.open(spool_path, Zlib::BEST_SPEED) do |spool|
      Zlib::GzipReader.open(dataset_path('releases')) do |gz|
        gz.each_line do |line|
          if (master_match = master_id_re.match(line))
            master_id = master_match[1].to_i
            counts[master_id] = counts[master_id].to_i + 1
          end

          unless collecting
            start_match = release_start_re.match(line)
            next unless start_match

            release_id = start_match[1].to_i
            selected = bitset_include?(main_release_ids, release_id)
            collecting = true
            buffer = line.dup if selected
            release_count += 1
            if (release_count % 1_000_000).zero?
              STDERR.puts "  #{release_count} releases scanned..."
            end
          else
            buffer << line if selected
          end

          next unless collecting && line.include?(end_marker)

          if selected
            write_spooled_release(spool, release_id, buffer)
            canonical_count += 1
          end

          collecting = false
          selected = false
          release_id = nil
          buffer = +''
        end
      end
    end

    STDERR.puts "  #{release_count} releases scanned, #{canonical_count} canonical releases spooled"
    STDERR.puts "  temporary canonical spool: #{format('%.1f', File.size(spool_path) / 1_048_576.0)} MiB"
    [counts, spool_path]
  rescue StandardError
    FileUtils.rm_f(spool_path) if spool_path
    raise
  end

  def write_spooled_release(file, release_id, xml)
    file.write("#{release_id}\t#{xml.bytesize}\n")
    file.write(xml)
  end

  def each_spooled_release(path)
    Zlib::GzipReader.open(path) do |gz|
      while (header = gz.gets)
        release_id_text, size_text = header.chomp.split("\t", 2)
        unless release_id_text&.match?(/\A\d+\z/) && size_text&.match?(/\A\d+\z/)
          raise "Invalid canonical release spool header: #{header.inspect}"
        end

        size = size_text.to_i
        xml = gz.read(size)
        unless xml && xml.bytesize == size
          raise "Truncated canonical release spool record for release #{release_id_text}"
        end

        yield release_id_text.to_i, xml
      end
    end
  end

  def load_candidate_album_masters(version_counts)
    STDERR.puts "Selecting candidate masters with at least #{@min_album_versions} release versions..."
    masters = {}

    each_selected_record(
      dataset_path('masters'),
      'master',
      ->(id, _line) { version_counts[id].to_i >= @min_album_versions }
    ) do |master_id, xml|
      doc = parse_xml(xml)
      master = doc.root
      next unless master

      artist_ids = REXML::XPath.match(master, './artists/artist/id').map { |node| node.text.to_i }.reject(&:zero?).uniq
      next unless artist_ids.size == 1

      main_release_id = integer_text(REXML::XPath.first(master, './main_release'))
      year = integer_text(REXML::XPath.first(master, './year'))
      title = text(REXML::XPath.first(master, './title'))
      next unless main_release_id && year && title

      genres = REXML::XPath.match(master, './genres/genre').map { |node| clean_text(node.text) }.reject(&:empty?).uniq
      next if genres.empty?

      masters[master_id] = {
        id: master_id,
        discogs_release_id: main_release_id,
        artist_id: artist_ids.first,
        title: title,
        year: year,
        genres: genres,
        versions: version_counts[master_id].to_i
      }
    end

    STDERR.puts "  #{masters.size} candidate single-artist masters before album filtering"
    masters
  end

  def load_main_releases(masters, spool_path)
    STDERR.puts 'Reading popular canonical releases and track lists...'
    master_by_release_id = {}
    masters.each_value { |master| master_by_release_id[master[:discogs_release_id]] = master }
    albums = {}

    each_spooled_release(spool_path) do |release_id, xml|
      master = master_by_release_id[release_id]
      next unless master

      doc = parse_xml(xml)
      release = doc.root
      next unless release
      next unless album_release?(release)

      tracks = parse_tracks(release)
      next if tracks.empty?

      country = text(REXML::XPath.first(release, './country'))
      albums[master[:id]] = master.merge(
        country: country,
        tracks: tracks
      )
    end

    STDERR.puts "  #{albums.size} canonical albums retained"
    albums
  end

  def select_popular_artists!(albums)
    artist_scores = Hash.new(0)
    albums.each_value do |album|
      artist_scores[album[:artist_id]] += album[:versions]
    end

    selected_artist_ids = artist_scores.filter_map do |artist_id, score|
      artist_id if score >= @min_artist_versions
    end.to_set

    before = albums.size
    albums.delete_if { |_id, album| !selected_artist_ids.include?(album[:artist_id]) }

    STDERR.puts "Selecting artists with at least #{@min_artist_versions} combined album release versions..."
    STDERR.puts "  #{selected_artist_ids.size} artists qualify; #{albums.size} of #{before} albums retained"

    selected_artist_ids
  end

  def album_release?(release)
    descriptions = REXML::XPath.match(release, './formats/format/descriptions/description').map { |node| clean_text(node.text) }
    return false unless descriptions.include?('Album')

    EXCLUDED_ALBUM_DESCRIPTIONS.none? { |description| descriptions.include?(description) }
  end

  def parse_tracks(release)
    raw_tracks = REXML::XPath.match(release, './tracklist/track').map do |track|
      {
        position: text(REXML::XPath.first(track, './position')) || '',
        title: text(REXML::XPath.first(track, './title')),
        duration: duration_to_seconds(text(REXML::XPath.first(track, './duration')))
      }
    end
    raw_tracks.select! { |track| track[:title] }

    has_positions = raw_tracks.any? { |track| !track[:position].empty? }
    raw_tracks.select! { |track| !track[:position].empty? } if has_positions

    raw_tracks.each_with_index.map do |track, index|
      {
        number: index + 1,
        title: track[:title],
        duration: track[:duration]
      }
    end
  end

  def duration_to_seconds(value)
    return nil unless value && value.match?(/\A\d{1,3}:\d{2}\z/)

    minutes, seconds = value.split(':').map(&:to_i)
    return nil if seconds >= 60

    minutes * 60 + seconds
  end

  def load_artists(wanted_artist_ids)
    STDERR.puts 'Reading artists and band memberships...'
    artists = {}
    memberships = Set.new

    each_selected_artist_record(dataset_path('artists'), wanted_artist_ids) do |artist_id, xml|
      doc = parse_xml(xml)
      artist = doc.root
      next unless artist

      details = artist_details(artist_id, artist)
      next unless details

      artists[artist_id] = details

      REXML::XPath.match(artist, './members/name').each do |member|
        member_id = member.attributes['id'].to_i
        member_name = clean_artist_name(member.text)
        next if member_id.zero? || !member_name

        artists[member_id] ||= { id: member_id, name: member_name }
        memberships << [member_id, artist_id]
      end
    end

    member_ids = memberships.map(&:first).to_set - wanted_artist_ids
    unless member_ids.empty?
      each_selected_artist_record(dataset_path('artists'), member_ids) do |artist_id, xml|
        artist = parse_xml(xml).root
        details = artist_details(artist_id, artist)
        artists[artist_id] = artists.fetch(artist_id, {}).merge(details) if details
      end
    end

    missing = wanted_artist_ids - artists.keys.to_set
    unless missing.empty?
      STDERR.puts "  warning: #{missing.size} selected artist records were not found in the artist dump"
    end

    [artists, memberships.to_a.sort]
  end

  def artist_details(artist_id, artist)
    return nil unless artist

    name = clean_artist_name(text(REXML::XPath.first(artist, './name')))
    return nil unless name

    profile = text(REXML::XPath.first(artist, './profile')) || ''
    born, died = life_years(profile)
    compact_hash(id: artist_id, name: name, born: born, died: died)
  end

  def life_years(profile)
    downcase = profile.downcase
    born = nearby_year(downcase, 'born')
    died = nearby_year(downcase, 'died')

    if born && died && died <= born
      born = nil
      died = nil
    end

    [born, died]
  end

  def nearby_year(text_value, word)
    index = text_value.index(word)
    return nil unless index

    snippet = text_value[index, 60]
    match = snippet.match(/(?:^|\D)(1[5-9]\d{2}|20\d{2})(?:\D|$)/)
    match && match[1].to_i
  end

  def normalize_genres(albums)
    names = albums.values.flat_map { |album| album[:genres] }.uniq.sort_by { |name| [name.downcase, name] }
    genre_id_by_name = {}
    genres = names.each_with_index.map do |name, index|
      id = index + 1
      genre_id_by_name[name] = id
      { id: id, name: name }
    end

    albums.each_value do |album|
      album[:genres] = album[:genres].map { |name| genre_id_by_name.fetch(name) }.sort
    end

    genres
  end

  def flatten_tracks(albums)
    albums.values.sort_by { |album| album[:id] }.flat_map do |album|
      album[:tracks].map do |track|
        compact_hash(
          album_id: album[:id],
          number: track[:number],
          title: track[:title],
          duration: track[:duration]
        )
      end
    end
  end


  def each_selected_artist_record(path, wanted_artist_ids, &handler)
    start_re = /<artist(?:\s|>)/
    id_re = /<id>(\d+)<\/id>/
    end_marker = '</artist>'
    collecting = false
    selected = false
    artist_id = nil
    prefix = +''
    buffer = +''

    Zlib::GzipReader.open(path) do |gz|
      gz.each_line do |line|
        unless collecting
          next unless start_re.match?(line)

          collecting = true
          selected = false
          artist_id = nil
          prefix = line.dup
          buffer = +''
        else
          prefix << line if artist_id.nil?
          buffer << line if artist_id && selected
        end

        if artist_id.nil? && (match = id_re.match(prefix))
          artist_id = match[1].to_i
          selected = wanted_artist_ids.include?(artist_id)
          buffer = prefix.dup if selected
          prefix = +''
        end

        next unless collecting && line.include?(end_marker)

        handler.call(artist_id, buffer) if selected && artist_id
        collecting = false
        selected = false
        artist_id = nil
        prefix = +''
        buffer = +''
      end
    end
  end

  def each_selected_record(path, tag, selector, &handler)
    start_re = /<#{Regexp.escape(tag)}\b[^>]*\bid="(\d+)"[^>]*>/
    end_marker = "</#{tag}>"
    collecting = false
    selected = false
    id = nil
    buffer = +''

    Zlib::GzipReader.open(path) do |gz|
      gz.each_line do |line|
        unless collecting
          match = start_re.match(line)
          next unless match

          id = match[1].to_i
          selected = selector.call(id, line)
          collecting = true
          buffer = line.dup if selected
        else
          buffer << line if selected
        end

        next unless collecting && line.include?(end_marker)

        handler.call(id, buffer) if selected
        collecting = false
        selected = false
        id = nil
        buffer = +''
      end
    end
  end

  def parse_xml(xml)
    REXML::Document.new(xml)
  rescue REXML::ParseException => e
    raise "Could not parse Discogs XML record: #{e.message}"
  end

  def integer_text(node)
    value = text(node)
    return nil unless value && value.match?(/\A\d+\z/)

    number = value.to_i
    number.zero? ? nil : number
  end

  def text(node)
    return nil unless node

    value = clean_text(node.text)
    value.empty? ? nil : value
  end

  def clean_text(value)
    value.to_s.encode(Encoding::UTF_8, invalid: :replace, undef: :replace, replace: '').strip
  end

  def clean_artist_name(value)
    return nil unless value

    cleaned = clean_text(value).sub(/\s+\(\d+\)\z/, '')
    cleaned.empty? ? nil : cleaned
  end

  def compact_hash(**values)
    values.reject { |_key, value| value.nil? }
  end

  def write_artists(artists)
    path = File.join(@output_dir, 'artists.txt')
    File.open(path, 'w') do |file|
      artists.values.sort_by { |artist| artist[:id] }.each { |artist| file.puts(JSON.generate(artist)) }
    end
  end

  def write_albums(albums)
    path = File.join(@output_dir, 'albums.txt')
    File.open(path, 'w') do |file|
      albums.values.sort_by { |album| [album[:year], album[:title].downcase, album[:id]] }.each do |album|
        record = album.reject { |key, _value| key == :tracks }
        file.puts(JSON.generate(record))
      end
    end
  end

  def write_tracks(tracks)
    path = File.join(@output_dir, 'tracks.txt')
    File.open(path, 'w') do |file|
      tracks.each { |track| file.puts(JSON.generate(track)) }
    end
  end

  def write_genres(genres)
    path = File.join(@output_dir, 'genres.txt')
    File.open(path, 'w') do |file|
      genres.each { |genre| file.puts(JSON.generate(genre)) }
    end
  end

  def write_mysql(albums, artists, memberships, genres, tracks)
    path = File.join(@output_dir, 'mysql.sql')

    File.open(path, 'w') do |file|
      file.puts <<~SQL
        -- Generated from the Discogs monthly data dumps at #{SOURCE_ROOT}
        -- Dump date: #{@dump_date}
        -- Generated: #{Time.now.utc.iso8601}
        --
        -- Import into an existing MySQL database, for example:
        --   mysql -u USER -p DATABASE < mysql.sql

        SET NAMES utf8mb4;
        SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS;
        SET FOREIGN_KEY_CHECKS=0;

        DROP TABLE IF EXISTS `artist_part_of`;
        DROP TABLE IF EXISTS `album_genre`;
        DROP TABLE IF EXISTS `track`;
        DROP TABLE IF EXISTS `album`;
        DROP TABLE IF EXISTS `genre`;
        DROP TABLE IF EXISTS `artist`;

        CREATE TABLE `artist` (
          `id` INT NOT NULL,
          `name` TEXT NOT NULL,
          `born` INT NULL,
          `died` INT NULL,
          PRIMARY KEY (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

        CREATE TABLE `genre` (
          `id` INT NOT NULL,
          `name` VARCHAR(255) NOT NULL,
          PRIMARY KEY (`id`),
          UNIQUE KEY `uq_genre_name` (`name`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

        CREATE TABLE `album` (
          `id` INT NOT NULL,
          `discogs_release_id` INT NOT NULL,
          `artist_id` INT NOT NULL,
          `title` TEXT NOT NULL,
          `year` INT NOT NULL,
          `country` VARCHAR(255) NULL,
          `versions` INT NOT NULL,
          PRIMARY KEY (`id`),
          KEY `idx_album_artist` (`artist_id`),
          KEY `idx_album_year` (`year`),
          KEY `idx_album_versions` (`versions`),
          CONSTRAINT `fk_album_artist`
            FOREIGN KEY (`artist_id`) REFERENCES `artist` (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

        CREATE TABLE `track` (
          `album_id` INT NOT NULL,
          `number` INT NOT NULL,
          `title` TEXT NOT NULL,
          `duration` INT NULL,
          PRIMARY KEY (`album_id`, `number`),
          CONSTRAINT `fk_track_album`
            FOREIGN KEY (`album_id`) REFERENCES `album` (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

        CREATE TABLE `album_genre` (
          `album_id` INT NOT NULL,
          `genre_id` INT NOT NULL,
          PRIMARY KEY (`album_id`, `genre_id`),
          KEY `idx_album_genre_genre` (`genre_id`),
          CONSTRAINT `fk_album_genre_album`
            FOREIGN KEY (`album_id`) REFERENCES `album` (`id`),
          CONSTRAINT `fk_album_genre_genre`
            FOREIGN KEY (`genre_id`) REFERENCES `genre` (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

        CREATE TABLE `artist_part_of` (
          `member_id` INT NOT NULL,
          `band_id` INT NOT NULL,
          PRIMARY KEY (`member_id`, `band_id`),
          KEY `idx_artist_part_of_band` (`band_id`),
          CONSTRAINT `fk_artist_part_of_member`
            FOREIGN KEY (`member_id`) REFERENCES `artist` (`id`),
          CONSTRAINT `fk_artist_part_of_band`
            FOREIGN KEY (`band_id`) REFERENCES `artist` (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
      SQL

      write_mysql_rows(file, 'artist', %w[id name born died], artists.values.sort_by { |artist| artist[:id] }.map do |artist|
        [artist[:id], artist[:name], artist[:born], artist[:died]]
      end)

      write_mysql_rows(file, 'genre', %w[id name], genres.map { |genre| [genre[:id], genre[:name]] })

      write_mysql_rows(file, 'album', %w[id discogs_release_id artist_id title year country versions],
                       albums.values.sort_by { |album| album[:id] }.map do |album|
        [album[:id], album[:discogs_release_id], album[:artist_id], album[:title], album[:year], album[:country], album[:versions]]
      end)

      write_mysql_rows(file, 'track', %w[album_id number title duration], tracks.map do |track|
        [track[:album_id], track[:number], track[:title], track[:duration]]
      end)

      album_genres = albums.values.sort_by { |album| album[:id] }.flat_map do |album|
        album[:genres].map { |genre_id| [album[:id], genre_id] }
      end
      write_mysql_rows(file, 'album_genre', %w[album_id genre_id], album_genres)
      write_mysql_rows(file, 'artist_part_of', %w[member_id band_id], memberships)

      file.puts
      file.puts 'SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;'
    end
  end

  def write_mysql_rows(file, table, columns, rows, batch_size: 1_000)
    return if rows.empty?

    quoted_columns = columns.map { |column| "`#{column}`" }.join(', ')
    rows.each_slice(batch_size) do |batch|
      file.puts
      file.puts "INSERT INTO `#{table}` (#{quoted_columns}) VALUES"
      file.puts batch.map { |row| "  (#{row.map { |value| mysql_literal(value) }.join(', ')})" }.join(",\n")
      file.puts ';'
    end
  end

  def mysql_literal(value)
    case value
    when nil
      'NULL'
    when Integer
      value.to_s
    when String
      encoded = value.encode(Encoding::UTF_8)
      "_utf8mb4 0x#{encoded.unpack1('H*')}"
    else
      raise "Unsupported MySQL value: #{value.inspect}"
    end
  end

  def write_neo4j(albums, artists, memberships, genres, tracks)
    path = File.join(@output_dir, 'neo4j.dump')
    artist_node_ids = {}
    album_node_ids = {}
    genre_node_ids = {}
    track_node_ids = {}
    next_node_id = 0

    File.open(path, 'w') do |file|
      artists.values.sort_by { |artist| artist[:id] }.each do |artist|
        node_id = next_node_id
        next_node_id += 1
        artist_node_ids[artist[:id]] = node_id
        write_neo4j_node(file, node_id, ['Artist'], artist)
      end

      albums.values.sort_by { |album| album[:id] }.each do |album|
        node_id = next_node_id
        next_node_id += 1
        album_node_ids[album[:id]] = node_id
        properties = album.reject { |key, _value| %i[tracks genres artist_id].include?(key) }
        write_neo4j_node(file, node_id, ['Album'], properties)
      end

      genres.each do |genre|
        node_id = next_node_id
        next_node_id += 1
        genre_node_ids[genre[:id]] = node_id
        write_neo4j_node(file, node_id, ['Genre'], genre)
      end

      tracks.each do |track|
        node_id = next_node_id
        next_node_id += 1
        track_node_ids[[track[:album_id], track[:number]]] = node_id
        properties = track.merge(id: "#{track[:album_id]}:#{track[:number]}")
        write_neo4j_node(file, node_id, ['Track'], properties)
      end

      albums.values.sort_by { |album| album[:id] }.each do |album|
        album_node = album_node_ids.fetch(album[:id])
        artist_node = artist_node_ids[album[:artist_id]]
        write_neo4j_relationship(file, artist_node, album_node, 'RELEASED') if artist_node

        album[:genres].each do |genre_id|
          write_neo4j_relationship(file, album_node, genre_node_ids.fetch(genre_id), 'IN_GENRE')
        end
      end

      tracks.each do |track|
        write_neo4j_relationship(
          file,
          album_node_ids.fetch(track[:album_id]),
          track_node_ids.fetch([track[:album_id], track[:number]]),
          'HAS_TRACK',
          number: track[:number]
        )
      end

      memberships.each do |member_id, band_id|
        member_node = artist_node_ids[member_id]
        band_node = artist_node_ids[band_id]
        next unless member_node && band_node

        write_neo4j_relationship(file, member_node, band_node, 'MEMBER_OF')
      end
    end
  end

  def write_neo4j_node(file, id, labels, properties)
    file.puts "n #{JSON.generate(id: id, labels: labels, properties: properties)}"
  end

  def write_neo4j_relationship(file, from, to, type, properties = {})
    file.puts "r #{JSON.generate(from: from, to: to, type: type, properties: properties)}"
  end

  def write_readme(album_count, artist_count, track_count, genre_count, membership_count)
    path = File.join(@output_dir, 'README.md')
    generated_at = Time.now.utc.iso8601

    File.write(path, <<~README)
      # Discogs-derived music dataset

      This directory was generated from the monthly Discogs data dumps published at:

      #{SOURCE_ROOT}

      Discogs dump date: #{@dump_date}
      Generated: #{generated_at}

      Source files used:

      #{DATASET_TYPES.map { |type| "- #{source_uri(type)}" }.join("\n")}

      The generated data contains canonical, single-artist albums from artists whose
      qualifying albums have at least #{@min_artist_versions} Discogs release versions
      in aggregate. An individual album needs at least #{@min_album_versions} versions
      before it contributes to that artist score or is included. Compilations, unofficial
      releases, reissues, remasters and tour recordings are excluded.

      Discogs' public CC0 dump contains catalog metadata but not a directly comparable
      IMDb-style vote count. The `versions` field therefore acts as a reproducible
      offline popularity proxy. Artist selection uses the sum of `versions` across
      qualifying albums, so artists with a substantial regional catalogue can qualify
      even when no single album has an unusually large international release history.
      Change the cutoffs with `--min-artist-versions` and `--min-album-versions`.

      ## Generated files

      - `artists.txt` — #{artist_count} artists, one JSON object per line
      - `albums.txt` — #{album_count} albums, one JSON object per line
      - `tracks.txt` — #{track_count} tracks, one JSON object per line
      - `genres.txt` — #{genre_count} genres, one JSON object per line
      - `mysql.sql` — creates the relational schema and inserts the complete dataset
      - `neo4j.dump` — logical Neo4jBolt dump ready for `neo4j_bolt load`

      The dataset also contains #{membership_count} artist-to-band membership links.

      ## MySQL

      `mysql.sql` creates these tables:

      - `artist`
      - `album`
      - `track`
      - `genre`
      - `album_genre`
      - `artist_part_of`

      Import it into an existing database with, for example:

      ```sh
      mysql -u USER -p DATABASE < mysql.sql
      ```

      `album.id` is the Discogs master-release ID and `album.discogs_release_id` is the
      canonical concrete release used for country and track information. `album.versions`
      contains the release-version count used as the popularity proxy, so it is also
      useful for sorting and classroom queries.

      ## Neo4j / Neo4jBolt

      `neo4j.dump` uses Neo4jBolt's textual logical dump format. It contains:

      - `Artist` nodes
      - `Album` nodes
      - `Track` nodes
      - `Genre` nodes
      - `(Artist)-[:RELEASED]->(Album)` relationships
      - `(Album)-[:HAS_TRACK]->(Track)` relationships
      - `(Album)-[:IN_GENRE]->(Genre)` relationships
      - `(Artist)-[:MEMBER_OF]->(Artist)` relationships for known band members

      Load it into an empty Neo4j database with:

      ```sh
      neo4j_bolt load neo4j.dump
      ```

      Neo4jBolt uses dump-local integer node IDs in this file to reconnect
      relationships while loading; these are not Neo4j database-internal IDs.

      ## Rebuilding

      The builder automatically looks for the newest complete monthly Discogs dump:

      ```sh
      ./discogs_prepare.rb
      ```

      Useful options:

      ```sh
      ./discogs_prepare.rb --min-artist-versions 150
      ./discogs_prepare.rb --min-album-versions 15
      ./discogs_prepare.rb --min-versions 15
      ./discogs_prepare.rb --dump-date 20260801
      ./discogs_prepare.rb --force-download
      ./discogs_prepare.rb --output /path/to/discogs
      ```

      `--min-versions` remains as a compatibility alias for `--min-album-versions`.

      The original compressed Discogs files are cached in `.downloads/` so subsequent
      runs can reuse them. `--force-download` refreshes the selected dump.

      Discogs publishes the database dump under CC0. See the Discogs data page and API
      terms for the conditions that apply to source data and other Discogs content.
    README
  end
end

options = {
  output_dir: 'discogs',
  min_artist_versions: 100,
  min_album_versions: 10,
  dump_date: nil,
  force_download: false
}

parser = OptionParser.new do |opts|
  opts.banner = 'Usage: discogs_prepare.rb [options]'

  opts.on('-o', '--output DIR', 'Output directory (default: discogs)') do |dir|
    options[:output_dir] = dir
  end

  opts.on('--min-artist-versions N', Integer, 'Minimum combined album release versions per artist (default: 100)') do |n|
    options[:min_artist_versions] = n
  end

  opts.on('--min-album-versions N', Integer, 'Minimum release versions per included album (default: 10)') do |n|
    options[:min_album_versions] = n
  end

  opts.on('--min-versions N', Integer, 'Alias for --min-album-versions') do |n|
    options[:min_album_versions] = n
  end

  opts.on('--dump-date YYYYMMDD', 'Use a specific monthly Discogs dump instead of auto-detecting the newest') do |value|
    options[:dump_date] = value
  end

  opts.on('--force-download', 'Download the selected Discogs source files again') do
    options[:force_download] = true
  end
end

parser.parse!

DiscogsDatasetBuilder.new(**options).run
