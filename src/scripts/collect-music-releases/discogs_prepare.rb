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
  EXCLUDED_ALBUM_DESCRIPTIONS = [
    'Compilation',
    'Unofficial Release',
    'Reissue',
    'Remastered',
    'Tour Recording'
  ].freeze

  def initialize(output_dir:, min_versions: 50, dump_date: nil, force_download: false)
    @output_dir = File.expand_path(output_dir)
    @download_dir = File.join(@output_dir, '.downloads')
    @min_versions = min_versions
    @requested_dump_date = dump_date
    @force_download = force_download
  end

  def run
    raise ArgumentError, '--min-versions must be at least 1' if @min_versions < 1

    FileUtils.mkdir_p(@download_dir)
    @dump_date = resolve_dump_date

    STDERR.puts "Using Discogs dump #{@dump_date}."
    STDERR.puts "Downloading Discogs source data to #{@download_dir}..."
    DATASET_TYPES.each { |type| download(type) }

    version_counts = count_master_versions
    masters = load_popular_album_masters(version_counts)
    albums = load_main_releases(masters)
    artists, memberships = load_artists(albums.values.map { |album| album[:artist_id] }.to_set)
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

  def request_text(uri, redirects_left = 5)
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

    case response
    when Net::HTTPSuccess
      response.body
    when Net::HTTPRedirection
      location = response['location']
      raise "Redirect without Location while requesting #{uri}" unless location

      request_text(URI.join(uri, location), redirects_left - 1)
    else
      raise "Request failed for #{uri}: #{response.code} #{response.message}"
    end
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

    STDERR.puts "  downloading #{uri}"
    File.open(tmp, 'wb') do |file|
      request_to_file(uri, file)
    end

    unless gzip_file?(tmp)
      raise "Downloaded #{uri} is not gzip data; refusing to cache it as #{File.basename(target)}"
    end

    File.rename(tmp, target)
  rescue StandardError
    FileUtils.rm_f(tmp) if tmp
    raise
  end

  def request_to_file(uri, file, redirects_left = 5)
    raise "Too many redirects while downloading #{uri}" if redirects_left.zero?

    request = Net::HTTP::Get.new(uri)
    request['User-Agent'] = 'workspace-discogs-dataset-builder/1.0'

    Net::HTTP.start(
      uri.host,
      uri.port,
      use_ssl: uri.scheme == 'https',
      open_timeout: 30,
      read_timeout: 180
    ) do |http|
      http.request(request) do |response|
        case response
        when Net::HTTPSuccess
          response.read_body { |chunk| file.write(chunk) }
        when Net::HTTPRedirection
          location = response['location']
          raise "Redirect without Location while downloading #{uri}" unless location

          request_to_file(URI.join(uri, location), file, redirects_left - 1)
        else
          raise "Download failed for #{uri}: #{response.code} #{response.message}"
        end
      end
    end
  end

  def count_master_versions
    STDERR.puts 'Counting release versions per Discogs master...'
    counts = []
    master_id_re = /<master_id\b[^>]*>(\d+)<\/master_id>/

    Zlib::GzipReader.open(dataset_path('releases')) do |gz|
      gz.each_line do |line|
        match = master_id_re.match(line)
        next unless match

        master_id = match[1].to_i
        counts[master_id] = counts[master_id].to_i + 1
      end
    end

    counts
  end

  def load_popular_album_masters(version_counts)
    STDERR.puts "Selecting masters with at least #{@min_versions} release versions..."
    masters = {}

    each_selected_record(
      dataset_path('masters'),
      'master',
      ->(id, _line) { version_counts[id].to_i >= @min_versions }
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

    STDERR.puts "  #{masters.size} popular single-artist masters before album filtering"
    masters
  end

  def load_main_releases(masters)
    STDERR.puts 'Reading canonical releases and track lists...'
    master_by_release_id = {}
    masters.each_value { |master| master_by_release_id[master[:discogs_release_id]] = master }
    albums = {}

    each_selected_record(
      dataset_path('releases'),
      'release',
      ->(release_id, _line) { master_by_release_id.key?(release_id) }
    ) do |release_id, xml|
      master = master_by_release_id.fetch(release_id)
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
          prefix = line.dup
          next unless line.include?(end_marker)
        else
          if artist_id.nil?
            prefix << line
            match = id_re.match(line)
            if match
              artist_id = match[1].to_i
              selected = wanted_artist_ids.include?(artist_id)
              buffer = prefix.dup if selected
              prefix = +'' unless selected
            end
          elsif selected
            buffer << line
          end
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

      The generated data contains canonical, single-artist album master releases whose
      Discogs master has at least #{@min_versions} release versions. Compilations,
      unofficial releases, reissues, remasters and tour recordings are excluded.

      Discogs' public CC0 dump contains catalog metadata but not a directly comparable
      IMDb-style vote count. The `versions` field therefore acts as a reproducible
      offline popularity proxy: an album with many pressings, territories and editions
      has a higher value than one represented by only a few releases. Change the cutoff
      with `--min-versions` when rebuilding the dataset.

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
      ./discogs_prepare.rb --min-versions 100
      ./discogs_prepare.rb --dump-date 20260801
      ./discogs_prepare.rb --force-download
      ./discogs_prepare.rb --output /path/to/discogs
      ```

      The original compressed Discogs files are cached in `.downloads/` so subsequent
      runs can reuse them. `--force-download` refreshes the selected dump.

      Discogs publishes the database dump under CC0. See the Discogs data page and API
      terms for the conditions that apply to source data and other Discogs content.
    README
  end
end

options = {
  output_dir: 'discogs',
  min_versions: 50,
  dump_date: nil,
  force_download: false
}

parser = OptionParser.new do |opts|
  opts.banner = 'Usage: discogs_prepare.rb [options]'

  opts.on('-o', '--output DIR', 'Output directory (default: discogs)') do |dir|
    options[:output_dir] = dir
  end

  opts.on('--min-versions N', Integer, 'Minimum Discogs release versions per album master (default: 50)') do |n|
    options[:min_versions] = n
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
