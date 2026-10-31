#!/usr/bin/env ruby
# frozen_string_literal: true

# Builds the classroom movie dataset (the Filmdatenbank) from Wikidata.
#
# Which films: those with many IMDb votes, read from IMDb's public
# title.ratings and title.basics files. Only the IMDb IDs are used, to find
# the films in Wikidata (property P345); nothing from IMDb is published.
# Everything in the output (titles, years, runtimes, genres, people, their
# jobs, birth and death years) comes from Wikidata, which is CC0. The video
# shop of the last chapter (copies, customers, loans) is invented, in rental.rb.

require 'digest'
require 'fileutils'
require 'json'
require 'net/http'
require 'openssl'
require 'optparse'
require 'set'
require 'time'
require 'uri'
require 'zlib'

require_relative '../lib/classroom_dataset'
require_relative '../lib/wikidata_client'
require_relative 'rental'

# Which films to include: IMDb IDs of non-adult feature films with at least
# min_votes votes, most voted first. Reads IMDb's public files, keeps only IDs.
class ImdbSelection
  SOURCE_URL = 'https://datasets.imdbws.com/'
  FILES = %w[title.ratings.tsv.gz title.basics.tsv.gz].freeze

  def initialize(download_dir:, min_votes:, force_download: false)
    @download_dir = download_dir
    @min_votes = min_votes
    @force_download = force_download
  end

  def ids
    FileUtils.mkdir_p(@download_dir)
    FILES.each { |name| download(name) }
    votes = {}
    each_tsv('title.ratings.tsv.gz') do |row|
      n = row['numVotes'].to_i
      votes[row['tconst']] = n if n >= @min_votes
    end
    films = []
    each_tsv('title.basics.tsv.gz') do |row|
      next unless votes.key?(row['tconst']) && row['titleType'] == 'movie' && row['isAdult'] == '0'

      films << row['tconst']
    end
    films.sort_by { |id| [-votes[id], id] }
  end

  private

  def download(name)
    target = File.join(@download_dir, name)
    if File.size?(target) && !@force_download
      STDERR.puts "  using cached #{name}"
      return
    end
    STDERR.puts "  downloading #{SOURCE_URL}#{name}"
    uri = URI.join(SOURCE_URL, name)
    File.open("#{target}.part", 'wb') do |file|
      Net::HTTP.start(uri.host, uri.port, use_ssl: true, open_timeout: 30, read_timeout: 120) do |http|
        http.request(Net::HTTP::Get.new(uri)) do |response|
          raise "Download failed for #{uri}: #{response.code}" unless response.is_a?(Net::HTTPSuccess)

          response.read_body { |chunk| file.write(chunk) }
        end
      end
    end
    File.rename("#{target}.part", target)
  end

  def each_tsv(name)
    Zlib::GzipReader.open(File.join(@download_dir, name), external_encoding: Encoding::UTF_8) do |gz|
      headers = gz.readline.chomp.split("\t", -1)
      gz.each_line { |line| yield headers.zip(line.chomp.split("\t", -1)).to_h }
    end
  end
end

class WikidataMovieBuilder
  # Wikidata property -> job title in the dataset, and how many people per
  # film and job are kept (the best known ones, by number of Wikipedia articles).
  JOBS = {
    'P161' => ['actor', 10],
    'P57' => ['director', 5],
    'P58' => ['writer', 5],
    'P86' => ['composer', 3],
    'P162' => ['producer', 5],
    'P344' => ['cinematographer', 3],
    'P1040' => ['editor', 3]
  }.freeze

  # English genre labels in Wikidata are many and fine-grained ("neo-noir",
  # "superhero film"). They map to a small set of genre names, like the ones
  # people know from film sites. A label may give several genres.
  GENRES = [
    [/\baction\b|martial arts|superhero|disaster|swashbuckler/, 'Action'],
    [/adventure|swashbuckler/, 'Adventure'],
    [/animat|anime|stop-motion/, 'Animation'],
    [/biographical|biopic/, 'Biography'],
    [/comedy|satir|parody|farce/, 'Comedy'],
    [/crime|gangster|heist|noir|mafia|detective/, 'Crime'],
    [/documentary/, 'Documentary'],
    [/drama|melodrama|tragedy/, 'Drama'],
    [/family|children/, 'Family'],
    [/fantasy|fairy tale|sword and sorcery/, 'Fantasy'],
    [/historical|history|period|epic film/, 'History'],
    [/horror|slasher|zombie|vampire|splatter/, 'Horror'],
    [/\bmusical\b/, 'Musical'],
    [/\bmusic\b|concert|rockumentary/, 'Music'],
    [/mystery|whodunit|detective/, 'Mystery'],
    [/romance|romantic/, 'Romance'],
    [/science fiction|sci-fi|cyberpunk|space opera|dystopi|post-apocalyptic|time travel/, 'Sci-Fi'],
    [/\bsports?\b|boxing|baseball|football|basketball/, 'Sport'],
    [/thriller|suspense|\bspy\b|espionage|psychological/, 'Thriller'],
    [/\bwar\b|anti-war/, 'War'],
    [/western/, 'Western']
  ].freeze

  MINUTE = 'http://www.wikidata.org/entity/Q7727'
  HOUR = 'http://www.wikidata.org/entity/Q25235'
  SECOND = 'http://www.wikidata.org/entity/Q11574'

  def initialize(output_dir:, min_votes: 100_000, force_download: false, offline: false, pause: 1.0, selection: nil, wikidata: nil)
    @output_dir = File.expand_path(output_dir)
    @min_votes = min_votes
    @selection = selection || ImdbSelection.new(download_dir: File.join(@output_dir, '.downloads'), min_votes: min_votes,
                                                force_download: force_download)
    @wikidata = wikidata || WikidataClient.new(cache_dir: File.join(@output_dir, '.wikidata-cache'), pause: pause, offline: offline)
    @stats = Hash.new(0)
  end

  def run
    STDERR.puts "Choosing films (IMDb votes >= #{@min_votes})..."
    imdb_ids = @selection.ids
    STDERR.puts "  #{imdb_ids.size} films"

    STDERR.puts 'Reading films from Wikidata...'
    films = load_films(imdb_ids)
    STDERR.puts 'Reading genres...'
    genre_labels = load_genres(films)
    STDERR.puts 'Reading cast and crew...'
    credits = load_credits(films)
    STDERR.puts 'Reading people...'
    people = load_people(credits.values.flatten(1).map(&:last).uniq)

    movies, crew, genres = assemble(films, genre_labels, credits, people)
    write(movies, crew, genres)
  end

  # --- reading -----------------------------------------------------------------

  def self.qid(uri)
    uri.to_s.split('/').last
  end

  # "1999-03-31T00:00:00Z" -> 1999, "-0500-01-01T00:00:00Z" -> -500
  def self.year(value)
    m = value.to_s.match(/\A(-?)0*(\d+)-/)
    return nil unless m

    year = m[2].to_i
    m[1] == '-' ? -year : year
  end

  def self.minutes(amount, unit)
    value = amount.to_f
    case unit
    when MINUTE, nil then value
    when HOUR then value * 60
    when SECOND then value / 60
    end&.round
  end

  def self.genres_for(label)
    text = label.to_s.downcase
    GENRES.select { |pattern, _| text.match?(pattern) }.map(&:last).uniq
  end

  def literal_list(values)
    values.map { |v| "\"#{v.gsub(/[^a-z0-9]/i, '')}\"" }.join(' ')
  end

  def entity_list(qids)
    qids.map { |q| "wd:#{q.gsub(/[^Q0-9]/, '')}" }.join(' ')
  end

  # imdb id -> { qid:, en:, de:, original:, years: [], minutes: [] }, in the
  # order of imdb_ids (most voted first).
  def load_films(imdb_ids)
    films = {}
    imdb_ids.each_slice(200).with_index do |batch, i|
      STDERR.puts "  #{i * 200}/#{imdb_ids.size}" if (i % 5).zero?
      rows = select_split(batch) do |part|
        <<~SPARQL
          SELECT ?imdb ?film ?en ?de ?original ?date ?amount ?unit WHERE {
            VALUES ?imdb { #{literal_list(part)} }
            ?film wdt:P345 ?imdb .
            OPTIONAL { ?film rdfs:label ?en FILTER(LANG(?en) = "en") }
            OPTIONAL { ?film rdfs:label ?de FILTER(LANG(?de) = "de") }
            OPTIONAL { ?film wdt:P1476 ?original }
            OPTIONAL { ?film wdt:P577 ?date }
            OPTIONAL { ?film p:P2047 ?ds . ?ds a wikibase:BestRank ; psv:P2047 [ wikibase:quantityAmount ?amount ; wikibase:quantityUnit ?unit ] }
          }
        SPARQL
      end
      rows.each do |row|
        film = (films[row['imdb']] ||= { qid: self.class.qid(row['film']), years: [], minutes: [] })
        next if film[:qid] != self.class.qid(row['film']) # one IMDb ID on two items: keep the first

        film[:en] ||= row['en']
        film[:de] ||= row['de']
        film[:original] ||= row['original']
        film[:years] << self.class.year(row['date']) if row['date']
        film[:minutes] << self.class.minutes(row['amount'], row['unit']) if row['amount']
      end
    end
    @stats[:not_in_wikidata] = imdb_ids.count { |id| !films.key?(id) }
    imdb_ids.filter_map { |id| films[id] && [id, films[id]] }.to_h
  end

  # qid -> [English genre labels]
  def load_genres(films)
    labels = Hash.new { |h, k| h[k] = [] }
    films.values.map { |f| f[:qid] }.each_slice(200) do |batch|
      rows = select_split(batch) do |part|
        <<~SPARQL
          SELECT ?film ?label WHERE {
            VALUES ?film { #{entity_list(part)} }
            ?film wdt:P136 ?genre .
            ?genre rdfs:label ?label FILTER(LANG(?label) = "en")
          }
        SPARQL
      end
      rows.each { |row| labels[self.class.qid(row['film'])] << row['label'] }
    end
    labels
  end

  # Runs a query for a batch; if the query service gives up on it, the batch
  # is split in halves (down to one item) instead of failing the whole build.
  def select_split(batch, &query)
    @wikidata.select(query.call(batch))
  rescue WikidataClient::Failed => e
    raise if batch.size <= 1

    STDERR.puts "  #{e.message}; splitting a batch of #{batch.size}"
    half = batch.size / 2
    select_split(batch[0, half], &query) + select_split(batch[half..], &query)
  end

  # qid -> [[job, person qid]], the best known people per job first
  def load_credits(films)
    credits = Hash.new { |h, k| h[k] = [] }
    unions = JOBS.keys.map { |p| "{ ?film wdt:#{p} ?person BIND(\"#{p}\" AS ?prop) }" }.join(" UNION\n  ")
    films.values.map { |f| f[:qid] }.each_slice(20).with_index do |batch, i|
      STDERR.puts "  #{i * 20}/#{films.size}" if (i % 20).zero?
      # The optimizer hint keeps the order as written: films first, then their
      # people. Otherwise the service may start from all humans and time out.
      rows = select_split(batch) do |part|
        <<~SPARQL
          SELECT ?film ?prop ?person ?links WHERE {
            hint:Query hint:optimizer "None" .
            VALUES ?film { #{entity_list(part)} }
            #{unions}
            ?person wdt:P31 wd:Q5 .
            ?person wikibase:sitelinks ?links .
          }
        SPARQL
      end
      rows.group_by { |r| [self.class.qid(r['film']), r['prop']] }.each do |(film, prop), list|
        job, keep = JOBS.fetch(prop)
        people = list.map { |r| [self.class.qid(r['person']), r['links'].to_i] }.uniq.sort_by { |q, links| [-links, q] }
        @stats[:people_left_out] += [people.size - keep, 0].max
        people.first(keep).each { |qid, _| credits[film] << [job, qid] }
      end
    end
    credits
  end

  # qid -> { name:, birth_year:, death_year: }
  def load_people(qids)
    people = {}
    qids.each_slice(300).with_index do |batch, i|
      STDERR.puts "  #{i * 300}/#{qids.size}" if (i % 5).zero?
      rows = select_split(batch) do |part|
        <<~SPARQL
          SELECT ?person ?en ?de ?mul ?birth ?death WHERE {
            VALUES ?person { #{entity_list(part)} }
            OPTIONAL { ?person rdfs:label ?en FILTER(LANG(?en) = "en") }
            OPTIONAL { ?person rdfs:label ?de FILTER(LANG(?de) = "de") }
            OPTIONAL { ?person rdfs:label ?mul FILTER(LANG(?mul) = "mul") }
            OPTIONAL { ?person wdt:P569 ?birth }
            OPTIONAL { ?person wdt:P570 ?death }
          }
        SPARQL
      end
      rows.each do |row|
        person = (people[self.class.qid(row['person'])] ||= { births: [], deaths: [] })
        person[:name] ||= row['en'] || row['mul'] || row['de']
        person[:births] << self.class.year(row['birth']) if row['birth']
        person[:deaths] << self.class.year(row['death']) if row['death']
      end
    end
    people.transform_values do |p|
      birth = p[:births].compact.min
      death = p[:deaths].compact.min
      if birth && death && death < birth
        @stats[:impossible_life_dates] += 1
        death = nil
      end
      { name: p[:name], birth_year: birth, death_year: death }
    end
  end

  # --- building the tables -------------------------------------------------------

  def assemble(films, genre_labels, credits, people)
    kept = films.values.select do |f|
      ok = f[:en] && !f[:years].compact.empty? && !f[:minutes].compact.empty?
      @stats[:films_without_title_year_or_runtime] += 1 unless ok
      ok
    end
    rows = kept.map do |f|
      title = f[:en]
      original = f[:original] if f[:original] && f[:original] != title
      german = f[:de] if f[:de] && ![title, original].include?(f[:de])
      { qid: f[:qid], title: title, original_title: original, german_title: german,
        year: f[:years].compact.min, runtime: f[:minutes].compact.max,
        genre_names: genre_labels[f[:qid]].flat_map { |l| self.class.genres_for(l) }.uniq.sort }
    end
    rows.sort_by! { |m| [m[:year], m[:title].downcase, m[:qid][1..].to_i] }
    rows.each_with_index { |m, i| m[:id] = i + 1 }
    @stats[:films_without_genre] = rows.count { |m| m[:genre_names].empty? }

    genre_names = rows.flat_map { |m| m[:genre_names] }.uniq.sort
    genres = genre_names.each_with_index.map { |name, i| { id: i + 1, name: name } }
    genre_id = genres.to_h { |g| [g[:name], g[:id]] }

    crew_id = {}
    crew = []
    movies = rows.map do |m|
      by_job = Hash.new { |h, k| h[k] = [] }
      credits[m[:qid]].each do |job, person_qid|
        person = people[person_qid]
        next unless person && person[:name]

        crew_id[person_qid] ||= begin
          crew << { id: crew.size + 1, wikidata: person_qid, name: person[:name],
                    birth_year: person[:birth_year], death_year: person[:death_year] }
          crew.size
        end
        by_job[job] << crew_id[person_qid] unless by_job[job].include?(crew_id[person_qid])
      end
      { id: m[:id], wikidata: m[:qid], title: m[:title], original_title: m[:original_title],
        german_title: m[:german_title], year: m[:year], runtime: m[:runtime],
        genres: m[:genre_names].map { |n| genre_id.fetch(n) }, crew: by_job.to_h }
    end
    [movies, crew, genres]
  end

  def jobs_for(movies)
    order = JOBS.values.map(&:first)
    used = movies.flat_map { |m| m[:crew].keys }.uniq
    (order & used).each_with_index.to_h { |job, i| [job, i + 1] }
  end

  def write(movies, crew, genres)
    FileUtils.mkdir_p(@output_dir)
    write_jsonl('movies.txt', movies.map(&:compact))
    write_jsonl('crew.txt', crew.map(&:compact))
    write_jsonl('genres.txt', genres)
    mysql = mysql_writer(movies, crew, genres)
    mysql.write
    write_neo4j(movies, crew, genres)
    write_readme(movies, crew, genres)

    STDERR.puts
    STDERR.puts "Done. Wrote to #{@output_dir}:"
    (%w[movies.txt crew.txt genres.txt] + mysql.files + [ClassroomDataset.neo4j_file('movies'), 'README.md']).each do |name|
      STDERR.puts "  #{name}"
    end
    report(movies, crew, genres)
  end

  def write_jsonl(name, rows)
    File.open(File.join(@output_dir, name), 'w') { |file| rows.each { |row| file.puts(JSON.generate(row)) } }
  end

  def mysql_writer(movies, crew, genres)
    jobs = jobs_for(movies)
    rental = Rental.build(movies)
    movie_genres = movies.flat_map { |m| m[:genres].map { |g| [m[:id], g] } }
    movie_crew = movies.flat_map do |m|
      m[:crew].flat_map { |job, ids| ids.map { |c| [m[:id], c, jobs.fetch(job)] } }
    end
    tables = [
      ClassroomDataset::Table.new(name: 'genre', columns: %w[id name], primary_key: %w[id],
                                  rows: genres.map { |g| [g[:id], g[:name]] }),
      ClassroomDataset::Table.new(name: 'movie', columns: %w[id title original_title german_title year runtime],
                                  primary_key: %w[id],
                                  rows: movies.map { |m| [m[:id], m[:title], m[:original_title], m[:german_title], m[:year], m[:runtime]] }),
      ClassroomDataset::Table.new(name: 'movie_genre', columns: %w[movie_id genre_id], primary_key: %w[movie_id genre_id],
                                  rows: movie_genres),
      ClassroomDataset::Table.new(name: 'crew', columns: %w[id name birth_year death_year], primary_key: %w[id],
                                  rows: crew.map { |p| [p[:id], p[:name], p[:birth_year], p[:death_year]] }),
      ClassroomDataset::Table.new(name: 'job', columns: %w[id title], primary_key: %w[id],
                                  rows: jobs.map { |title, id| [id, title] }),
      ClassroomDataset::Table.new(name: 'movie_crew', columns: %w[movie_id crew_id job_id],
                                  primary_key: %w[movie_id crew_id job_id], rows: movie_crew),
      ClassroomDataset::Table.new(name: 'copy', columns: %w[id movie_id format], primary_key: %w[id],
                                  rows: rental[:copies]),
      ClassroomDataset::Table.new(name: 'customer', columns: %w[id name], primary_key: %w[id],
                                  rows: rental[:customers]),
      ClassroomDataset::Table.new(name: 'loan', columns: %w[customer_id copy_id loan_date return_date],
                                  primary_key: %w[customer_id copy_id loan_date], rows: rental[:loans])
    ]
    ClassroomDataset::MysqlWriter.new(
      name: 'movies',
      output_dir: @output_dir,
      header: [
        'Movie data from Wikidata (https://www.wikidata.org/), CC0 1.0.',
        "Films chosen by number of IMDb votes (>= #{@min_votes}); no IMDb data is included.",
        'The video shop (copy, customer, loan) is invented.',
        "Generated: #{Time.now.utc.iso8601}"
      ],
      schema: MYSQL_SCHEMA,
      tables: tables,
      # the Filmdatenbank tutorial first models movies and genres, then adds
      # people, then a video shop
      parts: { 'films' => %w[genre movie movie_genre], 'crew' => %w[crew job movie_crew],
               'rental' => %w[copy customer loan] }
    )
  end

  # The schema the Filmdatenbank tutorial builds from its ER model
  # (src/content/filmdatenbank/film-3.er); the column lists must stay the same.
  MYSQL_SCHEMA = <<~SQL
    CREATE TABLE `genre` (
      `id` INT NOT NULL,
      `name` VARCHAR(255) NOT NULL,
      PRIMARY KEY (`id`),
      UNIQUE KEY `uq_genre_name` (`name`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

    CREATE TABLE `movie` (
      `id` INT NOT NULL,
      `title` VARCHAR(255) NOT NULL,
      `original_title` VARCHAR(255) NULL,
      `german_title` VARCHAR(255) NULL,
      `year` INT NOT NULL,
      `runtime` INT NOT NULL,
      PRIMARY KEY (`id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

    CREATE TABLE `movie_genre` (
      `movie_id` INT NOT NULL,
      `genre_id` INT NOT NULL,
      PRIMARY KEY (`movie_id`, `genre_id`),
      KEY `idx_movie_genre_genre` (`genre_id`),
      CONSTRAINT `fk_movie_genre_movie`
        FOREIGN KEY (`movie_id`) REFERENCES `movie` (`id`),
      CONSTRAINT `fk_movie_genre_genre`
        FOREIGN KEY (`genre_id`) REFERENCES `genre` (`id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

    CREATE TABLE `crew` (
      `id` INT NOT NULL,
      `name` VARCHAR(255) NOT NULL,
      `birth_year` INT NULL,
      `death_year` INT NULL,
      PRIMARY KEY (`id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

    CREATE TABLE `job` (
      `id` INT NOT NULL,
      `title` VARCHAR(255) NOT NULL,
      PRIMARY KEY (`id`),
      UNIQUE KEY `uq_job_title` (`title`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

    CREATE TABLE `movie_crew` (
      `movie_id` INT NOT NULL,
      `crew_id` INT NOT NULL,
      `job_id` INT NOT NULL,
      PRIMARY KEY (`movie_id`, `crew_id`, `job_id`),
      KEY `idx_movie_crew_crew` (`crew_id`),
      KEY `idx_movie_crew_job` (`job_id`),
      CONSTRAINT `fk_movie_crew_movie`
        FOREIGN KEY (`movie_id`) REFERENCES `movie` (`id`),
      CONSTRAINT `fk_movie_crew_crew`
        FOREIGN KEY (`crew_id`) REFERENCES `crew` (`id`),
      CONSTRAINT `fk_movie_crew_job`
        FOREIGN KEY (`job_id`) REFERENCES `job` (`id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

    CREATE TABLE `copy` (
      `id` INT NOT NULL,
      `movie_id` INT NOT NULL,
      `format` VARCHAR(255) NOT NULL,
      PRIMARY KEY (`id`),
      KEY `idx_copy_movie` (`movie_id`),
      CONSTRAINT `fk_copy_movie`
        FOREIGN KEY (`movie_id`) REFERENCES `movie` (`id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

    CREATE TABLE `customer` (
      `id` INT NOT NULL,
      `name` VARCHAR(255) NOT NULL,
      PRIMARY KEY (`id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

    CREATE TABLE `loan` (
      `customer_id` INT NOT NULL,
      `copy_id` INT NOT NULL,
      `loan_date` DATE NOT NULL,
      `return_date` DATE NULL,
      PRIMARY KEY (`customer_id`, `copy_id`, `loan_date`),
      KEY `idx_loan_copy` (`copy_id`),
      CONSTRAINT `fk_loan_customer`
        FOREIGN KEY (`customer_id`) REFERENCES `customer` (`id`),
      CONSTRAINT `fk_loan_copy`
        FOREIGN KEY (`copy_id`) REFERENCES `copy` (`id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
  SQL

  def write_neo4j(movies, crew, genres)
    ClassroomDataset.write_neo4j(File.join(@output_dir, ClassroomDataset.neo4j_file('movies'))) do |neo4j|
      movie_nodes = {}
      crew_nodes = {}
      genre_nodes = {}
      movies.each do |m|
        movie_nodes[m[:id]] = neo4j.node(['Movie'], m.slice(:id, :title, :original_title, :german_title, :year, :runtime).compact)
      end
      crew.each { |p| crew_nodes[p[:id]] = neo4j.node(['Person'], p.slice(:id, :name, :birth_year, :death_year).compact) }
      genres.each { |g| genre_nodes[g[:id]] = neo4j.node(['Genre'], id: g[:id], name: g[:name]) }
      movies.each do |m|
        from = movie_nodes.fetch(m[:id])
        m[:genres].each { |g| neo4j.relationship(from, genre_nodes.fetch(g), 'IN_GENRE') }
        m[:crew].each do |job, ids|
          ids.each { |c| neo4j.relationship(crew_nodes.fetch(c), from, job.upcase) }
        end
      end
    end
  end

  def report(movies, crew, genres)
    credits = movies.sum { |m| m[:crew].values.sum(&:size) }
    titles = movies.group_by { |m| m[:title] }.select { |_t, list| list.size > 1 }
    notes = []
    notes << "#{titles.size} titles occur more than once (e.g. #{titles.keys.first(3).join(', ')}): a title is not a key" if titles.any?
    notes << 'check by hand what the tutorial asks about: Back to the Future (1985), Christopher Nolan, Animation, at least five directors with five films'
    rental = Rental.build(movies)
    lent = rental[:loans].map { |l| l[1] }.uniq
    notes << "video shop (invented): #{rental[:copies].size} copies of #{rental[:copies].map { |c| c[1] }.uniq.size} films, " \
             "#{rental[:customers].size} customers, #{rental[:loans].size} loans, #{rental[:loans].count { |l| l[3].nil? }} still out, " \
             "#{rental[:copies].size - lent.size} copies never lent, " \
             "#{rental[:customers].size - rental[:loans].map(&:first).uniq.size} customers without a loan"
    ClassroomDataset.report('Sanity check:', {
      'movies' => movies.size,
      'people' => crew.size,
      'genres' => "#{genres.size} (#{genres.map { |g| g[:name] }.join(', ')})",
      'credits (movie, person, job)' => credits,
      'movies with a German title' => movies.count { |m| m[:german_title] },
      'years' => "#{movies.map { |m| m[:year] }.min}-#{movies.map { |m| m[:year] }.max}",
      'IMDb IDs not found in Wikidata' => @stats[:not_in_wikidata],
      'films without English title, year or runtime (left out)' => @stats[:films_without_title_year_or_runtime],
      'films without a known genre (kept)' => @stats[:films_without_genre],
      'people left out (beyond the limit per job)' => @stats[:people_left_out],
      'impossible death years dropped' => @stats[:impossible_life_dates],
      'Wikidata requests (not cached)' => @wikidata.requests
    }, notes)
  end

  def write_readme(movies, crew, genres)
    File.write(File.join(@output_dir, 'README.md'), <<~README)
      # Movie dataset (Wikidata)

      Films, genres and the people who made them, for the *Filmdatenbank mit MySQL*
      activity of the Hackschule Workspace. Built with `src/scripts/collect-movies` from
      https://github.com/specht/workspace.

      Generated: #{Time.now.utc.iso8601}

      ## Source and licence

      All data comes from [Wikidata](https://www.wikidata.org/), which is available
      under [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/): it may be
      copied, changed and shared without asking. Each film and person in `movies.txt`
      and `crew.txt` carries its Wikidata item ID (`wikidata`).

      Which films are included was decided with the number of user votes in IMDb's
      public `title.ratings` file: non-adult feature films with at least #{@min_votes}
      votes. Those films were looked up in Wikidata by their IMDb ID (property P345).
      Nothing else from IMDb is used or included.

      - `title`: English label; `german_title`: German label, if different;
        `original_title`: P1476, if different
      - `year`: earliest publication date (P577); `runtime`: duration in minutes (P2047)
      - genres: P136, mapped to #{genres.size} names (#{genres.map { |g| g[:name] }.join(', ')});
        a film may have none
      - people: cast member (P161, the 10 best known per film), director (P57),
        screenwriter (P58, as `writer`), composer (P86), producer (P162), director of
        photography (P344, as `cinematographer`) and film editor (P1040, as `editor`),
        only humans; best known means: most Wikipedia articles
      - `birth_year`, `death_year`: P569, P570; may be empty

      Wikidata is edited by many people. A wrong date or a missing genre is best fixed
      there, and the next build picks it up.

      The video shop in `copy`, `customer` and `loan` is invented: copies of some of
      the films, #{Rental::CUSTOMERS} customers with made-up names and their loans from
      #{Rental::FIRST_DAY.iso8601} to #{Rental::LAST_DAY.iso8601}, drawn with a fixed seed
      (`rental.rb`). `return_date` is empty while a copy is out.

      ## Files

      - `movies.txt` — #{movies.size} movies, one JSON object per line
      - `crew.txt` — #{crew.size} people, one JSON object per line
      - `genres.txt` — #{genres.size} genres, one JSON object per line
      - `movies.sql` — schema and complete data
      - `movies-schema.sql` — schema only
      - `movies-data.sql` — data only, for a schema built in class
      - `movies-data-films.sql` — data only: `genre`, `movie`, `movie_genre`
      - `movies-data-crew.sql` — data only: `crew`, `job`, `movie_crew`
      - `movies-data-rental.sql` — data only: `copy`, `customer`, `loan` (invented)
      - `movies-neo4j.dump` — logical Neo4jBolt dump for `neo4j_bolt load`

      Every classroom dataset names its files this way, after the dataset (`countries`, `movies`, `music`).

      ## MySQL

      One line per table, the primary key first, `→` for a foreign key:

      ```
      movie(id, title, original_title, german_title, year, runtime)
      genre(id, name)
      movie_genre(movie_id → movie, genre_id → genre)
      crew(id, name, birth_year, death_year)
      job(id, title)
      movie_crew(movie_id → movie, crew_id → crew, job_id → job)
      copy(id, movie_id → movie, format)
      customer(id, name)
      loan(customer_id → customer, copy_id → copy, loan_date, return_date)
      ```

      `movie_crew` connects a movie, a person and a job; a person appears once per job
      and movie. `job.title` is one of `actor`, `director`, `writer`, `composer`,
      `producer`, `cinematographer`, `editor`. Titles are not unique, so a film is identified by its `id`, or by
      title and year.

      #{ClassroomDataset.er_section(MYSQL_SCHEMA)}

      ```sh
      mysql < movies.sql                 # everything
      mysql < movies-data-films.sql     # or step by step into tables built in class
      mysql < movies-data-crew.sql
      mysql < movies-data-rental.sql
      ```

      ## Neo4j

      `Movie`, `Person` and `Genre` nodes (the video shop is not in the graph); `(Movie)-[:IN_GENRE]->(Genre)` and
      `(Person)-[:ACTOR|DIRECTOR|WRITER|…]->(Movie)`.

      ```sh
      neo4j_bolt load movies-neo4j.dump
      ```
    README
  end
end

options = {
  output_dir: 'movie-archive',
  min_votes: 100_000,
  force_download: false,
  offline: false,
  pause: 1.0
}

parser = OptionParser.new do |opts|
  opts.banner = 'Usage: collect-movies.rb [options]'
  opts.on('-o', '--output DIR', "Output directory (default: #{options[:output_dir]})") { |dir| options[:output_dir] = dir }
  opts.on('--min-votes N', Integer, "Films with at least N IMDb votes (default: #{options[:min_votes]})") { |n| options[:min_votes] = n }
  opts.on('--force-download', 'Download the IMDb vote files again') { options[:force_download] = true }
  opts.on('--offline', 'Use only cached Wikidata answers') { options[:offline] = true }
  opts.on('--pause SECONDS', Float, "Pause between Wikidata requests (default: #{options[:pause]})") { |s| options[:pause] = s }
end

if $PROGRAM_NAME == __FILE__
  parser.parse!
  WikidataMovieBuilder.new(**options).run
end
