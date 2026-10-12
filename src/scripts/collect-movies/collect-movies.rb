#!/usr/bin/env ruby
# frozen_string_literal: true

require 'fileutils'
require 'json'
require 'net/http'
require 'optparse'
require 'set'
require 'time'
require 'uri'
require 'zlib'

require_relative '../lib/classroom_dataset'

class ImdbDatasetBuilder
  SOURCE_URL = 'https://datasets.imdbws.com/'

  DATASETS = %w[
    title.ratings.tsv.gz
    title.basics.tsv.gz
    title.akas.tsv.gz
    title.principals.tsv.gz
    name.basics.tsv.gz
  ].freeze

  def initialize(output_dir:, min_votes: 100_000, force_download: false)
    @output_dir = File.expand_path(output_dir)
    @download_dir = File.join(@output_dir, '.downloads')
    @min_votes = min_votes
    @force_download = force_download
    @stats = Hash.new(0)
  end

  def run
    FileUtils.mkdir_p(@download_dir)

    STDERR.puts "Downloading IMDb source data to #{@download_dir}..."
    DATASETS.each { |name| download(name) }

    ratings = load_ratings
    movies, movie_ids, genres = load_movies(ratings)
    add_german_titles(movies, movie_ids)
    people_ids = add_principals(movies, movie_ids)
    crew, crew_ids = load_people(people_ids)
    replace_imdb_person_ids(movies, crew_ids)

    write_movies(movies)
    write_crew(crew)
    write_genres(genres)
    mysql = mysql_writer(movies, crew, genres)
    mysql.write
    write_neo4j(movies, crew, genres)
    write_readme(movies.size, crew.size, genres.size)

    STDERR.puts
    STDERR.puts "Done. Wrote to #{@output_dir}:"
    (%w[movies.txt crew.txt genres.txt] + mysql.files + [ClassroomDataset::NEO4J_FILE, 'README.md']).each do |name|
      STDERR.puts "  #{name}"
    end
    report(movies, crew, genres)
  end

  private

  def download(name)
    target = File.join(@download_dir, name)
    if File.exist?(target) && File.size?(target) && !@force_download
      STDERR.puts "  using cached #{name}"
      return
    end

    url = URI.join(SOURCE_URL, name)
    tmp = "#{target}.part"

    STDERR.puts "  downloading #{url}"
    File.open(tmp, 'wb') do |file|
      request_to_file(url, file)
    end
    File.rename(tmp, target)
  rescue StandardError
    FileUtils.rm_f(tmp) if tmp
    raise
  end

  def request_to_file(uri, file, redirects_left = 5)
    raise "Too many redirects while downloading #{uri}" if redirects_left.zero?

    request = Net::HTTP::Get.new(uri)
    request['User-Agent'] = 'imdb-dataset-builder/1.0'

    Net::HTTP.start(
      uri.host,
      uri.port,
      use_ssl: uri.scheme == 'https',
      open_timeout: 30,
      read_timeout: 120
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

  def each_tsv(name)
    path = File.join(@download_dir, name)

    # IMDb files are UTF-8, whatever the locale of the shell running the build
    Zlib::GzipReader.open(path, external_encoding: Encoding::UTF_8) do |gz|
      headers = gz.readline.chomp.split("\t", -1)
      gz.each_line do |line|
        values = line.chomp.split("\t", -1)
        yield headers.zip(values).to_h
      end
    end
  end

  def load_ratings
    STDERR.puts 'Parsing ratings...'
    ratings = {}

    each_tsv('title.ratings.tsv.gz') do |row|
      votes = row['numVotes'].to_i
      next if votes < @min_votes

      ratings[row['tconst']] = row['averageRating'].to_f
    end

    ratings
  end

  def load_movies(ratings)
    STDERR.puts 'Parsing titles...'

    movie_ids = Set.new
    movies = {}
    movie_id_by_imdb_id = {}
    genre_id_by_name = {}
    genres = []

    each_tsv('title.basics.tsv.gz') do |row|
      next unless row['titleType'] == 'movie'
      next unless row['isAdult'] == '0'
      next if missing?(row['runtimeMinutes'])
      next if missing?(row['startYear'])
      next if missing?(row['genres'])

      imdb_id = row['tconst']
      rating = ratings[imdb_id]
      next unless rating

      movie_ids << imdb_id
      movie_id_by_imdb_id[imdb_id] ||= movie_id_by_imdb_id.size + 1

      genre_ids = row['genres'].split(',').map do |name|
        genre_id_by_name[name] ||= begin
          id = genres.size + 1
          genres << { id: id, name: name }
          id
        end
      end

      movie = {
        id: movie_id_by_imdb_id.fetch(imdb_id),
        title: row['primaryTitle'],
        year: row['startYear'].to_i,
        runtime: row['runtimeMinutes'].to_i,
        genres: genre_ids,
        rating: rating
      }

      original_title = row['originalTitle']
      if !missing?(original_title) && original_title != row['primaryTitle']
        movie[:original_title] = original_title
      end

      movies[imdb_id] = movie
    end

    [movies, movie_ids, genres]
  end

  # Several German titles can exist (display title, working title, festival
  # title, a literal translation). The one IMDb displays in Germany wins; a title
  # without a type comes next; any other type only if nothing better exists.
  def add_german_titles(movies, movie_ids)
    STDERR.puts 'Parsing German titles...'
    best = {}

    each_tsv('title.akas.tsv.gz') do |row|
      imdb_id = row['titleId']
      next unless movie_ids.include?(imdb_id)
      next unless row['region'] == 'DE'

      title = row['title']
      next if missing?(title)

      types = missing?(row['types']) ? [] : row['types'].split(/[,\u0002]/)
      rank = if types.include?('imdbDisplay') then 0
             elsif types.empty? then 1
             else 2
             end
      best[imdb_id] = [rank, title] if best[imdb_id].nil? || rank < best[imdb_id][0]
    end

    # a German display title equal to the English or original title means the
    # film has no title of its own in Germany
    best.each do |imdb_id, (_rank, title)|
      movie = movies.fetch(imdb_id)
      movie[:german_title] = title unless [movie[:title], movie[:original_title]].include?(title)
    end
  end

  def add_principals(movies, movie_ids)
    STDERR.puts 'Parsing principals...'
    people_ids = Set.new

    each_tsv('title.principals.tsv.gz') do |row|
      imdb_id = row['tconst']
      next unless movie_ids.include?(imdb_id)

      category = row['category']
      category = 'actor' if category == 'actress'

      movie = movies.fetch(imdb_id)
      movie[:crew] ||= {}
      movie[:crew][category] ||= []
      # IMDb lists a person once per character, and actress -> actor merges
      # two categories: keep each person once per job, or (movie, person, job)
      # would not be unique in movie_crew
      if movie[:crew][category].include?(row['nconst'])
        @stats[:duplicate_principals] += 1
        next
      end
      movie[:crew][category] << row['nconst']
      people_ids << row['nconst']
    end

    people_ids
  end

  def load_people(people_ids)
    STDERR.puts 'Parsing names...'

    crew = {}
    crew_id_by_imdb_id = {}

    each_tsv('name.basics.tsv.gz') do |row|
      imdb_id = row['nconst']
      next unless people_ids.include?(imdb_id)
      next if missing?(row['birthYear'])

      crew_id_by_imdb_id[imdb_id] ||= crew_id_by_imdb_id.size + 1
      person = {
        id: crew_id_by_imdb_id.fetch(imdb_id),
        name: row['primaryName'],
        birth_year: row['birthYear'].to_i
      }
      unless missing?(row['deathYear'])
        death_year = row['deathYear'].to_i
        # years before the common era carry no sign in IMDb (Homer: 850, 800)
        if death_year < person[:birth_year]
          @stats[:impossible_life_dates] += 1
        else
          person[:death_year] = death_year
        end
      end
      crew[imdb_id] = person
    end

    [crew, crew_id_by_imdb_id]
  end

  def replace_imdb_person_ids(movies, crew_id_by_imdb_id)
    movies.each_value do |movie|
      next unless movie[:crew]

      movie[:crew].each do |category, people|
        movie[:crew][category] = people.filter_map { |imdb_id| crew_id_by_imdb_id[imdb_id] }
      end
      movie[:crew].delete_if { |_category, people| people.empty? }
      movie.delete(:crew) if movie[:crew].empty?
    end
  end

  def write_movies(movies)
    path = File.join(@output_dir, 'movies.txt')
    sorted = movies.values.sort_by { |movie| [movie[:year], movie[:title].downcase] }

    File.open(path, 'w') do |file|
      sorted.each { |movie| file.puts(JSON.generate(movie)) }
    end
  end

  def write_crew(crew)
    path = File.join(@output_dir, 'crew.txt')
    sorted = crew.values.sort_by { |person| [person[:birth_year], person[:name].downcase] }

    File.open(path, 'w') do |file|
      sorted.each { |person| file.puts(JSON.generate(person)) }
    end
  end

  def write_genres(genres)
    path = File.join(@output_dir, 'genres.txt')

    File.open(path, 'w') do |file|
      genres.each { |genre| file.puts(JSON.generate(genre)) }
    end
  end

  def sorted_movies(movies)
    movies.values.sort_by { |movie| [movie[:year], movie[:title].downcase, movie[:id]] }
  end

  def jobs_for(movies)
    job_ids = {}
    sorted_movies(movies).each do |movie|
      next unless movie[:crew]

      movie[:crew].each_key do |job|
        job_ids[job] ||= job_ids.size + 1
      end
    end
    job_ids
  end

  def mysql_writer(movies, crew, genres)
    jobs = jobs_for(movies)
    by_id = movies.values.sort_by { |movie| movie[:id] }
    movie_genres = by_id.flat_map { |movie| movie[:genres].map { |genre_id| [movie[:id], genre_id] } }
    movie_crew = by_id.flat_map do |movie|
      (movie[:crew] || {}).flat_map do |job, crew_ids|
        crew_ids.map { |crew_id| [movie[:id], crew_id, jobs.fetch(job)] }
      end
    end

    tables = [
      ClassroomDataset::Table.new(name: 'genre', columns: %w[id name], primary_key: %w[id],
                                  rows: genres.sort_by { |genre| genre[:id] }.map { |genre| [genre[:id], genre[:name]] }),
      ClassroomDataset::Table.new(name: 'movie', columns: %w[id title original_title german_title year runtime rating],
                                  primary_key: %w[id],
                                  rows: by_id.map do |movie|
                                    [movie[:id], movie[:title], movie[:original_title], movie[:german_title],
                                     movie[:year], movie[:runtime], movie[:rating]]
                                  end),
      ClassroomDataset::Table.new(name: 'movie_genre', columns: %w[movie_id genre_id], primary_key: %w[movie_id genre_id],
                                  rows: movie_genres),
      ClassroomDataset::Table.new(name: 'crew', columns: %w[id name birth_year death_year], primary_key: %w[id],
                                  rows: crew.values.sort_by { |person| person[:id] }.map do |person|
                                    [person[:id], person[:name], person[:birth_year], person[:death_year]]
                                  end),
      ClassroomDataset::Table.new(name: 'job', columns: %w[id title], primary_key: %w[id],
                                  rows: jobs.map { |title, id| [id, title] }.sort_by(&:first)),
      ClassroomDataset::Table.new(name: 'movie_crew', columns: %w[movie_id crew_id job_id],
                                  primary_key: %w[movie_id crew_id job_id], rows: movie_crew)
    ]

    ClassroomDataset::MysqlWriter.new(
      output_dir: @output_dir,
      header: [
        "Generated from the IMDb non-commercial datasets at #{SOURCE_URL}",
        "Generated: #{Time.now.utc.iso8601}"
      ],
      schema: MYSQL_SCHEMA,
      tables: tables,
      # the Filmdatenbank tutorial first models movies and genres, then adds people
      parts: { 'movies' => %w[genre movie movie_genre], 'crew' => %w[crew job movie_crew] }
    )
  end

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
      `rating` DECIMAL(3,1) NOT NULL,
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
      `birth_year` INT NOT NULL,
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
  SQL

  def write_neo4j(movies, crew, genres)
    ClassroomDataset.write_neo4j(File.join(@output_dir, ClassroomDataset::NEO4J_FILE)) do |neo4j|
      movie_nodes = {}
      crew_nodes = {}
      genre_nodes = {}

      movies.values.sort_by { |movie| movie[:id] }.each do |movie|
        properties = movie.slice(:id, :title, :year, :runtime, :rating)
        properties[:original_title] = movie[:original_title] if movie[:original_title]
        properties[:german_title] = movie[:german_title] if movie[:german_title]
        movie_nodes[movie[:id]] = neo4j.node(['Movie'], properties)
      end

      crew.values.sort_by { |person| person[:id] }.each do |person|
        crew_nodes[person[:id]] = neo4j.node(['Person'], person.slice(:id, :name, :birth_year, :death_year))
      end

      genres.sort_by { |genre| genre[:id] }.each do |genre|
        genre_nodes[genre[:id]] = neo4j.node(['Genre'], id: genre[:id], name: genre[:name])
      end

      movies.values.sort_by { |movie| movie[:id] }.each do |movie|
        from = movie_nodes.fetch(movie[:id])
        movie[:genres].each { |genre_id| neo4j.relationship(from, genre_nodes.fetch(genre_id), 'IN_GENRE') }

        (movie[:crew] || {}).each do |job, crew_ids|
          type = neo4j_relationship_type(job)
          crew_ids.each { |crew_id| neo4j.relationship(crew_nodes.fetch(crew_id), from, type) }
        end
      end
    end
  end

  def neo4j_relationship_type(job)
    type = job.to_s.upcase.gsub(/[^A-Z0-9_]/, '_').gsub(/_+/, '_').sub(/\A_+/, '').sub(/_+\z/, '')
    raise "Cannot derive Neo4j relationship type from #{job.inspect}" if type.empty?

    type
  end

  def report(movies, crew, genres)
    credits = movies.values.sum { |movie| (movie[:crew] || {}).values.sum(&:size) }
    titles = movies.values.group_by { |movie| movie[:title] }.select { |_title, list| list.size > 1 }
    notes = []
    notes << "#{titles.size} titles occur more than once (e.g. #{titles.keys.first(3).join(', ')}): a title is not a key" if titles.any?
    ClassroomDataset.report('Sanity check:', {
      'movies' => movies.size,
      'people' => crew.size,
      'genres' => genres.size,
      'credits (movie, person, job)' => credits,
      'movies with a German title' => movies.values.count { |movie| movie[:german_title] },
      'years' => "#{movies.values.map { |m| m[:year] }.min}-#{movies.values.map { |m| m[:year] }.max}",
      'duplicate credits removed' => @stats[:duplicate_principals],
      'impossible death years dropped' => @stats[:impossible_life_dates]
    }, notes)
  end

  def write_readme(movie_count, crew_count, genre_count)
    path = File.join(@output_dir, 'README.md')
    generated_at = Time.now.utc.iso8601

    File.write(path, <<~README)
      # IMDb-derived movie dataset

      This directory was generated from the IMDb datasets published at:

      #{SOURCE_URL}

      Generated: #{generated_at}

      Source files used:

      #{DATASETS.map { |name| "- #{SOURCE_URL}#{name}" }.join("\n")}

      The generated data contains only non-adult titles of type `movie` with a known
      year, runtime and genre, and with at least #{@min_votes} IMDb votes at generation
      time. German titles are added when available, preferring the title IMDb displays
      in Germany. `actress` principals are normalized to the `actor` category, matching
      the original classroom dataset. Death years before the birth year (ancient authors,
      whose years carry no sign in IMDb) are dropped.

      ## Generated files

      - `movies.txt` — #{movie_count} movies, one JSON object per line
      - `crew.txt` — #{crew_count} people, one JSON object per line
      - `genres.txt` — #{genre_count} genres, one JSON object per line
      - `mysql.sql` — schema and complete data
      - `mysql-schema.sql` — schema only
      - `mysql-data.sql` — data only, for a schema built in class
      - `mysql-data-movies.sql` — data only: `genre`, `movie`, `movie_genre`
      - `mysql-data-crew.sql` — data only: `crew`, `job`, `movie_crew`
      - `neo4j.dump` — logical Neo4jBolt dump ready for `neo4j_bolt load`

      Every classroom dataset (also the Discogs music dataset) uses these file names.

      ## MySQL

      The relational schema has the tables `movie`, `genre`, `movie_genre`, `crew`,
      `job` and `movie_crew`. `movie_crew` connects a movie, a person and a job; a person
      appears once per job and movie, even if IMDb lists several characters.

      Load everything into the current database:

      ```sh
      mysql < mysql.sql
      ```

      Or load the data step by step into tables created in class (the column names must
      match):

      ```sh
      mysql < mysql-data-movies.sql
      mysql < mysql-data-crew.sql
      ```

      Titles are not unique: two films may share a title (e.g. *Nosferatu*, 1922 and
      2024), so queries identify a film by its `id` or by title and year.

      ## Neo4j / Neo4jBolt

      `neo4j.dump` uses Neo4jBolt's textual logical dump format. It contains:

      - `Movie` nodes
      - `Person` nodes
      - `Genre` nodes
      - `(Movie)-[:IN_GENRE]->(Genre)` relationships
      - `(Person)-[:ACTOR|DIRECTOR|WRITER|...]->(Movie)` relationships

      Load it into an empty Neo4j database with:

      ```sh
      neo4j_bolt load neo4j.dump
      ```

      Neo4jBolt uses dump-local integer node IDs in this file to reconnect
      relationships while loading; these are not Neo4j database-internal IDs.

      ## Source cache and provenance

      The original compressed IMDb files are cached in `.downloads/` so the script can
      be run again without downloading them again. Use `--force-download` to refresh
      them.

      IMDb data is provided by IMDb. See the IMDb dataset page and its linked terms for
      the conditions that apply to use of the source data. This generated README is only
      a provenance note and does not replace those terms.
    README
  end

  def missing?(value)
    value.nil? || value == '\\N'
  end
end

options = {
  output_dir: 'videothek',
  min_votes: 100_000,
  force_download: false
}

parser = OptionParser.new do |opts|
  opts.banner = 'Usage: collect-movies.rb [options]'

  opts.on('-o', '--output DIR', "Output directory (default: #{options[:output_dir]})") do |dir|
    options[:output_dir] = dir
  end

  opts.on('--min-votes N', Integer, "Minimum number of IMDb votes (default: #{options[:min_votes]})") do |n|
    options[:min_votes] = n
  end

  opts.on('--force-download', 'Download all IMDb source files again') do
    options[:force_download] = true
  end
end

if $PROGRAM_NAME == __FILE__
  parser.parse!
  ImdbDatasetBuilder.new(**options).run
end