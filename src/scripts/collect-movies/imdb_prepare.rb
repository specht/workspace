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
    write_mysql(movies, crew, genres)
    write_neo4j(movies, crew, genres)
    write_readme(movies.size, crew.size, genres.size)

    STDERR.puts
    STDERR.puts "Done. Wrote:"
    %w[movies.txt crew.txt genres.txt mysql.sql neo4j.dump README.md].each do |name|
      STDERR.puts "  #{File.join(@output_dir, name)}"
    end
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

    Zlib::GzipReader.open(path) do |gz|
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

  def add_german_titles(movies, movie_ids)
    STDERR.puts 'Parsing German titles...'

    each_tsv('title.akas.tsv.gz') do |row|
      imdb_id = row['titleId']
      next unless movie_ids.include?(imdb_id)
      next unless row['region'] == 'DE'

      movie = movies.fetch(imdb_id)
      next if movie[:german_title]

      known_titles = [movie[:title], movie[:original_title]].compact
      title = row['title']
      next if known_titles.include?(title)

      movie[:german_title] = title
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
      person[:death_year] = row['deathYear'].to_i unless missing?(row['deathYear'])
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

  def write_mysql(movies, crew, genres)
    path = File.join(@output_dir, 'mysql.sql')
    jobs = jobs_for(movies)

    File.open(path, 'w') do |file|
      file.puts <<~SQL
        -- Generated from the IMDb non-commercial datasets at #{SOURCE_URL}
        -- Generated: #{Time.now.utc.iso8601}
        --
        -- Import into an existing MySQL database, for example:
        --   mysql -u USER -p DATABASE < mysql.sql

        SET NAMES utf8mb4;
        SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS;
        SET FOREIGN_KEY_CHECKS=0;

        DROP TABLE IF EXISTS `movie_crew`;
        DROP TABLE IF EXISTS `movie_genre`;
        DROP TABLE IF EXISTS `job`;
        DROP TABLE IF EXISTS `crew`;
        DROP TABLE IF EXISTS `genre`;
        DROP TABLE IF EXISTS `movie`;

        CREATE TABLE `movie` (
          `id` INT NOT NULL,
          `title` TEXT NOT NULL,
          `original_title` TEXT NULL,
          `german_title` TEXT NULL,
          `year` INT NOT NULL,
          `runtime` INT NOT NULL,
          `rating` DECIMAL(3,1) NOT NULL,
          PRIMARY KEY (`id`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

        CREATE TABLE `genre` (
          `id` INT NOT NULL,
          `name` VARCHAR(255) NOT NULL,
          PRIMARY KEY (`id`),
          UNIQUE KEY `uq_genre_name` (`name`)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

        CREATE TABLE `crew` (
          `id` INT NOT NULL,
          `name` TEXT NOT NULL,
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

      write_mysql_rows(file, 'genre', %w[id name], genres.sort_by { |genre| genre[:id] }.map do |genre|
        [genre[:id], genre[:name]]
      end)

      write_mysql_rows(file, 'crew', %w[id name birth_year death_year], crew.values.sort_by { |person| person[:id] }.map do |person|
        [person[:id], person[:name], person[:birth_year], person[:death_year]]
      end)

      write_mysql_rows(file, 'job', %w[id title], jobs.map { |title, id| [id, title] }.sort_by(&:first))

      write_mysql_rows(file, 'movie', %w[id title original_title german_title year runtime rating],
                       movies.values.sort_by { |movie| movie[:id] }.map do |movie|
        [movie[:id], movie[:title], movie[:original_title], movie[:german_title],
         movie[:year], movie[:runtime], movie[:rating]]
      end)

      movie_genres = []
      movie_crew = []
      movies.values.sort_by { |movie| movie[:id] }.each do |movie|
        movie[:genres].each { |genre_id| movie_genres << [movie[:id], genre_id] }
        next unless movie[:crew]

        movie[:crew].each do |job, crew_ids|
          crew_ids.each { |crew_id| movie_crew << [movie[:id], crew_id, jobs.fetch(job)] }
        end
      end

      write_mysql_rows(file, 'movie_genre', %w[movie_id genre_id], movie_genres)
      write_mysql_rows(file, 'movie_crew', %w[movie_id crew_id job_id], movie_crew)

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
    when Float
      raise "Cannot write non-finite number #{value.inspect} to MySQL" unless value.finite?

      value.to_s
    when String
      encoded = value.encode(Encoding::UTF_8)
      "_utf8mb4 0x#{encoded.unpack1('H*')}"
    else
      raise "Unsupported MySQL value: #{value.inspect}"
    end
  end

  def write_neo4j(movies, crew, genres)
    path = File.join(@output_dir, 'neo4j.dump')
    movie_node_ids = {}
    crew_node_ids = {}
    genre_node_ids = {}
    next_node_id = 0

    File.open(path, 'w') do |file|
      movies.values.sort_by { |movie| movie[:id] }.each do |movie|
        node_id = next_node_id
        next_node_id += 1
        movie_node_ids[movie[:id]] = node_id

        properties = {
          id: movie[:id],
          title: movie[:title],
          year: movie[:year],
          runtime: movie[:runtime],
          rating: movie[:rating]
        }
        properties[:original_title] = movie[:original_title] if movie[:original_title]
        properties[:german_title] = movie[:german_title] if movie[:german_title]
        write_neo4j_node(file, node_id, ['Movie'], properties)
      end

      crew.values.sort_by { |person| person[:id] }.each do |person|
        node_id = next_node_id
        next_node_id += 1
        crew_node_ids[person[:id]] = node_id

        properties = {
          id: person[:id],
          name: person[:name],
          birth_year: person[:birth_year]
        }
        properties[:death_year] = person[:death_year] if person[:death_year]
        write_neo4j_node(file, node_id, ['Person'], properties)
      end

      genres.sort_by { |genre| genre[:id] }.each do |genre|
        node_id = next_node_id
        next_node_id += 1
        genre_node_ids[genre[:id]] = node_id
        write_neo4j_node(file, node_id, ['Genre'], id: genre[:id], name: genre[:name])
      end

      movies.values.sort_by { |movie| movie[:id] }.each do |movie|
        from = movie_node_ids.fetch(movie[:id])
        movie[:genres].each do |genre_id|
          write_neo4j_relationship(file, from, genre_node_ids.fetch(genre_id), 'IN_GENRE')
        end

        next unless movie[:crew]

        movie[:crew].each do |job, crew_ids|
          type = neo4j_relationship_type(job)
          crew_ids.each do |crew_id|
            write_neo4j_relationship(file, crew_node_ids.fetch(crew_id), from, type)
          end
        end
      end
    end
  end

  def write_neo4j_node(file, id, labels, properties)
    file.puts "n #{JSON.generate(id: id, labels: labels, properties: properties)}"
  end

  def write_neo4j_relationship(file, from, to, type, properties = {})
    file.puts "r #{JSON.generate(from: from, to: to, type: type, properties: properties)}"
  end

  def neo4j_relationship_type(job)
    type = job.to_s.upcase.gsub(/[^A-Z0-9_]/, '_').gsub(/_+/, '_').sub(/\A_+/, '').sub(/_+\z/, '')
    raise "Cannot derive Neo4j relationship type from #{job.inspect}" if type.empty?

    type
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
      time. German alternative titles are added when available. `actress` principals
      are normalized to the `actor` category, matching the original classroom dataset.

      ## Generated files

      - `movies.txt` — #{movie_count} movies, one JSON object per line
      - `crew.txt` — #{crew_count} people, one JSON object per line
      - `genres.txt` — #{genre_count} genres, one JSON object per line
      - `mysql.sql` — creates the relational schema and inserts the complete dataset
      - `neo4j.dump` — logical Neo4jBolt dump ready for `neo4j_bolt load`

      ## MySQL

      `mysql.sql` creates these tables:

      - `movie`
      - `genre`
      - `crew`
      - `job`
      - `movie_genre`
      - `movie_crew`

      Import it into an existing database with, for example:

      ```sh
      mysql -u USER -p DATABASE < mysql.sql
      ```

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
  output_dir: 'imdb',
  min_votes: 100_000,
  force_download: false
}

parser = OptionParser.new do |opts|
  opts.banner = 'Usage: imdb_prepare.rb [options]'

  opts.on('-o', '--output DIR', 'Output directory (default: imdb)') do |dir|
    options[:output_dir] = dir
  end

  opts.on('--min-votes N', Integer, 'Minimum number of IMDb votes (default: 100000)') do |n|
    options[:min_votes] = n
  end

  opts.on('--force-download', 'Download all IMDb source files again') do
    options[:force_download] = true
  end
end

parser.parse!

ImdbDatasetBuilder.new(**options).run