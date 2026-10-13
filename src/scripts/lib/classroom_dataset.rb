# frozen_string_literal: true

require 'fileutils'
require 'json'
require 'time'

# Shared output contract for the classroom datasets (collect-movies,
# collect-music). Every dataset directory contains the same files, so
# tutorials and tools can load any dataset the same way:
#
#   mysql.sql              schema and data (DROP + CREATE + INSERT)
#   mysql-schema.sql       schema only (DROP + CREATE)
#   mysql-data.sql         data only (INSERT), for a schema students built themselves
#   mysql-data-PART.sql    data only, one part at a time (e.g. movies before crew)
#   neo4j.dump             logical Neo4jBolt dump for `neo4j_bolt load`
#
# plus the dataset's own JSON Lines files and README.md.
module ClassroomDataset
  MYSQL_FILES = %w[mysql.sql mysql-schema.sql mysql-data.sql].freeze
  NEO4J_FILE = 'neo4j.dump'

  # One table's rows. `primary_key` lists the column names that must be
  # unique together; a duplicate is a bug in the collector and stops the build,
  # because MySQL would reject the whole INSERT batch on import.
  Table = Struct.new(:name, :columns, :rows, :primary_key, keyword_init: true)

  class MysqlWriter
    # header: comment lines (without "-- ")
    # schema: the CREATE TABLE statements, in an order that satisfies the foreign keys
    # tables: Table objects in insert order
    # parts:  { 'movies' => %w[genre movie movie_genre], ... }, loaded in the
    #         listed order; a part may only point to tables of itself or of an
    #         earlier part
    def initialize(output_dir:, header:, schema:, tables:, parts: {})
      @output_dir = output_dir
      @header = header
      @schema = schema.strip
      @tables = tables
      @parts = parts
      check_tables!
    end

    def write
      by_name = @tables.to_h { |table| [table.name, table] }
      write_file('mysql.sql', 'schema and complete data') do |file|
        write_schema(file)
        write_data(file, @tables)
      end
      write_file('mysql-schema.sql', 'schema only') { |file| write_schema(file) }
      write_file('mysql-data.sql', 'data only, for an existing schema') { |file| write_data(file, @tables) }
      @parts.each do |part, names|
        tables = names.map { |name| by_name.fetch(name) { raise "Part #{part}: unknown table #{name}" } }
        write_file("mysql-data-#{part}.sql", "data only, part \"#{part}\" (#{names.join(', ')})") do |file|
          write_data(file, tables)
        end
      end
    end

    def files
      MYSQL_FILES + @parts.keys.map { |part| "mysql-data-#{part}.sql" }
    end

    private

    def check_tables!
      @tables.each do |table|
        width = table.columns.size
        bad = table.rows.find { |row| row.size != width }
        raise "#{table.name}: row #{bad.inspect} does not have #{width} columns" if bad
        next unless table.primary_key

        indexes = table.primary_key.map { |column| table.columns.index(column) || raise("#{table.name}: no column #{column}") }
        seen = {}
        table.rows.each do |row|
          key = indexes.map { |index| row[index] }
          raise "#{table.name}: duplicate primary key #{table.primary_key.zip(key).to_h}" if seen[key]

          seen[key] = true
        end
      end
    end

    def write_file(name, what)
      File.open(File.join(@output_dir, name), 'w') do |file|
        @header.each { |line| file.puts(line.empty? ? '--' : "-- #{line}") }
        file.puts "-- This file: #{what}."
        file.puts "-- Import into an existing database, for example:  mysql < #{name}"
        file.puts
        file.puts 'SET NAMES utf8mb4;'
        yield file
      end
    end

    def write_schema(file)
      table_names = @tables.map(&:name)
      file.puts
      file.puts 'SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS;'
      file.puts 'SET FOREIGN_KEY_CHECKS=0;'
      file.puts
      table_names.reverse_each { |name| file.puts "DROP TABLE IF EXISTS `#{name}`;" }
      file.puts
      file.puts @schema
      file.puts
      file.puts 'SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;'
    end

    def write_data(file, tables)
      file.puts
      file.puts 'SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS;'
      file.puts 'SET FOREIGN_KEY_CHECKS=0;'
      tables.each { |table| write_rows(file, table) }
      file.puts
      file.puts 'SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;'
      # Right after a large import InnoDB has no statistics for the new rows
      # yet, and a join over several tables can take a minute instead of a
      # second (Filmdatenbank, "Wer spielt mit wem?"). The import prints one
      # "OK" line per table for it.
      file.puts
      file.puts "ANALYZE TABLE #{tables.map { |table| "`#{table.name}`" }.join(', ')};"
    end

    def write_rows(file, table, batch_size: 1_000)
      return if table.rows.empty?

      quoted_columns = table.columns.map { |column| "`#{column}`" }.join(', ')
      table.rows.each_slice(batch_size) do |batch|
        file.puts
        file.puts "INSERT INTO `#{table.name}` (#{quoted_columns}) VALUES"
        file.puts batch.map { |row| "  (#{row.map { |value| ClassroomDataset.mysql_literal(value) }.join(', ')})" }.join(",\n")
        file.puts ';'
      end
    end
  end

  # Strings are written as ordinary quoted literals so students can read the
  # dump. Only a string with a backslash or a control character falls back to a
  # hex literal: its meaning would otherwise depend on the server's SQL mode
  # (NO_BACKSLASH_ESCAPES).
  def self.mysql_literal(value)
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
      if encoded.match?(/[\\\x00-\x1f\x7f]/)
        "_utf8mb4 0x#{encoded.unpack1('H*')}"
      else
        "'#{encoded.gsub("'", "''")}'"
      end
    else
      raise "Unsupported MySQL value: #{value.inspect}"
    end
  end

  # Neo4jBolt logical dump: "n {...}" node lines, then "r {...}" relationship lines.
  class Neo4jWriter
    def initialize(path)
      @file = File.open(path, 'w')
      @next_id = 0
      @relationships = {}
    end

    def node(labels, properties)
      id = @next_id
      @next_id += 1
      @file.puts "n #{JSON.generate(id: id, labels: labels, properties: properties)}"
      id
    end

    # The same relationship is written once, however often the source lists it.
    def relationship(from, to, type, properties = {})
      key = [from, to, type, properties]
      return if @relationships.key?(key)

      @relationships[key] = true
      @file.puts "r #{JSON.generate(from: from, to: to, type: type, properties: properties)}"
    end

    def close
      @file.close
    end
  end

  def self.write_neo4j(path)
    writer = Neo4jWriter.new(path)
    yield writer
  ensure
    writer&.close
  end

  # Printed at the end of every build, so a refresh can be judged before its
  # diff is committed.
  def self.report(title, counts, notes = [])
    width = counts.keys.map { |key| key.to_s.size }.max || 0
    STDERR.puts
    STDERR.puts title
    counts.each { |key, value| STDERR.puts "  #{key.to_s.ljust(width)}  #{value}" }
    notes.each { |note| STDERR.puts "  note: #{note}" }
  end
end
