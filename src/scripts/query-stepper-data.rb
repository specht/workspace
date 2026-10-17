#!/usr/bin/env ruby
# The tables of the query stepper (src/static/include/query_stepper.js),
# from the SQL files students download, as JSON for the browser:
#
#   src/scripts/query-stepper-data.rb countries countries.sql
#   src/scripts/query-stepper-data.rb movies movies-schema.sql movies-data-films.sql movies-data-crew.sql
#   src/scripts/query-stepper-data.rb music music-schema.sql
#
# countries (»Einführung in SQL«): all four tables, whole. movies
# (»Filmdatenbank«): genre, movie and movie_genre whole; of the people only
# directors and actors with at least MIN_ROLES films, so the file stays small
# enough for the browser. Writes
# src/static/query-stepper/<name>.json. The SQL files come from
# https://github.com/specht/workspace-files.
#
# music is not in the stepper; its file has the tables without rows, for
# _schema(music) only.
#
# With the rows go the keys of each table, from CREATE TABLE and ALTER TABLE
# in the same files: 'key' (the columns of the primary key) and
# 'references' (column => 'table.column'). The stepper shows them as a
# schema line, as does _schema(name) on a page (src/ruby/include/schemas.rb).
require 'json'

MIN_ROLES = 5
OUT_DIR = File.expand_path('../static/query-stepper', __dir__)

# The value tuples of every INSERT INTO `table` (…) VALUES (…), (…);
def inserts(sql)
    tables = Hash.new { |h, k| h[k] = { 'columns' => nil, 'rows' => [] } }
    sql.scan(/INSERT INTO `(\w+)` \(([^)]*)\) VALUES\s*(.*?);\s*$/m) do |table, columns, values|
        tables[table]['columns'] = columns.scan(/`(\w+)`/).flatten
        tables[table]['rows'].concat(tuples(values))
    end
    tables
end

def tuples(text)
    rows = []
    i = 0
    while (i = text.index('(', i))
        row = []
        i += 1
        loop do
            i += 1 while text[i] =~ /\s/
            if text[i] == "'"
                value = +''
                i += 1
                until text[i] == "'" && text[i + 1] != "'"
                    if text[i] == '\\' then value << text[i + 1]; i += 2
                    elsif text[i] == "'" then value << "'"; i += 2
                    else value << text[i]; i += 1
                    end
                end
                i += 1
                row << value
            else
                stop = text.index(/[,)]/, i)
                raw = text[i...stop].strip
                row << (raw == 'NULL' ? nil : (raw.include?('.') ? raw.to_f : raw.to_i))
                i = stop
            end
            i += 1 while text[i] =~ /\s/
            if text[i] == ','
                i += 1
            else
                i += 1
                break
            end
        end
        rows << row
    end
    rows
end

# The primary key and the foreign keys of every table
def keys(sql)
    tables = Hash.new { |h, k| h[k] = { 'key' => [], 'references' => {} } }
    names = ->(list) { list.scan(/`(\w+)`/).flatten }
    sql.scan(/CREATE TABLE `(\w+)` \((.*?)\n\)/m) do |table, body|
        tables[table]['columns'] = body.scan(/^\s*`(\w+)`/).flatten
        body.scan(/PRIMARY KEY \(([^)]*)\)/) { |(list)| tables[table]['key'] = names.(list) }
        body.scan(/FOREIGN KEY \(([^)]*)\)\s*REFERENCES `(\w+)` \(([^)]*)\)/) do |from, other, to|
            names.(from).zip(names.(to)).each { |a, b| tables[table]['references'][a] = "#{other}.#{b}" }
        end
    end
    sql.scan(/ALTER TABLE `(\w+)`\s+ADD PRIMARY KEY \(([^)]*)\)/) { |table, list| tables[table]['key'] = names.(list) }
    tables
end

def add_keys!(name, tables, files)
    found = files.map { |f| keys(File.read(f)) }.reduce({}) { |all, k| all.merge(k) }
    tables.each do |table, t|
        k = found[table] || { 'key' => [], 'references' => {} }
        references = k['references']
        t['key'] = k['key']
        t['references'] = references unless references.empty?
    end
end

# The countries of the world: every table as it is
def countries(files)
    files.map { |f| inserts(File.read(f)) }.reduce(:merge).to_h { |name, table| [name, table] }
end

def movies(files)
    tables = files.map { |f| inserts(File.read(f)) }.reduce(:merge)
    jobs = tables['job']['rows'].to_h { |id, title| [title, id] }
    roles = tables['movie_crew']['rows'].select { |r| r[2] == jobs['actor'] }.group_by { |r| r[1] }.transform_values(&:size)
    tables['movie_crew']['rows'].select! { |r| r[2] == jobs['director'] || (r[2] == jobs['actor'] && roles[r[1]] >= MIN_ROLES) }
    people = tables['movie_crew']['rows'].map { |r| r[1] }.to_h { |id| [id, true] }
    tables['crew']['rows'].select! { |r| people[r[0]] }
    note = "nur Regie und Schauspiel, Schauspielerinnen und Schauspieler ab #{MIN_ROLES} Filmen"
    tables['movie_crew']['note'] = note
    tables['crew']['note'] = note
    # The order of the chapter: movies and genres first
    %w[movie genre movie_genre crew job movie_crew].to_h { |name| [name, tables.fetch(name)] }
end

# The music: only the tables and their columns
def music(files)
    found = files.map { |f| keys(File.read(f)) }.reduce({}) { |all, k| all.merge(k) }
    found.to_h { |table, k| [table, { 'columns' => k['columns'], 'rows' => [] }] }
end

name, *files = ARGV
abort "usage: #{$0} countries countries.sql | movies movies-schema.sql movies-data-films.sql movies-data-crew.sql | music music-schema.sql" unless %w[countries movies music].include?(name) && !files.empty?
tables = send(name, files)
add_keys!(name, tables, files)
out = File.join(OUT_DIR, "#{name}.json")
File.write(out, JSON.generate({ 'source' => files.map { |f| File.basename(f) }.join(', '), 'tables' => tables }) + "\n")
tables.each { |table, t| puts "#{table}: #{t['rows'].size} rows, #{t['columns'].join(', ')}; key #{t['key'].join(', ')}#{t['references'] ? ", references #{t['references']}" : ''}" }
puts "#{out}: #{File.size(out) / 1024} kB"
