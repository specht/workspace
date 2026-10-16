#!/usr/bin/env ruby
# The tables of the query stepper (src/static/include/query_stepper.js) from
# terra1.sql, the sample database of the SQL introduction: all of land and
# the places in ort with at least MIN_PEOPLE inhabitants, so the file stays
# small enough for the browser. Writes src/static/query-stepper/terra1.json.
#
#   src/scripts/query-stepper-data.rb PATH/TO/terra1.sql
#
# terra1.sql comes from https://github.com/specht/workspace-files.
require 'json'

MIN_PEOPLE = 1_000_000
OUT = File.expand_path('../static/query-stepper/terra1.json', __dir__)

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

# Decimal columns come quoted ('8.51'): numbers again, as MySQL shows them.
NUMERIC = { 'land' => %w[Einwohner Flaeche KontinentFlaeche KontinentEinwohner], 'ort' => %w[Einwohner Laenge Breite] }.freeze

sql = File.read(ARGV.fetch(0) { abort "usage: #{$0} terra1.sql" })
tables = inserts(sql)
tables.each do |name, table|
    (NUMERIC[name] || []).each do |column|
        k = table['columns'].index(column)
        table['rows'].each do |row|
            value = row[k]
            value = Float(value) if value.is_a?(String) && value.match?(/\A-?\d+(\.\d+)?\z/)
            value = value.to_i if value.is_a?(Float) && value == value.floor
            row[k] = value
        end
    end
end
people = tables['ort']['columns'].index('Einwohner')
tables['ort']['rows'].select! { |row| row[people].to_i >= MIN_PEOPLE }
tables['ort']['note'] = "nur Orte ab #{MIN_PEOPLE.to_s.reverse.scan(/\d{1,3}/).join('.').reverse} Einwohnern"
File.write(OUT, JSON.generate({ 'source' => 'terra1.sql', 'tables' => tables }) + "\n")
tables.each { |name, t| puts "#{name}: #{t['rows'].size} rows, #{t['columns'].join(', ')}" }
puts "#{OUT}: #{File.size(OUT) / 1024} kB"
