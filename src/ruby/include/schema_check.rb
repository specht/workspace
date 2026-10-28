require_relative 'table_expect'
require_relative 'sql_check'

# Check type schema (docs/aufgaben.md): the tables in the person's own MySQL
# database. Reads information_schema as the person (SqlCheck.mysql_runner,
# read-only) and counts rows only where the task asks for them.
#
#   check:
#     type: schema
#     tables:
#       - name: movie_genre
#         columns: [movie_id, genre_id]
#         primary_key: [movie_id, genre_id]
#         foreign_keys: ["movie_id -> movie.id", "genre_id -> genre.id"]
#         indexes: [[movie_id]]
#         min_rows: 1
#     - name: city
#       rows_where:                    # rows that meet a condition, counted
#         - where: "name = 'Weimar'"
#           rows: 1
#     setup: Lege die Tabellen mit `mysql < film.sql` an.
module SchemaCheck
    COLUMNS = <<~SQL.freeze
        SELECT table_name, column_name, column_type FROM information_schema.columns
        WHERE table_schema = DATABASE() ORDER BY table_name, ordinal_position
    SQL
    PRIMARY = <<~SQL.freeze
        SELECT table_name, column_name FROM information_schema.key_column_usage
        WHERE table_schema = DATABASE() AND constraint_name = 'PRIMARY' ORDER BY table_name, ordinal_position
    SQL
    FOREIGN = <<~SQL.freeze
        SELECT table_name, constraint_name, column_name, referenced_table_name, referenced_column_name
        FROM information_schema.key_column_usage
        WHERE table_schema = DATABASE() AND referenced_table_name IS NOT NULL
        ORDER BY table_name, constraint_name, ordinal_position
    SQL
    INDEXES = <<~SQL.freeze
        SELECT table_name, index_name, column_name FROM information_schema.statistics
        WHERE table_schema = DATABASE() ORDER BY table_name, index_name, seq_in_index
    SQL
    PROBLEMS = %w[no_tables missing_table missing_column absent_column primary_key foreign_key index rows rows_where].freeze

    def self.spec_errors(check)
        errors = TableExpect.spec_errors(check['tables'], :rows => true)
        errors << 'setup must be text' if check.key?('setup') && !(check['setup'].is_a?(String) && !check['setup'].strip.empty?)
        errors
    end

    # Reads the tables (TableExpect shape). Returns [tables, stop] where stop
    # is a SqlCheck result tuple when the database cannot be read.
    def self.read(check, runner, env)
        results = [COLUMNS, PRIMARY, FOREIGN, INDEXES].map do |sql|
            result = runner.call(sql)
            stop = SqlCheck.connection_stop({}, env, result)
            return [nil, stop] if stop
            if result[:error]
                return [nil, [:unavailable, 'Ich kann den Aufbau deiner Datenbank nicht lesen.',
                    SqlCheck.diagnosis({}, nil, env, :error => result[:error]), nil, :unreachable]]
            end
            result[:rows]
        end
        columns, primary, foreign, indexes = results
        tables = {}
        columns.each do |table, column, type|
            t = (tables[table.to_s] ||= { :columns => [], :pk => [], :fks => [], :indexes => [], :rows => nil })
            t[:columns] << [column.to_s, type.to_s.upcase]
        end
        primary.each { |table, column| tables[table.to_s][:pk] << column.to_s if tables[table.to_s] }
        foreign.group_by { |row| [row[0].to_s, row[1].to_s] }.each do |(table, _), rows|
            next unless tables[table]

            tables[table][:fks] << { :cols => rows.map { |r| r[2].to_s }, :ref_table => rows.first[3].to_s,
                                     :ref_cols => rows.map { |r| r[4].to_s } }
        end
        indexes.group_by { |row| [row[0].to_s, row[1].to_s] }.each do |(table, name), rows|
            tables[table][:indexes] << { :name => name, :cols => rows.map { |r| r[2].to_s } } if tables[table]
        end
        check['tables'].map { |s| TableExpect.normalize(s) }.each do |spec|
            next unless (spec.key?('rows') || spec.key?('min_rows')) && tables[spec['name']]

            result = runner.call("SELECT COUNT(*) FROM `#{spec['name']}`")
            stop = SqlCheck.connection_stop({}, env, result)
            return [nil, stop] if stop

            tables[spec['name']][:rows] = result[:error] ? nil : result[:rows].dig(0, 0).to_i
        end
        check['tables'].map { |s| TableExpect.normalize(s) }.each do |spec|
            next unless spec['rows_where'] && tables[spec['name']]

            tables[spec['name']][:rows_where] = spec['rows_where'].map do |rule|
                result = runner.call("SELECT COUNT(*) FROM `#{spec['name']}` WHERE #{rule['where']}")
                stop = SqlCheck.connection_stop({}, env, result)
                return [nil, stop] if stop
                if result[:error]
                    return [nil, [:unavailable, 'Die Prüfung dieser Aufgabe lässt sich gerade nicht ausführen. Sag deiner Lehrkraft Bescheid.',
                        SqlCheck.diagnosis({}, nil, env, :error => result[:error]), nil, :reference_failed]]
                end
                result[:rows].dig(0, 0).to_i
            end
        end
        [tables, nil]
    end

    # What was read; with rows_where, what was counted first, since that is
    # what such a task is about.
    def self.describe(tables, env, check = {})
        counted = (check['tables'] || []).map { |s| TableExpect.normalize(s) }.flat_map do |spec|
            found = tables.dig(spec['name'], :rows_where) || []
            (spec['rows_where'] || []).each_with_index.map do |rule, j|
                "- #{TableExpect.code(spec['name'])} mit `#{rule['where']}`: #{found[j].nil? ? 'nicht gezählt' : TableExpect.rows_text(found[j])}"
            end
        end
        head = counted.empty? ? '' : "Gezählt in `#{env[:database] || '?'}`:\n\n#{counted.join("\n")}\n\n"
        "#{head}Gelesen: Datenbank `#{env[:database] || '?'}` (Primärschlüssel fett)\n\n#{TableExpect.describe(tables, :rows => true)}"
    end

    # [status, message, read, note, problem] like SqlCheck.check.
    def self.check(task, check, runner, env = {})
        if runner.nil?
            return [:unavailable, SqlCheck.connection_message(nil, env), SqlCheck.diagnosis({}, nil, env), nil, :unreachable]
        end

        tables, stop = read(check, runner, env)
        return stop if stop

        read = describe(tables, env, check)
        problem, message = if tables.empty?
            [:no_tables, "In deiner Datenbank#{env[:database] ? " `#{env[:database]}`" : ''} gibt es noch keine Tabellen."]
        else
            TableExpect.compare(check['tables'], tables, :database)
        end
        return [:correct, 'Passt.', read, nil, nil] if problem.nil?

        say = (task['feedback'] || []).find { |entry| entry['when']['problem'].to_s == problem.to_s }
        message = say ? say['say'] : [message, (check['setup'] if %i[no_tables missing_table].include?(problem))].compact.join(' ')
        [:wrong, message, read, nil, problem]
    end
end
