# Expected tables, shared by the check types er (the tables a model translates
# to) and schema (the tables in the person's database). Both hand over the
# same shape, so a task reads the same either way:
#
#   tables:
#     - name: movie_genre
#       columns: [movie_id, genre_id]        # must exist; more are allowed
#       primary_key: [movie_id, genre_id]
#       foreign_keys: ["movie_id -> movie.id", "genre_id -> genre"]
#       rows: 2740                           # schema only: exactly
#       min_rows: 1                          # schema only: at least
#       indexes: [[last_name, first_name]]   # schema only: each the start of an index
#
# actual: { name => { :columns => [[name, type]], :pk => [..],
#                     :fks => [{ :cols => [..], :ref_table =>, :ref_cols => [..] }],
#                     :indexes => [{ :name =>, :cols => [..] }] (schema only),
#                     :rows => Integer or nil } }
module TableExpect
    NAME = /\A[A-Za-z_][A-Za-z0-9_]*\z/
    FK = /\A\s*([A-Za-z_]\w*(?:\s*,\s*[A-Za-z_]\w*)*)\s*->\s*([A-Za-z_]\w*)(?:\s*\.\s*([A-Za-z_]\w*(?:\s*,\s*[A-Za-z_]\w*)*))?\s*\z/
    KEYS = %w[name columns primary_key foreign_keys rows min_rows indexes].freeze
    # What only the real database has, not a model
    DATABASE_KEYS = %w[rows min_rows indexes].freeze

    # "genre_id -> genre.id" -> { cols: ['genre_id'], ref_table: 'genre', ref_cols: ['id'] }
    # (ref_cols nil: the referenced table's primary key).
    def self.parse_fk(text)
        m = FK.match(text.to_s)
        return nil unless m

        split = ->(s) { s.split(',').map(&:strip) }
        { :cols => split.(m[1]), :ref_table => m[2], :ref_cols => m[3] && split.(m[3]) }
    end

    # Spec errors for a tables list (TaskSpec). rows: whether row counts are allowed.
    def self.spec_errors(tables, rows:)
        return ['tables must be a list of tables (name, columns, …)'] unless tables.is_a?(Array) && !tables.empty?

        errors = []
        tables.each_with_index do |t, i|
            t = { 'name' => t } if t.is_a?(String)
            unless t.is_a?(Hash) && t['name'].to_s =~ NAME
                errors << "tables[#{i}] needs a name (letters, digits, _)"
                next
            end
            extra = t.keys - (rows ? KEYS : KEYS - DATABASE_KEYS)
            errors << "tables[#{i}]: unknown keys: #{extra.join(', ')}" unless extra.empty?
            %w[columns primary_key].each do |k|
                next unless t.key?(k)
                errors << "tables[#{i}].#{k} must be a list of column names" unless t[k].is_a?(Array) && !t[k].empty? && t[k].all? { |c| c.to_s =~ NAME }
            end
            if t.key?('foreign_keys')
                fks = t['foreign_keys']
                if !fks.is_a?(Array) || fks.any? { |f| parse_fk(f).nil? }
                    errors << "tables[#{i}].foreign_keys must look like \"genre_id -> genre.id\""
                end
            end
            %w[rows min_rows].each do |k|
                errors << "tables[#{i}].#{k} must be a whole number ≥ 0" if t.key?(k) && !(t[k].is_a?(Integer) && t[k] >= 0)
            end
            if t.key?('indexes') && !(t['indexes'].is_a?(Array) && !t['indexes'].empty? &&
                                      t['indexes'].all? { |cols| cols.is_a?(Array) && !cols.empty? && cols.all? { |c| c.to_s =~ NAME } })
                errors << "tables[#{i}].indexes must be a list of column lists, such as [[last_name, first_name]]"
            end
        end
        errors
    end

    def self.normalize(spec)
        spec.is_a?(String) ? { 'name' => spec } : spec
    end

    def self.code(name)
        "`#{name.to_s.tr('`', "'")}`"
    end

    def self.list(names)
        names = names.map { |n| code(n) }
        names.size <= 1 ? names.join : "#{names[0..-2].join(', ')} und #{names.last}"
    end

    PLACE = {
        :model => { :none => 'Aus deinem Modell entsteht keine Tabelle %s.', :table => 'Die Tabelle %s' },
        :database => { :none => 'In deiner Datenbank gibt es keine Tabelle %s.', :table => 'Die Tabelle %s' },
    }.freeze

    # [problem, message] for the first expectation that does not hold, or nil.
    def self.compare(expected, actual, place)
        expected.each do |spec|
            spec = normalize(spec)
            name = spec['name']
            table = actual[name]
            if table.nil?
                other = actual.keys.find { |n| n.downcase == name.downcase }
                hint = other ? " Es gibt aber #{code(other)}; bei Namen zählt Groß- und Kleinschreibung." : ''
                return [:missing_table, format(PLACE[place][:none], code(name)) + hint]
            end
            label = format(PLACE[place][:table], code(name))
            columns = table[:columns].map(&:first)
            missing = (spec['columns'] || []).reject { |c| columns.include?(c) }
            unless missing.empty?
                return [:missing_column, "#{label} hat noch #{missing.size == 1 ? 'keine Spalte' : 'keine Spalten'} #{list(missing)}. Sie hat: #{list(columns)}."]
            end
            if spec['primary_key'] && table[:pk] != spec['primary_key']
                if table[:pk].empty?
                    return [:primary_key, "#{label} hat keinen Primärschlüssel; verlangt ist #{list(spec['primary_key'])}."]
                end
                return [:primary_key, "Der Primärschlüssel von #{code(name)} ist #{list(table[:pk])}, verlangt ist #{list(spec['primary_key'])}."]
            end
            (spec['foreign_keys'] || []).each do |text|
                fk = parse_fk(text)
                ref_cols = fk[:ref_cols] || actual.dig(fk[:ref_table], :pk)
                hit = table[:fks].find { |f| f[:cols] == fk[:cols] }
                want = "#{fk[:cols].join(', ')} → #{fk[:ref_table]}#{ref_cols ? "(#{ref_cols.join(', ')})" : ''}"
                if hit.nil?
                    return [:foreign_key, "#{label} hat keinen Fremdschlüssel #{fk[:cols].join(', ')} → #{fk[:ref_table]}. Verlangt ist `#{want}`."]
                end
                if hit[:ref_table] != fk[:ref_table] || (ref_cols && hit[:ref_cols] != ref_cols)
                    return [:foreign_key, "Der Fremdschlüssel #{list(fk[:cols])} von #{code(name)} zeigt auf #{hit[:ref_table]}(#{hit[:ref_cols].join(', ')}), verlangt ist `#{want}`."]
                end
            end
            (spec['indexes'] || []).each do |cols|
                cols = cols.map(&:to_s)
                next if (table[:indexes] || []).any? { |ix| ix[:cols].first(cols.size) == cols }

                have = (table[:indexes] || []).map { |ix| "#{ix[:name]} (#{ix[:cols].join(', ')})" }
                return [:index, "#{label} hat noch keinen Index, der mit #{list(cols)} anfängt." +
                    (have.empty? ? '' : " Sie hat: #{have.map { |x| code(x) }.join(', ')}.")]
            end
            rows = table[:rows]
            if spec.key?('rows') && rows != spec['rows']
                return [:rows, "#{label} hat #{rows_text(rows)}, erwartet #{spec['rows'] == 1 ? 'ist eine' : "sind #{spec['rows']}"}."]
            end
            if spec.key?('min_rows') && rows.to_i < spec['min_rows']
                return [:rows, rows.to_i.zero? ? "#{label} ist noch leer." : "#{label} hat #{rows_text(rows)}, erwartet #{spec['min_rows'] == 1 ? 'ist mindestens eine' : "sind mindestens #{spec['min_rows']}"}."]
            end
        end
        nil
    end

    def self.rows_text(n)
        n.to_i == 1 ? 'eine Zeile' : "#{n.to_i} Zeilen"
    end

    # Markdown: one line per table, as it really is.
    def self.describe(actual, rows: false)
        return 'keine Tabellen' if actual.empty?

        actual.map do |name, t|
            cols = t[:columns].map do |c, type|
                text = type ? "#{c} #{type}" : c
                t[:pk].include?(c) ? "**#{text}**" : text
            end
            fks = t[:fks].map { |f| "#{f[:cols].join(', ')} → #{f[:ref_table]}(#{f[:ref_cols].join(', ')})" }
            line = "- #{code(name)}: #{cols.join(', ')}"
            line += "; Fremdschlüssel #{fks.join(', ')}" unless fks.empty?
            others = (t[:indexes] || []).reject { |ix| ix[:name] == 'PRIMARY' }
            line += "; #{others.size == 1 ? 'Index' : 'Indizes'} #{others.map { |ix| "#{ix[:name]} (#{ix[:cols].join(', ')})" }.join(', ')}" unless others.empty?
            line += "; #{rows_text(t[:rows])}" if rows && !t[:rows].nil?
            line
        end.join("\n")
    end
end
