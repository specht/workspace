require 'bigdecimal'
require 'cgi'
require 'set'

# Check type sql (docs/aufgaben.md): the student writes a SELECT query; the
# server runs it in the student's own MySQL database, logged in as the
# student, next to the task's reference query, and compares the results.
#
# Read only (rule 1): only a single SELECT (or WITH … SELECT) is accepted,
# it runs inside START TRANSACTION READ ONLY with a time limit, and the
# connection is closed afterwards, which rolls the transaction back.
#
# The runner is passed in: lambda(sql) -> { columns:, rows:, truncated: } or
# { error: { code:, message: } }, or nil if the database cannot be reached.
module SqlCheck
    MAX_ROWS = 5000
    MAX_MS = 5000
    SHOWN_ROWS = 10
    SHOWN_CELL = 40

    # --- what may run -----------------------------------------------------------

    # Returns [query, nil] with a trailing ; removed, or [nil, message].
    def self.single_select(text)
        sql = text.to_s
        return [nil, 'Schreib zuerst eine Abfrage.'] if sql.strip.empty?

        plain = +''       # the query outside strings and comments
        state = :normal
        semicolon = nil   # position of the first ; outside strings/comments
        i = 0
        while i < sql.size
            c = sql[i]
            n = sql[i + 1]
            case state
            when :normal
                if c == "'" || c == '"' || c == '`'
                    state = c
                    plain << ' '
                elsif c == '#' || (c == '-' && n == '-' && (sql[i + 2].nil? || sql[i + 2] =~ /\s/))
                    state = :line
                elsif c == '/' && n == '*'
                    return [nil, 'Kommentare der Form `/*! … */` und `/*+ … */` sind hier nicht erlaubt.'] if %w[! +].include?(sql[i + 2])

                    state = :block
                    i += 1
                elsif c == ';'
                    semicolon ||= plain.size
                    plain << ';'
                else
                    plain << c
                end
            when :line
                state = :normal if c == "\n"
                plain << ' ' if c == "\n"
            when :block
                if c == '*' && n == '/'
                    state = :normal
                    plain << ' '
                    i += 1
                end
            else
                if c == '\\' && state != '`'
                    i += 1
                elsif c == state
                    if n == state
                        i += 1
                    else
                        state = :normal
                    end
                end
            end
            i += 1
        end
        return [nil, 'Ein Text in Anführungszeichen ist nicht geschlossen.'] if %w[' " `].include?(state)
        return [nil, 'Ein Kommentar `/* … */` ist nicht geschlossen.'] if state == :block

        if semicolon
            rest = plain[(semicolon + 1)..].to_s
            return [nil, 'Eine Abfrage reicht hier. Hinter dem `;` darf nichts mehr stehen.'] unless rest.strip.empty?
        end
        words = plain[0, semicolon || plain.size].to_s
        unless words =~ /\A[\s(]*(SELECT|WITH)\b/i
            first = words.strip[/\A\w+/]
            return [nil, "Hier geht nur eine Abfrage mit `SELECT`#{first ? ", nicht `#{first.upcase}`" : ''}. Die Prüfung liest deine Datenbank nur und ändert nichts."]
        end
        if words =~ /\bINTO\s+(OUTFILE|DUMPFILE)\b/i
            return [nil, 'In eine Datei schreiben (`INTO OUTFILE`) geht hier nicht. Die Prüfung liest nur.']
        end

        query = semicolon ? sql[0...index_of_semicolon(sql)] : sql
        [query.strip, nil]
    end

    # The position of the first ; outside strings and comments in sql.
    def self.index_of_semicolon(sql)
        state = :normal
        i = 0
        while i < sql.size
            c = sql[i]
            n = sql[i + 1]
            case state
            when :normal
                if c == "'" || c == '"' || c == '`' then state = c
                elsif c == '#' || (c == '-' && n == '-' && (sql[i + 2].nil? || sql[i + 2] =~ /\s/)) then state = :line
                elsif c == '/' && n == '*' then state = :block; i += 1
                elsif c == ';' then return i
                end
            when :line then state = :normal if c == "\n"
            when :block
                if c == '*' && n == '/' then state = :normal; i += 1 end
            else
                if c == '\\' && state != '`' then i += 1
                elsif c == state
                    if n == state then i += 1 else state = :normal end
                end
            end
            i += 1
        end
        sql.size
    end

    # --- comparing results ------------------------------------------------------

    # One value as text: numbers without trailing zeros (8.50 = 8.5),
    # NULL as nil.
    def self.value(v)
        case v
        when nil then nil
        when Integer then v.to_s
        when Float, BigDecimal, Rational
            return v.to_s if v.is_a?(Float) && !v.finite?

            BigDecimal(v.to_s).round(6).to_s('F').sub(/\.0\z/, '')
        when String
            v =~ /\A-?\d+(\.\d+)?\z/ && v.size < 40 ? value(BigDecimal(v)) : v
        else v.to_s
        end
    end

    def self.normalize(result)
        { :columns => result[:columns].map(&:to_s), :rows => result[:rows].map { |r| r.map { |v| value(v) } },
          :truncated => result[:truncated] }
    end

    def self.multiset(rows)
        rows.each_with_object(Hash.new(0)) { |r, h| h[r] += 1 }
    end

    # Finds which student column holds each expected column. Returns an
    # array (expected index -> student index) or nil.
    #   mode :same    the projected rows equal the expected rows (as a multiset)
    #   mode :subset  every projected row is one of the expected rows
    def self.assign(expected, given, mode)
        m = expected[:columns].size
        n = given[:columns].size
        return nil if n < m

        e_cols = (0...m).map { |i| expected[:rows].map { |r| r[i] } }
        g_cols = (0...n).map { |j| given[:rows].map { |r| r[j] } }
        candidates = (0...m).map do |i|
            (0...n).select do |j|
                if mode == :same
                    e_cols[i].sort_by(&:to_s) == g_cols[j].sort_by(&:to_s) && e_cols[i].count(nil) == g_cols[j].count(nil)
                else
                    allowed = e_cols[i].to_set
                    g_cols[j].all? { |v| allowed.include?(v) }
                end
            end
        end
        return nil if candidates.any?(&:empty?)

        target = mode == :same ? multiset(expected[:rows]) : expected[:rows].to_set
        tries = 0
        search = lambda do |i, used, chosen|
            tries += 1
            return nil if tries > 20_000
            if i == m
                projected = given[:rows].map { |r| chosen.map { |j| r[j] } }
                ok = mode == :same ? multiset(projected) == target : projected.all? { |p| target.include?(p) }
                return ok ? chosen.dup : nil
            end
            candidates[i].each do |j|
                next if used.include?(j)

                found = search.call(i + 1, used | [j], chosen + [j])
                return found if found
            end
            nil
        end
        search.call(0, [], [])
    end

    # "2 DESC, 1" -> [1, 0] (expected column indexes; the direction comes
    # from the reference query's own ORDER BY).
    def self.sort_columns(spec)
        spec.to_s.split(',').map { |part| part.strip[/\A\d+/].to_i - 1 }.reject(&:negative?)
    end

    # Is the student's result in the order the reference result defines?
    # Ties may come in any order. Returns nil or [row_before, row_after].
    def self.order_problem(expected, given, mapping, columns)
        key = ->(row) { columns.map { |c| row[c] } }
        rank = {}
        expected[:rows].each_with_index { |r, i| rank[key.(r)] ||= i }
        projected = given[:rows].map { |r| mapping.map { |j| r[j] } }
        projected.each_cons(2).each_with_index do |(a, b), i|
            next unless rank[key.(a)] && rank[key.(b)] && rank[key.(a)] > rank[key.(b)]

            return [given[:rows][i], given[:rows][i + 1]]
        end
        nil
    end

    # A value from the student's database inside Markdown: HTML escaped,
    # Markdown characters taken literally, shortened.
    def self.safe(v)
        return 'NULL' if v.nil?

        text = v.to_s.gsub(/\s+/, ' ')
        text = "#{text[0, SHOWN_CELL]}…" if text.size > SHOWN_CELL
        CGI.escapeHTML(text).gsub(/([\\`*_\[\]|~])/) { "\\#{$1}" }
    end

    def self.code(name)
        "`#{name.to_s.tr('`', "'")[0, SHOWN_CELL]}`"
    end

    def self.row_text(row)
        "(#{row.map { |v| safe(v) }.join(', ')})"
    end

    def self.lines(count)
        count == 1 ? '1 Zeile' : "#{count} Zeilen"
    end

    # Compares; returns [status, message, extra_note].
    def self.compare(check, expected, given)
        mode = check['compare'] == 'subset' ? :subset : :same
        m = expected[:columns].size
        n = given[:columns].size
        return [:wrong, 'Deine Abfrage liefert mehr als 5000 Zeilen. Das ist sicher nicht gemeint.', nil] if given[:truncated]

        if mode == :subset && check['rows'] && given[:rows].size != check['rows']
            return [:wrong, "Deine Abfrage liefert #{lines(given[:rows].size)}, gefragt sind #{check['rows']}.", nil]
        end
        if mode == :subset && given[:rows].uniq.size != given[:rows].size
            return [:wrong, 'In deinem Ergebnis kommt eine Zeile mehrfach vor.', nil]
        end
        if n < m
            names = expected[:columns].map { |c| code(c) }.join(', ')
            return [:wrong, "Deine Abfrage liefert #{n == 1 ? '1 Spalte' : "#{n} Spalten"}, gebraucht #{m == 1 ? 'wird 1' : "werden #{m}"} (#{names}).", nil]
        end
        if mode == :same && given[:rows].size != expected[:rows].size
            if given[:rows].empty?
                return [:wrong, 'Deine Abfrage liefert keine Zeilen.', nil]
            end
            return [:wrong, "Deine Abfrage liefert #{lines(given[:rows].size)}, richtig #{expected[:rows].size == 1 ? 'ist 1 Zeile' : "sind #{expected[:rows].size} Zeilen"}.", nil]
        end

        mapping = assign(expected, given, mode)
        return [:wrong, difference(expected, given, mode), nil] if mapping.nil?

        if check['sorted_by'] && (bad = order_problem(expected, given, mapping, sort_columns(check['sorted_by'])))
            return [:wrong, "Die Zeilen stimmen, aber die Reihenfolge noch nicht: #{row_text(bad[0])} steht vor #{row_text(bad[1])}.", nil]
        end

        extra = (0...n).to_a - mapping
        note = nil
        unless extra.empty?
            names = extra.map { |j| code(given[:columns][j]) }.join(', ')
            case check['extra_columns'] || 'note'
            when 'wrong'
                return [:wrong, "Die nötigen Spalten stimmen, aber #{extra.size == 1 ? 'die Spalte' : 'die Spalten'} #{names} #{extra.size == 1 ? 'gehört' : 'gehören'} nicht dazu.", nil]
            when 'note'
                note = if extra.size > 3
                    "Für die Antwort #{m == 1 ? 'reicht eine Spalte' : "reichen #{m} Spalten"} statt #{n}. Schreib hinter `SELECT` nur die, nach denen gefragt ist."
                else
                    "Für die Antwort #{extra.size == 1 ? 'brauchst du die Spalte' : 'brauchst du die Spalten'} #{names} nicht."
                end
            end
        end
        [:correct, nil, note]
    end

    # Why the rows differ, without giving away the answer: rows the student
    # has that do not belong (their own), and how many are missing.
    def self.difference(expected, given, mode)
        if expected[:rows].size == 1 && given[:rows].size == 1 && expected[:columns].size == 1
            row = given[:rows][0]
            return "Dein Ergebnis ist #{row.size == 1 ? safe(row[0]) : row_text(row)}. Das stimmt noch nicht."
        end

        # Best guess which student column belongs to which expected one: most
        # values in common.
        m = expected[:columns].size
        n = given[:columns].size
        used = []
        mapping = (0...m).map do |i|
            values = expected[:rows].map { |r| r[i] }.to_set
            best = ((0...n).to_a - used).max_by { |j| given[:rows].count { |r| values.include?(r[j]) } }
            used << best
            best
        end
        projected = given[:rows].map { |r| mapping.map { |j| r[j] } }
        target = expected[:rows].to_set
        foreign = given[:rows].each_with_index.reject { |_, k| target.include?(projected[k]) }.map(&:first)
        missing = mode == :same ? (expected[:rows].to_set - projected.to_set).size : 0
        parts = []
        unless foreign.empty?
            parts << "#{foreign.size == 1 ? '1 Zeile deines Ergebnisses gehört' : "#{foreign.size} Zeilen deines Ergebnisses gehören"} nicht dazu, zum Beispiel #{row_text(foreign.first)}."
        end
        parts << "#{missing == 1 ? '1 Zeile fehlt' : "#{missing} Zeilen fehlen"}." if missing > 0
        parts << 'Die Werte stimmen noch nicht.' if parts.empty?
        parts.join(' ')
    end

    # --- what the student sees --------------------------------------------------

    def self.cell(v)
        v.nil? ? '*NULL*' : safe(v)
    end

    # Markdown: the student's own result as a table, first rows only.
    def self.table(result)
        rows = result[:rows]
        count = result[:truncated] ? "mehr als #{MAX_ROWS} Zeilen" : lines(rows.size)
        return "Dein Ergebnis: #{count}." if result[:columns].empty?

        io = +"Dein Ergebnis: #{count}.\n\n"
        io << "| #{result[:columns].map { |c| cell(c) }.join(' | ')} |\n"
        io << "|#{' --- |' * result[:columns].size}\n"
        rows.first(SHOWN_ROWS).each { |r| io << "| #{r.map { |v| cell(v) }.join(' | ')} |\n" }
        io << "\n… und #{lines(rows.size - SHOWN_ROWS)} mehr." if rows.size > SHOWN_ROWS
        io
    end

    # MySQL's own message, with a sentence in German for the usual ones.
    def self.error_message(error, setup = nil)
        code = error[:code].to_i
        text = error[:message].to_s.strip
        said = case code
            when 1064 then 'MySQL versteht die Abfrage nicht. Die Meldung zeigt, ab welcher Stelle es nicht weitergeht:'
            when 1146 then "Diese Tabelle gibt es in deiner Datenbank nicht. `SHOW TABLES;` zeigt, welche es gibt.#{setup ? " #{setup}" : ''}"
            when 1054 then 'Diese Spalte gibt es nicht. `DESCRIBE` zeigt die Spalten einer Tabelle, zum Beispiel `DESCRIBE land;`.'
            when 1052 then 'Ein Spaltenname kommt in mehreren Tabellen vor. Schreib die Tabelle davor, zum Beispiel `ort.Name`.'
            when 1111, 1140 then 'COUNT, SUM und Co. passen hier nicht zu den anderen Spalten (oder gehören in `HAVING` statt `WHERE`).'
            when 3024, 1317 then "Die Abfrage lief länger als #{MAX_MS / 1000} Sekunden und wurde abgebrochen."
            when 1792 then 'Hier geht nur Lesen. Die Prüfung ändert nichts in deiner Datenbank.'
            when 1142, 1044, 1227 then 'Das darf deine Datenbank-Anmeldung nicht.'
            else 'MySQL meldet einen Fehler:'
        end
        "#{said}\n\n```\n#{text.gsub('```', "'''")}\n```"
    end

    # MySQL client errors (2xxx) and refused logins: not the student's query.
    def self.unreachable?(error)
        code = error[:code].to_i
        code >= 2000 && code < 3000 || code == 1045
    end

    # --- checking ---------------------------------------------------------------

    # Returns [status, message, read, note, problem].
    def self.check(task, check, answer, runner)
        query, refusal = single_select(answer)
        return [:invalid, refusal, nil, nil, :refused] if query.nil?
        return [:unavailable, UNREACHABLE, nil, nil, :unreachable] if runner.nil?

        # The sample data must be there as loaded, or an empty table would
        # make an empty answer right.
        (check['tables'] || {}).each do |name, rows|
            result = runner.call("SELECT COUNT(*) FROM `#{name}`")
            return [:unavailable, UNREACHABLE, nil, nil, :unreachable] if result.nil? || (result[:error] && unreachable?(result[:error]))

            if result[:error]
                return [:unavailable, "In deiner Datenbank gibt es die Tabelle `#{name}` nicht. #{check['setup'] || ''}".strip, nil, nil, :no_data]
            end
            count = result[:rows].dig(0, 0).to_i
            if rows && count != rows
                return [:unavailable, "Die Tabelle `#{name}` hat #{lines(count)}, die Beispieldaten haben #{rows}. Hast du etwas geändert? #{check['setup'] || ''}".strip, nil, nil, :data_changed]
            end
        end

        references = Array(check['expect_query'])
        expected = []
        references.each do |reference|
            result = runner.call(reference)
            return [:unavailable, UNREACHABLE, nil, nil, :unreachable] if result.nil? || (result[:error] && unreachable?(result[:error]))

            if result[:error]
                if result[:error][:code].to_i == 1146 || result[:error][:code].to_i == 1054
                    return [:unavailable, "In deiner Datenbank fehlen die Daten für diese Aufgabe. #{check['setup'] || ''}".strip, nil, nil, :no_data]
                end
                return [:unavailable, 'Die Musterlösung dieser Aufgabe lässt sich gerade nicht ausführen. Sag deiner Lehrkraft Bescheid.', nil, nil, :reference_failed]
            end
            expected << normalize(result)
        end

        result = runner.call(query)
        return [:unavailable, UNREACHABLE, nil, nil, :unreachable] if result.nil? || (result[:error] && unreachable?(result[:error]))
        if result[:error]
            # A typical mistake can also show as an error (6,046 instead of 6.046).
            say = feedback(task, check, query, nil, runner)
            message = error_message(result[:error], check['setup'])
            return [:wrong, say ? "#{say}\n\n#{message}" : message, nil, nil, :sql_error]
        end

        given = normalize(result)
        # Shown as MySQL sent it (182.60); compared normalized (182.6).
        read = table(:columns => result[:columns].map(&:to_s), :rows => result[:rows], :truncated => result[:truncated])
        outcomes = expected.map { |e| compare(check, e, given) }
        hit = outcomes.find { |status, _, _| status == :correct }
        return [:correct, 'Passt.', read, hit[2], nil] if hit

        say = feedback(task, check, query, given, runner)
        [:wrong, say || outcomes.first[1], read, nil, :result]
    end

    UNREACHABLE = 'Deine Datenbank ist gerade nicht erreichbar. Versuch es gleich noch einmal.'

    # A typical mistake: the student's result equals that of a known wrong
    # query (same_result_as), or the query itself looks like one (query_matches).
    def self.feedback(task, check, query, given, runner)
        (task['feedback'] || []).each do |entry|
            w = entry['when']
            ok = true
            if w['query_matches']
                ok &&= begin
                    Regexp.new(w['query_matches'], Regexp::IGNORECASE, timeout: 0.5).match?(query)
                rescue RegexpError, Regexp::TimeoutError
                    false
                end
            end
            if ok && w['same_result_as']
                next if given.nil?

                other = runner.call(w['same_result_as'])
                ok &&= other && !other[:error] && compare(check.merge('extra_columns' => 'ok'), normalize(other), given)[0] == :correct
            end
            return entry['say'] if ok
        end
        nil
    end

    # Runs one query in the student's database, logged in as the student,
    # read-only and with a time limit. Needs the mysql2 gem (on the server).
    def self.mysql_runner(host:, username:, password:, database:)
        lambda do |sql|
            require 'mysql2'
            client = nil
            begin
                client = Mysql2::Client.new(:host => host, :username => username, :password => password,
                    :database => database, :encoding => 'utf8mb4', :reconnect => false,
                    :connect_timeout => 5, :read_timeout => MAX_MS / 1000 + 10, :write_timeout => 5)
                client.query("SET SESSION max_execution_time = #{MAX_MS}")
                client.query('START TRANSACTION READ ONLY')
                # Values as MySQL sends them (strings), so the student sees
                # 182.60 like in mycli; compare() normalizes numbers.
                result = client.query(sql, :stream => true, :cache_rows => false, :as => :array, :cast => false)
                columns = result ? result.fields : []
                rows = []
                truncated = false
                result&.each do |row|
                    if rows.size >= MAX_ROWS
                        truncated = true
                        break
                    end
                    rows << row
                end
                { :columns => columns, :rows => rows, :truncated => truncated }
            rescue Mysql2::Error => e
                { :error => { :code => e.error_number, :message => e.message } }
            ensure
                # Closing the connection rolls the read-only transaction back.
                client&.close
            end
        end
    end
end
