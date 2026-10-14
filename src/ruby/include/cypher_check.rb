require_relative 'sql_check'

# Check type cypher (docs/aufgaben.md): the student writes a Cypher query;
# the server runs it in the student's own Neo4j database, logged in as the
# student (never as an administrator), next to the task's reference query,
# and compares the results like SqlCheck (same columns and rows, order only
# when asked for).
#
# Read only (rule 1), twice over:
# - before anything is sent, single_read refuses a query with a clause that
#   writes or reaches beyond the database (CREATE, MERGE, SET, DELETE,
#   REMOVE, DROP, LOAD CSV, CALL, FOREACH, USE and the administration
#   commands), outside strings and comments;
# - the query runs in a transaction in READ access mode, which Neo4j itself
#   enforces (»Writing in read access mode not allowed«), with a time limit,
#   and at most MAX_ROWS rows are read.
#
# The runner is passed in: lambda(query) -> { columns:, rows:, truncated: }
# or { error: { code:, message: } }.
module CypherCheck
    MAX_ROWS = SqlCheck::MAX_ROWS
    MAX_SECONDS = 5

    # Clauses that write, run procedures or leave the database. CALL also
    # covers subqueries, which these tasks never need.
    REFUSED = %w[CREATE MERGE SET DELETE DETACH REMOVE DROP FOREACH CALL USE
                 LOAD ALTER GRANT DENY REVOKE RENAME START STOP TERMINATE SHOW].freeze

    # The query without strings, comments and `quoted names`, so a word
    # inside »…« or a property called `set` does not count. Returns
    # [plain, nil] or [nil, message] for an unterminated string.
    def self.plain_text(text)
        plain = +''
        state = :normal
        i = 0
        while i < text.size
            c = text[i]
            n = text[i + 1]
            case state
            when :normal
                if c == "'" || c == '"' || c == '`'
                    state = c
                    plain << ' '
                elsif c == '/' && n == '/'
                    state = :line
                    i += 1
                elsif c == '/' && n == '*'
                    state = :block
                    i += 1
                else
                    plain << c
                end
            when :line
                if c == "\n"
                    state = :normal
                    plain << "\n"
                end
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
                    state = :normal
                end
            end
            i += 1
        end
        return [nil, 'In deiner Abfrage fehlt ein schließendes Anführungszeichen.'] if %w[' " `].include?(state)
        return [nil, 'In deiner Abfrage ist ein Kommentar `/*` nicht geschlossen.'] if state == :block

        [plain, nil]
    end

    # Returns [query, nil] with a trailing ; removed, or [nil, message].
    def self.single_read(text)
        query = text.to_s.strip
        return [nil, 'Schreib zuerst eine Abfrage.'] if query.empty?

        plain, problem = plain_text(query)
        return [nil, problem] if plain.nil?

        body = plain.rstrip.sub(/;\s*\z/, '')
        return [nil, 'Bitte nur eine Abfrage, ohne `;` dazwischen.'] if body.include?(';')

        words = body.scan(/[A-Za-z_][A-Za-z0-9_]*/).map(&:upcase)
        refused = words.find { |w| REFUSED.include?(w) }
        if refused
            return [nil, "`#{refused}` gehört zu Befehlen, die die Datenbank verändern oder über sie hinaus greifen. " \
                'Hier sind nur Abfragen erlaubt, die lesen: `MATCH …`, `WHERE …`, `RETURN …`.']
        end
        return [nil, 'Eine Abfrage braucht ein `RETURN`, sonst kommt nichts zurück.'] unless words.include?('RETURN')

        [query.sub(/;\s*\z/, ''), nil]
    end

    # --- values -------------------------------------------------------------

    # A value from the driver as plain Ruby (String, Integer, Float, nil,
    # true/false); nodes, relationships and paths as cypher-shell shows them.
    def self.plain_value(v)
        return v if v.nil? || v.is_a?(String) || v.is_a?(Integer) || v.is_a?(Float) || v == true || v == false

        if v.is_a?(Array)
            "[#{v.map { |x| show(plain_value(x)) }.join(', ')}]"
        elsif v.is_a?(Hash)
            "{#{v.map { |k, x| "#{k}: #{show(plain_value(x))}" }.join(', ')}}"
        elsif v.respond_to?(:labels) && v.respond_to?(:properties)
            labels = v.labels.map { |l| ":#{l}" }.join
            "(#{labels} #{plain_value(v.properties.transform_keys(&:to_s))})"
        elsif v.respond_to?(:type) && v.respond_to?(:properties) && v.respond_to?(:start_node_id)
            "[:#{v.type}]"
        elsif v.respond_to?(:nodes) && v.respond_to?(:relationships)
            "(Weg mit #{v.relationships.size} Beziehungen)"
        else
            v.to_s
        end
    end

    def self.show(v)
        v.is_a?(String) ? "\"#{v}\"" : v.nil? ? 'null' : v.to_s
    end

    # --- running --------------------------------------------------------------

    # Runs one query in the person's own database as them, read-only, with a
    # time limit. Needs the neo4j-ruby-driver gem (on the server).
    def self.runner(uri:, login:, password:)
        lambda do |query|
            require 'neo4j/driver'
            driver = nil
            begin
                driver = Neo4j::Driver::GraphDatabase.driver(uri, Neo4j::Driver::AuthTokens.basic(login, password),
                    connection_timeout: 5, max_transaction_retry_time: 0)
                driver.session(database: login, default_access_mode: Neo4j::Driver::AccessMode::READ) do |session|
                    session.execute_read(timeout: MAX_SECONDS) do |tx|
                        result = tx.run(query)
                        columns = result.keys.map(&:to_s)
                        rows = []
                        truncated = false
                        result.each do |record|
                            if rows.size >= MAX_ROWS
                                truncated = true
                                break
                            end
                            rows << record.values.map { |v| plain_value(v) }
                        end
                        { :columns => columns, :rows => rows, :truncated => truncated }
                    end
                end
            rescue Neo4j::Driver::Exceptions::ServiceUnavailableException, Neo4j::Driver::Exceptions::SessionExpiredException => e
                { :error => { :code => 'unreachable', :message => e.message } }
            rescue Neo4j::Driver::Exceptions::Neo4jException => e
                { :error => { :code => (e.respond_to?(:code) ? e.code : e.class.name).to_s, :message => e.message } }
            ensure
                driver&.close
            end
        end
    end

    # --- what the student sees ------------------------------------------------

    # Neo4j's own message, with a sentence in German for the usual ones.
    def self.error_message(error, setup = nil)
        code = error[:code].to_s
        text = error[:message].to_s.strip.lines.first(6).join.strip
        sentence = if code.include?('SyntaxError')
            'Neo4j versteht die Abfrage nicht. Die Meldung nennt die Stelle (line, column).'
        elsif code.include?('TransactionTimedOut') || text =~ /timed? ?out|terminated/i
            "Die Abfrage brauchte länger als #{MAX_SECONDS} Sekunden. Ein Muster ohne Richtung oder ohne Label kann sehr viele Wege finden."
        elsif text =~ /read access mode/i
            'Diese Abfrage würde die Datenbank verändern. Hier sind nur Abfragen erlaubt, die lesen.'
        elsif code.include?('Security') || code.include?('Unauthorized')
            'Die Anmeldung an deiner Neo4j-Datenbank hat nicht geklappt. Sag deiner Lehrkraft Bescheid.'
        elsif code.include?('DatabaseNotFound')
            "Deine Neo4j-Datenbank gibt es noch nicht. #{setup}".strip
        elsif code == 'unreachable'
            'Der Neo4j-Server ist gerade nicht erreichbar. Versuch es gleich noch einmal.'
        else
            'Neo4j meldet einen Fehler:'
        end
        "#{sentence}\n\n```text\n#{text.gsub('```', "'''")}\n```"
    end

    # --- the check ------------------------------------------------------------

    # The sample data must be there: counts of nodes per label (check
    # 'nodes'), as loaded. Otherwise an empty database would make an empty
    # answer right.
    def self.preflight(check, runner)
        (check['nodes'] || {}).each do |label, wanted|
            next unless label.to_s =~ /\A[A-Za-z_][A-Za-z0-9_]*\z/

            result = runner.call("MATCH (n:`#{label}`) RETURN count(n) AS n")
            return [:unavailable, error_message(result[:error], check['setup']), nil, nil, :unreachable] if result[:error]

            count = result[:rows].first&.first.to_i
            if count.zero?
                return [:unavailable, "In deiner Neo4j-Datenbank gibt es noch keine Knoten mit dem Label `#{label}`. #{check['setup'] || ''}".strip,
                    nil, nil, :no_data]
            end
            if wanted && count != wanted
                return [:unavailable, "In deiner Neo4j-Datenbank gibt es #{count} Knoten `#{label}`, die Filmdaten haben #{wanted}. " \
                    "Hast du etwas verändert? #{check['setup'] || ''}".strip, nil, nil, :no_data]
            end
        end
        nil
    end

    # [status, message, read, note, problem], as SqlCheck.check.
    def self.check(task, check, answer, runner)
        query, refusal = single_read(answer)
        return [:invalid, refusal, nil, nil, :refused] if query.nil?

        stop = preflight(check, runner)
        return stop if stop

        expected = []
        Array(check['expect_query']).each do |reference|
            result = runner.call(reference)
            if result[:error]
                return [:unavailable, error_message(result[:error], check['setup']), nil, nil, :reference_failed]
            end
            expected << SqlCheck.normalize(result)
        end
        if expected.all? { |e| e[:rows].empty? }
            return [:unavailable, "Die Musterlösung findet in deiner Datenbank nichts, vermutlich fehlen die Daten. #{check['setup'] || ''}".strip,
                nil, nil, :no_data]
        end

        result = runner.call(query)
        if result[:error]
            say = SqlCheck.feedback(task, check, query, nil, runner)
            message = error_message(result[:error], check['setup'])
            return [:wrong, say ? "#{say}\n\n#{message}" : message, nil, nil, :cypher_error]
        end

        given = SqlCheck.normalize(result)
        read = SqlCheck.table(:columns => result[:columns], :rows => result[:rows], :truncated => result[:truncated])
        outcomes = expected.map { |e| SqlCheck.compare(check, e, given) }
        hit = outcomes.find { |status, _, _| status == :correct }
        return [:correct, 'Passt.', read, hit[2], nil] if hit

        say = SqlCheck.feedback(task, check, query, given, runner)
        [:wrong, say || outcomes.first[1], read, nil, :result]
    end
end
