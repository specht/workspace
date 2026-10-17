require 'minitest/autorun'
require 'bigdecimal'
require_relative '../include/task_check'

class SqlCheckTest < Minitest::Test
    S = SqlCheck

    # --- what may run -----------------------------------------------------------

    def test_single_select
        assert_equal ['SELECT * FROM land', nil], S.single_select("SELECT * FROM land;\n")
        assert_equal ['select name from land', nil], S.single_select('select name from land')
        assert_equal ["WITH e AS (SELECT * FROM land) SELECT * FROM e", nil], S.single_select("WITH e AS (SELECT * FROM land) SELECT * FROM e;")
        assert_equal ['(SELECT 1)', nil], S.single_select('(SELECT 1);')
        # ; inside strings and comments does not count.
        assert_equal ["SELECT * FROM land WHERE name = 'a;b'", nil], S.single_select("SELECT * FROM land WHERE name = 'a;b';")
        assert_equal ["SELECT 1 -- egal; wirklich", nil], S.single_select("SELECT 1 -- egal; wirklich")
        assert_equal ["SELECT 'it''s'", nil], S.single_select("SELECT 'it''s';")
        assert_equal ["SELECT 'a\\';b'", nil], S.single_select("SELECT 'a\\';b';")
        # Comments before the SELECT are fine.
        assert_equal ["-- Aufgabe 1\nSELECT 1", nil], S.single_select("-- Aufgabe 1\nSELECT 1;")
    end

    def test_only_one_select
        assert_includes S.single_select('')[1], 'Schreib zuerst'
        assert_includes S.single_select('SELECT 1; DROP TABLE land;')[1], 'Eine Abfrage reicht'
        assert_includes S.single_select('DROP TABLE land')[1], 'nicht `DROP`'
        assert_includes S.single_select('delete from land')[1], 'nicht `DELETE`'
        assert_includes S.single_select("UPDATE land SET name = 'x'")[1], 'nur eine Abfrage mit `SELECT`'
        assert_includes S.single_select('/*! DROP TABLE land */ SELECT 1')[1], '/*!'
        assert_includes S.single_select("SELECT * FROM land INTO OUTFILE '/tmp/x'")[1], 'OUTFILE'
        assert_includes S.single_select("SELECT 'offen")[1], 'nicht geschlossen'
        assert_includes S.single_select('SELECT 1 /* offen')[1], 'nicht geschlossen'
        # A string that merely mentions DROP is fine.
        assert_nil S.single_select("SELECT 'DROP TABLE land'")[1]
    end

    def test_values
        assert_equal '8.5', S.value(BigDecimal('8.50'))
        assert_equal '8', S.value(BigDecimal('8.00'))
        assert_equal '197', S.value(197)
        assert_equal '0.333333', S.value(1.0 / 3)
        assert_equal '45', S.value('45')
        assert_equal 'Wien', S.value('Wien')
        assert_nil S.value(nil)
    end

    # --- comparing --------------------------------------------------------------

    def res(columns, *rows)
        S.normalize(:columns => columns, :rows => rows)
    end

    def test_same_rows_any_column_order
        e = res(%w[Name Hauptstadt], %w[Belgien Brüssel], %w[Österreich Wien])
        assert_equal :correct, S.compare({}, e, res(%w[Hauptstadt Name], %w[Wien Österreich], %w[Brüssel Belgien]))[0]
        status, message, = S.compare({}, e, res(%w[Name], %w[Belgien], %w[Österreich]))
        assert_equal :wrong, status
        assert_includes message, '1 Spalte, gebraucht werden 2 (`Name`, `Hauptstadt`)'
    end

    def test_extra_columns
        e = res(%w[Name], ['Armenien'], ['Elfenbeinküste'])
        g = res(%w[LNR Name Einwohner], ['AM', 'Armenien', BigDecimal('3.34')], ['CI', 'Elfenbeinküste', BigDecimal('20.3')])
        # Strict by default.
        status, message, = S.compare({}, e, g)
        assert_equal :wrong, status
        assert_equal 'Die Zeilen stimmen, aber die Spalten `LNR`, `Einwohner` gehören nicht dazu. Schreib hinter `SELECT` nur die Spalten, nach denen gefragt ist.', message
        status, _, note = S.compare({ 'extra_columns' => 'note' }, e, g)
        assert_equal :correct, status
        assert_equal 'Für die Antwort brauchst du die Spalten `LNR`, `Einwohner` nicht.', note
        assert_nil S.compare({ 'extra_columns' => 'ok' }, e, g)[2]
        # SELECT *: one sentence, not a list of every column.
        wide = res(%w[a b c d e Name], ['1', '2', '3', '4', '5', 'Armenien'], ['1', '2', '3', '4', '6', 'Elfenbeinküste'])
        assert_includes S.compare({}, e, wide)[1], 'Deine Abfrage liefert 6 Spalten, gefragt ist nur eine.'
    end

    def test_row_counts_and_differences
        e = res(%w[Name], ['Armenien'], ['Elfenbeinküste'])
        assert_includes S.compare({}, e, res(%w[Name], ['Armenien']))[1], 'liefert 1 Zeile, richtig sind 2 Zeilen'
        assert_includes S.compare({}, e, res(%w[Name]))[1], 'keine Zeilen'
        message = S.compare({}, e, res(%w[Name], ['Armenien'], ['Argentinien']))[1]
        assert_includes message, '1 Zeile deines Ergebnisses gehört nicht dazu, zum Beispiel (Argentinien).'
        assert_includes message, '1 Zeile fehlt.'
        # A single value is named, nothing more.
        assert_equal 'Dein Ergebnis ist 44. Das stimmt noch nicht.', S.compare({}, res(['COUNT(*)'], [45]), res(['COUNT(*)'], [44]))[1]
        # Same value under another column name and type.
        assert_equal :correct, S.compare({}, res(['COUNT(*)'], [45]), res(['anzahl'], [BigDecimal('45')]))[0]
    end

    def test_duplicates_count
        e = res(%w[Kontinent], ['Europa'], ['Asien'])
        assert_equal :wrong, S.compare({}, e, res(%w[Kontinent], ['Europa'], ['Europa']))[0]
    end

    def test_sorted_by_allows_ties
        e = res(%w[Name Einwohner], ['C', 3], ['A', 1], ['B', 1])
        check = { 'sorted_by' => '2 DESC' }
        assert_equal :correct, S.compare(check, e, res(%w[Name Einwohner], ['C', 3], ['B', 1], ['A', 1]))[0]
        status, message, = S.compare(check, e, res(%w[Name Einwohner], ['A', 1], ['C', 3], ['B', 1]))
        assert_equal :wrong, status
        assert_includes message, 'Reihenfolge noch nicht: (A, 1) steht vor (C, 3)'
        # Without sorted_by the order does not matter.
        assert_equal :correct, S.compare({}, e, res(%w[Name Einwohner], ['A', 1], ['C', 3], ['B', 1]))[0]
    end

    def test_subset
        e = res(%w[Name], ['Armant'], ['Armidale'], ['El Carmen'], ['Sarm'])
        check = { 'compare' => 'subset', 'rows' => 3 }
        assert_equal :correct, S.compare(check, e, res(%w[Name], ['Armidale'], ['Sarm'], ['Armant']))[0]
        assert_includes S.compare(check, e, res(%w[Name], ['Armidale']))[1], 'gefragt sind 3'
        assert_includes S.compare(check, e, res(%w[Name], ['Armidale'], ['Armidale'], ['Sarm']))[1], 'mehrfach'
        assert_includes S.compare(check, e, res(%w[Name], ['Armidale'], ['Berlin'], ['Sarm']))[1], '(Berlin)'
    end

    # --- the whole check, with a runner that answers from a table ---------------

    LAND = [
        ['A', 'Österreich', BigDecimal('8.51'), 'Europa'],
        ['B', 'Belgien', BigDecimal('11.27'), 'Europa'],
        ['D', 'Deutschland', BigDecimal('80.6'), 'Europe'],
        ['AM', 'Armenien', BigDecimal('3.34'), 'Asien'],
    ].freeze

    def runner(answers)
        calls = []
        r = lambda do |sql|
            calls << sql
            answers.fetch(sql) { { :error => { :code => 1064, :message => "You have an error in your SQL syntax; check the manual near '#{sql[0, 10]}' at line 1" } } }
        end
        [r, calls]
    end

    def task
        {
            'id' => 'europa', 'title' => 'Europa',
            'check' => {
                'type' => 'sql',
                'expect_query' => "SELECT COUNT(*) FROM land WHERE Kontinent IN ('Europa', 'Europe')",
                'tables' => { 'land' => 4 },
                'setup' => 'Lade zuerst die Beispieldaten: `mysql < countries.sql`.',
            },
            'feedback' => [
                { 'when' => { 'same_result_as' => "SELECT COUNT(*) FROM land WHERE Kontinent = 'Europa'" },
                  'say' => 'Du hast nur »Europa« gezählt.' },
                { 'when' => { 'query_matches' => 'kontinent\s*=\s*.europe.' }, 'say' => 'Nur »Europe«?' },
            ],
        }
    end

    def answers(extra = {})
        {
            'SELECT COUNT(*) FROM `land`' => { :columns => ['COUNT(*)'], :rows => [[4]] },
            "SELECT COUNT(*) FROM land WHERE Kontinent IN ('Europa', 'Europe')" => { :columns => ['COUNT(*)'], :rows => [[3]] },
            "SELECT COUNT(*) FROM land WHERE Kontinent = 'Europa'" => { :columns => ['COUNT(*)'], :rows => [[2]] },
        }.merge(extra)
    end

    def test_check_correct
        r, calls = runner(answers("SELECT COUNT(name) FROM land WHERE kontinent LIKE 'Europ%'" => { :columns => ['COUNT(name)'], :rows => [[3]] }))
        result = TaskCheck.check(task, "SELECT COUNT(name) FROM land WHERE kontinent LIKE 'Europ%';", sql: r)
        assert_equal :correct, result.status
        assert_includes result.read, "Dein Ergebnis: 1 Zeile.\n\n| COUNT(name) |\n| --- |\n| 3 |"
        assert_equal 3, calls.size
    end

    def test_check_feedback
        r, = runner(answers("SELECT COUNT(*) FROM land WHERE kontinent = 'Europa'" => { :columns => ['COUNT(*)'], :rows => [[2]] }))
        result = TaskCheck.check(task, "SELECT COUNT(*) FROM land WHERE kontinent = 'Europa'", sql: r)
        assert_equal :wrong, result.status
        assert_equal 'Du hast nur »Europa« gezählt.', result.message
        r, = runner(answers("SELECT COUNT(*) FROM land WHERE Kontinent = 'Europe'" => { :columns => ['COUNT(*)'], :rows => [[1]] }))
        assert_equal 'Nur »Europe«?', TaskCheck.check(task, "SELECT COUNT(*) FROM land WHERE Kontinent = 'Europe'", sql: r).message
    end

    def test_check_sql_error_counts
        r, = runner(answers)
        result = TaskCheck.check(task, 'SELECT COUNT(*) FORM land', sql: r)
        assert_equal :wrong, result.status
        assert_equal :sql_error, result.problem
        assert_includes result.message, 'MySQL versteht die Abfrage nicht'
        assert_includes result.message, "```\nYou have an error in your SQL syntax"
    end

    def test_check_refuses_writes_without_running
        r, calls = runner(answers)
        result = TaskCheck.check(task, 'DELETE FROM land', sql: r)
        assert_equal :invalid, result.status
        assert_empty calls
    end

    def test_check_needs_data
        r, = runner(answers('SELECT COUNT(*) FROM `land`' => { :error => { :code => 1146, :message => "Table 'anna.land' doesn't exist" } }))
        result = TaskCheck.check(task, 'SELECT 1', sql: r)
        assert_equal :unavailable, result.status
        assert_equal 'In deiner Datenbank gibt es die Tabelle `land` nicht. Lade zuerst die Beispieldaten: `mysql < countries.sql`.', result.message
        # Emptied table: an empty answer must not become right.
        r, = runner(answers('SELECT COUNT(*) FROM `land`' => { :columns => ['COUNT(*)'], :rows => [[0]] }))
        result = TaskCheck.check(task, 'SELECT 1', sql: r)
        assert_equal :data_changed, result.problem
        assert_includes result.message, 'hat 0 Zeilen, die Beispieldaten haben 4'
    end

    def test_check_needs_rows_from_the_reference
        r, = runner(answers("SELECT COUNT(*) FROM land WHERE Kontinent IN ('Europa', 'Europe')" => { :columns => ['COUNT(*)'], :rows => [] }))
        result = TaskCheck.check(task, 'SELECT 1', sql: r)
        assert_equal [:unavailable, :no_data], [result.status, result.problem]
        assert_includes result.message, 'Lade zuerst die Beispieldaten'
    end

    def test_check_unreachable
        assert_equal :unavailable, TaskCheck.check(task, 'SELECT 1', sql: nil).status
        r, = runner(answers('SELECT COUNT(*) FROM `land`' => { :error => { :code => 2003, :message => "Can't connect" } }))
        assert_equal :unreachable, TaskCheck.check(task, 'SELECT 1', sql: r).problem
    end

    ENV_ANNA = {
        :login => 'anna', :database => 'anna', :host => 'mysql',
        :workspace => -> { false },
        :my_cnf => -> { { 'user' => 'anna', 'database' => 'anna' } },
    }.freeze

    def test_diagnosis_when_the_table_is_missing
        r, = runner(answers(
            'SELECT COUNT(*) FROM `land`' => { :error => { :code => 1146, :message => "Table 'anna.land' doesn't exist" } },
            'SHOW TABLES' => { :columns => ['Tables_in_anna'], :rows => [['kunden']] },
            "SELECT table_schema, table_name FROM information_schema.tables WHERE table_name IN ('land') AND table_schema <> DATABASE()" =>
                { :columns => %w[table_schema table_name], :rows => [%w[db_x7k land]] },
        ))
        result = TaskCheck.check(task, 'SELECT 1', sql: r, sql_env: ENV_ANNA)
        assert_equal :unavailable, result.status
        assert_equal :no_data, result.problem
        assert_includes result.message, 'In deiner Datenbank `anna` gibt es die Tabelle `land` nicht.'
        read = result.read
        assert_includes read, '- **Datenbank:** `anna` auf `mysql`, angemeldet als `anna`'
        assert_includes read, '- **Tabellen in `anna`:** `kunden`'
        assert_includes read, '- **`land`:** fehlt'
        assert_includes read, '`land` in `db_x7k`. Geprüft wird aber `anna`'
        assert_includes read, '- **Workspace:** läuft gerade nicht.'
        assert_includes read, '- **`/workspace/.my.cnf`:** Datenbank `anna`'
        refute_includes read, 'nicht die geprüfte'
    end

    def test_diagnosis_names_the_login_problem
        r, = runner(answers('SELECT COUNT(*) FROM `land`' => { :error => { :code => 1045, :message => "Access denied for user 'anna'@'172.18.0.5' (using password: YES)" } }))
        env = ENV_ANNA.merge(:workspace => -> { true }, :my_cnf => -> { { 'database' => 'anna_alt' } })
        result = TaskCheck.check(task, 'SELECT 1', sql: r, sql_env: env)
        assert_equal :unreachable, result.problem
        assert_equal 'Die Anmeldung an `anna` klappt nicht.', result.message
        assert_includes result.read, "- **MySQL meldet** (1045):\n\n```\nAccess denied for user 'anna'"
        assert_includes result.read, '- **Workspace:** läuft'
        assert_includes result.read, 'Datenbank `anna_alt`. Das ist nicht die geprüfte Datenbank `anna`'

        r, = runner(answers('SELECT COUNT(*) FROM `land`' => { :error => { :code => 1049, :message => "Unknown database 'anna'" } }))
        assert_equal 'Die Datenbank `anna` gibt es auf dem Datenbankserver nicht.', TaskCheck.check(task, 'SELECT 1', sql: r, sql_env: ENV_ANNA).message
        r, = runner(answers('SELECT COUNT(*) FROM `land`' => { :error => { :code => 2005, :message => "Unknown MySQL server host 'mysql'" } }))
        result = TaskCheck.check(task, 'SELECT 1', sql: r, sql_env: ENV_ANNA.merge(:my_cnf => -> { :missing }))
        assert_includes result.message, 'antwortet gerade nicht'
        assert_includes result.read, "Unknown MySQL server host 'mysql'"
        assert_includes result.read, '`/workspace/.my.cnf`:** fehlt'
        result = TaskCheck.check(task, 'SELECT 1', sql: nil, sql_env: ENV_ANNA)
        assert_includes result.message, 'fehlen die Anmeldedaten'
        assert_includes result.read, '- **Datenbank:** `anna`'
    end

    def test_diagnosis_for_changed_data
        r, = runner(answers(
            'SELECT COUNT(*) FROM `land`' => { :columns => ['COUNT(*)'], :rows => [['0']] },
            'SHOW TABLES' => { :columns => ['Tables_in_anna'], :rows => [['land'], ['ort']] },
        ))
        result = TaskCheck.check(task, 'SELECT 1', sql: r, sql_env: ENV_ANNA)
        assert_equal :data_changed, result.problem
        assert_includes result.read, '- **Tabellen in `anna`:** `land`, `ort`'
        assert_includes result.read, '- **`land`:** 0 Zeilen, die Beispieldaten haben 4'
    end

    def test_table_escapes_cells
        t = S.table(res(%w[a b], ['x|y', nil]))
        assert_includes t, '| x\\|y | *NULL* |'
        # Values come from the student's database: no HTML gets through.
        assert_equal '&lt;script&gt;alert(1)&lt;/script&gt;', S.safe('<script>alert(1)</script>')
        assert_equal "Saint John&#39;s \\*", S.safe("Saint John's *")
        assert_includes S.compare({}, res(['n'], [1]), res(['n'], ['<b>x</b>']))[1], '&lt;b&gt;x&lt;/b&gt;'
        long = S.table(S.normalize(:columns => ['n'], :rows => (1..12).map { |i| [i] }))
        assert_includes long, 'Dein Ergebnis: 12 Zeilen.'
        assert_includes long, '… und 2 Zeilen mehr.'
    end
end
