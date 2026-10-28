require 'minitest/autorun'
require_relative '../include/schema_check'
require_relative '../include/task_spec'

class SchemaCheckTest < Minitest::Test
    COLUMNS = [
        %w[genre id int], %w[genre name varchar(255)],
        %w[movie id int], %w[movie title varchar(255)], %w[movie year int],
        %w[movie_genre movie_id int], %w[movie_genre genre_id int],
    ].freeze
    PRIMARY = [%w[genre id], %w[movie id], %w[movie_genre movie_id], %w[movie_genre genre_id]].freeze
    FOREIGN = [
        %w[movie_genre movie_genre_ibfk_1 movie_id movie id],
        %w[movie_genre movie_genre_ibfk_2 genre_id genre id],
    ].freeze
    ENV_ = { :login => 'u_anna', :database => 'u_anna', :host => 'mysql' }.freeze

    CHECK = {
        'type' => 'schema',
        'tables' => [
            { 'name' => 'movie', 'columns' => %w[id title year], 'primary_key' => ['id'], 'min_rows' => 1 },
            { 'name' => 'movie_genre', 'primary_key' => %w[movie_id genre_id],
              'foreign_keys' => ['movie_id -> movie.id', 'genre_id -> genre'] },
        ],
        'setup' => 'Lege die Tabellen mit `mysql < film.sql` an.',
    }.freeze

    INDEXES = [%w[movie PRIMARY id], %w[movie by_title title], %w[movie by_title year]].freeze

    def runner(columns: COLUMNS, primary: PRIMARY, foreign: FOREIGN, indexes: INDEXES, counts: { 'movie' => 2740 }, seen: [])
        lambda do |sql|
            seen << sql
            case sql
            when SchemaCheck::COLUMNS then { :columns => [], :rows => columns }
            when SchemaCheck::PRIMARY then { :columns => [], :rows => primary }
            when SchemaCheck::FOREIGN then { :columns => [], :rows => foreign }
            when SchemaCheck::INDEXES then { :columns => [], :rows => indexes }
            when /\ASELECT COUNT\(\*\) FROM `(\w+)`\z/ then { :columns => ['COUNT(*)'], :rows => [[counts.fetch($1, 0).to_s]] }
            else raise "unexpected #{sql}"
            end
        end
    end

    def test_a_good_database_passes_and_the_read_shows_it
        seen = []
        status, message, read, _, problem = SchemaCheck.check({}, CHECK, runner(:seen => seen), ENV_)
        assert_equal :correct, status, message
        assert_nil problem
        assert_includes read, '`movie`: **id INT**, title VARCHAR(255), year INT; Index by_title (title, year); 2740 Zeilen'
        assert_includes read, 'Fremdschlüssel movie_id → movie(id), genre_id → genre(id)'
        assert_equal ['SELECT COUNT(*) FROM `movie`'], seen.grep(/COUNT/), 'counts only where asked'
    end

    def test_no_tables_names_the_setup
        status, message, _, _, problem = SchemaCheck.check({}, CHECK, runner(:columns => [], :primary => [], :foreign => []), ENV_)
        assert_equal [:wrong, :no_tables], [status, problem]
        assert_includes message, '`u_anna` gibt es noch keine Tabellen'
        assert_includes message, 'mysql < film.sql'
    end

    def test_problems
        _, message, _, _, problem = SchemaCheck.check({}, CHECK, runner(:columns => COLUMNS.reject { |r| r[1] == 'year' }), ENV_)
        assert_equal :missing_column, problem
        assert_includes message, 'Die Tabelle `movie` hat noch keine Spalte `year`'

        _, message, _, _, problem = SchemaCheck.check({}, CHECK, runner(:foreign => FOREIGN.first(1)), ENV_)
        assert_equal :foreign_key, problem
        assert_includes message, 'genre_id → genre'

        _, message, _, _, problem = SchemaCheck.check({}, CHECK, runner(:primary => PRIMARY.first(3)), ENV_)
        assert_equal :primary_key, problem
        assert_includes message, 'ist `movie_id`, verlangt ist `movie_id` und `genre_id`'

        _, message, _, _, problem = SchemaCheck.check({}, CHECK, runner(:counts => {}), ENV_)
        assert_equal :rows, problem
        assert_includes message, '`movie` ist noch leer'

        _, message, _, _, problem = SchemaCheck.check({}, CHECK, runner(:columns => COLUMNS.map { |r| r[0] == 'movie' ? ['Movie', *r[1..]] : r }), ENV_)
        assert_equal :missing_table, problem
        assert_includes message, 'Es gibt aber `Movie`'
    end

    def test_unreachable_database_is_unavailable
        status, message, read, = SchemaCheck.check({}, CHECK, ->(_sql) { { :error => { :code => 2005, :message => "Unknown MySQL server host 'mysql'" } } }, ENV_)
        assert_equal :unavailable, status
        refute_empty message
        assert_includes read, '`u_anna`'
        status, = SchemaCheck.check({}, CHECK, nil, ENV_)
        assert_equal :unavailable, status
    end

    def test_spec
        task = { 'id' => 'tabellen', 'title' => 'Tabellen', 'prompt' => 'Los', 'check' => CHECK }
        assert_equal [], TaskSpec.validate_task(task)
        bad = { 'type' => 'schema', 'tables' => [{ 'name' => 'movie', 'foreign_keys' => ['genre_id genre'], 'min_rows' => -1 }] }
        errors = TaskSpec.validate_task(task.merge('check' => bad))
        assert(errors.any? { |e| e.include?('foreign_keys must look like') })
        assert(errors.any? { |e| e.include?('min_rows') })
    end

    # indexes: each list must be the start of an index, the primary key counts
    def test_indexes
        with = ->(*cols) { CHECK.merge('tables' => [{ 'name' => 'movie', 'indexes' => [cols] }]) }
        assert_equal :correct, SchemaCheck.check({}, with.('title'), runner, ENV_).first
        assert_equal :correct, SchemaCheck.check({}, with.('title', 'year'), runner, ENV_).first
        assert_equal :correct, SchemaCheck.check({}, with.('id'), runner, ENV_).first
        status, message, read, _, problem = SchemaCheck.check({}, with.('year'), runner, ENV_)
        assert_equal [:wrong, :index], [status, problem]
        assert_includes message, 'keinen Index, der mit `year` anfängt'
        assert_includes message, '`by_title (title, year)`'
        assert_includes read, 'Index by_title (title, year)'
        task = { 'id' => 'index', 'title' => 'Index', 'prompt' => 'Los', 'check' => with.('title') }
        assert_equal [], TaskSpec.validate_task(task)
        bad = CHECK.merge('tables' => [{ 'name' => 'movie', 'indexes' => ['title'] }])
        assert(TaskSpec.validate_task(task.merge('check' => bad)).any? { |e| e.include?('indexes must be a list of column lists') })
    end

    # rows_where: rows that meet a condition, counted read-only, for a task
    # that changes data (INSERT, UPDATE, DELETE in »Einführung in SQL«)
    def test_rows_where
        where = lambda do |counts, seen = []|
            lambda do |sql|
                seen << sql
                if (m = sql.match(/\ASELECT COUNT\(\*\) FROM `movie` WHERE (.*)\z/m))
                    next { :error => { :code => 1054, :message => "Unknown column 'x'" } } if m[1] == 'x = 1'

                    next { :columns => ['COUNT(*)'], :rows => [[counts.fetch(m[1]).to_s]] }
                end
                runner.call(sql)
            end
        end
        check = CHECK.merge('tables' => [{ 'name' => 'movie', 'rows_where' => [
            { 'where' => "title = 'Heat'", 'rows' => 1, 'say' => 'Heat fehlt noch.' },
            { 'where' => 'year = 1995', 'rows' => 1 },
        ] }])
        seen = []
        status, message, read, _, problem = SchemaCheck.check({}, check, where.({ "title = 'Heat'" => 1, 'year = 1995' => 1 }, seen), ENV_)
        assert_equal :correct, status, message
        assert_nil problem
        assert_includes seen, "SELECT COUNT(*) FROM `movie` WHERE title = 'Heat'"
        assert_includes read, "- `movie` mit `title = 'Heat'`: eine Zeile"
        assert read.index('Gezählt') < read.index('Gelesen'), 'what was counted comes first'

        status, message, _, _, problem = SchemaCheck.check({}, check, where.({ "title = 'Heat'" => 0, 'year = 1995' => 1 }), ENV_)
        assert_equal [:wrong, :rows_where, 'Heat fehlt noch.'], [status, problem, message]
        _, message, read, = SchemaCheck.check({}, check, where.({ "title = 'Heat'" => 1, 'year = 1995' => 12 }), ENV_)
        assert_equal 'Die Tabelle `movie` hat 12 Zeilen mit `year = 1995`, erwartet ist eine.', message
        assert_includes read, '`year = 1995`: 12 Zeilen'

        # a task's own feedback for the problem comes before the default text
        task = { 'feedback' => [{ 'when' => { 'problem' => 'rows_where' }, 'say' => 'Zähl nach.' }] }
        assert_equal 'Zähl nach.', SchemaCheck.check(task, check, where.({ "title = 'Heat'" => 1, 'year = 1995' => 2 }), ENV_)[1]

        # a condition MySQL cannot run is the task's fault, not the student's
        broken = CHECK.merge('tables' => [{ 'name' => 'movie', 'rows_where' => [{ 'where' => 'x = 1', 'rows' => 0 }] }])
        status, _, read, _, problem = SchemaCheck.check({}, broken, where.({}), ENV_)
        assert_equal [:unavailable, :reference_failed], [status, problem]
        assert_includes read, "Unknown column 'x'"

        # no table, no counting
        seen = []
        status, _, _, _, problem = SchemaCheck.check({}, check.merge('tables' => [{ 'name' => 'film', 'rows_where' => [{ 'where' => 'a = 1', 'rows' => 1 }] }]), where.({}, seen), ENV_)
        assert_equal [:wrong, :missing_table], [status, problem]
        assert_empty seen.grep(/WHERE a = 1/)
    end

    def test_rows_where_spec
        task = { 'id' => 'weimar', 'title' => 'Weimar', 'prompt' => 'Los',
                 'check' => { 'type' => 'schema', 'tables' => [{ 'name' => 'city', 'rows_where' => [{ 'where' => "name = 'Weimar'", 'rows' => 1, 'say' => 'Noch nicht.' }] }] } }
        assert_equal [], TaskSpec.validate_task(task)
        bad = lambda do |rules|
            TaskSpec.validate_task(task.merge('check' => { 'type' => 'schema', 'tables' => [{ 'name' => 'city', 'rows_where' => rules }] }))
        end
        assert(bad.([]).any? { |e| e.include?('rows_where must be a list') })
        assert(bad.([{ 'where' => 'a = 1; DROP TABLE city', 'rows' => 1 }]).any? { |e| e.include?('without ;') })
        assert(bad.([{ 'where' => 'a = 1', 'rows' => -1 }]).any? { |e| e.include?('rows must be a whole number') })
        assert(bad.([{ 'where' => 'a = 1', 'rows' => 1, 'min_rows' => 1 }]).any? { |e| e.include?('unknown keys: min_rows') })
        # a model has no rows: er does not take it
        assert(TableExpect.spec_errors([{ 'name' => 'city', 'rows_where' => [{ 'where' => 'a = 1', 'rows' => 1 }] }], :rows => false).any? { |e| e.include?('unknown keys: rows_where') })
    end
    # absent_columns: columns that must be gone, for a table split up (3NF
    # in »Filmdatenbank«, where the director's columns move to their own table)
    def test_absent_columns
        check = CHECK.merge('tables' => [{ 'name' => 'movie', 'columns' => %w[id title], 'absent_columns' => %w[year director] }])
        status, message, _, _, problem = SchemaCheck.check({}, check, runner, ENV_)
        assert_equal [:wrong, :absent_column], [status, problem]
        assert_equal 'Die Tabelle `movie` hat noch die Spalte `year`; sie gehört nicht mehr hinein.', message
        columns = COLUMNS + [%w[movie director varchar(255)]]
        _, message, = SchemaCheck.check({}, check, runner(:columns => columns), ENV_)
        assert_equal 'Die Tabelle `movie` hat noch die Spalten `year` und `director`; sie gehören nicht mehr hinein.', message
        status, = SchemaCheck.check({}, CHECK.merge('tables' => [{ 'name' => 'movie', 'absent_columns' => ['director'] }]), runner, ENV_)
        assert_equal :correct, status

        task = { 'id' => 'split', 'title' => 'Split', 'prompt' => 'Los', 'check' => check }
        assert_equal [], TaskSpec.validate_task(task)
        bad = check.merge('tables' => [{ 'name' => 'movie', 'absent_columns' => 'director' }])
        assert(TaskSpec.validate_task(task.merge('check' => bad)).any? { |e| e.include?('absent_columns must be a list of column names') })
    end
end
