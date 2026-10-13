require 'minitest/autorun'
require 'fileutils'
require 'open3'
require 'tmpdir'
require_relative '../include/er_check'
require_relative '../include/task_spec'

class ErCheckTest < Minitest::Test
    FILM = <<~ER
        entity movie
          id: int, key
          title: text
          year: int, optional

        entity genre
          id: int, key
          name: text, unique

        relationship has_genre: movie n:m genre
    ER

    CHECK = {
        'type' => 'er', 'file' => 'filmdatenbank/film.er',
        'entities' => [
            { 'name' => 'movie', 'key' => ['id'], 'attributes' => ['title', 'year: int, optional'] },
            { 'name' => 'genre', 'key' => ['id'], 'attributes' => ['name: text, unique'] },
        ],
        'relationships' => [{ 'between' => %w[movie genre], 'cardinality' => 'n:m' }],
        'tables' => [
            'movie', 'genre',
            { 'name' => 'movie_genre', 'columns' => %w[movie_id genre_id], 'primary_key' => %w[movie_id genre_id],
              'foreign_keys' => ['movie_id -> movie.id', 'genre_id -> genre'] },
        ],
    }.freeze

    # Runs the real command (node, the real er-core) on a model, as the
    # Workspace would in /workspace.
    def summary(text)
        skip 'node is not installed' unless system('node --version > /dev/null 2>&1')
        Dir.mktmpdir do |dir|
            FileUtils.mkdir_p(File.join(dir, 'filmdatenbank'))
            File.write(File.join(dir, 'filmdatenbank/film.er'), text) if text
            out, status = Open3.capture2(ErCheck.command('filmdatenbank/film.er'), :chdir => dir)
            assert status.success?, out
            ErCheck.parse(out)
        end
    end

    def evaluate(text, check = CHECK, task = {})
        ErCheck.evaluate(task, check, summary(text))
    end

    def test_the_core_is_found
        assert ErCheck.available?, ErCheck::CORE_PATH
    end

    def test_a_good_model_passes_and_the_read_shows_it
        status, problem, message, read = evaluate(FILM)
        assert_equal :correct, status, message
        assert_nil problem
        assert_includes read, '`movie`: **id**, title, year'
        assert_includes read, '`has_genre: movie n:m genre` → Tabelle `movie_genre`'
        assert_includes read, '`movie_genre`: **movie_id INT**, **genre_id INT**; Fremdschlüssel movie_id → movie(id), genre_id → genre(id)'
    end

    def test_missing_file
        status, problem, = evaluate(nil)
        assert_equal [:wrong, :missing], [status, problem]
    end

    def test_syntax_errors_come_first
        status, problem, message, = evaluate(FILM.sub('title: text', 'title text'))
        assert_equal [:wrong, :errors], [status, problem]
        assert_match(/Zeile 3:/, message)
    end

    def test_missing_entity_with_case_hint
        status, problem, message, = evaluate(FILM.sub('entity genre', 'entity Genre').sub('n:m genre', 'n:m Genre'))
        assert_equal [:wrong, :missing_entity], [status, problem]
        assert_includes message, 'Es gibt `Genre`'
    end

    def test_key_and_attributes
        _, problem, message, = evaluate(FILM.sub('  id: int, key', '  id: int').sub('title: text', 'title: text, key'))
        assert_equal :key, problem
        assert_includes message, 'Der Schlüssel von `movie` ist `title`, verlangt ist `id`'

        _, problem, message, = evaluate(FILM.sub("  year: int, optional\n", ''))
        assert_equal :missing_attribute, problem
        assert_includes message, '`movie` hat noch kein Attribut `year`'

        _, problem, message, = evaluate(FILM.sub('year: int, optional', 'year: int'))
        assert_equal :attribute, problem
        assert_includes message, '`movie.year` darf leer bleiben'

        _, problem, message, = evaluate(FILM.sub('year: int, optional', 'year: text, optional'))
        assert_equal :attribute, problem
        assert_includes message, 'verlangt ist `int`'
    end

    def test_relationships_in_either_order
        status, = evaluate(FILM.sub('movie n:m genre', 'genre n:m movie'), CHECK.merge('tables' => ['genre_movie']))
        assert_equal :correct, status

        _, problem, message, = evaluate(FILM.sub('movie n:m genre', 'movie n:1 genre'))
        assert_equal :cardinality, problem
        assert_includes message, 'ist `n:1`, gefragt ist `n:m`'

        _, problem, = evaluate(FILM.sub(/^relationship.*$/, ''))
        assert_equal :missing_relationship, problem
    end

    def test_one_to_many_read_from_the_other_side
        check = CHECK.merge('relationships' => [{ 'between' => %w[genre movie], 'cardinality' => '1:n' }], 'tables' => ['movie'])
        status, = evaluate(FILM.sub('movie n:m genre', 'movie n:1 genre'), check)
        assert_equal :correct, status
    end

    def test_three_way_relationship_and_its_table
        model = FILM + "\nentity crew\n  id: int, key\n  name: text\n\nentity job\n  id: int, key\n  name: text\n\nrelationship credit: movie, crew, job\n"
        check = CHECK.merge('relationships' => [{ 'between' => %w[job movie crew], 'table' => 'movie_crew' }])
        _, problem, message, = evaluate(model, check)
        assert_equal :missing_table, problem
        assert_includes message, '`as movie_crew`'
        status, = evaluate(model.sub('movie, crew, job', 'movie, crew, job as movie_crew'), check)
        assert_equal :correct, status
    end

    def test_tables_from_the_model
        check = CHECK.merge('tables' => [{ 'name' => 'movie_genre', 'primary_key' => %w[movie_id genre_id] }])
        _, problem, message, = evaluate(FILM.sub('movie n:m genre', 'movie n:m genre as film_genre'), check.merge('relationships' => []))
        assert_equal :missing_table, problem
        assert_includes message, 'Aus deinem Modell entsteht keine Tabelle `movie_genre`'
    end

    def test_feedback_by_problem
        task = { 'feedback' => [{ 'when' => { 'problem' => 'cardinality' }, 'say' => 'Ein Film hat mehrere Genres.' }] }
        _, _, message, = evaluate(FILM.sub('movie n:m genre', 'movie n:1 genre'), CHECK, task)
        assert_equal 'Ein Film hat mehrere Genres.', message
    end

    def test_spec
        task = { 'id' => 'modell', 'title' => 'Modell', 'prompt' => 'Los', 'check' => CHECK }
        assert_equal [], TaskSpec.validate_task(task)
        bad = CHECK.merge('file' => 'film.txt', 'entities' => [{ 'name' => 'movie', 'attributes' => ['year: integer'] }],
            'relationships' => [{ 'between' => %w[a b c], 'cardinality' => '1:n' }], 'tables' => [{ 'name' => 'x', 'rows' => 3 }])
        errors = TaskSpec.validate_task(task.merge('check' => bad))
        assert(errors.any? { |e| e.include?('.er') })
        assert(errors.any? { |e| e.include?('unknown type integer') })
        assert(errors.any? { |e| e.include?('cardinality only for two') })
        assert(errors.any? { |e| e.include?('unknown keys: rows') })
    end
end
