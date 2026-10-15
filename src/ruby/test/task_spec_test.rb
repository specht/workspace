require 'minitest/autorun'
require_relative '../include/task_spec'

class TaskSpecTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def minimal(check, extra = {})
        { 'activity' => 'demo', 'tasks' => [{ 'id' => 'eins', 'title' => 'Eins', 'prompt' => 'Frage?', 'check' => check }.merge(extra)] }
    end

    def errors_for(check, extra = {})
        TaskSpec.validate(minimal(check, extra), activity: 'demo')
    end

    # Every tasks.yaml in the content tree must validate. This is the M0
    # test: the tasks from the three sketches fit the format.
    def test_all_content_task_files_are_valid
        files = Dir[File.join(CONTENT, '**', 'tasks.yaml')]
        refute_empty files
        files.each do |path|
            data = TaskSpec.load_file(path)
            assert data['tasks'].size > 0, path
        end
    end

    def test_the_three_pilots_task_counts
        counts = %w[tcpip anaglyph showerjs].to_h do |slug|
            [slug, TaskSpec.load_file(File.join(CONTENT, slug, 'tasks.yaml'))['tasks'].size]
        end
        assert_equal({ 'tcpip' => 18, 'anaglyph' => 12, 'showerjs' => 11 }, counts)
    end

    def test_minimal_answer_task_is_valid
        assert_empty errors_for('type' => 'answer', 'expect' => '42')
    end

    def test_activity_must_match_folder
        errors = TaskSpec.validate(minimal('type' => 'answer', 'expect' => '42'), activity: 'other')
        assert errors.any? { |e| e.include?("activity must be 'other'") }
    end

    def test_duplicate_and_malformed_ids
        data = minimal('type' => 'answer', 'expect' => '42')
        data['tasks'] << data['tasks'].first.dup
        data['tasks'] << data['tasks'].first.merge('id' => 'Großbuchstaben')
        errors = TaskSpec.validate(data, activity: 'demo')
        assert errors.any? { |e| e.include?('duplicate id') }
        assert errors.any? { |e| e.include?('id must match') }
    end

    def test_unknown_check_type_and_keys
        assert errors_for('type' => 'magic').any? { |e| e.include?('check.type must be one of') }
        assert errors_for('type' => 'answer', 'expect' => 'x', 'colour' => 'red').any? { |e| e.include?('unknown keys for answer: colour') }
        assert errors_for({ 'type' => 'answer', 'expect' => 'x' }, 'difficulty' => 3).any? { |e| e.include?('unknown keys: difficulty') }
    end

    def test_choice_correct_must_point_to_an_option
        assert_empty errors_for('type' => 'choice', 'options' => %w[a b], 'correct' => 1)
        refute_empty errors_for('type' => 'choice', 'options' => %w[a b], 'correct' => 2)
        refute_empty errors_for('type' => 'choice', 'options' => %w[a], 'correct' => 0)
    end

    def test_unknown_variable_is_reported
        errors = errors_for('type' => 'answer', 'expect' => '${workspace.password}')
        assert errors.any? { |e| e.include?('unknown variable ${workspace.password}') }
        assert_empty errors_for('type' => 'answer', 'expect' => '${workspace.ip}')
    end

    def test_regexes_are_checked
        refute_empty errors_for('type' => 'answer', 'compare' => 'regex', 'expect' => '^(file')
        refute_empty errors_for('type' => 'command', 'output' => { 'matching' => '[' })
    end

    def test_paths_stay_inside_the_workspace
        refute_empty errors_for('type' => 'files', 'path' => '/etc/passwd', 'expect' => { 'exists' => true })
        refute_empty errors_for('type' => 'files', 'path' => '../other', 'expect' => { 'exists' => true })
        refute_empty errors_for('type' => 'html', 'file' => '~/index.html', 'expect' => { 'parses' => true })
        assert_empty errors_for('type' => 'files', 'path' => 'shower.js/pictures', 'expect' => { 'exists' => true })
    end

    def test_sizes_are_parsed
        assert_equal 500_000, TaskSpec.parse_size('500kB')
        assert_equal 2_000_000, TaskSpec.parse_size('2MB')
        assert_nil TaskSpec.parse_size('viel')
        refute_empty errors_for('type' => 'files', 'path' => 'p', 'expect' => { 'max_size' => 'viel' })
    end

    def test_feedback_conditions_depend_on_the_check_type
        ok = errors_for({ 'type' => 'number', 'expect' => 5 }, 'feedback' => [{ 'when' => { 'between' => [1, 2] }, 'say' => 'Fast.' }])
        assert_empty ok
        bad = errors_for({ 'type' => 'number', 'expect' => 5 }, 'feedback' => [{ 'when' => { 'picked' => 1 }, 'say' => 'Fast.' }])
        assert bad.any? { |e| e.include?('number knows equals, between') }
    end

    def test_network_checks_cannot_be_used_in_exams
        check = { 'type' => 'measure', 'fields' => [{ 'label' => 'x', 'run' => 'ping -c 1 8.8.8.8', 'extract' => 'ping_rtt_avg' }] }
        assert_empty errors_for(check)
        assert errors_for(check, 'exam' => true).any? { |e| e.include?('cannot be used in exams') }
    end

    def test_prediction_only_for_closed_answers
        assert_empty errors_for({ 'type' => 'choice', 'options' => %w[a b], 'correct' => 0 }, 'prediction' => true)
        refute_empty errors_for({ 'type' => 'reflection' }, 'prediction' => true)
    end

    def test_git_check_only_reads_and_knows_its_expectations
        assert_empty errors_for('type' => 'git', 'repo' => 'data-structures-sandbox', 'expect' => { 'has_remote' => true, 'committed' => true, 'pushed' => true })
        refute_empty errors_for('type' => 'git', 'repo' => 'data-structures-sandbox', 'expect' => { 'pushed' => false })
        refute_empty errors_for('type' => 'git', 'repo' => 'data-structures-sandbox', 'expect' => { 'force_push' => true })
        assert_empty errors_for('type' => 'git', 'expect' => { 'identity' => true })
        refute_empty errors_for('type' => 'git', 'expect' => { 'identity' => true, 'pushed' => true })
        refute_empty errors_for('type' => 'git', 'expect' => { 'identity' => false })
    end

    def test_check_version_ignores_wording_but_not_grading
        task = minimal('type' => 'number', 'expect' => 5)['tasks'].first
        reworded = task.merge('prompt' => 'Andere Frage?', 'hint' => 'Tipp')
        regraded = task.merge('check' => { 'expect' => 6, 'type' => 'number' })
        reordered = task.merge('check' => { 'expect' => 5, 'type' => 'number' })
        assert_equal TaskSpec.check_version(task), TaskSpec.check_version(reworded)
        assert_equal TaskSpec.check_version(task), TaskSpec.check_version(reordered)
        refute_equal TaskSpec.check_version(task), TaskSpec.check_version(regraded)
    end
end
