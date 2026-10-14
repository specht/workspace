require 'minitest/autorun'
require 'fileutils'
require 'json'
require 'tmpdir'
require_relative '../include/keyboard_check'
require_relative '../include/task_check'
require_relative '../include/task_spec'
require_relative '../include/workspace_files'

class KeyboardCheckTest < Minitest::Test
    TASK = { 'id' => 'markieren', 'title' => 'Text markieren',
             'check' => { 'type' => 'keyboard', 'section' => 'Text markieren' } }.freeze

    def setup
        @root = Dir.mktmpdir
    end

    def teardown
        FileUtils.rm_rf(@root)
    end

    def write_state(state)
        FileUtils.mkdir_p(File.join(@root, '.hs-kbd-tutorial'))
        File.write(File.join(@root, KeyboardCheck::STATE_FILE), state.is_a?(String) ? state : JSON.generate(state))
    end

    def run_check(task = TASK)
        TaskCheck.check(task, '', :workspace => WorkspaceFiles.new(@root))
    end

    def keys(heading)
        KeyboardCheck.section(heading)['steps'].map { |s| s['key'] }
    end

    # The extension's own sections.yaml is the list of steps.
    def test_sections_come_from_the_extension
        headings = KeyboardCheck.sections.map { |s| s['heading'] }
        assert_includes headings, 'Die Tastatur'
        assert_includes headings, 'Code bearbeiten'
        assert_equal %w[select-with-mouse select-with-keyboard select-word-line-all], keys('Text markieren')
    end

    def test_all_steps_of_the_section_done
        write_state(keys('Text markieren').to_h { |k| [k, true] }.merge('press-keys' => false))
        result = run_check
        assert_equal :correct, result.status
        assert_includes result.message, 'Alle 3 Schritte'
        assert_includes result.read, '3 von 3'
    end

    def test_open_steps_are_named
        write_state('select-with-mouse' => true, 'select-with-keyboard' => false)
        result = run_check
        assert_equal :wrong, result.status
        assert_equal :incomplete, result.problem
        assert_includes result.message, '1 von 3'
        assert_includes result.message, '»Text mit der Tastatur markieren« und »Wörter, Zeilen und alles markieren«'
    end

    def test_long_lists_are_shortened
        write_state({})
        task = TASK.merge('check' => { 'type' => 'keyboard', 'section' => 'Navigieren im Dokument' })
        message = run_check(task).message
        assert_includes message, '0 von 7'
        assert_includes message, 'und 4 weitere'
    end

    def test_without_state_file_it_says_how_to_open_the_tutorial
        result = run_check
        assert_equal :wrong, result.status
        assert_equal :missing, result.problem
        assert_includes result.message, 'Tastatur-Symbol'
    end

    def test_a_broken_state_file_is_not_a_crash
        write_state('{ nope')
        assert_equal :unreadable, run_check.problem
    end

    def test_spec_names_a_real_section
        assert_empty TaskSpec.validate_check('keyboard', TASK['check'])
        refute_empty TaskSpec.validate_check('keyboard', { 'type' => 'keyboard', 'section' => 'Gibt es nicht' })
        refute_empty TaskSpec.validate_check('keyboard', { 'type' => 'keyboard', 'section' => 'Die Tastatur', 'extra' => 1 })
    end
end
