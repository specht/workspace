require 'minitest/autorun'
require 'tmpdir'
require 'open3'
require 'yaml'
require_relative '../include/ruby_check'
require_relative '../include/task_spec'

# Runs the harness with the local ruby, in a folder like /workspace.
class RubyCheckTest < Minitest::Test
    LOCK = <<~RB
        START = 'start'
        ACCEPTING = ['1213']
        TRANSITIONS = {
            ['start', '1'] => '1',
            ['1', '2'] => '12',
            ['12', '1'] => '121',
            ['121', '3'] => '1213',
        }

        def opens(digits)
            state = START
            digits.each_char do |digit|
                state = TRANSITIONS.fetch([state, digit], START)
            end
            ACCEPTING.include?(state)
        end

        puts 'Das sieht die Prüfung nicht.'

        if __FILE__ == $0
            raise 'Das läuft beim Prüfen nicht.'
        end
    RB

    CHECK = YAML.safe_load(<<~YAML)
        type: ruby
        file: automata/lock.rb
        cases:
          - opens('1213'): true
          - opens('12'): false
          - call: opens('121213')
            expect: true
            say: Nach 121 und einer 2 stimmt vom Code noch 12.
    YAML

    def setup
        @dir = Dir.mktmpdir
        Dir.mkdir(File.join(@dir, 'automata'))
    end

    def teardown
        FileUtils.rm_rf(@dir)
    end

    def run_check(source, check = CHECK)
        File.write(File.join(@dir, 'automata', 'lock.rb'), source) if source
        output, _status = Open3.capture2e('sh', '-c', RubyCheck.command(check['file'], RubyCheck.cases(check)), :chdir => @dir)
        RubyCheck.evaluate({}, check, output)
    end

    def test_cases_in_both_forms
        cases = RubyCheck.cases(CHECK)
        assert_equal ["opens('1213')", 'true', nil], cases[0].values_at('call', 'expect', 'say')
        assert_equal ["opens('121213')", 'true'], cases[2].values_at('call', 'expect')
        assert_match(/noch 12/, cases[2]['say'])
    end

    def test_names_the_first_failing_call_and_its_hint
        status, message, read = run_check(LOCK)
        assert_equal :wrong, status
        assert_includes message, "`opens('121213')` ergibt `false`, richtig wäre `true`."
        assert_includes message, 'noch 12'
        assert_includes read, "<i class='bi bi-check-lg'></i> `opens('1213')` → `true`"
        assert_includes read, "<i class='bi bi-x-lg'></i> `opens('121213')` → `false`"
        refute_includes read.to_s, 'sieht die Prüfung'
    end

    def test_correct_when_every_call_fits
        fixed = LOCK.sub("['121', '3'] => '1213',", "['121', '3'] => '1213',\n    ['121', '2'] => '12',")
        status, message, = run_check(fixed)
        assert_equal :correct, status
        assert_equal 'Alle 3 Aufrufe liefern, was sie sollen.', message
    end

    def test_syntax_error_with_line
        status, message, = run_check("x = 1\ndef opens(digits\n    true\n")
        assert_equal :wrong, status
        assert_match(/Syntaxfehler/, message)
        assert_match(/Zeile \d/, message)
    end

    def test_error_while_loading_with_line
        status, message, = run_check("x = 1\nputs undefined_name\n")
        assert_equal :wrong, status
        assert_match(/Schon beim Laden.*bricht Ruby ab.*Zeile 2.*NameError/, message)
    end

    def test_exit_while_loading
        status, message, = run_check("exit 1\n")
        assert_equal :wrong, status
        assert_match(/Schon beim Laden.*SystemExit/, message)
    end

    def test_error_in_a_call
        status, message, = run_check("def opens(digits)\n    digits.fetch(10)\nend\n")
        assert_equal :wrong, status
        assert_match(/`opens\('1213'\)` bricht ab \(Zeile 2\): `NoMethodError/, message)
    end

    def test_missing_method
        status, message, = run_check("START = 'start'\n")
        assert_equal :wrong, status
        assert_match(/NoMethodError: undefined method 'opens'/, message)
    end

    def test_missing_file
        status, message, = run_check(nil)
        assert_equal :wrong, status
        assert_equal '`automata/lock.rb` gibt es nicht.', message
    end

    def test_byte_strings_compare_with_text
        check = YAML.safe_load(<<~YAML)
            type: ruby
            file: automata/lock.rb
            cases:
              - unpack([65, 66]): "'AB'"
        YAML
        status, = run_check("def unpack(bytes)\n    bytes.pack('C*')\nend\n", check)
        assert_equal :correct, status
    end

    def test_spec
        task = { 'id' => 'schloss', 'title' => 'Schloss', 'prompt' => 'x', 'check' => CHECK }
        assert_empty TaskSpec.validate({ 'activity' => 'automata', 'tasks' => [task] }, activity: 'automata')
        bad = CHECK.merge('file' => 'automata/lock.py', 'cases' => [{ 'call' => 'opens()' }, 'opens'])
        errors = TaskSpec.validate({ 'activity' => 'automata', 'tasks' => [task.merge('check' => bad)] }, activity: 'automata')
        assert errors.any? { |e| e.include?('file must end in .rb') }
        assert errors.any? { |e| e.include?('cases[0] needs call and expect') }
        assert errors.any? { |e| e.include?('cases[1] must be') }
    end
end
