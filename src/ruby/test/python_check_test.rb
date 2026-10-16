require 'minitest/autorun'
require 'tmpdir'
require 'open3'
require 'yaml'
require_relative '../include/python_check'
require_relative '../include/task_spec'

# Runs the harness with the local python3, in a folder like /workspace.
class PythonCheckTest < Minitest::Test
    LOCK = <<~PY
        START = 'start'
        ACCEPTING = {'1213'}
        TRANSITIONS = {
            ('start', '1'): '1',
            ('1', '2'): '12',
            ('12', '1'): '121',
            ('121', '3'): '1213',
        }


        def opens(digits):
            state = START
            for digit in digits:
                state = TRANSITIONS.get((state, digit), START)
            return state in ACCEPTING


        print('Das sieht die Prüfung nicht.')
    PY

    CHECK = YAML.safe_load(<<~YAML)
        type: python
        file: automata/lock.py
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
        File.write(File.join(@dir, 'automata', 'lock.py'), source) if source
        output, _status = Open3.capture2e('sh', '-c', PythonCheck.command(check['file'], PythonCheck.cases(check)), :chdir => @dir)
        PythonCheck.evaluate({}, check, output)
    end

    def test_cases_in_both_forms
        cases = PythonCheck.cases(CHECK)
        assert_equal ["opens('1213')", 'True', nil], cases[0].values_at('call', 'expect', 'say')
        assert_equal ["opens('121213')", 'True'], cases[2].values_at('call', 'expect')
        assert_match(/noch 12/, cases[2]['say'])
    end

    def test_names_the_first_failing_call_and_its_hint
        status, message, read = run_check(LOCK)
        assert_equal :wrong, status
        assert_includes message, "`opens('121213')` ergibt `False`, richtig wäre `True`."
        assert_includes message, 'noch 12'
        assert_includes read, "<i class='bi bi-check-lg'></i> `opens('1213')` → `True`"
        assert_includes read, "<i class='bi bi-x-lg'></i> `opens('121213')` → `False`"
        refute_includes read.to_s, 'sieht die Prüfung'
    end

    def test_correct_when_every_call_fits
        fixed = LOCK.sub("('121', '3'): '1213',", "('121', '3'): '1213',\n    ('121', '2'): '12',")
        status, message, = run_check(fixed)
        assert_equal :correct, status
        assert_equal 'Alle 3 Aufrufe liefern, was sie sollen.', message
    end

    def test_syntax_error_with_line
        status, message, = run_check("def opens(digits)\n    return True\n")
        assert_equal :wrong, status
        assert_match(/Zeile 1/, message)
        assert_match(/Syntaxfehler/, message)
    end

    def test_error_while_loading_with_line
        status, message, = run_check("x = 1\nprint(undefined_name)\n")
        assert_equal :wrong, status
        assert_match(/Schon beim Laden.*Zeile 2.*NameError/, message)
    end

    def test_error_in_a_call
        status, message, = run_check("def opens(digits):\n    return digits[10] == '3'\n")
        assert_equal :wrong, status
        assert_match(/`opens\('1213'\)` bricht ab \(Zeile 2\): `IndexError/, message)
    end

    def test_missing_function
        status, message, = run_check("START = 'start'\n")
        assert_equal :wrong, status
        assert_match(/NameError: name 'opens' is not defined/, message)
    end

    def test_missing_file
        status, message, = run_check(nil)
        assert_equal :wrong, status
        assert_equal '`automata/lock.py` gibt es nicht.', message
    end

    def test_timeout_and_garbage
        status, message, = PythonCheck.evaluate({}, CHECK, '', timed_out: true)
        assert_equal :wrong, status
        assert_match(/Endlosschleife/, message)
        status, message, read = PythonCheck.evaluate({}, CHECK, "python3: not found\n")
        assert_equal :wrong, status
        assert_match(/ließ sich nicht prüfen/, message)
        assert_includes read, 'python3: not found'
    end

    def test_a_call_that_must_raise
        check = YAML.safe_load(<<~YAML)
            type: python
            file: automata/lock.py
            cases:
              - parse('(1)'): "'(1)'"
              - call: parse('(1')
                raises: true
                say: Eine Klammer, die nie zugeht, ist ein Fehler.
        YAML
        status, _message, read = run_check("def parse(text):\n    if text.count('(') != text.count(')'):\n        raise ValueError('Klammer fehlt')\n    return text\n", check)
        assert_equal :correct, status
        assert_match(/Klammer fehlt/, read)
        status, message, = run_check("def parse(text):\n    return text\n", check)
        assert_equal :wrong, status
        assert_equal "`parse('(1')` ergibt `'(1'`, richtig wäre ein Fehler. Eine Klammer, die nie zugeht, ist ein Fehler.", message
        assert_empty PythonCheck.spec_errors(check)
        assert_includes PythonCheck.spec_errors(check.merge('cases' => [{ 'call' => 'f()', 'raises' => 'ja' }])), 'cases[0]: raises must be true'
    end

    def test_spec
        task = { 'id' => 'schloss', 'title' => 'Schloss', 'prompt' => 'x', 'check' => CHECK }
        assert_empty TaskSpec.validate({ 'activity' => 'automata', 'tasks' => [task] }, activity: 'automata')
        bad = CHECK.merge('file' => 'automata/lock.rb', 'cases' => [{ 'call' => 'opens()' }, 'opens'])
        errors = TaskSpec.validate({ 'activity' => 'automata', 'tasks' => [task.merge('check' => bad)] }, activity: 'automata')
        assert errors.any? { |e| e.include?('file must end in .py') }
        assert errors.any? { |e| e.include?('cases[0] needs call and expect') }
        assert errors.any? { |e| e.include?('cases[1] must be') }
    end
end
