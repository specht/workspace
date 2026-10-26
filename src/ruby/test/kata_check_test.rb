require 'minitest/autorun'
require_relative '../include/kata_check'
require_relative '../include/kata_runner'
require_relative '../include/task_spec'

# The check type function with real runs in the Workspace image; the parts
# that need no container (the spec, a full queue) run everywhere.
class KataCheckTest < Minitest::Test
    RUN = lambda do |files:, argv:, timeout:|
        KataRunner.run(:files => files, :argv => argv, :timeout => timeout)
    end

    def python_check
        { 'type' => 'function', 'language' => 'python', 'cases' => [
            { 'gcd(60, 48)' => 12 },
            { 'call' => 'gcd(7, 7)', 'expect' => 7, 'say' => 'Der größte Teiler einer Zahl mit sich selbst ist sie selbst.' },
        ] }
    end

    def ruby_check
        python_check.merge('language' => 'ruby')
    end

    def task(check)
        { 'id' => 'ggt', 'title' => 'Der größte gemeinsame Teiler', 'prompt' => 'Schreib `gcd(a, b)`.', 'check' => check }
    end

    def needs_docker
        available = system('docker', 'image', 'inspect', KataRunner::IMAGE, :out => File::NULL, :err => File::NULL)
        skip "no Docker image #{KataRunner::IMAGE}" unless available
    end

    def test_the_spec
        assert_empty TaskSpec.validate_single_task(task(python_check).merge('starter' => "def gcd(a, b):\n    pass\n", 'level' => 3))
        refute_empty TaskSpec.validate_single_task(task(python_check.merge('language' => 'cobol')))
        refute_empty TaskSpec.validate_single_task(task(python_check).merge('level' => 4))
        refute_empty TaskSpec.validate_single_task(task(python_check.merge('cases' => [])))
        number = { 'id' => 'n', 'title' => 'Zahl', 'prompt' => 'Wie viele?', 'check' => { 'type' => 'number', 'expect' => 3 } }
        refute_empty TaskSpec.validate_single_task(number.merge('starter' => 'x'))
    end

    def test_nothing_typed_is_no_attempt
        status, message, = KataCheck.check(task(python_check), python_check, "  \n", RUN)
        assert_equal :invalid, status
        assert_equal KataCheck::EMPTY, message
    end

    def test_a_full_queue_says_so
        status, message, = KataCheck.check(task(python_check), python_check, "def gcd(a, b):\n    return a\n", ->(**_) { nil })
        assert_equal :unavailable, status
        assert_equal KataCheck::BUSY, message
    end

    def test_python_right_and_wrong
        needs_docker
        right = "def gcd(a, b):\n    while b:\n        a, b = b, a % b\n    return a\n\nprint('nur zum Ausprobieren')\n"
        status, message, read = KataCheck.check(task(python_check), python_check, right, RUN)
        assert_equal :correct, status, message
        assert_includes read, '`gcd(60, 48)` → `12`'
        wrong = "def gcd(a, b):\n    return min(a, b)\n"
        status, message, = KataCheck.check(task(python_check), python_check, wrong, RUN)
        assert_equal :wrong, status
        assert_equal '`gcd(60, 48)` ergibt `48`, richtig wäre `12`.', message
    end

    def test_ruby_right_and_with_a_syntax_error
        needs_docker
        right = "def gcd(a, b)\n  b.zero? ? a : gcd(b, a % b)\nend\n"
        status, message, = KataCheck.check(task(ruby_check), ruby_check, right, RUN)
        assert_equal :correct, status, message
        status, message, = KataCheck.check(task(ruby_check), ruby_check, "def gcd(a, b)\n  a +\n", RUN)
        assert_equal :wrong, status
        assert_match(/\AIn deinem Code \(Zeile \d+\) ist ein Syntaxfehler/, message)
    end

    def test_an_endless_loop_is_named
        needs_docker
        check = python_check.merge('timeout' => 2)
        status, message, = KataCheck.check(task(check), check, "def gcd(a, b):\n    while True:\n        pass\n", RUN)
        assert_equal :wrong, status
        assert_equal 'Dein Code lief länger als 2 Sekunden und wurde abgebrochen. Steckt irgendwo eine Endlosschleife?', message
    end

    def test_forbid_switches_off_what_would_solve_it
        refute_empty TaskSpec.validate_single_task(task(ruby_check.merge('forbid' => ['gcd'])))
        refute_empty TaskSpec.validate_single_task(task(python_check.merge('forbid' => ['Integer#gcd'])))
        assert_empty TaskSpec.validate_single_task(task(python_check.merge('forbid' => ['math.gcd', 'sorted', 'list.sort'])))
        needs_docker
        check = ruby_check.merge('forbid' => ['Integer#gcd'])
        status, message, = KataCheck.check(task(check), check, "def gcd(a, b)\n  a.gcd(b)\nend\n", RUN)
        assert_equal :wrong, status
        assert_equal '`gcd(60, 48)` (Zeile 2) benutzt `Integer#gcd`. Das gibt es in dieser Aufgabe nicht: Du schreibst es gerade selbst.', message
        # the expected values may still use it
        status, = KataCheck.check(task(check), check, "def gcd(a, b)\n  b.zero? ? a : gcd(b, a % b)\nend\n", RUN)
        assert_equal :correct, status
        check = python_check.merge('forbid' => ['math.gcd'])
        status, message, = KataCheck.check(task(check), check, "from math import gcd as g\ndef gcd(a, b):\n    return g(a, b)\n", RUN)
        assert_equal :wrong, status
        assert_includes message, 'benutzt `math.gcd`'
        sorting = { 'type' => 'function', 'language' => 'python', 'forbid' => ['sorted', 'list.sort'],
                    'cases' => [{ 'smallest([3, 1, 2])' => 1 }] }
        status, message, = KataCheck.check(task(sorting), sorting, "def smallest(xs):\n    xs.sort()\n    return xs[0]\n", RUN)
        assert_equal :wrong, status
        assert_equal 'Dein Code (Zeile 2) benutzt `list.sort`. Das gibt es in dieser Aufgabe nicht: Du schreibst es gerade selbst.', message
        status, = KataCheck.check(task(sorting), sorting, "def smallest(xs):\n    return min(xs)\n", RUN)
        assert_equal :correct, status
    end

    def test_random_cases_are_the_same_for_a_person_and_name_the_value
        random = { 'count' => 4, 'call' => 'gcd(a, b)', 'args' => { 'a' => '1..1000', 'b' => '1..1000' }, 'reference' => 'a.gcd(b)' }
        check = ruby_check.merge('random' => random)
        assert_empty TaskSpec.validate_single_task(task(check))
        refute_empty TaskSpec.validate_single_task(task(ruby_check.merge('random' => random.merge('args' => { 'a' => '9..1' }))))
        seed = KataCheck.seed('ada@example.org', 'kata-demo/ggt', 'v1')
        assert_equal KataCheck.random_cases(check, seed), KataCheck.random_cases(check, seed)
        refute_equal KataCheck.random_cases(check, seed), KataCheck.random_cases(check, KataCheck.seed('linus@example.org', 'kata-demo/ggt', 'v1'))
        first = KataCheck.random_cases(check, seed).first
        assert_match(/\Agcd\(\d+, \d+\)\z/, first['call'])
        needs_docker
        # a table of the fixed cases does not pass
        table = "def gcd(a, b)\n  { [60, 48] => 12, [7, 7] => 7 }.fetch([a, b], 1)\nend\n"
        status, message, = KataCheck.check(task(check), check, table, RUN, :seed => seed)
        assert_equal :wrong, status
        assert_match(/\A`gcd\(\d+, \d+\)` ergibt `1`, richtig wäre `\d+`\.\z/, message)
        refute_includes message, 'a.gcd'
        right = "def gcd(a, b)\n  b.zero? ? a : gcd(b, a % b)\nend\n"
        assert_equal :correct, KataCheck.check(task(check), check, right, RUN, :seed => seed).first
        python = python_check.merge('random' => random.merge('reference' => 'math.gcd(a, b)'))
        right = "def gcd(a, b):\n    while b:\n        a, b = b, a % b\n    return a\n"
        assert_equal :correct, KataCheck.check(task(python), python, right, RUN, :seed => seed).first
    end

    def test_same_as_recognises_a_typical_wrong_solution
        feedback = [{ 'when' => { 'same_as' => "def gcd(a, b) = [a, b].min\n" },
                      'say' => 'Die kleinere Zahl ist nur manchmal ein Teiler der größeren.' }]
        t = task(ruby_check).merge('feedback' => feedback)
        assert_empty TaskSpec.validate_single_task(t)
        refute_empty TaskSpec.validate_single_task(task(ruby_check).merge('feedback' => [{ 'when' => { 'same_as' => '' }, 'say' => 'x' }]))
        needs_docker
        status, message, = KataCheck.check(t, ruby_check, "def gcd(a, b)\n  a < b ? a : b\nend\n", RUN)
        assert_equal :wrong, status
        assert_equal 'Die kleinere Zahl ist nur manchmal ein Teiler der größeren.', message
        status, message, = KataCheck.check(t, ruby_check, "def gcd(a, b)\n  a\nend\n", RUN)
        assert_equal :wrong, status
        assert_equal '`gcd(60, 48)` ergibt `60`, richtig wäre `12`.', message
    end

    def test_running_the_program_shows_what_it_prints
        needs_docker
        ran = KataCheck.run_program('python', "print('Hallo')\nprint(1 / 0)\n", RUN)
        assert_equal :error, ran[:status]
        assert_includes ran[:output], "Hallo\n"
        assert_includes ran[:output], 'ZeroDivisionError'
        ran = KataCheck.run_program('ruby', "puts 6 * 7\n", RUN)
        assert_equal :ran, ran[:status]
        assert_equal "42\n", ran[:output]
    end
end
