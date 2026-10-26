require 'minitest/autorun'
require_relative '../include/kata_check'
require_relative '../include/kata_runner'
require_relative '../include/task_spec'

# The check type tests: the student's tests against a right and broken
# versions, with real runs in the Workspace image; the spec everywhere.
class TestsCheckTest < Minitest::Test
    RUN = lambda do |files:, argv:, timeout:|
        KataRunner.run(:files => files, :argv => argv, :timeout => timeout)
    end

    def check(language = 'ruby')
        {
            'ruby' => { 'type' => 'tests', 'language' => 'ruby', 'function' => 'even',
                        'correct' => "def even(n)\n  n % 2 == 0\nend\n",
                        'mutants' => [{ 'code' => "def even(n)\n  n % 2 == 1\nend\n", 'say' => 'Vertauscht.' },
                                      { 'code' => "def even(n)\n  n > 0 && n % 2 == 0\nend\n", 'say' => 'Bei 0 falsch.' }] },
            'python' => { 'type' => 'tests', 'language' => 'python', 'function' => 'even',
                          'correct' => "def even(n):\n    return n % 2 == 0\n",
                          'mutants' => [{ 'code' => "def even(n):\n    return n % 2 == 1\n", 'say' => 'Vertauscht.' },
                                        { 'code' => "def even(n):\n    return n > 0 and n % 2 == 0\n", 'say' => 'Bei 0 falsch.' }] },
            'javascript' => { 'type' => 'tests', 'language' => 'javascript', 'function' => 'even',
                              'correct' => "function even(n) {\n  return n % 2 === 0;\n}\n",
                              'mutants' => [{ 'code' => "function even(n) {\n  return Math.abs(n % 2) === 1;\n}\n", 'say' => 'Vertauscht.' },
                                            { 'code' => "function even(n) {\n  return n > 0 && n % 2 === 0;\n}\n", 'say' => 'Bei 0 falsch.' }] },
        }.fetch(language)
    end

    def needs_docker
        available = system('docker', 'image', 'inspect', KataRunner::IMAGE, :out => File::NULL, :err => File::NULL)
        skip "no Docker image #{KataRunner::IMAGE}" unless available
    end

    def test_the_spec
        assert_empty TestsCheck.spec_errors(check)
        refute_empty TestsCheck.spec_errors(check.merge('mutants' => []))
        refute_empty TestsCheck.spec_errors(check.merge('function' => 'two words'))
        assert_empty TaskSpec.validate_single_task({ 'id' => 't', 'title' => 'T', 'prompt' => 'P', 'check' => check, 'starter' => "assert_equal(true, even(4))\n" })
    end

    def test_writing_the_function_itself_is_refused_before_running
        status, message, = TestsCheck.check({}, check, "def even(n)\n  true\nend\n", ->(**_) { raise 'must not run' })
        assert_equal :wrong, status
        assert_includes message, 'nicht die Funktion `even` selbst'
    end

    def test_each_language_from_nothing_to_all_caught
        needs_docker
        tests = {
            'ruby' => ["# noch nichts\n", "assert_equal(false, even(4))\n", "assert_equal(true, even(4))\nassert_equal(false, even(7))\n",
                       "assert_equal(true, even(4))\nassert_equal(false, even(7))\nassert_equal(true, even(0))\n"],
            'python' => ["# noch nichts\n", "assert_equal(False, even(4))\n", "assert_equal(True, even(4))\nassert_equal(False, even(7))\n",
                         "assert_equal(True, even(4))\nassert_equal(False, even(7))\nassert_equal(True, even(0))\n"],
            'javascript' => ["// noch nichts\n", "assertEqual(false, even(4));\n", "assertEqual(true, even(4));\nassertEqual(false, even(7));\n",
                             "assertEqual(true, even(4));\nassertEqual(false, even(7));\nassertEqual(true, even(0));\n"],
        }
        tests.each do |language, (none, wrong, partly, all)|
            c = check(language)
            assert_includes TestsCheck.check({}, c, none, RUN)[1], 'noch keinen Test', language
            status, message, = TestsCheck.check({}, c, wrong, RUN)
            assert_equal :wrong, status, language
            assert_includes message, 'Zeile 1', language
            assert_equal [:wrong, 'Bei 0 falsch.'], TestsCheck.check({}, c, partly, RUN).first(2), language
            status, message, = TestsCheck.check({}, c, all, RUN)
            assert_equal :correct, status, "#{language}: #{message}"
            assert_includes message, '3 Prüfungen', language
        end
    end

    # in Python True is not 1: a test expecting 1 does not pass a True
    def test_true_is_not_one_in_python
        needs_docker
        status, message, = TestsCheck.check({}, check('python'), "assert_equal(1, even(4))\n", RUN)
        assert_equal :wrong, status
        assert_includes message, 'ergibt `True`'
    end
end
