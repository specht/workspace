require 'minitest/autorun'
require 'open3'
require 'tmpdir'
require_relative '../include/kata_check'
require_relative '../include/task_spec'

# Katas about circuits (KataCheck with gates:, GateCheck): the functions may
# combine their inputs only through the gates of the check. Runs with the
# local ruby and python3, as task_examples_test does.
class GateCheckTest < Minitest::Test
    RUN = lambda do |files:, argv:, timeout:|
        Dir.mktmpdir('gate-check') do |dir|
            files.each { |name, text| File.write(File.join(dir, name), text) }
            stdout, stderr, status = Open3.capture3(*argv, :chdir => dir)
            Struct.new(:stdout, :stderr, :status, :timed_out, :too_long)
                  .new(stdout, stderr, status.exitstatus, false, false)
        end
    end

    def and_check(language)
        { 'type' => 'function', 'language' => language, 'gates' => ['nand'],
          'cases' => [{ 'and_(0, 0)' => 0 }, { 'and_(0, 1)' => 0 }, { 'and_(1, 0)' => 0 }, { 'and_(1, 1)' => 1 }] }
    end

    def task(check)
        { 'id' => 'und', 'title' => 'UND aus NAND', 'prompt' => 'Schreib `and_`.', 'check' => check }
    end

    def check(check, code)
        KataCheck.check(task(check), check, code, RUN)
    end

    RUBY_NAND = "def nand(a, b)\n  a == 1 && b == 1 ? 0 : 1\nend\n"
    PYTHON_NAND = "def nand(a, b):\n    return 0 if a == 1 and b == 1 else 1\n"

    def test_the_spec
        assert_empty TaskSpec.validate_single_task(task(and_check('ruby')).merge('starter' => RUBY_NAND))
        refute_empty TaskSpec.validate_single_task(task(and_check('ruby').merge('gates' => ['and'])))
        refute_empty TaskSpec.validate_single_task(task(and_check('ruby').merge('gates' => [])))
        refute_empty TaskSpec.validate_single_task(task(and_check('javascript')))
        steps = and_check('ruby').merge('cases' => [{ 'x = 1; and_(x, 1)' => 1 }])
        refute_empty TaskSpec.validate_single_task(task(steps))
    end

    def test_wired_calls
        wired = GateCheck.wired([{ 'call' => 'add4([1, 0], [0, 1])', 'expect' => '[[1, 1], 0]' }], 'ruby')
        assert_equal 'kata_wired(:add4, [1, 0], [0, 1])', wired.first['call']
        assert_equal "_k_wired('f')", GateCheck.wired([{ 'call' => 'f()', 'expect' => '0' }], 'python').first['call']
        assert_equal '`nand`', GateCheck.named(['nand'])
        assert_equal '`and_`, `or_` und `xor_`', GateCheck.named(%w[and_ or_ xor_])
    end

    def test_ruby_through_the_gate_passes
        status, message, read = check(and_check('ruby'), RUBY_NAND + "\ndef and_(a, b)\n  x = nand(a, b)\n  nand(x, x)\nend\n")
        assert_equal :correct, status, message
        assert_includes read, 'Die Eingänge gehen nur durch `nand`.'
        assert_includes read, '`and_(1, 1)` → `1`'
    end

    def test_ruby_that_computes_itself_fails
        status, message, read = check(and_check('ruby'), RUBY_NAND + "\ndef and_(a, b)\n  a & b\nend\n")
        assert_equal :wrong, status
        assert_includes message, 'Mit 0 und 1 stimmt `and_(0, 0)`.'
        assert_includes message, GateCheck::REFUSED
        assert_includes read, "<i class='bi bi-x-lg'></i> Nur mit `nand`"
        status, message, = check(and_check('ruby'), RUBY_NAND + "\ndef and_(a, b)\n  a == 1 && b == 1 ? 1 : 0\nend\n")
        assert_equal :wrong, status
        assert_includes message, 'Kommen die Eingänge aber als Signale'
        status, = check(and_check('ruby'), RUBY_NAND + "\nTABLE = [[0, 0], [0, 1]]\ndef and_(a, b)\n  TABLE[a][b]\nend\n")
        assert_equal :wrong, status
    end

    def test_a_wrong_answer_gets_the_usual_message
        status, message, = check(and_check('ruby'), RUBY_NAND + "\ndef and_(a, b)\n  nand(a, b)\nend\n")
        assert_equal :wrong, status
        assert_includes message, '`and_(0, 0)` ergibt `1`, richtig wäre `0`.'
    end

    def test_python_through_the_gate_passes_and_if_fails
        status, message, read = check(and_check('python'), PYTHON_NAND + "\ndef and_(a, b):\n    x = nand(a, b)\n    return nand(x, x)\n")
        assert_equal :correct, status, message
        assert_includes read, 'Die Eingänge gehen nur durch `nand`.'
        status, message, = check(and_check('python'), PYTHON_NAND + "\ndef and_(a, b):\n    if a and b:\n        return 1\n    return 0\n")
        assert_equal :wrong, status
        assert_includes message, GateCheck::REFUSED
        status, = check(and_check('python'), PYTHON_NAND + "\ndef and_(a, b):\n    return a * b\n")
        assert_equal :wrong, status
    end

    # A constant goes into a gate (a carry that starts at 0); a text that
    # chooses what to compute is passed on; lists are wired too.
    def test_constants_lists_and_choices
        gates = %w[and_ or_ xor_ not_]
        plain = "def and_(a, b) = a & b\ndef or_(a, b) = a | b\ndef xor_(a, b) = a ^ b\ndef not_(a) = 1 - a\n"
        code = plain + <<~RUBY
            def pick(op, a, b)
              carry = 0
              bits = a.zip(b).map { |x, y| op == 'and' ? and_(x, y) : xor_(xor_(x, y), carry) }
              [bits, not_(carry)]
            end
        RUBY
        c = { 'type' => 'function', 'language' => 'ruby', 'gates' => gates,
              'cases' => [{ "pick('and', [1, 1], [0, 1])" => '[[0, 1], 1]' }, { "pick('xor', [1, 1], [0, 1])" => '[[1, 0], 1]' }] }
        status, message, = check(c, code)
        assert_equal :correct, status, message
        python = "def and_(a, b):\n    return a & b\ndef or_(a, b):\n    return a | b\ndef xor_(a, b):\n    return a ^ b\ndef not_(a):\n    return 1 - a\n" \
                 "def pick(op, a, b):\n    carry = 0\n    bits = [and_(x, y) if op == 'and' else xor_(xor_(x, y), carry) for x, y in zip(a, b)]\n    return [bits, not_(carry)]\n"
        status, message, = check(c.merge('language' => 'python'), python)
        assert_equal :correct, status, message
    end

    # A gate of the check can be called itself, in both languages.
    def test_a_gate_as_a_case
        c = and_check('ruby').merge('cases' => [{ 'and_(1, 1)' => 1 }, { 'nand(1, 1)' => 0 }])
        status, message, = check(c, RUBY_NAND + "\ndef and_(a, b)\n  x = nand(a, b)\n  nand(x, x)\nend\n")
        assert_equal :correct, status, message
        status, message, = check(c.merge('language' => 'python'), PYTHON_NAND + "\ndef and_(a, b):\n    x = nand(a, b)\n    return nand(x, x)\n")
        assert_equal :correct, status, message
    end
end
