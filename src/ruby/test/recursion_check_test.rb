require 'minitest/autorun'
require_relative '../include/recursion_check'

class RecursionCheckTest < Minitest::Test
    def check(language, **more)
        { 'language' => language, 'recursive' => true, 'loops' => false, 'cases' => [{ 'call' => 'factorial(5)', 'expect' => '120' }] }
            .merge(more.transform_keys(&:to_s))
    end

    RECURSIVE = {
        'ruby' => "def factorial(n)\n  return 1 if n == 0\n  n * factorial(n - 1)\nend\n",
        'python' => "def factorial(n):\n    # factorial(n) for n = 0 is 1\n    if n == 0:\n        return 1\n    return n * factorial(n - 1)\n",
        'javascript' => "function factorial(n) {\n  // a while would be a loop: 'while'\n  if (n === 0) {\n    return 1;\n  }\n  return n * factorial(n - 1);\n}\n",
    }.freeze
    LOOPED = {
        'ruby' => "def factorial(n)\n  result = 1\n  (1..n).each { |i| result *= i }\n  result\nend\n",
        'python' => "def factorial(n):\n    result = 1\n    for i in range(1, n + 1):\n        result *= i\n    return result\n",
        'javascript' => "function factorial(n) {\n  let result = 1;\n  for (let i = 1; i <= n; i++) {\n    result *= i;\n  }\n  return result;\n}\n",
    }.freeze

    def test_a_recursion_passes_in_each_language
        RECURSIVE.each { |language, code| assert_nil RecursionCheck.problem(check(language), code, 'factorial'), language }
    end

    def test_a_loop_is_named
        assert_includes RecursionCheck.problem(check('ruby'), LOOPED['ruby'], 'factorial'), '`each` mit einem Block'
        assert_includes RecursionCheck.problem(check('python'), LOOPED['python'], 'factorial'), '`for`'
        assert_includes RecursionCheck.problem(check('javascript'), LOOPED['javascript'], 'factorial'), '`for`'
    end

    def test_without_the_call_of_itself
        python = "def factorial(n):\n    return 1 if n == 0 else n * fact(n - 1)\n"
        assert_equal RecursionCheck::NOT_RECURSIVE, RecursionCheck.problem(check('python'), python, 'factorial')
        arrow = "const factorial = (n) => {\n  return n * 2;\n};\n"
        assert_equal RecursionCheck::NOT_RECURSIVE, RecursionCheck.problem(check('javascript'), arrow, 'factorial')
    end

    # in JavaScript a spread of its own call counts, a method of the same name not
    def test_a_spread_counts_a_method_does_not
        spread = "function factorial(n) {\n  return n === 0 ? 1 : [...[factorial(n - 1)]][0] * n;\n}\n"
        assert_nil RecursionCheck.problem(check('javascript'), spread, 'factorial')
        hanoi = "function factorial(n) {\n  return n === 0 ? [] : [...factorial(n - 1), n];\n}\n"
        assert_nil RecursionCheck.problem(check('javascript'), hanoi, 'factorial')
        method = "function factorial(n) {\n  return Math.factorial(n);\n}\n"
        assert_equal RecursionCheck::NOT_RECURSIVE, RecursionCheck.problem(check('javascript'), method, 'factorial')
        dart = "List<int> factorial(int n) {\n  return n == 0 ? [] : [n, ...factorial(n - 1)];\n}\n"
        assert_nil RecursionCheck.problem(check('dart'), dart, 'factorial')
        dart_method = "int factorial(int n) {\n  return math.factorial(n);\n}\n"
        assert_equal RecursionCheck::NOT_RECURSIVE, RecursionCheck.problem(check('dart'), dart_method, 'factorial')
    end

    # recursive with a loop where loops are allowed (all orders of a text)
    def test_loops_may_be_allowed
        assert_nil RecursionCheck.problem(check('ruby', :loops => nil).reject { |k, _| k == 'loops' }, "def factorial(n)\n  return 1 if n == 0\n  [n].each { |k| k }\n  n * factorial(n - 1)\nend\n", 'factorial')
    end

    def test_the_name_comes_from_the_first_case
        assert_equal 'factorial', RecursionCheck.function_name(check('ruby'))
        assert_equal 'gcd', RecursionCheck.function_name({ 'cases' => [{ 'gcd(60, 48)' => 12 }] })
    end
end
