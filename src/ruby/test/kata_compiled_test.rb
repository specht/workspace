require 'minitest/autorun'
require_relative '../include/kata_check'

# Katas in Dart and Java (KataCompiled): what can be tested without a
# compiler. The runs themselves are covered by the task examples, on a
# machine with dart and javac.
class KataCompiledTest < Minitest::Test
    def test_forbidden_uses_are_found_in_the_code_but_not_in_strings_or_comments
        code = "int listSum(List<int> numbers) {\n  // not numbers.reduce here\n  final s = 'max(1, 2)';\n  return numbers.fold(0, (a, b) => a + b);\n}\n"
        assert_equal ['fold', 4], KataCompiled.forbidden(code, ['.reduce', '.fold'])
        assert_nil KataCompiled.forbidden(code, ['max', '.reduce'])
        assert_equal ['Math.max', 2], KataCompiled.forbidden("int m(int a, int b) {\n    return Math.max(a, b);\n}\n", ['Math.max'])
        assert_equal ['max', 1], KataCompiled.forbidden("int m(int a, int b) => max(a, b);\n", ['max'])
        assert_nil KataCompiled.forbidden("int m(int a, int b) => a.max;\n", ['max'])
    end

    def test_forbid_entries_have_three_forms
        assert_empty KataCompiled.forbid_errors(['max', '.reduce', 'Math.max'])
        refute_empty KataCompiled.forbid_errors(['Array#sum'])
        refute_empty KataCompiled.forbid_errors([])
    end

    def test_java_code_goes_into_the_class_and_its_lines_count_back
        code = "import java.util.function.Function;\n\nint twice(int n) {\n    return 2 * n;\n}\n"
        source, first, last, ranges = KataCompiled.java_source('HsKata', code, [{ 'call' => 'twice(2)', 'expect' => '4' }])
        lines = source.lines
        assert_match(/\Aimport java\.util\.function\.Function;/, lines[1])
        assert_equal "int twice(int n) {\n", lines[first + 1]
        assert_equal code.split("\n", -1).size, last - first + 1
        assert_includes lines[ranges.first.first - 1], 'void __hsCase0()'
        assert_includes source, 'class HsRunHsKata'
    end

    def test_a_compiler_error_in_the_code_names_its_line
        code = "int twice(int n) {\n    return 2 * n\n}\n"
        _, first, = KataCompiled.java_source('HsKata', code, [])
        errors = "HsKata.java:#{first + 1}: error: ';' expected\n        return 2 * n\n                    ^\n1 error\n"
        status, message, read = KataCompiled.java_problem(code, [], errors)
        assert_equal :wrong, status
        assert_includes message, 'Zeile 2'
        assert_includes read, 'Zeile 2: error'
    end

    def test_each_target_has_its_section
        out = "\n__HS_TARGET__ kata\n\n__HS_RESULT__{}\n\n__HS_TARGET__ alt1\n__HS_COMPILE__\nalt1.dart:1:1: Error: x\n"
        sections = KataCompiled.sections(out)
        assert_equal %w[kata alt1], sections.keys
        assert_includes sections['alt1'], '__HS_COMPILE__'
    end

    def test_random_references_get_the_values
        assert_equal '(3 + 4)', KataCompiled.reference('a + b', { 'a' => '3', 'b' => '4' })
        assert_equal 'List.of(1, 2)', KataCompiled.literal([1, 2], 'java')
        assert_equal '"a\\$b"', KataCompiled.literal('a$b', 'dart')
    end

    def test_a_function_kata_may_be_in_dart_or_java
        assert_empty KataCheck.spec_errors({ 'type' => 'function', 'language' => 'dart', 'cases' => [{ 'call' => 'f(1)', 'expect' => '1' }], 'forbid' => ['.reduce'] })
        assert_empty KataCheck.spec_errors({ 'type' => 'function', 'language' => 'java', 'cases' => [{ 'call' => 'f(1)', 'expect' => '1' }] })
        assert KataCheck.top_level?("int f() => 1;\nvoid main() { print(f()); }\n", 'dart')
        refute KataCheck.top_level?("int f() => 1;\n", 'java')
    end
end
