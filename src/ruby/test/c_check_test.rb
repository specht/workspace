require 'minitest/autorun'
require 'open3'
require 'tmpdir'
require 'fileutils'
require_relative '../include/c_check'
require_relative '../include/task_spec'

# Runs the harness of check type c with the local cc, as TaskExamples does.
class CCheckTest < Minitest::Test
    PROGRAM = <<~C
        #include <stdio.h>
        #include <stdlib.h>
        #include <stdbool.h>

        typedef struct { double x, y; } vec;

        int square(int n) { return n * n; }
        double half(double x) { return x / 2; }
        vec turn(vec v) { return (vec){-v.y, v.x}; }
        bool even(int n) { return n % 2 == 0; }
        const char *greet(void) { return "Hallo"; }
        int crash(void) { int *p = NULL; return *p; }
        int spin(void) { volatile int i = 0; for (;;) i++; return i; }
        int quit(void) { exit(3); }
        int noisy(void) { printf("Zwischenstand\\n"); return 7; }

        int main(void)
        {
            printf("Das Programm selbst läuft nicht.\\n");
            return 1;
        }
    C

    def setup
        skip 'no cc here' unless system('cc --version', :out => File::NULL, :err => File::NULL)
        @root = Dir.mktmpdir
        FileUtils.mkdir_p(File.join(@root, 'p'))
        File.write(File.join(@root, 'p', 'main.c'), PROGRAM)
    end

    def teardown
        FileUtils.rm_rf(@root) if @root
    end

    def check(cases, extra = {})
        { 'type' => 'c', 'file' => 'p/main.c', 'cases' => cases }.merge(extra)
    end

    def run_check(check)
        output, = Open3.capture2e('sh', '-c', CCheck.command_for_check(check), :chdir => @root)
        CCheck.evaluate({}, check, output)
    end

    def test_numbers_strings_bools_and_fields
        status, message, read = run_check(check([
            { 'square(7)' => 49 },
            { 'half(1)' => 0.5 },
            { 'turn((vec){1, 2}).x' => -2 },
            { 'even(4)' => true },
            { 'greet()' => '"Hallo"' },
            { 'noisy()' => 7 },
        ]))
        assert_equal :correct, status, message
        assert_includes read, '`square(7)` → `49`'
        assert_includes read, '`greet()` → `Hallo`'
        assert_includes read, '`even(4)` → `true`'
    end

    def test_a_wrong_value_and_the_tolerance
        status, message, = run_check(check([{ 'half(1)' => 0.6 }]))
        assert_equal :wrong, status
        assert_equal '`half(1)` ergibt `0.5`, richtig wäre `0.6`.', message
        status, = run_check(check([{ 'half(1)' => 0.6 }], 'tolerance' => 0.2))
        assert_equal :correct, status
    end

    def test_infinity
        status, message, = run_check(check([{ 'half(1.0 / 0.0)' => Float::INFINITY }]))
        assert_equal :correct, status, message
    end

    def test_crash_loop_and_exit_cost_only_their_case
        status, message, read = run_check(check([
            { 'crash()' => 0 },
            { 'spin()' => 0 },
            { 'quit()' => 0 },
            { 'square(3)' => 9 },
        ]))
        assert_equal :wrong, status
        assert_includes message, 'greift auf Speicher zu'
        assert_includes read, 'Endlosschleife'
        assert_includes read, 'exit(3)'
        assert_includes read, '`square(3)` → `9`'
    end

    def test_a_function_that_is_not_there
        status, message, = run_check(check([{ 'cube(2)' => 8 }]))
        assert_equal :wrong, status
        assert_includes message, 'gibt es `cube` noch nicht'
    end

    def test_a_call_that_does_not_fit
        status, message, = run_check(check([{ 'square(1, 2)' => 1 }]))
        assert_equal :wrong, status
        assert_includes message, 'Der Aufruf `square(1, 2)` passt noch nicht'
    end

    def test_an_error_in_the_file
        File.write(File.join(@root, 'p', 'main.c'), "int square(int n) { return n * n }\n")
        status, message, = run_check(check([{ 'square(2)' => 4 }]))
        assert_equal :wrong, status
        assert_includes message, '`main.c` lässt sich nicht übersetzen (Zeile 1)'
    end

    def test_pixelram_comes_from_make
        File.write(File.join(@root, 'p', 'main.c'), "#include \"pixelram.h\"\nint square(int n) { return n * n; }\n")
        status, message, = run_check(check([{ 'square(2)' => 4 }]))
        assert_equal :wrong, status
        assert_includes message, 'holt erst `make`'

        dir = File.join(@root, 'p', '.pixelram', 'v0.1.0')
        FileUtils.mkdir_p(dir)
        File.write(File.join(dir, 'pixelram.h'), "void present(void);\n")
        File.write(File.join(@root, 'p', 'main.c'),
                   "#include \"pixelram.h\"\nint square(int n) { return n * n; }\nint main(void) { present(); return 0; }\n")
        status, message, = run_check(check([{ 'square(2)' => 4 }]))
        assert_equal :correct, status, message
    end

    def test_setup_helpers_and_their_errors
        status, message, = run_check(check([{ 'twice_square(3)' => 18 }], 'setup' => "int twice_square(int n)\n{\n    return 2 * square(n);\n}\n"))
        assert_equal :correct, status, message
        status, message, = run_check(check([{ 'twice(3)' => 6 }], 'setup' => "int twice(int n)\n{\n    return turn(n);\n}\n"))
        assert_equal :wrong, status
        assert_includes message, 'Die Prüfung passt noch nicht zu deinem Programm'
    end

    def test_a_missing_file
        status, message, = run_check(check([{ 'square(2)' => 4 }]).merge('file' => 'p/nope.c'))
        assert_equal :wrong, status
        assert_includes message, 'gibt es nicht'
    end

    def test_spec
        assert_empty CCheck.spec_errors(check([{ 'square(2)' => 4 }]))
        refute_empty CCheck.spec_errors(check([{ 'call' => 'square(2)', 'raises' => true }]))
        refute_empty CCheck.spec_errors(check([{ 'square(2)' => 4 }], 'tolerance' => -1))
        refute_empty CCheck.spec_errors(check([{ 'square(2)' => 4 }]).merge('file' => 'p/main.py'))
        refute_empty CCheck.spec_errors(check([{ 'square(2)' => 4 }], 'setup' => 3))
    end
end
