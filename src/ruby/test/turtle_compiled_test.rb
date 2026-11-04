require 'minitest/autorun'
require_relative '../include/turtle_compiled'

# The turtle in Java and Dart (TurtleCompiled): reading back what the
# program wrote and putting code into main here; the programs themselves in
# real Kata containers, skipped where there is no Docker image.
class TurtleCompiledTest < Minitest::Test
    M = TurtleCompiled::MARK
    PROGRAM = { :first => 1, :last => 9 }.freeze

    def parse(stdout, count = 1, timed_out: false)
        TurtleCompiled.parse('java', "void main() {}\n", stdout, count, PROGRAM, :timed_out => timed_out)
    end

    def test_commands_output_and_errors_come_back_per_run
        out = "#{M}START\t0\n#{M}forward\t100\t2\nhallo\n#{M}color\thue:0\t3\n#{M}color\tred\t4\n#{M}END\t0\n" \
              "#{M}START\t1\n#{M}left\t90.5\t0\n#{M}ERROR\t7\tArithmeticException: / by zero\n#{M}END\t1\n"
        result = parse(out, 2)
        assert_nil result[:limit]
        first, second = result[:runs]
        assert_equal [['forward', 100, 2], ['out', "hallo\n"], ['color', '#d81818', 3], ['color', '#ff0000', 4]], first[:items]
        assert_nil first[:error]
        assert_equal [['left', 90.5, nil]], second[:items]
        assert_equal({ :message => 'ArithmeticException: / by zero', :line => 7 }, second[:error])
    end

    def test_output_without_a_newline_stays_before_the_next_command
        result = parse("#{M}START\t0\nab#{M}forward\t5\t1\nc\n#{M}END\t0\n")
        assert_equal [['out', 'ab'], ['forward', 5, 1], ['out', "c\n"]], result[:runs][0][:items]
    end

    def test_a_limit_ends_everything
        result = parse("#{M}START\t0\n#{M}forward\t1\t1\n#{M}LIMIT\tcommands\n", 2)
        assert_equal 'commands', result[:limit]
        assert_equal 1, result[:runs].size
        assert_nil result[:runs][0][:error]
    end

    def test_a_program_that_runs_too_long_says_so
        result = parse("#{M}START\t0\n#{M}forward\t1\t1\n", 1, :timed_out => true)
        assert_equal "Dein Programm lief länger als #{TurtleCompiled::RUN_SECONDS} Sekunden.", result[:runs][0][:error][:message]
    end

    def test_only_commands_go_into_main
        assert TurtleCompiled.wrap?("forward(100);\nleft(90);\n")
        refute TurtleCompiled.wrap?("void main() {\n  forward(1);\n}\n")
        refute TurtleCompiled.wrap?("void square(double size) {\n  forward(size);\n}\n")
        files, first, last = TurtleCompiled.java_files("import java.util.*;\nforward(100);\n", [nil])
        assert_equal "void main() {\n", files['HsTurtle.java'].lines[first - 2]
        assert_equal '', files['HsTurtle.java'].lines[first - 1].strip
        assert_equal 3, last - first + 1 # with the empty line after the last
        dart = TurtleCompiled.dart_files("forward(100);\n", [nil, 'square(5)'])
        assert_equal "import 'turtle.dart'; void main() { forward(100);", dart['kata.dart'].lines.first.chomp
        assert_includes dart['run.dart'], "turtleRun(1, true, () { kata.main(); turtleLoaded(); square(5); })"
    end

    def test_a_compile_error_names_the_persons_line
        errors = "HsTurtle.java:8: error: cannot find symbol\n        fowrard(100);\n        ^\n  symbol:   method fowrard(int)\n  location: class HsTurtle\n1 error\n"
        error = TurtleCompiled.compile_error('java', "void main() {\n    fowrard(100);\n}\n", errors, 7, 9)
        assert_equal({ :message => 'cannot find symbol (method fowrard(int))', :line => 2, :compile => true }, error)
        dart = TurtleCompiled.compile_error('dart', "void square(double s) { forward(s); }\nsquare(10);\n",
                                            "kata.dart:2:8: Error: Expected an identifier, but got '10'.\n", 1, 2)
        assert_equal 2, dart[:line]
        assert_includes dart[:message], TurtleCompiled::MAIN_MISSING
    end

    # ----- in containers -----

    def docker?
        system('docker', 'image', 'inspect', KataRunner::IMAGE, :out => File::NULL, :err => File::NULL)
    end

    def run_compiled(language, code, runs)
        kata = lambda do |files:, argv:, timeout:, dialog: nil, max_output: KataRunner::MAX_OUTPUT|
            KataRunner.run(:files => files, :argv => argv, :timeout => timeout, :max_output => max_output)
        end
        TurtleCompiled.run(language, code, runs, kata)
    end

    def test_java_draws_prints_and_starts_each_run_afresh
        skip "no Docker image #{KataRunner::IMAGE}" unless docker?
        HotCompile.enabled = false
        code = "int calls = 0;\n\nvoid square(double size) {\n    calls++;\n    for (int i = 0; i < 4; i++) {\n        forward(size * calls);\n        left(90);\n    }\n}\n\n" \
               "void main() {\n    IO.println(\"hallo\");\n    color(\"rot\");\n}\n"
        result = run_compiled('java', code, [nil, 'square(10)'])
        program, call = result[:runs]
        assert_equal [['out', "hallo\n"]], program[:items]
        assert_match(/\AIllegalArgumentException: color kennt »rot« nicht/, program[:error][:message])
        assert_equal 13, program[:error][:line]
        # the program ran first, silently, and stopped at its error; the
        # call starts with a new object, so calls is 1 again
        assert_equal [['forward', 10, 6], ['left', 90, 7]] * 4, call[:items]
    end

    def test_dart_draws_and_starts_each_run_afresh
        skip "no Docker image #{KataRunner::IMAGE}" unless docker?
        HotCompile.enabled = false
        code = "var calls = 0;\n\nvoid square(double size) {\n  calls++;\n  for (var i = 0; i < 4; i++) {\n    forward(size * calls);\n    left(90);\n  }\n}\n\n" \
               "void main() {\n  square(5);\n  print('hallo');\n}\n"
        result = run_compiled('dart', code, [nil, 'square(10)'])
        program, call = result[:runs]
        assert_equal [['forward', 5, 6], ['left', 90, 7]] * 4 + [['out', "hallo\n"]], program[:items]
        # main first, silently (calls 1), then the call (calls 2); without a
        # fresh isolate calls would go on from the run before, to 3
        assert_equal [['forward', 20, 6], ['left', 90, 7]] * 4, call[:items]
    end
end
