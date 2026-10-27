require 'minitest/autorun'
require 'open3'
require 'tmpdir'
require_relative '../include/java_check'
require_relative '../include/dart_check'
require_relative '../include/javascript_check'

# The checks java and dart (JavaCheck, DartCheck): the harness they write
# and how they read what comes back. The compiler messages are the ones
# javac 25 and Dart 3.13 gave in the Workspace image; a real run needs a
# Workspace (docs/aufgaben.md, »java, dart«).
class JavaDartCheckTest < Minitest::Test
    JAVA = {
        'file' => 'bibliothek/Book.java',
        'cases' => [
            { 'new Book("Faust", "Goethe").getTitle()' => '"Faust"' },
            { 'call' => 'Book b = new Book("A", "B"); b.lend(); b.isAvailable()', 'expect' => false, 'say' => 'Ein ausgeliehenes Buch ist nicht mehr da.' },
            { 'call' => 'new Book("", "x")', 'raises' => true },
        ],
    }.freeze

    DART = {
        'file' => 'bibliothek/library.dart',
        'cases' => [
            { "Book('Faust', 'Goethe').title" => "'Faust'" },
            { 'call' => "final b = Book('A', 'B'); b.lend(); b.isAvailable", 'expect' => false },
        ],
    }.freeze

    def test_steps
        assert_equal ['a = Account.new(5)', 'a.deposit(3)', 'a.balance'], CallCheck.steps('a = Account.new(5); a.deposit(3); a.balance')
        assert_equal ['f("x; y")', 'g()'], CallCheck.steps('f("x; y"); g();')
        assert_equal ['h({ a; b })'], CallCheck.steps('h({ a; b })')
    end

    def test_java_harness_runs_the_steps_before_the_value
        source, ranges, = JavaCheck.harness(JavaCheck.cases(JAVA))
        assert_includes source, "            Book b = new Book(\"A\", \"B\");\n            b.lend();\n            Object got = (b.isAvailable());"
        assert_includes source, 'Object expected = (false);'
        assert_includes source, 'add("new Book(\"\", \"x\")", "null", null, true, error(e));', 'a raises case passes on an exception'
        assert_equal 3, ranges.size
        assert ranges.each_cons(2).all? { |a, b| a.last < b.first }
    end

    def test_java_compile_errors_said_for_the_person
        cases = JavaCheck.cases(JAVA)
        _, ranges, = JavaCheck.harness(cases)
        line = ranges[1].first + 3
        missing = "HsHarness.java:#{line}: error: cannot find symbol\n            b.lend();\n             ^\n  symbol:   method lend()\n  location: variable b of type Book\n1 error\n"
        assert_equal '`Book` hat noch keine Methode `lend`. Heißt sie genau so?', JavaCheck.compile_problem(JAVA['file'], cases, missing)[1]
        no_class = "HsHarness.java:#{line}: error: cannot find symbol\n  symbol:   class Book\n  location: class HsHarness\n"
        assert_match(/Die Klasse `Book` gibt es noch nicht.*`Book\.java` im Ordner `bibliothek`/, JavaCheck.compile_problem(JAVA['file'], cases, no_class)[1])
        syntax = "Book.java:3: error: ';' expected\n    public String getTitle() { return \"x\" }\n1 error\n"
        assert_equal "`Book.java` lässt sich nicht übersetzen (Zeile 3): `';' expected`", JavaCheck.compile_problem(JAVA['file'], cases, syntax)[1]
        output = "__HS_COMPILE__\n#{syntax}"
        assert_equal :wrong, JavaCheck.evaluate({}, JAVA, output)[0]
    end

    def test_java_results
        line = %(__HS_RESULT__{"cases":[{"call":"new Book(\\"Faust\\", \\"Goethe\\").getTitle()","expect":"\\"Faust\\"","ok":true,"got":"\\"Faust\\""},) +
               %({"call":"Book b = new Book(\\"A\\", \\"B\\"); b.lend(); b.isAvailable()","expect":"false","ok":false,"got":"true"},) +
               %({"call":"new Book(\\"\\", \\"x\\")","expect":"null","ok":true,"error":"IllegalArgumentException: leer (Book.java, Zeile 5)"}]})
        status, message, = JavaCheck.evaluate({}, JAVA, "\n#{line}\n")
        assert_equal :wrong, status
        assert_equal '`Book b = new Book("A", "B"); b.lend(); b.isAvailable()` ergibt `true`, richtig wäre `false`. Ein ausgeliehenes Buch ist nicht mehr da.', message
    end

    def test_dart_literal_and_harness
        assert_equal %q("Book('Faust', \"x \$y\").title"), DartCheck.literal(%q{Book('Faust', "x $y").title})
        source, = DartCheck.harness(DartCheck.cases(DART))
        assert_includes source, "    final b = Book('A', 'B');\n    b.lend();\n    return (b.isAvailable);"
        assert_includes source, "import '__HS_FILE__';"
    end

    def test_dart_compile_errors_said_for_the_person
        cases = DartCheck.cases(DART)
        _, ranges, = DartCheck.harness(cases)
        line = ranges[1].first + 4
        missing = "harness.dart:#{line}:7: Error: The method 'lend' isn't defined for the type 'Book'.\n - 'Book' is from 'bibliothek/library.dart'.\n"
        assert_equal '`Book` hat noch keine Methode `lend`. Heißt sie genau so?', DartCheck.compile_problem(DART['file'], cases, missing)[1]
        getter = "harness.dart:#{line}:7: Error: The getter 'isAvailable' isn't defined for the type 'Book'.\n"
        assert_equal '`Book` hat noch kein `isAvailable`. Heißt es genau so?', DartCheck.compile_problem(DART['file'], cases, getter)[1]
        syntax = "bibliothek/library.dart:2:16: Error: Expected ';' after this.\n  final String title\n"
        assert_equal "`library.dart` enthält einen Fehler (Zeile 2): `Expected ';' after this.`", DartCheck.compile_problem(DART['file'], cases, syntax)[1]
        assert_equal :wrong, DartCheck.evaluate({}, DART, "__HS_COMPILE__\n#{syntax}")[0]
    end

    def test_javascript_for_a_file_in_the_workspace
        script = JavascriptCheck.command('bibliothek/library.js', JavascriptCheck.cases('cases' => [{ "new Book('A', 'B').title" => "'A'" }]))
        assert_includes script, "cat > \"$t/harness.js\" <<'__HS_HARNESS__'"
        assert_match(/^node "\$t\/harness\.js" bibliothek\/library\.js /, script, 'the file is the first argument, not the script')
        assert_includes JavascriptCheck.spec_errors('file' => 'x.py', 'cases' => [{ 'f()' => 1 }]), 'file must end in .js'
    end

    # A Workspace file may be a module (module.exports, require); a Kata's
    # code may not.
    def test_javascript_modules_only_for_workspace_files
        skip 'node not installed' unless system('node --version', out: File::NULL, err: File::NULL)
        Dir.mktmpdir do |dir|
            File.write(File.join(dir, 'm.js'), "const fs = require('fs');\nclass A { v() { return 2; } }\nmodule.exports = { A };\n")
            cases = JavascriptCheck.cases('cases' => [{ 'new A().v()' => 2 }])
            out, = Open3.capture2('bash', '-c', JavascriptCheck.command('m.js', cases), chdir: dir)
            assert_equal true, JSON.parse(out[/#{JavascriptCheck::RESULT_MARK}(.*)/, 1])['cases'][0]['ok'], out
            _, _, harness, *arguments = JavascriptCheck.argv('m.js', cases)
            File.write(File.join(dir, 'harness.js'), harness)
            out, = Open3.capture2('node', 'harness.js', *arguments, chdir: dir)
            assert_equal 'load', JSON.parse(out[/#{JavascriptCheck::RESULT_MARK}(.*)/, 1])['error']['kind'], 'a Kata has no require'
        end
    end

    def test_spec_errors
        assert_empty JavaCheck.spec_errors(JAVA)
        assert_includes JavaCheck.spec_errors(JAVA.merge('file' => 'x.py')), 'file must end in .java'
        assert_includes DartCheck.spec_errors(DART.merge('setup' => '')), 'setup must be Dart code (text)'
    end
end
