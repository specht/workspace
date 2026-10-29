require 'json'
require_relative 'dart_check'
require_relative 'java_check'

# Check type tests (TestsCheck) in Dart and Java, which compile before they
# run. Each version (the right one and the broken ones) gets the person's
# tests beside it, all compiled in one go; then each version runs as a
# process of its own, so that one that loops forever stops alone.
#
# - Dart: harness.dart has assertEqual; version<i>.dart is a version;
#   tests<i>.dart imports both and has the person's lines as the body of
#   hsTests, starting on its first line, so line numbers stay. run.dart
#   runs the tests of the version given on the command line.
# - Java: HsTests has assertEqual; HsV<i> extends it with the person's
#   lines as the body of __hsTests (at the same line in every file) and
#   the version as members. HsTestsRun runs version i.
#
# Each run prints TestsCheck::MARK and its report as JSON, as in the other
# languages; a program that does not compile prints COMPILE_MARK and the
# compiler's errors instead.
module TestsCompiled
    LANGUAGES = %w[dart java].freeze
    COMPILE_MARK = '__TESTS_COMPILE__'
    VERSION_SECONDS = 10
    IMPORT = /\A\s*import\s+[^;]+;\s*\z/

    DART_HARNESS = <<~'DART'
        import 'dart:convert';

        var _count = 0;
        final _failures = <List<Object?>>[];

        bool _same(Object? a, Object? b) {
          if (a is List && b is List) {
            if (a.length != b.length) return false;
            for (var i = 0; i < a.length; i++) {
              if (!_same(a[i], b[i])) return false;
            }
            return true;
          }
          if (a is Map && b is Map) {
            if (a.length != b.length) return false;
            for (final key in a.keys) {
              if (!b.containsKey(key) || !_same(a[key], b[key])) return false;
            }
            return true;
          }
          if (a is Set && b is Set) return a.length == b.length && a.containsAll(b);
          if (a is bool || b is bool) return a == b;
          return a == b;
        }

        String repr(Object? v) {
          if (v is String) return "'" + v.replaceAll('\\', '\\\\').replaceAll("'", "\\'") + "'";
          if (v is List) return '[' + v.map(repr).join(', ') + ']';
          if (v is Set) return '{' + v.map(repr).join(', ') + '}';
          if (v is Map) return '{' + v.entries.map((e) => repr(e.key) + ': ' + repr(e.value)).join(', ') + '}';
          return '$v';
        }

        int? lineOf(String trace) {
          final at = RegExp(r'tests\d+\.dart:(\d+)').firstMatch(trace);
          return at == null ? null : int.parse(at.group(1)!);
        }

        bool assertEqual(Object? expected, Object? actual) {
          _count++;
          if (_same(expected, actual)) return true;
          _failures.add([lineOf(StackTrace.current.toString()), repr(expected), repr(actual)]);
          return false;
        }

        void report(List<Object?>? error) {
          print('__TESTS__' + jsonEncode({'count': _count, 'failures': _failures.take(5).toList(), 'error': error}));
        }
    DART

    def self.java_harness(first, last)
        <<~JAVA
            import java.util.*;

            public abstract class HsTests {
                static int __hsCount = 0;
                static final List<String> __hsFailures = new ArrayList<>();

                abstract void __hsTests();

                static String __hsJson(String s) {
                    StringBuilder b = new StringBuilder("\\"");
                    for (char c : s.toCharArray()) {
                        if (c == '"' || c == '\\\\') b.append('\\\\').append(c);
                        else if (c < 32) b.append(String.format("\\\\u%04x", (int) c));
                        else b.append(c);
                    }
                    return b.append('"').toString();
                }

                static String __hsRepr(Object v) {
                    if (v == null) return "null";
                    if (v instanceof String s) return "\\"" + s.replace("\\\\", "\\\\\\\\").replace("\\"", "\\\\\\"") + "\\"";
                    if (v instanceof Character c) return "'" + c + "'";
                    if (v instanceof Collection<?> c) {
                        StringJoiner j = new StringJoiner(", ", "[", "]");
                        for (Object o : c) j.add(__hsRepr(o));
                        return j.toString();
                    }
                    if (v instanceof Map<?, ?> m) {
                        StringJoiner j = new StringJoiner(", ", "{", "}");
                        for (Map.Entry<?, ?> e : m.entrySet()) j.add(__hsRepr(e.getKey()) + "=" + __hsRepr(e.getValue()));
                        return j.toString();
                    }
                    return String.valueOf(v);
                }

                static boolean __hsSame(Object a, Object b) {
                    if (a == null || b == null) return a == b;
                    if (a instanceof Number x && b instanceof Number y) {
                        if (x instanceof Double || x instanceof Float || y instanceof Double || y instanceof Float) {
                            return x.doubleValue() == y.doubleValue();
                        }
                        return x.longValue() == y.longValue();
                    }
                    if (a instanceof List<?> x && b instanceof List<?> y) {
                        if (x.size() != y.size()) return false;
                        for (int i = 0; i < x.size(); i++) if (!__hsSame(x.get(i), y.get(i))) return false;
                        return true;
                    }
                    return a.equals(b);
                }

                // the line of the person's tests a call came from
                static String __hsLine(StackTraceElement[] trace) {
                    for (StackTraceElement e : trace) {
                        if (e.getFileName() != null && e.getFileName().matches("HsV\\\\d+\\\\.java")
                                && e.getLineNumber() >= #{first} && e.getLineNumber() <= #{last}) {
                            return String.valueOf(e.getLineNumber() - #{first - 1});
                        }
                    }
                    return "null";
                }

                public boolean assertEqual(Object expected, Object actual) {
                    __hsCount++;
                    if (__hsSame(expected, actual)) return true;
                    __hsFailures.add("[" + __hsLine(new Throwable().getStackTrace()) + "," + __hsJson(__hsRepr(expected)) + "," + __hsJson(__hsRepr(actual)) + "]");
                    return false;
                }

                static void __hsReport(String error) {
                    List<String> first = __hsFailures.subList(0, Math.min(5, __hsFailures.size()));
                    System.out.println("__TESTS__{\\"count\\":" + __hsCount + ",\\"failures\\":[" + String.join(",", first) + "],\\"error\\":" + error + "}");
                    System.out.flush();
                }
            }
        JAVA
    end

    # { files:, argv:, timeout:, first:, last: }
    def self.program(language, code, versions)
        lines = code.to_s.gsub("\r\n", "\n").split("\n", -1)
        lines = [''] if lines.empty?
        loop_body = lambda do |start|
            "for i in #{versions.each_index.to_a.join(' ')}; do\n" \
                "timeout #{VERSION_SECONDS} #{start} \"$i\"\necho\ndone\n"
        end
        compile_failed = "printf '\\n%s\\n' '#{COMPILE_MARK}'\ncat errors\nexit 0\n"
        files = {}
        if language == 'dart'
            files['harness.dart'] = DART_HARNESS
            imports = []
            cases = []
            versions.each_with_index do |v, i|
                files["version#{i}.dart"] = v
                body = lines.dup
                body[0] = "import 'harness.dart'; import 'version#{i}.dart'; void hsTests() { #{body[0]}"
                files["tests#{i}.dart"] = body.join("\n") + "\n}\n"
                imports << "import 'tests#{i}.dart' as t#{i};"
                cases << "    case #{i}:\n      t#{i}.hsTests();\n      break;"
            end
            files['run.dart'] = <<~DART
                import 'harness.dart';
                #{imports.join("\n")}

                void main(List<String> args) {
                  List<Object?>? error;
                  try {
                    switch (int.parse(args[0])) {
                #{cases.join("\n")}
                    }
                  } catch (e, trace) {
                    final text = '$e';
                    error = [lineOf(trace.toString()), text.length > 300 ? text.substring(0, 300) : text];
                  }
                  report(error);
                }
            DART
            script = "dart=#{DartCheck::DART}; [ -x \"$dart\" ] || dart=dart\n" \
                     "if ! \"$dart\" compile kernel run.dart -o run.dill > errors 2>&1; then\n#{compile_failed}fi\n" \
                     "#{loop_body.call('"$dart" run.dill')}"
            first = 1
            last = lines.size
        else
            imports = (lines + versions.flat_map { |v| v.lines.map(&:chomp) }).select { |l| l =~ IMPORT }.map(&:strip).uniq
            own = lines.map { |l| l =~ IMPORT ? '' : l }
            header = "import java.util.*;\nimport java.util.function.*;\n#{imports.join("\n")}\n\n"
            first = nil
            versions.each_with_index do |v, i|
                source = +"#{header}public class HsV#{i} extends HsTests {\nvoid __hsTests() {\n"
                first = source.count("\n") + 1
                source << own.join("\n") << "\n}\n"
                source << v.lines.map { |l| l =~ IMPORT ? "\n" : l }.join << "\n}\n"
                files["HsV#{i}.java"] = source
            end
            last = first + own.size - 1
            files['HsTests.java'] = java_harness(first, last)
            cases = versions.each_index.map { |i| "            case #{i} -> new HsV#{i}();" }
            files['HsTestsRun.java'] = <<~JAVA
                public class HsTestsRun {
                    public static void main(String[] args) {
                        HsTests tests = switch (Integer.parseInt(args[0])) {
                #{cases.join("\n")}
                            default -> null;
                        };
                        String error = "null";
                        try {
                            tests.__hsTests();
                        } catch (Throwable e) {
                            String text = e instanceof StackOverflowError ? "StackOverflowError" : e.getClass().getSimpleName() + (e.getMessage() == null ? "" : ": " + e.getMessage());
                            if (text.length() > 300) text = text.substring(0, 300);
                            error = "[" + HsTests.__hsLine(e.getStackTrace()) + "," + HsTests.__hsJson(text) + "]";
                        }
                        HsTests.__hsReport(error);
                        System.exit(0);
                    }
                }
            JAVA
            jvm = JavaCheck::JVM.join(' ')
            names = versions.each_index.map { |i| "HsV#{i}.java" }.join(' ')
            script = "mkdir -p out\n" \
                     "if ! javac -nowarn -encoding UTF-8 #{JavaCheck::JVM.map { |o| "-J#{o}" }.join(' ')} -d out HsTests.java #{names} HsTestsRun.java 2> errors; then\n#{compile_failed}fi\n" \
                     "#{loop_body.call("java #{jvm} -cp out HsTestsRun")}"
        end
        timeout = [20 + VERSION_SECONDS * versions.size, 55].min
        { :files => files, :argv => ['sh', '-c', script], :timeout => timeout, :first => first, :last => last }
    end

    # The message for tests that do not compile, with the person's line.
    def self.compile_problem(language, errors, first, last)
        lines = errors.to_s.lines.map(&:rstrip)
        if language == 'dart'
            at = lines.index { |l| l =~ /\Atests\d+\.dart:\d+:\d+: Error: / }
            number, message = at && lines[at].match(/\Atests\d+\.dart:(\d+):\d+: Error: (.*)\z/).captures
        else
            at = lines.index { |l| l =~ /\AHsV\d+\.java:\d+: error: / }
            if at
                n, message = lines[at].match(/\AHsV\d+\.java:(\d+): error: (.*)\z/).captures
                number = n.to_i.between?(first, last) ? n.to_i - first + 1 : nil
                symbol = lines[(at + 1)..(at + 4)].to_a.find { |l| l =~ /\A\s*symbol:/ }
                message = "#{message} (#{symbol.strip.sub(/\Asymbol:\s*/, '')})" if symbol && message.start_with?('cannot find symbol')
            end
        end
        return "Deine Tests ließen sich nicht übersetzen:\n\n```\n#{lines.first(6).join("\n")[0, 600]}\n```" unless message

        "Deine Tests ließen sich nicht übersetzen#{number ? " (Zeile #{number})" : ''}: `#{message[0, 200]}`"
    end
end
