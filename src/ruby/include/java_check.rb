require 'json'
require 'shellwords'
require_relative 'call_check'

# Check type java (docs/aufgaben.md): calls into the Java classes the person
# wrote and compares what comes back, like RubyCheck and CCheck.
#
# Java has no eval, so the harness is written as Java source around the
# calls of the check (HsHarness.java), like CCheck's: a shell script, run in
# the person's Workspace as the guest user that can read /workspace but not
# change it (the code is theirs, rule 1), writes it into a fresh folder in
# /tmp, compiles it with every .java file of the folder the file is in, into
# that /tmp folder, and runs it. Their own main never runs.
#
#   check:
#     type: java
#     file: bibliothek/Book.java       # the file the task is about; its whole folder is compiled
#     setup: |                         # optional: Java in the harness class, helpers for the calls
#       static Book faust() { return new Book("Faust", "Goethe"); }
#     cases:
#       - new Book("Faust", "Goethe").getTitle(): '"Faust"'
#       - call: Book b = faust(); b.lend(); b.isAvailable()
#         expect: false
#         say: Ein ausgeliehenes Buch ist nicht mehr da.
#       - call: new Book("", "Goethe")
#         raises: true
#
# Calls and expected values are Java. A call of several steps, divided by
# »;« (CallCheck.steps), runs the steps before the last, whose value counts;
# each case runs in a method of its own, so names do not clash. true, false,
# ~ and numbers in YAML become true, false, null and the number. Results are
# compared by value: numbers as numbers (an int equals a long, doubles within
# 1e-9), lists and arrays element by element, maps by keys and values, the
# rest with equals; a value is shown like a literal ("Faust", [1, 2]), an
# object of the person's class with its toString. Output of their code is
# dropped. An exception ends only its case and is shown with the file and
# line of their code it came from.
module JavaCheck
    MAX_CASES = CallCheck::MAX_CASES
    RESULT_MARK = CallCheck::RESULT_MARK
    COMPILE_MARK = '__HS_COMPILE__'
    HARNESS = 'HsHarness'
    MAX_ERROR_LINES = 6
    # quick to start rather than quick to run: the calls are short
    JVM = %w[-XX:+UseSerialGC -XX:TieredStopAtLevel=1].freeze

    # The cases of a check as [{ 'call' =>, 'expect' => (Java), 'say' =>, 'raises' => }].
    def self.cases(check)
        CallCheck.cases(check, method(:java_value))
    end

    def self.java_value(value)
        case value
        when true then 'true'
        when false then 'false'
        when nil then 'null'
        else value.to_s
        end
    end

    def self.spec_errors(check)
        errors = CallCheck.spec_errors(check, '.java')
        errors << 'setup must be Java code (text)' if check.key?('setup') && !(check['setup'].is_a?(String) && !check['setup'].strip.empty?)
        errors
    end

    # A Java string literal for text.
    def self.literal(text)
        '"' + text.to_s.gsub('\\') { '\\\\' }.gsub('"', '\\"').gsub("\n", '\\n').gsub("\r", '\\r').gsub("\t", '\\t') + '"'
    end

    # The harness: helpers, the setup of the check, a method per case and a
    # main that runs them. Returns [source, ranges, setup_lines]: for each case
    # the lines of its method, and the lines of the setup.
    def self.harness(cases, setup = nil)
        source = +<<~JAVA
            import java.util.*;

            public class #{HARNESS} {
                static final java.io.PrintStream OUT = System.out;
                static final StringBuilder RESULT = new StringBuilder();

                static String quote(String s) {
                    return "\\"" + s.replace("\\\\", "\\\\\\\\").replace("\\"", "\\\\\\"").replace("\\n", "\\\\n").replace("\\u001B", "\\\\u001B") + "\\"";
                }

                static List<Object> listOf(Object array) {
                    List<Object> list = new ArrayList<>();
                    int n = java.lang.reflect.Array.getLength(array);
                    for (int i = 0; i < n; i++) list.add(java.lang.reflect.Array.get(array, i));
                    return list;
                }

                static String repr(Object v) {
                    if (v == null) return "null";
                    if (v instanceof String s) return quote(s);
                    if (v instanceof Character c) return "'" + c + "'";
                    if (v.getClass().isArray()) return repr(listOf(v));
                    if (v instanceof Collection<?> c) {
                        StringJoiner j = new StringJoiner(", ", "[", "]");
                        for (Object o : c) j.add(repr(o));
                        return j.toString();
                    }
                    if (v instanceof Map<?, ?> m) {
                        StringJoiner j = new StringJoiner(", ", "{", "}");
                        for (Map.Entry<?, ?> e : m.entrySet()) j.add(repr(e.getKey()) + "=" + repr(e.getValue()));
                        return j.toString();
                    }
                    return String.valueOf(v);
                }

                static boolean same(Object a, Object b) {
                    if (a == null || b == null) return a == b;
                    if (a.getClass().isArray()) a = listOf(a);
                    if (b.getClass().isArray()) b = listOf(b);
                    if (a instanceof Number x && b instanceof Number y) {
                        if (x instanceof Double || x instanceof Float || y instanceof Double || y instanceof Float) {
                            double p = x.doubleValue(), q = y.doubleValue();
                            return p == q || Math.abs(p - q) <= 1e-9;
                        }
                        return x.longValue() == y.longValue();
                    }
                    if (a instanceof List<?> x && b instanceof List<?> y) {
                        if (x.size() != y.size()) return false;
                        for (int i = 0; i < x.size(); i++) if (!same(x.get(i), y.get(i))) return false;
                        return true;
                    }
                    if (a instanceof Map<?, ?> x && b instanceof Map<?, ?> y) {
                        if (x.size() != y.size()) return false;
                        for (Object k : x.keySet()) if (!y.containsKey(k) || !same(x.get(k), y.get(k))) return false;
                        return true;
                    }
                    return a.equals(b);
                }

                // where in the person's code an exception came from
                static String error(Throwable e) {
                    String text = e.getClass().getSimpleName() + (e.getMessage() == null ? "" : ": " + e.getMessage());
                    for (StackTraceElement s : e.getStackTrace()) {
                        String file = s.getFileName();
                        if (file != null && !file.equals("#{HARNESS}.java") && s.getLineNumber() > 0 && !s.getClassName().startsWith("java.")) {
                            text += " (" + file + ", Zeile " + s.getLineNumber() + ")";
                            break;
                        }
                    }
                    return text.length() > 300 ? text.substring(0, 300) : text;
                }

                static String json(String s) {
                    StringBuilder b = new StringBuilder("\\"");
                    for (char c : s.toCharArray()) {
                        if (c == '"' || c == '\\\\') b.append('\\\\').append(c);
                        else if (c < 32) b.append(String.format("\\\\u%04x", (int) c));
                        else b.append(c);
                    }
                    return b.append('"').toString();
                }

                static void add(String call, String expect, String got, boolean ok, String error) {
                    if (RESULT.length() > 0) RESULT.append(',');
                    RESULT.append("{\\"call\\":").append(json(call)).append(",\\"expect\\":").append(json(expect)).append(",\\"ok\\":").append(ok);
                    if (got != null) RESULT.append(",\\"got\\":").append(json(got.length() > 300 ? got.substring(0, 300) + " …" : got));
                    if (error != null) RESULT.append(",\\"error\\":").append(json(error));
                    RESULT.append('}');
                }

        JAVA
        setup_lines = nil
        if setup
            first = source.count("\n") + 1
            source << setup.chomp.gsub(/^/, '    ') << "\n\n"
            setup_lines = first..(source.count("\n"))
        end
        ranges = []
        cases.each_with_index do |c, i|
            first = source.count("\n") + 1
            *prepare, last = CallCheck.steps(c['call'])
            body = prepare.map { |step| "            #{step};\n" }.join
            call = literal(c['call'])
            expect = literal(c['expect'])
            source << "    static void case#{i}() {\n"
            if c['raises']
                source << "        try {\n#{body}            Object got = (#{last});\n            add(#{call}, #{expect}, repr(got), false, null);\n"
                source << "        } catch (Throwable e) {\n            add(#{call}, #{expect}, null, true, error(e));\n        }\n"
            else
                source << "        try {\n            Object expected = (#{c['expect']});\n#{body}            Object got = (#{last});\n"
                source << "            add(#{call}, #{expect}, repr(got), same(got, expected), null);\n"
                source << "        } catch (Throwable e) {\n            add(#{call}, #{expect}, null, false, error(e));\n        }\n"
            end
            source << "    }\n\n"
            ranges << (first..source.count("\n"))
        end
        source << <<~JAVA
                public static void main(String[] args) {
                    System.setOut(new java.io.PrintStream(java.io.OutputStream.nullOutputStream()));
            #{cases.each_index.map { |i| "        case#{i}();" }.join("\n")}
                    OUT.println();
                    OUT.println("#{RESULT_MARK}{\\"cases\\":[" + RESULT + "]}");
                    OUT.flush();
                    System.exit(0);
                }
            }
        JAVA
        [source, ranges, setup_lines]
    end

    def self.command_for_check(check)
        command(check['file'], cases(check), check['setup'])
    end

    # The script that prints the result, run in /workspace as the guest.
    def self.command(file, cases, setup = nil)
        source, = harness(cases, setup)
        quoted = Shellwords.escape(file)
        jvm = JVM.join(' ')
        javac_jvm = JVM.map { |o| "-J#{o}" }.join(' ')
        <<~SH
            f="$PWD/"#{quoted}
            if [ ! -f "$f" ]; then printf '\\n%s\\n' '#{RESULT_MARK}{"error":{"kind":"missing"}}'; exit 0; fi
            t=$(mktemp -d) || exit 1
            trap 'rm -rf "$t"' EXIT
            cat > "$t/#{HARNESS}.java" <<'__HS_HARNESS__'
            #{source.chomp}
            __HS_HARNESS__
            d=$(dirname "$f")
            if ! javac -nowarn -encoding UTF-8 #{javac_jvm} -d "$t/out" "$d"/*.java "$t/#{HARNESS}.java" 2> "$t/errors"; then
                echo '#{COMPILE_MARK}'
                sed "s|$t/||g; s|$d/||g" "$t/errors"
                exit 0
            fi
            java #{jvm} -cp "$t/out" #{HARNESS}
        SH
    end

    def self.parse(output)
        CallCheck.parse(output)
    end

    # [status, message, read] for the output of the command.
    def self.evaluate(task, check, output, timed_out: false)
        output = output.to_s
        list = cases(check)
        if !timed_out && !CallCheck.parse(output) && (at = output.index(COMPILE_MARK))
            return compile_problem(check['file'], list, output[(at + COMPILE_MARK.size)..], check['setup'])
        end

        CallCheck.evaluate(check, list, output, language: 'Java', timed_out: timed_out)
    end

    # The first error of the compiler, said for the person: in one of their
    # files, a class or method the check uses that is not there, or a call
    # that does not fit.
    def self.compile_problem(file, cases, errors, setup = nil)
        lines = errors.to_s.lines.map(&:rstrip)
        at = lines.index { |l| l =~ /\A\S+\.java:\d+: error:/ }
        read = lines.reject { |l| l.strip.empty? || l.start_with?('Note:') }.first(MAX_ERROR_LINES)
        read = read.empty? ? nil : "```\n#{read.join("\n").gsub('```', "'''")}\n```"
        return [:wrong, "`#{file}` ließ sich nicht übersetzen.", read] if at.nil?

        where, number, message = lines[at].match(/\A(\S+\.java):(\d+): error: (.*)\z/).captures
        detail = lines[(at + 1)..(at + 5)].to_a
        symbol = detail.find { |l| l =~ /\A\s*symbol:/ }.to_s.sub(/\A\s*symbol:\s*/, '')
        location = detail.find { |l| l =~ /\A\s*location:/ }.to_s.sub(/\A\s*location:\s*/, '')
        owner = location[/(?:class|of type)\s+(\w+)/, 1]
        folder = File.dirname(file)

        if where == "#{HARNESS}.java"
            _, ranges, setup_lines = harness(cases, setup)
            index = ranges.index { |r| r.include?(number.to_i) }
            call = index ? cases[index]['call'] : nil
            if message.start_with?('cannot find symbol')
                if (name = symbol[/\Aclass\s+(\w+)/, 1])
                    return [:wrong, "Die Klasse `#{name}` gibt es noch nicht. Heißt sie genau so, und liegt sie als `#{name}.java` im Ordner `#{folder}`?", read]
                end
                if (name = symbol[/\Amethod\s+(\w+)/, 1])
                    return [:wrong, "#{owner ? "`#{owner}`" : 'Deine Klasse'} hat noch keine Methode `#{name}`. Heißt sie genau so?", read]
                end
                if (name = symbol[/\Avariable\s+(\w+)/, 1])
                    return [:wrong, "`#{name}` kennt Java hier nicht.#{owner ? " Hat `#{owner}` ein `#{name}`, das man von außen sehen darf?" : ''}", read]
                end
            end
            if (name = message[/\Aconstructor (\w+) in class \w+ cannot be applied/, 1])
                return [:wrong, "Der Konstruktor von `#{name}` passt noch nicht zu `#{call}`: Bekommt er dieselben Werte in derselben Reihenfolge?", read] if call
            end
            if (m = message.match(/\A(\w+)\((.*)\) has private access in (\w+)\z/)) || (m = message.match(/\A(\w+) has private access in (\w+)\z/))
                return [:wrong, "`#{m[1]}` ist in `#{m.captures.last}` privat, die Prüfung kommt nicht heran. Soll man es von außen benutzen können?", read]
            end
            return [:wrong, "Der Aufruf `#{call}` passt noch nicht zu deinem Programm. Der Compiler sagt: `#{message}`", read] if call
            return [:wrong, "Die Prüfung passt noch nicht zu deinem Programm. Der Compiler sagt: `#{message}`", read] if setup_lines&.include?(number.to_i)

            return [:wrong, "`#{file}` ließ sich nicht übersetzen: `#{message}`", read]
        end
        [:wrong, "`#{where}` lässt sich nicht übersetzen (Zeile #{number}): `#{message}`", read]
    end
end
