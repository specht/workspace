require 'json'
require_relative 'call_check'
require_relative 'dart_check'
require_relative 'java_check'

# Katas (check type function) in Dart and Java, the two languages that
# compile before they run (KataCheck hands them over). The person's code is
# what they typed into the card, as in the other languages:
#
# - Dart: a library, kata.dart, with top-level functions and classes; the
#   harness of DartCheck imports it and runs the calls (one harness per
#   target, harness_kata.dart, harness_alt1.dart …).
# - Java: methods and classes without a class around them, as a compact
#   source file of Java 25 has them (java kata.java runs it on
#   »Ausführen«). For the check the code goes into a class of its own,
#   HsKata (HsAlt1 … for the typical wrong solutions of same_as), as
#   instance members, as in a compact source file; the cases are instance
#   methods of that class, its helpers have names beginning with __hs, and
#   a class beside it (HsRunHsKata) starts it, so that a main of the
#   person's own never runs and never clashes. import lines move above the
#   class, their line stays empty, so line numbers can be counted back.
#
# forbid: what the code may not use, found in its text with strings and
# comments blanked (RecursionCheck.blank): a function (max), a method of
# any object (.reduce), a static method (Math.max) or a type (HashSet). The
# code then does not run at all.
#
# The run prints, per target, a line with TARGET_MARK and the target, then
# the result of DartCheck's or JavaCheck's kind, or COMPILE_MARK and what the
# compiler said.
module KataCompiled
    LANGUAGES = %w[dart java].freeze
    TARGET_MARK = '__HS_TARGET__'
    COMPILE_MARK = '__HS_COMPILE__'
    RESULT_MARK = CallCheck::RESULT_MARK
    FORBID = /\A(?:(?:[A-Z]\w*)?\.?[a-z_]\w*|[A-Z]\w*)\z/
    MAX_ERROR_LINES = 6
    JVM = JavaCheck::JVM

    def self.target_names(language, count)
        names = ['kata'] + (1..count).map { |i| "alt#{i}" }
        language == 'java' ? names.map { |n| "Hs#{n.capitalize}" } : names
    end

    # --- forbid -------------------------------------------------------------

    def self.forbid_errors(list)
        return [] if list.is_a?(Array) && !list.empty? && list.all? { |f| f.is_a?(String) && f =~ FORBID }

        ['forbid must list functions (max), methods of any object (.reduce), static methods (Math.max) or types (HashSet)']
    end

    # [name, line] of the first forbidden use in code, or nil.
    def self.forbidden(code, list)
        text = RecursionCheck.blank(code.to_s, :javascript)
        (list || []).each do |entry|
            pattern = case entry
                when /\A\.(\w+)\z/ then /\.\s*#{Regexp.escape($1)}\b/
                when /\A([A-Z]\w*)\.(\w+)\z/ then /\b#{Regexp.escape($1)}\s*\.\s*#{Regexp.escape($2)}\b/
                when /\A[A-Z]\w*\z/ then /(?<![\w.$])#{Regexp.escape(entry)}\b/
                else /(?<![\w.$])#{Regexp.escape(entry)}\s*\(/
                end
            m = text.match(pattern) or next
            return [entry.sub(/\A\./, ''), text[0...m.begin(0)].count("\n") + 1]
        end
        nil
    end

    # --- values -------------------------------------------------------------

    def self.literal(value, language)
        case value
        when true then 'true'
        when false then 'false'
        when nil then 'null'
        when Array
            inner = value.map { |v| literal(v, language) }.join(', ')
            language == 'java' ? "List.of(#{inner})" : "[#{inner}]"
        when String then language == 'dart' ? JSON.generate(value).gsub('$', '\\$') : JSON.generate(value)
        else value.to_s
        end
    end

    # The reference of a random case with the values put in place of the
    # names (Dart and Java have no lambda to call on the spot as Ruby has).
    def self.reference(reference, shown)
        pattern = /\b(#{shown.keys.map { |n| Regexp.escape(n) }.join('|')})\b/
        "(#{reference.gsub(pattern) { shown[$1] }})"
    end

    # --- Java ---------------------------------------------------------------

    IMPORT = /\A\s*import\s+(?:static\s+)?[\w.]+(?:\.\*)?\s*;\s*\z/

    # [source, first, last, ranges]: the class name with the person's code in
    # lines first..last and the lines of each case's method.
    def self.java_source(name, code, cases)
        lines = code.to_s.gsub("\r\n", "\n").split("\n", -1)
        imports = lines.select { |l| l =~ IMPORT }.map(&:strip)
        lines = lines.map { |l| l =~ IMPORT ? '' : l }
        # what a compact source file imports by itself (java.base), as far
        # as a Kata needs it
        source = +"import java.util.*;\nimport java.util.function.*;\nimport java.util.stream.*;\nimport java.math.*;\n#{imports.join("\n")}\n\npublic class #{name} {\n"
        first = source.count("\n") + 1
        source << lines.join("\n") << "\n"
        last = first + lines.size - 1
        source << <<~JAVA
            // --- the check ---
            // set by the starter before it silences System.out
            static java.io.PrintStream __HS_OUT;
            static final StringBuilder __HS_RESULT = new StringBuilder();

            static String __hsQuote(String s) {
                return "\\"" + s.replace("\\\\", "\\\\\\\\").replace("\\"", "\\\\\\"").replace("\\n", "\\\\n") + "\\"";
            }

            static List<Object> __hsListOf(Object array) {
                List<Object> list = new ArrayList<>();
                int n = java.lang.reflect.Array.getLength(array);
                for (int i = 0; i < n; i++) list.add(java.lang.reflect.Array.get(array, i));
                return list;
            }

            static String __hsRepr(Object v) {
                if (v == null) return "null";
                if (v instanceof String s) return __hsQuote(s);
                if (v instanceof Character c) return "'" + c + "'";
                if (v.getClass().isArray()) return __hsRepr(__hsListOf(v));
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
                if (a.getClass().isArray()) a = __hsListOf(a);
                if (b.getClass().isArray()) b = __hsListOf(b);
                if (a instanceof Number x && b instanceof Number y) {
                    if (x instanceof Double || x instanceof Float || y instanceof Double || y instanceof Float) {
                        double p = x.doubleValue(), q = y.doubleValue();
                        return p == q || Math.abs(p - q) <= 1e-9;
                    }
                    return x.longValue() == y.longValue();
                }
                if (a instanceof List<?> x && b instanceof List<?> y) {
                    if (x.size() != y.size()) return false;
                    for (int i = 0; i < x.size(); i++) if (!__hsSame(x.get(i), y.get(i))) return false;
                    return true;
                }
                if (a instanceof Set<?> x && b instanceof Set<?> y) {
                    if (x.size() != y.size()) return false;
                    for (Object p : x) {
                        boolean found = false;
                        for (Object q : y) if (__hsSame(p, q)) { found = true; break; }
                        if (!found) return false;
                    }
                    return true;
                }
                if (a instanceof Map<?, ?> x && b instanceof Map<?, ?> y) {
                    if (x.size() != y.size()) return false;
                    for (Object k : x.keySet()) if (!y.containsKey(k) || !__hsSame(x.get(k), y.get(k))) return false;
                    return true;
                }
                return a.equals(b);
            }

            // the line of the person's code an exception came from
            static String __hsError(Throwable e) {
                String text = e.getClass().getSimpleName() + (e.getMessage() == null ? "" : ": " + e.getMessage());
                if (e instanceof StackOverflowError) text = "StackOverflowError";
                for (StackTraceElement s : e.getStackTrace()) {
                    if ("#{name}.java".equals(s.getFileName()) && s.getLineNumber() >= #{first} && s.getLineNumber() <= #{last}) {
                        text += " (Zeile " + (s.getLineNumber() - #{first - 1}) + ")";
                        break;
                    }
                }
                return text.length() > 300 ? text.substring(0, 300) : text;
            }

            static String __hsJson(String s) {
                StringBuilder b = new StringBuilder("\\"");
                for (char c : s.toCharArray()) {
                    if (c == '"' || c == '\\\\') b.append('\\\\').append(c);
                    else if (c < 32) b.append(String.format("\\\\u%04x", (int) c));
                    else b.append(c);
                }
                return b.append('"').toString();
            }

            static void __hsAdd(String call, String expect, String got, boolean ok, String error) {
                if (__HS_RESULT.length() > 0) __HS_RESULT.append(',');
                __HS_RESULT.append("{\\"call\\":").append(__hsJson(call)).append(",\\"expect\\":").append(__hsJson(expect)).append(",\\"ok\\":").append(ok);
                if (got != null) __HS_RESULT.append(",\\"got\\":").append(__hsJson(got.length() > 300 ? got.substring(0, 300) + " …" : got));
                if (error != null) __HS_RESULT.append(",\\"error\\":").append(__hsJson(error));
                __HS_RESULT.append('}');
            }

        JAVA
        ranges = []
        cases.each_with_index do |c, i|
            from = source.count("\n") + 1
            *prepare, last_step = CallCheck.steps(c['call'])
            body = prepare.map { |step| "            #{step};\n" }.join
            call = JavaCheck.literal(c['call'])
            expect = JavaCheck.literal(c['expect'])
            source << "    void __hsCase#{i}() {\n"
            if c['raises']
                source << "        try {\n#{body}            Object got = (#{last_step});\n            __hsAdd(#{call}, #{expect}, __hsRepr(got), false, null);\n"
                source << "        } catch (Throwable e) {\n            __hsAdd(#{call}, #{expect}, null, true, __hsError(e));\n        }\n"
            else
                source << "        try {\n            Object expected = (#{c['expect']});\n#{body}            Object got = (#{last_step});\n"
                source << "            __hsAdd(#{call}, #{expect}, __hsRepr(got), __hsSame(got, expected), null);\n"
                source << "        } catch (Throwable e) {\n            __hsAdd(#{call}, #{expect}, null, false, __hsError(e));\n        }\n"
            end
            source << "    }\n\n"
            ranges << (from..source.count("\n"))
        end
        source << <<~JAVA
                void __hsRun() {
            #{cases.each_index.map { |i| "        __hsCase#{i}();" }.join("\n")}
                    __HS_OUT.println();
                    __HS_OUT.println("#{RESULT_MARK}{\\"cases\\":[" + __HS_RESULT + "]}");
                    __HS_OUT.flush();
                }
            }

            class HsRun#{name} {
                public static void main(String[] args) {
                    #{name}.__HS_OUT = System.out;
                    System.setOut(new java.io.PrintStream(java.io.OutputStream.nullOutputStream()));
                    new #{name}().__hsRun();
                    System.exit(0);
                }
            }
        JAVA
        [source, first, last, ranges]
    end

    # --- the run ------------------------------------------------------------

    # { files:, argv: } for the code and the typical wrong solutions.
    def self.program(language, code, alternatives, cases)
        targets = target_names(language, alternatives.size)
        codes = [code] + alternatives
        if language == 'dart'
            files = {}
            targets.each_with_index do |t, i|
                files["#{t}.dart"] = codes[i].to_s
                files["harness_#{t}.dart"] = DartCheck.harness(cases)[0].sub('__HS_FILE__', "#{t}.dart")
            end
            loop = targets.map do |t|
                "printf '\\n%s %s\\n' '#{TARGET_MARK}' '#{t}'\n" \
                    "\"$dart\" harness_#{t}.dart > out 2> errors\n" \
                    "if grep -q '#{RESULT_MARK}' out; then cat out; else echo '#{COMPILE_MARK}'; cat errors; fi"
            end
            script = "dart=#{DartCheck::DART}; [ -x \"$dart\" ] || dart=dart\n#{loop.join("\n")}\n"
        else
            files = targets.each_with_index.to_h { |t, i| ["#{t}.java", java_source(t, codes[i], cases)[0]] }
            javac = "javac -nowarn -encoding UTF-8 #{JVM.map { |o| "-J#{o}" }.join(' ')} -d out #{targets.map { |t| "#{t}.java" }.join(' ')}"
            runs = targets.map { |t| "printf '\\n%s %s\\n' '#{TARGET_MARK}' '#{t}'\njava #{JVM.join(' ')} -cp out HsRun#{t}" }
            script = "if #{javac} 2> errors; then\n#{runs.join("\n")}\nelse\n" \
                     "printf '\\n%s %s\\n%s\\n' '#{TARGET_MARK}' '#{targets.first}' '#{COMPILE_MARK}'\ncat errors\nfi\n"
        end
        { :files => files, :argv => ['sh', '-c', script], :targets => targets }
    end

    # {target => output} of a run.
    def self.sections(stdout)
        stdout.to_s.split(/^#{TARGET_MARK} /o).drop(1).to_h do |part|
            name, rest = part.split("\n", 2)
            [name.strip, rest.to_s]
        end
    end

    # [status, message, read] for the person's section.
    def self.evaluate(language, check, code, cases, output, timed_out:)
        if !timed_out && !CallCheck.parse(output) && (at = output.index(COMPILE_MARK))
            errors = output[(at + COMPILE_MARK.size)..]
            return language == 'dart' ? dart_problem(cases, errors) : java_problem(code, cases, errors)
        end
        status, message, read = CallCheck.evaluate(check.merge('file' => nil), cases, output, language: language == 'dart' ? 'Dart' : 'Java', timed_out: timed_out)
        tidy = ->(text) { text&.gsub(/\(kata\.dart, Zeile (\d+)\)/, '(Zeile \1)') }
        [status, tidy.call(message), tidy.call(read)]
    end

    def self.read_block(lines)
        lines = lines.reject { |l| l.strip.empty? || l.start_with?('Note:') }.first(MAX_ERROR_LINES)
        lines.empty? ? nil : "```\n#{lines.join("\n").gsub('```', "'''")}\n```"
    end

    # The first error Dart reports, said for the person.
    def self.dart_problem(cases, errors)
        lines = errors.to_s.lines.map(&:rstrip).map { |l| l.sub(%r{\A(?:file://)?\S*/(?=[^/\s]+\.dart:)}, '') }
        at = lines.index { |l| l =~ /\A\S+\.dart:\d+:\d+: Error: / }
        shown = lines.map { |l| l.sub(/\Akata\.dart:(\d+):(\d+):/, 'Zeile \1:') }
        read = read_block(shown)
        return [:wrong, 'Dein Code ließ sich nicht ausführen.', read] if at.nil?

        where, number, message = lines[at].match(/\A(\S+\.dart):(\d+):\d+: Error: (.*)\z/).captures
        return [:wrong, "In deinem Code, Zeile #{number}, versteht Dart etwas nicht: `#{message}`", read] if where == 'kata.dart'

        _, ranges, = DartCheck.harness(cases)
        index = ranges.index { |r| r.include?(number.to_i) }
        call = index ? cases[index]['call'] : nil
        if (name = message[/\A(?:Method not found|Undefined name|Type not found|Couldn't find constructor): '(\w+)'/, 1])
            return [:wrong, "In deinem Code gibt es `#{name}` noch nicht. Heißt es genau so?", read]
        end
        if (m = message.match(/\AThe (method|getter|setter) '(\w+)' isn't defined for the (?:class|type) '(\w+)'/))
            said = m[1] == 'method' ? "keine Methode `#{m[2]}`. Heißt sie genau so?" : "kein `#{m[2]}`. Heißt es genau so?"
            return [:wrong, "`#{m[3]}` hat noch #{said}", read]
        end
        return [:wrong, "Der Aufruf `#{call}` passt noch nicht zu deinem Code. Dart sagt: `#{message}`", read] if call

        [:wrong, "Dein Code ließ sich nicht ausführen. Dart sagt: `#{message}`", read]
    end

    # The first error of the compiler, said for the person, with the lines
    # counted in their code.
    def self.java_problem(code, cases, errors)
        _, first, last, ranges = java_source('HsKata', code, cases)
        own = ->(n) { n.between?(first, last) }
        lines = errors.to_s.lines.map(&:rstrip)
        shown = lines.map do |l|
            l.sub(/\AHsKata\.java:(\d+):/) { own.call($1.to_i) ? "Zeile #{$1.to_i - first + 1}:" : 'Prüfung:' }
             .gsub(/ in class HsKata\b/, '').gsub(/\bclass HsKata\b/, 'deinem Code')
        end
        read = read_block(shown)
        at = lines.index { |l| l =~ /\AHsKata\.java:\d+: error:/ }
        return [:wrong, 'Dein Code ließ sich nicht übersetzen.', read] if at.nil?

        number, message = lines[at].match(/\AHsKata\.java:(\d+): error: (.*)\z/).captures
        number = number.to_i
        message = message.gsub(/ in class HsKata\b/, '')
        detail = lines[(at + 1)..(at + 6)].to_a
        field = ->(key) { detail.find { |l| l =~ /\A\s*#{key}:/ }.to_s.sub(/\A\s*#{key}:\s*/, '') }
        if own.call(number)
            return [:wrong, "In deinem Code, Zeile #{number - first + 1}, versteht Java etwas nicht: `#{message}`", read]
        end

        index = ranges.index { |r| r.include?(number) }
        call = index ? cases[index]['call'] : nil
        if message.start_with?('cannot find symbol')
            symbol = field.call('symbol')
            return [:wrong, "In deinem Code gibt es noch keine Methode `#{$1}`. Heißt sie genau so?", read] if symbol =~ /\Amethod\s+(\w+)/
            return [:wrong, "Die Klasse `#{$1}` gibt es in deinem Code noch nicht. Heißt sie genau so?", read] if symbol =~ /\Aclass\s+(\w+)/
            return [:wrong, "`#{$1}` kennt Java hier nicht. Heißt es in deinem Code genau so?", read] if symbol =~ /\Avariable\s+(\w+)/
        end
        if message =~ /\A(?:method|constructor) (\w+) .*cannot be applied to given types/ && call
            required = field.call('required')
            found = field.call('found')
            unless required.empty?
                return [:wrong, "Der Aufruf `#{call}` passt nicht zu `#{$1}` in deinem Code: Dort steht als Parameter `#{required}`, die Prüfung gibt `#{found}`.", read]
            end
        end
        if call && message.start_with?('incompatible types')
            return [:wrong, "Der Aufruf `#{call}` passt nicht zu den Typen in deinem Code: Haben die Parameter deiner Methode die Typen, die die Aufgabe nennt? Der Compiler sagt: `#{message}`", read]
        end
        return [:wrong, "Der Aufruf `#{call}` passt noch nicht zu deinem Code. Der Compiler sagt: `#{message}`", read] if call

        [:wrong, "Dein Code ließ sich nicht übersetzen. Der Compiler sagt: `#{message}`", read]
    end

    # What runs on »Ausführen«: Dart with its main, Java as a compact source
    # file (Java 25), which needs a main too.
    def self.run_argv(language)
        if language == 'dart'
            ['sh', '-c', "dart=#{DartCheck::DART}; [ -x \"$dart\" ] || dart=dart; exec \"$dart\" kata.dart"]
        else
            ['java', *JVM, 'kata.java']
        end
    end

    def self.has_main?(code)
        RecursionCheck.blank(code.to_s, :javascript).match?(/\bmain\s*\(/)
    end
end
