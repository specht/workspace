require 'json'
require_relative 'dart_check'
require_relative 'java_check'

# Pip's plans in Dart and Java (PipCheck), the two languages that compile
# before they run. Compiling takes longer than the plan (javac about four
# seconds in a Kata container, Dart one and a half), so all maps of a check
# run in one container: the program is compiled once, then started once a
# map, and the first thing it does is tell the world which map it plays
# (»_map«). The shell script marks each start and what came of it in its
# output (MAP_MARK, STATUS_MARK, END_MARK), so output, errors and exit
# status are read back map by map.
#
# - Dart: Pip's commands are top-level functions in pip.dart; the person's
#   code is kata.dart with »import 'pip.dart';« put in front of its first
#   line, so its line numbers stay. run.dart starts the map given on the
#   command line: main of kata.dart, or for a map with call the call
#   (kata.dart imported again without its main, so the call finds the
#   person's functions).
# - Java: Pip's commands are methods of the class Pip; the person's code
#   goes into HsPip extends Pip as members, as in a compact source file of
#   Java 25 (import lines move above the class, their line stays empty).
#   A call becomes a method of HsPip of its own. HsPipRun starts the map.
#
# A plan without main (the clicked plan of the first chapter, or someone
# who writes only commands) is put into one: in Dart on its first line, in
# Java in a line of its own above it.
module PipCompiled
    LANGUAGES = %w[dart java].freeze
    MAP_MARK = '__PIP_MAP__'
    STATUS_MARK = '__PIP_STATUS__'
    END_MARK = '__PIP_END__'
    COMPILE_MARK = '__PIP_COMPILE__'
    # a map whose program runs longer is stopped (exit status 124)
    MAP_SECONDS = 10
    COMPILE_SECONDS = 20
    MAIN = /\bmain\s*\(/
    # a declaration of a function or method of the person's own
    DECLARATION = /^\s*(?:static\s+)?(?:void|int|bool|boolean|String|double|var|List<\w+>)\s+\w+\s*\([^)]*\)\s*\{/
    IMPORT = /\A\s*import\s+[^;]+;\s*\z/

    # true when the code has no main but some map needs one
    def self.wrap?(code, maps)
        maps.any? { |m| !m['call'] } && code.to_s !~ MAIN
    end

    # The message for a plan that needs main and cannot get one: it has
    # functions of its own, which in Java cannot stand inside main.
    def self.main_missing(language, code, maps)
        return nil unless wrap?(code, maps) && code.to_s.lines.any? { |l| l =~ DECLARATION }

        "Dein Plan hat eigene Funktionen, aber kein `void main() { … }`. Dort fängt Pip an: Die Befehle des Plans stehen in `main`, deine Funktionen daneben."
    end

    # --- Dart ---------------------------------------------------------------

    def self.dart_type(command)
        case command
        when 'cave_map' then 'List<String>'
        when 'facing' then 'String'
        else PipCheck::QUESTIONS.include?(command) ? 'bool' : 'void'
        end
    end

    def self.dart_pip(commands)
        names = PipCheck::NAMES.fetch('dart')
        functions = commands.map do |c|
            name = names.fetch(c)
            case dart_type(c)
            when 'void' then "void #{name}() {\n  _pip('#{c}');\n}\n"
            when 'bool' then "bool #{name}() => _pip('#{c}') == true;\n"
            when 'String' then "String #{name}() => _pip('#{c}') as String;\n"
            else "List<String> #{name}() => List<String>.from(_pip('#{c}') as List);\n"
            end
        end
        <<~DART
            import 'dart:convert';
            import 'dart:io';

            Object? _pip(String command, [String? text]) {
              final at = RegExp(r'kata\\.dart:(\\d+)').firstMatch(StackTrace.current.toString());
              final line = at == null ? null : int.parse(at.group(1)!);
              stdout.writeln('\\x1ePIP ' + jsonEncode({'c': command, 'l': line, 't': text}));
              final answer = (stdin.readLineSync() ?? 'end').trim();
              if (answer == 'end') {
                exit(0);
              }
              if (answer.startsWith('[') || answer.startsWith('"')) {
                return jsonDecode(answer);
              }
              return answer == 'true' ? true : (answer == 'false' ? false : null);
            }

            void pipStart(int map) {
              _pip('_map', '$map');
            }

            void pipLoaded() {
              _pip('_loaded');
            }

            void say(Object? text) {
              _pip('say', '$text');
            }

            #{functions.join("\n")}
        DART
    end

    def self.dart_files(check, code, maps)
        lines = code.to_s.gsub("\r\n", "\n").split("\n", -1)
        lines = [''] if lines.empty?
        if wrap?(code, maps)
            lines[0] = "void main() { #{lines[0]}"
            lines << '}'
        end
        lines[0] = "import 'pip.dart'; #{lines[0]}"
        cases = maps.each_with_index.map do |m, i|
            body = m['call'] ? "pipLoaded();\n      #{m['call'].strip.sub(/;?\z/, ';')}" : 'kata.main();'
            "    case #{i}:\n      #{body}\n      break;"
        end
        run = <<~DART
            import 'pip.dart';
            import 'kata.dart' as kata;
            import 'kata.dart' hide main;

            void main(List<String> args) {
              final map = int.parse(args[0]);
              pipStart(map);
              switch (map) {
            #{cases.join("\n")}
              }
            }
        DART
        { 'pip.dart' => dart_pip(check['commands']), 'kata.dart' => lines.join("\n") + "\n", 'run.dart' => run }
    end

    # --- Java ---------------------------------------------------------------

    def self.java_method(command)
        name = PipCheck::NAMES.fetch('java').fetch(command)
        case command
        when 'cave_map' then "    public List<String> #{name}() { return __hsStrings(__hsAsk(\"#{command}\", null)); }\n"
        when 'facing' then "    public String #{name}() { return __hsString(__hsAsk(\"#{command}\", null)); }\n"
        else
            if PipCheck::QUESTIONS.include?(command)
                "    public boolean #{name}() { return \"true\".equals(__hsAsk(\"#{command}\", null)); }\n"
            else
                "    public void #{name}() { __hsAsk(\"#{command}\", null); }\n"
            end
        end
    end

    # Pip.java: the commands; first..last are the lines of the person's code
    # in HsPip.java, for the line a command came from.
    def self.java_pip(commands, first, last)
        <<~JAVA
            import java.io.*;
            import java.util.*;

            public class Pip {
                static final BufferedReader __HS_IN = new BufferedReader(new InputStreamReader(System.in));

                static String __hsJson(String s) {
                    StringBuilder b = new StringBuilder("\\"");
                    for (char c : s.toCharArray()) {
                        if (c == '"' || c == '\\\\') b.append('\\\\').append(c);
                        else if (c < 32) b.append(String.format("\\\\u%04x", (int) c));
                        else b.append(c);
                    }
                    return b.append('"').toString();
                }

                static String __hsAsk(String command, String text) {
                    String line = "null";
                    for (StackTraceElement e : new Throwable().getStackTrace()) {
                        if ("HsPip.java".equals(e.getFileName()) && e.getLineNumber() >= #{first} && e.getLineNumber() <= #{last}) {
                            line = String.valueOf(e.getLineNumber() - #{first - 1});
                            break;
                        }
                    }
                    System.out.println("\\u001ePIP {\\"c\\":" + __hsJson(command) + ",\\"l\\":" + line + ",\\"t\\":" + (text == null ? "null" : __hsJson(text)) + "}");
                    System.out.flush();
                    String answer;
                    try {
                        answer = __HS_IN.readLine();
                    } catch (IOException e) {
                        answer = null;
                    }
                    if (answer == null || answer.trim().equals("end")) {
                        System.out.flush();
                        Runtime.getRuntime().halt(0);
                    }
                    return answer.trim();
                }

                // the world's answers to cave_map and facing are JSON: a list of
                // strings, or a string
                static int __hsAt;

                static String __hsReadString(String s) {
                    StringBuilder b = new StringBuilder();
                    __hsAt++;
                    while (s.charAt(__hsAt) != '"') {
                        char c = s.charAt(__hsAt++);
                        if (c == '\\\\') {
                            char e = s.charAt(__hsAt++);
                            if (e == 'u') {
                                b.append((char) Integer.parseInt(s.substring(__hsAt, __hsAt + 4), 16));
                                __hsAt += 4;
                            } else {
                                b.append(e == 'n' ? '\\n' : e == 't' ? '\\t' : e);
                            }
                        } else {
                            b.append(c);
                        }
                    }
                    __hsAt++;
                    return b.toString();
                }

                static String __hsString(String s) {
                    __hsAt = 0;
                    return __hsReadString(s);
                }

                static List<String> __hsStrings(String s) {
                    List<String> list = new ArrayList<>();
                    __hsAt = 1;
                    while (__hsAt < s.length()) {
                        char c = s.charAt(__hsAt);
                        if (c == '"') list.add(__hsReadString(s));
                        else __hsAt++;
                    }
                    return list;
                }

                static void __hsStart(int map) { __hsAsk("_map", String.valueOf(map)); }

                static void __hsLoaded() { __hsAsk("_loaded", null); }

                public void say(Object text) { __hsAsk("say", String.valueOf(text)); }

            #{commands.map { |c| java_method(c) }.join}}
        JAVA
    end

    # [files, first, last]
    def self.java_files(check, code, maps)
        lines = code.to_s.gsub("\r\n", "\n").split("\n", -1)
        imports = lines.select { |l| l =~ IMPORT }.map(&:strip)
        lines = lines.map { |l| l =~ IMPORT ? '' : l }
        source = +"import java.util.*;\nimport java.util.function.*;\n#{imports.join("\n")}\n\npublic class HsPip extends Pip {\n"
        wrap = wrap?(code, maps)
        source << "void main() {\n" if wrap
        first = source.count("\n") + 1
        source << lines.join("\n") << "\n"
        last = first + lines.size - 1
        source << "}\n" if wrap
        maps.each_with_index do |m, i|
            source << "    void __hsCall#{i}() {\n        #{m['call'].strip.sub(/;?\z/, ';')}\n    }\n" if m['call']
        end
        source << "}\n"
        cases = maps.each_with_index.map do |m, i|
            m['call'] ? "            case #{i} -> { Pip.__hsLoaded(); pip.__hsCall#{i}(); }" : "            case #{i} -> pip.main();"
        end
        run = <<~JAVA
            public class HsPipRun {
                public static void main(String[] args) {
                    int map = Integer.parseInt(args[0]);
                    Pip.__hsStart(map);
                    HsPip pip = new HsPip();
                    switch (map) {
            #{cases.join("\n")}
                        default -> { }
                    }
                }
            }
        JAVA
        [{ 'Pip.java' => java_pip(check['commands'], first, last), 'HsPip.java' => source, 'HsPipRun.java' => run }, first, last]
    end

    # --- the run ------------------------------------------------------------

    # { files:, argv:, timeout:, first:, last: }: compile once, then each map
    # of order, until one ends with an error or runs too long.
    def self.program(check, code, maps, order)
        language = check['language']
        loop_body = lambda do |start|
            "for i in #{order.join(' ')}; do\n" \
                "printf '\\n%s %s\\n' '#{MAP_MARK}' \"$i\"\n" \
                "timeout #{MAP_SECONDS} #{start} \"$i\" 2> err; s=$?\n" \
                "printf '\\n%s %s\\n' '#{STATUS_MARK}' \"$s\"\n" \
                "cat err\n" \
                "printf '\\n%s\\n' '#{END_MARK}'\n" \
                "[ \"$s\" = 0 ] || break\n" \
                "done\n"
        end
        compile_failed = "printf '\\n%s\\n' '#{COMPILE_MARK}'\ncat errors\nexit 0\n"
        if language == 'dart'
            files = dart_files(check, code, maps)
            first = 1
            last = files['kata.dart'].count("\n")
            script = "dart=#{DartCheck::DART}; [ -x \"$dart\" ] || dart=dart\n" \
                     "if ! \"$dart\" compile kernel run.dart -o run.dill > errors 2>&1; then\n#{compile_failed}fi\n" \
                     "#{loop_body.call('"$dart" run.dill')}"
        else
            files, first, last = java_files(check, code, maps)
            jvm = JavaCheck::JVM.join(' ')
            script = "mkdir -p out\n" \
                     "if ! javac -nowarn -encoding UTF-8 #{JavaCheck::JVM.map { |o| "-J#{o}" }.join(' ')} -d out Pip.java HsPip.java HsPipRun.java 2> errors; then\n#{compile_failed}fi\n" \
                     "#{loop_body.call("java #{jvm} -cp out HsPipRun")}"
        end
        timeout = [COMPILE_SECONDS + MAP_SECONDS * order.size, 55].min
        { :files => files, :argv => ['sh', '-c', script], :timeout => timeout, :first => first, :last => last }
    end

    # { compile: errors or nil, maps: { index => { output:, errors:, status:, timed_out: } } }
    def self.sections(stdout)
        text = stdout.to_s
        if (at = text.index("\n#{COMPILE_MARK}\n"))
            return { :compile => text[(at + COMPILE_MARK.size + 2)..].to_s, :maps => {} }
        end

        maps = {}
        text.scan(/\n#{MAP_MARK} (\d+)\n(.*?)\n#{STATUS_MARK} (\d+)\n(.*?)\n#{END_MARK}\n/m) do |index, output, status, errors|
            status = status.to_i
            maps[index.to_i] = { :output => output, :errors => errors, :status => status, :timed_out => status == 124 }
        end
        { :compile => nil, :maps => maps }
    end

    # The person's line for a line of HsPip.java, or nil outside their code.
    def self.own_line(n, first, last)
        n.between?(first, last) ? n - first + 1 : nil
    end

    # Errors as if they came from the person's file: »kata.java:5« for
    # HsPip.java's lines, the class names of the check gone.
    def self.tidy(language, errors, first, last)
        return errors.to_s if language == 'dart'

        errors.to_s.gsub(/HsPip\.java:(\d+)/) { (n = own_line($1.to_i, first, last)) ? "kata.java:#{n}" : 'Prüfung' }
                   .gsub(/\bHsPip(?:Run)?[.$]/, '').gsub(/\bPip\.__hs\w+\([^)]*\)/, 'Pip')
                   .gsub(/Exception in thread "main" /, '')
    end

    # [message, read] for a program that does not compile.
    def self.compile_problem(language, code, errors, first, last)
        lines = errors.to_s.lines.map(&:rstrip)
        if language == 'dart'
            at = lines.index { |l| l =~ /\A(?:kata|run)\.dart:\d+:\d+: Error: / }
            return ['Dein Plan ließ sich nicht übersetzen.', read_block(lines)] if at.nil?

            file, number, message = lines[at].match(/\A(kata|run)\.dart:(\d+):\d+: Error: (.*)\z/).captures
            shown = lines.map { |l| l.sub(/\Akata\.dart:(\d+):\d+: /, 'Zeile \1: ').sub(/\Arun\.dart:\d+:\d+: /, 'Prüfung: ') }
                         .reject { |l| l.include?("import 'pip.dart';") && l !~ /Error/ }
            number = file == 'kata' ? number.to_i : nil
        else
            at = lines.index { |l| l =~ /\AHsPip(?:Run)?\.java:\d+: error: / }
            return ['Dein Plan ließ sich nicht übersetzen.', read_block(lines)] if at.nil?

            file, number, message = lines[at].match(/\A(HsPip(?:Run)?)\.java:(\d+): error: (.*)\z/).captures
            number = file == 'HsPip' ? own_line(number.to_i, first, last) : nil
            detail = lines[(at + 1)..(at + 6)].to_a
            symbol = detail.find { |l| l =~ /\A\s*symbol:/ }.to_s
            message = "#{message} (#{symbol.strip.sub(/\Asymbol:\s*/, '')})" if message.start_with?('cannot find symbol') && !symbol.empty?
            shown = lines.map { |l| l.sub(/\AHsPip\.java:(\d+): /) { (n = own_line($1.to_i, first, last)) ? "Zeile #{n}: " : 'Prüfung: ' } }
                         .map { |l| l.gsub(/\bclass HsPip\b/, 'deinem Plan').gsub(/\bHsPip\./, '') }
        end
        at_line = number ? " (Zeile #{number})" : ''
        ["Dein Plan ließ sich nicht übersetzen#{at_line}: `#{message.to_s[0, 200]}`", read_block(shown)]
    end

    def self.read_block(lines)
        shown = lines.reject(&:empty?).first(8)
        shown.empty? ? nil : "```\n#{shown.join("\n")}\n```"
    end

    # The name in a compiler's message about a name it does not know.
    def self.unknown_name(errors)
        m = errors.to_s.match(/Method not found: '(\w+)'|Undefined name '(\w+)'|The (?:method|function) '(\w+)' isn't defined|symbol:\s+(?:method|variable) (\w+)/)
        m && m.captures.compact.first
    end
end
