require 'json'
require_relative 'dart_check'
require_relative 'java_check'
require_relative 'hot_compile'
require_relative 'kata_runner'
require_relative 'turtle_world'

# The turtle in Java and Dart (docs/katas.md, »Turtle«), the two languages
# that compile: they run on the server, in a Kata container, through the
# Katas' queue (/api/turtle/run), and the page replays what came back as it
# replays the browser's worker (turtle_canvas.js, ServerRunner). All runs of
# a card go into one container and one process: the program, and for a card
# with calls each call from the start, without drawing the program first,
# as in the worker.
#
# - Java: the commands are methods of Turtle (Turtle.java); the person's
#   code goes into HsTurtle extends Turtle as members, as in a compact
#   source file (import lines move above the class, their line stays
#   empty). A call becomes a method of its own; each run gets a new
#   HsTurtle, so its fields start again.
# - Dart: the commands are top-level functions in turtle.dart; kata.dart is
#   the person's code with »import 'turtle.dart';« in front of its first
#   line. Each run is an isolate of its own, so that top-level variables
#   start again.
#
# Code without main and without functions of its own (the first cards:
# just commands) is put into main, as Pip's plans are. Code with functions
# and without main has nothing that runs by itself: a card with calls needs
# no more.
#
# Each command writes a line »\u001e command \t value \t line«; START, END,
# ERROR and LIMIT lines mark the runs. What the program prints stands in
# between.
module TurtleCompiled
    LANGUAGES = %w[java dart].freeze
    PRELUDES = File.expand_path('../../static/include/turtle', __dir__)
    COLORS = File.join(PRELUDES, 'colors.json')
    MARK = "\u001e"
    COMPILE_MARK = '__TURTLE_COMPILE__'
    # how long the program may run, all runs together, after compiling
    RUN_SECONDS = 10
    COMPILE_SECONDS = 20
    # 200 000 commands of about 16 bytes, and what the program prints
    MAX_OUTPUT = 8 * 1024 * 1024
    MAIN = /\bmain\s*\(/
    DECLARATION = /^\s*(?:static\s+)?(?:void|int|bool|boolean|String|double|num|var|List<[\w<>, ]+>)\s+\w+\s*\([^)]*\)\s*\{/
    IMPORT = /\A\s*import\s+(?:static\s+)?[\w.]+(?:\.\*)?\s*;\s*\z/
    # what javac and Dart say about a command outside every function
    STRAY = /invalid method declaration|<identifier> expected|class, interface, enum, or record expected|illegal start of type|Expected an identifier|Expected a function body|Expected a declaration|Variables must be declared|is already declared in this scope/
    MAIN_MISSING = 'Dein Programm hat eigene Funktionen, aber kein `void main() { … }`. Dort fängt die Schildkröte an: ' \
                   'Die Befehle, die zuerst laufen, stehen in `main`, deine Funktionen daneben.'

    # true when the code is only commands: they go into main
    def self.wrap?(code)
        code.to_s !~ MAIN && code.to_s.lines.none? { |l| l =~ DECLARATION }
    end

    def self.main?(code)
        code.to_s =~ MAIN ? true : false
    end

    def self.prelude(name)
        File.read(File.join(PRELUDES, name))
    end

    # --- Java ---------------------------------------------------------------

    # [files, first, last]: first..last are the person's lines in HsTurtle.java
    def self.java_files(code, runs)
        lines = code.to_s.gsub("\r\n", "\n").split("\n", -1)
        imports = lines.select { |l| l =~ IMPORT }.map(&:strip)
        lines = lines.map { |l| l =~ IMPORT ? '' : l }
        source = +"import java.util.*;\nimport java.util.function.*;\n#{imports.join("\n")}\n\npublic class HsTurtle extends Turtle {\n"
        wrap = wrap?(code)
        source << "void main() {\n" if wrap
        first = source.count("\n") + 1
        source << lines.join("\n") << "\n"
        last = first + lines.size - 1
        source << "}\n" if wrap
        runs.each_with_index do |call, i|
            source << "    void __hsCall#{i}() {\n        #{call.strip.sub(/;?\z/, ';')}\n    }\n" if call
        end
        source << "}\n"
        main = wrap || main?(code) ? 't.main();' : ''
        steps = runs.each_with_index.map do |call, i|
            body = call ? "#{main} Turtle.loaded(); t.__hsCall#{i}();" : main
            <<~JAVA.gsub(/^/, '        ')
                {
                    Turtle.begin(#{i}, #{call ? 'true' : 'false'});
                    HsTurtle t = new HsTurtle();
                    try {
                        #{body}
                    } catch (Turtle.Stop s) {
                        System.out.flush();
                        return;
                    } catch (Throwable e) {
                        Turtle.error(e);
                    }
                    Turtle.end(#{i});
                }
            JAVA
        end
        run = <<~JAVA
            public class HsTurtleRun {
                public static void main(String[] args) throws Exception {
                    Turtle.start(#{first}, #{last});
            #{steps.join}    }
            }
        JAVA
        [{ 'Turtle.java' => prelude('Turtle.java'), 'HsTurtle.java' => source, 'HsTurtleRun.java' => run }, first, last]
    end

    # --- Dart ---------------------------------------------------------------

    def self.dart_files(code, runs)
        lines = code.to_s.gsub("\r\n", "\n").split("\n", -1)
        lines = [''] if lines.empty?
        wrap = wrap?(code)
        if wrap
            lines[0] = "void main() { #{lines[0]}"
            lines << '}'
        end
        lines[0] = "import 'turtle.dart'; #{lines[0]}"
        main = wrap || main?(code) ? 'kata.main();' : ''
        steps = runs.each_with_index.map do |call, i|
            body = call ? "#{main} turtleLoaded(); #{call.strip.sub(/;?\z/, ';')}" : main
            "  if (!await Isolate.run(() => turtleRun(#{i}, #{call ? 'true' : 'false'}, () { #{body} }))) return;\n"
        end
        run = +"import 'dart:isolate';\nimport 'turtle.dart';\n"
        # always: also code that only declares compiles, with its errors
        run << "import 'kata.dart' as kata;\n"
        run << "import 'kata.dart' hide main;\n" if runs.any? { |call| call }
        run << "\nFuture<void> main() async {\n#{steps.join}}\n"
        { 'turtle.dart' => prelude('turtle.dart'), 'kata.dart' => lines.join("\n") + "\n", 'run.dart' => run }
    end

    # --- the run ------------------------------------------------------------

    # { files:, argv:, timeout:, first:, last:, hot: } for the code and the
    # runs (TurtleCheck.runs: nil for the program, a call as text).
    def self.program(language, code, runs)
        compile_failed = "printf '\\n%s\\n' '#{COMPILE_MARK}'\ncat errors\nexit 0\n"
        if language == 'dart'
            files = dart_files(code, runs)
            first = 1
            last = files['kata.dart'].count("\n")
            script = "dart=#{DartCheck::DART}; [ -x \"$dart\" ] || dart=dart\n" \
                     "#{HotCompile.step('run', '"$dart" compile kernel run.dart -o run.dill > errors 2>&1', compile_failed)}" \
                     "timeout #{RUN_SECONDS} \"$dart\" run.dill\n"
            hot = { :language => 'dart', :entries => ['run.dart'] }
        else
            files, first, last = java_files(code, runs)
            javac = "javac -nowarn -encoding UTF-8 #{JavaCheck::JVM.map { |o| "-J#{o}" }.join(' ')} -d out Turtle.java HsTurtle.java HsTurtleRun.java 2> errors"
            script = "mkdir -p out\n" \
                     "#{HotCompile.step('javac', javac, compile_failed)}" \
                     "timeout #{RUN_SECONDS} java #{JavaCheck::JVM.join(' ')} -cp out HsTurtleRun\n"
            hot = { :language => 'java', :entries => ['javac'] }
        end
        files['colors.json'] = File.read(COLORS)
        { :files => files, :argv => ['sh', '-c', script], :timeout => COMPILE_SECONDS + RUN_SECONDS + 5,
          :first => first, :last => last, :hot => hot }
    end

    # Runs the code with kata (a lambda as KataCheck's, KataRunner.run's
    # arguments) and answers what the worker would have sent, per run:
    # { runs: [{ items: [[command, value, line] or ['out', text]], error:
    # { message, line, compile } or nil }], limit: nil, 'commands' or
    # 'output' }, or nil when nothing could run (the queue gave up).
    def self.run(language, code, runs, kata)
        program = program(language, code, runs)
        result = kata.call(:files => HotCompile.files(program), :argv => program[:argv], :timeout => program[:timeout],
                           :max_output => MAX_OUTPUT)
        return nil if result.nil?

        parse(language, code, result.stdout.to_s, runs.size, program, :timed_out => result.timed_out)
    end

    def self.parse(language, code, stdout, count, program, timed_out: false)
        if (at = stdout.index("\n#{COMPILE_MARK}\n"))
            error = compile_error(language, code, stdout[(at + COMPILE_MARK.size + 2)..].to_s, program[:first], program[:last])
            return { :runs => [{ :items => [], :error => error }], :limit => nil }
        end

        runs = []
        limit = nil
        current = nil
        ended = 0
        # the text between records, as it came: a record starts a line of
        # its own only after a newline the program printed
        stdout.split(MARK).each_with_index do |part, i|
            if i.zero?
                next
            end

            record, text = part.split("\n", 2)
            fields = record.to_s.split("\t", -1)
            case fields[0]
            when 'START' then runs << (current = { :items => [], :error => nil })
            when 'END'
                ended += 1
                current = nil
            when 'LIMIT' then limit = fields[1] == 'output' ? 'output' : 'commands'
            when 'ERROR'
                line = fields[1].to_i
                current[:error] ||= { :message => fields[2..].join("\t"), :line => line.positive? ? line : nil } if current
            else
                if current && TurtleWorld::COMMANDS.include?(fields[0])
                    current[:items] << item(fields)
                end
            end
            current[:items] << ['out', text] if current && text && !text.empty?
        end
        # a run without its end: the program stopped (exit) or was stopped
        if ended < count && limit.nil? && (runs.empty? || runs.last[:error].nil?)
            message = timed_out ? "Dein Programm lief länger als #{RUN_SECONDS} Sekunden." : 'Dein Programm hat vorzeitig aufgehört.'
            runs << { :items => [], :error => nil } if runs.empty?
            runs.last[:error] = { :message => message, :line => nil }
        end
        { :runs => runs, :limit => limit }
    end

    # [command, value, line] with numbers as numbers and colours as hex,
    # as turtle_worker.js sends them
    def self.item(fields)
        command, value, line = fields
        value = case command
            when 'forward', 'back', 'left', 'right', 'width' then number(value)
            when 'color', 'background' then TurtleWorld.color_hex(value) || value
            end
        line = line.to_i
        [command, value, line.positive? ? line : nil]
    end

    def self.number(text)
        Integer(text, exception: false) || Float(text, exception: false)
    end

    # { message, line, compile: true } for a program that does not compile.
    def self.compile_error(language, code, errors, first, last)
        lines = errors.to_s.lines.map(&:rstrip)
        number = nil
        if language == 'dart'
            lines = lines.map { |l| l.sub(%r{\A(?:file://)?\S*/(?=[^/\s]+\.dart:)}, '') }
            at = lines.index { |l| l =~ /\A(?:kata|run)\.dart:\d+:\d+: Error: / }
            if at
                file, n, message = lines[at].match(/\A(kata|run)\.dart:(\d+):\d+: Error: (.*)\z/).captures
                number = file == 'kata' ? n.to_i : nil
            end
        else
            at = lines.index { |l| l =~ /\AHsTurtle(?:Run)?\.java:\d+: error: / }
            if at
                file, n, message = lines[at].match(/\A(HsTurtle(?:Run)?)\.java:(\d+): error: (.*)\z/).captures
                number = file == 'HsTurtle' && n.to_i.between?(first, last) ? n.to_i - first + 1 : nil
                symbol = lines[(at + 1)..(at + 6)].to_a.find { |l| l =~ /\A\s*symbol:/ }.to_s
                message = "#{message} (#{symbol.strip.sub(/\Asymbol:\s*/, '')})" if message.start_with?('cannot find symbol') && !symbol.empty?
                message = message.gsub(/\bclass HsTurtle\b/, 'deinem Programm')
            end
        end
        message ||= lines.reject(&:empty?).first.to_s
        message = "#{message[0, 300]}\n\n#{MAIN_MISSING}" if !main?(code) && !wrap?(code) && message =~ STRAY
        { :message => message, :line => number, :compile => true }
    end
end
