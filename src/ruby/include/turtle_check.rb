require 'json'
require 'open3'
require 'tmpdir'
require 'stringio'
require_relative 'turtle_world'

# Check type turtle (docs/plan-katas.md, »Turtle«): a program typed into the
# card that draws. It runs in the browser (turtle_worker.js), which reports
# the commands of each run; here they are replayed (TurtleWorld) and the
# lines compared with the picture the task asks for.
#
#   check:
#     type: turtle
#     language: ruby
#     draw: |                        # the picture of the whole program
#       4.times { forward 100; left 90 }
#     max_lines: 3                   # optional: in at most so many lines
#     colors: true                   # optional: the colours count too
#
#   or, for a function of the student's own, each call from the start:
#
#     cases:
#       - call: square(50)
#         draw: 4.times { forward 50; left 90 }
#
#   or, for a picture of one's own:
#
#     free: { min_lines: 20, min_colors: 2 }
#
# The comparison does not care in which order or direction lines were
# drawn, whether a line is drawn in pieces or twice, or about small errors
# of rounding: it samples both pictures every STEP and asks whether a line
# of the other one is within NEAR. What is missing and what is too much
# goes back to the card, which draws it over the picture.
module TurtleCheck
    STEP = 2.0
    NEAR = 1.6
    CELL = 10.0
    MAX_RUNS = 12
    FILES = { 'ruby' => 'kata.rb', 'python' => 'kata.py', 'javascript' => 'kata.js' }.freeze
    LANGUAGES = FILES.keys.freeze
    FREE = %w[min_lines min_colors].freeze
    # what the card says under its picture for a call (»square(50)«)
    EMPTY = 'Schreib zuerst ein Programm.'

    def self.spec_errors(check)
        errors = []
        errors << 'language must be ruby, python or javascript' unless LANGUAGES.include?(check['language'])
        kinds = %w[draw cases free].select { |k| check.key?(k) }
        errors << 'a turtle check needs exactly one of draw, cases and free' unless kinds.size == 1
        if check.key?('draw')
            errors.concat(draw_errors(check['draw'], 'draw'))
        end
        if check.key?('cases')
            cases = check['cases']
            if cases.is_a?(Array) && !cases.empty? && cases.size <= MAX_RUNS
                cases.each_with_index do |c, i|
                    unless c.is_a?(Hash) && c['call'].is_a?(String) && !c['call'].strip.empty?
                        errors << "cases[#{i}] needs call"
                        next
                    end
                    errors.concat(draw_errors(c['draw'], "cases[#{i}].draw"))
                end
            else
                errors << "cases must list 1 to #{MAX_RUNS} calls"
            end
        end
        if check.key?('free')
            free = check['free']
            unless free.is_a?(Hash) && !free.empty? && (free.keys - FREE).empty? && free.values.all? { |v| v.is_a?(Integer) && v > 0 }
                errors << "free must name #{FREE.join(' and/or ')} as whole numbers above 0"
            end
        end
        errors << 'max_lines must be a whole number above 0' if check.key?('max_lines') && !(check['max_lines'].is_a?(Integer) && check['max_lines'] > 0)
        errors << 'colors must be true or false' if check.key?('colors') && ![true, false].include?(check['colors'])
        errors
    end

    def self.draw_errors(source, where)
        return ["#{where} must be Ruby that draws"] unless source.is_a?(String) && !source.strip.empty?

        world = TurtleWorld.draw(source)
        world.segments.empty? ? ["#{where} draws nothing"] : []
    rescue StandardError, ScriptError => e
        ["#{where}: #{e.class}: #{e.message}"]
    end

    # What the browser runs: null for the program, or each call.
    def self.runs(check)
        check['cases'] ? check['cases'].map { |c| c['call'] } : [nil]
    end

    # The picture the card shows faintly before anything runs: the whole
    # program's, or the first call's.
    def self.target(check)
        source = check['draw'] || (check['cases'] && check['cases'].first['draw'])
        source ? TurtleWorld.draw(source).segments.map(&:to_a) : nil
    end

    # ----- comparing -----

    # The lines of a picture in cells of CELL, for »is there one near?«.
    class Index
        def initialize(segments)
            @cells = Hash.new { |h, k| h[k] = [] }
            segments.each do |s|
                pad = NEAR + 0.01
                (((([s.x1, s.x2].min - pad) / CELL).floor)..((([s.x1, s.x2].max + pad) / CELL).floor)).each do |cx|
                    (((([s.y1, s.y2].min - pad) / CELL).floor)..((([s.y1, s.y2].max + pad) / CELL).floor)).each do |cy|
                        @cells[[cx, cy]] << s
                    end
                end
            end
        end

        def near?(x, y, color)
            (@cells[[(x / CELL).floor, (y / CELL).floor]] || []).any? do |s|
                (color.nil? || s.color == color) && TurtleCheck.distance(x, y, s) <= NEAR
            end
        end
    end

    def self.distance(x, y, s)
        dx = s.x2 - s.x1
        dy = s.y2 - s.y1
        length2 = dx * dx + dy * dy
        t = length2.zero? ? 0 : (((x - s.x1) * dx + (y - s.y1) * dy) / length2).clamp(0.0, 1.0)
        Math.hypot(x - (s.x1 + t * dx), y - (s.y1 + t * dy))
    end

    # The pieces of segments that no line of other comes near, as segments
    # (a single point as one of length 0).
    def self.uncovered(segments, other, colors)
        index = Index.new(other)
        segments.flat_map do |s|
            n = [(s.length / STEP).ceil, 1].max
            covered = (0..n).map do |i|
                t = i.to_f / n
                index.near?(s.x1 + t * (s.x2 - s.x1), s.y1 + t * (s.y2 - s.y1), colors ? s.color : nil)
            end
            runs = []
            start = nil
            covered.each_with_index do |c, i|
                if !c && start.nil?
                    start = i
                elsif c && start
                    runs << [start, i - 1]
                    start = nil
                end
            end
            runs << [start, n] if start
            runs.map { |a, b| piece(s, a.to_f / n, b.to_f / n) }
        end
    end

    def self.piece(s, a, b)
        TurtleWorld::Segment.new(s.x1 + a * (s.x2 - s.x1), s.y1 + a * (s.y2 - s.y1),
                                 s.x1 + b * (s.x2 - s.x1), s.y1 + b * (s.y2 - s.y1), s.color, s.width)
    end

    # { missing:, extra: } as segments; both empty when the pictures agree.
    def self.diff(expected, drawn, colors: false)
        { :missing => uncovered(expected, drawn, colors), :extra => uncovered(drawn, expected, colors) }
    end

    def self.length(segments) = segments.sum(&:length)

    # ----- the check -----

    # runs: what the browser reported, [[[command, value], …], …], one list
    # per run (TurtleCheck.runs). [status, message, show] where show is what
    # the card draws: the run, the picture asked for and the difference.
    def self.check(check, code, runs)
        return [:invalid, EMPTY, nil] if code.to_s.strip.empty?

        wanted = self.runs(check)
        unless runs.is_a?(Array) && runs.size == wanted.size
            return [:invalid, 'Dein Programm ist noch nicht bis zum Ende gelaufen.', nil]
        end

        worlds = runs.map { |commands| TurtleWorld.replay(commands) }
        if check['free']
            free_check(check['free'], worlds.first)
        else
            sources = check['cases'] ? check['cases'].map { |c| c['draw'] } : [check['draw']]
            sources.each_with_index do |source, i|
                expected = TurtleWorld.draw(source).segments
                drawn = worlds[i].segments
                d = diff(expected, drawn, :colors => check['colors'] == true)
                next if d[:missing].empty? && d[:extra].empty?

                call = check['cases'] ? check['cases'][i]['call'] : nil
                show = { :run => i, :call => call, :expected => expected.map(&:to_a), :drawn => drawn.map(&:to_a),
                         :missing => d[:missing].map(&:to_a), :extra => d[:extra].map(&:to_a) }
                return [:wrong, message(d, drawn, expected, call, check['colors'] == true), show]
            end
            if check['max_lines'] && (n = lines(code)) > check['max_lines']
                return [:wrong, "Das Bild stimmt, aber dein Programm hat #{n} Zeilen. Geht es in höchstens #{check['max_lines']}? " \
                                'Was sich wiederholt, kann eine Schleife machen.', nil]
            end
            n = wanted.size
            [:correct, n > 1 ? "Alle #{n} Aufrufe zeichnen genau das Richtige." : 'Dein Bild stimmt.', nil]
        end
    rescue TurtleWorld::Error => e
        [:invalid, "Das Bild ließ sich nicht nachzeichnen (#{e.message}). Lass dein Programm noch einmal laufen.", nil]
    end

    def self.free_check(free, world)
        lines = world.segments.size
        if free['min_lines'] && lines < free['min_lines']
            return [:wrong, "Dein Bild hat #{lines == 1 ? 'eine Linie' : "#{lines} Linien"}. Zeichne mindestens #{free['min_lines']}.", nil]
        end
        colors = world.segments.map(&:color).uniq.size
        if free['min_colors'] && colors < free['min_colors']
            return [:wrong, "Dein Bild hat #{colors == 1 ? 'eine Farbe' : "#{colors} Farben"}. Nimm mindestens #{free['min_colors']}.", nil]
        end
        [:correct, 'Ein schönes Bild!', nil]
    end

    # »Bei `square(50)` fehlt ein Stück von etwa 50 Schritten (gestrichelt),
    # und ein kleines Stück ist zu viel (rot).«
    def self.message(d, drawn, expected, call, colors)
        at = call ? "Bei `#{call.strip}`" : 'In deinem Bild'
        if drawn.empty?
            return call ? "#{at} zeichnet die Schildkröte nichts." : 'Die Schildkröte hat nichts gezeichnet.'
        end
        if colors
            plain = diff(expected, drawn)
            return "#{at} stimmen die Linien, aber nicht alle Farben." if plain[:missing].empty? && plain[:extra].empty?
        end
        missing = d[:missing].empty? ? nil : "fehlt #{amount(d[:missing])} (gestrichelt)"
        extra = d[:extra].empty? ? nil : "ist #{amount(d[:extra])} zu viel (rot)"
        if missing && extra
            "#{at} #{missing}, und #{amount(d[:extra])} ist zu viel (rot)."
        else
            "#{at} #{missing || extra}."
        end
    end

    def self.amount(segments)
        n = length(segments).round
        n < 3 ? 'ein kleines Stück' : "ein Stück von etwa #{n} Schritten"
    end

    # ----- running on this machine (tests and examples only) -----

    PRELUDES = File.expand_path('../../static/include/turtle', __dir__)

    # The program and its calls as the worker runs them, with the local
    # ruby, python3 or node and the same commands (turtle.rb, turtle.py,
    # turtle.js): [runs, error] with runs as the browser reports them.
    def self.record(language, code, runs, timeout: 10)
        Dir.mktmpdir('turtle') do |dir|
            File.write(File.join(dir, FILES.fetch(language)), code.to_s)
            File.write(File.join(dir, 'runs.json'), JSON.generate(runs))
            argv = case language
                when 'ruby' then ['ruby', '-W0', '-e', RUBY_DRIVER, PRELUDES]
                when 'python' then ['python3', '-I', '-c', PYTHON_DRIVER, PRELUDES]
                when 'javascript' then ['node', '-e', JS_DRIVER, PRELUDES]
            end
            out, err, status = Open3.capture3(*argv, :chdir => dir)
            raise "turtle #{language} driver failed: #{err[0, 400]}" unless status.success?

            data = JSON.parse(out.lines.last.to_s)
            [data['runs'], data['error']]
        end
    end

    RUBY_DRIVER = <<~'RUBY'
        require 'json'
        require 'stringio'
        $quiet = false
        $commands = []
        def __turtle_host(command, value, line)
            $commands << [command, value] unless $quiet
            nil
        end
        load File.join(ARGV[0], 'turtle.rb')
        code = File.read('kata.rb')
        runs = []
        error = nil
        JSON.parse(File.read('runs.json')).each do |call|
            $commands = []
            $quiet = !call.nil?
            begin
                $stdout = StringIO.new
                TOPLEVEL_BINDING.eval(code, 'kata.rb', 1)
                if call
                    $quiet = false
                    TOPLEVEL_BINDING.eval(call, 'call', 1)
                end
            rescue SystemExit
            rescue Exception => e
                error ||= "#{e.class}: #{e.message}"
            ensure
                $stdout = STDOUT
            end
            runs << $commands
        end
        puts JSON.generate({ 'runs' => runs, 'error' => error })
    RUBY

    PYTHON_DRIVER = <<~'PY'
        import json, sys, io, os
        source = open(os.path.join(sys.argv[1], 'turtle.py'), encoding='utf-8').read()
        code = open('kata.py', encoding='utf-8').read()
        runs, error = [], None
        for call in json.load(open('runs.json')):
            commands, state = [], {'quiet': call is not None}
            def host(command, value, line, commands=commands, state=state):
                if not state['quiet']:
                    commands.append([command, value])
            ns = {'__name__': '__main__', '_turtle_host': host}
            exec(compile(source, 'turtle.py', 'exec'), ns)
            real = sys.stdout
            sys.stdout = io.StringIO()
            try:
                exec(compile(code, 'kata.py', 'exec'), ns)
                if call is not None:
                    state['quiet'] = False
                    exec(compile(call, 'call', 'exec'), ns)
            except SystemExit:
                pass
            except BaseException as e:
                error = error or f'{type(e).__name__}: {e}'
            finally:
                sys.stdout = real
            runs.append(commands)
        print(json.dumps({'runs': runs, 'error': error}))
    PY

    JS_DRIVER = <<~'JS'
        const fs = require('fs');
        const path = require('path');
        const { turtleCommands } = require(path.join(process.argv[1], 'turtle.js'));
        const code = fs.readFileSync('kata.js', 'utf8');
        const runs = [];
        let error = null;
        for (const call of JSON.parse(fs.readFileSync('runs.json', 'utf8'))) {
            const commands = [];
            let quiet = call !== null;
            const turtle = turtleCommands((command, value) => { if (!quiet) commands.push(value === undefined ? [command] : [command, value]); return null; });
            const names = Object.keys(turtle);
            const silent = { log() {}, error() {}, warn() {}, info() {} };
            try {
                new Function(...names, 'console', '__loaded', code + '\n;' + (call === null ? '' : '__loaded();\n' + call + ';'))(
                    ...names.map((n) => turtle[n]), silent, () => { quiet = false; });
            } catch (e) {
                error = error || (e.name + ': ' + e.message);
            }
            runs.push(commands);
        }
        console.log(JSON.stringify({ runs, error }));
    JS

    # The lines that count for max_lines: not empty, not only a comment.
    def self.lines(code)
        code.to_s.lines.count { |l| l.strip != '' && !l.strip.start_with?('#', '//') }
    end
end
