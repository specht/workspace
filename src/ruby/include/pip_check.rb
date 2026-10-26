require 'json'
require_relative 'pip_world'
require_relative 'kata_runner'
require_relative 'kata_check'

# Check type pip (docs/plan-katas.md, section 2): a plan for Pip, the whole
# program typed into the card. It runs in a container of its own
# (KataRunner, through KataQueue); a prelude, invisible to the student,
# defines Pip's commands as ordinary functions (Ruby: move, front_clear?;
# Python: move(), front_clear(); JavaScript: move(), frontClear()). Each one
# writes a line to the world on the server (PipWorld, through KataRunner's
# dialog) and waits for its answer, so the world decides everything and
# nothing can be faked. The student's code runs from its own file: line
# numbers are its own.
#
#   check:
#     type: pip
#     language: ruby
#     commands: [move, turn_left, take]     # what this chapter knows
#     maps:                                 # each runs the program afresh
#       - facing: east
#         map: |
#           #######
#           #P.o.L#
#           #######
#     goal: { lamp: true, stones: all }     # what holds after each map
#     max_lines: 6                          # optional: the plan in at most so many lines
#
# The result names the map and the line where it went wrong (»In Karte 2
# läuft Pip in Zeile 6 gegen die Wand«) and carries the run of that map, or
# of the first when all are right, for the card to play back (replay).
module PipCheck
    FILES = KataCheck::FILES
    # The commands by their name in the world, as each language spells them.
    NAMES = {
        'ruby' => { 'move' => 'move', 'turn_left' => 'turn_left', 'take' => 'take', 'wait' => 'wait', 'pull' => 'pull',
                    'on_stone' => 'on_stone?', 'front_clear' => 'front_clear?', 'glibber_ahead' => 'glibber_ahead?',
                    'on_lamp' => 'on_lamp?', 'on_trail' => 'on_trail?', 'puddle_ahead' => 'puddle_ahead?', 'bello_here' => 'bello_here?',
                    'say' => 'say' },
        'python' => { 'move' => 'move', 'turn_left' => 'turn_left', 'take' => 'take', 'wait' => 'wait', 'pull' => 'pull',
                      'on_stone' => 'on_stone', 'front_clear' => 'front_clear', 'glibber_ahead' => 'glibber_ahead',
                      'on_lamp' => 'on_lamp', 'on_trail' => 'on_trail', 'puddle_ahead' => 'puddle_ahead', 'bello_here' => 'bello_here',
                      'say' => 'say' },
        'javascript' => { 'move' => 'move', 'turn_left' => 'turnLeft', 'take' => 'take', 'wait' => 'wait', 'pull' => 'pull',
                          'on_stone' => 'onStone', 'front_clear' => 'frontClear', 'glibber_ahead' => 'glibberAhead',
                          'on_lamp' => 'onLamp', 'on_trail' => 'onTrail', 'puddle_ahead' => 'puddleAhead', 'bello_here' => 'belloHere',
                          'say' => 'say' },
    }.freeze
    COMMANDS = NAMES['ruby'].keys.freeze
    MAX_MAPS = 6
    SECONDS = 10

    EVENTS = {
        'wall' => 'läuft Pip gegen die Wand',
        'no_stone' => 'greift Pip ins Leere: Hier liegt kein Funkenstein',
        'no_lever' => 'zieht Pip an einem Hebel, der nicht da ist',
        'caught' => 'erwischt ein Glibber Pip',
        'steps' => "hält Pip nach #{PipWorld::MAX_STEPS} Schritten an. Läuft Pip im Kreis?",
    }.freeze

    def self.spec_errors(check)
        errors = []
        errors << 'language must be ruby, python or javascript' unless NAMES.key?(check['language'])
        commands = check['commands']
        unless commands.is_a?(Array) && !commands.empty? && (commands - COMMANDS).empty?
            errors << "commands must list some of #{COMMANDS.join(', ')}"
        end
        maps = check['maps']
        if maps.is_a?(Array) && !maps.empty? && maps.size <= MAX_MAPS
            maps.each_with_index do |m, i|
                unless m.is_a?(Hash) && m['map'].is_a?(String)
                    errors << "maps[#{i}] needs map"
                    next
                end
                begin
                    PipWorld.new(m['map'], :facing => m['facing'] || 'east')
                rescue PipWorld::Error => e
                    errors << "maps[#{i}]: #{e.message}"
                end
            end
        else
            errors << "maps must list 1 to #{MAX_MAPS} maps"
        end
        goal = check['goal']
        unless goal.is_a?(Hash) && !goal.empty? && (goal.keys - %w[lamp stones gate]).empty?
            errors << 'goal must name lamp: true, stones: all and/or gate: open'
        end
        errors << 'max_lines must be a whole number above 0' if check.key?('max_lines') && !(check['max_lines'].is_a?(Integer) && check['max_lines'] > 0)
        errors
    end

    # The prelude for language: Pip's commands of this chapter as functions,
    # then the student's file.
    def self.prelude(language, commands)
        names = NAMES.fetch(language)
        defined = commands + ['say']
        case language
        when 'python'
            functions = defined.map do |c|
                c == 'say' ? "def say(text):\n    _pip('say', str(text))\n" : "def #{names[c]}():\n    return _pip('#{c}')\n"
            end.join
            <<~PY
                import json, sys
                def _pip(command, text=None):
                    frame = sys._getframe(1)
                    while frame is not None and frame.f_code.co_filename != 'kata.py':
                        frame = frame.f_back
                    line = frame.f_lineno if frame is not None else None
                    sys.stdout.write('\\x1ePIP ' + json.dumps({'c': command, 'l': line, 't': text}) + '\\n')
                    sys.stdout.flush()
                    answer = sys.stdin.readline().strip()
                    if answer == 'end':
                        sys.stdout.flush()
                        import os
                        os._exit(0)
                    return {'true': True, 'false': False}.get(answer)
                #{functions}
                _names = {name: value for name, value in globals().items() if not name.startswith('_')}
                _names['__name__'] = '__main__'
                exec(compile(open('kata.py', encoding='utf-8').read(), 'kata.py', 'exec'), _names)
            PY
        when 'ruby'
            functions = defined.map do |c|
                c == 'say' ? "def say(text) = __pip('say', text.to_s)\n" : "def #{names[c]} = __pip('#{c}')\n"
            end.join
            <<~RUBY
                require 'json'
                $stdout.sync = true
                def __pip(command, text = nil)
                    location = caller_locations.find { |l| File.basename(l.path) == 'kata.rb' }
                    $stdout.write("\\x1ePIP " + JSON.generate({ 'c' => command, 'l' => location&.lineno, 't' => text }) + "\\n")
                    answer = $stdin.gets.to_s.strip
                    exit!(0) if answer == 'end'
                    { 'true' => true, 'false' => false }[answer]
                end
                #{functions}
                load File.expand_path('kata.rb')
            RUBY
        when 'javascript'
            functions = defined.map do |c|
                c == 'say' ? "globalThis.say = (text) => pip('say', String(text));\n" : "globalThis.#{names[c]} = () => pip('#{c}');\n"
            end.join
            <<~JS
                const fs = require('fs');
                const path = require('path');
                function readLine() {
                    const one = Buffer.alloc(1);
                    let line = '';
                    while (fs.readSync(0, one, 0, 1, null) === 1) {
                        const c = one.toString();
                        if (c === '\\n') break;
                        line += c;
                    }
                    return line;
                }
                function pip(command, text) {
                    const at = String(new Error().stack).split('\\n').find((l) => l.includes('kata.js:'));
                    const line = at ? Number(at.split('kata.js:')[1].split(':')[0]) : null;
                    fs.writeSync(1, '\\x1ePIP ' + JSON.stringify({ c: command, l: line, t: text === undefined ? null : text }) + '\\n');
                    const answer = readLine().trim();
                    if (answer === 'end') process.exit(0);
                    return answer === 'true' ? true : answer === 'false' ? false : undefined;
                }
                #{functions}
                require(path.resolve('kata.js'));
            JS
        end
    end

    def self.argv(language, commands)
        code = prelude(language, commands)
        case language
        when 'python' then ['python3', '-I', '-B', '-c', code]
        when 'ruby' then ['ruby', '-W0', '-e', code]
        when 'javascript' then ['node', '-e', code]
        end
    end

    # One run of the program on map (a Hash with map and facing):
    # { world:, output:, errors:, timed_out: }. run: lambda(files:, argv:,
    # timeout:, dialog:) -> KataRunner::Result or nil.
    def self.run_map(check, code, map, run)
        world = PipWorld.new(map['map'], :facing => map['facing'] || 'east')
        allowed = check['commands'] + ['say']
        dialog = lambda do |line|
            request = JSON.parse(line) rescue nil
            next 'end' unless request.is_a?(Hash) && allowed.include?(request['c'])

            answer = world.command(request['c'], :line => request['l'], :text => request['t'])
            answer == true || answer == false ? answer.to_s : answer
        end
        language = check['language']
        result = run.call(:files => { FILES.fetch(language) => code.to_s }, :argv => argv(language, check['commands']),
                          :timeout => SECONDS, :dialog => dialog)
        return nil if result.nil?

        world.done unless world.ended || result.timed_out
        { :world => world, :output => result.stdout, :errors => result.stderr, :timed_out => result.timed_out, :status => result.status }
    end

    # [status, message, read, replay]
    # The lines that count for max_lines: not empty, not only a comment.
    def self.lines(code)
        code.to_s.lines.count { |l| l.strip != '' && !l.strip.start_with?('#', '//') }
    end

    def self.check(task, check, code, run)
        return [:invalid, KataCheck::EMPTY, nil, nil] if code.to_s.strip.empty?

        first = nil
        check['maps'].each_with_index do |map, i|
            ran = run_map(check, code, map, run)
            return [:unavailable, KataCheck::BUSY, nil, nil] if ran.nil?

            world = ran[:world]
            replay = replay(world, i, check['maps'].size, ran[:output])
            first ||= replay
            where = check['maps'].size > 1 ? "In Karte #{i + 1}" : 'Hier'
            if ran[:timed_out]
                return [:wrong, "#{where} lief dein Plan länger als #{SECONDS} Sekunden. Steckt irgendwo eine Endlosschleife?", nil, replay]
            end
            unless ran[:errors].to_s.strip.empty? || ran[:status] == 0
                error = ran[:errors].to_s.strip.lines.last.to_s.strip
                line = ran[:errors][/kata\.(?:rb|py|js)"?(?:, line |:)(\d+)/, 1]
                message = "#{where} bricht dein Plan ab#{line ? " (Zeile #{line})" : ''}: `#{error[0, 200]}`"
                explanation = KataCheck.explain(ran[:errors], check['language'])
                message += "\n\n#{explanation}" if explanation
                return [:wrong, message, nil, replay]
            end
            if (event = EVENTS[world.ended.to_s])
                line = world.trace.last[:line]
                return [:wrong, "#{where}#{line ? " in Zeile #{line}" : ''} #{event}.", nil, replay]
            end
            missing = goal_missing(check['goal'], world)
            return [:wrong, "#{where} #{missing}", nil, replay] if missing
        end
        if check['max_lines'] && (n = lines(code)) > check['max_lines']
            return [:wrong, "Pip schafft es, aber dein Plan hat #{n} Zeilen. Geht es in höchstens #{check['max_lines']}?", nil, first]
        end
        [:correct, check['maps'].size > 1 ? "Pip schafft alle #{check['maps'].size} Karten." : 'Pip schafft es.', nil, first]
    end

    def self.goal_missing(goal, world)
        if goal['stones'] == 'all' && world.stones_left > 0
            n = world.stones_left
            return "liegt am Ende noch #{n == 1 ? 'ein Funkenstein' : "#{n} Funkensteine"} da."
        end
        return 'steht Pip am Ende nicht an der Lampe.' if goal['lamp'] && !world.at_lamp?
        return 'ist das Tor am Ende nicht offen.' if goal['gate'] == 'open' && !world.gate_open?

        nil
    end

    # What the card plays back: the map and every frame of the run.
    def self.replay(world, index, count, output)
        { :map => index + 1, :maps => count, :layout => world.layout, :frames => world.trace, :ended => world.ended,
          :said => world.said, :output => output.to_s[0, 4000] }
    end
end
