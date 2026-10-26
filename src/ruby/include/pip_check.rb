require 'json'
require_relative 'pip_world'
require_relative 'pip_maps'
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
#     dry: true                             # optional: a puddle ends the run
#     buttons: [move, turn_left]            # optional: the plan is clicked, not typed
#
# goal knows lamp: true, stones: all, gate: open, bello: true (Pip finds
# Bello; that ends the run, like the lamp) and say: stones (the last thing
# Pip says is the number of stones taken). A map may be made from the
# seed (PipMaps: generate: maze, size: [6, 5]); the first one is always
# written out, the card shows it. A map with call: (»turn_right«) checks a
# function of the student's own: the program is loaded with its commands
# ignored, then call runs, and expect: { facing: south, moves: 3 } says
# where Pip looks and how many tiles Pip has walked.
#
# The result names the map and the line where it went wrong (»In Karte 2
# läuft Pip in Zeile 6 gegen die Wand«) and carries the run of that map, or
# of the first when all are right, for the card to play back (replay).
module PipCheck
    FILES = KataCheck::FILES
    # The commands by their name in the world, as each language spells them.
    NAMES = {
        'ruby' => { 'move' => 'move', 'turn_left' => 'turn_left', 'take' => 'take', 'wait' => 'wait', 'pull' => 'pull',
                    'on_stone' => 'on_stone?', 'on_lever' => 'on_lever?', 'front_clear' => 'front_clear?', 'glibber_ahead' => 'glibber_ahead?',
                    'on_lamp' => 'on_lamp?', 'trail_ahead' => 'trail_ahead?', 'puddle_ahead' => 'puddle_ahead?', 'bello_here' => 'bello_here?',
                    'cave_map' => 'cave_map', 'facing' => 'facing', 'say' => 'say' },
        'python' => { 'move' => 'move', 'turn_left' => 'turn_left', 'take' => 'take', 'wait' => 'wait', 'pull' => 'pull',
                      'on_stone' => 'on_stone', 'on_lever' => 'on_lever', 'front_clear' => 'front_clear', 'glibber_ahead' => 'glibber_ahead',
                      'on_lamp' => 'on_lamp', 'trail_ahead' => 'trail_ahead', 'puddle_ahead' => 'puddle_ahead', 'bello_here' => 'bello_here',
                      'cave_map' => 'cave_map', 'facing' => 'facing', 'say' => 'say' },
        'javascript' => { 'move' => 'move', 'turn_left' => 'turnLeft', 'take' => 'take', 'wait' => 'wait', 'pull' => 'pull',
                          'on_stone' => 'onStone', 'on_lever' => 'onLever', 'front_clear' => 'frontClear', 'glibber_ahead' => 'glibberAhead',
                          'on_lamp' => 'onLamp', 'trail_ahead' => 'trailAhead', 'puddle_ahead' => 'puddleAhead', 'bello_here' => 'belloHere',
                          'cave_map' => 'caveMap', 'facing' => 'facing', 'say' => 'say' },
    }.freeze
    COMMANDS = NAMES['ruby'].keys.freeze
    # What each command does, for the card's list of commands (TaskCard).
    DESCRIPTIONS = {
        'move' => 'ein Feld vorwärts', 'turn_left' => 'eine Vierteldrehung nach links', 'take' => 'den Funkenstein hier aufheben',
        'wait' => 'einen Schritt stehen bleiben', 'pull' => 'am Hebel hier ziehen', 'on_stone' => 'liegt hier ein Funkenstein?',
        'on_lever' => 'steht hier ein Hebel?', 'front_clear' => 'ist das Feld vorn frei?', 'glibber_ahead' => 'ist vorn ein Glibber, oder kommt gleich einer?',
        'on_lamp' => 'steht Pip an der Lampe?', 'trail_ahead' => 'sind vorn Pfotenabdrücke?', 'puddle_ahead' => 'ist vorn eine Pfütze?',
        'bello_here' => 'ist Bello hier?', 'cave_map' => 'die Karte des Wichtels, wie sie gerade ist: eine Liste von Zeilen',
        'facing' => 'wohin Pip schaut: "east", "north", "west" oder "south"', 'say' => 'etwas sagen, ohne einen Schritt',
    }.freeze
    QUESTIONS = %w[on_stone on_lever front_clear glibber_ahead on_lamp trail_ahead puddle_ahead bello_here cave_map facing].freeze

    # How a command is written in language: a step with its parentheses
    # (Ruby without), say with a text.
    def self.spelling(language, command)
        name = NAMES.fetch(language).fetch(command)
        return language == 'ruby' ? 'say "…"' : 'say("…")' if command == 'say'
        return name if language == 'ruby'

        "#{name}()"
    end
    MAX_MAPS = 6
    # what a button can do: a step on the map the browser can show at once
    BUTTONS = %w[move turn_left take].freeze
    BUTTON_LABELS = { 'move' => ['bi-arrow-up', 'Vorwärts'], 'turn_left' => ['bi-arrow-counterclockwise', 'Links drehen'],
                      'take' => ['bi-gem', 'Nimm'] }.freeze
    SECONDS = 10

    EVENTS = {
        'wall' => 'läuft Pip gegen die Wand',
        'gate' => 'bleibt Pip vor den Stacheln stehen',
        'no_stone' => 'greift Pip ins Leere: Hier liegt kein Funkenstein',
        'no_lever' => 'zieht Pip an einem Hebel, der nicht da ist',
        'caught' => 'erwischt ein Glibber Pip',
        'chomped' => 'hat der Mampfer Pip zum Frühstück',
        'puddle' => 'tritt Pip in eine Pfütze',
        'steps' => "hält Pip nach #{PipWorld::MAX_STEPS} Schritten an. Läuft Pip im Kreis?",
    }.freeze
    # where Pip looks, as seen on the map
    LOOKS = { 'east' => 'nach rechts', 'west' => 'nach links', 'north' => 'nach oben', 'south' => 'nach unten' }.freeze
    GOALS = %w[lamp stones gate bello say].freeze

    def self.spec_errors(check)
        errors = []
        errors << 'language must be ruby, python or javascript' unless NAMES.key?(check['language'])
        commands = check['commands']
        unless commands.is_a?(Array) && !commands.empty? && (commands - COMMANDS).empty?
            errors << "commands must list some of #{COMMANDS.join(', ')}"
        end
        # buttons: the plan is clicked together, one command a click, and
        # only watched in the editor (task_card.rb, tasks.js); one map
        if check.key?('buttons')
            buttons = check['buttons']
            unless buttons.is_a?(Array) && !buttons.empty? && (buttons - BUTTONS).empty? && (buttons - (commands || [])).empty?
                errors << "buttons must list some of #{BUTTONS.join(', ')}, each also in commands"
            end
            maps = check['maps']
            if maps.is_a?(Array) && maps.size == 1 && maps.first.is_a?(Hash)
                # the browser moves Pip itself (pip_world.js, PipButtons):
                # walls, stones and the lamp, nothing that moves or switches
                errors << 'a map for buttons has only # . o L P' unless maps.first['map'].to_s.delete("#.oLP \n").empty?
                errors << 'a map for buttons has no call' if maps.first['call']
            else
                errors << 'buttons need exactly one map'
            end
            errors << 'buttons do not go with dry' if check['dry']
        end
        maps = check['maps']
        if maps.is_a?(Array) && !maps.empty? && maps.size <= MAX_MAPS
            maps.each_with_index do |m, i|
                if m.is_a?(Hash) && m.key?('generate') && i > 0
                    errors.concat(PipMaps.errors(m).map { |e| "maps[#{i}]: #{e}" })
                    next
                end
                unless m.is_a?(Hash) && m['map'].is_a?(String)
                    errors << "maps[#{i}] needs map#{i.zero? ? ' (the first map is written out, the card shows it)' : ''}"
                    next
                end
                begin
                    PipWorld.new(m['map'], :facing => m['facing'] || 'east')
                rescue PipWorld::Error => e
                    errors << "maps[#{i}]: #{e.message}"
                end
                if m.key?('call') || m.key?('expect')
                    expect = m['expect']
                    errors << "maps[#{i}]: call needs a call as text" unless m['call'].is_a?(String) && !m['call'].strip.empty?
                    unless expect.is_a?(Hash) && !expect.empty? && (expect.keys - %w[facing moves]).empty? &&
                           (!expect.key?('facing') || LOOKS.key?(expect['facing'])) && (!expect.key?('moves') || expect['moves'].is_a?(Integer))
                        errors << "maps[#{i}]: expect must name facing (east, north, west, south) and/or moves (a number)"
                    end
                end
            end
        else
            errors << "maps must list 1 to #{MAX_MAPS} maps"
        end
        goal = check['goal']
        expecting = maps.is_a?(Array) && !maps.empty? && maps.all? { |m| m.is_a?(Hash) && m.key?('expect') }
        unless (goal.nil? && expecting) || (goal.is_a?(Hash) && !goal.empty? && (goal.keys - GOALS).empty?)
            errors << 'goal must name lamp: true, stones: all, gate: open, bello: true and/or say: stones (or every map has expect)'
        end
        errors << 'max_lines must be a whole number above 0' if check.key?('max_lines') && !(check['max_lines'].is_a?(Integer) && check['max_lines'] > 0)
        errors
    end

    # The prelude for language: Pip's commands of this chapter as functions,
    # then the student's file; with call, then »_loaded« (the world stops
    # ignoring commands) and the call.
    def self.prelude(language, commands, call = nil)
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
                    if answer.startswith(('[', '"')):
                        return json.loads(answer)
                    return {'true': True, 'false': False}.get(answer)
                #{functions}
                _names = {name: value for name, value in globals().items() if not name.startswith('_')}
                _names['__name__'] = '__main__'
                exec(compile(open('kata.py', encoding='utf-8').read(), 'kata.py', 'exec'), _names)
                #{call ? "_pip('_loaded')\nexec(compile(#{JSON.generate(call)}, 'call', 'exec'), _names)" : ''}
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
                    return JSON.parse(answer) if answer.start_with?('[', '"')

                    { 'true' => true, 'false' => false }[answer]
                end
                #{functions}
                load File.expand_path('kata.rb')
                #{call ? "__pip('_loaded')\nTOPLEVEL_BINDING.eval(#{call.inspect}, 'call')" : ''}
            RUBY
        when 'javascript'
            functions = defined.map do |c|
                c == 'say' ? "globalThis.say = (text) => pip('say', String(text));\n" : "globalThis.#{names[c]} = () => pip('#{c}');\n"
            end.join
            <<~JS
                const fs = require('fs');
                const vm = require('vm');
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
                    if (answer.startsWith('[') || answer.startsWith('"')) return JSON.parse(answer);
                    return answer === 'true' ? true : answer === 'false' ? false : undefined;
                }
                #{functions}
                // a script, not a module: the student's functions are global, for call
                vm.runInThisContext(fs.readFileSync('kata.js', 'utf8'), { filename: 'kata.js' });
                #{call ? "pip('_loaded');\nvm.runInThisContext(#{JSON.generate(call)}, { filename: 'call.js' });" : ''}
            JS
        end
    end

    def self.argv(language, commands, call = nil)
        code = prelude(language, commands, call)
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
        world = PipWorld.new(map['map'], :facing => map['facing'] || 'east', :dry => check['dry'] == true)
        world.quiet = true if map['call']
        allowed = check['commands'] + ['say']
        dialog = lambda do |line|
            request = JSON.parse(line) rescue nil
            if request.is_a?(Hash) && request['c'] == '_loaded' && map['call']
                world.quiet = false
                next 'ok'
            end
            next 'end' unless request.is_a?(Hash) && allowed.include?(request['c'])

            answer = world.command(request['c'], :line => request['l'], :text => request['t'])
            # cave_map and facing: data, as JSON (the prelude reads it back)
            next JSON.generate(answer) if PipWorld::QUERIES.include?(request['c']) && answer != 'end'

            answer == true || answer == false ? answer.to_s : answer
        end
        language = check['language']
        result = run.call(:files => { FILES.fetch(language) => code.to_s }, :argv => argv(language, check['commands'], map['call']),
                          :timeout => SECONDS, :dialog => dialog)
        return nil if result.nil?

        unless world.ended || result.timed_out
            # a plan that ends early is not the end of the run: Pip stands
            # still, and the Mampfer comes (PipWorld#chase). Not after a
            # crash, which is its own message, nor for a call of a function.
            crashed = !(result.stderr.to_s.strip.empty? || result.status == 0)
            world.chase unless crashed || map['call'] || goal_missing(check['goal'] || {}, world).nil?
            world.done
        end
        { :world => world, :output => result.stdout, :errors => result.stderr, :timed_out => result.timed_out, :status => result.status }
    end

    # [status, message, read, replay]
    # The lines that count for max_lines: not empty, not only a comment.
    def self.lines(code)
        code.to_s.lines.count { |l| l.strip != '' && !l.strip.start_with?('#', '//') }
    end

    # The maps of a check for seed: the written ones as they are, the
    # others made by PipMaps.
    def self.maps(check, seed)
        check['maps'].each_with_index.map { |m, i| m.key?('generate') ? PipMaps.make(m, seed, i) : m }
    end

    # selected: the map the card shows; it runs first, and its run is what
    # the card plays when the plan works on every map.
    def self.check(task, check, code, run, seed: 0, selected: 0)
        return [:invalid, KataCheck::EMPTY, nil, nil] if code.to_s.strip.empty?

        first = nil
        # the run of every map so far by its index, for the card to show
        # each (runs)
        runs = []
        with_runs = ->(r) { r.merge(:runs => runs) }
        maps = maps(check, seed)
        selected = 0 unless selected.is_a?(Integer) && selected.between?(0, maps.size - 1)
        order = [selected] + ((0...maps.size).to_a - [selected])
        order.each do |i|
            map = maps[i]
            ran = run_map(check, code, map, run)
            return [:unavailable, KataCheck::BUSY, nil, nil] if ran.nil?

            world = ran[:world]
            replay = replay(world, i, check['maps'].size, ran[:output])
            runs[i] = replay
            replay = with_runs.call(replay)
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
            if world.chased > 0 && world.ended == :chomped
                n = world.chased == 1 ? 'einem Schritt' : "#{world.chased} Schritten"
                return [:wrong, "#{where} ist dein Plan zu Ende, bevor Pip es geschafft hat. Pip bleibt stehen, " \
                                "und der Mampfer kommt: Nach #{n} hat er Pip zum Frühstück.", nil, replay]
            end
            if (event = EVENTS[world.ended.to_s])
                line = world.trace.last[:line]
                return [:wrong, "#{where}#{line ? " in Zeile #{line}" : ''} #{event}.", nil, replay]
            end
            missing = expect_missing(map, world) || goal_missing(check['goal'] || {}, world)
            return [:wrong, "#{where} #{missing}", nil, replay] if missing
        end
        if check['max_lines'] && (n = lines(code)) > check['max_lines']
            return [:wrong, "Pip schafft es, aber dein Plan hat #{n} Zeilen. Geht es in höchstens #{check['max_lines']}?", nil, first]
        end
        [:correct, check['maps'].size > 1 ? "Pip schafft alle #{check['maps'].size} Karten." : 'Pip schafft es.', nil, first]
    end

    # call and expect of a map: »schaut Pip nach `turn_right` nach oben
    # statt nach unten«.
    def self.expect_missing(map, world)
        expect = map['expect']
        return nil unless expect

        call = "`#{map['call'].strip}`"
        if expect['facing'] && world.pip[:facing] != expect['facing']
            return "schaut Pip nach #{call} #{LOOKS[world.pip[:facing]]} statt #{LOOKS[expect['facing']]}."
        end
        if expect['moves'] && world.moves != expect['moves']
            n = world.moves
            return "geht Pip bei #{call} #{n == 1 ? 'ein Feld' : "#{n} Felder"} statt #{expect['moves']}."
        end

        nil
    end

    def self.goal_missing(goal, world)
        if goal['stones'] == 'all' && world.stones_left > 0
            n = world.stones_left
            return "liegt am Ende noch #{n == 1 ? 'ein Funkenstein' : "#{n} Funkensteine"} da."
        end
        return 'steht Pip am Ende nicht an der Lampe.' if goal['lamp'] && !world.at_lamp?
        return 'ragen die Stacheln am Ende noch aus dem Boden.' if goal['gate'] == 'open' && !world.gate_open?
        return 'hat Pip Bello am Ende nicht gefunden.' if goal['bello'] && !world.at_bello?
        if goal['say'] == 'stones'
            n = world.taken
            stones = n == 1 ? 'einen Funkenstein' : "#{n} Funkensteine"
            return "sagt Pip am Ende nichts. Pip hat #{stones} aufgehoben: Wie viele, soll Pip sagen." if world.said.empty?
            # a sentence counts too, as long as its one number is the right one
            # (»Ich habe 4 Steine eingesammelt.«)
            return "sagt Pip am Ende »#{world.said.last}«, aber Pip hat #{stones} aufgehoben." unless world.said.last.scan(/\d+/) == [n.to_s]
        end

        nil
    end

    # What the card plays back: the map and every frame of the run.
    def self.replay(world, index, count, output)
        { :map => index + 1, :maps => count, :layout => world.layout, :frames => world.trace, :ended => world.ended,
          :said => world.said, :output => output.to_s[0, 4000] }
    end
end
