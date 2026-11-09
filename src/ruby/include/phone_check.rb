require 'shellwords'

# Check type phone (docs/aufgaben.md): plays the person's Telefonspiel with
# given keys and compares what it says (docs/plan-telefonspiel.md).
#
# The game runs with the emulator's test mode, `telefon --test`, in the
# person's Workspace as the guest that can read /workspace but not change it
# (the code is theirs, rule 1), once per case. Each keys() of the game takes
# the next answer of the case; pauses take no time. The emulator prints one
# line per event (»Telefon: …«, »Musik: …«, »Tasten: …«, »Ausgabe: …«,
# »Fehler: …«, »Ende: …«), and this module reads them.
#
#   check:
#     type: phone
#     file: telefon/abenteuer.rb
#     cases:
#       - keys: ''                       # no keys: what the game says first
#         says: [Höhle]                  # each pattern in something the game says
#         music: true                    # it plays music (a file that exists, or a link)
#       - keys: '1'                      # the answers to keys(), with commas; »-« for none
#         says: [Fackel]
#         not_says: [Wasser]
#         ends: true                     # and then the game is over (no more keys asked)
#         say: Nach der 1 geht es in den linken Gang.
#
# Patterns are regular expressions, without regard to upper and lower case.
module PhoneCheck
    MARK = '@@hs-phone-case '
    MAX_CASES = 12
    SECONDS = 5                    # per case, unless the check says otherwise
    CASE_KEYS = %w[keys says not_says music ends say].freeze
    SHOWN_LINES = 30

    # The emulator: on PATH in the Workspace (/usr/local/bin/telefon); the
    # examples set HS_TELEFON to the one in the repository.
    def self.telefon
        ENV['HS_TELEFON'].to_s.empty? ? 'telefon' : ENV['HS_TELEFON']
    end

    def self.cases(check)
        (check['cases'] || []).first(MAX_CASES)
    end

    def self.seconds(check)
        (check['timeout'] || SECONDS).to_i
    end

    # for TaskCheck.check_calls: all cases together, within the 30 seconds
    # a check may take
    def self.timeout(check)
        [cases(check).size * (seconds(check) + 1) + 2, 30].min
    end

    def self.command_for_check(check)
        cases(check).each_with_index.map do |c, i|
            "printf '%s\\n' #{Shellwords.escape("#{MARK}#{i}")}; " +
                Shellwords.join([telefon, '--test', '--seconds', seconds(check).to_s, '--keys', c['keys'].to_s, check['file'].to_s]) +
                ' 2>&1'
        end.join('; ')
    end

    def self.patterns(value)
        Array(value).map(&:to_s)
    end

    def self.spec_errors(check)
        errors = []
        list = check['cases']
        return ['phone needs cases'] unless list.is_a?(Array) && !list.empty?

        errors << "phone has at most #{MAX_CASES} cases" if list.size > MAX_CASES
        errors << 'phone: timeout must be a number of seconds' if check.key?('timeout') && !check['timeout'].is_a?(Integer)
        list.each_with_index do |c, i|
            unless c.is_a?(Hash)
                errors << "phone case #{i + 1} must be a mapping"
                next
            end
            (c.keys - CASE_KEYS).each { |k| errors << "phone case #{i + 1}: unknown key #{k}" }
            errors << "phone case #{i + 1}: keys must be text like '1,2'" if c.key?('keys') && !c['keys'].is_a?(String) && !c['keys'].is_a?(Integer)
            errors << "phone case #{i + 1}: keys may hold 0-9, *, #, - and commas" unless c['keys'].to_s.match?(/\A[0-9*#,\- ]*\z/)
            %w[says not_says].each do |key|
                patterns(c[key]).each do |pattern|
                    Regexp.new(pattern)
                rescue RegexpError => e
                    errors << "phone case #{i + 1}: #{key} #{pattern.inspect} is no regular expression (#{e.message})"
                end
            end
            errors << "phone case #{i + 1}: music must be true or a pattern" if c.key?('music') && !(c['music'] == true || c['music'].is_a?(String))
            errors << "phone case #{i + 1}: ends must be true or false" if c.key?('ends') && ![true, false].include?(c['ends'])
            errors << "phone case #{i + 1}: say must be text" if c.key?('say') && !c['say'].is_a?(String)
            if (CASE_KEYS - %w[keys say] & c.keys).empty?
                errors << "phone case #{i + 1} expects nothing (says, not_says, music or ends)"
            end
        end
        errors
    end

    # »Ohne Tasten«, »Nach der Taste 1«, »Nach den Tasten 1 und dann 2«
    def self.after_keys(keys)
        answers = keys.to_s.split(',').map(&:strip)
        return 'Ohne eine Taste' if answers.empty? || answers.all? { |a| a == '-' }

        shown = answers.map { |a| a == '-' ? 'nichts' : a }
        return "Nach der Taste #{shown.first}" if shown.size == 1 && shown.first.length == 1
        return "Nach den Tasten #{shown.first}" if shown.size == 1

        "Nach den Tasten #{shown[0..-2].join(', ')} und dann #{shown.last}"
    end

    # a pattern as the message can name it: plain words as they are
    def self.shown(pattern)
        pattern.match?(/[\\\[\](){}|^$.*+?]/) ? nil : "»#{pattern}«"
    end

    def self.matches?(pattern, text)
        Regexp.new(pattern, Regexp::IGNORECASE, timeout: 0.5).match?(text)
    end

    # the output split by case: [[line, …], …]
    def self.split(output)
        runs = []
        output.to_s.each_line do |line|
            line = line.chomp
            if line.start_with?(MARK)
                runs << []
            elsif runs.any?
                runs.last << line
            end
        end
        runs
    end

    def self.read_text(cases, runs)
        cases.each_with_index.map do |c, i|
            lines = runs[i] || []
            shown = lines.first(SHOWN_LINES)
            shown << "… (#{lines.size - SHOWN_LINES} weitere Zeilen)" if lines.size > SHOWN_LINES
            "#{after_keys(c['keys'])}:\n\n```\n#{shown.empty? ? '(keine Ausgabe)' : shown.join("\n").gsub('```', "'''")}\n```"
        end.join("\n\n")
    end

    # What is wrong with one run, or nil.
    def self.problem(c, lines)
        # c['file']: the game's file, for the line of an error (set by evaluate)
        said = lines.select { |l| l.start_with?('Telefon: ') }.map { |l| l.delete_prefix('Telefon: ') }
        music = lines.select { |l| l.start_with?('Musik: ') }.map { |l| l.delete_prefix('Musik: ') }
        finish = lines.reverse.find { |l| l.start_with?('Ende: ') }.to_s.delete_prefix('Ende: ')
        errors = lines.select { |l| l.start_with?('Fehler: ') }.map { |l| l.delete_prefix('Fehler: ') }
        when_ = after_keys(c['keys'])

        if lines.any? { |l| l.match?(/\A(telefon: command not found|sh: \d+: telefon: not found|.*telefon: not found)/i) }
            return 'Den Befehl `telefon` gibt es in deinem Workspace noch nicht. Starte ihn neu, dann ist er da.'
        end
        if lines.empty?
            return "#{when_} kommt aus dem Spiel gar nichts."
        end
        if finish.start_with?('Das Spiel ließ sich nicht starten')
            return "Das Spiel ließ sich nicht starten: #{finish.sub(/\ADas Spiel ließ sich nicht starten: /, '')}"
        end
        if finish.start_with?('Das Spiel ist mit einem Fehler beendet')
            # the line that names the error: first in Ruby, last in Python and Node
            lines_ = errors.reject { |e| e.strip.empty? || e.match?(/\A\s/) || e.start_with?('Traceback') }
            last = lines_.reverse.find { |e| e.match?(/(Error|Exception)\b/) } || lines_.last || errors.last
            name = File.basename(c['file'].to_s)
            line = errors.reverse.map { |e| e[/File ".*?#{Regexp.escape(name)}", line (\d+)/, 1] || e[/#{Regexp.escape(name)}:(\d+)/, 1] }.compact.first
            where = line ? " in Zeile #{line}" : ''
            return "#{when_} bricht das Spiel#{where} mit einem Fehler ab#{last ? ": `#{last.strip.sub(/\A.*?:\d+:in '[^']*': /, '')}`" : '.'}"
        end
        if finish.start_with?('Das Spiel lief länger')
            return "#{when_} lief das Spiel länger als die Prüfung wartet. Steckt eine Schleife fest, die nie nach Tasten fragt?"
        end

        patterns(c['says']).each do |pattern|
            next if said.any? { |s| matches?(pattern, s) }

            name = shown(pattern)
            return c['say'] || (name ? "#{when_} sagt das Spiel nichts mit #{name}." : "#{when_} sagt das Spiel noch nicht, was es soll.")
        end
        patterns(c['not_says']).each do |pattern|
            hit = said.find { |s| matches?(pattern, s) }
            next unless hit

            return c['say'] || "#{when_} sagt das Spiel »#{hit}«; das gehört hier nicht hin."
        end
        if c['music']
            missing = music.find { |m| m.include?('(diese Datei gibt es im Ordner des Spiels nicht)') }
            playing = music.reject { |m| m == 'aus' || m.include?('(diese Datei gibt es') }
            playing = playing.select { |m| matches?(c['music'], m) } if c['music'].is_a?(String)
            if playing.empty?
                return "Die Musik `#{missing.sub(/ \(diese Datei.*\z/, '')}` gibt es im Ordner des Spiels nicht." if missing
                return c['say'] || "#{when_} spielt das Spiel keine Musik."
            end
        end
        if c['ends'] == true && finish.start_with?('Das Spiel wartet auf')
            return c['say'] || "#{when_} fragt das Spiel noch nach einer Taste; hier sollte es zu Ende sein."
        end
        if c['ends'] == false && !finish.start_with?('Das Spiel wartet auf')
            return c['say'] || "#{when_} ist das Spiel schon zu Ende; es sollte noch nach einer Taste fragen."
        end
        nil
    end

    # [status, message, read] for TaskCheck.check_calls.
    def self.evaluate(task, check, output, timed_out: false)
        list = cases(check)
        runs = split(output)
        read = read_text(list, runs)
        if timed_out && runs.size < list.size
            return [:wrong, "Das Spiel lief länger als die Prüfung wartet (#{timeout(check)} Sekunden). Steckt eine Schleife fest, die nie nach Tasten fragt?", read]
        end
        return [:wrong, 'Das Spiel ließ sich nicht prüfen.', "```\n#{output.to_s.lines.last(8).join}\n```"] if runs.empty?

        list.each_with_index do |c, i|
            message = problem(c.merge('file' => check['file']), runs[i] || [])
            return [:wrong, message, read] if message
        end
        n = list.size
        [:correct, n == 1 ? 'Das Spiel tut, was es soll.' : "In allen #{n} Proben tut das Spiel, was es soll.", read]
    rescue RegexpError, Regexp::TimeoutError
        [:wrong, 'Das Spiel ließ sich nicht prüfen.', read]
    end
end
