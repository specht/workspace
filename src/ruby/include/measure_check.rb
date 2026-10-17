require 'json'
require 'shellwords'

# Check types measure and command (docs/aufgaben.md).
#
# measure: the student measures something in their Workspace (ping,
# traceroute, curl) and types in the value; the server runs the same
# command in the student's container, as the student, and compares. The
# command comes from the task file, never from the student.
#
# command: the student types a command; it runs in their own container, as
# them, with a time limit, exactly as if they had typed it in their
# terminal. The output is compared with what the task expects.
#
# The runner is passed in: lambda(command, timeout) -> { output:, status:,
# timed_out: } or nil if the Workspace is not running.
module MeasureCheck
    COUNTRIES = {
        'de' => %w[Deutschland Germany], 'be' => %w[Belgien Belgium], 'nl' => %w[Niederlande Holland Netherlands],
        'fr' => %w[Frankreich France], 'at' => %w[Österreich Austria], 'ch' => %w[Schweiz Switzerland],
        'uk' => %w[Großbritannien England Vereinigtes\ Königreich UK], 'us' => %w[USA Vereinigte\ Staaten],
        'pe' => %w[Peru], 'it' => %w[Italien Italy], 'es' => %w[Spanien Spain], 'pl' => %w[Polen Poland],
        'se' => %w[Schweden Sweden], 'dk' => %w[Dänemark Denmark], 'ie' => %w[Irland Ireland],
        'lu' => %w[Luxemburg Luxembourg], 'cz' => %w[Tschechien Czechia], 'jp' => %w[Japan],
    }.freeze
    # Hosting companies whose servers answer with a name under a generic
    # domain (.net, .com): the country they are in. rubiomonocoat.pe (TCP/IP
    # tutorial) moved from linweb443.webhosting.be to Combell in 2026.
    HOSTERS = {
        'combell-ops.net' => 'be', 'combell.net' => 'be',
        'your-server.de' => 'de', 'your-cloud.host' => 'de', 'hetzner.com' => 'de',
        'ovh.net' => 'fr', 'ip-ovh.net' => 'fr',
    }.freeze
    INITIAL_TTLS = [32, 64, 128, 255].freeze

    # --- extractors (pure) ------------------------------------------------------

    def self.extract(name, output)
        text = output.to_s
        case name
        when 'ping_rtt_avg'
            m = text.match(%r{(?:rtt|round-trip) min/avg/max(?:/mdev|/stddev)? = [\d.]+/([\d.]+)/})
            m && m[1].to_f
        when 'ping_hops'
            m = text.match(/ttl=(\d+)/i)
            return nil unless m

            ttl = m[1].to_i
            initial = INITIAL_TTLS.find { |i| i >= ttl }
            initial && initial - ttl
        when 'ping_reply_country'
            m = text.match(/bytes from ([^\s:(]+)/)
            return nil unless m

            host = m[1]
            return nil if host =~ /\A[\d.]+\z/ || host.include?(':')

            host = host.downcase
            hoster = HOSTERS.keys.find { |domain| host == domain || host.end_with?(".#{domain}") }
            tld = hoster ? HOSTERS[hoster] : host.split('.').last
            tld = 'uk' if tld == 'gb'
            (COUNTRIES[tld] || [tld.upcase]).first
        when 'traceroute_hops'
            text.lines.count { |l| l =~ /\A\s*\d+\s/ }
        when 'dns_answers'
            # dig +short: one answer per line; of an MX record only the name,
            # without its priority; without the final dot, in lower case
            answers = text.lines.map(&:strip).reject { |l| l.empty? || l.start_with?(';') }
                          .map { |l| l.split.last.sub(/\.\z/, '').downcase }
            answers.empty? ? nil : answers
        when /\Ahttp_header:(.+)\z/
            header = $1.downcase
            line = text.lines.find { |l| l.split(':', 2).first.to_s.strip.downcase == header }
            line && line.split(':', 2)[1].to_s.strip
        end
    end

    # --- measure ----------------------------------------------------------------

    # answer: JSON list of the student's values, one per field (or a single
    # value for one field). Returns [status, message, read].
    def self.measure(check, answer, runner)
        fields = check['fields']
        given = parse_answer(answer, fields.size)
        if given.nil? || given.any? { |g| g.to_s.strip.empty? }
            return [:invalid, fields.size == 1 ? 'Trag zuerst deinen Messwert ein.' : 'Trag zuerst in jedes Feld einen Messwert ein.', nil]
        end

        measured = fields.map do |field|
            result = runner.call(field['run'], 20)
            return [:unavailable, 'Starte zuerst deinen Workspace. Ich messe in deinem Workspace nach.', nil] if result.nil?

            value = extract(field['extract'], result[:output])
            [field, value, result]
        end
        read = measured.map do |field, value, _|
            "- #{field['label']}: `#{field['run']}` ergibt gerade #{value.nil? ? 'nichts Verwertbares' : "**#{format_value(value, check)}**"}"
        end.join("\n")
        if measured.any? { |_, value, _| value.nil? }
            return [:unavailable, 'Meine Messung hat gerade kein Ergebnis gebracht (vielleicht keine Antwort im Netz). Versuch es gleich noch einmal.', read]
        end

        text = check['compare'] == 'text' || measured.any? { |_, v, _| v.is_a?(String) || v.is_a?(Array) }
        problems = []
        measured.each_with_index do |(field, value, _), i|
            ok = if value.is_a?(Array)
                # Several answers (DNS): any one of them is right
                value.any? { |v| same_text?(given[i].to_s.strip.sub(/\.\z/, ''), v) }
            else
                text ? same_text?(given[i], value) : close_enough?(given[i], value, check)
            end
            next if ok

            shown = text ? "»#{given[i].strip}«" : "#{given[i].strip} #{check['unit']}".strip
            problems << "#{field['label']}: Du hast #{shown} eingetragen, meine Messung ergibt gerade #{format_value(value, check)}."
        end
        return [:correct, 'Passt zu meiner Messung.', read] if problems.empty?

        [:wrong, problems.join("\n\n"), read]
    end

    def self.parse_answer(answer, count)
        text = answer.to_s.strip
        values = if text.start_with?('[')
            JSON.parse(text) rescue nil
        else
            [text]
        end
        return nil unless values.is_a?(Array) && values.size == count

        values.map(&:to_s)
    end

    def self.format_value(value, check)
        return value.to_s if value.is_a?(String)
        return value.join(', ') if value.is_a?(Array)

        number = value.is_a?(Float) ? (value < 10 ? value.round(2) : value.round(1)) : value
        "#{number.to_s.sub('.', ',')} #{check['unit']}".strip
    end

    # Within tolerance (absolute) or tolerance_percent (of the measurement).
    # Values below 1 count as equal when both are below 1 (loopback pings).
    def self.close_enough?(given, measured, check)
        value = TaskCheck.parse_number(given)
        return false if value.nil?
        return true if value < 1 && measured < 1 && measured.is_a?(Float)

        tolerance = check['tolerance'].to_f
        tolerance = [tolerance, measured.abs * check['tolerance_percent'].to_f / 100].max if check['tolerance_percent']
        (value - measured).abs <= tolerance + 1e-9
    end

    def self.same_text?(given, measured)
        g = given.to_s.strip.downcase
        m = measured.to_s.strip.downcase
        return false if g.empty?
        return true if g == m

        # nginx/1.24.0 matches »nginx«; a country matches its other names.
        aliases = COUNTRIES.values.find { |names| names.map(&:downcase).include?(m) }
        return true if aliases && aliases.map(&:downcase).include?(g)

        m.split(%r{[/\s(]}).first == g
    end

    # --- command ----------------------------------------------------------------

    MAX_SHOWN_LINES = 12
    # Printed instead of running the command when its folder (dir) is missing
    NO_DIR = '__HS_NO_DIR__'

    # The folder a command runs in, relative to /workspace: the task's dir,
    # the folder the page works in, so a student types what they would in
    # their terminal there. A command that names the folder itself
    # (bitstein/tatort.txt, cd bitstein) runs in /workspace as before.
    def self.command_dir(check, command)
        dir = check['dir'].to_s.sub(%r{/+\z}, '')
        return nil if dir.empty?
        return nil if command.match?(%r{(?<![\w.-])#{Regexp.escape(dir)}(/|(?![\w.-]))})

        dir
    end

    # Returns [status, message, read].
    def self.command(task, check, answer, runner)
        command = answer.to_s.strip
        return [:invalid, 'Trag zuerst einen Befehl ein.', nil] if command.empty?
        return [:invalid, 'Ein Befehl pro Zeile reicht hier.', nil] if command.include?("\n")

        dir = command_dir(check, command)
        run = dir ? "cd -- #{Shellwords.escape(dir)} 2>/dev/null || { echo #{NO_DIR}; exit 1; }; #{command}" : command
        result = runner.call(run, check['timeout'] || 10)
        return [:unavailable, 'Starte zuerst deinen Workspace. Der Befehl läuft in deinem Workspace.', nil] if result.nil?

        output = result[:output].to_s
        if dir && output.start_with?(NO_DIR)
            return [:wrong, "Der Befehl läuft im Ordner `#{dir}` in deinem Workspace, aber den gibt es dort noch nicht.", nil]
        end
        lines = output.lines.map(&:chomp)
        lines.pop while lines.any? && lines.last.strip.empty?
        shown = lines.first(MAX_SHOWN_LINES).join("\n")
        more = lines.size > MAX_SHOWN_LINES ? "\n… (#{lines.size - MAX_SHOWN_LINES} weitere Zeilen)" : ''
        place = "In `/workspace#{dir ? "/#{dir}" : ''}`:\n\n"
        read = "#{place}`$ #{command.gsub('`', "'")}`\n\n```\n#{shown.empty? ? '(keine Ausgabe)' : shown}#{more}\n```"
        if result[:timed_out]
            return [:wrong, "Der Befehl lief länger als #{check['timeout'] || 10} Sekunden und wurde abgebrochen.", read]
        end

        expect = check['output'] || {}
        problem = if expect['lines'] && lines.size != expect['lines']
            "Die Ausgabe hat #{lines.size} #{lines.size == 1 ? 'Zeile' : 'Zeilen'}, erwartet #{expect['lines'] == 1 ? 'ist eine' : "sind #{expect['lines']}"}."
        elsif expect['matching'] && !lines.all? { |l| Regexp.new(expect['matching'], timeout: 0.5).match?(l) }
            'Die Ausgabe sieht noch nicht so aus wie verlangt.'
        elsif expect['not_matching'] && lines.any? { |l| Regexp.new(expect['not_matching'], timeout: 0.5).match?(l) }
            'Die Ausgabe sieht noch nicht so aus wie verlangt.'
        elsif lines.empty?
            'Der Befehl hat nichts ausgegeben.'
        end
        return [:correct, 'Passt.', read] if problem.nil?

        say = (task['feedback'] || []).find do |entry|
            pattern = entry['when']['output_matches']
            pattern && lines.any? { |l| Regexp.new(pattern, timeout: 0.5).match?(l) }
        end
        [:wrong, say ? say['say'] : problem, read]
    rescue RegexpError, Regexp::TimeoutError
        [:wrong, 'Die Ausgabe sieht noch nicht so aus wie verlangt.', read]
    end
end
