require 'json'

# What the check types python and ruby share (docs/aufgaben.md): both call
# into a file the person wrote and compare what comes back. Each language
# has its own harness (PythonCheck::HARNESS, RubyCheck::HARNESS) that loads
# the file in the person's Workspace as the guest, evaluates every call and
# prints one JSON line after RESULT_MARK:
#
#   {"cases": [{"call", "expect", "got", "ok", "error", "line"}, ...]}
#
#   {"error": {"kind": "missing" | "syntax" | "load", "line", "message"}}
#
# A case with raises: true expects the call to fail with an error (a
# StandardError in Ruby, an Exception in Python): then error holds it and ok is
# true. A case with show: true (a Kata's random cases, whose expect is the
# reference to compute) also gets shown, the expected value as it came out,
# so that messages name the value, not the reference.
#
# Everything here works on that line and can be tested without a Workspace.
module CallCheck
    MAX_CASES = 50
    MAX_SHOWN = 12
    RESULT_MARK = '__HS_RESULT__'

    # The cases of a check as [{ 'call' =>, 'expect' =>, 'say' =>, 'raises' => }],
    # with true, false, ~ and numbers from YAML written by literal in the
    # language.
    def self.cases(check, literal)
        (check['cases'] || []).map do |item|
            if item.is_a?(Hash) && item.key?('call')
                raises = item['raises'] == true
                { 'call' => item['call'].to_s, 'expect' => raises ? literal.call(nil) : literal.call(item['expect']),
                  'say' => item['say'], 'raises' => raises }
            else
                call, expect = item.first
                { 'call' => call.to_s, 'expect' => literal.call(expect), 'say' => nil, 'raises' => false }
            end
        end
    end

    def self.spec_errors(check, extension)
        errors = []
        file = check['file']
        errors << "file must end in #{extension}" if file.is_a?(String) && !file.end_with?(extension)
        list = check['cases']
        unless list.is_a?(Array) && !list.empty? && list.size <= MAX_CASES
            return errors << "cases must be a list of 1 to #{MAX_CASES} calls"
        end

        list.each_with_index do |item, i|
            if item.is_a?(Hash) && item.key?('call')
                extra = item.keys - %w[call expect say raises]
                errors << "cases[#{i}]: unknown keys: #{extra.join(', ')}" unless extra.empty?
                errors << "cases[#{i}]: raises must be true" if item.key?('raises') && item['raises'] != true
                errors << "cases[#{i}]: raises or expect, not both" if item.key?('raises') && item.key?('expect')
                unless item['call'].is_a?(String) && !item['call'].strip.empty? && (item.key?('expect') || item['raises'] == true)
                    errors << "cases[#{i}] needs call and expect (or raises: true)"
                end
                errors << "cases[#{i}]: say must be text" if item.key?('say') && !item['say'].is_a?(String)
            elsif !(item.is_a?(Hash) && item.size == 1 && item.keys.first.is_a?(String))
                errors << "cases[#{i}] must be »call: expected« or have call, expect and say"
            end
        end
        errors << 'timeout must be 1 to 30 seconds' if check.key?('timeout') && !(check['timeout'].is_a?(Integer) && check['timeout'].between?(1, 30))
        errors
    end

    def self.parse(output)
        line = output.to_s.lines.reverse.find { |l| l.start_with?(RESULT_MARK) }
        return nil unless line

        data = JSON.parse(line.delete_prefix(RESULT_MARK))
        data.is_a?(Hash) ? data : nil
    rescue JSON::ParserError
        nil
    end

    # How messages name what was checked: a file of the Workspace by its
    # path, the code typed into a card (a Kata, KataCheck) as »dein Code«.
    def self.names(file)
        return { :it => "`#{file}`", :in => "In `#{file}`", :of => "von `#{file}`" } if file

        { :it => 'Dein Code', :in => 'In deinem Code', :of => 'deines Codes' }
    end

    # [status, message, read] for the harness output; language is the name
    # in messages (»bricht Python ab«), cases the result of cases above.
    def self.evaluate(check, cases, output, language:, timed_out: false)
        file = check['file']
        name = names(file)
        seconds = check['timeout'] || 10
        if timed_out
            return [:wrong, "#{name[:it]} lief länger als #{seconds} Sekunden und wurde abgebrochen. Steckt irgendwo eine Endlosschleife?", nil]
        end

        data = parse(output)
        if data.nil?
            shown = output.to_s.strip.lines.last(8).join
            read = shown.empty? ? nil : "```\n#{shown.gsub('```', "'''")}\n```"
            return [:wrong, "#{name[:it]} ließ sich nicht prüfen.", read]
        end

        if (error = data['error'])
            message = case error['kind']
                when 'missing' then "#{name[:it]} gibt es nicht."
                when 'syntax' then "#{name[:in]}#{at_line(error['line'])} ist ein Syntaxfehler: #{error['message']}."
                else "Schon beim Laden #{name[:of]} bricht #{language} ab#{at_line(error['line'])}: `#{error['message']}`"
            end
            return [:wrong, message, nil]
        end

        results = data['cases'] || []
        says = cases.map { |c| c['say'] }
        read = results.first(MAX_SHOWN).map do |r|
            icon = r['ok'] ? "<i class='bi bi-check-lg'></i>" : "<i class='bi bi-x-lg'></i>"
            outcome = r['error'] ? "bricht ab: `#{r['error']}`" : "`#{r['got']}`"
            "- #{icon} `#{r['call']}` → #{outcome}"
        end
        read << "- … und #{results.size - MAX_SHOWN} weitere" if results.size > MAX_SHOWN
        read = read.join("\n")

        failed_at = results.index { |r| !r['ok'] }
        if failed_at.nil?
            n = results.size
            return [:correct, n == 1 ? 'Der Aufruf liefert, was er soll.' : "Alle #{n} Aufrufe liefern, was sie sollen.", read]
        end

        r = results[failed_at]
        message = if cases[failed_at] && cases[failed_at]['raises']
            "`#{r['call']}` ergibt `#{r['got']}`, richtig wäre ein Fehler."
        elsif r['error']
            "`#{r['call']}` bricht ab#{at_line(r['line'])}: `#{r['error']}`"
        else
            "`#{r['call']}` ergibt `#{r['got']}`, richtig wäre `#{r['shown'] || r['expect']}`."
        end
        message += " #{says[failed_at]}" if says[failed_at]
        [:wrong, message, read]
    end

    def self.at_line(line)
        line ? " (Zeile #{line})" : ''
    end
end
