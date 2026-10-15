require 'json'
require 'shellwords'

# Check type python (docs/aufgaben.md): calls into a Python file the person
# wrote and compares what comes back.
#
# A fixed harness (HARNESS) loads the file with python3 in the person's
# Workspace, as the guest user that can read /workspace but not change it
# (the code is theirs, rule 1), evaluates each call from the task in the
# file's namespace and prints one JSON line. What the program itself prints
# is swallowed. Everything after that works on the JSON and can be tested
# without a Workspace.
#
#   check:
#     type: python
#     file: automata/lock.py
#     cases:
#       - opens('1213'): true                # call: expected value
#       - opens('121213'): true
#       - call: run(['münze']) == 'offen'
#         expect: false
#         say: Nach einer Münze darf das Drehkreuz noch nicht offen sein.
#
# Calls and expected values are Python expressions; true, false, ~ and
# numbers in YAML become True, False, None and the number.
module PythonCheck
    MAX_CASES = 50
    MAX_SHOWN = 12
    RESULT_MARK = '__HS_RESULT__'

    HARNESS = <<~PY
        import contextlib, importlib.util, io, json, os, sys, traceback
        path, cases = sys.argv[1], json.loads(sys.argv[2])
        full = os.path.abspath(path)
        sink = io.StringIO()
        result = {'cases': []}

        def line_in_file(error):
            lines = [f.lineno for f in traceback.extract_tb(error.__traceback__) if os.path.abspath(f.filename) == full]
            return lines[-1] if lines else None

        def text(error):
            return (type(error).__name__ + ': ' + str(error))[:300]

        def short(value):
            shown = repr(value)
            return shown if len(shown) <= 200 else shown[:200] + '…'

        def main():
            if not os.path.isfile(full):
                result['error'] = {'kind': 'missing'}
                return
            sys.path.insert(0, os.path.dirname(full))
            spec = importlib.util.spec_from_file_location('student', full)
            module = importlib.util.module_from_spec(spec)
            try:
                with contextlib.redirect_stdout(sink):
                    spec.loader.exec_module(module)
            except SyntaxError as error:
                result['error'] = {'kind': 'syntax', 'line': error.lineno, 'message': error.msg}
                return
            except BaseException as error:
                result['error'] = {'kind': 'load', 'line': line_in_file(error), 'message': text(error)}
                return
            for case in cases:
                entry = {'call': case['call'], 'expect': case['expect']}
                try:
                    expected = eval(case['expect'], {})
                    with contextlib.redirect_stdout(sink):
                        got = eval(case['call'], vars(module))
                    entry['got'] = short(got)
                    entry['ok'] = bool(got == expected)
                except BaseException as error:
                    entry['ok'] = False
                    entry['error'] = text(error)
                    entry['line'] = line_in_file(error)
                result['cases'].append(entry)

        main()
        sys.stdout.write('\\n#{RESULT_MARK}' + json.dumps(result) + '\\n')
    PY

    # The cases of a check as [{ 'call' =>, 'expect' => (Python), 'say' => }].
    def self.cases(check)
        (check['cases'] || []).map do |item|
            if item.is_a?(Hash) && item.key?('call')
                { 'call' => item['call'].to_s, 'expect' => python_value(item['expect']), 'say' => item['say'] }
            else
                call, expect = item.first
                { 'call' => call.to_s, 'expect' => python_value(expect), 'say' => nil }
            end
        end
    end

    def self.python_value(value)
        case value
        when true then 'True'
        when false then 'False'
        when nil then 'None'
        else value.to_s
        end
    end

    def self.spec_errors(check)
        errors = []
        file = check['file']
        errors << 'file must end in .py' if file.is_a?(String) && !file.end_with?('.py')
        list = check['cases']
        unless list.is_a?(Array) && !list.empty? && list.size <= MAX_CASES
            return errors << "cases must be a list of 1 to #{MAX_CASES} calls"
        end

        list.each_with_index do |item, i|
            if item.is_a?(Hash) && item.key?('call')
                extra = item.keys - %w[call expect say]
                errors << "cases[#{i}]: unknown keys: #{extra.join(', ')}" unless extra.empty?
                errors << "cases[#{i}] needs call and expect" unless item['call'].is_a?(String) && !item['call'].strip.empty? && item.key?('expect')
                errors << "cases[#{i}]: say must be text" if item.key?('say') && !item['say'].is_a?(String)
            elsif !(item.is_a?(Hash) && item.size == 1 && item.keys.first.is_a?(String))
                errors << "cases[#{i}] must be »call: expected« or have call, expect and say"
            end
        end
        errors << 'timeout must be 1 to 30 seconds' if check.key?('timeout') && !(check['timeout'].is_a?(Integer) && check['timeout'].between?(1, 30))
        errors
    end

    # The command that prints the result, run in /workspace as the guest.
    def self.command(file, cases)
        payload = cases.map { |c| { 'call' => c['call'], 'expect' => c['expect'] } }
        Shellwords.join(['python3', '-I', '-B', '-c', HARNESS, file, JSON.generate(payload)])
    end

    def self.parse(output)
        line = output.to_s.lines.reverse.find { |l| l.start_with?(RESULT_MARK) }
        return nil unless line

        data = JSON.parse(line.delete_prefix(RESULT_MARK))
        data.is_a?(Hash) ? data : nil
    rescue JSON::ParserError
        nil
    end

    # [status, message, read] for the harness output.
    def self.evaluate(task, check, output, timed_out: false)
        file = check['file']
        seconds = check['timeout'] || 10
        if timed_out
            return [:wrong, "`#{file}` lief länger als #{seconds} Sekunden und wurde abgebrochen. Steckt irgendwo eine Endlosschleife?", nil]
        end

        data = parse(output)
        if data.nil?
            shown = output.to_s.strip.lines.last(8).join
            read = shown.empty? ? nil : "```\n#{shown.gsub('```', "'''")}\n```"
            return [:wrong, "`#{file}` ließ sich nicht prüfen.", read]
        end

        if (error = data['error'])
            message = case error['kind']
                when 'missing' then "`#{file}` gibt es nicht."
                when 'syntax' then "In `#{file}`#{at_line(error['line'])} ist ein Syntaxfehler: #{error['message']}."
                else "Schon beim Laden von `#{file}` bricht Python ab#{at_line(error['line'])}: `#{error['message']}`"
            end
            return [:wrong, message, nil]
        end

        results = data['cases'] || []
        says = cases(check).map { |c| c['say'] }
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
        message = if r['error']
            "`#{r['call']}` bricht ab#{at_line(r['line'])}: `#{r['error']}`"
        else
            "`#{r['call']}` ergibt `#{r['got']}`, richtig wäre `#{r['expect']}`."
        end
        message += " #{says[failed_at]}" if says[failed_at]
        [:wrong, message, read]
    end

    def self.at_line(line)
        line ? " (Zeile #{line})" : ''
    end
end
