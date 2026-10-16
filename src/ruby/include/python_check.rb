require 'json'
require 'shellwords'
require_relative 'call_check'

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
# numbers in YAML become True, False, None and the number. Reading the
# result and the feedback are shared with RubyCheck (CallCheck).
module PythonCheck
    MAX_CASES = CallCheck::MAX_CASES
    RESULT_MARK = CallCheck::RESULT_MARK

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
            # -I keeps PYTHONPATH and the current folder out; what the person
            # installed with pip (Pillow for Steganographie) comes back in.
            import site
            site.addsitedir(site.getusersitepackages())
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
        CallCheck.cases(check, method(:python_value))
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
        CallCheck.spec_errors(check, '.py')
    end

    # The command that prints the result, run in /workspace as the guest.
    def self.command(file, cases)
        payload = cases.map { |c| { 'call' => c['call'], 'expect' => c['expect'] } }
        Shellwords.join(['python3', '-I', '-B', '-c', HARNESS, file, JSON.generate(payload)])
    end

    def self.parse(output)
        CallCheck.parse(output)
    end

    # [status, message, read] for the harness output.
    def self.evaluate(task, check, output, timed_out: false)
        CallCheck.evaluate(check, cases(check), output, language: 'Python', timed_out: timed_out)
    end
end
