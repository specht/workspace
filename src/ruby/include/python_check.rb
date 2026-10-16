require 'json'
require 'shellwords'
require_relative 'call_check'

# Check type python (docs/aufgaben.md): calls into a Python file the person
# wrote and compares what comes back.
#
# A fixed harness (HARNESS) loads the file with python3 in the person's
# Workspace, as the guest user that can read /workspace but not change it
# (the code is theirs, rule 1), evaluates each call from the task in the
# file's namespace and prints one JSON line. Everything after that works on
# the JSON and can be tested without a Workspace.
#
# Only the definitions of the file are run: imports, functions, classes,
# constants (names in capitals) and the module variables that these or the
# calls of the check use, and statements that fill one of them (TABLE[key] =
# …, TABLE.append(…), also in a loop). The program itself at the top level (a
# loop that reads input, a picture that is saved) is left out, so a plain
# script needs no »if __name__ == '__main__':«; the line numbers stay as they
# are. An import after the first function or class may belong to the program:
# it runs, but a missing module is no error.
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
#       - call: run(['tritt'])
#         raises: true                       # must fail with an exception
#
# Calls and expected values are Python expressions; true, false, ~ and
# numbers in YAML become True, False, None and the number. Reading the
# result and the feedback are shared with RubyCheck (CallCheck).
module PythonCheck
    MAX_CASES = CallCheck::MAX_CASES
    RESULT_MARK = CallCheck::RESULT_MARK

    HARNESS = <<~PY
        import ast, contextlib, importlib.util, io, json, os, sys, traceback
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

        def definitions(tree, wanted):
            """The module with only the definitions and the variables they need."""
            def loaded(node):
                """The names a statement reads; of a function or class only those from outside."""
                names = {n.id for n in ast.walk(node) if isinstance(n, ast.Name) and isinstance(n.ctx, ast.Load)}
                if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                    local = {n.id for n in ast.walk(node) if isinstance(n, ast.Name) and isinstance(n.ctx, ast.Store)}
                    local |= {a.arg for n in ast.walk(node) if isinstance(n, ast.arguments)
                              for a in n.posonlyargs + n.args + n.kwonlyargs + [n.vararg, n.kwarg] if a}
                    names -= local
                return names

            def stored(node):
                targets = node.targets if isinstance(node, ast.Assign) else [node.target]
                return {n.id for t in targets for n in ast.walk(t) if isinstance(n, ast.Name)}

            mutators = {'append', 'extend', 'insert', 'update', 'add', 'setdefault', 'remove', 'discard', 'clear', 'sort', 'reverse', 'pop'}

            def base(node):
                while isinstance(node, (ast.Subscript, ast.Attribute)):
                    node = node.value
                return node.id if isinstance(node, ast.Name) else None

            def filled(node):
                """The names a statement writes into, anywhere inside it."""
                names = set()
                for n in ast.walk(node):
                    if isinstance(n, (ast.Assign, ast.AugAssign, ast.AnnAssign)):
                        for target in (n.targets if isinstance(n, ast.Assign) else [n.target]):
                            for t in ast.walk(target):
                                if isinstance(t, (ast.Subscript, ast.Attribute, ast.Name)) and base(t):
                                    names.add(base(t))
                    elif isinstance(n, ast.Call) and isinstance(n.func, ast.Attribute) and n.func.attr in mutators and base(n.func.value):
                        names.add(base(n.func.value))
                return names

            defining = (ast.Import, ast.ImportFrom, ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)
            body = tree.body
            kept = {i for i, node in enumerate(body) if isinstance(node, defining) or
                    (isinstance(node, ast.Try) and all(isinstance(n, (ast.Import, ast.ImportFrom)) for n in node.body))}
            needed = set(wanted)
            for i in kept:
                needed |= loaded(body[i])
            changed = True
            while changed:
                changed = False
                for i, node in enumerate(body):
                    if i in kept or isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                        continue
                    if any(name in needed or name.isupper() for name in filled(node)):
                        kept.add(i)
                        needed |= loaded(node)
                        changed = True
            begun = min([i for i, node in enumerate(body) if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef))], default=len(body))
            statements = []
            for i in sorted(kept):
                node = body[i]
                if i > begun and isinstance(node, (ast.Import, ast.ImportFrom)):
                    # an import of the program: a missing module is no error
                    handler = ast.ExceptHandler(type=ast.Name(id='ImportError', ctx=ast.Load()), name=None, body=[ast.Pass()])
                    node = ast.copy_location(ast.Try(body=[node], handlers=[handler], orelse=[], finalbody=[]), node)
                    ast.fix_missing_locations(node)
                statements.append(node)
            return ast.Module(body=statements, type_ignores=[])

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
                with open(full, encoding='utf-8') as file:
                    tree = ast.parse(file.read(), full)
                wanted = set()
                for case in cases:
                    for expression in (case['call'], case.get('expect') or 'None'):
                        try:
                            wanted |= {n.id for n in ast.walk(ast.parse(expression, mode='eval')) if isinstance(n, ast.Name)}
                        except SyntaxError:
                            pass
                code = compile(definitions(tree, wanted), full, 'exec')
                with contextlib.redirect_stdout(sink):
                    exec(code, vars(module))
            except SyntaxError as error:
                result['error'] = {'kind': 'syntax', 'line': error.lineno, 'message': error.msg}
                return
            except BaseException as error:
                result['error'] = {'kind': 'load', 'line': line_in_file(error), 'message': text(error)}
                return
            for case in cases:
                entry = {'call': case['call'], 'expect': case['expect']}
                if case.get('raises'):
                    try:
                        with contextlib.redirect_stdout(sink):
                            got = eval(case['call'], vars(module))
                        entry['got'] = short(got)
                        entry['ok'] = False
                    except Exception as error:
                        entry['ok'] = True
                        entry['error'] = text(error)
                    result['cases'].append(entry)
                    continue
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
        payload = cases.map { |c| { 'call' => c['call'], 'expect' => c['expect'], 'raises' => c['raises'] } }
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
