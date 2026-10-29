require 'digest'
require_relative 'call_check'
require_relative 'python_check'
require_relative 'ruby_check'
require_relative 'javascript_check'
require_relative 'recursion_check'
require_relative 'gate_check'
require_relative 'kata_compiled'

# Check type function (docs/aufgaben.md, docs/plan-katas.md): a Kata. The
# code is typed into the card, not written in the Workspace; it runs in a
# container of its own (KataRunner, through KataQueue), with the same
# harness as the checks python and ruby: only the definitions are loaded,
# each call of the task is evaluated and compared, and the result reads like
# theirs, with »dein Code« in place of the file.
#
#   check:
#     type: function
#     language: ruby              # ruby, python, javascript, dart or java; per language as a variant
#     forbid: [Integer#gcd]       # built-ins that are no help here
#     cases:
#       - gcd(60, 48): 12
#       - call: gcd(7, 7)
#         expect: 7
#         say: Der größte Teiler einer Zahl mit sich selbst ist sie selbst.
#     random:                     # cases of their own for each person
#       count: 4
#       call: gcd(a, b)
#       args: { a: 1..1000, b: 1..1000 }
#       reference: a.gcd(b)       # computed in the run, never shown
#     timeout: 10                 # seconds for all cases together
#     slow: Für eine Million …    # the message when the time is up, where
#                                 # speed is the point (not »Endlosschleife?«)
#     recursive: true             # the function must call itself, and
#     loops: false                # use no loop (RecursionCheck, read from
#                                 # the code once the cases pass)
#     gates: [nand]               # only these gates may combine the 0s and
#                                 # 1s of the arguments (GateCheck)
#
#   feedback:
#     - when: { same_as: 'def gcd(a, b) = [a, b].min' }
#       say: Die kleinere Zahl ist nur manchmal ein Teiler der größeren.
#
# forbid: in Ruby Class#method or Class.method, made to fail when the
# person's code calls it (the harness and the reference may still use it);
# in Python a built-in (sorted), a module's function (math.gcd), a
# built-in type (str) or a method of one (list.sort); types and their
# methods cannot be switched off and are looked for in the code instead; in
# JavaScript a function or method by its path (Math.max,
# Array.prototype.sort), made to fail like in Ruby (JavascriptCheck); in
# Dart and Java found in the text of the code (KataCompiled), which runs
# them differently, compiled. random: the values come from a seed per
# person and task version, so a second check has the same cases and a table
# of answers does not pass; the message names the value the reference gave.
# same_as: a typical wrong solution, written by the author and never shown,
# runs on the same cases; if the person's results are its results, its say
# replaces the message.
module KataCheck
    CHECKS = { 'python' => PythonCheck, 'ruby' => RubyCheck, 'javascript' => JavascriptCheck,
               'dart' => DartCheck, 'java' => JavaCheck }.freeze
    FILES = { 'python' => 'kata.py', 'ruby' => 'kata.rb', 'javascript' => 'kata.js', 'dart' => 'kata.dart', 'java' => 'kata.java' }.freeze
    HARNESS_FILES = { 'python' => 'harness.py', 'ruby' => 'harness.rb', 'javascript' => 'harness.js' }.freeze
    # What runs on »Ausführen«: the program as it is, top-level code included.
    PROGRAMS = { 'python' => %w[python3 -I -B kata.py], 'ruby' => %w[ruby -W0 kata.rb], 'javascript' => %w[node kata.js] }.freeze
    MAX_CODE = 4000
    # What is stored as the task's answer: the code per language, as JSON,
    # so each language keeps its own (TaskStore, max:).
    MAX_STORED = (CHECKS.size + 1) * (MAX_CODE + 64)
    RUN_SECONDS = 10
    MAX_RANDOM = 20
    MAX_SECONDS = 30
    FORBIDDEN = 'KataForbidden'

    RUBY_FORBID = /\A[A-Z]\w*(::[A-Z]\w*)*[#.][a-z_]\w*[?!]?\z/
    PYTHON_FORBID = /\A[a-z_]\w*(\.[a-z_]\w*)*\z/
    JAVASCRIPT_FORBID = /\A[A-Za-z_$][\w$]*(\.[A-Za-z_$][\w$]*)+\z/
    # Python's built-in types, whose methods are looked for in the code
    PYTHON_TYPES = %w[list str dict set tuple int float bytes].freeze
    RANGE = /\A\s*(-?\d+)\s*\.\.\s*(-?\d+)\s*\z/

    BUSY = 'Gerade prüfen sehr viele auf einmal. Versuch es gleich noch einmal.'
    EMPTY = 'Du hast noch keinen Code geschrieben.'

    def self.spec_errors(check)
        errors = []
        language = check['language']
        errors << "language must be #{CHECKS.keys.join(' or ')}" unless CHECKS.key?(language)
        errors.concat(CallCheck.spec_errors(check, ''))
        if check.key?('forbid') && KataCompiled::LANGUAGES.include?(language)
            errors.concat(KataCompiled.forbid_errors(check['forbid']))
        elsif check.key?('forbid')
            pattern = { 'python' => PYTHON_FORBID, 'javascript' => JAVASCRIPT_FORBID }.fetch(language, RUBY_FORBID)
            unless check['forbid'].is_a?(Array) && !check['forbid'].empty? && check['forbid'].all? { |f| f.is_a?(String) && f =~ pattern }
                errors << { 'python' => 'forbid must list names like sorted, math.gcd or list.sort',
                            'javascript' => 'forbid must list functions like Math.max or Array.prototype.sort' }
                    .fetch(language, 'forbid must list methods like Integer#gcd or Math.sqrt')
            end
        end
        errors.concat(random_errors(check['random'])) if check.key?('random')
        errors.concat(GateCheck.spec_errors(check)) if check.key?('gates')
        errors << 'gates are not available in Dart and Java' if check.key?('gates') && KataCompiled::LANGUAGES.include?(language)
        errors << 'slow must be text' if check.key?('slow') && !(check['slow'].is_a?(String) && !check['slow'].strip.empty?)
        # read from the code once the cases pass (RecursionCheck)
        errors << 'recursive must be true' if check.key?('recursive') && check['recursive'] != true
        errors << 'loops must be false' if check.key?('loops') && check['loops'] != false
        if (check.key?('recursive') || check.key?('loops')) && RecursionCheck.function_name(check).nil?
            errors << 'recursive and loops need cases that call the function'
        end
        errors
    end

    def self.random_errors(random)
        return ['random must be a mapping with count, call, args and reference'] unless random.is_a?(Hash)

        errors = []
        extra = random.keys - %w[count call args reference]
        errors << "random: unknown keys: #{extra.join(', ')}" unless extra.empty?
        errors << "random.count must be 1 to #{MAX_RANDOM}" unless random['count'].is_a?(Integer) && random['count'].between?(1, MAX_RANDOM)
        %w[call reference].each do |key|
            errors << "random.#{key} must be an expression" unless random[key].is_a?(String) && !random[key].strip.empty?
        end
        args = random['args']
        if args.is_a?(Hash) && !args.empty? && args.keys.all? { |k| k.to_s =~ /\A[a-z_]\w*\z/ }
            args.each do |name, values|
                ok = (values.is_a?(String) && values =~ RANGE && $1.to_i <= $2.to_i) ||
                    (values.is_a?(Array) && !values.empty? && values.all? { |v| literal?(v) })
                errors << "random.args.#{name} must be a range like 1..100 or a list of values" unless ok
            end
        else
            errors << 'random.args must name each argument with its values'
        end
        errors
    end

    def self.literal?(value)
        [Integer, Float, String, TrueClass, FalseClass, NilClass].any? { |k| value.is_a?(k) } ||
            (value.is_a?(Array) && value.all? { |v| literal?(v) })
    end

    def self.feedback_errors(feedback)
        (feedback || []).each_with_index.filter_map do |entry, i|
            code = entry.is_a?(Hash) && entry['when'].is_a?(Hash) ? entry['when']['same_as'] : nil
            next unless entry.is_a?(Hash) && entry['when'].is_a?(Hash) && entry['when'].key?('same_as')

            "feedback[#{i}].when.same_as must be code of at most #{MAX_CODE} characters" unless code.is_a?(String) && !code.strip.empty? && code.size <= MAX_CODE
        end
    end

    # A seed for the random cases of one person and one task version.
    def self.seed(email, task_key, version)
        Digest::SHA256.hexdigest("#{email}\n#{task_key}\n#{version}")[0, 15].to_i(16)
    end

    # A value as a literal of the language.
    def self.literal(value, language)
        return KataCompiled.literal(value, language) if KataCompiled::LANGUAGES.include?(language)

        case value
        when true then language == 'python' ? 'True' : 'true'
        when false then language == 'python' ? 'False' : 'false'
        when nil then { 'python' => 'None', 'javascript' => 'null' }.fetch(language, 'nil')
        when Array then "[#{value.map { |v| literal(v, language) }.join(', ')}]"
        when String then JSON.generate(value)
        else value.to_s
        end
    end

    # The random cases of a check: [{ 'call', 'expect', 'show' }], the call
    # with the values filled in, expect the reference applied to them.
    def self.random_cases(check, seed)
        random = check['random']
        return [] unless random

        language = check['language']
        rng = Random.new(seed)
        names = random['args'].keys.map(&:to_s)
        Array.new(random['count']) do
            values = names.to_h do |name|
                spec = random['args'][name]
                value = spec.is_a?(String) && spec =~ RANGE ? rng.rand($1.to_i..$2.to_i) : spec[rng.rand(spec.size)]
                [name, value]
            end
            shown = values.transform_values { |v| literal(v, language) }
            call = random['call'].gsub(/\b(#{names.map { |n| Regexp.escape(n) }.join('|')})\b/) { shown[$1] }
            arguments = names.map { |n| shown[n] }.join(', ')
            expect = if KataCompiled::LANGUAGES.include?(language)
                KataCompiled.reference(random['reference'], shown)
            elsif language == 'python'
                "(lambda #{names.join(', ')}: #{random['reference']})(#{arguments})"
            elsif language == 'javascript'
                "((#{names.join(', ')}) => (#{random['reference']}))(#{arguments})"
            else
                "->(#{names.join(', ')}) { #{random['reference']} }.call(#{arguments})"
            end
            { 'call' => call, 'expect' => expect, 'say' => nil, 'raises' => false, 'show' => true }
        end
    end

    # What runs before the harness when check forbids built-ins (the
    # JavaScript harness gets the list as an argument instead).
    def self.prelude(check)
        gates = check['gates'] ? GateCheck.prelude(check['language'], check['gates']) : ''
        gates + forbid_prelude(check)
    end

    def self.forbid_prelude(check)
        list = JSON.generate(check['forbid'] || [])
        return '' if list == '[]' || check['language'] == 'javascript'

        if check['language'] == 'python'
            <<~PY
                import ast as _k_ast, builtins as _k_builtins, importlib as _k_importlib, json as _k_json, os as _k_os, sys as _k_sys
                class #{FORBIDDEN}(BaseException):
                    pass
                _k_full = _k_os.path.abspath(_k_sys.argv[1])
                def _k_wrap(original, name):
                    def forbidden(*args, **kwargs):
                        if _k_sys._getframe(1).f_code.co_filename == _k_full:
                            raise #{FORBIDDEN}(name)
                        return original(*args, **kwargs)
                    return forbidden
                # methods of built-in types and built-in types themselves (str,
                # list) cannot be wrapped: isinstance(x, str) must keep working.
                # They are looked for in the code instead.
                _k_methods = {}
                _k_names = {}
                for _k_name in _k_json.loads(#{list.inspect}):
                    _k_owner, _, _k_attr = _k_name.rpartition('.')
                    if _k_owner in #{PYTHON_TYPES.inspect}:
                        _k_methods[_k_attr] = _k_name
                    elif _k_owner:
                        _k_module = _k_importlib.import_module(_k_owner)
                        setattr(_k_module, _k_attr, _k_wrap(getattr(_k_module, _k_attr), _k_name))
                    elif isinstance(getattr(_k_builtins, _k_attr), type):
                        _k_names[_k_attr] = _k_name
                    else:
                        setattr(_k_builtins, _k_attr, _k_wrap(getattr(_k_builtins, _k_attr), _k_name))
                if (_k_methods or _k_names) and _k_os.path.isfile(_k_full):
                    try:
                        _k_tree = _k_ast.parse(open(_k_full, encoding='utf-8').read())
                    except SyntaxError:
                        _k_tree = None
                    for _k_node in (_k_ast.walk(_k_tree) if _k_tree else []):
                        _k_found = (_k_methods.get(_k_node.attr) if isinstance(_k_node, _k_ast.Attribute) else
                                    _k_names.get(_k_node.id) if isinstance(_k_node, _k_ast.Name) else None)
                        if _k_found:
                            _k_sys.stdout.write('\\n#{CallCheck::RESULT_MARK}' + _k_json.dumps({'error': {'kind': 'load', 'line': _k_node.lineno,
                                'message': '#{FORBIDDEN}: ' + _k_found}}) + '\\n')
                            _k_sys.exit(0)
            PY
        else
            <<~RUBY
                require 'json'
                class #{FORBIDDEN} < ScriptError; end
                kata_full = File.expand_path(ARGV[0])
                JSON.parse(#{list.inspect}).each do |name|
                    owner_name, kind, method_name = name.match(/\\A(.+?)([#.])([^#.]+)\\z/).captures
                    owner = Object.const_get(owner_name)
                    (kind == '#' ? owner : owner.singleton_class).prepend(Module.new do
                        define_method(method_name) do |*args, **options, &block|
                            where = caller_locations(1, 1).first
                            raise #{FORBIDDEN}, name if where && File.expand_path(where.path) == kata_full
                            super(*args, **options, &block)
                        end
                    end)
                end
            RUBY
        end
    end

    # [status, message, read]. run: lambda(files:, argv:, timeout:) ->
    # KataRunner::Result, or nil when no run could start (the queue waited
    # too long). seed: for the random cases (seed above).
    def self.check(task, check, code, run, seed: 0)
        return [:invalid, EMPTY, nil] if code.to_s.strip.empty?

        language_name = check['language']
        return compiled_check(task, check, code, run, seed) if KataCompiled::LANGUAGES.include?(language_name)

        language = CHECKS.fetch(language_name)
        file = FILES.fetch(language_name)
        harness = HARNESS_FILES.fetch(language_name)
        cases = language.cases(check) + random_cases(check, seed)
        # with gates: every case once more, wired, after the plain ones
        plain = cases.size
        cases += GateCheck.wired(cases, language_name) if check['gates']
        alternatives = (task['feedback'] || []).select { |f| f.dig('when', 'same_as') }
        files = { file => code.to_s, harness => prelude(check) + language::HARNESS }
        alternatives.each_with_index { |f, i| files["alt#{i + 1}#{File.extname(file)}"] = f['when']['same_as'] }
        # the language's command for its harness, taken apart: what comes
        # before the harness (interpreter and options), and the arguments
        # after the file (the cases, for JavaScript also what is forbidden)
        full = language_name == 'javascript' ? language.argv(file, cases, check['forbid'] || []) : language.argv(file, cases)
        at = full.index(language::HARNESS)
        interpreter = full[0...at - 1]
        arguments = full[(at + 2)..]
        targets = [file] + alternatives.each_index.map { |i| "alt#{i + 1}#{File.extname(file)}" }
        argv = if targets.size == 1
            interpreter + [harness, file] + arguments
        else
            positions = arguments.each_index.map { |i| "\"$#{i}\"" }.join(' ')
            ['sh', '-c', "for f in #{targets.join(' ')}; do #{interpreter.join(' ')} #{harness} \"$f\" #{positions}; done", *arguments]
        end
        seconds = check['timeout'] || 10
        timeout = [seconds * targets.size, MAX_SECONDS].min
        result = run.call(:files => files, :argv => argv, :timeout => timeout)
        return [:unavailable, BUSY, nil] if result.nil?

        outputs = result.too_long ? [] : result.stdout.split(/^(?=#{CallCheck::RESULT_MARK})/)
                                                  .select { |part| part.start_with?(CallCheck::RESULT_MARK) }
        mine = outputs.first.to_s
        evaluate = lambda do |output|
            language.evaluate(task, check.merge('file' => nil), output, :timed_out => result.timed_out && outputs.empty?)
        end
        status, message, read = check['gates'] ? GateCheck.evaluate(check, mine, plain, evaluate) : evaluate.call(mine)
        message = forbidden_message(message) if message.to_s.include?("#{FORBIDDEN}: ")
        message = check['slow'] if check['slow'] && result.timed_out && outputs.empty?
        if status == :wrong && !alternatives.empty?
            say = same_as(mine, outputs.drop(1), alternatives)
            message = say if say
        end
        explanation = status == :wrong ? explain(message, language_name) : nil
        message = "#{message}\n\n#{explanation}" if explanation
        # right results, but a loop where a recursion is asked for
        if status == :correct && (problem = RecursionCheck.problem(check, code, RecursionCheck.function_name(check)))
            return [:wrong, problem, read]
        end
        [status, message, read]
    end

    # Dart and Java (KataCompiled): the code and the typical wrong solutions
    # compiled and run in one go, each target's output in a section.
    def self.compiled_check(task, check, code, run, seed)
        language_name = check['language']
        if (found = KataCompiled.forbidden(code, check['forbid']))
            return [:wrong, "Dein Code (Zeile #{found[1]}) benutzt `#{found[0]}`. Das gibt es in dieser Aufgabe nicht: Du schreibst es gerade selbst.", nil]
        end

        language = CHECKS.fetch(language_name)
        cases = language.cases(check) + random_cases(check, seed)
        alternatives = (task['feedback'] || []).select { |f| f.dig('when', 'same_as') }
        program = KataCompiled.program(language_name, code, alternatives.map { |f| f['when']['same_as'] }, cases)
        seconds = check['timeout'] || 10
        timeout = [seconds * program[:targets].size, MAX_SECONDS].min
        result = run.call(:files => program[:files], :argv => program[:argv], :timeout => timeout)
        return [:unavailable, BUSY, nil] if result.nil?

        sections = result.too_long ? {} : KataCompiled.sections(result.stdout)
        mine = sections[program[:targets].first]
        timed_out = result.timed_out && !(mine && CallCheck.parse(mine))
        status, message, read = KataCompiled.evaluate(language_name, check, code, cases, mine.to_s, :timed_out => timed_out)
        message = check['slow'] if check['slow'] && timed_out
        if status == :wrong && !alternatives.empty?
            others = program[:targets].drop(1).map { |t| sections[t].to_s }
            say = same_as(mine.to_s, others, alternatives)
            message = say if say
        end
        explanation = status == :wrong ? explain(message, language_name) : nil
        message = "#{message}\n\n#{explanation}" if explanation
        if status == :correct && (problem = RecursionCheck.problem(check, code, RecursionCheck.function_name(check)))
            return [:wrong, problem, read]
        end
        [status, message, read]
    end

    # Error messages that stop a beginner, each with a line in German that
    # says what usually lies behind it; the original stays above it.
    # [language, pattern, explanation]; the name in the pattern's first
    # group goes into the explanation as %s.
    EXPLAINED = [
        ['python', /IndentationError|expected an indented block|unindent does not match|unexpected indent/,
         'Python liest an der Einrückung, was zusammengehört: Nach einer Zeile mit `:` kommen eingerückte Zeilen, und alle Zeilen eines Blocks sind gleich weit eingerückt.'],
        ['python', /expected ':'/,
         'Nach `def …`, `if …`, `else`, `for …` und `while …` steht in Python ein Doppelpunkt.'],
        ['python', /NameError: name '([^']+)' is not defined/,
         '`%s` kennt Python an dieser Stelle nicht. Ist es anders geschrieben, oder bekommt es erst später einen Wert?'],
        ['python', /can only concatenate str \(not "int"\) to str|unsupported operand type\(s\) for \+: 'int' and 'str'/,
         'Text und Zahl lassen sich nicht mit `+` aneinanderhängen. `str(zahl)` macht aus der Zahl einen Text.'],
        ['python', /'NoneType' object/,
         'Hier ist ein Wert `None`, also nichts. Gibt eine Funktion vielleicht nichts mit `return` zurück?'],
        ['ruby', /undefined local variable or method '([^']+)'|undefined local variable or method `([^']+)'/,
         '`%s` kennt Ruby an dieser Stelle nicht. Ist es anders geschrieben, oder bekommt es erst später einen Wert?'],
        ['ruby', /undefined method '([^']+)' for nil|undefined method `([^']+)' for nil/,
         'Hier ist ein Wert `nil`, also nichts, und der kann kein `%s`. Gibt eine Funktion vielleicht nichts zurück?'],
        ['ruby', /unexpected end-of-input|expected an `end`|expected a matching `end`|unexpected 'end'|unexpected `end'/,
         'Die `end` passen nicht: Jedes `def`, `if`, `while` und `do` braucht genau ein `end`.'],
        ['javascript', /ReferenceError: (\S+) is not defined/,
         '`%s` kennt JavaScript an dieser Stelle nicht. Ist es anders geschrieben, oder fehlt ein `let` oder `const`?'],
        ['javascript', /Cannot read properties of (?:undefined|null) \(reading '([^']+)'\)/,
         'Hier ist ein Wert `undefined` oder `null`, also nichts, und der hat kein `%s`. Gibt eine Funktion vielleicht nichts mit `return` zurück?'],
        ['javascript', /Unexpected end of input|missing \) after argument list|Unexpected token '\}'/,
         'Die Klammern passen nicht: Jede `{` und jede `(` braucht ihr Gegenstück.'],
        ['dart', /Expected ';' after this/,
         'In Dart endet jede Anweisung mit einem Semikolon `;`.'],
        ['dart', /A value of type '[^']+' can't be returned from (?:a function|the function|the method)/,
         'Was die Funktion zurückgibt, passt nicht zu dem Typ, der vor ihrem Namen steht.'],
        ['dart', /The body might complete normally, causing 'null' to be returned/,
         'Deine Funktion kommt ans Ende, ohne etwas mit `return` zurückzugeben: Wo fehlt ein `return`?'],
        ['java', /unreported exception InterruptedException/,
         'Das ist das `wait()`, das in Java jedes Objekt hat, kein Befehl für Pip: Pip bleibt mit `pause()` einen Schritt stehen.'],
        ['java', /not a statement/,
         'Diese Zeile tut nichts: Ein Name allein ist noch kein Befehl. Ein Aufruf braucht seine Klammern, `move();`, und ein Ergebnis muss irgendwohin, etwa in eine Variable.'],
        ['java', /';' expected/,
         'In Java endet jede Anweisung mit einem Semikolon `;`.'],
        ['java', /missing return statement/,
         'Deine Methode kommt ans Ende, ohne etwas mit `return` zurückzugeben: Wo fehlt ein `return`?'],
        ['java', /incompatible types: (\S+) cannot be converted/,
         'Ein Wert vom Typ `%s` steht dort, wo Java einen anderen Typ erwartet. Passt der Typ vor dem Namen der Methode oder der Variablen?'],
        ['java', /StackOverflowError/,
         'Deine Methode ruft sich so oft selbst auf, dass der Speicher dafür ausgeht. Erreicht sie den kleinsten Fall, in dem sie ohne Aufruf fertig ist?'],
        ['dart', /Stack Overflow/,
         'Deine Funktion ruft sich so oft selbst auf, dass der Speicher dafür ausgeht. Erreicht sie den kleinsten Fall, in dem sie ohne Aufruf fertig ist?'],
        ['ruby', /String can't be coerced into Integer|no implicit conversion of Integer into String/,
         'Text und Zahl lassen sich nicht mit `+` aneinanderhängen. `zahl.to_s` macht aus der Zahl einen Text, oder du schreibst `"…#{zahl}…"`.'],
    ].freeze

    def self.explain(text, language)
        EXPLAINED.each do |lang, pattern, explanation|
            next unless lang == language && (m = text.to_s.match(pattern))

            name = m.captures.compact.first
            return name ? format(explanation.gsub('%', '%%').sub('%%s', '%s'), name) : explanation
        end
        nil
    end

    # »`Integer#gcd` gibt es in dieser Aufgabe nicht«, with the call or the
    # line that used it.
    def self.forbidden_message(message)
        name = message[/#{FORBIDDEN}: ([^`\s]+)/o, 1]
        where = message[/\A`[^`]+`/] || 'Dein Code'
        line = message[/\(Zeile \d+\)/]
        "#{where}#{line ? " #{line}" : ''} benutzt `#{name}`. Das gibt es in dieser Aufgabe nicht: Du schreibst es gerade selbst."
    end

    # The say of the first wrong solution whose results are the person's.
    def self.same_as(mine, others, alternatives)
        own = CallCheck.parse(mine)
        return nil unless own && own['cases']

        key = ->(data) { data['cases'].map { |c| [c['ok'], c['got'], c['error'] ? c['error'].split(':').first : nil] } }
        alternatives.each_with_index do |f, i|
            data = CallCheck.parse(others[i].to_s)
            return f['say'] if data && data['cases'] && key.call(data) == key.call(own)
        end
        nil
    end

    # The stored answer with code for language: {"python": "…", "ruby": "…"}
    # as JSON. previous: what was stored before (JSON, or older plain text).
    def self.stored_answer(previous, language, code)
        codes = begin
            value = JSON.parse(previous.to_s)
            value.is_a?(Hash) ? value.select { |k, v| CHECKS.key?(k) && v.is_a?(String) } : {}
        rescue JSON::ParserError
            {}
        end
        JSON.generate(codes.merge(language => code.to_s[0, MAX_CODE]))
    end

    # Lines that only define (a function, a constant, an import) and so
    # print nothing; anything else at the start of a line is code of the
    # program's own (puts greet('Ada')), which the one button runs before
    # the check (/api/kata/check), so that its output shows.
    DEFINING = {
        'ruby' => /\A(?:def |end\b|class |module |require|private\b|[A-Z][A-Z0-9_]*\s*=|[\]})])/,
        'python' => /\A(?:def |class |import |from |@|[A-Z][A-Z0-9_]*\s*=|[\]})])/,
        'javascript' => /\A(?:(?:async\s+)?function\b|(?:const|let|var)\s|class |[\]})]|\/\/|\/\*|\*|['"]use strict)/,
    }.freeze

    def self.top_level?(code, language)
        return KataCompiled.has_main?(code) if KataCompiled::LANGUAGES.include?(language)

        defining = DEFINING[language] or return true
        code.to_s.each_line.any? do |line|
            next false if line =~ /\A\s/ || line.strip.empty? || line.start_with?('#')

            line !~ defining
        end
    end

    # »Ausführen«: the program once, with what it prints. { status:, output:,
    # message: } with status :ran, :error (it ended with an error),
    # :timed_out, :too_long or :unavailable.
    def self.run_program(language, code, run)
        return { :status => :invalid, :output => '', :message => EMPTY } if code.to_s.strip.empty?

        argv = KataCompiled::LANGUAGES.include?(language) ? KataCompiled.run_argv(language) : PROGRAMS.fetch(language)
        result = run.call(:files => { FILES.fetch(language) => code.to_s }, :argv => argv, :timeout => RUN_SECONDS)
        return { :status => :unavailable, :output => '', :message => BUSY } if result.nil?

        output = result.stdout + result.stderr
        if result.timed_out
            { :status => :timed_out, :output => output, :message => "Dein Programm lief länger als #{RUN_SECONDS} Sekunden und wurde abgebrochen. Steckt irgendwo eine Endlosschleife?" }
        elsif result.too_long
            { :status => :too_long, :output => output, :message => 'Dein Programm hat so viel ausgegeben, dass es abgebrochen wurde.' }
        else
            status = result.status == 0 ? :ran : :error
            { :status => status, :output => output, :message => status == :error ? explain(output, language) : nil }
        end
    end
end
