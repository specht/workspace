require 'digest'
require_relative 'call_check'
require_relative 'python_check'
require_relative 'ruby_check'

# Check type function (docs/aufgaben.md, docs/plan-katas.md): a Kata. The
# code is typed into the card, not written in the Workspace; it runs in a
# container of its own (KataRunner, through KataQueue), with the same
# harness as the checks python and ruby: only the definitions are loaded,
# each call of the task is evaluated and compared, and the result reads like
# theirs, with »dein Code« in place of the file.
#
#   check:
#     type: function
#     language: ruby              # ruby or python; per language as a variant
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
#
#   feedback:
#     - when: { same_as: 'def gcd(a, b) = [a, b].min' }
#       say: Die kleinere Zahl ist nur manchmal ein Teiler der größeren.
#
# forbid: in Ruby Class#method or Class.method, made to fail when the
# person's code calls it (the harness and the reference may still use it);
# in Python a built-in (sorted), a module's function (math.gcd), a
# built-in type (str) or a method of one (list.sort); types and their
# methods cannot be switched off and are looked for in the code instead. random: the values come from a seed per
# person and task version, so a second check has the same cases and a table
# of answers does not pass; the message names the value the reference gave.
# same_as: a typical wrong solution, written by the author and never shown,
# runs on the same cases; if the person's results are its results, its say
# replaces the message.
module KataCheck
    CHECKS = { 'python' => PythonCheck, 'ruby' => RubyCheck }.freeze
    FILES = { 'python' => 'kata.py', 'ruby' => 'kata.rb' }.freeze
    HARNESS_FILES = { 'python' => 'harness.py', 'ruby' => 'harness.rb' }.freeze
    # What runs on »Ausführen«: the program as it is, top-level code included.
    PROGRAMS = { 'python' => %w[python3 -I -B kata.py], 'ruby' => %w[ruby -W0 kata.rb] }.freeze
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
        if check.key?('forbid')
            pattern = language == 'python' ? PYTHON_FORBID : RUBY_FORBID
            unless check['forbid'].is_a?(Array) && !check['forbid'].empty? && check['forbid'].all? { |f| f.is_a?(String) && f =~ pattern }
                errors << (language == 'python' ? 'forbid must list names like sorted, math.gcd or list.sort' :
                    'forbid must list methods like Integer#gcd or Math.sqrt')
            end
        end
        errors.concat(random_errors(check['random'])) if check.key?('random')
        errors << 'slow must be text' if check.key?('slow') && !(check['slow'].is_a?(String) && !check['slow'].strip.empty?)
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
        case value
        when true then language == 'python' ? 'True' : 'true'
        when false then language == 'python' ? 'False' : 'false'
        when nil then language == 'python' ? 'None' : 'nil'
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
            expect = if language == 'python'
                "(lambda #{names.join(', ')}: #{random['reference']})(#{arguments})"
            else
                "->(#{names.join(', ')}) { #{random['reference']} }.call(#{arguments})"
            end
            { 'call' => call, 'expect' => expect, 'say' => nil, 'raises' => false, 'show' => true }
        end
    end

    # What runs before the harness when check forbids built-ins.
    def self.prelude(check)
        list = JSON.generate(check['forbid'] || [])
        return '' if list == '[]'

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
        language = CHECKS.fetch(language_name)
        file = FILES.fetch(language_name)
        harness = HARNESS_FILES.fetch(language_name)
        cases = language.cases(check) + random_cases(check, seed)
        alternatives = (task['feedback'] || []).select { |f| f.dig('when', 'same_as') }
        files = { file => code.to_s, harness => prelude(check) + language::HARNESS }
        alternatives.each_with_index { |f, i| files["alt#{i + 1}#{File.extname(file)}"] = f['when']['same_as'] }
        payload = language.argv(file, cases).last
        interpreter = language.argv(file, cases).first(language == PythonCheck ? 3 : 2)
        targets = [file] + alternatives.each_index.map { |i| "alt#{i + 1}#{File.extname(file)}" }
        argv = if targets.size == 1
            interpreter + [harness, file, payload]
        else
            ['sh', '-c', "for f in #{targets.join(' ')}; do #{interpreter.join(' ')} #{harness} \"$f\" \"$0\"; done", payload]
        end
        seconds = check['timeout'] || 10
        timeout = [seconds * targets.size, MAX_SECONDS].min
        result = run.call(:files => files, :argv => argv, :timeout => timeout)
        return [:unavailable, BUSY, nil] if result.nil?

        outputs = result.too_long ? [] : result.stdout.split(/^(?=#{CallCheck::RESULT_MARK})/)
                                                  .select { |part| part.start_with?(CallCheck::RESULT_MARK) }
        mine = outputs.first.to_s
        status, message, read = language.evaluate(task, check.merge('file' => nil), mine,
            :timed_out => result.timed_out && outputs.empty?)
        message = forbidden_message(message) if message.to_s.include?("#{FORBIDDEN}: ")
        message = check['slow'] if check['slow'] && result.timed_out && outputs.empty?
        if status == :wrong && !alternatives.empty?
            say = same_as(mine, outputs.drop(1), alternatives)
            message = say if say
        end
        explanation = status == :wrong ? explain(message, language_name) : nil
        message = "#{message}\n\n#{explanation}" if explanation
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

    # »Ausführen«: the program once, with what it prints. { status:, output:,
    # message: } with status :ran, :error (it ended with an error),
    # :timed_out, :too_long or :unavailable.
    def self.run_program(language, code, run)
        return { :status => :invalid, :output => '', :message => EMPTY } if code.to_s.strip.empty?

        result = run.call(:files => { FILES.fetch(language) => code.to_s }, :argv => PROGRAMS.fetch(language), :timeout => RUN_SECONDS)
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
