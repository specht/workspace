require 'json'
require_relative 'call_check'

# A Kata about logic circuits (check type function with gates:,
# docs/aufgaben.md): the person's functions may combine their inputs only
# through the gates the check names, as wires in a circuit go only through
# gates.
#
#   check:
#     type: function
#     language: ruby               # ruby or python
#     gates: [nand]                # the gates that may combine signals
#     cases:
#       - and_(1, 1): 1
#
# The cases run twice. First as they are, with the person's own gates from
# the starter (def nand(a, b) …), so that the usual messages come. Then
# every case once more, wired (wired below): each 0 and 1 in the arguments,
# also inside lists, becomes a signal, an object that only the check's own
# gates can read, and the gates named in gates: are the check's for that
# call. A signal cannot be added, compared or turned into a truth value
# (KataSignalError); what comes back is read as 0 and 1 again. A function
# that computes with its inputs itself (a & b, a == 1, [0, 0, 0, 1][2 * a + b],
# a table of answers) passes the first run and fails the second; one built
# only from the gates gives the same in both. A constant 0 or 1 may go into
# a gate (a carry that starts at 0) and come back as it is.
#
# Only for calls of one function, name(arguments), in Ruby and Python;
# anything that is no 0 or 1 (a text like 'add' that chooses an operation)
# is passed on as it is.
module GateCheck
    # the gates the check knows, by their names in Ruby and Python, which
    # cannot use and, or and not as names
    TRUTH = {
        'not_' => '1 - a',
        'and_' => 'a & b',
        'or_' => 'a | b',
        'xor_' => 'a ^ b',
        'nand' => '1 - (a & b)',
        'nor' => '1 - (a | b)',
    }.freeze
    LANGUAGES = %w[ruby python].freeze
    CALL = /\A\s*([a-z_]\w*)\s*\((.*)\)\s*\z/m
    ERROR = 'KataSignalError'
    # what a signal says when it is used like a number (in the person's
    # code: the line of the error is theirs)
    REFUSED = 'Mit einem Signal rechnet nur ein Gatter'

    def self.spec_errors(check)
        errors = []
        gates = check['gates']
        unless gates.is_a?(Array) && !gates.empty? && gates.all? { |g| TRUTH.key?(g) } && gates.uniq.size == gates.size
            errors << "gates must list gates from #{TRUTH.keys.join(', ')}"
        end
        errors << 'gates work in ruby and python only' unless LANGUAGES.include?(check['language'])
        (check['cases'] || []).each_with_index do |item, i|
            call = item.is_a?(Hash) && item.key?('call') ? item['call'] : (item.is_a?(Hash) && item.size == 1 ? item.keys.first : nil)
            errors << "cases[#{i}]: with gates, a case calls one function: name(arguments)" unless call.is_a?(String) && call =~ CALL
            errors << "cases[#{i}]: with gates, raises does not work" if item.is_a?(Hash) && item['raises']
        end
        random = check['random']
        if random.is_a?(Hash) && !(random['call'].is_a?(String) && random['call'] =~ CALL)
            errors << 'random.call: with gates, the call is one function: name(arguments)'
        end
        errors
    end

    # The names of the gates for a message: »`nand`«, »`and_`, `or_` und `xor_`«.
    def self.named(gates)
        names = gates.map { |g| "`#{g}`" }
        names.size <= 1 ? names.join : "#{names[0..-2].join(', ')} und #{names[-1]}"
    end

    # The cases once more, wired: [{ 'call', 'expect', 'show' }] for the
    # harness. call: the case's call, name(arguments), turned into
    # kata_wired(:name, arguments) (Ruby) or _k_wired('name', arguments).
    def self.wired(cases, language)
        cases.map do |c|
            name, arguments = c['call'].match(CALL).captures
            head = language == 'python' ? "_k_wired('#{name}'" : "kata_wired(:#{name}"
            call = arguments.strip.empty? ? "#{head})" : "#{head}, #{arguments})"
            { 'call' => call, 'expect' => c['expect'], 'say' => nil, 'raises' => false, 'show' => c['show'] }.compact
        end
    end

    # What runs before the harness: the signal, the check's gates and the
    # function that makes a call wired.
    def self.prelude(language, gates)
        names = JSON.generate(gates)
        if language == 'python'
            truth = gates.map { |g| "#{g.inspect}: (lambda a, b=0: #{TRUTH[g]})" }.join(', ')
            arity = gates.to_h { |g| [g, g == 'not_' ? 1 : 2] }
            <<~PY
                import json as _k_gate_json, sys as _k_gate_sys
                import builtins as _k_gate_builtins
                class #{ERROR}(Exception):
                    pass
                class _KataSignal:
                    """A wire with 0 or 1 on it, which only a gate of the check reads."""
                    __slots__ = ('_k_bit',)
                    def __init__(self, bit):
                        object.__setattr__(self, '_k_bit', bit)
                    def __repr__(self):
                        return 'Signal'
                    def _k_refuse(self, *args, **kwargs):
                        raise #{ERROR}(#{REFUSED.inspect})
                    __hash__ = object.__hash__
                for _k_name in ('__eq__ __ne__ __lt__ __le__ __gt__ __ge__ __bool__ __int__ __index__ __float__ '
                                '__add__ __radd__ __sub__ __rsub__ __mul__ __rmul__ __truediv__ __rtruediv__ '
                                '__floordiv__ __rfloordiv__ __mod__ __rmod__ __pow__ __rpow__ __and__ __rand__ '
                                '__or__ __ror__ __xor__ __rxor__ __invert__ __neg__ __pos__ __abs__ '
                                '__lshift__ __rlshift__ __rshift__ __rrshift__').split():
                    setattr(_KataSignal, _k_name, _KataSignal._k_refuse)
                _K_TRUTH = {#{truth}}
                _K_ARITY = _k_gate_json.loads(#{JSON.generate(arity).inspect})
                _K_WIRED = _k_gate_json.loads(#{names.inspect})
                _K_MISSING = object()
                def _k_bit(x):
                    if type(x) is _KataSignal:
                        return object.__getattribute__(x, '_k_bit')
                    if type(x) is int and x in (0, 1):
                        return x
                    raise #{ERROR}('Ein Gatter bekommt 0, 1 oder ein Signal, nicht ' + repr(x)[:40])
                def _k_wrap(value):
                    if type(value) in (list, tuple):
                        return type(value)(_k_wrap(v) for v in value)
                    if type(value) is int and value in (0, 1):
                        return _KataSignal(value)
                    return value
                def _k_unwrap(value):
                    if type(value) in (list, tuple):
                        return type(value)(_k_unwrap(v) for v in value)
                    if type(value) is _KataSignal:
                        return object.__getattribute__(value, '_k_bit')
                    return value
                def _k_gate(name):
                    truth = _K_TRUTH[name]
                    def gate(*inputs):
                        if len(inputs) != _K_ARITY[name]:
                            raise TypeError(name + '() takes ' + str(_K_ARITY[name]) + ' arguments (' + str(len(inputs)) + ' given)')
                        return _KataSignal(truth(*[_k_bit(x) for x in inputs]))
                    gate.__name__ = name
                    return gate
                def _k_wired(name, *arguments):
                    scope = _k_gate_sys._getframe(1).f_globals
                    saved = {g: scope.get(g, _K_MISSING) for g in _K_WIRED}
                    for g in _K_WIRED:
                        scope[g] = _k_gate(g)
                    try:
                        return _k_unwrap(scope[name](*_k_wrap(list(arguments))))
                    finally:
                        for g, f in saved.items():
                            if f is _K_MISSING:
                                scope.pop(g, None)
                            else:
                                scope[g] = f
                _k_gate_builtins._k_wired = _k_wired
            PY
        else
            truth = gates.map { |g| "#{g.inspect} => ->(#{g == 'not_' ? 'a' : 'a, b'}) { #{TRUTH[g]} }" }.join(', ')
            <<~RUBY
                require 'json'
                class #{ERROR} < StandardError; end
                # A wire with 0 or 1 on it, which only a gate of the check reads.
                class KataSignal
                    def initialize(bit)
                        @bit = bit
                    end

                    def inspect = 'Signal'
                    def to_s = 'Signal'

                    %i[== != ! < > <= >= <=> === + - * / % ** & | ^ ~ << >> -@ +@ coerce to_i to_int to_f
                       zero? nonzero? even? odd? succ pred abs].each do |name|
                        define_method(name) { |*| raise #{ERROR}, #{REFUSED.inspect} }
                    end
                end
                KATA_TRUTH = { #{truth} }.freeze
                KATA_WIRED = JSON.parse(#{names.inspect}).freeze

                def kata_bit(x)
                    return x.instance_variable_get(:@bit) if KataSignal === x
                    return x if Integer === x && (x.equal?(0) || x.equal?(1))

                    raise #{ERROR}, "Ein Gatter bekommt 0, 1 oder ein Signal, nicht \#{x.inspect[0, 40]}"
                end

                def kata_wired(name, *arguments)
                    wrap = ->(v) { Array === v ? v.map(&wrap) : (Integer === v && (v.equal?(0) || v.equal?(1)) ? KataSignal.new(v) : v) }
                    unwrap = ->(v) { Array === v ? v.map(&unwrap) : (KataSignal === v ? v.instance_variable_get(:@bit) : v) }
                    saved = KATA_WIRED.to_h { |g| [g, (Object.instance_method(g) rescue nil)] }
                    KATA_WIRED.each do |g|
                        truth = KATA_TRUTH.fetch(g)
                        Object.send(:define_method, g) do |*inputs|
                            raise ArgumentError, "wrong number of arguments (given \#{inputs.size}, expected \#{truth.arity})" unless inputs.size == truth.arity

                            KataSignal.new(truth.call(*inputs.map { |x| kata_bit(x) }))
                        end
                        Object.send(:private, g)
                    end
                    begin
                        unwrap.call(send(name, *wrap.call(arguments)))
                    ensure
                        saved.each do |g, original|
                            if original
                                Object.send(:define_method, g, original)
                                Object.send(:private, g)
                            else
                                Object.send(:remove_method, g)
                            end
                        end
                    end
                end
            RUBY
        end
    end

    # [status, message, read] after the run: output is the harness's line
    # for the person's code, with the plain cases first and their wired
    # copies after them. evaluate: the language's evaluate for an output of
    # the plain cases only.
    def self.evaluate(check, output, plain, evaluate)
        data = CallCheck.parse(output)
        return evaluate.call(output) unless data && data['cases'].is_a?(Array)

        results = data['cases']
        shown = data.merge('cases' => results.first(plain))
        status, message, read = evaluate.call("\n#{CallCheck::RESULT_MARK}#{JSON.generate(shown)}\n")
        return [status, message, read] unless status == :correct

        gates = named(check['gates'])
        wired = results.drop(plain)
        failed = wired.index { |r| !r['ok'] }
        if failed.nil?
            line = "- <i class='bi bi-check-lg'></i> Die Eingänge gehen nur durch #{gates}."
            return [:correct, message, [read, line].compact.join("\n")]
        end

        r = wired[failed]
        call = results[failed]['call']
        outcome = r['error'] ? "bricht es ab#{CallCheck.at_line(r['line'])}: `#{r['error']}`" : "ergibt es `#{r['got']}`"
        line = "- <i class='bi bi-x-lg'></i> Nur mit #{gates}: `#{call}` → #{r['error'] ? "bricht ab: `#{r['error']}`" : "`#{r['got']}`"}"
        message = "Mit 0 und 1 stimmt `#{call}`. Kommen die Eingänge aber als Signale, die nur #{gates} verknüpfen kann, #{outcome}. " \
                  "Rechnet oder vergleicht dein Code selbst mit den Eingängen (`+`, `==`, `if`, eine Tabelle)? In dieser Aufgabe geht jedes Signal nur durch #{gates}."
        [:wrong, message, [read, line].compact.join("\n")]
    end
end
