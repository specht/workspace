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
#     language: python            # ruby or python; per language as a variant
#     cases:
#       - gcd(60, 48): 12
#       - call: gcd(7, 7)
#         expect: 7
#         say: Der größte Teiler einer Zahl mit sich selbst ist sie selbst.
#     timeout: 10                 # seconds for all cases together
module KataCheck
    CHECKS = { 'python' => PythonCheck, 'ruby' => RubyCheck }.freeze
    FILES = { 'python' => 'kata.py', 'ruby' => 'kata.rb' }.freeze
    # What runs on »Ausführen«: the program as it is, top-level code included.
    PROGRAMS = { 'python' => %w[python3 -I -B kata.py], 'ruby' => %w[ruby -W0 kata.rb] }.freeze
    MAX_CODE = 4000
    # What is stored as the task's answer: the code per language, as JSON,
    # so each language keeps its own (TaskStore, max:).
    MAX_STORED = (CHECKS.size + 1) * (MAX_CODE + 64)
    RUN_SECONDS = 10

    BUSY = 'Gerade prüfen sehr viele auf einmal. Versuch es gleich noch einmal.'
    EMPTY = 'Du hast noch keinen Code geschrieben.'

    def self.spec_errors(check)
        errors = []
        errors << "language must be #{CHECKS.keys.join(' or ')}" unless CHECKS.key?(check['language'])
        errors.concat(CallCheck.spec_errors(check, ''))
        errors
    end

    # [status, message, read]. run: lambda(files:, argv:, timeout:) ->
    # KataRunner::Result, or nil when no run could start (the queue waited
    # too long).
    def self.check(task, check, code, run)
        return [:invalid, EMPTY, nil] if code.to_s.strip.empty?

        language = CHECKS.fetch(check['language'])
        file = FILES.fetch(check['language'])
        seconds = check['timeout'] || 10
        result = run.call(:files => { file => code.to_s }, :argv => language.argv(file, language.cases(check)), :timeout => seconds)
        return [:unavailable, BUSY, nil] if result.nil?

        output = result.too_long ? '' : result.stdout
        language.evaluate(task, check.merge('file' => nil), output, :timed_out => result.timed_out)
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
            { :status => result.status == 0 ? :ran : :error, :output => output, :message => nil }
        end
    end
end
