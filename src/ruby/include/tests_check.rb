require 'json'

# Check type tests (docs/aufgaben.md, docs/plan-katas.md, set »Tests
# schreiben«): the student writes tests instead of code, assertions typed
# into the card. They run against the right version of a function, where
# all must pass, and against broken ones, each of which at least one test
# must catch; the feedback names the first that slips through.
#
#   check:
#     type: tests
#     language: ruby                # ruby, python or javascript; per language as a variant
#     function: even                # what the tests call: they must not define it
#     correct: |                    # the right version
#       def even(n) = n % 2 == 0
#     mutants:                      # broken versions, each with what it does
#       - code: "def even(n) = n.odd?"
#         say: Eine Fassung, die »gerade« und »ungerade« vertauscht, besteht deine Tests.
#
# A test is a line assert_equal(expected, actual) (assertEqual in
# JavaScript), defined by the harness: no framework, the same in each
# language. Every version runs in a run of its own (KataRunner), one after
# the other in the same container: the version's file is loaded, then the
# tests from tests.rb (.py, .js), and the harness prints what failed, with
# the line in the tests, and how many assertions ran.
module TestsCheck
    FILES = { 'ruby' => 'tests.rb', 'python' => 'tests.py', 'javascript' => 'tests.js' }.freeze
    COMMANDS = { 'ruby' => 'ruby -W0', 'python' => 'python3 -B', 'javascript' => 'node' }.freeze
    EXTENSIONS = { 'ruby' => 'rb', 'python' => 'py', 'javascript' => 'js' }.freeze
    MARK = '__TESTS__'
    SECONDS = 10
    MAX_MUTANTS = 10

    HARNESS = {
        'ruby' => <<~'RUBY',
            require 'json'
            $k_count = 0
            $k_failures = []
            def assert_equal(expected, actual)
              $k_count += 1
              return true if expected == actual
              line = caller_locations.find { |l| l.path.end_with?('tests.rb') }&.lineno
              $k_failures << [line, expected.inspect, actual.inspect]
              false
            end
            load ARGV[0]
            error = nil
            begin
              load File.expand_path('tests.rb')
            rescue Exception => e
              line = e.backtrace.to_a.map { |l| l[/tests\.rb:(\d+)/, 1] }.compact.first
              error = [line && line.to_i, "#{e.class}: #{e.message}"[0, 300]]
            end
            puts "__TESTS__" + JSON.generate({ 'count' => $k_count, 'failures' => $k_failures.first(5), 'error' => error })
        RUBY
        'python' => <<~'PYTHON',
            import json, sys, traceback, inspect
            _k = {'count': 0, 'failures': []}
            def _same(a, b):
                # True is not 1 here, a number is its number (2 == 2.0)
                if isinstance(a, bool) or isinstance(b, bool):
                    return type(a) is type(b) and a == b
                return a == b
            def assert_equal(expected, actual):
                _k['count'] += 1
                if _same(expected, actual):
                    return True
                line = next((f.lineno for f in inspect.stack() if f.filename == 'tests.py'), None)
                _k['failures'].append([line, repr(expected), repr(actual)])
                return False
            scope = {'assert_equal': assert_equal, '__name__': 'kata'}
            exec(compile(open(sys.argv[1], encoding='utf-8').read(), sys.argv[1], 'exec'), scope)
            error = None
            try:
                exec(compile(open('tests.py', encoding='utf-8').read(), 'tests.py', 'exec'), scope)
            except BaseException as e:
                lines = [t.lineno for t in traceback.extract_tb(e.__traceback__) if t.filename == 'tests.py']
                error = [lines[-1] if lines else None, (type(e).__name__ + ': ' + str(e))[:300]]
            print('__TESTS__' + json.dumps({'count': _k['count'], 'failures': _k['failures'][:5], 'error': error}))
        PYTHON
        'javascript' => <<~'JS',
            const fs = require('fs');
            const util = require('util');
            const vm = require('vm');
            const k = { count: 0, failures: [] };
            globalThis.assertEqual = function (expected, actual) {
              k.count++;
              if (util.isDeepStrictEqual(expected, actual)) return true;
              const match = /tests\.js:(\d+)/.exec(new Error().stack || '');
              k.failures.push([match ? Number(match[1]) : null, util.inspect(expected), util.inspect(actual)]);
              return false;
            };
            vm.runInThisContext(fs.readFileSync(process.argv[2], 'utf8'), { filename: process.argv[2] });
            let error = null;
            try {
              vm.runInThisContext(fs.readFileSync('tests.js', 'utf8'), { filename: 'tests.js' });
            } catch (e) {
              const match = /tests\.js:(\d+)/.exec((e && e.stack) || '');
              error = [match ? Number(match[1]) : null, String(e).slice(0, 300)];
            }
            console.log('__TESTS__' + JSON.stringify({ count: k.count, failures: k.failures.slice(0, 5), error }));
        JS
    }.freeze

    def self.spec_errors(check)
        errors = []
        errors << 'language must be ruby, python or javascript' unless FILES.key?(check['language'])
        errors << 'function must name the function the tests call' unless check['function'].is_a?(String) && check['function'] =~ /\A[A-Za-z_]\w*\z/
        errors << 'correct must be the code of the right version' unless check['correct'].is_a?(String) && !check['correct'].strip.empty?
        mutants = check['mutants']
        if mutants.is_a?(Array) && mutants.size.between?(1, MAX_MUTANTS)
            mutants.each_with_index do |m, i|
                ok = m.is_a?(Hash) && (m.keys - %w[code say]).empty? && m['code'].is_a?(String) && !m['code'].strip.empty? &&
                     m['say'].is_a?(String) && !m['say'].strip.empty?
                errors << "mutants[#{i}] must have code and say" unless ok
            end
        else
            errors << "mutants must list 1 to #{MAX_MUTANTS} broken versions"
        end
        errors
    end

    # Defines the function itself, so that the tests would test that.
    def self.defines?(code, language, name)
        n = Regexp.escape(name)
        case language
        when 'javascript' then code =~ /\bfunction\s+#{n}\b|\b(?:const|let|var)\s+#{n}\s*=/
        else code =~ /^\s*def\s+#{n}\b/
        end
    end

    # [status, message, read] like KataCheck.check.
    def self.check(task, check, code, run)
        return [:invalid, KataCheck::EMPTY, nil] if code.to_s.strip.empty?

        language = check['language']
        name = check['function']
        if defines?(code.to_s, language, name)
            return [:wrong, "Schreib hier nur Tests, nicht die Funktion `#{name}` selbst: Die Prüfung bringt ihre eigenen Fassungen mit, eine richtige und fehlerhafte.", nil]
        end

        ext = EXTENSIONS.fetch(language)
        versions = [check['correct']] + check['mutants'].map { |m| m['code'] }
        files = { FILES.fetch(language) => code.to_s, "harness.#{ext}" => HARNESS.fetch(language) }
        names = versions.each_index.map { |i| "version#{i}.#{ext}" }
        versions.each_with_index { |v, i| files[names[i]] = v }
        argv = ['sh', '-c', "for f in #{names.join(' ')}; do #{COMMANDS.fetch(language)} harness.#{ext} \"$f\"; echo; done"]
        result = run.call(:files => files, :argv => argv, :timeout => [SECONDS * versions.size, KataCheck::MAX_SECONDS].min)
        return [:unavailable, KataCheck::BUSY, nil] if result.nil?

        reports = result.stdout.to_s.lines.filter_map { |l| l.start_with?(MARK) ? (JSON.parse(l[MARK.size..]) rescue nil) : nil }
        if reports.empty?
            return [:wrong, 'Deine Tests liefen zu lange und wurden abgebrochen. Steckt irgendwo eine Endlosschleife?', nil] if result.timed_out

            return [:wrong, "Deine Tests ließen sich nicht ausführen:\n\n```\n#{result.stderr.to_s.strip.lines.last(3).join[0, 500]}\n```", nil]
        end

        right = reports.first
        if right['error']
            line, error = right['error']
            return [:wrong, "Deine Tests brechen mit der richtigen Fassung ab#{line ? " (Zeile #{line})" : ''}: `#{error}`", nil]
        end
        assert = language == 'javascript' ? 'assertEqual' : 'assert_equal'
        return [:wrong, "Du hast noch keinen Test geschrieben: eine Zeile `#{assert}(erwartet, ergebnis)`.", nil] if right['count'].to_i.zero?

        unless right['failures'].empty?
            line, expected, actual = right['failures'].first
            return [:wrong, "#{line ? "Zeile #{line}: " : ''}Die richtige Fassung ergibt `#{actual}`, dein Test erwartet `#{expected}`. Stimmt der erwartete Wert?", nil]
        end

        check['mutants'].each_with_index do |mutant, i|
            report = reports[i + 1]
            return [:unavailable, KataCheck::BUSY, nil] if report.nil? && result.timed_out
            next if report.nil? || report['error'] || !report['failures'].empty?

            return [:wrong, mutant['say'], nil]
        end

        count = right['count'].to_i
        [:correct, "Deine #{count == 1 ? 'Prüfung' : "#{count} Prüfungen"} bestehen mit der richtigen Fassung und erkennen alle #{check['mutants'].size} fehlerhaften.", nil]
    end
end
