require 'shellwords'

# Check type unit_tests (docs/aufgaben.md, »Testen«): the person's tests, a
# file in their Workspace written for the language's own test framework
# (minitest, pytest, node --test), run against the right version of the
# file they test, where every test must pass, and against broken versions,
# each of which at least one test must catch; the feedback names the first
# that slips through. Like TestsCheck for the Katas, with files and real
# frameworks.
#
#   check:
#     type: unit_tests
#     language: ruby                     # ruby, python, javascript; per language as a variant
#     file: notenrechner/test_noten.rb   # the person's tests
#     correct: loesung/noten.rb          # the right version, in the activity's folder;
#                                        # each version is written next to the tests
#                                        # under this name (noten.rb)
#     mutants:                           # broken versions: one change each in correct
#       - replace: ['between?(0, 15)', 'between?(0, 16)']
#         say: Eine Fassung, die 16 Punkte annimmt, besteht deine Tests.
#     timeout: 30                        # for all versions together; default 5 + 3 a version
#
# TaskSpec.load_file reads correct and makes the mutants (resolve!): a
# replace that is not in correct exactly once is an error in tasks.yaml.
#
# A shell script, run in the person's Workspace as the guest user that can
# read /workspace but not change it, writes each version into a folder of
# its own in /tmp with a copy of the tests and starts the framework there,
# with a time limit. A version passes when the framework ends with 0. The
# person's own file of the same name is never run.
module UnitTestsCheck
    LANGUAGES = %w[ruby python javascript].freeze
    EXTENSIONS = { 'ruby' => '.rb', 'python' => '.py', 'javascript' => '.js' }.freeze
    # the framework, started in the version's folder with the tests' file name
    COMMANDS = {
        'ruby' => 'ruby -W0',
        'python' => 'python3 -m pytest -q -p no:cacheprovider',
        'javascript' => 'node --test --test-reporter=tap',
    }.freeze
    NAMES = { 'ruby' => 'minitest', 'python' => 'pytest', 'javascript' => 'node --test' }.freeze
    MARK = '__HS_UNIT__'
    VERSION_SECONDS = 10
    MAX_MUTANTS = 10
    MAX_SHOWN = 40

    def self.spec_errors(check)
        errors = []
        language = check['language']
        errors << "language must be one of #{LANGUAGES.join(', ')}" unless LANGUAGES.include?(language)
        file = check['file']
        if file.is_a?(String) && LANGUAGES.include?(language) && !file.end_with?(EXTENSIONS[language])
            errors << "file must end in #{EXTENSIONS[language]}"
        end
        correct = check['correct']
        unless correct.is_a?(String) && correct.match?(%r{\A[a-z0-9_-]+(/[a-z0-9_.-]+)*\z}) && !correct.include?('..')
            errors << 'correct must be a file in the activity folder (loesung/noten.rb)'
        end
        mutants = check['mutants']
        if mutants.is_a?(Array) && mutants.size.between?(1, MAX_MUTANTS)
            mutants.each_with_index do |m, i|
                unless m.is_a?(Hash) && (m.keys - %w[replace say]).empty?
                    errors << "mutants[#{i}] has replace and say"
                    next
                end
                pair = m['replace']
                unless pair.is_a?(Array) && pair.size == 2 && pair.all? { |s| s.is_a?(String) } && !pair[0].empty? && pair[0] != pair[1]
                    errors << "mutants[#{i}].replace must be two different texts, [what, by what]"
                end
                errors << "mutants[#{i}].say must be text" unless m['say'].is_a?(String) && !m['say'].strip.empty?
            end
        else
            errors << "mutants must be a list of 1 to #{MAX_MUTANTS}"
        end
        if check.key?('timeout') && !(check['timeout'].is_a?(Integer) && check['timeout'].between?(1, 60))
            errors << 'timeout must be 1 to 60 seconds'
        end
        errors
    end

    # Reads correct (in dir, the activity's folder) into correct_code and
    # makes each mutant's code, for every unit_tests check of the tasks,
    # also in their language variants. Returns the errors.
    def self.resolve!(data, dir)
        errors = []
        (data['tasks'] || []).each do |task|
            next unless task.is_a?(Hash)

            [task, *task.values.select { |v| v.is_a?(Hash) && v['check'].is_a?(Hash) }].each do |part|
                check = part['check']
                next unless check.is_a?(Hash) && check['type'] == 'unit_tests'

                path = File.join(dir, check['correct'].to_s)
                unless File.file?(path)
                    errors << "task #{task['id']}: correct: #{check['correct']} is not there"
                    next
                end
                code = File.read(path, encoding: 'UTF-8')
                check['correct_code'] = code
                (check['mutants'] || []).each_with_index do |m, i|
                    from, to = m['replace']
                    count = code.scan(from).size
                    if count == 1
                        m['code'] = code.sub(from) { to }
                    else
                        errors << "task #{task['id']}: mutants[#{i}]: »#{from}« is #{count} times in #{check['correct']}, not once"
                    end
                end
            end
        end
        errors
    end

    # for all versions together, unless the check says: a framework starts
    # in about a second, a few more for the Workspace
    def self.timeout(check)
        [5 + 3 * (1 + (check['mutants'] || []).size), 60].min
    end

    def self.module_name(check)
        File.basename(check['correct'].to_s)
    end

    # The command that prints the result, run in /workspace as the guest.
    def self.command_for_check(check)
        versions = [check['correct_code'].to_s] + (check['mutants'] || []).map { |m| m['code'].to_s }
        file = check['file']
        tests = File.basename(file)
        module_file = Shellwords.escape(module_name(check))
        run = COMMANDS.fetch(check['language'])
        script = +<<~SH
            f="$PWD/"#{Shellwords.escape(file)}
            if [ ! -f "$f" ]; then echo '#{MARK} missing'; exit 0; fi
            t=$(mktemp -d) || exit 1
            trap 'rm -rf "$t"' EXIT
            export PYTHONDONTWRITEBYTECODE=1
        SH
        versions.each_with_index do |code, i|
            marker = "__HS_VERSION_#{i}__"
            script << "mkdir \"$t/#{i}\"\ncat > \"$t/#{i}/\"#{module_file} <<'#{marker}'\n#{code.chomp}\n#{marker}\n"
            script << "cp \"$f\" \"$t/#{i}/\"#{Shellwords.escape(tests)}\n"
        end
        versions.each_index do |i|
            script << "(cd \"$t/#{i}\" && timeout #{VERSION_SECONDS} #{run} #{Shellwords.escape(tests)}) > \"$t/#{i}.out\" 2>&1\n"
            script << "echo \"#{MARK} #{i} $?\"\n"
        end
        # what the right version said, for the feedback; the folder in /tmp
        # is no business of the person
        script << "echo '#{MARK} output'\nhead -c 20000 \"$t/0.out\" | sed \"s|$t/0/||g\"\n"
        script
    end

    # How many tests the framework ran, from its summary; nil if it said
    # nothing (it did not get that far).
    def self.count(language, output)
        case language
        when 'ruby' then output[/(\d+) runs?, \d+ assertions?/, 1]&.to_i
        when 'python'
            return 0 if output.include?('no tests ran')

            numbers = %w[passed failed error errors].map { |w| output[/(\d+) #{w}\b/, 1].to_i }
            numbers.sum.positive? ? numbers.sum : nil
        when 'javascript' then output[/^# tests (\d+)/, 1]&.to_i
        end
    end

    # [status, message, read] for the output of the command.
    def self.evaluate(task, check, output, timed_out: false)
        output = output.to_s
        file = check['file']
        tests = "`#{File.basename(file)}`"
        subject = "`#{module_name(check)}`"
        if output.include?("#{MARK} missing")
            return [:wrong, "`#{file}` gibt es nicht. Lege die Datei mit deinen Tests im Ordner `#{File.dirname(file)}` an.", nil]
        end

        statuses = output.scan(/^#{MARK} (\d+) (\d+)$/).to_h { |i, s| [i.to_i, s.to_i] }
        shown = output.split("#{MARK} output\n", 2)[1].to_s
        lines = shown.lines
        read = lines.empty? ? nil : "```\n#{lines.last(MAX_SHOWN).join.gsub('```', "'''").chomp}\n```"
        if timed_out || statuses.empty?
            return [:wrong, "Deine Tests liefen zu lange und wurden abgebrochen. Steckt irgendwo eine Endlosschleife?", read] if timed_out

            return [:wrong, 'Deine Tests ließen sich nicht ausführen.', read]
        end

        right = statuses[0]
        count = count(check['language'], shown)
        if right == 124
            return [:wrong, "Mit dem richtigen #{subject} laufen deine Tests länger als #{VERSION_SECONDS} Sekunden. Steckt irgendwo eine Endlosschleife?", read]
        end
        if count.to_i.zero? && (right.zero? || (check['language'] == 'python' && right == 5))
            return [:wrong, "In #{tests} läuft noch kein Test. #{first_test_hint(check['language'])}", read]
        end
        unless right.zero?
            return [:wrong, "Mit dem richtigen #{subject} bestehen nicht alle deine Tests. Unter »Details« steht, was #{NAMES[check['language']]} meldet: Welcher Test schlägt fehl, und stimmt dort der erwartete Wert?", read]
        end

        (check['mutants'] || []).each_with_index do |m, i|
            status = statuses[i + 1]
            next if status.nil? || !status.zero?

            return [:wrong, m['say'], read]
        end

        mutants = (check['mutants'] || []).size
        tested, find = count == 1 ? ['Dein Test besteht', 'findet'] : ["Deine #{count} Tests bestehen", 'finden']
        found = mutants == 1 ? 'den Fehler in der fehlerhaften Fassung' : "die Fehler in allen #{mutants} fehlerhaften Fassungen"
        [:correct, "#{tested} mit dem richtigen #{subject} und #{find} #{found}.", read]
    end

    def self.first_test_hint(language)
        case language
        when 'ruby' then 'Ein Test ist eine Methode, deren Name mit `test_` beginnt, in einer Klasse, die von `Minitest::Test` erbt.'
        when 'python' then 'Ein Test ist eine Funktion, deren Name mit `test_` beginnt, in einer Datei, deren Name mit `test_` beginnt.'
        else 'Ein Test ist ein Aufruf `test(\'Name\', () => { … })`.'
        end
    end
end
