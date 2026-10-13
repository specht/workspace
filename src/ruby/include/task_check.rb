require 'openssl'
require 'uri'
require_relative 'task_spec'
require_relative 'file_checks'
require_relative 'git_check'
require_relative 'submission_store'
require_relative 'measure_check'
require_relative 'scene_check'
require_relative 'sql_check'

# Checks a student's answer to a task (docs/aufgaben.md, »Prüfen«).
#
# answer, choice, number and reflection need only the answer. files, html,
# css and references read the student's Workspace folder through
# WorkspaceFiles; git gets the repository state from the caller (read inside
# the container); submit plans a copy that the caller then stores. This
# module never touches Neo4j, Docker or HTTP itself.
module TaskCheck
    ANSWER_TYPES = %w[answer choice number reflection].freeze
    FILE_TYPES = %w[files html css references].freeze
    # Types this version can check.
    RUNS = (ANSWER_TYPES + FILE_TYPES + %w[git submit code measure command scene sql]).freeze
    LANGUAGE_NAMES = { 'ruby' => 'Ruby', 'python' => 'Python', 'javascript' => 'JavaScript' }.freeze

    # status:
    #   :correct, :wrong   – checked; counts as an attempt
    #   :sent              – stored for the teacher (reflection); counts
    #   :invalid           – could not be read (no number, too short, …);
    #                        does not count
    #   :unavailable       – cannot be checked right now (Workspace not
    #                        running, type not available yet); does not count
    # message:  Markdown for the student
    # solution: Markdown, only with :correct and if the task has one
    # read:     Markdown, what a Workspace check actually read
    # warnings: Markdown hints that do not make the task fail
    # problem:  code of what was missing (file checks)
    # plan:     the files a submit check will copy (SubmissionStore::Plan)
    Result = Struct.new(:status, :message, :solution, :read, :warnings, :problem, :plan, keyword_init: true) do
        def counts?
            %i[correct wrong sent].include?(status)
        end
    end

    UNAVAILABLE_VARIABLE = {
        'workspace.ip' => 'Starte zuerst deinen Workspace. Dann kann ich die Adresse mit der deines Workspaces vergleichen.',
        'workspace.live_url' => 'Starte zuerst deinen Workspace. Dann kann ich die Adresse mit der deiner Live-Vorschau vergleichen.',
        'person.codeword' => 'Für diese Aufgabe gibt es noch kein Codewort.',
    }.freeze

    CODEWORDS = %w[
        Ameise Biber Dachs Eule Fuchs Gecko Hase Igel Jaguar Koala Lama Luchs
        Marder Nashorn Otter Panda Qualle Rabe Robbe Specht Tapir Uhu Wal Wiesel
        Yak Zebra Elch Falke Kranich Pinguin Schnecke Taube
    ].freeze

    NO_WORKSPACE = 'Dein Workspace-Ordner ist noch leer. Öffne den Workspace einmal, dann kann ich nachsehen.'
    NOT_RUNNING = 'Starte zuerst deinen Workspace. Den Stand von Git liest der Workspace in deinem laufenden Workspace.'

    # workspace: WorkspaceFiles for the student's folder, or nil if there is none
    # git:       lambda(repo) -> GitCheck.parse result, or nil if the Workspace
    #            is not running
    # code_solved: lambda(codebite) -> languages in which the person has solved
    #              that Code Bites task (from the Code Bites results)
    # runner:      lambda(command, timeout) -> { output:, status:, timed_out: },
    #              runs in the person's container as them; nil if not running
    # sql:         lambda(query) -> result, runs in the person's own MySQL
    #              database as them, read-only (SqlCheck); nil if unreachable
    def self.check(task, answer, variables: {}, workspace: nil, git: nil, code_solved: nil, runner: nil, sql: nil)
        check = task['check']
        type = check['type']
        unless RUNS.include?(type)
            return Result.new(:status => :unavailable,
                :message => 'Diese Aufgabe kann der Workspace noch nicht selbst prüfen. Zeig dein Ergebnis deiner Lehrkraft.')
        end
        return check_code(task, check, code_solved) if type == 'code'
        return check_run(task, check, answer, runner) if %w[measure command].include?(type)
        return check_scene(task, check, workspace, runner) if type == 'scene'
        return check_sql(task, check, answer, sql) if type == 'sql'
        return check_workspace(task, check, answer, workspace, git, runner) unless ANSWER_TYPES.include?(type)

        missing = variables_used(check).reject { |name| variables[name].is_a?(String) && !variables[name].empty? }
        unless missing.empty?
            return Result.new(:status => :unavailable,
                :message => UNAVAILABLE_VARIABLE.fetch(missing.first, 'Diese Aufgabe kann gerade nicht geprüft werden.'))
        end

        answer = answer.to_s
        if answer.strip.empty?
            return Result.new(:status => :invalid,
                :message => type == 'choice' ? 'Wähle zuerst eine Antwort aus.' : 'Du hast noch nichts eingegeben.')
        end

        result = send("check_#{type}", task, check, answer, variables)
        if result.status == :correct
            result.message ||= task['prediction'] ? 'Gut vermutet.' : 'Richtig.'
            result.solution = task['solution']
        elsif result.status == :wrong
            result.message ||= task['prediction'] ?
                'Deine Vermutung war eine andere. Probier es aus und vergleiche.' :
                'Das stimmt noch nicht.'
        end
        result
    end

    # measure and command run in the person's Workspace (MeasureCheck).
    def self.check_run(task, check, answer, runner)
        runner ||= ->(_command, _timeout) { nil }
        status, message, read = if check['type'] == 'measure'
            MeasureCheck.measure(check, answer, runner)
        else
            MeasureCheck.command(task, check, answer, runner)
        end
        Result.new(:status => status, :message => message, :read => read,
            :solution => status == :correct ? task['solution'] : nil)
    end

    # sql: the query runs in the person's own database (SqlCheck).
    def self.check_sql(task, check, answer, sql)
        status, message, read, note, problem = SqlCheck.check(task, check, answer, sql)
        Result.new(:status => status, :message => message, :read => read, :problem => problem,
            :warnings => note ? [note] : nil, :solution => status == :correct ? task['solution'] : nil)
    end

    # code: solved in the Code Bites editor, in one of the allowed languages.
    def self.check_code(task, check, code_solved)
        solved = code_solved&.call(check['codebite'])
        return Result.new(:status => :unavailable, :message => 'Melde dich an, damit ich deine gelösten Aufgaben sehe.') if solved.nil?

        allowed = check['languages'] || TaskSpec::CODE_LANGUAGES
        hits = solved & allowed
        if hits.empty?
            other = solved.empty? ? '' : " In #{solved.map { |l| LANGUAGE_NAMES[l] || l }.join(' und ')} zählt sie hier nicht; erlaubt #{allowed.size == 1 ? 'ist' : 'sind'} #{allowed.map { |l| LANGUAGE_NAMES[l] || l }.join(', ')}."
            return Result.new(:status => :invalid,
                :message => "Noch nicht gelöst. Öffne die Aufgabe im Code-Editor; sobald dein Code alle Tests besteht, ist sie hier abgehakt.#{other}")
        end
        Result.new(:status => :correct, :message => "Gelöst in #{hits.map { |l| LANGUAGE_NAMES[l] || l }.join(' und ')}.",
            :solution => task['solution'])
    end

    # files, html, css, references, git and submit.
    NOT_RUNNING_SCENE = 'Starte zuerst deinen Workspace. Die Szene liest dein eigener Viewer in deinem Workspace.'

    # Runs scene_dump.js for one file in the person's Workspace.
    def self.scene_dump(file, times, runner)
        result = runner&.call(SceneCheck.command(file, times), 15)
        return [nil, :not_running] if result.nil?
        return [nil, :timeout] if result[:timed_out]

        [SceneCheck.parse(result[:output]), nil]
    end

    # scene: what is where in an Anaglyph scene (SceneCheck).
    def self.check_scene(task, check, workspace, runner)
        return Result.new(:status => :unavailable, :message => NO_WORKSPACE) if workspace.nil?

        file = check['file']
        viewer = File.join(File.dirname(file), 'include', 'script.js')
        if workspace.entry(file)[:type] != :file
            other = workspace.other_case(file)
            hint = other ? " Es gibt aber `#{other}`; Groß- und Kleinschreibung zählen." : " Lege die Datei im Ordner `#{File.dirname(file)}` an."
            return Result.new(:status => :wrong, :problem => :missing, :message => "`#{file}` gibt es nicht.#{hint}")
        end
        if workspace.entry(viewer)[:type] != :file
            return Result.new(:status => :unavailable, :problem => :no_viewer,
                :message => "Ich finde den Viewer nicht (`#{viewer}`). Liegt die Vorlage in `/workspace/#{File.dirname(file)}`?")
        end
        source = begin
            workspace.read_text(file, :max => 1_000_000)
        rescue WorkspaceFiles::Refused => e
            return Result.new(:status => :wrong, :problem => e.problem, :message => e.message)
        end
        dump, failure = scene_dump(file, check['at_times'] || [0], runner)
        return Result.new(:status => :unavailable, :message => NOT_RUNNING_SCENE) if failure == :not_running
        if failure == :timeout
            return Result.new(:status => :wrong, :problem => :timeout, :message => 'Die Szene braucht zu lange zum Lesen. Steht irgendwo eine Rechnung, die nie fertig wird?')
        end

        status, problem, message, read = SceneCheck.evaluate(task, check, dump, source)
        Result.new(:status => status, :problem => problem, :message => message, :read => read,
            :solution => status == :correct ? task['solution'] : nil)
    end

    def self.check_workspace(task, check, answer, workspace, git, runner = nil)
        type = check['type']
        return Result.new(:status => :unavailable, :message => NO_WORKSPACE) if workspace.nil?

        if type == 'submit'
            begin
                plan = SubmissionStore.plan(workspace, check, :picked => answer.to_s.strip)
            rescue SubmissionStore::Refused => e
                return Result.new(:status => :invalid, :message => e.message, :problem => e.problem)
            end
            requires = check['requires'] || {}
            if (requires['scene_loads'] || requires['min_objects']) && runner
                plan.files.select { |f| f[:path].end_with?('.ini') }.each do |f|
                    dump, failure = scene_dump(f[:path], [0], runner)
                    return Result.new(:status => :unavailable, :message => NOT_RUNNING_SCENE) if failure == :not_running
                    if dump.nil? || !(dump['errors'] || []).empty?
                        first = dump && dump['errors'].first
                        return Result.new(:status => :invalid, :problem => :scene_error,
                            :message => "Der Viewer meldet Fehler in `#{f[:path]}`#{first ? ": #{first}" : ''}. Repariere das vor der Abgabe.")
                    end
                    count = dump['objects'].reject { |o| o['shape'] == 'grid' }.size
                    if requires['min_objects'] && count < requires['min_objects']
                        return Result.new(:status => :invalid, :problem => :too_few_objects,
                            :message => "In `#{f[:path]}` stehen #{count} Objekte; für die Galerie sind mindestens #{requires['min_objects']} verlangt.")
                    end
                end
                plan.notes.reject! { |n| n.include?('prüft der Workspace noch nicht') }
            end
            message = "Abgegeben. Deine Lehrkraft sieht genau diesen Stand. Du kannst jederzeit neu abgeben oder die Abgabe löschen."
            return Result.new(:status => :sent, :message => message, :plan => plan,
                :read => SubmissionStore.describe(plan.files, plan.total), :warnings => plan.notes)
        end

        if type == 'git'
            repo = check['repo']
            state = if %i[missing].include?(workspace.entry(repo)[:type])
                { :repo => false, :git => false }
            else
                git&.call(repo)
            end
            return Result.new(:status => :unavailable, :message => NOT_RUNNING) if state.nil?

            ok, problem, message = GitCheck.evaluate(repo, check['expect'] || {}, state)
            outcome = FileChecks::Outcome.new(:ok => ok, :problem => problem, :message => message,
                :read => GitCheck.describe(repo, state), :warnings => [])
        else
            outcome = FileChecks.send(type, check, workspace)
        end

        if outcome.ok
            Result.new(:status => :correct, :message => 'Passt.', :solution => task['solution'],
                :read => outcome.read, :warnings => outcome.warnings || [])
        else
            say = (task['feedback'] || []).find { |entry| entry['when']['problem'].to_s == outcome.problem.to_s }
            Result.new(:status => :wrong, :message => say ? say['say'] : outcome.message, :problem => outcome.problem,
                :read => outcome.read, :warnings => outcome.warnings || [])
        end
    end

    # Names of the ${...} variables a check uses.
    def self.variables_used(check)
        JSON.generate(check).scan(TaskSpec::VARIABLE_PATTERN).flatten.uniq
    end

    def self.fill(text, variables)
        text.to_s.gsub(TaskSpec::VARIABLE_PATTERN) { variables[$1].to_s }
    end

    # --- answer -------------------------------------------------------------

    def self.check_answer(task, check, answer, variables)
        expected = Array(check['expect']).map { |e| fill(e, variables) }
        correct = case check['compare']
            when 'url' then expected.any? { |e| same_url?(answer, e) }
            when 'regex' then expected.any? { |e| regex_match?(e, normalize_text(answer, true)) }
            else expected.any? { |e| normalize_text(answer, check['case_sensitive']) == normalize_text(e, check['case_sensitive']) }
        end
        return Result.new(:status => :correct) if correct

        say = (task['feedback'] || []).find do |entry|
            w = entry['when']
            (w.key?('equals') && normalize_text(answer, check['case_sensitive']) == normalize_text(fill(w['equals'], variables), check['case_sensitive'])) ||
                (w.key?('matches') && regex_match?(w['matches'], normalize_text(answer, true)))
        end
        Result.new(:status => :wrong, :message => say && say['say'])
    end

    # Trims, collapses whitespace, drops quotes or backticks around the whole
    # answer and, unless case_sensitive, ignores case.
    def self.normalize_text(text, case_sensitive = false)
        s = text.to_s.strip.gsub(/\s+/, ' ')
        s = s[1..-2].strip while s.size >= 2 && ['``', '""', "''", '»«', '„“', '“”'].include?(s[0] + s[-1])
        case_sensitive ? s : s.downcase
    end

    def self.regex_match?(pattern, text)
        Regexp.new(pattern, timeout: 0.5).match?(text)
    rescue RegexpError, Regexp::TimeoutError
        false
    end

    # Same address: scheme does not matter, host ignores case, path, query and
    # port must match (an empty path is /).
    def self.same_url?(given, expected)
        a = parse_url(given)
        b = parse_url(expected)
        return false if a.nil? || b.nil?

        a[:host] == b[:host] && a[:port] == b[:port] && a[:path] == b[:path] && a[:query] == b[:query]
    end

    def self.parse_url(text)
        s = normalize_text(text, true)
        s = "http://#{s}" unless s =~ %r{\A[a-z][a-z0-9+.-]*://}i
        uri = URI.parse(s)
        return nil if uri.host.nil? || uri.host.empty?

        port = uri.port
        port = nil if [80, 443].include?(port)
        path = uri.path.to_s.empty? ? '/' : URI::DEFAULT_PARSER.unescape(uri.path)
        { :host => uri.host.downcase, :port => port, :path => path, :query => uri.query }
    rescue URI::Error
        nil
    end

    # --- number -------------------------------------------------------------

    def self.check_number(task, check, answer, _variables)
        value = parse_number(answer)
        if value.nil?
            return Result.new(:status => :invalid, :message => 'Ich finde in deiner Antwort keine Zahl.')
        end
        tolerance = check['tolerance'] || 0
        return Result.new(:status => :correct) if (value - check['expect']).abs <= tolerance + 1e-9

        say = (task['feedback'] || []).find do |entry|
            w = entry['when']
            (w.key?('equals') && (value - w['equals'].to_f).abs < 1e-9) ||
                (w.key?('between') && value >= w['between'][0].to_f && value <= w['between'][1].to_f)
        end
        Result.new(:status => :wrong, :message => say && say['say'])
    end

    # The first number in a text. Accepts 111, 0,111, 0.111, 11 100,
    # 1.234,5 and a minus sign; a unit after the number is ignored.
    def self.parse_number(text)
        s = text.to_s.tr("   ", '   ').tr("−", '-')
        s = s.gsub(/(?<=\d)[ '](?=\d{3}(?!\d))/, '')
        m = s.match(/[-+]?\d+(?:[.,]\d{3})*(?:[.,]\d+)?/)
        return nil unless m

        n = m[0]
        if n.include?('.') && n.include?(',')
            n = n.index('.') < n.index(',') ? n.delete('.').tr(',', '.') : n.delete(',')
        elsif n.count(',') == 1
            n = n.tr(',', '.')
        elsif n.count(',') > 1 || n.count('.') > 1
            n = n.delete(',.')
        end
        Float(n)
    rescue ArgumentError
        nil
    end

    # --- choice -------------------------------------------------------------

    def self.check_choice(task, check, answer, _variables)
        picked = answer.to_s.split(',').map(&:strip).reject(&:empty?)
        unless picked.all? { |p| p =~ /\A\d+\z/ }
            return Result.new(:status => :invalid, :message => 'Wähle eine der Antworten aus.')
        end
        picked = picked.map(&:to_i).uniq.sort
        unless picked.all? { |p| p < check['options'].size }
            return Result.new(:status => :invalid, :message => 'Wähle eine der Antworten aus.')
        end
        correct = Array(check['correct']).sort
        return Result.new(:status => :correct) if picked == correct

        say = (task['feedback'] || []).find do |entry|
            want = Array(entry['when']['picked']).sort
            want.size == 1 ? picked.include?(want.first) : picked == want
        end
        message = say && say['say']
        if message.nil? && correct.size > 1
            message = 'Das stimmt noch nicht ganz. Mehrere Antworten können richtig sein.'
        end
        Result.new(:status => :wrong, :message => message)
    end

    # --- reflection ---------------------------------------------------------

    def self.check_reflection(_task, check, answer, _variables)
        words = word_count(answer)
        min = check['min_words'] || 1
        if words < min
            return Result.new(:status => :invalid,
                :message => "Schreib noch etwas mehr: mindestens #{min} Wörter (bisher #{words}).")
        end
        Result.new(:status => :sent, :message => 'Gespeichert. Nur du und deine Lehrkraft können die Antwort lesen.')
    end

    def self.word_count(text)
        text.to_s.scan(/[[:alnum:]][[:alnum:]'’.-]*/).size
    end

    # --- variables ----------------------------------------------------------

    # A per-person, per-task codeword like »Otter-42«. Anything that hands out
    # the codeword (a bot in the Workspace) must compute it the same way.
    def self.codeword(secret, email, task_key)
        digest = OpenSSL::HMAC.digest('SHA256', secret.to_s, "#{email}\n#{task_key}")
        bytes = digest.bytes
        "#{CODEWORDS[bytes[0] % CODEWORDS.size]}-#{10 + bytes[1] % 90}"
    end
end
