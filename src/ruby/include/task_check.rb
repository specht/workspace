require 'openssl'
require 'uri'
require_relative 'task_spec'
require_relative 'file_checks'
require_relative 'git_check'
require_relative 'submission_store'
require_relative 'measure_check'
require_relative 'scene_check'
require_relative 'sql_check'
require_relative 'er_check'
require_relative 'classes_check'
require_relative 'python_check'
require_relative 'ruby_check'
require_relative 'prolog_check'
require_relative 'c_check'
require_relative 'javascript_check'
require_relative 'java_check'
require_relative 'dart_check'
require_relative 'schema_check'
require_relative 'keyboard_check'
require_relative 'circuit_check'
require_relative 'turtle_check'
require_relative 'glsl_check'

# Checks a student's answer to a task (docs/aufgaben.md, »Prüfen«).
#
# answer, choice, number and reflection need only the answer. files, html,
# css and references read the student's Workspace folder through
# WorkspaceFiles; git gets the repository state from the caller (read inside
# the container); submit plans a copy that the caller then stores. This
# module never touches Neo4j, Docker or HTTP itself.
module TaskCheck
    ANSWER_TYPES = %w[answer choice number reflection].freeze
    FILE_TYPES = %w[files html css references story].freeze
    # Types the class board can check for a whole group at once: they only
    # read files or git status (/api/class/check_now).
    LIVE_TYPES = (FILE_TYPES + %w[git keyboard]).freeze
    # Types this version can check.
    RUNS = (ANSWER_TYPES + FILE_TYPES + %w[git submit measure command scene sql cypher er classes schema keyboard python ruby prolog javascript c java dart function pip tests circuit turtle]).freeze

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
    # replay: what the card plays back (Pip's run, PipCheck.replay)
    Result = Struct.new(:status, :message, :solution, :read, :warnings, :problem, :plan, :replay, keyword_init: true) do
        def counts?
            %i[correct wrong sent].include?(status)
        end
    end

    UNAVAILABLE_VARIABLE = {
        'workspace.ip' => 'Starte zuerst deinen Workspace. Dann kann ich die Adresse mit der deines Workspaces vergleichen.',
        'workspace.mac' => 'Starte zuerst deinen Workspace. Dann kann ich die MAC-Adresse mit der deines Workspaces vergleichen.',
        'workspace.live_url' => 'Starte zuerst deinen Workspace. Dann kann ich die Adresse mit der deiner Live-Vorschau vergleichen.',
        'person.codeword' => 'Für diese Aufgabe gibt es noch kein Codewort.',
    }.freeze

    CODEWORDS = %w[
        Ameise Biber Dachs Eule Fuchs Gecko Hase Igel Jaguar Koala Lama Luchs
        Marder Nashorn Otter Panda Qualle Rabe Robbe Specht Tapir Uhu Wal Wiesel
        Yak Zebra Elch Falke Kranich Pinguin Schnecke Taube
    ].freeze

    NO_WORKSPACE = 'Dein Workspace-Ordner ist noch leer. Öffne den Workspace einmal, dann kann ich nachsehen.'
    NOT_RUNNING = 'Den Stand von Git konnte ich gerade nicht lesen. Versuch es gleich noch einmal; klappt es dann immer noch nicht, starte deinen Workspace und prüfe dort.'

    # workspace: WorkspaceFiles for the student's folder, or nil if there is none
    # git:       lambda(repo) -> GitCheck.parse result, or nil if the Workspace
    #            is not running
    # runner:      lambda(command, timeout) -> { output:, status:, timed_out: },
    #              runs in the person's container as them; nil if not running.
    #              Only for commands that come from the task or from us.
    # guest_runner: the same, but as a guest user that can read /workspace and
    #              not change it: for commands the person typed (command)
    # sql:         lambda(query) -> result, runs in the person's own MySQL
    #              database as them, read-only (SqlCheck); nil if unreachable
    # sql_env:     what to name when the database cannot be used (login,
    #              database, host, workspace running, .my.cnf); see SqlCheck
    # kata:        lambda(files:, argv:, timeout:) -> KataRunner::Result, a run
    #              in a container of its own (function); nil if none
    # kata_seed:   for a Kata's random cases (KataCheck.seed)
    # kata_map:    Pip: the map the card shows, run first (PipCheck.check)
    # turtle:      what the browser reported of a turtle program's runs:
    #              { 'runs' => [[[command, value], …], …], 'error', 'line' }
    def self.check(task, answer, variables: {}, workspace: nil, git: nil, runner: nil, guest_runner: nil, sql: nil, sql_env: {}, cypher: nil, kata: nil, kata_seed: 0, kata_map: 0, turtle: nil)
        check = task['check']
        type = check['type']
        unless RUNS.include?(type)
            return Result.new(:status => :unavailable,
                :message => 'Diese Aufgabe kann der Workspace noch nicht selbst prüfen. Zeig dein Ergebnis deiner Lehrkraft.')
        end
        return check_run(task, check, answer, type == 'command' ? guest_runner : runner) if %w[measure command].include?(type)
        return check_scene(task, check, workspace, runner) if type == 'scene'
        return check_sql(task, check, answer, sql, sql_env) if type == 'sql'
        return check_cypher(task, check, answer, cypher) if type == 'cypher'
        return check_er(task, check, workspace, runner) if type == 'er'
        return check_classes(task, check, workspace, runner) if type == 'classes'
        return check_calls(task, check, workspace, guest_runner, PythonCheck) if type == 'python'
        return check_calls(task, check, workspace, guest_runner, RubyCheck) if type == 'ruby'
        return check_calls(task, check, workspace, guest_runner, PrologCheck) if type == 'prolog'
        return check_calls(task, check, workspace, guest_runner, JavascriptCheck) if type == 'javascript'
        return check_calls(task, check, workspace, guest_runner, CCheck) if type == 'c'
        return check_calls(task, check, workspace, guest_runner, JavaCheck) if type == 'java'
        return check_calls(task, check, workspace, guest_runner, DartCheck) if type == 'dart'
        return check_function(task, check, answer, kata, kata_seed) if type == 'function'
        return check_tests(task, check, answer, kata) if type == 'tests'
        return check_pip(task, check, answer, kata, kata_seed, kata_map) if type == 'pip'
        return check_schema(task, check, sql, sql_env) if type == 'schema'
        return check_keyboard(task, check, workspace) if type == 'keyboard'
        return check_circuit(task, check, answer) if type == 'circuit'
        return check_turtle(task, check, answer, turtle) if type == 'turtle'
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
            result.message = task['prediction'] ? wrong_prediction(result.message) : (result.message || 'Das stimmt noch nicht.')
        end
        result
    end

    # A wrong prediction says so first, then why (the task's feedback) or
    # what to do. The card shows only the colour of a hint, not »falsch«.
    WRONG_PREDICTION = 'Deine Vermutung stimmt nicht.'

    def self.wrong_prediction(message)
        return "#{WRONG_PREDICTION} Probier es aus und sieh nach, was wirklich passiert." if message.to_s.strip.empty?

        message.lstrip.start_with?('```', '<') ? "#{WRONG_PREDICTION}\n\n#{message}" : "#{WRONG_PREDICTION} #{message}"
    end

    # measure and command run in the person's Workspace (MeasureCheck);
    # command, typed by the person, only ever with the guest runner (rule 1).
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
    def self.check_sql(task, check, answer, sql, env = {})
        status, message, read, note, problem = SqlCheck.check(task, check, answer, sql, env || {})
        Result.new(:status => status, :message => message, :read => read, :problem => problem,
            :warnings => note ? [note] : nil, :solution => status == :correct ? task['solution'] : nil)
    end

    # cypher: a query in the person's own Neo4j database, read-only
    # (CypherCheck). nil runner: Neo4j cannot be reached for this person.
    def self.check_cypher(task, check, answer, runner)
        if runner.nil?
            return Result.new(:status => :unavailable,
                :message => 'Deine Neo4j-Datenbank ist noch nicht eingerichtet. Öffne einmal den Neo4j Browser über das Menü, dann klappt es.')
        end
        status, message, read, note, problem = CypherCheck.check(task, check, answer, runner)
        Result.new(:status => status, :message => message, :read => read, :problem => problem,
            :warnings => note ? [note] : nil, :solution => status == :correct ? task['solution'] : nil)
    end

    # schema: the tables in the person's own database (SchemaCheck).
    def self.check_schema(task, check, sql, env = {})
        status, message, read, note, problem = SchemaCheck.check(task, check, sql, env || {})
        Result.new(:status => status, :message => message, :read => read, :problem => problem,
            :warnings => note ? [note] : nil, :solution => status == :correct ? task['solution'] : nil)
    end

    # keyboard: one section of the Keyboard-Tutorial, from its state file
    # (KeyboardCheck). Read like a file; the Workspace need not run.
    def self.check_keyboard(task, check, workspace)
        return Result.new(:status => :unavailable, :message => NO_WORKSPACE) if workspace.nil?

        state, problem = KeyboardCheck.read_state(workspace)
        status, problem, message, read = KeyboardCheck.evaluate(check, state, problem)
        Result.new(:status => status, :problem => problem, :message => message, :read => read,
            :solution => status == :correct ? task['solution'] : nil)
    end

    NOT_RUNNING_ER = 'Starte zuerst deinen Workspace. Dein Modell liest die ER-Werkstatt in deinem Workspace.'

    # er: an ER model in a .er file, read by the ER-Werkstatt core (ErCheck).
    def self.check_er(task, check, workspace, runner)
        return Result.new(:status => :unavailable, :message => NO_WORKSPACE) if workspace.nil?
        return Result.new(:status => :unavailable, :message => 'Die ER-Werkstatt fehlt auf dem Server. Sag deiner Lehrkraft Bescheid.') unless ErCheck.available?

        file = check['file']
        if workspace.entry(file)[:type] != :file
            other = workspace.other_case(file)
            hint = other ? " Es gibt aber `#{other}`; Groß- und Kleinschreibung zählen." : " Lege sie im Ordner `#{File.dirname(file)}` an."
            return Result.new(:status => :wrong, :problem => :missing, :message => "`#{file}` gibt es nicht.#{hint}")
        end
        result = runner&.call(ErCheck.command(file), 15)
        return Result.new(:status => :unavailable, :message => NOT_RUNNING_ER) if result.nil?
        return Result.new(:status => :unavailable, :message => 'Das Lesen des Modells hat zu lange gedauert. Versuch es gleich noch einmal.') if result[:timed_out]

        status, problem, message, read = ErCheck.evaluate(task, check, ErCheck.parse(result[:output]))
        Result.new(:status => status, :problem => problem, :message => message, :read => read,
            :solution => status == :correct ? task['solution'] : nil)
    end

    NOT_RUNNING_CLASSES = 'Starte zuerst deinen Workspace. Dein Diagramm liest die UML-Werkstatt in deinem Workspace.'

    # classes: a class diagram, read by the UML-Werkstatt's core in the
    # Workspace (ClassesCheck), like er.
    def self.check_classes(task, check, workspace, runner)
        return Result.new(:status => :unavailable, :message => NO_WORKSPACE) if workspace.nil?
        return Result.new(:status => :unavailable, :message => 'Die UML-Werkstatt fehlt auf dem Server. Sag deiner Lehrkraft Bescheid.') unless ClassesCheck.available?

        file = check['file']
        if workspace.entry(file)[:type] != :file
            other = workspace.other_case(file)
            hint = other ? " Es gibt aber `#{other}`; Groß- und Kleinschreibung zählen." : " Lege sie im Ordner `#{File.dirname(file)}` an."
            return Result.new(:status => :wrong, :problem => :missing, :message => "`#{file}` gibt es nicht.#{hint}")
        end
        result = runner&.call(ClassesCheck.command(file), 15)
        return Result.new(:status => :unavailable, :message => NOT_RUNNING_CLASSES) if result.nil?
        return Result.new(:status => :unavailable, :message => 'Das Lesen des Diagramms hat zu lange gedauert. Versuch es gleich noch einmal.') if result[:timed_out]

        status, problem, message, read = ClassesCheck.evaluate(task, check, ClassesCheck.parse(result[:output]))
        Result.new(:status => status, :problem => problem, :message => message, :read => read,
            :solution => status == :correct ? task['solution'] : nil)
    end

    NOT_RUNNING_CALLS = 'Starte zuerst deinen Workspace. Dein Programm läuft zum Prüfen in deinem Workspace.'

    # python, ruby, javascript, c, java, dart: calls into the person's own
    # file (PythonCheck, RubyCheck, JavascriptCheck, CCheck, JavaCheck, DartCheck). The code is theirs, so it only ever runs as the guest
    # (rule 1).
    def self.check_calls(task, check, workspace, runner, language)
        return Result.new(:status => :unavailable, :message => NO_WORKSPACE) if workspace.nil?

        file = check['file']
        if workspace.entry(file)[:type] != :file
            other = workspace.other_case(file)
            hint = other ? " Es gibt aber `#{other}`; Groß- und Kleinschreibung zählen." : " Lege sie im Ordner `#{File.dirname(file)}` an."
            return Result.new(:status => :wrong, :problem => :missing, :message => "`#{file}` gibt es nicht.#{hint}")
        end
        seconds = check['timeout'] || 10
        command = language.respond_to?(:command_for_check) ? language.command_for_check(check) : language.command(file, language.cases(check))
        result = runner&.call(command, seconds)
        return Result.new(:status => :unavailable, :message => NOT_RUNNING_CALLS) if result.nil?

        status, message, read = language.evaluate(task, check, result[:output], timed_out: result[:timed_out])
        Result.new(:status => status, :message => message, :read => read,
            :solution => status == :correct ? task['solution'] : nil)
    end

    # function: a Kata, the code of the card in a container of its own.
    def self.check_function(task, check, answer, kata, seed)
        kata ||= ->(**_) { nil }
        status, message, read = KataCheck.check(task, check, answer, kata, :seed => seed)
        Result.new(:status => status, :message => message, :read => read,
            :solution => status == :correct ? task['solution'] : nil)
    end

    # tests: a Kata of tests, against a right and broken versions (TestsCheck).
    def self.check_tests(task, check, answer, kata)
        kata ||= ->(**_) { nil }
        status, message, read = TestsCheck.check(task, check, answer, kata)
        Result.new(:status => status, :message => message, :read => read,
            :solution => status == :correct ? task['solution'] : nil)
    end

    # circuit: a netlist from the Logik-Werkstatt, simulated on the server
    # (CircuitCheck); nothing in it runs.
    def self.check_circuit(task, check, answer)
        status, message, read = CircuitCheck.check(task, check, answer)
        Result.new(:status => status, :message => message, :read => read,
            :solution => status == :correct ? task['solution'] : nil)
    end

    # turtle: a program that ran in the browser; its lines against the
    # picture asked for (TurtleCheck). replay is what the card draws when
    # the picture is not right.
    def self.check_turtle(task, check, answer, report)
        return Result.new(:status => :invalid, :message => TurtleCheck::EMPTY) if answer.to_s.strip.empty?
        return Result.new(:status => :invalid, :message => 'Lass dein Programm zuerst bis zum Ende laufen.') unless report.is_a?(Hash)

        if report['error']
            line = report['line'].is_a?(Integer) ? " (Zeile #{report['line']})" : ''
            message = "Dein Programm bricht ab#{line}: `#{report['error'].to_s[0, 200]}`"
            explanation = KataCheck.explain(report['error'].to_s, check['language'])
            message += "\n\n#{explanation}" if explanation
            return Result.new(:status => :wrong, :message => message)
        end
        status, message, show = TurtleCheck.check(check, answer, report['runs'])
        Result.new(:status => status, :message => message, :replay => show,
            :solution => status == :correct ? task['solution'] : nil)
    end

    # pip: a plan for Pip, against the world on the server (PipCheck).
    def self.check_pip(task, check, answer, kata, seed, map)
        kata ||= ->(**_) { nil }
        status, message, read, replay = PipCheck.check(task, check, answer, kata, :seed => seed, :selected => map)
        Result.new(:status => status, :message => message, :read => read, :replay => replay,
            :solution => status == :correct ? task['solution'] : nil)
    end

    # files, html, css, references, git and submit.
    NOT_RUNNING_SCENE = 'Starte zuerst deinen Workspace. Die Szene liest dein eigener Viewer in deinem Workspace.'

    # »steht kein Objekt«, »steht 1 Objekt«, »stehen 3 Objekte«.
    def self.objects_phrase(count)
        case count
        when 0 then 'steht kein Objekt'
        when 1 then 'steht 1 Objekt'
        else "stehen #{count} Objekte"
        end
    end

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
            # With preview: anaglyph the card checks the scene in the browser,
            # with the picture it draws anyway (tasks.js): no Workspace has
            # to run for a hand-in, and student expressions are evaluated in
            # the student's browser only.
            if check['preview'] == 'anaglyph'
                plan.notes.reject! { |n| n.include?('prüft der Workspace noch nicht') }
            elsif (requires['scene_loads'] || requires['min_objects']) && runner
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
                            :message => "In `#{f[:path]}` #{TaskCheck.objects_phrase(count)}; für die Galerie sind mindestens #{requires['min_objects']} verlangt.")
                    end
                end
                plan.notes.reject! { |n| n.include?('prüft der Workspace noch nicht') }
            end
            message = "Abgegeben. Deine Lehrkraft sieht genau diesen Stand. Du kannst jederzeit neu abgeben oder die Abgabe löschen."
            return Result.new(:status => :sent, :message => message, :plan => plan,
                :read => SubmissionStore.describe(plan.files, plan.total), :warnings => plan.notes)
        end

        if type == 'git'
            repo = check['repo'].to_s
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

        # a shader that has what is looked for must also compile (GlslCheck)
        if outcome.ok && type == 'files' && check.dig('expect', 'compile') == 'glsl' && runner
            message = GlslCheck.check(check['path'], runner)
            outcome = FileChecks::Outcome.new(:ok => false, :problem => :compile_error, :message => message,
                :read => outcome.read, :warnings => outcome.warnings) if message
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

    # Names of the ${...} variables a check uses. Only the known ones
    # (TaskSpec::VARIABLES): ${name} in a JavaScript template literal, say an
    # option of a choice, is text.
    def self.variables_used(check)
        JSON.generate(check).scan(TaskSpec::VARIABLE_PATTERN).flatten.uniq & TaskSpec::VARIABLES
    end

    def self.fill(text, variables)
        text.to_s.gsub(TaskSpec::VARIABLE_PATTERN) { |found| TaskSpec::VARIABLES.include?($1) ? variables[$1].to_s : found }
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

    # Commands an answer may start with when it is the command that finds
    # the number, not the number (»grep -i hinweis tatort.txt | wc -l«). The
    # check never runs it; it says what to do instead, before looking for a
    # number (»head -n 5 … | wc -l« is not the answer 5).
    COMMANDS = %w[grep egrep wc cat head tail sort uniq cut tr awk sed find ls echo printf expr bc seq less file
                  du df ps stat python python3 ruby node sqlite3 mysql].freeze

    def self.command?(answer)
        words = answer.to_s.strip.split(/\s+/)
        answer.to_s.include?('|') || (words.size > 1 && COMMANDS.include?(words.first))
    end

    def self.check_number(task, check, answer, _variables)
        if command?(answer)
            return Result.new(:status => :invalid,
                :message => 'Das ist der Befehl, der die Zahl findet. Führe ihn im Terminal aus und gib hier die Zahl ein, die er ausgibt.')
        end
        value = parse_number(answer)
        if value.nil?
            return Result.new(:status => :invalid, :message => 'Ich finde in deiner Antwort keine Zahl.')
        end
        tolerance = check['tolerance'] || 0
        # »57.596« may be 57,596 or, written the German way, 57596: either
        # reading counts.
        values = [value, thousands_reading(answer)].compact
        return Result.new(:status => :correct) if values.any? { |v| (v - check['expect']).abs <= tolerance + 1e-9 }

        say = (task['feedback'] || []).find do |entry|
            w = entry['when']
            values.any? do |v|
                (w.key?('equals') && (v - w['equals'].to_f).abs < 1e-9) ||
                    (w.key?('between') && v >= w['between'][0].to_f && v <= w['between'][1].to_f)
            end
        end
        Result.new(:status => :wrong, :message => say && say['say'])
    end

    # The number with a single dot before exactly three digits read as a
    # thousands separator (57.596 as 57596), or nil if it has no such dot.
    def self.thousands_reading(text)
        m = text.to_s.match(/[-+−]?\d{1,3}\.\d{3}(?![\d.,])/)
        return nil unless m && text.to_s.scan(/\d[.,]\d/).size == 1

        Float(m[0].tr('−', '-').delete('.'))
    rescue ArgumentError
        nil
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
            message = task['prediction'] ? 'Mehrere Antworten können richtig sein.' : 'Das stimmt noch nicht ganz. Mehrere Antworten können richtig sein.'
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
