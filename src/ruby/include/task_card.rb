require 'cgi'
require 'digest'
require_relative 'task_check'
require_relative 'hint_steps'

# HTML of a task card on an activity page (docs/aufgaben.md, »Aufgaben auf
# der Seite«). The card never contains what is checked against (expect,
# correct, solution); tasks.js sends the answer to /api/task/check.
module TaskCard
    # _task(id) on a line of its own.
    PLACEHOLDER = /^[ \t]*_task\(\s*([a-z0-9][a-z0-9-]*)\s*\)[ \t]*$/

    # Replaces each _task(id) line by a numbered slot marker. Returns the new
    # Markdown and the list of ids in page order.
    # Fenced code blocks are left alone, so pages can show the placeholder.
    def self.mark_slots(markdown)
        ids = []
        out = markdown.split(/(^```.*?^```[ \t]*$)/m).map.with_index do |piece, i|
            next piece if i.odd?

            piece.gsub(PLACEHOLDER) do
                ids << $1
                "\n<div class='task-slot' data-slot='#{ids.size - 1}'></div>\n"
            end
        end.join
        [out, ids]
    end

    # render: lambda Markdown -> HTML, used for prompt, hint and options.
    # language: the programming language of this card when the task has one
    # for each (CodeLanguages); tasks.js sends it along. id_suffix keeps the
    # element ids of the second card apart from the first.
    def self.html(task, activity:, number:, render:, language: nil, id_suffix: '')
        h = ->(s) { CGI.escapeHTML(s.to_s) }
        check = task['check']
        type = check['type']
        key = "#{activity}/#{task['id']}"
        field = "task-#{activity}-#{task['id']}#{id_suffix}"
        runs = TaskCheck::RUNS.include?(type)

        io = +''
        io << "<section class='task-card' id='aufgabe-#{h.(task['id'])}#{h.(id_suffix)}' data-task='#{h.(key)}' data-type='#{h.(type)}'"
        io << " data-language='#{h.(language)}'" if language
        io << " data-preview='#{h.(task['check']['preview'])}'" if task['check'].is_a?(Hash) && task['check']['preview']
        if task['check'].is_a?(Hash) && task['check']['preview'] && task['check'].dig('requires', 'min_objects')
            io << " data-min-objects='#{task['check']['requires']['min_objects'].to_i}'"
        end
        io << " data-prediction='1'" if task['prediction']
        io << " aria-labelledby='#{h.(field)}-title'>"
        io << "<div class='task-head'>"
        io << "<span class='task-number'>#{task['prediction'] ? 'Vermutung' : 'Aufgabe'} #{number.to_i}</span>"
        io << "<span class='task-title' id='#{h.(field)}-title'>#{h.(task['title'])}</span>"
        io << level_html(task['level']) if task['level']
        if task['extra']
            io << "<span class='task-extra' title='Für Schnelle und Neugierige; zählt nicht mit'><i class='bi bi-stars' aria-hidden='true'></i>Extra</span>"
        end
        io << "<span class='task-status' aria-live='polite'></span>"
        io << '</div>'
        io << "<div class='task-prompt'>#{render.(task['prompt'])}</div>"

        if runs
            reads = nil
            io << "<form class='task-form' autocomplete='off'>"
            case type
            when 'answer'
                io << "<input type='text' class='form-control task-input' name='answer' aria-labelledby='#{h.(field)}-title'"
                io << " placeholder='#{h.(check['placeholder'])}'" if check['placeholder']
                io << " spellcheck='false'>"
            when 'number'
                io << "<div class='input-group task-number-input'>"
                io << "<input type='text' inputmode='decimal' class='form-control task-input' name='answer' aria-labelledby='#{h.(field)}-title'"
                io << " placeholder='#{h.(check['placeholder'])}'" if check['placeholder']
                io << '>'
                io << "<span class='input-group-text'>#{h.(check['unit'])}</span>" if check['unit']
                io << '</div>'
            when 'choice'
                multiple = check['correct'].is_a?(Array) && check['correct'].size > 1
                io << "<fieldset class='task-options'><legend class='visually-hidden'>#{h.(task['title'])}</legend>"
                io << "<div class='task-options-note'>Mehrere Antworten können richtig sein.</div>" if multiple
                check['options'].each_with_index do |option, i|
                    id = "#{field}-#{i}"
                    io << "<div class='form-check'>"
                    io << "<input class='form-check-input' type='#{multiple ? 'checkbox' : 'radio'}' name='#{h.(field)}' id='#{h.(id)}' value='#{i}'>"
                    io << "<label class='form-check-label' for='#{h.(id)}'>#{inline(render.(option))}</label>"
                    io << '</div>'
                end
                io << '</fieldset>'
            when 'reflection'
                io << "<textarea class='form-control task-input' name='answer' rows='4' aria-labelledby='#{h.(field)}-title'"
                io << " placeholder='#{h.(check['placeholder'])}'" if check['placeholder']
                io << '></textarea>'
                if check['min_words']
                    io << "<div class='task-words'>Mindestens #{check['min_words'].to_i} Wörter. Nur du und deine Lehrkraft können die Antwort lesen.</div>"
                end
            when 'measure'
                io << "<div class='task-measure'>"
                check['fields'].each_with_index do |f, i|
                    id = "#{field}-m#{i}"
                    io << "<label class='task-measure-label' for='#{h.(id)}'>#{h.(f['label'])}</label>"
                    io << "<div class='input-group task-number-input'>"
                    io << "<input type='text' class='form-control task-measure-input' id='#{h.(id)}' autocomplete='off'#{check['compare'] == 'text' ? '' : " inputmode='decimal'"}>"
                    io << "<span class='input-group-text'>#{h.(check['unit'])}</span>" if check['unit']
                    io << '</div>'
                end
                io << '</div>'
                reads = 'Der Workspace misst danach selbst nach, in deinem Workspace, und vergleicht.'
            when 'sql'
                io << "<textarea class='form-control task-input task-sql' name='answer' rows='4' spellcheck='false' autocapitalize='off' placeholder='SELECT …' aria-labelledby='#{h.(field)}-title'></textarea>"
                reads = "Läuft in deiner eigenen MySQL-Datenbank, angemeldet als du, und liest nur: eine Abfrage mit <code>SELECT</code>, höchstens #{SqlCheck::MAX_MS / 1000} Sekunden. <kbd>Strg</kbd>+<kbd>Enter</kbd> prüft."
            when 'cypher'
                io << "<textarea class='form-control task-input task-sql task-cypher' name='answer' rows='4' spellcheck='false' autocapitalize='off' placeholder='MATCH …' aria-labelledby='#{h.(field)}-title'></textarea>"
                reads = "Läuft in deiner eigenen Neo4j-Datenbank, angemeldet als du, und liest nur: eine Abfrage mit <code>MATCH</code> und <code>RETURN</code>, höchstens #{CypherCheck::MAX_SECONDS} Sekunden. <kbd>Strg</kbd>+<kbd>Enter</kbd> prüft."
            when 'pip'
                # Pip's plan (PipCheck): the first map above the editor, for
                # pip_world.js to draw and to play the runs on; map and editor
                # side by side on a wide screen.
                first = check['maps'].first
                world = PipWorld.new(first['map'], :facing => first['facing'] || 'east')
                # every map of the check, for the card's tabs; one made from a
                # person's seed (generate:) only shows after a run
                maps = check['maps'].map do |m|
                    # a maze of w × h cells is 2w + 1 × 2h + 1 tiles (PipMaps)
                    next { :size => m['size'].map { |n| 2 * n + 1 }, :call => m['call'] } if m['generate']

                    w = PipWorld.new(m['map'], :facing => m['facing'] || 'east')
                    { :layout => w.layout, :start => w.trace.first, :call => m['call'] }
                end
                io << "<div class='pip-layout'>"
                io << "<div class='pip-world' data-layout='#{h.(JSON.generate(world.layout))}' data-start='#{h.(JSON.generate(world.trace.first))}' data-maps='#{h.(JSON.generate(maps))}'></div>"
                buttons = check['buttons']
                # with buttons the plan is clicked together and only watched
                io << "<textarea class='form-control task-input task-code' name='answer' rows='6' spellcheck='false' autocapitalize='off'"
                io << ' readonly' if buttons
                io << " data-code-language='#{h.(check['language'])}' data-starter='#{h.(task['starter'])}' aria-labelledby='#{h.(field)}-title'>"
                io << "#{h.(task['starter'])}</textarea>"
                if buttons
                    # a button a command: a click moves Pip and writes its
                    # line into the plan; »Rückgängig« takes the last one back
                    io << "<div class='pip-buttons' aria-label='Pip steuern'>"
                    buttons.each do |command|
                        icon, label = PipCheck::BUTTON_LABELS.fetch(command)
                        line = PipCheck.spelling(check['language'], command) + (check['language'] == 'javascript' ? ';' : '')
                        io << "<button type='button' class='btn btn-outline-success pip-button' data-command='#{h.(command)}' data-line='#{h.(line)}'>" \
                              "<i class='bi #{icon}' aria-hidden='true'></i>#{h.(label)}</button>"
                    end
                    io << "<button type='button' class='btn btn-outline-secondary pip-undo' disabled><i class='bi bi-arrow-return-left' aria-hidden='true'></i>Rückgängig</button>"
                    io << '</div>'
                else
                    # the commands this task knows, as the language writes them
                    io << "<div class='pip-commands' aria-label='Befehle für Pip'>"
                    (check['commands'] + ['say']).each do |command|
                        io << "<code class='pip-command#{PipCheck::QUESTIONS.include?(command) ? ' pip-question' : ''}' title='#{h.(PipCheck::DESCRIPTIONS[command])}'>#{h.(PipCheck.spelling(check['language'], command))}</code>"
                    end
                    io << '</div>'
                end
                io << '</div>'
                maps = check['maps'].size
                reads = if buttons
                    'Jeder Klick schickt Pip einen Schritt los und schreibt die Zeile dazu in den Plan; <em>Rückgängig</em> nimmt die letzte Zeile wieder heraus. ' \
                        'Erreicht Pip die Lampe, wird der Plan geprüft, als hättest du ihn getippt: in einem eigenen kleinen Rechner, getrennt von deinem Workspace und ohne Internet.'
                else
                    "Dein Plan läuft in einem eigenen kleinen Rechner, getrennt von deinem Workspace und ohne Internet. Pip macht genau, was draufsteht. " \
                    "<em>Ausführen</em> schickt Pip #{maps > 1 ? "durch alle #{maps} Karten, jedes Mal von vorn," : 'los'} und spielt den Lauf auf der Karte ab; schafft Pip es, ist die Kata gelöst. <kbd>Strg</kbd>+<kbd>Enter</kbd> führt aus."
                end
            when 'function', 'tests'
                # A Kata: the code is written here; it starts with the task's
                # starter. tests: the code is tests (TestsCheck).
                io << "<textarea class='form-control task-input task-code' name='answer' rows='6' spellcheck='false' autocapitalize='off'"
                io << " data-code-language='#{h.(check['language'])}' data-starter='#{h.(task['starter'])}' aria-labelledby='#{h.(field)}-title'>"
                io << "#{h.(task['starter'])}</textarea>"
                reads = if type == 'tests'
                    "Deine Tests laufen in einem eigenen kleinen Rechner, getrennt von deinem Workspace und ohne Internet: einmal mit einer richtigen Fassung von <code>#{h.(check['function'])}</code>, bei der alle bestehen müssen, und mit #{check['mutants'].size} fehlerhaften, von denen jede an mindestens einem Test scheitern muss. <kbd>Strg</kbd>+<kbd>Enter</kbd> führt aus."
                else
                    "Dein Code läuft in einem eigenen kleinen Rechner, getrennt von deinem Workspace und ohne Internet, höchstens #{(check['timeout'] || 10).to_i} Sekunden. " \
                        "<em>Ausführen</em> startet ihn einmal und zeigt, was er ausgibt, und ruft dann deine Funktionen auf und vergleicht. <kbd>Strg</kbd>+<kbd>Enter</kbd> führt aus."
                end
            when 'turtle'
                # The turtle (TurtleCheck): its picture above the editor,
                # with what the task asks for drawn faintly; the program runs
                # in the browser (turtle_canvas.js, turtle_worker.js).
                target = TurtleCheck.target(check)
                calls = TurtleCheck.runs(check)
                io << "<div class='turtle-layout'>"
                io << "<div class='turtle-stage' data-language='#{h.(check['language'])}' data-runs='#{h.(JSON.generate(calls))}'"
                io << " data-target='#{h.(JSON.generate(target))}'" if target
                io << " data-target-call='#{h.(calls.first)}'" if target && calls.first
                io << '></div>'
                io << "<textarea class='form-control task-input task-code' name='answer' rows='6' spellcheck='false' autocapitalize='off'"
                io << " data-code-language='#{h.(check['language'])}' data-starter='#{h.(task['starter'])}' aria-labelledby='#{h.(field)}-title'>"
                io << "#{h.(task['starter'])}</textarea>"
                io << '</div>'
                reads = 'Dein Programm läuft hier in deinem Browser. Die Schildkröte zeichnet, während es läuft, bis es zu Ende ist oder du es anhältst. ' \
                    "Ist es zu Ende, geht das Bild an den Server, und der vergleicht es mit dem#{check['free'] ? ', was die Aufgabe verlangt' : ' Bild, das die Aufgabe zeigt'}#{check['cases'] ? ', für jeden Aufruf von vorn' : ''}. " \
                    '<kbd>Strg</kbd>+<kbd>Enter</kbd> führt aus.'
            when 'circuit'
                # The Logik-Werkstatt's editor (circuit_editor.js) with the
                # task's switches and lamps; it keeps the netlist in the
                # hidden field, which is the answer (CircuitCheck).
                attrs = { 'inputs' => check['inputs'].join(' '), 'outputs' => check['outputs'].join(' ') }
                attrs['gates'] = check['gates'].join(' ') if check['gates']
                attrs['max-gates'] = check['max_gates'].to_s if check['max_gates']
                attrs['min-blocks'] = check['min_blocks'].to_s if check['min_blocks']
                attrs['blocks'] = 'false' if check['blocks'] == false
                attrs['memory'] = 'true' if check['sequence']
                io << "<div class='circuit-editor' data-task-editor='true'#{attrs.map { |k, v| " data-#{k}='#{h.(v)}'" }.join}></div>"
                io << "<textarea class='task-input circuit-answer' name='answer' hidden aria-hidden='true' tabindex='-1'></textarea>"
                steps = check['sequence'] ? 'stellt die Schalter Schritt für Schritt so, wie die Aufgabe es sagt' : "schaltet jede Zeile der Wahrheitstabelle ein, #{1 << check['inputs'].size} Zeilen"
                reads = "Deine Schaltung geht als Text an den Server. Er #{steps}, lässt die Gatter Runde für Runde rechnen wie hier im Editor und vergleicht die Lampen. Ausgeführt wird dabei nichts."
            when 'command'
                io << "<input type='text' class='form-control task-input task-command' name='answer' spellcheck='false' autocomplete='off' placeholder='Befehl' aria-labelledby='#{h.(field)}-title'>"
                where = check['dir'] ? "/workspace/#{check['dir'].to_s.sub(%r{/+\z}, '')}" : '/workspace'
                reads = "Läuft in deinem Workspace in <code>#{h.(where)}</code>, höchstens #{(check['timeout'] || 10).to_i} Sekunden, als Gast: Der Befehl kann lesen, aber nichts verändern oder löschen."
            when 'submit'
                if check['pick']
                    io << "<label class='task-pick-label' for='#{h.(field)}-pick'>Welche Datei gibst du ab?</label>"
                    io << "<select class='form-select task-input task-pick' id='#{h.(field)}-pick' name='answer' data-pick='#{h.(check['pick'])}'>"
                    io << "<option value=''>Datei aus #{h.(File.dirname(check['pick']))}/ wählen …</option></select>"
                end
                reads = reads_line(check, h)
            else
                reads = reads_line(check, h)
            end
            label = case
                when %w[command sql cypher].include?(type) then 'Ausführen und prüfen'
                when type == 'reflection' then 'Speichern'
                when type == 'submit' then 'Abgeben'
                when task['prediction'] then 'Vermutung abgeben'
                # a Kata: one button runs the code (Pip: plays the run) and,
                # when it works, solves the Kata
                when %w[pip function tests turtle].include?(type) then 'Ausführen'
                else 'Prüfen'
            end
            io << "<div class='task-actions'>"
            play = "<i class='bi bi-play-fill' aria-hidden='true'></i>"
            # a clicked plan is checked at the lamp (PipButtons)
            clicked = type == 'pip' && check['buttons']
            io << "<button type='submit' class='btn btn-success task-submit'#{clicked ? ' hidden' : ''}>#{%w[pip function tests turtle].include?(type) ? play : ''}#{label}</button>"
            io << "<button type='button' class='btn btn-link btn-sm task-delete' hidden>Meine Antwort löschen</button>"
            # What the check reads, for whoever wants to know (rule 5), behind
            # a small (i) so the card itself stays short.
            if reads
                io << "<button type='button' class='task-reads-toggle' aria-expanded='false' aria-controls='#{h.(field)}-reads' title='Was passiert beim Prüfen?'>"
                io << "<i class='bi bi-info-circle' aria-hidden='true'></i><span class='visually-hidden'>Was passiert beim Prüfen?</span></button>"
            end
            if %w[function pip tests turtle].include?(type)
                io << "<span class='task-queue' aria-live='polite'></span>"
                # Einreichen (KataHandIns): shown once the Kata is solved
                io << "<button type='button' class='btn btn-link btn-sm task-hand-in' hidden " \
                    "title='Deine Lehrkraft kann deine Lösung in der Stunde zeigen, ohne deinen Namen. Du kannst sie jederzeit zurücknehmen.'>Einreichen</button>"
            end
            io << '</div>'
            io << "<div class='task-reads' id='#{h.(field)}-reads' hidden>#{reads}</div>" if reads
            io << "<pre class='task-console no-copy' hidden aria-live='polite' aria-label='Ausgabe'></pre>" if %w[function pip tests turtle].include?(type)
            io << '</form>'
        else
            io << "<p class='task-later'><i class='bi bi-hourglass-split'></i>Diese Aufgabe kann der Workspace noch nicht selbst prüfen. Zeig dein Ergebnis deiner Lehrkraft.</p>"
        end

        # one tip or several: the same toggle with the chevron as everywhere (HintSteps)
        if task['hint']
            io << HintSteps.html(Array(task['hint']).map { |hint| render.(hint) }, :extra_class => 'task-hint')
        end
        io << "<div class='task-feedback' aria-live='polite'></div>"
        io << "<div class='task-login-note'><a href='/login'>Melde dich an</a>, damit der Workspace deine Antwort prüfen und speichern kann.</div>" if runs
        io << '</section>'
        io
    end

    # The level of a Kata (TaskSpec::LEVELS): a small mark with its name and
    # three rising bars, as many filled as the level (default.css).
    LEVEL_NAMES = { 1 => 'Einstieg', 2 => 'Übung', 3 => 'Knobelei' }.freeze
    LEVEL_TITLES = { 1 => 'Zum Einstieg: eine Sache auf einmal', 2 => 'Zum Üben: wie eben, ein wenig anders',
                     3 => 'Zum Knobeln: braucht eine eigene Idee' }.freeze

    def self.level_html(level)
        name = LEVEL_NAMES.fetch(level)
        bars = (1..3).map { |i| "<i#{i <= level ? " class='on'" : ''}></i>" }.join
        "<span class='task-level task-level-#{level}' title='#{LEVEL_TITLES.fetch(level)}'>" \
            "<span class='task-level-bars' aria-hidden='true'>#{bars}</span>#{name}</span>"
    end

    # Which files a Workspace check reads or copies, with their real paths
    # (rule 5: addresses stay visible).
    def self.reads_line(check, h)
        path = ->(rel) { "<code>/workspace/#{h.(rel)}</code>" }
        size = ->(text) { h.(text.to_s.sub(/\A(\d+(?:\.\d+)?)\s*(B|kB|MB)\z/, '\\1 \\2')) }
        case check['type']
        when 'files' then "Sieht nach in #{path.(check['path'])}"
        when 'keyboard' then "Liest #{path.('.hs-kbd-tutorial/.state.json')}, den Fortschritt des Keyboard-Tutorials"
        when 'html', 'css' then "Liest #{path.(check['file'])}"
        when 'story' then "Liest #{path.("#{check['project']}/.story-tools/analysis.json")}, was BIF über deine Geschichte herausgefunden hat"
        when 'references'
            files = (check['scan'] || []).map { |f| path.("#{check['root']}/#{f}") }
            "Liest #{files.join(' und ')}"
        when 'git'
            if check['repo'].to_s.empty?
                'Liest <code>git config user.name</code> und <code>user.email</code> in deinem Workspace, ändert nichts'
            else
                "Liest <code>git status</code> in #{path.(check['repo'])}, ändert nichts"
            end
        when 'scene'
            viewer = File.join(File.dirname(check['file'].to_s), 'include', 'script.js')
            "Liest #{path.(check['file'])} mit deinem Viewer #{path.(viewer)}, in deinem Workspace"
        when 'er' then "Liest #{path.(check['file'])} mit der ER-Werkstatt, in deinem Workspace"
        when 'python', 'ruby'
            n = (check['cases'] || []).size
            "Lädt #{path.(check['file'])} in deinem Workspace, als Gast, der nichts verändern kann, und probiert #{n == 1 ? 'einen Aufruf' : "#{n} Aufrufe"} aus"
        when 'c'
            n = (check['cases'] || []).size
            "Übersetzt #{path.(check['file'])} in deinem Workspace, als Gast, der nichts verändern kann, startet nicht dein <code>main</code>, sondern probiert #{n == 1 ? 'einen Aufruf' : "#{n} Aufrufe"} aus"
        when 'schema'
            counts = (check['tables'] || []).any? { |t| t.is_a?(Hash) && t['rows_where'] }
            counts ? 'Zählt Zeilen in deiner MySQL-Datenbank, angemeldet als du, und ändert nichts' : 'Liest den Aufbau deiner MySQL-Datenbank, angemeldet als du, und ändert nichts'
        when 'submit'
            if check['pick']
                "Kopiert die gewählte Datei, höchstens #{size.(check['max_size'] || '20MB')}"
            else
                without = (check['exclude'] || []).map { |e| "<code>#{h.(File.basename(e))}</code>" }
                "Kopiert #{(check['paths'] || []).map { |p| path.(p) }.join(', ')}#{without.empty? ? '' : " ohne #{without.join(', ')}"}, höchstens #{size.(check['max_size'] || '20MB')}"
            end
        else ''
        end
    end

    # The order in which one person sees the options of a choice task: the
    # same every time for this person and task, different for the neighbour,
    # so »the third answer« cannot be passed on. Positions only; every option
    # keeps its original number as its value, so answers, correct and
    # feedback (picked) still count from the order in tasks.yaml.
    # A plain Fisher-Yates on SHA-256, so the order never changes with Ruby.
    def self.option_order(person, task_key, count)
        order = (0...count).to_a
        return order if person.to_s.empty? || count < 2

        digest = Digest::SHA256.digest("#{person}\n#{task_key}")
        (count - 1).downto(1) do |i|
            digest = Digest::SHA256.digest(digest)
            j = digest.unpack1('Q>') % (i + 1)
            order[i], order[j] = order[j], order[i]
        end
        order
    end

    # { id => order } for the choice tasks on a page that are shuffled
    # (all, unless check.shuffle is false). tasks: { id => task }.
    def self.option_orders(person, activity, ids, tasks)
        ids.each_with_object({}) do |id, orders|
            check = (tasks[id] || {})['check'] || {}
            next unless check['type'] == 'choice' && check['shuffle'] != false

            orders[id] = option_order(person, "#{activity}/#{id}", Array(check['options']).size)
        end
    end

    # Shown where _task(id) names a task that tasks.yaml does not have.
    def self.missing_html(id)
        "<div class='task-missing'>Aufgabe »#{CGI.escapeHTML(id.to_s)}« fehlt in tasks.yaml.</div>"
    end

    # Option labels are rendered as Markdown but must stay inline.
    def self.inline(html)
        html.to_s.strip.sub(/\A<p>(.*)<\/p>\z/m, '\1')
    end
end
