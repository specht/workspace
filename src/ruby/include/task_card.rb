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
            when 'function'
                # A Kata: the code is written here; it starts with the task's starter.
                io << "<textarea class='form-control task-input task-code' name='answer' rows='6' spellcheck='false' autocapitalize='off'"
                io << " data-code-language='#{h.(check['language'])}' data-starter='#{h.(task['starter'])}' aria-labelledby='#{h.(field)}-title'>"
                io << "#{h.(task['starter'])}</textarea>"
                reads = "Dein Code läuft in einem eigenen kleinen Rechner, getrennt von deinem Workspace und ohne Internet, höchstens #{(check['timeout'] || 10).to_i} Sekunden. " \
                    "<em>Ausführen</em> startet ihn einmal und zeigt, was er ausgibt, <em>Prüfen</em> ruft deine Funktionen auf und vergleicht. <kbd>Strg</kbd>+<kbd>Enter</kbd> prüft."
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
                else 'Prüfen'
            end
            io << "<div class='task-actions'>"
            io << "<button type='button' class='btn btn-outline-success task-run'>Ausführen</button>" if type == 'function'
            io << "<button type='submit' class='btn btn-success task-submit'>#{label}</button>"
            io << "<button type='button' class='btn btn-link btn-sm task-delete' hidden>Meine Antwort löschen</button>"
            # What the check reads, for whoever wants to know (rule 5), behind
            # a small (i) so the card itself stays short.
            if reads
                io << "<button type='button' class='task-reads-toggle' aria-expanded='false' aria-controls='#{h.(field)}-reads' title='Was passiert beim Prüfen?'>"
                io << "<i class='bi bi-info-circle' aria-hidden='true'></i><span class='visually-hidden'>Was passiert beim Prüfen?</span></button>"
            end
            io << "<span class='task-queue' aria-live='polite'></span>" if type == 'function'
            io << '</div>'
            io << "<div class='task-reads' id='#{h.(field)}-reads' hidden>#{reads}</div>" if reads
            io << "<pre class='task-console no-copy' hidden aria-live='polite' aria-label='Ausgabe'></pre>" if type == 'function'
            io << '</form>'
        else
            io << "<p class='task-later'><i class='bi bi-hourglass-split'></i>Diese Aufgabe kann der Workspace noch nicht selbst prüfen. Zeig dein Ergebnis deiner Lehrkraft.</p>"
        end

        if task['hint'].is_a?(Array)
            io << HintSteps.html(task['hint'].map { |hint| render.(hint) }, :extra_class => 'task-hint')
        elsif task['hint']
            io << "<details class='task-hint'><summary>Tipp</summary>#{render.(task['hint'])}</details>"
        end
        io << "<div class='task-feedback' aria-live='polite'></div>"
        io << "<div class='task-login-note'><a href='/login'>Melde dich an</a>, damit der Workspace deine Antwort prüfen und speichern kann.</div>" if runs
        io << '</section>'
        io
    end

    # The level of a Kata (TaskSpec::LEVELS) as one to three dots; the name
    # for those who cannot see them.
    LEVEL_NAMES = { 1 => 'Einstieg', 2 => 'Übung', 3 => 'Knobelei' }.freeze

    def self.level_html(level)
        name = LEVEL_NAMES.fetch(level)
        dots = Array.new(level) { "<i class='bi bi-circle-fill' aria-hidden='true'></i>" }.join
        "<span class='task-level' title='#{name}'>#{dots}<span class='visually-hidden'>#{name}</span></span>"
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
        when 'schema' then 'Liest den Aufbau deiner MySQL-Datenbank, angemeldet als du, und ändert nichts'
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
