require 'cgi'
require 'digest'
require_relative 'task_check'

# HTML of a task card on an activity page (docs/aufgaben.md, »Aufgaben auf
# der Seite«). The card never contains what is checked against (expect,
# correct, solution); tasks.js sends the answer to /api/task/check.
module TaskCard
    # _task(id) on a line of its own.
    PLACEHOLDER = /^[ \t]*_task\(\s*([a-z0-9][a-z0-9-]*)\s*\)[ \t]*$/

    # Replaces each _task(id) line by a numbered slot marker. Returns the new
    # Markdown and the list of ids in page order. Slot markers use numbers
    # only, so the hyphenation pass cannot change them.
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
    def self.html(task, activity:, number:, render:)
        h = ->(s) { CGI.escapeHTML(s.to_s) }
        check = task['check']
        type = check['type']
        key = "#{activity}/#{task['id']}"
        field = "task-#{activity}-#{task['id']}"
        runs = TaskCheck::RUNS.include?(type)

        io = +''
        io << "<section class='task-card' id='aufgabe-#{h.(task['id'])}' data-task='#{h.(key)}' data-type='#{h.(type)}'"
        io << " data-prediction='1'" if task['prediction']
        io << " aria-labelledby='#{h.(field)}-title'>"
        io << "<div class='task-head'>"
        io << "<span class='task-number'>#{task['prediction'] ? 'Vermutung' : 'Aufgabe'} #{number.to_i}</span>"
        io << "<span class='task-title' id='#{h.(field)}-title'>#{h.(task['title'])}</span>"
        io << "<span class='task-status' aria-live='polite'></span>"
        io << '</div>'
        io << "<div class='task-prompt'>#{render.(task['prompt'])}</div>"

        if runs
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
                io << "<div class='task-reads'>Der Workspace misst danach selbst nach, in deinem Workspace, und vergleicht.</div>"
            when 'sql'
                io << "<textarea class='form-control task-input task-sql' name='answer' rows='4' spellcheck='false' autocapitalize='off' placeholder='SELECT …' aria-labelledby='#{h.(field)}-title'></textarea>"
                io << "<div class='task-reads'>Läuft in deiner eigenen MySQL-Datenbank, angemeldet als du, und liest nur: eine Abfrage mit <code>SELECT</code>, höchstens #{SqlCheck::MAX_MS / 1000} Sekunden. <kbd>Strg</kbd>+<kbd>Enter</kbd> prüft.</div>"
            when 'command'
                io << "<input type='text' class='form-control task-input task-command' name='answer' spellcheck='false' autocomplete='off' placeholder='Befehl' aria-labelledby='#{h.(field)}-title'>"
                io << "<div class='task-reads'>Läuft in deinem Workspace in <code>/workspace</code>, höchstens #{(check['timeout'] || 10).to_i} Sekunden, als Gast: Der Befehl kann lesen, aber nichts verändern oder löschen.</div>"
            when 'submit'
                if check['pick']
                    io << "<label class='task-pick-label' for='#{h.(field)}-pick'>Welche Datei gibst du ab?</label>"
                    io << "<select class='form-select task-input task-pick' id='#{h.(field)}-pick' name='answer' data-pick='#{h.(check['pick'])}'>"
                    io << "<option value=''>Datei aus #{h.(File.dirname(check['pick']))}/ wählen …</option></select>"
                end
                io << "<div class='task-reads'>#{reads_line(check, h)}</div>"
            else
                io << "<div class='task-reads'>#{reads_line(check, h)}</div>"
            end
            label = case
                when type == 'code' then 'Stand holen'
                when type == 'command' || type == 'sql' then 'Ausführen und prüfen'
                when type == 'reflection' then 'Speichern'
                when type == 'submit' then 'Abgeben'
                when task['prediction'] then 'Vermutung abgeben'
                else 'Prüfen'
            end
            io << "<div class='task-actions'>"
            if type == 'code'
                io << "<a class='btn btn-success task-open' href='/codebites/#{h.(check['codebite'])}'><i class='bi bi-code-slash'></i>Im Code-Editor lösen</a>"
                io << "<button type='submit' class='btn btn-outline-secondary btn-sm task-submit'>#{label}</button>"
            else
                io << "<button type='submit' class='btn btn-success task-submit'><i class='bi bi-check2'></i>#{label}</button>"
            end
            io << "<button type='button' class='btn btn-link btn-sm task-delete' hidden>Meine Antwort löschen</button>"
            io << '</div>'
            io << '</form>'
        else
            io << "<p class='task-later'><i class='bi bi-hourglass-split'></i>Diese Aufgabe kann der Workspace noch nicht selbst prüfen. Zeig dein Ergebnis deiner Lehrkraft.</p>"
        end

        if task['hint']
            io << "<details class='task-hint'><summary>Tipp</summary>#{render.(task['hint'])}</details>"
        end
        io << "<div class='task-feedback' aria-live='polite'></div>"
        io << "<div class='task-login-note'><a href='/login'>Melde dich an</a>, damit der Workspace deine Antwort prüfen und speichern kann.</div>" if runs
        io << '</section>'
        io
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
        when 'schema' then 'Liest den Aufbau deiner MySQL-Datenbank, angemeldet als du, und ändert nichts'
        when 'code'
            langs = (check['languages'] || %w[ruby python javascript]).map { |l| { 'ruby' => 'Ruby', 'python' => 'Python', 'javascript' => 'JavaScript' }[l] || l }
            "Code Bites: <code>#{h.(check['codebite'])}</code>, in #{h.(langs.join(', '))}. Ist sie gelöst, wird sie hier von selbst abgehakt."
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
