require 'minitest/autorun'
require 'cgi'
require_relative '../include/task_card'

class TaskCardTest < Minitest::Test
    # Stand-in for Redcarpet: escapes and turns `code` into <code>.
    RENDER = lambda do |md|
        "<p>#{CGI.escapeHTML(md.to_s.strip).gsub(/`([^`]+)`/, '<code>\1</code>')}</p>"
    end

    def tasks
        @tasks ||= TaskSpec.load_file(File.expand_path('../../content/tcpip/tasks.yaml', __dir__))['tasks']
            .to_h { |t| [t['id'], t] }
    end

    def card(id, number = 1)
        TaskCard.html(tasks[id], activity: 'tcpip', number: number, render: RENDER)
    end

    def test_mark_slots
        md = "Text\n\n_task(adresse)\n\nMehr\n  _task( loopback )  \nim Satz _task(nein) bleibt\n"
        out, ids = TaskCard.mark_slots(md)
        assert_equal %w[adresse loopback], ids
        assert_includes out, "<div class='task-slot' data-slot='0'></div>"
        assert_includes out, "<div class='task-slot' data-slot='1'></div>"
        assert_includes out, 'im Satz _task(nein) bleibt'
        fenced, ids = TaskCard.mark_slots("```markdown\n_task(beispiel)\n```\n\n_task(echt)\n")
        assert_equal %w[echt], ids
        assert_includes fenced, "```markdown\n_task(beispiel)\n```"
    end

    def test_answer_card
        html = card('adresse', 3)
        assert_includes html, "data-task='tcpip/adresse'"
        assert_includes html, "id='aufgabe-adresse'"
        assert_includes html, 'Aufgabe 3'
        assert_includes html, "placeholder='z. B. 172.17.0.42'"
        assert_includes html, '<details class=\'task-hint\'><summary>Tipp</summary>'
        assert_includes html, 'Prüfen'
        assert_includes html, 'task-login-note'
    end

    def test_cards_never_contain_what_is_checked
        tasks.each_value do |task|
            html = TaskCard.html(task, activity: 'tcpip', number: 1, render: RENDER)
            refute_includes html, 'workspace.ip'
            refute_includes html, 'person.codeword'
            if task['solution']
                refute_includes html, CGI.escapeHTML(task['solution'].strip[0, 30])
            end
        end
        refute_includes card('lichtgeschwindigkeit'), '111'
    end

    def test_choice_card_and_prediction
        html = card('ttl-drei', 2)
        assert_includes html, 'Vermutung 2'
        assert_includes html, 'Vermutung abgeben'
        assert_equal 4, html.scan("type='radio'").size
        assert_includes html, "name='task-tcpip-ttl-drei'"
        assert_includes html, "value='3'"
        refute_includes html, '<label class=\'form-check-label\' for=\'task-tcpip-ttl-drei-0\'><p>'
    end

    def test_multiple_choice_uses_checkboxes
        task = { 'id' => 'm', 'title' => 'M', 'prompt' => 'P',
                 'check' => { 'type' => 'choice', 'options' => %w[a b c], 'correct' => [0, 1] } }
        html = TaskCard.html(task, activity: 'x', number: 1, render: RENDER)
        assert_equal 3, html.scan("type='checkbox'").size
        assert_includes html, 'Mehrere Antworten'
    end

    def test_number_and_reflection
        assert_includes card('lichtgeschwindigkeit'), "<span class='input-group-text'>ms</span>"
        assert_includes card('lichtgeschwindigkeit'), "inputmode='decimal'"
        html = card('traceroute-erklaeren')
        assert_includes html, '<textarea'
        assert_includes html, 'Mindestens 20 Wörter'
        assert_includes html, 'Speichern'
    end

    def test_later_types_have_no_form
        task = TaskSpec.load_file(File.expand_path('../../content/anaglyph/tasks.yaml', __dir__))['tasks'].find { |t| t['check']['type'] == 'scene' }
        html = TaskCard.html(task, activity: 'anaglyph', number: 1, render: RENDER)
        refute_includes html, '<form'
        assert_includes html, 'noch nicht selbst prüfen'
        refute_includes html, 'task-login-note'
    end

    def test_measure_and_command_cards
        html = card('entfernungen')
        assert_equal 3, html.scan("class='form-control task-measure-input'").size
        assert_includes html, ">rubiomonocoat.pe</label>"
        assert_includes html, "<span class='input-group-text'>ms</span>"
        refute_includes html, 'ping -c'
        html = card('katzenfakt')
        assert_includes html, 'task-command'
        assert_includes html, 'höchstens 10 Sekunden'
        assert_includes html, 'Ausführen und prüfen'
        refute_includes html, 'jq -r'
    end

    def test_workspace_cards_name_the_files_they_read
        html = card('spiel-2048')
        assert_includes html, 'Sieht nach in <code>/workspace/2048</code>'
        assert_includes html, 'Prüfen'
        refute_includes html, 'task-input'
        shower = TaskSpec.load_file(File.expand_path('../../content/showerjs/tasks.yaml', __dir__))['tasks'].to_h { |t| [t['id'], t] }
        card = ->(id) { TaskCard.html(shower[id], activity: 'showerjs', number: 1, render: RENDER) }
        assert_includes card.('eigene-regel'), 'Liest <code>/workspace/shower.js/styles.css</code>'
        assert_includes card.('alles-im-ordner'), '<code>/workspace/shower.js/index.html</code> und <code>/workspace/shower.js/styles.css</code>'
        abgabe = card.('abgabe')
        assert_includes abgabe, 'Kopiert <code>/workspace/shower.js</code> ohne <code>.git</code>, höchstens 20 MB'
        assert_includes abgabe, 'Abgeben'
        anaglyph = TaskSpec.load_file(File.expand_path('../../content/anaglyph/tasks.yaml', __dir__))['tasks'].to_h { |t| [t['id'], t] }
        galerie = TaskCard.html(anaglyph['galerie'], activity: 'anaglyph', number: 1, render: RENDER)
        assert_includes galerie, "data-pick='anaglyph/*.ini'"
        assert_includes galerie, '<select'
    end

    def test_every_task_on_a_page_exists
        root = File.expand_path('../../content', __dir__)
        Dir[File.join(root, '**/*.md')].each do |page|
            _, ids = TaskCard.mark_slots(File.read(page, encoding: 'UTF-8'))
            next if ids.empty?
            tasks_path = File.join(File.dirname(page), 'tasks.yaml')
            assert File.file?(tasks_path), "#{page} uses _task() but has no tasks.yaml"
            known = TaskSpec.load_file(tasks_path)['tasks'].map { |t| t['id'] }
            ids.each { |id| assert_includes known, id, "#{page}: _task(#{id})" }
            assert_equal ids.uniq, ids, "#{page} places a task twice"
        end
    end

    def test_text_is_escaped
        task = { 'id' => 'e', 'title' => '<script>', 'prompt' => 'P', 'check' => { 'type' => 'answer', 'expect' => 'x', 'placeholder' => "a'b" } }
        html = TaskCard.html(task, activity: 'x', number: 1, render: RENDER)
        refute_includes html, '<script>'
        assert_includes html, "placeholder='a&#39;b'"
    end

    def test_code_card_links_to_the_editor
        task = TaskSpec.load_file(File.expand_path('../../content/uebung-arrays/tasks.yaml', __dir__))['tasks'].first
        html = TaskCard.html(task, activity: 'uebung-arrays', number: 1, render: RENDER)
        assert_includes html, "href='/codebites/array_sum'"
        assert_includes html, 'Im Code-Editor lösen'
        assert_includes html, 'Stand holen'
        assert_includes html, 'Code Bites: <code>array_sum</code>, in Ruby, Python, JavaScript'
    end

    # docs/vorlage-aktivitaet is what authors copy; it must stay valid.
    def test_the_template_for_authors_is_valid
        dir = File.expand_path('../../../docs/vorlage-aktivitaet', __dir__)
        tasks = TaskSpec.load_file(File.join(dir, 'tasks.yaml'))['tasks'].map { |t| t['id'] }
        _, ids = TaskCard.mark_slots(File.read(File.join(dir, 'vorlage-aktivitaet.md'), encoding: 'UTF-8'))
        assert_equal tasks, ids
        assert File.file?(File.join(dir, 'lehrkraft.md'))
    end
end
