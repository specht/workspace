require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/task_check'

class TaskCheckTest < Minitest::Test
    def tcpip
        @tcpip ||= TaskSpec.load_file(File.expand_path('../../content/tcpip/tasks.yaml', __dir__))['tasks']
            .to_h { |t| [t['id'], t] }
    end

    def showerjs
        @showerjs ||= TaskSpec.load_file(File.expand_path('../../content/showerjs/tasks.yaml', __dir__))['tasks']
            .to_h { |t| [t['id'], t] }
    end

    def test_answer_with_workspace_ip
        task = tcpip['adresse']
        vars = { 'workspace.ip' => '172.17.0.42' }
        assert_equal :correct, TaskCheck.check(task, ' 172.17.0.42 ', variables: vars).status
        assert_equal :correct, TaskCheck.check(task, '`172.17.0.42`', variables: vars).status
        wrong = TaskCheck.check(task, '172.17.0.41', variables: vars)
        assert_equal :wrong, wrong.status
        assert_equal 'Das stimmt noch nicht.', wrong.message
        lo = TaskCheck.check(task, '127.0.0.1', variables: vars)
        assert_match(/Adresse von `lo`/, lo.message)
    end

    def test_missing_variable_is_unavailable_and_does_not_count
        result = TaskCheck.check(tcpip['adresse'], '172.17.0.42', variables: {})
        assert_equal :unavailable, result.status
        assert_match(/Starte zuerst deinen Workspace/, result.message)
        refute result.counts?
    end

    # ${name} that is not one of TaskSpec::VARIABLES is text: a JavaScript
    # template literal in a choice (kata-functions, predict-badge).
    def test_template_literal_is_no_variable
        task = { 'id' => 'x', 'check' => { 'type' => 'choice', 'options' => ['Bello', '${name} ist ${age}'], 'correct' => 1 } }
        assert_empty TaskCheck.variables_used(task['check'])
        assert_equal '${name} ist ${age}', TaskCheck.fill('${name} ist ${age}', {})
        assert_equal :correct, TaskCheck.check(task, '1').status
    end

    def test_answer_text_ignores_case_unless_asked
        task = { 'id' => 'x', 'check' => { 'type' => 'answer', 'expect' => ['Hallo Welt', 'Moin'] } }
        assert_equal :correct, TaskCheck.check(task, 'hallo   WELT', variables: {}).status
        assert_equal :correct, TaskCheck.check(task, 'moin', variables: {}).status
        task['check']['case_sensitive'] = true
        assert_equal :wrong, TaskCheck.check(task, 'hallo welt', variables: {}).status
    end

    def test_answer_url
        task = showerjs['adresse']
        vars = { 'workspace.live_url' => 'https://abc123-5500.code.hackschule.de' }
        %w[
            https://abc123-5500.code.hackschule.de/pictures/fuji.jpg
            http://ABC123-5500.code.hackschule.de/pictures/fuji.jpg
            abc123-5500.code.hackschule.de/pictures/fuji.jpg
        ].each do |given|
            assert_equal :correct, TaskCheck.check(task, given, variables: vars).status, given
        end
        assert_equal :wrong, TaskCheck.check(task, 'https://abc123-5500.code.hackschule.de/pictures/Fuji.jpg', variables: vars).status
        assert_equal :wrong, TaskCheck.check(task, 'https://abc123-5500.code.hackschule.de/', variables: vars).status
        assert_equal :wrong, TaskCheck.check(task, 'kein link', variables: vars).status
    end

    def test_answer_regex
        task = { 'id' => 'x', 'check' => { 'type' => 'answer', 'compare' => 'regex', 'expect' => '^ls\s+-l' } }
        assert_equal :correct, TaskCheck.check(task, 'ls  -la', variables: {}).status
        assert_equal :wrong, TaskCheck.check(task, 'ls', variables: {}).status
    end

    def test_number_with_tolerance_and_feedback
        task = tcpip['lichtgeschwindigkeit']
        ['111', '111 ms', 'etwa 108', '116,9', '111.0'].each do |given|
            result = TaskCheck.check(task, given, variables: {})
            assert_equal :correct, result.status, given
            assert_match(/111 ms/, result.solution)
        end
        one_way = TaskCheck.check(task, '55,5', variables: {})
        assert_equal :wrong, one_way.status
        assert_match(/nur eine Richtung/, one_way.message)
        seconds = TaskCheck.check(task, '0,111', variables: {})
        assert_match(/Sekunden/, seconds.message)
        assert_equal :invalid, TaskCheck.check(task, 'keine Ahnung', variables: {}).status
        refute TaskCheck.check(task, 'keine Ahnung', variables: {}).counts?
    end

    # A German thousands dot: 57.596 bytes is 57596, as the pages write it;
    # 0.111 stays a decimal, and a comma is always the decimal comma.
    def test_number_with_a_thousands_dot
        task = { 'id' => 'x', 'check' => { 'type' => 'number', 'expect' => 57596 } }
        ['57596', '57.596', 'Es sind 57.596 Bytes', '57 596'].each do |given|
            assert_equal :correct, TaskCheck.check(task, given, variables: {}).status, given
        end
        ['57,596', '57.6'].each do |given|
            assert_equal :wrong, TaskCheck.check(task, given, variables: {}).status, given
        end
        small = { 'id' => 'y', 'check' => { 'type' => 'number', 'expect' => 0.111 } }
        assert_equal :correct, TaskCheck.check(small, '0.111', variables: {}).status
    end

    # A command that finds the number is not the number: the check says to
    # run it, and it does not count as a wrong answer
    def test_a_command_instead_of_the_number
        task = { 'id' => 'x', 'check' => { 'type' => 'number', 'expect' => 181 } }
        ['grep -i hinweis tatort.txt | wc -l', 'grep -ic hinweis tatort.txt', 'head -n 5 tatort.txt | wc -l', 'wc -l tatort.txt', 'echo 181'].each do |given|
            result = TaskCheck.check(task, given, variables: {})
            assert_equal :invalid, result.status, given
            assert_match(/Führe ihn im Terminal aus/, result.message)
            refute result.counts?
        end
        ['181', '181 Zeilen', 'es sind 181'].each do |given|
            assert_equal :correct, TaskCheck.check(task, given, variables: {}).status, given
        end
        # a single word is no command: still »keine Zahl«
        assert_match(/keine Zahl/, TaskCheck.check(task, 'grep', variables: {}).message)
    end

    def test_parse_number
        {
            '111' => 111.0, '0,111' => 0.111, '0.111' => 0.111, '11 100 km' => 11100.0,
            "11 100" => 11100.0, '1.234,5' => 1234.5, '1,234.5' => 1234.5, '-3' => -3.0,
            "−3,5" => -3.5, 'ca. 42 ms' => 42.0, 'nichts' => nil,
        }.each do |text, value|
            if value.nil?
                assert_nil TaskCheck.parse_number(text), text.inspect
            else
                assert_equal value, TaskCheck.parse_number(text), text.inspect
            end
        end
    end

    def test_choice_single_and_feedback
        task = tcpip['ttl-drei']
        assert_equal :correct, TaskCheck.check(task, '1', variables: {}).status
        wrong = TaskCheck.check(task, '0', variables: {})
        assert_equal :wrong, wrong.status
        assert_match(/\ADeine Vermutung stimmt nicht\. .*nichts mit Zeit/, wrong.message)
        other = TaskCheck.check(task, '3', variables: {})
        assert_equal 'Deine Vermutung stimmt nicht. Probier es aus und sieh nach, was wirklich passiert.', other.message
        assert_equal :invalid, TaskCheck.check(task, '7', variables: {}).status
        assert_equal :invalid, TaskCheck.check(task, 'zwei', variables: {}).status
        assert_equal :invalid, TaskCheck.check(task, '', variables: {}).status
    end

    def test_choice_multiple
        task = { 'id' => 'x', 'check' => { 'type' => 'choice', 'options' => %w[a b c], 'correct' => [0, 2] } }
        assert_equal :correct, TaskCheck.check(task, '2,0', variables: {}).status
        result = TaskCheck.check(task, '0', variables: {})
        assert_equal :wrong, result.status
        assert_match(/Mehrere Antworten/, result.message)
    end

    def test_reflection
        task = tcpip['traceroute-erklaeren']
        short = TaskCheck.check(task, 'traceroute schickt Pakete.', variables: {})
        assert_equal :invalid, short.status
        assert_match(/mindestens 20 Wörter \(bisher 3\)/, short.message)
        long = TaskCheck.check(task, (['Wort'] * 20).join(' '), variables: {})
        assert_equal :sent, long.status
        assert long.counts?
    end

    def test_types_from_later_milestones_are_unavailable
        scene = TaskSpec.load_file(File.expand_path('../../content/anaglyph/tasks.yaml', __dir__))['tasks'].find { |t| t['check']['type'] == 'scene' }
        result = TaskCheck.check(scene, 'x', variables: {})
        assert_equal :unavailable, result.status
        refute result.counts?
    end

    def test_workspace_checks_need_a_workspace
        result = TaskCheck.check(tcpip['spiel-2048'], '', variables: {})
        assert_equal :unavailable, result.status
        assert_equal TaskCheck::NO_WORKSPACE, result.message
    end

    def with_workspace
        Dir.mktmpdir do |tmp|
            FileUtils.mkdir_p(File.join(tmp, 'shower.js'))
            File.write(File.join(tmp, 'shower.js', 'index.html'), "<section class='slide'><h2>A</h2></section>")
            yield tmp, WorkspaceFiles.new(tmp)
        end
    end

    def test_file_check_results_and_feedback_by_problem
        with_workspace do |tmp, ws|
            result = TaskCheck.check(showerjs['vorlage'], '', variables: {}, workspace: ws)
            assert_equal :correct, result.status
            assert_includes result.read, 'index.html'
            task = tcpip['spiel-2048']
            result = TaskCheck.check(task, '', variables: {}, workspace: ws)
            assert_equal :wrong, result.status
            assert_equal :missing, result.problem
            task = task.merge('feedback' => [{ 'when' => { 'problem' => 'missing' }, 'say' => 'Erst übersetzen.' }])
            assert_equal 'Erst übersetzen.', TaskCheck.check(task, '', variables: {}, workspace: ws).message
        end
    end

    def test_git_check_uses_the_state_from_the_container
        task = { 'id' => 'push', 'check' => { 'type' => 'git', 'repo' => 'sandbox', 'expect' => { 'pushed' => true } } }
        with_workspace do |tmp, ws|
            # Missing folder: no need to ask the container.
            result = TaskCheck.check(task, '', variables: {}, workspace: ws, git: ->(_) { flunk 'not asked' })
            assert_equal :no_repo, result.problem
            FileUtils.mkdir_p(File.join(tmp, 'sandbox'))
            assert_equal :unavailable, TaskCheck.check(task, '', variables: {}, workspace: ws, git: ->(_) { nil }).status
            state = GitCheck.parse("COUNT 2\nREMOTE origin\nSTATUS ## main...origin/main [ahead 1]\n")
            result = TaskCheck.check(task, '', variables: {}, workspace: ws, git: ->(repo) { assert_equal 'sandbox', repo; state })
            assert_equal :not_pushed, result.problem
            assert_includes result.read, '1 noch nicht gepusht'
        end
    end

    def test_git_identity_asks_the_container_about_the_workspace
        task = { 'id' => 'git-name', 'check' => { 'type' => 'git', 'expect' => { 'identity' => true } },
                 'feedback' => [{ 'when' => { 'problem' => 'placeholder' }, 'say' => 'Abgeschrieben.' }] }
        with_workspace do |_tmp, ws|
            state = GitCheck.parse("NAME Vorname Nachname\nEMAIL lena@example.org\nNOGIT\n")
            result = TaskCheck.check(task, '', variables: {}, workspace: ws, git: ->(repo) { assert_equal '', repo; state })
            assert_equal :placeholder, result.problem
            assert_includes result.message, 'Abgeschrieben.'
            assert_includes result.read, '`lena@example.org`'
            state = GitCheck.parse("NAME Lena Muster\nEMAIL lena@example.org\nNOGIT\n")
            assert_equal :correct, TaskCheck.check(task, '', variables: {}, workspace: ws, git: ->(_) { state }).status
        end
    end

    def test_submit_plans_and_does_not_count_refusals
        with_workspace do |tmp, ws|
            result = TaskCheck.check(showerjs['abgabe'], '', variables: {}, workspace: ws)
            assert_equal :sent, result.status
            assert_equal ['shower.js/index.html'], result.plan.files.map { |f| f[:path] }
            assert_includes result.read, '1 Datei'
            File.write(File.join(tmp, 'shower.js', 'index.html'), "<section>\n<div>\n</section>")
            result = TaskCheck.check(showerjs['abgabe'], '', variables: {}, workspace: ws)
            assert_equal :invalid, result.status
            refute result.counts?
        end
    end

    def test_codeword_is_stable_and_personal
        a = TaskCheck.codeword('salt', 'a@example.org', 'tcpip/codewort')
        assert_match(/\A[A-ZÄÖÜ][a-zäöüß]+-\d\d\z/, a)
        assert_equal a, TaskCheck.codeword('salt', 'a@example.org', 'tcpip/codewort')
        others = %w[b c d e f g].map { |x| TaskCheck.codeword('salt', "#{x}@example.org", 'tcpip/codewort') }
        refute others.all? { |o| o == a }
        task = tcpip['codewort']
        assert_equal :correct, TaskCheck.check(task, a.downcase, variables: { 'person.codeword' => a }).status
    end

    def test_every_pilot_task_can_be_checked_without_crashing
        %w[tcpip anaglyph showerjs].each do |activity|
            tasks = TaskSpec.load_file(File.expand_path("../../content/#{activity}/tasks.yaml", __dir__))['tasks']
            vars = { 'workspace.ip' => '172.17.0.2', 'workspace.live_url' => 'https://x-5500.example.org', 'person.codeword' => 'Otter-42' }
            tasks.each do |task|
                ['', 'x', '1', '0,1', 'eins zwei drei'].each do |answer|
                    result = TaskCheck.check(task, answer, variables: vars)
                    assert_includes %i[correct wrong sent invalid unavailable], result.status, "#{activity}/#{task['id']}"
                end
            end
        end
    end

    def test_er_reads_the_model_in_the_workspace
        skip 'node is not installed' unless system('node --version > /dev/null 2>&1')
        task = { 'id' => 'modell', 'check' => { 'type' => 'er', 'file' => 'film/film.er',
            'entities' => [{ 'name' => 'movie', 'key' => ['id'] }] } }
        Dir.mktmpdir do |tmp|
            ws = WorkspaceFiles.new(tmp)
            runner = lambda do |command, _seconds|
                out = IO.popen(['sh', '-c', command], :chdir => tmp, &:read)
                { :output => out, :status => 0, :timed_out => false }
            end
            assert_equal :unavailable, TaskCheck.check(task, '', workspace: nil).status
            missing = TaskCheck.check(task, '', workspace: ws, runner: runner)
            assert_equal [:wrong, :missing], [missing.status, missing.problem]
            FileUtils.mkdir_p(File.join(tmp, 'film'))
            File.write(File.join(tmp, 'film/film.er'), "entity movie\n  id: int, key\n  title: text\n")
            assert_equal TaskCheck::NOT_RUNNING_ER, TaskCheck.check(task, '', workspace: ws, runner: nil).message
            result = TaskCheck.check(task, '', workspace: ws, runner: runner)
            assert_equal :correct, result.status, result.message
            assert_includes result.read, '`movie`: **id**, title'
        end
    end
end
