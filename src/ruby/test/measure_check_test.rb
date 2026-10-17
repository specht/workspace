require 'minitest/autorun'
require_relative '../include/task_check'

class MeasureCheckTest < Minitest::Test
    PING_LOCAL = <<~OUT
        PING 127.0.0.1 (127.0.0.1) 56(84) bytes of data.

        --- 127.0.0.1 ping statistics ---
        3 packets transmitted, 3 received, 0% packet loss, time 2041ms
        rtt min/avg/max/mdev = 0.031/0.046/0.059/0.011 ms
    OUT
    PING_PE = <<~OUT
        PING rubiomonocoat.pe (185.182.56.12) 56(84) bytes of data.
        64 bytes from linweb443.webhosting.be (185.182.56.12): icmp_seq=1 ttl=52 time=14.5 ms
        64 bytes from linweb443.webhosting.be (185.182.56.12): icmp_seq=2 ttl=52 time=14.3 ms

        --- rubiomonocoat.pe ping statistics ---
        rtt min/avg/max/mdev = 14.301/14.452/14.610/0.126 ms
    OUT
    PING_GOOGLE = "rtt min/avg/max/mdev = 9.871/10.240/10.700/0.300 ms\n"
    CURL = "HTTP/2 200 \r\nserver: nginx/1.24.0\r\ncontent-type: text/html\r\n"

    def tasks
        @tasks ||= TaskSpec.load_file(File.expand_path('../../content/tcpip/tasks.yaml', __dir__))['tasks'].to_h { |t| [t['id'], t] }
    end

    def runner(outputs)
        ->(command, _timeout) { { :output => outputs.find { |k, _| command.include?(k) }&.last.to_s, :status => 0 } }
    end

    def test_extractors
        assert_in_delta 0.046, MeasureCheck.extract('ping_rtt_avg', PING_LOCAL), 1e-9
        assert_in_delta 14.452, MeasureCheck.extract('ping_rtt_avg', PING_PE), 1e-9
        assert_equal 12, MeasureCheck.extract('ping_hops', PING_PE)
        assert_equal 'Belgien', MeasureCheck.extract('ping_reply_country', PING_PE)
        # A hoster's generic name counts as the hoster's country.
        combell = "64 bytes from 188.208.37.117.static.hosted.by.combell-ops.net (188.208.37.117): icmp_seq=1 ttl=51 time=25.3 ms\n"
        assert_equal 'Belgien', MeasureCheck.extract('ping_reply_country', combell)
        assert_equal 'nginx/1.24.0', MeasureCheck.extract('http_header:Server', CURL)
        assert_equal 3, MeasureCheck.extract('traceroute_hops', "traceroute to x\n 1  a  1 ms\n 2  b  2 ms\n 3  c  3 ms\n")
        assert_nil MeasureCheck.extract('ping_rtt_avg', 'ping: unknown host')
    end

    def test_three_distances
        task = tasks['entfernungen']
        run = runner('127.0.0.1' => PING_LOCAL, '8.8.8.8' => PING_GOOGLE, 'rubiomonocoat' => PING_PE)
        ok = TaskCheck.check(task, '["0,05", "12", "15 ms"]', variables: {}, runner: run)
        assert_equal :correct, ok.status, ok.message
        assert_includes ok.read, '`ping -c 3 -q 8.8.8.8` ergibt gerade **10,2 ms**'
        wrong = TaskCheck.check(task, '["0,05", "40", "15"]', variables: {}, runner: run)
        assert_equal :wrong, wrong.status
        assert_includes wrong.message, '8.8.8.8: Du hast 40 ms eingetragen, meine Messung ergibt gerade 10,2 ms.'
        assert_equal :invalid, TaskCheck.check(task, '["1", "", "2"]', variables: {}, runner: run).status
        assert_equal :unavailable, TaskCheck.check(task, '["1", "2", "3"]', variables: {}, runner: ->(_, _) { nil }).status
        assert_equal :unavailable, TaskCheck.check(task, '["1", "2", "3"]', variables: {}).status
    end

    def test_country_hops_and_header
        run = runner('rubiomonocoat' => PING_PE, 'curl' => CURL)
        assert_equal :correct, TaskCheck.check(tasks['standort'], 'belgien', variables: {}, runner: run).status
        assert_equal :correct, TaskCheck.check(tasks['standort'], 'Belgium', variables: {}, runner: run).status
        peru = TaskCheck.check(tasks['standort'], 'Peru', variables: {}, runner: run)
        assert_equal :wrong, peru.status
        assert_includes peru.message, 'meine Messung ergibt gerade Belgien'
        assert_equal :correct, TaskCheck.check(tasks['router-zaehlen'], '13', variables: {}, runner: run).status
        assert_equal :wrong, TaskCheck.check(tasks['router-zaehlen'], '20', variables: {}, runner: run).status
        assert_equal :correct, TaskCheck.check(tasks['webserver'], 'nginx', variables: {}, runner: run).status
        assert_equal :correct, TaskCheck.check(tasks['webserver'], 'nginx/1.24.0', variables: {}, runner: run).status
        assert_equal :wrong, TaskCheck.check(tasks['webserver'], 'Apache', variables: {}, runner: run).status
    end

    def test_command
        task = tasks['katzenfakt']
        out = lambda { |text| ->(_command, _timeout) { { :output => text, :status => 0 } } }
        ok = TaskCheck.check(task, 'curl -s https://catfact.ninja/fact | jq -r .fact', variables: {}, guest_runner: out.("Cats sleep a lot.\n"))
        assert_equal :correct, ok.status
        assert_includes ok.read, 'Cats sleep a lot.'
        assert_match(/Anführungszeichen/, TaskCheck.check(task, 'x', variables: {}, guest_runner: out.("\"Cats sleep.\"\n")).message)
        assert_match(/noch JSON/, TaskCheck.check(task, 'x', variables: {}, guest_runner: out.("{\"fact\": \"x\"}\n")).message)
        assert_match(/2 Zeilen/, TaskCheck.check(task, 'x', variables: {}, guest_runner: out.("a\nb\n")).message)
        timed = TaskCheck.check(task, 'sleep 99', variables: {}, guest_runner: ->(_, _) { { :output => '', :timed_out => true } })
        assert_match(/länger als 10 Sekunden/, timed.message)
        assert_equal :invalid, TaskCheck.check(task, '', variables: {}, guest_runner: out.('')).status
        assert_equal :invalid, TaskCheck.check(task, "ls\nrm x", variables: {}, guest_runner: out.('')).status
        # A typed command never runs as the person: without a guest runner it does not run at all.
        student = ->(_c, _t) { raise 'must not run as the student' }
        assert_equal :unavailable, TaskCheck.check(task, 'rm -rf ~', variables: {}, runner: student).status
    end

    # dir: the command runs in that folder, as typed in a terminal there; a
    # command that names the folder runs in /workspace as before.
    def test_command_in_its_folder
        require 'open3'
        require 'tmpdir'
        task = { 'id' => 'hinweise', 'check' => { 'type' => 'command', 'dir' => 'bitstein', 'output' => { 'lines' => 2, 'matching' => '^HINWEIS' } } }
        Dir.mktmpdir do |root|
            FileUtils.mkdir_p(File.join(root, 'bitstein'))
            File.write(File.join(root, 'bitstein/tatort.txt'), "HINWEIS: eins\nKaffee\nHINWEIS: zwei\n")
            run = ->(command, _t) { out, st = Open3.capture2e('sh', '-c', command, :chdir => root); { :output => out, :status => st.exitstatus } }
            [
                "grep '^HINWEIS' tatort.txt",
                "grep '^HINWEIS' bitstein/tatort.txt",
                "cd bitstein && grep '^HINWEIS' tatort.txt",
                "grep '^HINWEIS' ~/bitstein/tatort.txt".sub('~', root),
            ].each do |command|
                result = TaskCheck.check(task, command, variables: {}, guest_runner: run)
                assert_equal :correct, result.status, "#{command}: #{result.message}"
            end
            ok = TaskCheck.check(task, "grep '^HINWEIS' tatort.txt", variables: {}, guest_runner: run)
            assert_includes ok.read, 'In `/workspace/bitstein`'
            FileUtils.rm_rf(File.join(root, 'bitstein'))
            missing = TaskCheck.check(task, "grep '^HINWEIS' tatort.txt", variables: {}, guest_runner: run)
            assert_equal :wrong, missing.status
            assert_includes missing.message, 'im Ordner `bitstein`'
        end
        assert_empty TaskSpec.validate_check('command', task['check'])
        refute_empty TaskSpec.validate_check('command', task['check'].merge('dir' => '../etc'))
        refute_empty TaskSpec.validate_check('command', task['check'].merge('dir' => '/etc'))
    end
end
