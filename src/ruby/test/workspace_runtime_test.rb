require 'minitest/autorun'
require 'stringio'
require 'open3'
require_relative '../include/workspace_runtime'

class WorkspaceRuntimeTest < Minitest::Test
    def setup
        @captures = []
        @oks = []
        @capture_outputs = []
        @stats_output = ''
        @runtime = WorkspaceRuntime::DirectDocker.new(
            :capture => lambda do |command, **options|
                @captures << [command, options]
                @capture_outputs.shift || ''
            end,
            :ok => lambda do |command, **options|
                @oks << [command, options]
                true
            end,
            :each_line => lambda do |command, &block|
                @stats_command = command
                block.call(StringIO.new(@stats_output))
            end,
        )
    end

    def test_running_workspaces_filters_and_normalizes_network_entries
        @capture_outputs << JSON.generate([{
            'Containers' => {
                'a' => {'Name' => 'hs_code_abcdefghijklmnop', 'IPv4Address' => '172.20.0.7/16'},
                'b' => {'Name' => 'workspace_mysql_1', 'IPv4Address' => '172.20.0.3/16'},
            },
        }])

        assert_equal(
            {'abcdefghijklmnop' => {:ip => '172.20.0.7'}},
            @runtime.running_workspaces(:timeout => 9),
        )
        assert_equal(
            ['docker network inspect workspace_user', {:timeout => 9}],
            @captures.first,
        )
    end

    def test_workspace_state_preserves_inspect_behavior
        @capture_outputs << JSON.generate([{
            'NetworkSettings' => {
                'Networks' => {
                    'workspace_user' => {'IPAddress' => '172.20.0.8'},
                },
            },
        }])

        assert_equal(
            {:tag => 'abcdefghijklmnop', :running => true, :ip => '172.20.0.8'},
            @runtime.workspace_state('abcdefghijklmnop', :timeout => 10),
        )
        assert_equal(
            ['docker inspect hs_code_abcdefghijklmnop', {:timeout => 10, :allow_failure => true}],
            @captures.first,
        )
    end

    def test_start_workspace_keeps_current_resource_limits_and_mounts
        env = {
            'MYSQL_HOST' => 'mysql',
            'MYSQL_USER' => 'student',
            'MYSQL_PASSWORD' => 'mysql-secret',
            'MYSQL_DATABASE' => 'student',
            'NEO4J_URI' => 'neo4j://neo4j:7687',
            'NEO4J_USERNAME' => 'student',
            'NEO4J_PASSWORD' => 'neo4j-secret',
            'NEO4J_DATABASE' => 'student',
        }

        @runtime.start_workspace(
            :fs_tag => 'abcdefghijklmnop',
            :workspace_login => 'max.mustermann',
            :database_environment => env,
            :host_data_path => '/srv/workspace',
            :test_mode => true,
            :timeout => 60,
        )

        command, options = @oks.first
        assert_includes command, 'docker run --cpus=4 --memory=4g --memory-swap=4g --pids-limit=256'
        assert_includes command, '-e WORKSPACE_USER=max.mustermann'
        assert_includes command, '-v /srv/workspace/user/abcdefghijklmnop/config:/config'
        assert_includes command, '-v /srv/workspace/user/abcdefghijklmnop/workspace:/workspace'
        assert_includes command, '--network workspace_user'
        assert_includes command, '-v /dev/null:/etc/resolv.conf:ro'
        assert_includes command, '-e DOTNET_CLI_TELEMETRY_OPTOUT=1'
        assert_includes command, '--cap-drop NET_RAW'
        assert_includes command, '--security-opt no-new-privileges'
        assert command.end_with?('--name hs_code_abcdefghijklmnop hs_code_server')
        assert_equal({:timeout => 60}, options)
    end

    def test_stop_workspace_is_best_effort_like_current_code
        @runtime.stop_workspace('abcdefghijklmnop', :timeout => 10)

        assert_equal(
            ['docker kill hs_code_abcdefghijklmnop', {:timeout => 10, :allow_failure => true}],
            @oks.first,
        )
    end

    def test_workspace_stats_keeps_only_student_workspaces
        @stats_output = <<~JSONL
            {"Name":"hs_code_abcdefghijklmnop","CPUPerc":"1.00%"}
            {"Name":"workspace_mysql_1","CPUPerc":"2.00%"}
        JSONL

        assert_equal(
            {'abcdefghijklmnop' => {'Name' => 'hs_code_abcdefghijklmnop', 'CPUPerc' => '1.00%'}},
            @runtime.workspace_stats,
        )
        assert_equal 'docker stats --no-stream --format "{{ json . }}"', @stats_command
    end

    def test_live_app_socket_parser_keeps_current_proc_net_filter
        @capture_outputs << <<~PROCNET
          sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode
           0: 00000000:04D2 00000000:0000 0A 00000000:00000000 00:00000000 00000000  1000        0 4242
           1: 00000000:115C 00000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 4243
        PROCNET

        assert_equal(
            [{:port => 1234, :inode => '4242'}],
            @runtime.live_app_sockets('abcdefghijklmnop', :uid => 1000, :timeout => 10),
        )
        command, options = @captures.first
        assert_equal "docker exec hs_code_abcdefghijklmnop sh -c 'cat /proc/net/tcp /proc/net/tcp6 2>/dev/null'", command
        assert_equal({:timeout => 10, :allow_failure => true, :log_command => false}, options)
    end

    def test_live_app_process_parser_preserves_inode_mapping
        @capture_outputs << "4242\t123\truby\truby server.rb\n"

        assert_equal(
            {'4242' => {:pid => 123, :process => 'ruby', :command => 'ruby server.rb'}},
            @runtime.live_app_processes('abcdefghijklmnop', :uid => 1000, :timeout => 10),
        )
        command, options = @captures.first
        assert command.start_with?('docker exec -u 1000 hs_code_abcdefghijklmnop sh -c ')
        assert_equal({:timeout => 10, :allow_failure => true, :log_command => false}, options)
    end
    def test_git_status_runs_only_in_a_running_container
        @capture_outputs << '[]'
        assert_nil @runtime.git_status('abc', :repo => 'sandbox/structures', :uid => '1000:1000', :script => 'echo "$1"', :timeout => 5)
        assert_equal 1, @captures.size

        @capture_outputs << JSON.generate([{ 'NetworkSettings' => { 'Networks' => { 'workspace_user' => { 'IPAddress' => '172.20.0.9' } } } }])
        @capture_outputs << "STATUS ## main\n"
        output = @runtime.git_status('abc', :repo => 'my repo', :uid => '1000:1000', :script => 'echo "$1"', :timeout => 5)
        assert_equal "STATUS ## main\n", output
        command, options = @captures.last
        # HOME as in the student's terminal: Git reads /workspace/.gitconfig.
        assert_equal "docker exec -u 1000:1000 -e HOME\\=/workspace hs_code_abc sh -c echo\\ \\\"\\$1\\\" sh /workspace/my\\ repo", command
        assert_equal true, options[:allow_failure]
    end

    # Not running: the same script in a reader container, the folder
    # read-only, as the student, without network. Git never runs on the
    # server itself.
    def test_git_status_reads_a_stopped_workspace_in_a_reader_container
        @capture_outputs << '[]'
        @capture_outputs << "TOP /workspace/sandbox\n"
        output = @runtime.git_status('abc', :repo => 'sandbox', :uid => '1000:1000', :script => 'echo "$1"',
            :timeout => 5, :host_data_path => '/srv/data')
        assert_equal "TOP /workspace/sandbox\n", output
        command = @captures.last.first
        assert_match(/docker run --rm --name hs_code_offline_[0-9a-f]{8} --network none --read-only/, command)
        assert_includes command, '-u 1000:1000'
        assert_includes command, '-v /srv/data/user/abc/workspace:/workspace:ro'
        assert_includes command, '--entrypoint sh hs_code_server'
        assert_includes command, 'sh /workspace/sandbox'
    end

    def test_run_script_passes_arguments
        @capture_outputs << JSON.generate([{ 'NetworkSettings' => { 'Networks' => { 'workspace_user' => { 'IPAddress' => '172.20.0.9' } } } }])
        @capture_outputs << "STATE current\n"
        output = @runtime.run_script('abc', :script => 'echo "$1 $2"', :args => ['/workspace/anaglyph', 'a' * 40], :uid => '1000:1000', :timeout => 10)
        assert_equal "STATE current\n", output
        command, = @captures.last
        assert_equal "docker exec -u 1000:1000 -e HOME\\=/workspace hs_code_abc sh -c echo\\ \\\"\\$1\\ \\$2\\\" sh /workspace/anaglyph #{'a' * 40}", command
    end

    def test_run_command
        @capture_outputs << JSON.generate([{ 'NetworkSettings' => { 'Networks' => { 'workspace_user' => { 'IPAddress' => '172.20.0.9' } } } }])
        @capture_outputs << "Cats sleep.\n__EXIT 0\n"
        result = @runtime.run_command('abc', :command => 'curl -s x | jq -r .fact', :uid => '1000:1000', :seconds => 10)
        assert_equal({ :output => 'Cats sleep.', :status => 0, :timed_out => false }, result)
        command, options = @captures.last
        assert_includes command, 'docker exec -u 1000:1000 -w /workspace -e HOME\\=/workspace hs_code_abc sh -c'
        assert_includes command, 'curl\\ -s\\ x\\ \\|\\ jq\\ -r\\ .fact 10'
        assert_equal 20, options[:timeout]

        @capture_outputs << JSON.generate([{ 'NetworkSettings' => { 'Networks' => { 'workspace_user' => { 'IPAddress' => '172.20.0.9' } } } }])
        @capture_outputs << "\n__EXIT 124\n"
        assert @runtime.run_command('abc', :command => 'sleep 99', :uid => '1000:1000', :seconds => 2)[:timed_out]

        @capture_outputs << JSON.generate([{ 'NetworkSettings' => { 'Networks' => { 'workspace_user' => { 'IPAddress' => '172.20.0.9' } } } }])
        @capture_outputs << "x\n__EXIT 0\n"
        @runtime.run_command('abc', :command => 'ls', :uid => '65534:65534', :seconds => 2, :home => '/tmp')
        assert_includes @captures.last.first, 'docker exec -u 65534:65534 -w /workspace -e HOME\\=/tmp hs_code_abc'

        @capture_outputs << '[]'
        assert_nil @runtime.run_command('abc', :command => 'ls', :uid => '1000:1000', :seconds => 2)
    end

    # The script itself, run locally with sh.
    def test_run_script_locally
        out, = Open3.capture2('sh', '-c', WorkspaceRuntime::DirectDocker::RUN_SCRIPT, 'sh', 'echo hallo; exit 3', '5')
        assert_equal "hallo\n__EXIT 3\n", out
        out, = Open3.capture2('sh', '-c', WorkspaceRuntime::DirectDocker::RUN_SCRIPT, 'sh', 'sleep 5', '1')
        assert_match(/__EXIT 124/, out)
    end
end
