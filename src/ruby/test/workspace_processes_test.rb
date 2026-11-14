require 'minitest/autorun'
require_relative '../include/workspace_processes'

# What keeps a Workspace busy (WorkspaceProcesses): the share of a core
# from two readings, memory and running time, a process named by program
# and file only, VS Code by what it is.
class WorkspaceProcessesTest < Minitest::Test
    def reading(ticks_a, ticks_b)
        sep = "\x1f"
        <<~OUT
            H 100 4096 1000.00
            P 10 500 1000 25000 node\t/app/code-server/lib/node#{sep}/app/code-server/out/node/entry
            P 42 #{ticks_a} 90000 3000 python3\tpython3#{sep}-u#{sep}/workspace/projekt/schleife.py#{sep}--geheim
            P 43 5 99000 100 sleep\tsleep#{sep}100
            P 2 1 10 0 s6-supervise\ts6-supervise#{sep}code-server
            P 3 1 10 900 bash\tbash#{sep}./run
            M
            P 10 502 1000 25000 node\t/app/code-server/lib/node#{sep}/app/code-server/out/node/entry
            P 42 #{ticks_b} 90000 3000 python3\tpython3#{sep}-u#{sep}/workspace/projekt/schleife.py#{sep}--geheim
            P 44 1 99900 50 ls\tls
            P 2 1 10 0 s6-supervise\ts6-supervise#{sep}code-server
            P 3 1 10 900 bash\tbash#{sep}./run
        OUT
    end

    def test_the_busiest_first_with_its_share_memory_and_time
        list = WorkspaceProcesses.parse(reading(800, 850))
        assert_equal [42, 10], list.map { |p| p[:pid] }, 'ended and new ones drop out'
        busy = list.first
        assert_equal 'python3 projekt/schleife.py', busy[:label]
        assert_equal 100.0, busy[:cpu]
        assert_equal 3000 * 4096, busy[:rss]
        assert_equal 100, busy[:seconds]
        assert_equal 'VS Code', list.last[:label]
        assert_equal 4.0, list.last[:cpu]
    end

    def test_labels_name_program_and_file_only
        assert_equal 'node server.js', WorkspaceProcesses.label(['/usr/bin/node', 'server.js', '--port', '3000'], 'node')
        assert_equal 'dart linked_list_test.dart', WorkspaceProcesses.label(['dart', 'test', '/tmp/x/linked_list_test.dart'], 'dart')
        assert_equal 'bash', WorkspaceProcesses.label(['bash'], 'bash')
        assert_equal 'sh -c', WorkspaceProcesses.label(['sh', '-c', 'python3 /workspace/a.py & sleep 9'], 'sh')
        assert_equal 'kworker', WorkspaceProcesses.label([], 'kworker')
        assert_equal 'VS Code: Erweiterungen', WorkspaceProcesses.label(['/app/code-server/lib/node', '--type=extensionHost'], 'node')
        refute_includes WorkspaceProcesses.label(['python3', 'a.py', '--password=geheim'], 'python3'), 'geheim'
    end
end
