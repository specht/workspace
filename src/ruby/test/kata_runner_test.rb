require 'minitest/autorun'
require_relative '../include/kata_runner'

# Runs real containers from the Workspace image; skipped where there is no
# Docker or no image (./build-image.sh).
class KataRunnerTest < Minitest::Test
    def setup
        available = system('docker', 'image', 'inspect', KataRunner::IMAGE, :out => File::NULL, :err => File::NULL)
        skip "no Docker image #{KataRunner::IMAGE}" unless available
    end

    def test_each_language_runs_and_reads_its_input
        python = KataRunner.run(files: { 'kata.py' => "print('Hallo, ' + input() + '!')\n" }, argv: %w[python3 -I kata.py], input: "Ada\n")
        assert python.ok?, python.stderr
        assert_equal "Hallo, Ada!\n", python.stdout
        ruby = KataRunner.run(files: { 'kata.rb' => "puts gets.to_i * 2\n" }, argv: %w[ruby kata.rb], input: "21\n")
        assert_equal "42\n", ruby.stdout
        node = KataRunner.run(files: { 'kata.js' => "console.log([3, 1, 2].sort().join(' '))\n" }, argv: %w[node kata.js])
        assert_equal "1 2 3\n", node.stdout
    end

    def test_an_endless_loop_stops_at_the_time_limit
        started = Time.now
        result = KataRunner.run(files: { 'kata.py' => "while True:\n    pass\n" }, argv: %w[python3 kata.py], timeout: 2)
        assert result.timed_out
        refute result.ok?
        assert_operator Time.now - started, :<, 8
    end

    def test_too_much_output_stops_the_run
        result = KataRunner.run(files: { 'kata.rb' => "loop { puts 'x' * 1000 }\n" }, argv: %w[ruby kata.rb], timeout: 10)
        assert result.too_long
        assert_operator result.stdout.bytesize, :<=, KataRunner::MAX_OUTPUT + 64 * 1024
    end

    def test_no_network_and_nothing_to_write_outside_tmp
        code = "import socket\ntry:\n    socket.create_connection(('1.1.1.1', 80), timeout=2)\n    print('net')\nexcept OSError:\n    print('no net')\n" \
               "try:\n    open('/etc/kata', 'w')\nexcept OSError:\n    print('read-only')\n"
        result = KataRunner.run(files: { 'kata.py' => code }, argv: %w[python3 kata.py])
        assert_equal "no net\nread-only\n", result.stdout
    end

    def test_a_run_can_be_stopped_from_outside
        stop = nil
        thread = Thread.new do
            KataRunner.run(files: { 'kata.py' => "import time\ntime.sleep(30)\n" }, argv: %w[python3 kata.py], timeout: 20,
                           cancel: ->(kill) { stop = kill })
        end
        sleep 0.1 until stop
        sleep 0.5
        stop.call
        result = thread.value
        assert result.cancelled
        refute result.timed_out
    end

    def test_output_comes_while_the_program_runs
        seen = []
        KataRunner.run(files: { 'kata.py' => "import time\nprint('a', flush=True)\ntime.sleep(0.5)\nprint('b')\n" },
                       argv: %w[python3 kata.py], on_output: ->(stream, text) { seen << [stream, text, Time.now] })
        assert_equal "a\nb\n", seen.map { |s| s[1] }.join
        assert_operator seen.last[2] - seen.first[2], :>, 0.3 if seen.size > 1
    end
end
