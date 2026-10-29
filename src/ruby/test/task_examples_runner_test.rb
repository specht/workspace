require 'minitest/autorun'
require 'tmpdir'
require_relative '../include/task_examples'

# The local runs of the task examples end by themselves even when the test
# that started them is killed (TaskExamples.bounded): a stopped run once
# left a Kata harness computing for 32 hours in the Ruby container.
class TaskExamplesRunnerTest < Minitest::Test
    def alive?(marker)
        system('pgrep', '-f', marker, :out => File::NULL)
    end

    def test_a_run_ends_after_its_time_even_without_the_test
        Dir.mktmpdir do |root|
            marker = "sleep 31.#{rand(10**6)}"
            starter = fork do
                TaskExamples.local_kata(root).call(:files => {}, :argv => ['sh', '-c', marker], :timeout => 1)
            end
            sleep 0.5
            assert alive?(marker), 'the program runs'
            Process.kill('KILL', starter)
            Process.wait(starter)
            # 1 second plus 2 of timeout(1), and its 2 seconds of -k
            deadline = Time.now + 8
            sleep 0.2 while alive?(marker) && Time.now < deadline
            refute alive?(marker), 'the program still runs without the test that started it'
        end
    end

    def test_a_run_within_its_time_is_unchanged
        Dir.mktmpdir do |root|
            result = TaskExamples.local_kata(root).call(:files => { 'a.txt' => 'hello' }, :argv => ['cat', 'a.txt'], :timeout => 5)
            assert_equal 'hello', result.stdout
            assert_equal 0, result.status
            refute result.timed_out
            output = TaskExamples.local_runner(root).call('echo hi; exit 3', 5)
            assert_equal "hi\n", output[:output]
            assert_equal 3, output[:status]
        end
    end
end
