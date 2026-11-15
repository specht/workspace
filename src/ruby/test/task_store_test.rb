require 'minitest/autorun'
require_relative '../include/task_store'

class TaskStoreTest < Minitest::Test
    def test_state_key_is_stable_and_hides_the_email
        a = TaskStore.state_key('a@example.org', 'tcpip/adresse')
        assert_equal a, TaskStore.state_key('a@example.org', 'tcpip/adresse')
        refute_equal a, TaskStore.state_key('b@example.org', 'tcpip/adresse')
        refute_equal a, TaskStore.state_key('a@example.org', 'tcpip/loopback')
        refute_includes a, 'example'
        assert_equal 32, a.size
    end

    def test_first_attempt
        s = TaskStore.next_state(nil, task: 'tcpip/adresse', version: 'v1', status: 'wrong', answer: '127.0.0.1', now: 100)
        assert_equal 1, s[:attempts]
        assert_equal 'wrong', s[:first_status]
        assert_nil s[:first_correct]
        assert_equal '127.0.0.1', s[:last_answer]
    end

    def test_later_attempts_keep_first_status_and_first_correct
        s1 = TaskStore.next_state(nil, task: 't/a', version: 'v1', status: 'wrong', answer: 'x', now: 100)
        s2 = TaskStore.next_state(s1, task: 't/a', version: 'v1', status: 'correct', answer: 'y', now: 200)
        s3 = TaskStore.next_state(s2, task: 't/a', version: 'v1', status: 'wrong', answer: 'z', now: 300)
        assert_equal 3, s3[:attempts]
        assert_equal 'wrong', s3[:first_status]
        assert_equal 200, s3[:first_correct]
        assert_equal 'wrong', s3[:status]
        assert_equal 'z', s3[:last_answer]
    end

    def test_new_version_starts_again
        s1 = TaskStore.next_state(nil, task: 't/a', version: 'v1', status: 'correct', answer: 'x', now: 100)
        s2 = TaskStore.next_state(s1, task: 't/a', version: 'v2', status: 'wrong', answer: 'x', now: 200)
        assert_equal 1, s2[:attempts]
        assert_nil s2[:first_correct]
        assert_equal 'wrong', s2[:first_status]
    end

    def test_long_answers_are_cut
        s = TaskStore.next_state(nil, task: 't/a', version: 'v1', status: 'sent', answer: 'x' * 5000, now: 1)
        assert_equal TaskStore::MAX_ANSWER, s[:last_answer].size
    end

    def test_public_state_marks_changed_tasks
        stored = { :version => 'v1', :status => 'correct', :last_answer => 'x', :attempts => 2, :updated => 5 }
        assert_equal 'correct', TaskStore.public_state(stored, 'v1')[:status]
        assert_equal 'stale', TaskStore.public_state(stored, 'v2')[:status]
        assert_equal 'x', TaskStore.public_state(stored, 'v2')[:answer]
    end

    # a prediction keeps its first answer, for the class board's count
    def test_prediction_keeps_first_answer
        s1 = TaskStore.next_state(nil, task: 't/p', version: 'v1', status: 'wrong', answer: '50', now: 100, prediction: true)
        s2 = TaskStore.next_state(s1, task: 't/p', version: 'v1', status: 'correct', answer: '7', now: 200, prediction: true)
        assert_equal '50', s2[:first_answer]
        assert_equal '7', s2[:last_answer]
        # a new version of the check: the next answer is the first again
        s3 = TaskStore.next_state(s2, task: 't/p', version: 'v2', status: 'correct', answer: '7', now: 300, prediction: true)
        assert_equal '7', s3[:first_answer]
    end

    def test_no_first_answer_unless_prediction_or_unknown
        s = TaskStore.next_state(nil, task: 't/a', version: 'v1', status: 'wrong', answer: 'x', now: 100)
        assert_nil s[:first_answer]
        # tried before first answers were kept: the first is not known
        old = { :version => 'v1', :attempts => 2, :status => 'wrong', :last_answer => '8' }
        s = TaskStore.next_state(old, task: 't/p', version: 'v1', status: 'correct', answer: '7', now: 200, prediction: true)
        assert_nil s[:first_answer]
    end
end
