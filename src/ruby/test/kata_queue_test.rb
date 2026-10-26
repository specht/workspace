require 'minitest/autorun'
require_relative '../include/kata_queue'

class KataQueueTest < Minitest::Test
    # A job that runs until it is released, and says when it has started.
    def gate
        started = Queue.new
        release = Queue.new
        job = lambda do |ticket|
            stopped = false
            ticket.on_stop(-> { stopped = true; release << :stop })
            started << ticket.owner
            release.pop
            stopped ? :stopped : :done
        end
        [job, started, release]
    end

    def test_runs_wait_until_a_place_is_free_first_come_first_served
        queue = KataQueue.new(parallel: 2)
        job, started, release = gate
        a = queue.submit('a', &job)
        b = queue.submit('b', &job)
        c = queue.submit('c', &job)
        d = queue.submit('d', &job)
        2.times { started.pop }
        assert_equal :running, a.state
        assert_equal :running, b.state
        assert_equal 0, queue.ahead(c)
        assert_equal 1, queue.ahead(d)
        release << :go
        assert_includes %w[c d], started.pop
        release << :go
        release << :go
        release << :go
        [a, b, c, d].each { |t| assert_equal :done, t.wait(5) }
        assert_nil queue.ahead(d)
    end

    def test_a_new_run_replaces_the_waiting_one_of_the_same_person
        queue = KataQueue.new(parallel: 1)
        job, started, release = gate
        queue.submit('a', &job)
        started.pop
        first = queue.submit('b', &job)
        second = queue.submit('b', &job)
        assert_equal :cancelled, first.state
        assert_equal 0, queue.ahead(second)
        release << :go
        assert_equal 'b', started.pop
        release << :go
        assert_equal :done, second.wait(5)
        assert_equal :done, second.result
    end

    def test_a_new_run_stops_the_running_one_of_the_same_person
        queue = KataQueue.new(parallel: 1)
        job, started, release = gate
        first = queue.submit('a', &job)
        started.pop
        second = queue.submit('a', &job)
        assert_equal :cancelled, first.wait(5)
        assert_equal 'a', started.pop
        release << :go
        assert_equal :done, second.wait(5)
        assert_equal :done, second.result
    end

    def test_an_error_in_a_job_ends_its_ticket_not_the_queue
        queue = KataQueue.new(parallel: 1)
        broken = queue.submit('a') { raise 'kaputt' }
        assert_equal :done, broken.wait(5)
        assert_equal 'kaputt', broken.error.message
        fine = queue.submit('b') { 42 }
        assert_equal :done, fine.wait(5)
        assert_equal 42, fine.result
    end

    def test_wait_gives_up_after_its_timeout
        queue = KataQueue.new(parallel: 1)
        job, started, release = gate
        ticket = queue.submit('a', &job)
        started.pop
        assert_equal :running, ticket.wait(0.05)
        release << :go
        assert_equal :done, ticket.wait(5)
    end
end
