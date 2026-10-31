require 'minitest/autorun'
require_relative '../include/interleaving_check'

# The order of steps from the widget (interleaving.js), run with the same
# rules on the server
class InterleavingCheckTest < Minitest::Test
    COUNTER = {
        'type' => 'interleaving',
        'shared' => { 'counter' => 0 },
        'threads' => {
            'A' => ['a ← counter', 'a ← a + 1', 'counter ← a'],
            'B' => ['b ← counter', 'b ← b + 1', 'counter ← b'],
        },
        'goal' => 'counter == 1',
    }.freeze

    # Two accounts, each thread takes the two locks in its own order
    DEADLOCK = {
        'type' => 'interleaving',
        'shared' => { 'x' => 10, 'y' => 10 },
        'locks' => %w[m n],
        'threads' => {
            'A' => ['lock m', 'lock n', 'x ← x - 5', 'y ← y + 5', 'unlock n', 'unlock m'],
            'B' => ['lock n', 'lock m', 'y ← y - 3', 'x ← x + 3', 'unlock m', 'unlock n'],
        },
        'goal' => 'deadlock',
    }.freeze

    def check(spec, answer, task = {})
        InterleavingCheck.check(task, spec, answer)
    end

    def test_a_lost_update_and_the_order_that_does_not_lose_it
        status, message, read = check(COUNTER, 'A B A B A B')
        assert_equal :correct, status, message
        assert_equal 'Passt: Am Ende ist counter = 1.', message
        assert_equal 'Deine Reihenfolge: A B A B A B. Am Ende: counter = 1.', read

        status, message, = check(COUNTER, 'A A A B B B')
        assert_equal :wrong, status
        assert_equal 'Am Ende ist counter = 2; gesucht ist eine Reihenfolge mit counter == 1.', message

        _, message, = check(COUNTER, 'A A A B B B', { 'feedback' => [{ 'when' => { 'problem' => 'goal' }, 'say' => 'Hier kommt jede Erhöhung an.' }] })
        assert_equal 'Hier kommt jede Erhöhung an.', message
    end

    def test_every_thread_must_finish_and_steps_must_exist
        assert_match(/B ist noch nicht fertig/, check(COUNTER, 'A A A B')[1])
        assert_match(/Schritt 4: A ist schon fertig/, check(COUNTER, 'A A A A')[1])
        assert_equal :invalid, check(COUNTER, 'A C')[0]
        assert_equal :invalid, check(COUNTER, '')[0]
    end

    def test_locks_wait_and_a_deadlock_is_found
        status, message, read = check(DEADLOCK, 'A B')
        assert_equal :correct, status, message
        assert_includes read, 'A steht bei »lock n«, B steht bei »lock m«'

        assert_match(/Schritt 3: B wartet auf den Lock n, den A hält/, check(DEADLOCK, 'A A B')[1])
        assert_match(/Alle Threads sind fertig geworden/, check(DEADLOCK, 'A A A A A A B B B B B B')[1])
        assert_match(/Es kann noch ein Thread weiter: A\./, check(DEADLOCK, 'A A')[1])
    end

    def test_with_a_lock_no_update_is_lost
        locked = COUNTER.merge('locks' => ['m'], 'goal' => 'counter == 2', 'threads' => {
            'A' => ['lock m', 'a ← counter', 'a ← a + 1', 'counter ← a', 'unlock m'],
            'B' => ['lock m', 'b ← counter', 'b ← b + 1', 'counter ← b', 'unlock m'],
        })
        assert_empty InterleavingCheck.spec_errors(locked)
        assert_equal :correct, check(locked, 'A A A A A B B B B B')[0]
        assert_match(/B wartet auf den Lock m, den A hält/, check(locked, 'A B')[1])
    end

    def test_spec
        assert_empty InterleavingCheck.spec_errors(COUNTER)
        assert_empty InterleavingCheck.spec_errors(DEADLOCK)
        bad = COUNTER.merge('threads' => { 'A' => ['a = counter', 'lock q'], 'b' => ['x'] }, 'goal' => 'total == 1', 'extra' => 1)
        errors = InterleavingCheck.spec_errors(bad)
        assert_includes errors, 'threads must map 2 to 4 capital letters to their lines'
        assert_includes errors, 'goal must be »deadlock« or a comparison of a shared variable with a number (counter == 1)'
        assert_includes errors, 'unknown keys: extra'
        errors = InterleavingCheck.spec_errors(COUNTER.merge('threads' => { 'A' => ['a = counter'], 'B' => ['lock q'] }))
        assert(errors.any? { |e| e.include?('»a = counter« is not') })
        assert_includes errors, 'threads.B[0]: no lock q in locks'
    end
end
