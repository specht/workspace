require 'minitest/autorun'
require 'json'
require_relative '../include/trace_check'

# Trace tables: filled in cell by cell; the feedback names the first wrong
# cell and never the right value.
class TraceCheckTest < Minitest::Test
    CHECK = { 'type' => 'trace', 'columns' => %w[n steps], 'rows' => [[18, 1], [9, 2], [4, 3]], 'given' => [[0, 0], [0, 1]] }.freeze
    TASK = { 'check' => CHECK, 'feedback' => [{ 'when' => { 'cell' => [2, 0], 'equals' => 4.5 }, 'say' => 'Der Rest fällt weg.' }] }.freeze

    def check(rows)
        TraceCheck.check(TASK, CHECK, JSON.generate(rows))
    end

    def test_spec
        assert_empty TraceCheck.spec_errors(CHECK)
        refute_empty TraceCheck.spec_errors(CHECK.merge('rows' => [[1]]))
        refute_empty TraceCheck.spec_errors(CHECK.merge('given' => [[5, 0]]))
        refute_empty TraceCheck.spec_errors(CHECK.merge('given' => [[0, 0], [0, 1], [1, 0], [1, 1], [2, 0], [2, 1]]))
        refute_empty TraceCheck.spec_errors(CHECK.merge('columns' => []))
    end

    def test_correct_and_written_the_german_way
        assert_equal :correct, check([['', ''], ['9', '2'], ['4', '3']])[0]
        assert_equal :correct, check([['', ''], ['9,0', ' 2 '], ['4', '3']])[0]
        assert_equal :correct, TraceCheck.check(TASK, CHECK, TraceCheck.correct_answer(CHECK))[0]
    end

    def test_empty_fields
        status, message, = check([['', ''], ['9', ''], ['4', '3']])
        assert_equal :invalid, status
        assert_equal 'Ein Feld ist noch leer.', message
        assert_equal :invalid, TraceCheck.check(TASK, CHECK, 'kein JSON')[0]
    end

    def test_first_wrong_cell_without_the_answer
        status, message, = check([['', ''], ['8', '2'], ['3', '3']])
        assert_equal :wrong, status
        assert_includes message, 'Durchlauf 2, `n`'
        assert_includes message, 'Insgesamt stimmen 2 Felder'
        refute_includes message, '9'
    end

    def test_own_feedback_for_a_typical_mistake
        _, message, = check([['', ''], ['9', '2'], ['4,5', '3']])
        assert_equal 'Durchlauf 3, `n`: Der Rest fällt weg.', message
    end
end
