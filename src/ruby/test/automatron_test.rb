require 'minitest/autorun'
require_relative '../include/automatron'

# The subset construction can grow exponentially; Automatron is open
# without a login, so it stops at SubsetTrace::MAX_STATES.
class AutomatronTest < Minitest::Test
    def dfa_for(regex)
        nfa = NFA.from_regex(regex)
        nfa.compact_ids!
        nfa.topo_relabel!
        SubsetTrace.build_with_trace(nfa)
    end

    def test_a_classroom_regex_works
        dfa, trace = dfa_for('a(b|a)*a+')
        refute_nil dfa
        assert trace[:state_map].size <= SubsetTrace::MAX_STATES
    end

    def test_an_exploding_regex_stops_early
        regex = '(a|b)*a' + '(a|b)' * 19
        started = Process.clock_gettime(Process::CLOCK_MONOTONIC)
        assert_raises(SubsetTrace::TooLarge) { dfa_for(regex) }
        assert Process.clock_gettime(Process::CLOCK_MONOTONIC) - started < 2
    end
end
