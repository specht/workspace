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

    # Typos are the student's, not a server error: a RuntimeError the route
    # turns into a message (400), never a NoMethodError.
    def test_broken_regexes_raise_a_readable_error
        { 'a$b' => /Unsupported character in regex: '\$'/, '(ab' => /parentheses/, 'ab)' => /parentheses/,
          'a|' => /Invalid/, '|a' => /Invalid/, '*a' => /Invalid/, '()' => /Invalid/, 'a(|b)' => /Invalid/ }.each do |regex, message|
            error = assert_raises(RuntimeError, regex) { NFA.from_regex(regex) }
            assert_match message, error.message, regex
        end
    end

    def test_symbols_on_one_edge_become_ranges
        assert_equal '0, 2–9', DotRender.symbols_label(%w[9 0 2 3 4 5 6 7 8])
        assert_equal 'ε, a, b', DotRender.symbols_label(%w[b a ε])
        assert_equal 'a, c–e, x', DotRender.symbols_label(%w[a c d e x])
    end
end
