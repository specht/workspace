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

    # The intermediate DFA of a long expression is left out with a note; the
    # smallest DFA is still drawn (main.rb, svg_from_dot).
    def test_a_drawing_too_large_to_read_is_left_out
        a, b, c = '(0|3|6|9)', '(1|4|7)', '(2|5|8)'
        divisible_by_3 = "(#{a}|#{c}#{a}*#{b}|(#{b}|#{c}#{a}*#{c})(#{a}|#{b}#{a}*#{c})*(#{c}|#{b}#{a}*#{b}))*"
        nfa = NFA.from_regex(divisible_by_3)
        nfa.compact_ids!
        nfa.topo_relabel!
        dfa, = SubsetTrace.build_with_trace(nfa)
        min, = DFAMinimizer.minimize_with_trace(dfa)
        assert_equal 3, min.states.size
        big = DotRender.dfa(dfa, title: 'DFA', sink_explicit: true, q: 'r')
        refute DotRender.drawable?(big)
        assert_includes DotRender.not_drawn_html(big), "#{DotRender.edges(big)} Übergänge"
        assert DotRender.drawable?(DotRender.dfa(min, title: 'MinDFA', sink_explicit: true, q: 's'))
    end
end
