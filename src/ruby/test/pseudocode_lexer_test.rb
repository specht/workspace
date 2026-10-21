require 'minitest/autorun'
require_relative '../include/code_highlight'

# The Handbuch's pseudocode (```pseudocode, pseudocode_lexer.rb): keywords,
# functions, fields, ← and constants; a German remark in parentheses is a
# comment, a tuple or the arguments of a call are code. The Algo Stepper's
# code box (algo_stepper.js, highlightPseudocode) follows the same rules.
class PseudocodeLexerTest < Minitest::Test
    def tokens(code)
        Rouge::Lexers::Pseudocode.new.lex(code).reject { |_, value| value.strip.empty? }.map { |token, value| [token.shortname, value] }
    end

    def test_keywords_functions_fields_and_assignment
        assert_equal [%w[k if], %w[n value], %w[o <], %w[n node], %w[p .], %w[na value], %w[p :], %w[n node], %w[p .], %w[na left], %w[o ←],
                      %w[nf remove], %w[p (], %w[n node], %w[p .], %w[na left], %w[p ,], %w[n value], %w[p )]],
                     tokens('if value < node.value: node.left ← remove(node.left, value)')
    end

    def test_constants_and_numbers
        assert_equal [%w[k if], %w[n node], %w[kc empty], %w[p :], ['kc', 'not found']], tokens('if node empty: not found')
        assert_includes tokens('i ← (i - 1) / 2'), %w[mi 1]
    end

    def test_a_german_remark_is_a_comment_and_a_tuple_is_not
        assert_includes tokens('merge_sort(list, start, stop):   (stop gehört nicht mehr dazu)'), ['c1', '(stop gehört nicht mehr dazu)']
        assert_includes tokens('done (oder Fehler, je nach Aufgabe)'), ['c1', '(oder Fehler, je nach Aufgabe)']
        refute tokens('push (0, start)').any? { |token, _| token == 'c1' }
        refute tokens('merge_sort(list, start, stop):').any? { |token, _| token == 'c1' }
        refute tokens('if (j ≥ stop or list[i] ≤ list[j]):').any? { |token, _| token == 'c1' }
    end

    def test_blocks_on_the_page_are_highlighted
        html = Nokogiri::HTML::DocumentFragment.parse("<pre><code class='pseudocode'>return x\n</code></pre>")
        CodeHighlight.apply(html)
        assert_includes html.at_css('pre')['class'], 'code-pseudocode'
        assert html.at_css('pre span.k')
    end
end
