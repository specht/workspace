require 'minitest/autorun'
require 'cgi'
require_relative '../include/code_highlight'

# Context lines (»~ «) in a code block: faint spans, without the marker.
class CodeContextTest < Minitest::Test
    def render(text, language = 'c')
        doc = Nokogiri::HTML::DocumentFragment.parse("<pre><code class='#{language}'>#{CGI.escapeHTML(text)}</code></pre>")
        CodeHighlight.apply(doc)
        doc.at('pre')
    end

    def test_context_lines_are_marked_and_lose_their_marker
        pre = render("~ void f(void) {\n    int a = 1;\n~ }\n")
        assert_includes pre['class'], 'has-code-context'
        lines = pre.css('.code-line')
        assert_equal 3, lines.size
        assert_equal [true, false, true], lines.map { |l| l['class'].include?('code-context') }
        refute_includes pre.text, '~'
        assert_equal ['void f(void) {', '    int a = 1;', '}'], lines.map(&:text)
        assert_equal %w[code-line code-cut code-line code-cut code-line], pre.element_children.map { |e| e['class'].split.first }
    end

    def test_blank_lines_next_to_context_are_context
        # main { … } with a new blank line and three new declarations
        _, context = CodeHighlight.split_context("~ main {\n~     padding: 2rem;\n\n    display: flex;\n\n    gap: 2rem;\n\n~ }\n")
        assert_equal [1, 2, 3, 7, 8], context
        # a blank line among new code only stays new
        _, none = CodeHighlight.split_context("a\n\nb\n")
        assert_empty none
    end

    def test_blocks_without_context_stay_as_they_were
        pre = render("int a = ~b;\n")
        assert_empty pre.css('.code-line')
        assert_equal "int a = ~b;\n", pre.text
    end
end
