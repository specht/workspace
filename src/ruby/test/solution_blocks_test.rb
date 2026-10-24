require 'minitest/autorun'
require 'nokogiri'
require 'redcarpet'
require_relative '../include/code_languages'
require_relative '../include/solution_blocks'

class SolutionBlocksTest < Minitest::Test
    PAGE = <<~MD
        <div class='hint own'>
        <strong>Jetzt bist du dran:</strong> Schreib <code>reflect</code>.
        </div>

        <!-- solution -->
        ```c
        vec reflect(vec dir, vec n)
        ```

        Ein Satz dazu.
        <!-- /solution -->

        Weiter für alle.

        ```markdown
        <!-- solution -->
        ```
    MD

    def render(markdown)
        markdown = SolutionBlocks.mark(CodeLanguages.mark(markdown))
        html = Redcarpet::Markdown.new(Redcarpet::Render::HTML, :fenced_code_blocks => true).render(markdown)
        root = Nokogiri::HTML(html)
        CodeLanguages.wrap(root)
        SolutionBlocks.wrap(root)
    end

    def test_the_code_is_folded_with_its_markdown
        root = render(PAGE)
        details = root.css('details.solution')
        assert_equal 1, details.size
        assert_equal SolutionBlocks::SUMMARY, details.first.at_css('summary').text
        assert_includes details.first.at_css('pre').text, 'vec reflect'
        assert_includes details.first.text, 'Ein Satz dazu.'
        refute_includes details.first.text, 'Weiter für alle.'
        assert_empty root.css('div.solution-start, div.solution-end')
    end

    def test_markers_in_a_code_block_stay_text
        root = render(PAGE)
        assert_includes root.css('pre').last.text, '<!-- solution -->'
    end

    def test_parts_for_one_language_can_be_folded
        root = render(<<~MD)
            <!-- solution -->
            <!-- language: ruby -->
            ```ruby
            def a; end
            ```
            <!-- /language -->
            <!-- language: python -->
            ```python
            def a(): pass
            ```
            <!-- /language -->
            <!-- /solution -->
        MD
        assert_equal %w[ruby python], root.css('details.solution > div.code-language').map { |b| b['data-language'] }
    end

    def test_a_solution_inside_a_part_for_one_language
        root = render(<<~MD)
            <!-- language: ruby -->
            Ein Satz für Ruby.

            <!-- solution -->
            ```ruby
            def a; end
            ```
            <!-- /solution -->
            <!-- /language -->
            <!-- language: python -->
            <!-- solution -->
            ```python
            def a(): pass
            ```
            <!-- /solution -->
            <!-- /language -->
        MD
        assert_equal %w[ruby python], root.css('div.code-language > details.solution').map { |d| d.parent['data-language'] }
        assert_includes root.at_css('div.code-language[data-language=ruby]').text, 'Ein Satz für Ruby.'
    end

    def test_a_missing_end_is_reported
        messages = []
        markdown = SolutionBlocks.mark("<!-- solution -->\n\n```c\nint x;\n```\n")
        root = Nokogiri::HTML(Redcarpet::Markdown.new(Redcarpet::Render::HTML, :fenced_code_blocks => true).render(markdown))
        SolutionBlocks.wrap(root, :warn => ->(m) { messages << m })
        assert_equal ['<!-- solution --> without <!-- /solution -->'], messages
        assert_equal 1, root.css('details.solution pre').size
    end
end
