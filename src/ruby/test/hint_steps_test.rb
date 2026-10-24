require 'minitest/autorun'
require 'nokogiri'
require 'redcarpet'
require_relative '../include/solution_blocks'
require_relative '../include/hint_steps'

class HintStepsTest < Minitest::Test
    PAGE = <<~MD
        <div class='hint own'>
        <strong>Jetzt bist du dran:</strong> Schreib <code>reflect</code>.
        </div>

        <!-- hint -->
        Was passiert mit dem Teil entlang der Normalen?
        <!-- /hint -->
        <!-- hint -->
        ```c
        return sub(dir, …);
        ```
        <!-- /hint -->

        <!-- solution -->
        ```c
        vec reflect(vec dir, vec n)
        ```
        <!-- /solution -->

        ```markdown
        <!-- hint -->
        ```
    MD

    def render(markdown)
        markdown = HintSteps.mark(SolutionBlocks.mark(markdown))
        root = Nokogiri::HTML(Redcarpet::Markdown.new(Redcarpet::Render::HTML, :fenced_code_blocks => true).render(markdown))
        SolutionBlocks.wrap(root)
        HintSteps.wrap(root)
    end

    def test_steps_are_grouped_and_numbered
        root = render(PAGE)
        boxes = root.css('div.hint-steps')
        assert_equal 1, boxes.size
        assert_equal HintSteps::DELAY_SECONDS.to_s, boxes.first['data-delay']
        assert_equal ['Tipp 1 von 2', 'Tipp 2 von 2'], boxes.first.css('details.hint-step > summary').map(&:text)
        assert_includes boxes.first.css('details.hint-step').last.at_css('pre').text, 'return sub(dir'
        assert_equal 1, root.css('details.solution').size
        assert_empty root.css('div.hint-start, div.hint-end')
    end

    def test_markers_in_a_code_block_stay_text
        assert_includes render(PAGE).css('pre').last.text, '<!-- hint -->'
    end

    def test_html_for_a_task
        html = HintSteps.html(['<p>A</p>', '<p>B</p>'], :extra_class => 'task-hint')
        assert_includes html, "<details class='hint-step task-hint'><summary>Tipp 2 von 2</summary><p>B</p></details>"
    end
end
