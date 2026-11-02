require 'minitest/autorun'
require_relative '../include/uml_diagrams'

# _uml_diagram draws with the real UML-Werkstatt core, so the test needs node
# (in the Ruby container and in CI); without it, it skips.
class UmlDiagramsTest < Minitest::Test
    SCHOOL = <<~UML
        class School
          - name: String
          + School(name: String)

        class Room
          - number: String

        association rooms: School 1 -- 1..* Room
    UML

    def setup
        skip 'node is not installed' unless system('node --version > /dev/null 2>&1')
    end

    def expand(markdown)
        warnings = []
        [UmlDiagrams.expand(markdown, :warn => ->(w) { warnings << w }), warnings]
    end

    def test_the_block_stays_and_the_diagram_comes_under_it
        html, warnings = expand("Vorher\n\n```uml\n#{SCHOOL}```\n\n_uml_diagram\n\nNachher\n")
        assert_empty warnings
        assert html.start_with?("Vorher\n\n```uml\n#{SCHOOL}```\n\n<div class='uml-figure'><svg")
        assert html.end_with?("</div>\n\nNachher\n")
        assert_includes html, 'aria-label="Klassendiagramm. Klassen: School mit name: String, School(name); Room mit number: String. Assoziationen: rooms zwischen School (1) und Room (1..*)."'
        refute_match(/\n[ \t]*\n/, html[html.index('<div'), html.index('</div>') - html.index('<div')])
    end

    def test_only_the_block_right_above_is_drawn
        lamp = "class Lamp\n  - color: String\n"
        html, warnings = expand("```uml\n#{lamp}```\n\nText\n\n```uml\n#{SCHOOL}```\n_uml_diagram\n")
        assert_empty warnings
        assert html.start_with?("```uml\n#{lamp}```\n\nText\n\n```uml\n#{SCHOOL}```\n\n<div class='uml-figure'>")
        refute_includes html, 'Lamp mit'
    end

    def test_umlaut_in_a_comment_and_umlaut_in_the_page
        html, warnings = expand("Größe\n\n```uml\n# Räume\n#{SCHOOL}```\n\n_uml_diagram\n")
        assert_empty warnings
        assert_includes html, "<div class='uml-figure'>"
    end

    def test_an_error_warns_and_leaves_the_line
        markdown = "```uml\nclas School\n```\n\n_uml_diagram\n"
        html, warnings = expand(markdown)
        assert_equal markdown, html
        assert_equal 1, warnings.size
        assert_match(/_uml_diagram: the diagram has 1 error\(s\), line 1/, warnings.first)
    end
end
