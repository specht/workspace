require 'minitest/autorun'
require_relative '../include/markdown_attributes'

# < and > inside the quoted values of HTML tags in Markdown, which Redcarpet
# would take as the end of the tag (MarkdownAttributes)
class MarkdownAttributesTest < Minitest::Test
    def test_brackets_in_an_alt_text_are_escaped
        line = %(<img src='wenn.svg' alt='Die Formel =WENN(E33>=4;"ja";"nein")'>\n)
        assert_equal %(<img src='wenn.svg' alt='Die Formel =WENN(E33&gt;=4;"ja";"nein")'>\n), MarkdownAttributes.escape(line)
    end

    def test_double_quotes_and_less_than
        assert_equal %(<img alt="t &lt; 0">\n), MarkdownAttributes.escape(%(<img alt="t < 0">\n))
    end

    def test_text_and_tags_without_brackets_stay
        text = "Ein Pfeil -> und <b>fett</b>, a < b.\n<img src='x.svg' alt='nichts'>\n"
        assert_equal text, MarkdownAttributes.escape(text)
    end

    def test_code_stays
        text = "```html\n<img alt='a>b'>\n```\nIm Text `<img alt='a>b'>` als Code.\n"
        assert_equal text, MarkdownAttributes.escape(text)
    end
end
