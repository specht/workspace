require 'minitest/autorun'
require_relative '../include/html_lite'

class HtmlLiteTest < Minitest::Test
    SLIDES = <<~HTML
        <!DOCTYPE html>
        <html lang="de">
        <head>
            <meta charset="utf-8">
            <title>Meine Präsentation</title>
            <link rel="stylesheet" href="styles.css">
            <style>section > h2 { color: red; }</style>
        </head>
        <body class="shower list">
            <!-- Titelfolie -->
            <section class="slide" id="titel">
                <h1>Vulkane</h1>
                <p>Lena Muster<br>
                <p>10b
            </section>
            <section class="slide">
                <h2>Wie entsteht ein Vulkan?</h2>
                <ul>
                    <li>Magma
                    <li>Druck <b>steigt</b>
                </ul>
                <img src="pictures/fuji.jpg" alt="Der Fuji">
            </section>
            <script>if (a < b && c > d) {}</script>
        </body>
        </html>
    HTML

    def test_valid_page_has_no_problems_and_can_be_queried
        doc = HtmlLite.parse(SLIDES)
        assert_empty doc.problems.map(&:message)
        slides = doc.select('section.slide')
        assert_equal 2, slides.size
        assert_equal 'titel', slides[0].attrs['id']
        assert_equal 11, slides[0].line
        assert_equal 'Wie entsteht ein Vulkan?', slides[1].at('h2').text
        assert_equal 2, doc.select('section.slide li').size
        assert_equal 'Druck steigt', doc.select('li').last.text.strip
        assert_equal 'pictures/fuji.jpg', doc.at('img').attrs['src']
        assert_includes doc.at('script').text, 'a < b'
        assert_equal %w[shower list], doc.at('body').classes
    end

    def test_unclosed_element_is_reported_with_its_line
        doc = HtmlLite.parse("<section class='slide'>\n<h2>Titel</h2>\n<div>\nText\n</section>\n")
        assert_equal 1, doc.problems.size
        assert_equal 5, doc.problems.first.line
        assert_match(/`<\/section>` kommt, aber `<div>` aus Zeile 3 ist noch offen/, doc.problems.first.message)
    end

    def test_closing_tag_without_opening
        doc = HtmlLite.parse("<p>Text</p>\n</div>\n")
        assert_equal [2], doc.problems.map(&:line)
        assert_match(/`<\/div>` hat kein passendes `<div>`/, doc.problems.first.message)
    end

    def test_open_at_end
        doc = HtmlLite.parse("<section>\n<h2>Titel\n</section>")
        assert_match(/`<\/section>` kommt, aber `<h2>` aus Zeile 2 ist noch offen/, doc.problems.first.message)
        doc = HtmlLite.parse("<div>\n<span>x</span>\n")
        assert_match(/`<div>` aus Zeile 1 wird nie geschlossen/, doc.problems.first.message)
    end

    def test_unterminated_tag_comment_and_quote
        assert_match(/nie mit `-->`/, HtmlLite.parse("<p>a</p>\n<!-- offen\n").problems.first.message)
        assert_match(/`<img` wird nie mit `>`/, HtmlLite.parse("<img src='x.jpg'").problems.first.message)
        doc = HtmlLite.parse("<img src=\"a.jpg alt=\"x\">\n<p>b</p>")
        assert_match(/Bei `src=` in `<img>` fehlt vermutlich/, doc.problems.first.message)
        doc = HtmlLite.parse("<h2 class='x'\n<p>Text</p>")
        assert_match(/fehlt das `>`/, doc.problems.first.message)
    end

    def test_void_and_optional_end_tags
        assert_empty HtmlLite.parse("<br><br/><hr><img src=a alt=''><input type=checkbox checked></br>").problems
        assert_match(/`<\/img>` gibt es nicht/, HtmlLite.parse('<img src=a></img>').problems.first.message)
        doc = HtmlLite.parse("<table><tr><td>a<td>b<tr><td>c</table>")
        assert_empty doc.problems
        assert_equal 2, doc.select('tr').size
        assert_equal 3, doc.select('td').size
    end

    def test_nested_lists_keep_their_items
        doc = HtmlLite.parse("<ul><li>a<ul><li>b<li>c</ul><li>d</ul>")
        assert_empty doc.problems
        assert_equal 2, doc.select('ul').first.elements.size
        assert_equal 2, doc.select('ul ul li').size
    end

    def test_attributes
        doc = HtmlLite.parse(%(<img SRC="a.jpg" alt = 'Ein "Bild"' data-x=1 hidden>))
        img = doc.at('img')
        assert_equal 'a.jpg', img.attrs['src']
        assert_equal 'Ein "Bild"', img.attrs['alt']
        assert_equal '1', img.attrs['data-x']
        assert_equal '', img.attrs['hidden']
    end

    def test_lone_less_than_is_text
        doc = HtmlLite.parse('<p>3 < 4 und 5 > 2</p>')
        assert_empty doc.problems
        assert_equal '3 < 4 und 5 > 2', doc.at('p').text
    end
end
