require 'minitest/autorun'
require_relative '../include/task_html'

# Messages mix authored Markdown with text from student files; the HTML that
# reaches innerHTML must not contain anything that runs.
class TaskHtmlTest < Minitest::Test
    def html(markdown)
        TaskHtml.render(markdown)
    end

    def test_authored_markdown_and_kbd_stay
        out = html("Speichere mit <kbd>Strg</kbd><kbd>S</kbd> und sieh in `index.html` nach.\n\n- eins\n- zwei")
        assert_includes out, '<kbd>Strg</kbd><kbd>S</kbd>'
        assert_includes out, '<code>index.html</code>'
        assert_includes out, '<li>eins</li>'
    end

    def test_icons_links_and_code_blocks_stay
        out = html("<i class='bi bi-check-lg'></i> [Profil](/profil), [Doku](https://example.com)\n\n```ruby\nputs 1\n```")
        assert_includes out, '<i class="bi bi-check-lg"></i>'
        assert_includes out, '<a href="/profil">Profil</a>'
        assert_includes out, '<a href="https://example.com">Doku</a>'
        assert_includes out, "<pre><code>puts 1\n</code></pre>"
    end

    def test_script_and_event_attributes_are_removed
        out = html("Vorher <script>x()</script> <b onclick=\"x()\">fett</b> <img src=\"a\" onerror=\"x()\"> nachher")
        refute_match(/<script|onclick|onerror|<img/i, out)
        assert_includes out, '<b>fett</b>'
        assert_includes out, 'nachher'
    end

    def test_a_backtick_from_a_student_file_cannot_open_markup
        css = "a`<b onmouseover=\"x()\">b"
        out = html("`#{css[0, 40]}`: Hier fehlt der Doppelpunkt.")
        refute_match(/onmouseover/i, out)
    end

    def test_a_blank_line_from_a_student_file_cannot_open_markup
        value = "x\n\n<iframe src=\"/\"></iframe><a href=\"javascript:x()\">y</a>"
        out = html("Der Name `#{value}` ist ungültig.")
        refute_match(/<iframe|javascript:/i, out)
    end

    # An RFC or a manual opens beside the activity; links within the site
    # stay in the tab.
    def test_links_to_other_sites_open_in_a_new_tab
        out = html("[RFC 6238](https://www.rfc-editor.org/rfc/rfc6238.html) und [Handbuch](/handbuch/http)")
        assert_includes out, '<a href="https://www.rfc-editor.org/rfc/rfc6238.html" target="_blank" rel="noopener">RFC 6238</a>'
        assert_includes out, '<a href="/handbuch/http">Handbuch</a>'
    end

    def test_unsafe_links_lose_their_address
        out = html("[klick](javascript:x()) <a href=\"data:text/html,x\">d</a> <a href=\"//evil.example\">e</a>")
        refute_match(/href="(javascript|data|\/\/)/i, out)
        refute_match(/<a href="javascript/i, out)
    end

    def test_comments_and_frames_are_removed
        out = html("<!-- x --><iframe src=\"/\"></iframe><svg><script>x()</script></svg>Text")
        refute_match(/<!--|<iframe|<svg|<script/i, out)
        assert_includes out, 'Text'
    end

    def test_empty_is_nil
        assert_nil html(nil)
        assert_nil html("  \n")
    end
end
