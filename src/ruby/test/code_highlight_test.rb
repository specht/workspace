require 'minitest/autorun'
begin
    require 'nokogiri'
    require_relative '../include/code_highlight'
rescue LoadError
    # nokogiri and rouge are gems of the Ruby service; the test runs there.
end

class CodeHighlightTest < Minitest::Test
    def setup
        skip 'nokogiri or rouge not installed' unless defined?(CodeHighlight) && defined?(Nokogiri)
    end

    def html(fragment)
        f = Nokogiri::HTML.fragment(fragment)
        CodeHighlight.apply(f)
        f.to_html
    end

    # A fence without a language used to stay a raw <pre><code>, which the
    # page styled like inline code, one pink box per line.
    def test_a_fence_without_language_is_plain_text
        out = html("<pre><code>&lt;&lt;&lt;&lt;&lt;&lt;&lt; HEAD\nHOLAAA\n</code></pre>")
        refute_includes out, '<code'
        assert_includes out, '&lt;&lt;&lt;&lt;&lt;&lt;&lt; HEAD'
    end

    def test_languages_and_task_card_fragments
        out = html("<div class='task-prompt'><pre><code class='cypher'>MATCH (m:Movie) RETURN m</code></pre></div>")
        assert_includes out, "class=\"code-cypher\""
        assert_includes out, '<span class="nc">Movie</span>'
        out = html("<pre><code class='er'>entity movie\n  id: int, primary key\n</code></pre>")
        assert_includes out, "class=\"code-er\""
        assert_includes out, '<span class="kp">primary key</span>'
        # An unknown language stays as it is.
        assert_includes html("<pre><code class='mermaid'>graph TD</code></pre>"), "<code class=\"mermaid\">"
    end

    # ```bash_copy: the page puts a copy button on the block; suffixes come
    # in any order.
    def test_copy_marks_the_block_and_keeps_the_language
        out = html("<pre><code class='bash_copy'>wget https://example.com/a.sql | grep a</code></pre>")
        assert_includes out, 'copy-code'
        assert_includes out, '<span class="nb">grep </span>'
        out = html("<pre><code class='bash_copy_wrap'>wget https://example.com/a.sql</code></pre>")
        assert_includes out, 'copy-code'
        assert_includes out, 'wrap-code'
        refute_includes html("<pre><code class='bash'>ls</code></pre>"), 'copy-code'
    end
end
