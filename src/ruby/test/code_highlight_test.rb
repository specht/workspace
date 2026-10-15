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

    # A language CodeHighlight does not know stays a raw <pre><code>, without
    # colours and without a warning (```asm did, on the MiniCPU pages).
    def test_every_fence_language_in_the_content_is_highlighted
        content = File.expand_path('../../content', __dir__)
        used = Hash.new { |h, k| h[k] = [] }
        Dir[File.join(content, '**', '*.md')].each do |file|
            File.read(file, encoding: 'UTF-8').scan(/^ *```([\w-]+)\s*$/).flatten.uniq.each do |language|
                used[language] << file.sub(content + '/', '')
            end
        end
        unknown = used.keys.reject do |language|
            !html("<pre><code class='#{language}'>x</code></pre>").include?('<code')
        end
        assert_empty unknown.map { |l| "#{l}: #{used[l].first(3).join(', ')}" }
    end

    def test_languages_and_task_card_fragments
        assert_includes html("<pre><code class='minicpu'>loop: DEC A</code></pre>"), "class=\"code-minicpu\""
        # PHP: a snippet is PHP from the start, a file only inside <?php … ?>
        assert_includes html("<pre><code class='php'>$stmt = $db-&gt;prepare('SELECT 1');</code></pre>"), '<span class="nv">$stmt</span>'
        file = html("<pre><code class='php'>&lt;h1&gt;Hallo&lt;/h1&gt;\n&lt;?php echo $name; ?&gt;</code></pre>")
        assert_includes file, '<span class="nt">&lt;h1&gt;</span>'
        assert_includes file, '<span class="nv">$name</span>'
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
    # Long listings get the smaller font of the _include_file listings.
    def test_long_code
        long = (1..20).map { |i| "x = #{i}" }.join("\n")
        short = (1..19).map { |i| "x = #{i}" }.join("\n")
        assert_includes html("<pre><code class=\"ruby\">#{long}\n</code></pre>"), 'long-code'
        refute_includes html("<pre><code class=\"ruby\">#{short}\n</code></pre>"), 'long-code'
        refute_includes html("<pre><code class=\"ruby_lineno\">#{long}\n</code></pre>"), 'long-code'
    end
end
