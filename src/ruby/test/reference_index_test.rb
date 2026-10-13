require 'minitest/autorun'
require 'nokogiri'
require_relative '../include/reference_index'

class ReferenceIndexTest < Minitest::Test
    def test_slug
        assert_equal 'ssh-schluessel-fuer-gitlab', ReferenceIndex.slug('SSH-Schlüssel für GitLab')
        assert_equal 'bilder-zuschneiden', ReferenceIndex.slug("Bil­der zu­schneiden")
    end

    def test_headings_get_anchors_and_parts_keep_their_names
        root = Nokogiri::HTML(<<~HTML)
            <h1>Seite</h1>
            <div id='grundlagen' class='reference-part'></div>
            <h2>Grundlagen</h2>
            <h3>Text und Über&shy;schriften</h3>
            <h3>Listen</h3>
            <h2>Listen</h2>
            <h3 id='eigen'>Eigene Marke</h3>
        HTML
        outline = ReferenceIndex.anchor_headings(root)
        assert_equal [
            { :level => 2, :text => 'Grundlagen', :id => 'grundlagen' },
            { :level => 3, :text => 'Text und Überschriften', :id => 'text-und-ueberschriften' },
            { :level => 3, :text => 'Listen', :id => 'listen' },
            { :level => 2, :text => 'Listen', :id => 'listen-2' },
            { :level => 3, :text => 'Eigene Marke', :id => 'eigen' },
        ], outline
        assert_nil root.at_css('h2')['id'], 'the part marker is the anchor'
        assert_equal 'listen-2', root.css('h2')[1]['id']
    end

    def test_search_sections_hold_the_text_up_to_the_next_heading
        root = Nokogiri::HTML(<<~HTML)
            <h2 id='ssh'>SSH-Schlüssel</h2>
            <p>Zeige den öffentlichen Schlüssel an:</p>
            <pre>cat ~/.ssh/id_ed25519.pub</pre>
            <script>ignored()</script>
            <h3 id='fehler'>Permission denied</h3>
            <p>Meldet SSH <code>Permission denied (publickey)</code>, fehlt der Schl&shy;üssel.</p>
            <table><tr><td>bool</td><td>wahr oder falsch</td></tr><tr><td>text</td><td>Zeichen</td></tr></table>
            <div id='name' class='reference-part'></div>
            <h2>Git sagen, wer du bist</h2>
        HTML
        sections = ReferenceIndex.search_sections(root)
        assert_equal %w[ssh fehler name], sections.map { |s| s[:id] }
        assert_equal 'Zeige den öffentlichen Schlüssel an: cat ~/.ssh/id_ed25519.pub', sections[0][:text]
        assert_includes sections[1][:text], 'publickey'
        assert_includes sections[1][:text], 'Schlüssel'
        assert_includes sections[1][:text], 'bool wahr oder falsch text Zeichen'
        assert_equal '', sections[2][:text]
    end

    def test_index_has_one_card_per_page_and_a_search_index
        html = ReferenceIndex.html([
            { :slug => 'reference/gitlab', :title => 'Git und GitLab', :abstract => 'Einmal einrichten.',
              :section_label => 'Git', :section_icon => '/images/penguin.png',
              :outline => [{ :level => 2, :text => 'SSH-Schlüssel', :id => 'ssh-schluessel' },
                           { :level => 2, :text => 'Git sagen, wer du bist', :id => 'git-name' }],
              :sections => [{ :id => 'ssh-schluessel', :heading => 'SSH-Schlüssel', :text => 'publickey </script>' }] },
            { :slug => 'reference/html-folien', :title => 'HTML-Folien',
              :section_label => 'Präsentationen',
              :outline => [{ :level => 2, :text => 'Grundlagen', :id => 'grundlagen' },
                           { :level => 3, :text => 'Listen <ul>', :id => 'listen' },
                           { :level => 3, :text => 'Text', :id => 'text' },
                           { :level => 2, :text => 'Bilder', :id => 'bilder' }] },
        ])
        doc = Nokogiri::HTML(html)
        cards = doc.css('.reference-card')
        assert_equal ['Git und GitLab', 'HTML-Folien'], cards.map { |c| c.at_css('h2').text }
        assert_equal 'Einmal einrichten.', cards[0].at_css('.reference-card-abstract').text
        assert_equal ['/reference/gitlab#ssh-schluessel', '/reference/gitlab#git-name'], cards[0].css('.reference-outline > li > a').map { |a| a['href'] }
        assert_equal 'Listen <ul> · Text', cards[1].at_css('.reference-outline-sub').text
        assert_equal 1, doc.css('.reference-card-label img').size
        script = doc.at_css('#reference-search-index')
        refute_includes script.inner_html, '</script>'
        index = JSON.parse(script.text)
        assert_equal '/reference/gitlab', index[0]['url']
        assert_equal 'publickey </script>', index[0]['sections'][0]['text']
        assert_equal [], index[1]['sections']
    end
end
