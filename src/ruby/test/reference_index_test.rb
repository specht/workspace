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

    def test_index_lists_pages_and_headings_without_pictures
        html = ReferenceIndex.html([
            { :slug => 'reference/gitlab', :title => 'Git und GitLab',
              :section_label => 'Git', :section_icon => '/images/penguin.png',
              :outline => [{ :level => 2, :text => 'SSH-Schlüssel', :id => 'ssh-schluessel' },
                           { :level => 2, :text => 'Git sagen, wer du bist', :id => 'git-name' }] },
            { :slug => 'reference/html-folien', :title => 'HTML-Folien',
              :section_label => 'Präsentationen',
              :outline => [{ :level => 2, :text => 'Grundlagen', :id => 'grundlagen' },
                           { :level => 3, :text => 'Listen <ul>', :id => 'listen' },
                           { :level => 2, :text => 'Bilder', :id => 'bilder' },
                           { :level => 3, :text => 'Zuschneiden', :id => 'zuschneiden' }] },
        ])
        doc = Nokogiri::HTML(html)
        cards = doc.css('.reference-card')
        assert_equal ['Git und GitLab', 'Grundlagen', 'Bilder'], cards.map { |c| c.at_css('h2').text }
        assert_equal ['/reference/gitlab#ssh-schluessel', '/reference/gitlab#git-name'], cards[0].css('li a').map { |a| a['href'] }
        assert_equal 'Listen <ul>', cards[1].at_css('li a').text
        assert_equal '/reference/html-folien#grundlagen', cards[1].at_css('h2 a')['href']
        assert_equal 1, doc.css('.reference-card-label img').size
        assert_equal 0, doc.css('.tutorial_card3').size
    end
end
