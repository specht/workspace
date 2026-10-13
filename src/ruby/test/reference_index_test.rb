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
                           { :level => 3, :text => 'Schlüssel <anlegen>', :id => 'anlegen' }] },
        ])
        doc = Nokogiri::HTML(html)
        assert_equal ['/reference/gitlab#ssh-schluessel', '/reference/gitlab#anlegen'],
            doc.css('.reference-outline a').map { |a| a['href'] }
        assert_equal 'Schlüssel <anlegen>', doc.css('.reference-outline ul a').first.text
        assert_equal 1, doc.css('.reference-block-label img').size
        assert_equal 0, doc.css('.reference-block > img, .tutorial_card3').size
    end
end
