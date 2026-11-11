require 'minitest/autorun'
require 'nokogiri'
require_relative '../include/activity_parts'

# An activity in parts (ActivityParts): one page per chapter, the row of
# parts, the other chapters as empty containers that know their address and
# anchors, the link to the next part.
class ActivityPartsTest < Minitest::Test
    PAGE = <<~HTML
        <h1>Audio komprimieren</h1>
        <p class='abstract'>Worum es geht.</p>
        <div class='autotoc-container' data-label='1. Ton als Zahlen'><h2>1. Ton als Zahlen</h2><h3>Ein Ton aus Zahlen</h3><p>Eins</p></div>
        <div class='autotoc-container' data-label='2. Kleine Tricks'><h2>2. Kleine Tricks</h2><p>Zwei</p></div>
        <div class='autotoc-container' data-label='3. Frequenzen'><h2>3. Frequenzen</h2><h3>Eine Telefonnummer aus Tönen</h3><p>Drei</p></div>
    HTML

    def parts
        ActivityParts.split(Nokogiri::HTML(PAGE), 'audio')
    end

    def test_one_part_per_chapter_with_its_title
        assert_equal ['1. Ton als Zahlen', '2. Kleine Tricks', '3. Frequenzen'], parts.map(&:last)
    end

    def test_a_part_has_its_chapter_and_empty_containers_for_the_others
        root = parts[1].first
        assert_includes root.text, 'Zwei'
        refute_includes root.text, 'Eins'
        refute_includes root.text, 'Drei'
        stubs = root.css('div.autotoc-container[data-href]')
        assert_equal ['/audio', '/audio/3'], stubs.map { |s| s['data-href'] }
        assert_equal ['1. Ton als Zahlen', '3. Frequenzen'], stubs.map { |s| s['data-label'] }
    end

    def test_the_empty_containers_know_the_anchors_the_browser_gives
        stub = parts[0].first.css('div.autotoc-container[data-href]').last
        assert_equal %w[3-frequenzen eine-telefonnummer-aus-tönen], stub['data-anchors'].split
    end

    def test_the_abstract_only_on_the_first_part
        assert parts[0].first.at_css('p.abstract')
        assert_nil parts[1].first.at_css('p.abstract')
    end

    def test_the_row_of_parts_marks_the_current_one
        nav = parts[1].first.at_css('nav.activity-parts')
        assert nav, 'the row of parts under the title'
        assert_equal 'h1', nav.previous_element.name
        current = nav.at_css('[aria-current=page]')
        assert_equal '2Kleine Tricks', current.text
        assert_equal ['/audio', '/audio/3'], nav.css('a').map { |a| a['href'] }
    end

    def test_each_part_but_the_last_links_to_the_next
        assert_equal '/audio/2', parts[0].first.at_css('.activity-part-next a')['href']
        assert_includes parts[1].first.at_css('.activity-part-next').text, 'Teil 3: Frequenzen'
        assert_nil parts[2].first.at_css('.activity-part-next')
    end

    def test_the_closing_chapter_ends_the_last_part
        page = PAGE.sub('</div>\n', "</div>\n") + "<div class='autotoc-container' data-label='4. Wie geht es weiter?'><h2>4. Wie geht es weiter?</h2><p>Vier</p></div>"
        split = ActivityParts.split(Nokogiri::HTML(page), 'audio')
        assert_equal 3, split.size
        last = split[2].first
        assert_includes last.text, 'Vier'
        assert_equal 'Wie geht es weiter?', last.css('h2').last.text
        refute_includes split[0].first.text, 'Vier'
    end

    def test_a_page_with_one_chapter_is_not_split
        one = "<h1>Kurz</h1><div class='autotoc-container' data-label='1. Alles'><h2>1. Alles</h2></div>"
        assert_empty ActivityParts.split(Nokogiri::HTML(one), 'kurz')
    end

    def test_slugify_as_the_browser
        assert_equal '3-frequenzen', ActivityParts.slugify('3. Frequenzen')
        assert_equal 'μ-law-und-g711', ActivityParts.slugify('μ-law und G.711')
        assert_equal 'leise-stellen-grobe-stufen', ActivityParts.slugify('  Leise Stellen, grobe Stufen ')
    end
end
