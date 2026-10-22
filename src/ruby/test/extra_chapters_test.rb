require 'minitest/autorun'
require 'redcarpet'
require 'nokogiri'
require_relative '../include/extra_chapters'

# Optional chapters: <!-- extra --> on the line before a ## heading gives
# the heading a badge, its chapter the class the tables of contents read,
# and the first of a run a note that says where the page goes on.
class ExtraChaptersTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    # Like Main.render_content_markdown and Main.inject_autotoc (main.rb):
    # Redcarpet, then each h2 with what follows it in a numbered
    # .autotoc-container.
    def render(markdown)
        html = Redcarpet::Markdown.new(Redcarpet::Render::HTML, :fenced_code_blocks => true).render(markdown)
        root = Nokogiri::HTML(html)
        warnings = []
        ExtraChapters.find(root, :warn => ->(message) { warnings << message })
        h2_list = root.css('h2')
        if h2_list.size > 1
            h2_list.each_with_index do |h2, index|
                container = Nokogiri::XML::Node.new('div', root)
                container['class'] = 'autotoc-container'
                label = "#{index + 1}. #{h2.text}".strip
                h2.content = label
                container['data-label'] = label
                h2.add_previous_sibling(container)
                container.add_child(h2)
                node = container.next_element
                while node && node.name != 'h2'
                    following = node.next_element
                    container.add_child(node)
                    node = following
                end
            end
        end
        ExtraChapters.mark(root)
        [root, warnings]
    end

    PAGE = <<~EOS
        # Seite

        ## Anfang

        Text.

        ## Kern

        Mehr.

        <!-- extra -->
        ## Tiefer

        Für Schnelle.

        <!-- extra -->
        ## Noch tiefer

        Auch.

        ## Wie geht es weiter?

        Ende.
    EOS

    def test_badge_class_and_container
        root, warnings = render(PAGE)
        assert_empty warnings
        extra = root.css('h2.extra')
        assert_equal ['3. Tiefer', '4. Noch tiefer'], extra.map { |h| h.children.first.text.strip }
        assert_equal ['Tiefer', 'Noch tiefer'], extra.map { |h| ExtraChapters.title(h) }
        extra.each do |heading|
            badge = heading.at_css('.extra-badge')
            refute_nil badge
            assert_equal 'Extra', badge.text
            refute_nil badge.at_css('i.bi.bi-stars[aria-hidden]')
            assert_includes heading.parent.classes, 'autotoc-container'
            assert_includes heading.parent.classes, 'extra'
        end
        # The label of the sticky bar stays without the badge.
        assert_equal ['3. Tiefer', '4. Noch tiefer'], root.css('.autotoc-container.extra').map { |c| c['data-label'] }
        assert_equal 2, root.css('.autotoc-container.extra').size
        assert_equal 3, root.css('.autotoc-container:not(.extra)').size
        assert_empty root.xpath('//comment()')
    end

    def test_note_under_first_of_a_run_links_to_the_next_core_chapter
        root, = render(PAGE)
        notes = root.css('p.extra-note')
        assert_equal 1, notes.size
        assert_equal 'Tiefer', ExtraChapters.title(notes.first.previous_element)
        assert_match(/dieses und das nächste Kapitel/, notes.first.text)
        link = notes.first.at_css('a')
        assert_equal 'Wie geht es weiter?', link.text
        target = root.css('h2').find { |h| h['id'] == link['href'].delete_prefix('#') }
        refute_nil target
        assert_equal '5-wie-geht-es-weiter', target['id']
    end

    def test_trailing_extras_say_the_page_is_complete_without_them
        root, = render("## A\n\nx\n\n<!-- extra -->\n## B\n\ny\n\n<!-- extra -->\n## C\n\nz\n")
        notes = root.css('p.extra-note')
        assert_equal 1, notes.size
        assert_match(/Ab hier Extras für Schnelle: Die Seite ist ohne sie vollständig/, notes.first.text)

        root, = render("## A\n\nx\n\n<!-- extra -->\n## B\n\ny\n")
        assert_match(/ohne dieses Kapitel vollständig/, root.at_css('p.extra-note').text)
    end

    def test_two_runs_get_a_note_each
        root, = render("## A\n\n<!-- extra -->\n## B\n\n## C\n\n<!-- extra -->\n## D\n\n## E\n")
        notes = root.css('p.extra-note')
        assert_equal 2, notes.size
        assert_match(/Ein Extra für Schnelle\. Wer es auslässt, springt zu C\./, notes[0].text)
        assert_match(/springt zu E\./, notes[1].text)
    end

    def test_marker_not_before_a_heading_warns
        root, warnings = render("## A\n\n<!-- extra -->\n\nEin Absatz.\n\n## B\n")
        assert_equal 1, warnings.size
        assert_empty root.css('.extra')
    end

    def test_without_marker_nothing_changes
        root, = render("## A\n\n<!-- tutorial-scope -->\n\n## B\n")
        assert_empty root.css('.extra, .extra-note, .extra-badge')
    end

    # In the pages themselves: a marker stands directly before a ## heading,
    # and an extra chapter has no task, or »x von y Aufgaben« could never
    # be complete for someone who leaves the extras out.
    def test_markers_in_content
        problems = []
        Dir[File.join(CONTENT, '**', '*.md')].each do |file|
            lines = File.readlines(file, encoding: 'UTF-8').map(&:rstrip)
            next unless lines.any? { |l| l.strip == '<!-- extra -->' }
            name = file.sub(CONTENT + '/', '')
            in_extra = false
            in_code = false
            lines.each_with_index do |line, i|
                in_code = !in_code if line.start_with?('```')
                next if in_code
                if line.strip == '<!-- extra -->'
                    problems << "#{name}:#{i + 1}: not on the line before a ## heading" unless lines[i + 1].to_s.start_with?('## ')
                elsif line.start_with?('## ')
                    in_extra = lines[i - 1].to_s.strip == '<!-- extra -->'
                elsif in_extra && line =~ /\A_task\(/
                    problems << "#{name}:#{i + 1}: #{line} in an extra chapter"
                end
            end
        end
        assert_empty problems, problems.join("\n")
    end
end
