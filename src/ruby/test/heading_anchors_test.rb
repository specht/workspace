require 'minitest/autorun'
require 'uri'

# A link to a place on the same page (](#…), href='#…') finds its heading.
# The ids are made in the browser from a heading's text (_template.html,
# slugify), and a chapter's text starts with its number (Main.inject_autotoc):
# »## Quellen« as the ninth chapter is #9-quellen, as a section (###) it is
# #quellen. Regrouping an outline changes them; this finds the links left
# behind, on the page itself and from other pages. Links to tasks
# (#aufgabe-…), to ids in the page's HTML and to parts (#stack) count too.
class HeadingAnchorsTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def self.slugify(text)
        text.downcase.strip.gsub('&shy;', '').gsub(/\s+/, '-').gsub(/[^\p{L}\p{N}-]+/, '')
            .gsub(/-+/, '-').gsub(/\A-|-\z/, '')
    end

    # The ids of a page's headings and elements, as the browser has them.
    def self.ids(markdown)
        prose = markdown.gsub(/^[ \t]*```.*?^[ \t]*```/m, '').gsub(/<!--.*?-->/m, '')
        numbered = prose.scan(/^## /).size > 1
        ids = {}
        chapter = 0
        prose.each_line do |line|
            next unless line =~ /^(##|###) (.+)$/
            level = Regexp.last_match(1)
            text = Regexp.last_match(2).gsub(/`([^`]*)`/, '\1').gsub(/\[([^\]]*)\]\([^)]*\)/, '\1')
                .gsub(/<[^>]+>/, '').delete('*')
            if level == '##'
                chapter += 1
                text = "#{chapter}. #{text}" if numbered
            end
            base = slugify(text)
            id = base
            n = 2
            while ids.key?(id)
                id = "#{base}-#{n}"
                n += 1
            end
            ids[id] = true
        end
        markdown.scan(/\bid=['"]([^'"]+)['"]/).flatten.each { |id| ids[id] = true }
        # a part of a Handbuch page is a place of its own (#stack)
        markdown.scan(/<!-- part: ([\w-]+) -->/).flatten.each { |id| ids[id] = true }
        ids
    end

    def self.links(markdown)
        markdown.scan(/\]\(#([^)\s]+)\)|href=['"]#([^'"]+)['"]/).map do |a, b|
            URI.decode_www_form_component(a || b)
        end
    end

    def test_ids_as_the_browser_makes_them
        page = "## Einrichten\n\n## Quellen\n\n### Eine Formel\n\n```\n## kein Kapitel\n```\n\n## Quellen\n"
        assert_equal %w[1-einrichten 2-quellen eine-formel 3-quellen], HeadingAnchorsTest.ids(page).keys
        assert_equal %w[quellen], HeadingAnchorsTest.ids("## Quellen\n").keys, 'one chapter alone has no number'
    end

    # The Markdown of the page a path names (/rle, /reference/uml), nil for
    # anything else.
    def self.page(path)
        file = if path.start_with?('/reference/')
                   name = path.delete_prefix('/reference/')
                   File.join(CONTENT, 'reference', name, "#{name}.md")
               else
                   name = path.delete_prefix('/')
                   File.join(CONTENT, name, "#{name}.md")
               end
        name.match?(%r{\A[\w-]+\z}) && File.file?(file) ? File.read(file) : nil
    end

    def test_every_link_to_another_page_finds_its_place
        broken = []
        Dir[File.join(CONTENT, '**', '*.{md,yaml}')].each do |file|
            File.read(file).scan(%r{\]\((/(?:reference/)?[\w-]+)#([^)\s]+)\)|href=['"](/(?:reference/)?[\w-]+)#([^'"]+)['"]}).each do |a, b, c, d|
                path, id = a || c, URI.decode_www_form_component(b || d)
                next if id.start_with?('aufgabe-')
                target = HeadingAnchorsTest.page(path)
                next unless target
                # a Handbuch page names its places its own way (the server
                # sets ids, umlauts spelled out); only a number in the id
                # breaks there when the outline changes
                next if path.start_with?('/reference/') && !id.match?(/\A\d+-/)
                broken << "#{file.delete_prefix(CONTENT + '/')}: #{path}##{id}" unless HeadingAnchorsTest.ids(target).key?(id)
            end
        end
        assert_empty broken, 'a link to a heading of another page that is not there; into the Handbuch, ' \
            'link the part (#code) instead of a numbered chapter'
    end

    def test_every_link_on_a_page_finds_its_place
        broken = Dir[File.join(CONTENT, '*', '*.md')].filter_map do |file|
            slug = File.basename(File.dirname(file))
            next unless File.basename(file, '.md') == slug

            markdown = File.read(file)
            ids = HeadingAnchorsTest.ids(markdown)
            missing = HeadingAnchorsTest.links(markdown).reject { |id| id.start_with?('aufgabe-') || ids.key?(id) }
            "#{slug}: #{missing.map { |id| "##{id}" }.join(', ')}" unless missing.empty?
        end
        assert_empty broken, 'a link to a heading that is not there (after regrouping, the number in an id changes)'
    end
end
