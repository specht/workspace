require 'cgi'
require 'nokogiri'

# An activity in parts (meta parts:, docs/aktivitaet-schreiben.md »Eine
# Aktivität in Teilen«): one page per chapter (##), for a long activity that
# is one course (one tasks.yaml, one teacher note, one card), read over
# several lessons.
#
#   /slug      the first chapter, with the abstract
#   /slug/2 …  the others
#
# The Markdown stays one file, so recipes, tasks, stages and tests see it as
# before; only the rendered page is split. Each part keeps what stands
# before the first chapter (title, scripts, the trigger of the side
# contents) and gets, under the title, a row of all parts with the current
# one marked, and at its end a link to the next. The other chapters stay as
# empty containers with their label and address (data-href): the bar of
# chapters at the top lists all parts and opens them (_template.html), and
# their anchors (data-anchors, the ids the browser gives headings) send an
# old link like /slug#ein-ton-aus-zahlen to the part that has it.
module ActivityParts
    # The address of part number (from 1).
    def self.url(slug, number)
        number == 1 ? "/#{slug}" : "/#{slug}/#{number}"
    end

    # The chapters of a rendered page: the containers inject_autotoc made,
    # one per h2.
    def self.chapters(root)
        root.css('div.autotoc-container')
    end

    # The id the browser gives a heading (slugify in _template.html), so
    # that the server knows which part has it.
    def self.slugify(text)
        text.to_s.downcase.strip
            .gsub('&shy;', '').delete("­")
            .gsub(/\s+/, '-')
            .gsub(/[^\p{L}\p{N}-]+/u, '')
            .gsub(/-+/, '-')
            .gsub(/\A-|-\z/, '')
    end

    # One document per chapter, or [] when the page has fewer than two.
    # Returns [[root, title], …].
    def self.split(root, slug)
        containers = chapters(root)
        return [] if containers.size < 2

        titles = containers.map { |c| c['data-label'].to_s.strip }
        anchors = containers.map do |c|
            c.css('h2, h3, h4, h5').map { |h| slugify(h.text) }.reject(&:empty?).uniq
        end
        (0...containers.size).map do |i|
            doc = root.dup
            parts = chapters(doc).to_a
            parts.each_with_index do |container, j|
                next if j == i

                stub = Nokogiri::XML::Node.new('div', doc)
                stub['class'] = container['class']
                stub['data-label'] = container['data-label']
                stub['data-href'] = url(slug, j + 1)
                stub['data-anchors'] = anchors[j].join(' ')
                container.replace(stub)
            end
            doc.css('p.abstract').each(&:remove) if i > 0
            nav = Nokogiri::HTML::DocumentFragment.parse(nav_html(slug, titles, i + 1))
            h1 = doc.at_css('h1')
            h1 ? h1.add_next_sibling(nav) : parts[i].add_previous_sibling(nav)
            parts[i].add_child(Nokogiri::HTML::DocumentFragment.parse(next_html(slug, titles, i + 2))) if i + 1 < titles.size
            [doc, titles[i]]
        end
    end

    # The row of parts under the title: number and title of each, the
    # current one marked and not a link.
    def self.nav_html(slug, titles, current)
        items = titles.each_with_index.map do |title, index|
            number = index + 1
            label = CGI.escapeHTML(title.sub(/\A\d+\.\s*/, ''))
            inner = "<span class='activity-part-number'>#{number}</span><span class='activity-part-title'>#{label}</span>"
            if number == current
                "<li><span class='activity-part current' aria-current='page'>#{inner}</span></li>"
            else
                "<li><a class='activity-part' href='#{url(slug, number)}'>#{inner}</a></li>"
            end
        end
        "<nav class='activity-parts' aria-label='Teile'><ol>#{items.join}</ol></nav>"
    end

    # At the end of a part: on to the next.
    def self.next_html(slug, titles, number)
        label = CGI.escapeHTML(titles[number - 1].sub(/\A\d+\.\s*/, ''))
        "<p class='activity-part-next'><a class='btn btn-outline-secondary' href='#{url(slug, number)}'>" \
            "Weiter mit Teil #{number}: #{label} <i class='bi bi-arrow-right' aria-hidden='true'></i></a></p>"
    end
end
