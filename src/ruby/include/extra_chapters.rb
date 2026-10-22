require 'cgi'
require 'nokogiri'

# Optional chapters (»Extras«) of a long activity page. The author writes
# <!-- extra --> on the line before a chapter's ## heading; Redcarpet passes
# the comment through. find (before Main.inject_autotoc) gives the heading
# the class 'extra'; mark (after it) gives its chapter (.autotoc-container)
# the same class, puts a badge into the heading (the tables of contents
# leave it out of their labels and show their own mark, _template.html) and
# a one-line note under the first heading of every run of extra chapters:
# which chapters are extra and where the page goes on for everyone
# (docs/aktivitaet-schreiben.md).
module ExtraChapters
    MARKER = 'extra'
    ICON = 'bi-stars'
    NUMBER_WORDS = %w(null ein zwei drei vier fünf sechs sieben acht neun zehn).freeze

    # Chapters as inject_autotoc sees them.
    def self.chapters(root)
        root.css('h2').reject { |h| h['data-autotoc'] == 'ignore' }
    end

    # The heading's text without the number inject_autotoc put in front
    # and without the badge.
    def self.title(heading)
        copy = heading.dup
        copy.css('.extra-badge').each(&:remove)
        copy.text.strip.sub(/\A\d+\.\s*/, '')
    end

    # An id like the one the left table of contents would give the heading
    # (slugify in _template.html), so a link to it lands there.
    def self.slug(text)
        text.downcase.strip.delete("\u00ad").gsub(/\s+/, '-').gsub(/[^\p{L}\p{N}-]+/, '').squeeze('-').gsub(/\A-|-\z/, '')
    end

    # Before Main.inject_autotoc: each marker gives the heading right after
    # it the class 'extra' and goes. (Afterwards a marker in front of a
    # paragraph could no longer be told from one in front of a heading.)
    def self.find(root, warn: nil)
        root.xpath('//comment()').each do |comment|
            next unless comment.content.strip == MARKER
            target = comment.next_element
            if target && target.name == 'h2'
                target['class'] = [target['class'], 'extra'].compact.join(' ')
            else
                warn&.call("<!-- #{MARKER} --> is not on the line before a ## heading")
            end
            comment.remove
        end
        root
    end

    # After Main.inject_autotoc: the chapter's container gets the class the
    # sticky bar reads, the heading its badge, the first of a run its note.
    def self.mark(root)
        list = chapters(root)
        marked = list.select { |h| h.classes.include?('extra') }
        return root if marked.empty?

        marked.each do |heading|
            container = heading.parent
            if container && container.name == 'div' && container.classes.include?('autotoc-container')
                container['class'] = [container['class'], 'extra'].compact.join(' ')
            end
        end
        extra = ->(heading) { heading.classes.include?('extra') }

        # Notes first (they read the titles), then the badges.
        list.each_with_index do |heading, index|
            next unless extra.(heading)
            next if index > 0 && extra.(list[index - 1])
            run = list[index..].take_while { |h| extra.(h) }.size
            following = list[index + run]
            note = Nokogiri::XML::Node.new('p', root.document)
            note['class'] = 'extra-note'
            note.inner_html = note_html(run, following)
            heading.add_next_sibling(note)
        end
        marked.each do |heading|
            heading.add_child(" <span class='extra-badge'><i class='bi #{ICON}' aria-hidden='true'></i>Extra</span>")
        end
        root
    end

    def self.note_html(run, following)
        unless following
            return run == 1 ?
                'Ein Extra für Schnelle: Die Seite ist ohne dieses Kapitel vollständig.' :
                'Ab hier Extras für Schnelle: Die Seite ist ohne sie vollständig.'
        end
        following['id'] = slug(following.text) if following['id'].to_s.empty?
        link = "<a href='##{CGI.escapeHTML(following['id'])}'>#{CGI.escapeHTML(title(following))}</a>"
        if run == 1
            "Ein Extra für Schnelle. Wer es auslässt, springt zu #{link}."
        else
            others = run == 2 ? 'das nächste Kapitel' : "die nächsten #{NUMBER_WORDS[run - 1] || run - 1} Kapitel"
            "Extras für Schnelle: dieses und #{others}. Wer sie auslässt, springt zu #{link}."
        end
    end
end
