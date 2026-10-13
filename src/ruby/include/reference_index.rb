require 'cgi'

# The Handbuch start page: one block per reference page with its headings as
# links, so a reader finds »Bilder zuschneiden« without opening every page.
# No pictures; this is an index to look things up, not a menu to browse.
module ReferenceIndex
    UMLAUTS = { 'ä' => 'ae', 'ö' => 'oe', 'ü' => 'ue', 'ß' => 'ss', 'Ä' => 'ae', 'Ö' => 'oe', 'Ü' => 'ue' }.freeze

    # »Bilder zuschneiden« -> »bilder-zuschneiden«.
    def self.slug(text)
        text.to_s.delete("­").gsub(/[äöüßÄÖÜ]/, UMLAUTS).downcase
            .gsub(/[^a-z0-9]+/, '-').gsub(/\A-+|-+\z/, '')
    end

    def self.heading_text(node)
        node.text.delete("­").gsub(/\s+/, ' ').strip
    end

    # Gives every h2 and h3 of a reference page an anchor and returns the
    # outline [{ level:, text:, id: }]. A heading right after a part marker
    # (<div id='part' class='reference-part'>) is reached by the part's name,
    # so links into the page and _include() boxes agree.
    def self.anchor_headings(root)
        taken = root.css('[id]').map { |n| n['id'] }.to_h { |id| [id, true] }
        root.css('h2, h3').map do |heading|
            text = heading_text(heading)
            next nil if text.empty?
            part = heading.previous_element
            id = if heading['id']
                heading['id']
            elsif part && part.name == 'div' && part['class'].to_s.split.include?('reference-part')
                part['id']
            else
                base = slug(text)
                base = 'abschnitt' if base.empty?
                candidate = base
                n = 2
                while taken[candidate]
                    candidate = "#{base}-#{n}"
                    n += 1
                end
                taken[candidate] = true
                heading['id'] = candidate
            end
            { :level => heading.name[1].to_i, :text => text, :id => id }
        end.compact
    end

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    # pages: [{ slug:, title:, outline:, section_label:, section_icon: }]
    def self.html(pages)
        return "<p>Hier gibt es noch keine Seiten.</p>" if pages.empty?

        io = +"<div class='reference-index'>"
        io << "<div class='reference-filter'><input type='search' class='form-control' id='reference-filter' " \
              "placeholder='Im Handbuch suchen, z. B. Bild, Farbe, SSH' aria-label='Im Handbuch suchen' autocomplete='off'></div>"
        io << "<div class='reference-blocks'>"
        pages.each do |page|
            url = "/#{page[:slug]}"
            io << "<section class='reference-block'>"
            io << "<div class='reference-block-label'>"
            io << "<img alt='' src='#{h(page[:section_icon])}'>" if page[:section_icon]
            io << "#{h(page[:section_label])}</div>"
            io << "<h2 data-autotoc='ignore'><a href='#{h(url)}'>#{page[:title]}</a></h2>"
            io << outline_html(url, page[:outline] || [])
            io << '</section>'
        end
        io << '</div>'
        io << "<p class='reference-filter-empty' hidden>Nichts gefunden. Versuch ein anderes Wort.</p>"
        io << '</div>'
        io
    end

    # h2 headings as a list, the h3 headings below each as a run of links.
    def self.outline_html(url, outline)
        return '' if outline.empty?

        groups = []
        outline.each do |item|
            if item[:level] == 2 || groups.empty?
                groups << { :head => item, :items => [] }
            else
                groups.last[:items] << item
            end
        end
        io = +"<ul class='reference-outline'>"
        groups.each do |group|
            head = group[:head]
            # A heading with subheadings reads as a group label, one without as a link.
            css = group[:items].empty? ? '' : " class='reference-outline-head'"
            io << "<li><a#{css} href='#{h(url)}##{h(head[:id])}'>#{h(head[:text])}</a>"
            unless group[:items].empty?
                io << "<ul>"
                group[:items].each { |item| io << "<li><a href='#{h(url)}##{h(item[:id])}'>#{h(item[:text])}</a></li>" }
                io << '</ul>'
            end
            io << '</li>'
        end
        io << '</ul>'
        io
    end
end
