require 'cgi'
require 'json'

# The Handbuch start page: one card per reference page with its chapters as
# links, and a full-text search over every section, so a reader finds
# »Bilder zuschneiden« or the »public key« without opening every page.
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
            id = if heading['id']
                heading['id']
            elsif part_id(heading)
                part_id(heading)
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

    # The text of each h2 and h3 section of an anchored page, for the search:
    # [{ id:, heading:, text:, code:, lead: }]. A section runs to the next h2
    # or h3. code holds the inline code of the section (`ls`, `<table>`), one
    # piece each, so the search ranks a section that names a command above
    # one where the letters stand inside a word (»als«).
    def self.search_sections(root)
        root.css('h2, h3').map do |heading|
            id = heading['id'] || part_id(heading)
            next nil unless id
            text = +''
            code = []
            lead = nil
            node = heading.next_sibling
            while node && !(node.element? && %w[h2 h3].include?(node.name))
                text << ' ' << plain_text(node)
                code.concat(inline_code(node))
                lead ||= clean(node.text) if node.element? && node.name == 'p' && !clean(node.text).empty?
                node = node.next_sibling
            end
            { :id => id, :heading => heading_text(heading), :text => clean(text)[0, 4000],
              :code => code.uniq.join(' ')[0, 1000], :lead => lead }
        end.compact
    end

    # What a reader never sees: the recipes of the screenshots and other
    # comments, scripts, styles, templates.
    INVISIBLE = %w[script style template noscript].freeze

    # The texts of the code elements in a node that are not in a code block.
    def self.inline_code(node)
        return [] unless node.element?
        codes = node.name == 'code' ? [node] : node.css('code')
        codes.reject { |c| c.ancestors('pre').any? || c.ancestors(INVISIBLE.join(', ')).any? }
            .map { |c| clean(c.text) }.reject(&:empty?)
    end

    def self.clean(text)
        text.to_s.delete("\u00ad").gsub(/\s+/, ' ').strip
    end

    # The first sentence of a section, for a chapter without sections of its
    # own: »Farbnamen« gets a line like a chapter of »HTML-Folien« has.
    def self.lead_sentence(text, limit = 110)
        text = text.to_s.strip
        return nil if text.empty?
        sentence = text[/\A.+?[.!?](?=\s|\z)/] || text
        return sentence if sentence.size <= limit
        "#{sentence[0, limit].sub(/\s+\S*\z/, '')} …"
    end

    BLOCKS = %w[p li td th tr dt dd div pre br h4 h5 h6 table ul ol blockquote figcaption].freeze

    # Text of a node with a space after each block, so table cells and list
    # items do not run into each other (»falsch BOOLEAN«, not »falschBOOLEAN«).
    # Only what the page shows: a comment (the recipe of a screenshot, a
    # part marker) or a script gives nothing.
    def self.plain_text(node)
        return node.text if node.text? || node.cdata?
        return '' unless node.element? && !INVISIBLE.include?(node.name)
        copy = node.dup
        copy.css(INVISIBLE.join(', ')).remove
        copy.css(BLOCKS.join(', ')).each { |block| block.add_next_sibling(Nokogiri::XML::Text.new(' ', block.document)) }
        copy.text
    end

    # The id of the part marker right before a heading, which is its anchor.
    def self.part_id(heading)
        part = heading.previous_element
        part['id'] if part && part.name == 'div' && part['class'].to_s.split.include?('reference-part')
    end

    # pages: [{ slug:, title:, outline:, sections:, section_label:, section_icon: }]
    #
    # One card per page: its area and title, then its chapters (h2)
    # as links, each with its sections (h3) in one line. While a search is
    # typed, a list of matching sections replaces the cards (reference.html).
    def self.html(pages)
        return "<p>Hier gibt es noch keine Seiten.</p>" if pages.empty?

        io = +"<div class='reference-index'>"
        io << "<div class='reference-search'><i class='bi bi-search' aria-hidden='true'></i><input type='search' class='form-control' id='reference-filter' " \
              "placeholder='Im Handbuch suchen, z. B. Bild zuschneiden, SSH, primary key' aria-label='Im Handbuch suchen' autocomplete='off'></div>"
        io << "<div class='reference-results' hidden aria-live='polite'></div>"
        io << "<div class='reference-cards'>"
        pages.each do |page|
            url = "/#{page[:slug]}"
            io << "<section class='reference-card'>"
            io << "<div class='reference-card-label'>"
            io << "<img alt='' src='#{h(page[:section_icon])}'>" if page[:section_icon]
            io << "#{h(page[:section_label])}</div>"
            io << "<h2 data-autotoc='ignore'><a href='#{h(url)}'>#{h(strip_tags(page[:title]))}</a></h2>"
            chapters = chapters(page[:outline] || [])
            leads = (page[:sections] || []).to_h { |section| [section[:id], section[:lead]] }
            unless chapters.empty?
                io << "<ul class='reference-outline'>"
                chapters.each do |chapter|
                    io << "<li><a href='#{h(url)}##{h(chapter[:head][:id])}'>#{h(chapter[:head][:text])}</a>"
                    if !chapter[:items].empty?
                        io << "<span class='reference-outline-sub'>"
                        io << chapter[:items].map { |item| "<a href='#{h(url)}##{h(item[:id])}'>#{h(item[:text])}</a>" }.join(', ')
                        io << '</span>'
                    elsif (lead = lead_sentence(leads[chapter[:head][:id]]))
                        io << "<span class='reference-outline-sub'>#{h(lead)}</span>"
                    end
                    io << '</li>'
                end
                io << '</ul>'
            end
            io << '</section>'
        end
        io << '</div>'
        index = pages.map do |page|
            { :url => "/#{page[:slug]}", :page => strip_tags(page[:title]), :area => page[:section_label].to_s,
              :words => page[:search_words] || [],
              :sections => (page[:sections] || []).map { |s| { :id => s[:id], :heading => s[:heading], :text => s[:text], :code => s[:code].to_s } } }
        end
        # </script> inside the text must not end the script element.
        io << "<script type='application/json' id='reference-search-index'>#{index.to_json.gsub('</', '<\\/')}</script>"
        io << '</div>'
        io
    end

    def self.strip_tags(html)
        CGI.unescapeHTML(html.to_s.gsub(/<[^>]*>/, '')).delete("\u00ad").strip
    end

    # The page's h2 headings, each with the h3 headings below it:
    # [{ head:, items: [...] }]. h3 headings before the first h2 stand alone.
    def self.chapters(outline)
        groups = []
        outline.each do |item|
            if item[:level] == 2 || groups.empty?
                groups << { :head => item, :items => [] }
            else
                groups.last[:items] << item
            end
        end
        groups
    end
end
