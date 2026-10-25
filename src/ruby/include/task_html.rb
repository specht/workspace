require 'nokogiri'
require 'redcarpet'
require 'set'
require_relative 'external_links'

# Task texts as HTML for the browser: a check's message, its warnings, what
# it read and the solution (the task card and the class page put them in
# with innerHTML). A message mixes the authors' Markdown, which may use
# <kbd> and the like, with text from the student's own files: a CSS line, a
# branch name, a scene's name. Whatever ends up in that mix, the HTML is
# filtered afterwards to a short list of harmless elements and attributes,
# so nothing from a student's file can become markup that runs: no script,
# no event attribute, no javascript: link, no frame.
module TaskHtml
    ELEMENTS = %w[
        p br hr div span code pre kbd samp var strong em b i u s del ins mark small sup sub
        ul ol li dl dt dd blockquote a table thead tbody tfoot tr th td caption
        h1 h2 h3 h4 h5 h6
    ].to_set.freeze

    # Gone with everything inside them; any other element not in ELEMENTS
    # is replaced by its contents.
    DROPPED = %w[script style iframe frame frameset object embed template noscript
                 svg math form input button textarea select option link meta base title].to_set.freeze

    SAFE_HREF = %r{\A(https?://|/(?!/)|#|mailto:)}i

    def self.render(markdown)
        return nil if markdown.nil? || markdown.to_s.strip.empty?

        @renderer ||= Redcarpet::Markdown.new(Redcarpet::Render::HTML.new(:safe_links_only => true),
            :fenced_code_blocks => true, :tables => true)
        sanitize(@renderer.render(markdown.to_s))
    end

    def self.sanitize(html)
        fragment = Nokogiri::HTML5.fragment(html.to_s)
        clean(fragment)
        ExternalLinks.mark(fragment)    # target and rel set after the filter
        fragment.to_html
    end

    def self.clean(node)
        node.children.to_a.each do |child|
            if child.element?
                name = child.name.downcase
                if DROPPED.include?(name)
                    child.remove
                elsif !ELEMENTS.include?(name)
                    clean(child)
                    child.replace(child.children)
                else
                    child.attribute_nodes.each do |attribute|
                        child.remove_attribute(attribute.name) unless allowed?(name, attribute.name.downcase, attribute.value)
                    end
                    clean(child)
                end
            elsif !child.text?
                child.remove
            end
        end
    end

    def self.allowed?(element, name, value)
        case name
        when 'href' then element == 'a' && value.strip.match?(SAFE_HREF)
        when 'title' then true
        when 'class'
            # Bootstrap icons (<i class='bi bi-check-lg'>) and the language of
            # a code block.
            value.match?(/\A(bi( bi-[a-z0-9-]+)*|language-[a-z0-9_+-]+)\z/)
        when 'colspan', 'rowspan' then value.match?(/\A\d{1,2}\z/)
        when 'align' then %w[left right center].include?(value)
        else false
        end
    end
end
