require 'cgi'
require './include/content_parts.rb'

# Person boxes (docs/aktivitaet-schreiben.md): who a page names, with a photo
# and a few sentences. Every person is a part of the Handbuch page Personen
# (src/content/reference/personen), a heading and the box; a page places the
# box with _person(name) on a line of its own. The Personen page says for
# each person on which pages they appear.
module PersonBoxes
    PAGE = 'personen'
    PERSON = /^_person\((#{ContentParts::NAME})\)[ \t]*$/

    def self.file(reference_root)
        ContentParts.reference_file(reference_root, PAGE)
    end

    # Replaces _person(name) with the box alone: no heading, no Handbuch
    # frame, the photo's path pointing to the Personen page's folder.
    def self.expand(markdown, page_dir:, reference_root:, warn: nil)
        return markdown unless markdown =~ PERSON

        source = file(reference_root)
        parts = source ? ContentParts.parts(File.read(source, encoding: 'UTF-8')) : {}
        markdown.gsub(PERSON) do
            name = $1
            text = parts[name]
            if text.nil?
                warn&.call("_person(#{name}): no such person on the page #{PAGE}")
                next "<div class='hint'>Person fehlt: #{name}</div>"
            end
            text = ContentParts.strip_leading_heading(text)
            "\n#{ContentParts.rewrite_relative_sources(text, from_dir: File.dirname(source), to_dir: page_dir)}\n"
        end
    end

    # name => [{ url:, title: }] of the pages that place each person, in the
    # catalogs' order. pages: [[url, Markdown file]] of the pages a reader
    # may be led to (not hidden, not parked).
    def self.uses(pages)
        result = Hash.new { |h, k| h[k] = [] }
        pages.each do |url, path|
            next unless path && File.file?(path)

            markdown = File.read(path, encoding: 'UTF-8')
            title = ContentParts.page_title(markdown) || url
            markdown.scan(PERSON).flatten.uniq.each { |name| result[name] << { :url => url, :title => title } }
        end
        result
    end

    # On the Personen page: in each box, before the photo's source, the pages
    # that place the person.
    def self.annotate(markdown, uses)
        current = nil
        markdown.lines.map do |line|
            current = $1 if line =~ ContentParts::PART_MARKER
            if current && line.start_with?("<p class='person-credit'>") && !uses[current].empty?
                links = uses[current].map { |page| "<a href='#{CGI.escapeHTML(page[:url])}'>#{CGI.escapeHTML(page[:title])}</a>" }
                line = "<p class='person-pages'>Im Workspace: #{links.join(', ')}</p>\n#{line}"
            end
            line
        end.join
    end
end
