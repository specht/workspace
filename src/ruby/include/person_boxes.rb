require 'cgi'
require './include/content_parts.rb'
require './include/person_sources.rb'

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
            text = with_sources(ContentParts.strip_leading_heading(text), sources_html(name, sources(source)))
            "\n#{ContentParts.rewrite_relative_sources(text, from_dir: File.dirname(source), to_dir: page_dir)}\n"
        end
    end

    # The checked sources next to the Personen page (sources.yaml,
    # PersonSources), { name => claims }.
    def self.sources(source)
        source ? PersonSources.read_sources(File.dirname(source)) : {}
    end

    # A small info icon in the box's corner; behind it »Mehr dazu«, the
    # sources its sentences were checked against, for whoever wants to read
    # on. Nothing when a box has none.
    def self.sources_html(name, sources)
        links = PersonSources.reading(sources[name])
        return '' if links.empty?

        items = links.map do |l|
            "<li><a href='#{CGI.escapeHTML(l[:url])}' target='_blank' rel='noopener'>#{CGI.escapeHTML(l[:label])}</a></li>"
        end.join
        "<details class='person-sources'><summary title='Mehr dazu' aria-label='Mehr dazu: Quellen'>" \
            "<i class='bi bi-info-circle' aria-hidden='true'></i></summary>" \
            "<div class='person-sources-list'><strong>Mehr dazu</strong><ul>#{items}</ul></div></details>\n"
    end

    # html right after the box's opening line
    def self.with_sources(text, html)
        return text if html.empty?

        text.sub(/^<div class=['"]person\b[^>]*>[ \t]*\n/) { |line| line + html }
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

    # On the Personen page: in each box, before the photo's source (or at its
    # end when there is none), the pages that place the person.
    def self.annotate(markdown, uses, sources = {})
        out = []
        current = nil
        done = false
        line_for = lambda do |name|
            links = uses[name].map { |page| "<a href='#{CGI.escapeHTML(page[:url])}'>#{CGI.escapeHTML(page[:title])}</a>" }
            "<p class='person-pages'>Im Workspace: #{links.join(', ')}</p>\n"
        end
        markdown.each_line do |line|
            if current && line =~ /^<div class=['"]person\b/
                out << line
                out << sources_html(current, sources)
                next
            end
            if line =~ ContentParts::PART_MARKER
                current = $1
                done = false
            elsif current && !done && !uses[current].empty? && line.start_with?("<p class='person-credit'>")
                out << line_for.call(current)
                done = true
            elsif current && line =~ ContentParts::PART_END
                if !done && !uses[current].empty?
                    last = out.rindex { |l| l.start_with?('</div>') }
                    out.insert(last, line_for.call(current)) if last
                end
                current = nil
            end
            out << line
        end
        out.join
    end
end
