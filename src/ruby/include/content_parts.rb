require 'cgi'
require 'pathname'
require 'yaml'

# Text-level building blocks of the content model (see docs/aufgaben.md):
# finding a page's Markdown file, reference parts, _include() and _uses().
#
# Everything here works on Markdown strings before Redcarpet sees them and
# needs only the standard library, so it can be tested on its own.
module ContentParts
    NAME = /[a-z0-9][a-z0-9-]*/
    PART_MARKER = /^<!--\s*part:\s*(#{NAME})\s*-->[ \t]*$/
    PART_END = /^<!--\s*\/part\s*-->[ \t]*$/
    # _include(slug#part) or _include(slug#part, done: task-id): with done,
    # the box shrinks to one line once that task on the page is solved.
    INCLUDE = /_include\(\s*(#{NAME})#(#{NAME})\s*(?:,\s*done:\s*(#{NAME})\s*)?\)/
    USES = /_uses\(([^)\n]*)\)/
    # <!-- short: … --> right after a part marker: the one line that stands
    # for the whole part once a person has been through it on another page.
    SHORT = /^<!--\s*short:\s*(.*?)\s*-->[ \t]*\n?/
    # Page variables that parts may use. Only these names are replaced, so
    # ${...} in JavaScript or shell examples stays untouched.
    VARIABLES = {
        'template' => '[Adresse der Vorlage]',
        'class_repo' => '[Adresse des Klassen-Repositorys]',
        # Not from the page, from the site's configuration (CONTACT_EMAIL,
        # WEB_ROOT).
        'contact_email' => '[Kontaktadresse]',
        'web_root' => '[Adresse des Workspaces]',
    }.freeze
    SITE_VARIABLES = %w[contact_email web_root].freeze

    # The page's values for VARIABLES, from its meta block (not the site's).
    def self.page_variables(meta)
        (VARIABLES.keys - SITE_VARIABLES).to_h { |name| [name, meta[name].is_a?(String) && !meta[name].strip.empty? ? meta[name].strip : nil] }
    end
    VARIABLE = /\$\{(#{VARIABLES.keys.join('|')})\}/
    NOTE_FILE = 'lehrkraft.md'

    # The Markdown file that is the page of a content folder: <name>.md if it
    # exists, otherwise the first other .md file. Teacher notes and custom
    # pages (+name.md) are never the page.
    def self.page_file(dir, name)
        own = File.join(dir, "#{name}.md")
        return own if File.file?(own)

        Dir[File.join(dir, '*.md')].sort.reject do |path|
            base = File.basename(path)
            base.include?('+') || base == NOTE_FILE
        end.first
    end

    # The YAML of the <div class='meta'> block as a Hash ({} if there is none
    # or it cannot be read).
    def self.meta(markdown)
        m = markdown.match(/<div\s+class=['"]meta['"]>\s*\n(.*?)<\/div>/m)
        return {} unless m

        data = YAML.safe_load(m[1], permitted_classes: [], aliases: false)
        data.is_a?(Hash) ? data : {}
    rescue Psych::Exception
        {}
    end

    # name => Markdown of each part of a reference page. A part starts at
    # <!-- part: name --> and ends at the next part, at <!-- /part -->, or at
    # the end of the file.
    def self.parts(markdown)
        result = {}
        current = nil
        markdown.each_line do |line|
            if (m = line.match(PART_MARKER))
                current = m[1]
                result[current] = +''
            elsif line =~ PART_END
                current = nil
            elsif current
                result[current] << line
            end
        end
        result.transform_values(&:strip)
    end

    # On the reference page itself, a part marker becomes an anchor.
    def self.mark_parts(markdown)
        markdown
            .gsub(SHORT, '')
            .gsub(PART_MARKER) { "<div id='#{$1}' class='reference-part'></div>" }
            .gsub(PART_END, '')
    end

    # Drops the first heading of an included part: the including page writes
    # its own heading.
    # Text of the page's first-level heading (nil if there is none).
    def self.page_title(markdown)
        line = markdown.lines.find { |l| l =~ /^#\s+\S/ }
        line && plain(line.sub(/^#\s+/, ''))
    end

    # Text of the heading a part starts with (nil if it starts without one).
    def self.heading_text(text)
        line = text.lines.find { |l| !l.strip.empty? }
        line && line.start_with?('#') ? plain(line.sub(/^#+\s*/, '')) : nil
    end

    # Heading text without Markdown marks.
    def self.plain(text)
        text.to_s.strip.gsub(/[`*_]/, '').gsub(/<[^>]*>/, '').strip
    end

    # Sub-headings of an included part sit one level below an activity's
    # section (h3), so h4, however deep the part sits on its reference page.
    INCLUDED_SUB_LEVEL = 4

    def self.heading_level(text)
        line = text.lines.find { |l| !l.strip.empty? }
        line && line[/\A(#+)\s/, 1]&.size
    end

    # Moves every heading (outside ``` blocks) by delta levels, within h2..h6.
    def self.shift_headings(text, delta)
        return text if delta.zero?

        fence = false
        text.lines.map do |line|
            fence = !fence if line.start_with?('```')
            next line if fence || line !~ /\A(#+)(\s)/

            level = [[$1.size + delta, 2].max, 6].min
            '#' * level + line.sub(/\A#+/, '')
        end.join
    end

    def self.strip_leading_heading(text)
        lines = text.lines
        index = lines.index { |line| !line.strip.empty? }
        return text if index.nil? || !lines[index].start_with?('#')

        lines.delete_at(index)
        lines.join.strip
    end

    # Relative image and video paths in a part are relative to the reference
    # page; on the including page they must point to the same files.
    def self.rewrite_relative_sources(text, from_dir:, to_dir:)
        fix = lambda do |src|
            return src if src.start_with?('/', '#') || src =~ /\A[a-z]+:/i

            Pathname.new(File.expand_path(src, from_dir)).relative_path_from(Pathname.new(to_dir)).to_s
        end
        # Capture the groups first: fix runs its own regex and resets $1..$3.
        text
            .gsub(/(\bsrc=)(['"])([^'"]+)\2/) { attr, quote, src = $1, $2, $3; "#{attr}#{quote}#{fix.call(src)}#{quote}" }
            .gsub(/(!\[[^\]]*\]\()([^)\s]+)/) { prefix, src = $1, $2; "#{prefix}#{fix.call(src)}" }
    end

    def self.reference_file(reference_root, slug)
        dir = File.join(reference_root, slug)
        File.directory?(dir) ? page_file(dir, slug) : nil
    end

    # Replaces _include(slug#part) with that part of the reference page,
    # framed by two marker divs that the renderer turns into one box.
    # done: names a task on the page; tasks.js folds the box once it is solved.
    # ${name} in the part is replaced from variables (e.g. the page's template).
    def self.expand_includes(markdown, page_dir:, reference_root:, variables: {}, warn: nil)
        markdown.gsub(INCLUDE) do
            slug, part, done = $1, $2, $3
            file = reference_file(reference_root, slug)
            text = file && parts(File.read(file, encoding: 'UTF-8'))[part]
            if text.nil?
                warn&.call("_include(#{slug}##{part}): no such part")
                next "<div class='hint'>Baustein fehlt: #{slug}##{part}</div>"
            end

            short = text[SHORT, 1]
            text = text.sub(SHORT, '')
            source = File.read(file, encoding: 'UTF-8')
            title = heading_text(text)
            label = [page_title(source), title].compact.uniq.join(' › ')
            level = heading_level(text)
            text = strip_leading_heading(text)
            text = shift_headings(text, INCLUDED_SUB_LEVEL - (level + 1)) if level
            text = rewrite_relative_sources(text, from_dir: File.dirname(file), to_dir: page_dir)
            text.scan(VARIABLE).flatten.uniq.each do |name|
                warn&.call("_include(#{slug}##{part}): ${#{name}} has no value on this page") if variables[name].nil?
            end
            text = fill_variables(text, variables)
            done_attrs = done ? " data-done='#{done}'" : ''
            short_attr = short ? " data-short='#{CGI.escapeHTML(inline_html(fill_variables(short, variables)))}'" : ''
            "\n<div class='reference-include-begin'></div>\n\n#{text}\n\n" \
                "<div class='reference-include-end' data-href='/reference/#{slug}##{part}' " \
                "data-label='#{CGI.escapeHTML(label.empty? ? slug : label)}' data-part='#{slug}##{part}' " \
                "data-title='#{CGI.escapeHTML(title || label)}'#{done_attrs}#{short_attr}></div>\n"
        end
    end

    # The parts a page includes, as 'slug#part'.
    def self.included_parts(markdown)
        markdown.scan(INCLUDE).map { |slug, part, _| "#{slug}##{part}" }.uniq
    end

    # A short line of Markdown as HTML: only `code` and <kbd>keys</kbd> are
    # understood, everything else is escaped. Inside `code`, <kbd> is text.
    def self.inline_html(text)
        text.to_s.split(/(`[^`]+`)/).map do |piece|
            if piece.start_with?('`') && piece.end_with?('`') && piece.size > 1
                "<code>#{CGI.escapeHTML(piece[1...-1])}</code>"
            else
                CGI.escapeHTML(piece).gsub(%r{&lt;kbd&gt;([^&<>]+?)&lt;/kbd&gt;}) { "<kbd>#{$1}</kbd>" }
            end
        end.join
    end

    # Task ids named by _include(…, done: id) in a page's Markdown.
    def self.done_tasks(markdown)
        markdown.scan(INCLUDE).map { |m| m[2] }.compact
    end

    # Replaces the known ${name} variables with the page's values, or with a
    # readable placeholder where the page has none (e.g. on the reference
    # page itself).
    def self.fill_variables(markdown, variables = {})
        markdown.gsub(VARIABLE) { (variables[$1] || VARIABLES[$1]).to_s }
    end

    # Replaces _uses(a, b) with small links to the parts a and b on the
    # reference pages listed in the page's meta field reference.
    def self.expand_uses(markdown, references:, reference_root:, warn: nil)
        index = {}
        Array(references).each do |slug|
            file = reference_file(reference_root, slug.to_s)
            if file.nil?
                warn&.call("reference page #{slug} does not exist")
                next
            end
            parts(File.read(file, encoding: 'UTF-8')).each do |name, text|
                # The chip shows the part's heading (»Wenn eine Regel nicht
                # wirkt«), not its name (wirkt-nicht).
                index[name] ||= [slug, heading_text(text.sub(SHORT, '')) || name]
            end
        end
        markdown.gsub(USES) do
            names = $1.split(',').map(&:strip).reject(&:empty?)
            links = names.map do |name|
                slug, title = index[name]
                if slug
                    "<a href='/reference/#{slug}##{name}' title='Im Handbuch nachlesen'><i class='bi bi-book' aria-hidden='true'></i>#{CGI.escapeHTML(title)}</a>"
                else
                    warn&.call("_uses(#{name}): not found in #{Array(references).join(', ')}")
                    "<span>#{name}</span>"
                end
            end
            "\n<div class='uses'>#{links.join}</div>\n"
        end
    end
end
