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
    INCLUDE = /_include\(\s*(#{NAME})#(#{NAME})\s*\)/
    USES = /_uses\(([^)\n]*)\)/
    # Page variables that parts may use. Only these names are replaced, so
    # ${...} in JavaScript or shell examples stays untouched.
    VARIABLES = {
        'template' => '[Adresse der Vorlage]',
    }.freeze
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
    # ${name} in the part is replaced from variables (e.g. the page's template).
    def self.expand_includes(markdown, page_dir:, reference_root:, variables: {}, warn: nil)
        markdown.gsub(INCLUDE) do
            slug, part = $1, $2
            file = reference_file(reference_root, slug)
            text = file && parts(File.read(file, encoding: 'UTF-8'))[part]
            if text.nil?
                warn&.call("_include(#{slug}##{part}): no such part")
                next "<div class='hint'>Baustein fehlt: #{slug}##{part}</div>"
            end

            source = File.read(file, encoding: 'UTF-8')
            label = [page_title(source), heading_text(text)].compact.uniq.join(' › ')
            text = strip_leading_heading(text)
            text = rewrite_relative_sources(text, from_dir: File.dirname(file), to_dir: page_dir)
            text.scan(VARIABLE).flatten.uniq.each do |name|
                warn&.call("_include(#{slug}##{part}): ${#{name}} has no value on this page") if variables[name].nil?
            end
            text = fill_variables(text, variables)
            "\n<div class='reference-include-begin'></div>\n\n#{text}\n\n" \
                "<div class='reference-include-end' data-href='/reference/#{slug}##{part}' " \
                "data-label='#{CGI.escapeHTML(label.empty? ? slug : label)}'></div>\n"
        end
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
            parts(File.read(file, encoding: 'UTF-8')).each_key { |name| index[name] ||= slug }
        end
        markdown.gsub(USES) do
            names = $1.split(',').map(&:strip).reject(&:empty?)
            links = names.map do |name|
                slug = index[name]
                if slug
                    "<a href='/reference/#{slug}##{name}'>#{name}</a>"
                else
                    warn&.call("_uses(#{name}): not found in #{Array(references).join(', ')}")
                    "<span>#{name}</span>"
                end
            end
            "\n<div class='uses'>#{links.join}</div>\n"
        end
    end
end
