require 'cgi'
require 'nokogiri'

# The whole code of each step, for teachers (docs/teacher-access.md): a
# teacher can try what a step builds without going through all the steps
# before it.
#
# Where the steps are: activities that build a program step by step keep
# each step's complete program in stufen/, and the page's screenshot recipes
# write it into the Workspace where the page has built it up to (write-file:
# raytracer.c <- file:stufen/05-boden.c). So wherever a recipe writes a text
# file from stufen/, that file is the program at this point of the page;
# pictures, archives and examples from other folders are not steps. A step
# without a recipe says so on a line of its own, with any text file in the
# page's folder:
#
#   <!-- stage: stufen/10-glas.c -->
#   <!-- stage: stufen/shader.fs as light.fs -->
#
# A file shows up only where it differs from the last time the page wrote
# it, so the same program does not come twice. A teacher gets a small button
# at the side for each such file, after the step's picture (after a gallery
# when the recipe is in one), never inside a task card; it copies the whole
# file.
#
# Everyone gets the same page HTML; page_html cuts the buttons and the code
# out for everyone but teachers
# (StageCode.strip), also while a teacher sees the page as a student
# (Nachahmen).
module StageCode
    WRITE = /\Awrite-file:\s*(\S+)\s*<-\s*file:(\S+)\s*\z/
    MARKER = /\A<!--\s*stage:\s*(\S+)(?:\s+as\s+(\S+))?\s*-->\s*\z/
    RECIPE = '<!-- tutorial-screenshot'
    # the buttons' wrapper; the code in it is escaped, so no </div> inside
    # ends it early (HTML comments as markers do not survive the rendering)
    STRIP = %r{<div class=["']stage-code["']>.*?</div>}m

    # A file at one step: the name in the Workspace, the file in the page's
    # folder (relative), its text.
    File_ = Struct.new(:target, :source, :text, keyword_init: true)

    # [markdown with a slot at each step, [[File_, …] per slot]]
    def self.mark(markdown, page_dir)
        lines = markdown.lines
        out = []
        points = []
        last = {}             # target => text written there last
        pending = nil         # files waiting for their place
        wait = nil            # :image (after the next picture) or :gallery (after its end)
        in_recipe = false
        in_gallery = false
        written = []
        place = lambda do
            next if pending.nil? || pending.empty?

            out << "\n<div class='stage-code-slot' data-stage='#{points.size}'></div>\n\n"
            points << pending
            pending = nil
            wait = nil
        end
        collect = lambda do |files|
            changed = files.select { |f| last[f.target] != f.text }
            files.each { |f| last[f.target] = f.text }
            next if changed.empty?

            pending = (pending || []).reject { |f| changed.any? { |c| c.target == f.target } } + changed
        end
        lines.each do |line|
            stripped = line.strip
            if in_recipe
                if (m = stripped.match(WRITE)) && m[2].start_with?('stufen/') && (file = read(page_dir, m[2], m[1]))
                    written << file
                end
                if stripped.include?('-->')
                    in_recipe = false
                    collect.call(written)
                    written = []
                    wait = in_gallery ? :gallery : :image if pending
                end
                out << line
                next
            end
            if stripped.start_with?(RECIPE)
                # the step before gets its box before the next recipe starts
                place.call if pending && wait == :image
                in_recipe = !stripped.end_with?('-->')
                out << line
                next
            end
            if (m = stripped.match(MARKER))
                out << line
                file = read(page_dir, m[1], m[2] || File.basename(m[1]))
                collect.call([file]) if file
                place.call
                next
            end
            # a heading or a task card ends the step: the box comes before it
            place.call if pending && wait == :image && (stripped.start_with?('#') || stripped.start_with?('_task('))
            in_gallery = true if stripped =~ /\A<div class=['"]gallery\b/
            out << line
            if in_gallery && stripped == '</div>'
                in_gallery = false
                place.call if wait == :gallery
            elsif !in_gallery && wait == :image && stripped.start_with?('<img')
                place.call
            end
        end
        place.call
        [out.join, points]
    end

    BINARY = %w[.png .jpg .jpeg .gif .webp .bmp .ico .pdf .gz .tgz .zip .tar .wav .mp3 .ogg .mp4 .ttf .woff .woff2 .bin].freeze

    # The text file source in page_dir, or nil when it is not there, not
    # inside, or not text (a picture, an archive).
    def self.read(page_dir, source, target)
        base = File.expand_path(page_dir)
        path = File.expand_path(source, base)
        return nil unless path.start_with?(base + '/') && File.file?(path)
        return nil if BINARY.include?(File.extname(path).downcase)

        text = File.read(path, :encoding => 'utf-8')
        return nil if !text.valid_encoding? || text.include?("\0")

        File_.new(:target => target, :source => path.delete_prefix(base + '/'), :text => text)
    end

    # Replaces the slots in root (Nokogiri) with the buttons.
    def self.insert(root, points)
        root.css('div.stage-code-slot').each do |slot|
            files = points[slot['data-stage'].to_i] || []
            if files.empty?
                slot.remove
            else
                slot.replace(Nokogiri::HTML::DocumentFragment.parse(buttons(files)))
            end
        end
    end

    # A small button at the side for each file: it copies the whole file
    # (_template.html, .stage-code-copy); the text waits in a <template>.
    def self.buttons(files)
        list = files.map do |f|
            name = CGI.escapeHTML(File.basename(f.target))
            label = files.size == 1 ? 'Code bis hier' : name
            "<button type='button' class='stage-code-copy' title='Für Lehrkräfte: den ganzen Code von #{name} bis hier kopieren'>" \
                "<i class='bi bi-copy' aria-hidden='true'></i>#{label}</button>" \
                "<template>#{CGI.escapeHTML(f.text)}</template>"
        end.join
        "<div class='stage-code'>#{list}</div>"
    end

    # The page without the buttons and their code, for everyone but teachers.
    def self.strip(html)
        html.include?('stage-code') ? html.gsub(STRIP, '') : html
    end
end
