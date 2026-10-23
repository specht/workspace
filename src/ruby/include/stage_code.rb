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
# file from stufen/ (or the finished program from loesung/), that file is
# the program at this point of the page. Some pages keep their steps
# elsewhere in their folder (page-start.svelte, step2.lua, lehrkraft/): a
# file counts there when the page writes its target more than once, a
# program that grows. Pictures, archives, one-off examples and programs of
# fewer than MIN_LINES lines (shown whole on the page anyway, a typo to show
# an error) are not steps. A step without a picture
# (<!-- tutorial-step … -->) gets its button right after it. A recipe
# that writes a code block of the page (write-file: lichtspur.c <-
# previous-code, previous-code (LABEL), code:A||B, chosen as the screenshot
# generator chooses it) has the program in that block. A step without a
# recipe says so on a line of its own, with any text file in the page's
# folder:
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
    WRITE_CODE = /\Awrite-file:\s*(\S+)\s*<-\s*(previous-code(?:\s+\([^)]+\))?|code:.+)\z/
    FENCE = /\A```[^`]*\z/
    LABEL = /\A<!--\s*screenshot-code:\s*(.+?)\s*-->\z/
    # files a code block can be a whole program of (not notes, data, Markdown)
    CODE = %w[.c .h .frag .vert .fs .vs .glsl .rb .py .js .mjs .html .css .lua .svelte .sql .tex .bib .room .ini
              .cbl .f90 .pas .java .cs .go .dart .rs .cpp .bas .lisp .st .asm .sh .ts].freeze
    MARKER = /\A<!--\s*stage:\s*(\S+)(?:\s+as\s+(\S+))?\s*-->\s*\z/
    RECIPE = /\A<!-- tutorial-(screenshot|step)\b/
    FOLDERS = %w[stufen/ loesung/].freeze
    MIN_LINES = 5
    # the buttons' wrapper; the code in it is escaped, so no </div> inside
    # ends it early (HTML comments as markers do not survive the rendering)
    STRIP = %r{<div class=["']stage-code["']>.*?</div>}m

    # A file at one step: the name in the Workspace, the file in the page's
    # folder (relative), its text.
    File_ = Struct.new(:target, :source, :text, keyword_init: true)

    # [markdown with a slot at each step, [[File_, …] per slot]]
    def self.mark(markdown, page_dir)
        lines = markdown.lines
        # how often the page writes each target: more than once, it grows
        writes = Hash.new(0)
        lines.each { |l| (m = l.strip.match(/\Awrite-file:\s*(\S+)\s*<-/)) && writes[m[1]] += 1 }
        out = []
        points = []
        last = {}             # target => text written there last
        pending = nil         # files waiting for their place
        wait = nil            # :image (after the next picture) or :gallery (after its end)
        in_recipe = false
        step = false          # the recipe is a tutorial-step, without a picture
        included = false      # inside a Handbuch part (_include): its examples are not steps
        in_gallery = false
        written = []
        blocks = []           # the page's code blocks so far: [label, text without context lines]
        fence = nil           # the block being read
        label = nil           # a screenshot-code label right before a fence
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
            if fence
                if stripped.start_with?('```')
                    blocks << [fence[0], fence[1].reject { |l| l =~ /\A~( |$)/ }.join]
                    fence = nil
                else
                    fence[1] << line
                end
                out << line
                next
            end
            if !in_recipe && line.start_with?('```') && stripped =~ FENCE
                fence = [label, []]
                label = nil
                out << line
                next
            end
            label = (m = stripped.match(LABEL)) ? m[1] : (stripped.empty? ? label : nil)
            included = true if stripped.start_with?("<div class='reference-include-begin'>")
            included = false if stripped.start_with?("<div class='reference-include-end'")
            if in_recipe
                if included
                    # a Handbuch example, not a step of this page
                elsif (m = stripped.match(WRITE)) && (file = read(page_dir, m[2], m[1]))
                    step_folder = FOLDERS.any? { |f| m[2].start_with?(f) }
                    grows = writes[m[1]] > 1 && CODE.include?(File.extname(m[1]).downcase) && long?(file.text)
                    written << file if step_folder || grows
                elsif (m = stripped.match(WRITE_CODE)) && CODE.include?(File.extname(m[1]).downcase) &&
                      (text = code_source(blocks, m[2])) && long?(text)
                    written << File_.new(:target => m[1], :source => nil, :text => text.end_with?("\n") ? text : text + "\n")
                end
                if stripped.include?('-->')
                    in_recipe = false
                    collect.call(written)
                    written = []
                    wait = in_gallery ? :gallery : :image if pending
                    out << line
                    place.call if step && !in_gallery    # no picture follows a tutorial-step
                    next
                end
                out << line
                next
            end
            if (r = stripped.match(RECIPE))
                # the step before gets its box before the next recipe starts
                place.call if pending && wait == :image
                step = r[1] == 'step'
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

    def self.long?(text)
        text.lines.count { |l| !l.strip.empty? } >= MIN_LINES
    end

    # The block a code source names, as the screenshot generator finds it
    # (src/tutorial-screenshots/server.js, findCodeSource), or nil.
    def self.code_source(blocks, selector)
        if selector == 'previous-code'
            blocks.last&.last
        elsif (m = selector.match(/\Aprevious-code\s+\((.+)\)\z/))
            blocks.find { |l, _| l == m[1].strip }&.last
        else
            needles = selector.delete_prefix('code:').split('||').map(&:strip).reject(&:empty?)
            blocks.reverse.find { |_, text| needles.all? { |n| text.include?(n) } }&.last
        end
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
