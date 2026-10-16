require 'cgi'
require 'set'
require 'uri'
require 'pathname'
require_relative 'workspace_files'
require_relative 'html_lite'
require_relative 'css_lite'
require_relative 'task_spec'
require_relative 'story_check'

# The check types that read files in a student's Workspace on the server
# (docs/aufgaben.md): files, html, css, references and story. They only read,
# through WorkspaceFiles, and never execute or decode anything.
#
# Each check returns an Outcome:
#   ok       – true if everything that is expected is there
#   problem  – a code for the first thing that is missing (feedback rules can
#              replace the message with when: { problem: <code> })
#   message  – Markdown for the student, naming real files and lines
#   read     – Markdown: what the check actually read (rule 2: feedback
#              shows what is really there)
#   warnings – hints that do not make the task fail
module FileChecks
    Outcome = Struct.new(:ok, :problem, :message, :read, :warnings, keyword_init: true)

    FORMATS = {
        'elf' => 'ein ausführbares Programm (ELF)',
        'png' => 'ein PNG-Bild',
        'jpeg' => 'ein JPEG-Bild',
        'gif' => 'ein GIF-Bild',
        'webp' => 'ein WebP-Bild',
        'svg' => 'ein SVG-Bild',
        'pdf' => 'ein PDF',
        'zip' => 'ein ZIP-Archiv',
        'text' => 'eine Textdatei',
        'empty' => 'leer',
        'unknown' => 'in einem unbekannten Format',
    }.freeze
    EXTENSION_FORMATS = {
        'jpg' => 'jpeg', 'jpeg' => 'jpeg', 'png' => 'png', 'gif' => 'gif', 'webp' => 'webp',
        'svg' => 'svg', 'pdf' => 'pdf', 'zip' => 'zip',
    }.freeze
    MAX_SHOWN = 3

    def self.ok(read: nil, warnings: [])
        Outcome.new(:ok => true, :read => read, :warnings => warnings)
    end

    def self.failed(problem, message, read: nil, warnings: [])
        Outcome.new(:ok => false, :problem => problem, :message => message, :read => read, :warnings => warnings)
    end

    # »1 Zeile«, »12 Zeilen«.
    def self.lines_text(count)
        count == 1 ? '1 Zeile' : "#{count} Zeilen"
    end

    def self.code(text)
        "`#{text.to_s.gsub('`', "'")}`"
    end

    def self.size_text(bytes)
        # kB and MB as in max_size: 1 kB = 1000 Bytes.
        if bytes < 1000 then "#{bytes} Bytes"
        elsif bytes < 1_000_000 then "#{(bytes / 1000.0).round(1).to_s.sub('.', ',').sub(/,0\z/, '')} kB"
        else "#{(bytes / 1_000_000.0).round(1).to_s.sub('.', ',').sub(/,0\z/, '')} MB"
        end
    end

    # Format of a file from its first bytes.
    def self.detect_format(head, size)
        return 'empty' if size.to_i.zero?

        bytes = head.to_s.b
        return 'elf' if bytes.start_with?("\x7FELF".b)
        return 'png' if bytes.start_with?("\x89PNG\r\n\x1A\n".b)
        return 'jpeg' if bytes.start_with?("\xFF\xD8\xFF".b)
        return 'gif' if bytes.start_with?('GIF87a'.b) || bytes.start_with?('GIF89a'.b)
        return 'webp' if bytes[0, 4] == 'RIFF'.b && bytes[8, 4] == 'WEBP'.b
        return 'pdf' if bytes.start_with?('%PDF'.b)
        return 'zip' if bytes.start_with?("PK\x03\x04".b)

        text = bytes.dup.force_encoding('UTF-8')
        return 'unknown' if !text.valid_encoding? && !text.byteslice(0, text.bytesize - 3).to_s.valid_encoding?
        return 'unknown' if bytes.include?("\0".b)
        return 'svg' if text.scrub =~ /\A\s*(<\?xml[^>]*>\s*)?(<!--.*?-->\s*)*(<!DOCTYPE svg[^>]*>\s*)?<svg[\s>]/mi

        'text'
    end

    def self.first_bytes_hex(head, n = 4)
        head.to_s.b[0, n].unpack('C*').map { |b| format('%02X', b) }.join(' ')
    end

    def self.refused(error, what = nil)
        hint = what ? " #{what}" : ''
        failed(error.problem == :missing ? :missing : error.problem, "#{error.message}#{hint}")
    end

    # A listing of a folder as a Markdown code block.
    # formats: false lists names and sizes only, without opening any file
    # (used when a path is missing, so a folder like .ssh is never read).
    def self.listing(files, rel, formats: true)
        entries = files.list(rel)
        return "#{code(rel)} ist leer." if entries.empty?

        width = entries.map { |e| e[:name].size }.max
        lines = entries.map do |e|
            detail = case e[:type]
                when :dir then 'Ordner'
                when :symlink then 'Link (wird nicht gelesen)'
                when :file
                    if formats
                        format = detect_format(files.head("#{rel}/#{e[:name]}".sub(%r{\A/}, ''), 64), e[:size]) rescue 'unknown'
                        "#{size_text(e[:size])}, #{FORMATS[format]}"
                    else
                        size_text(e[:size])
                    end
                else 'kein normaler Eintrag'
            end
            "#{e[:name].ljust(width)}  #{detail}"
        end
        "Inhalt von #{code(rel.empty? ? '/workspace' : rel)}:\n\n```\n#{lines.join("\n")}\n```"
    end

    # What is wrong with a file name, in words (for name: patterns).
    def self.name_issues(name)
        issues = []
        issues << 'Leerzeichen' if name.include?(' ')
        issues << 'Großbuchstaben' if name =~ /[A-ZÄÖÜ]/
        issues << 'Umlaute oder ß' if name =~ /[äöüÄÖÜß]/
        issues << 'Sonderzeichen' if name =~ /[^A-Za-z0-9äöüÄÖÜß .\-_]/
        issues
    end

    # --- files ----------------------------------------------------------------

    def self.files(check, ws)
        path = check['path']
        expect = check['expect'] || {}
        entry = ws.entry(path)
        show = check['show'] == 'listing'
        parent = path.include?('/') ? File.dirname(path) : ''
        read_parent = lambda { ws.entry(parent)[:type] == :dir ? listing(ws, parent, :formats => false) : nil }

        case entry[:type]
        when :missing
            other = ws.other_case(path)
            message = if other
                "#{code(path)} gibt es nicht, aber #{code(other)}. Linux unterscheidet Groß- und Kleinschreibung."
            else
                "#{code(path)} gibt es nicht (gesucht in `/workspace/#{path}`)."
            end
            return failed(:missing, message, :read => read_parent.call)
        when :symlink
            return failed(:symlink, "#{code(entry[:link])} ist ein symbolischer Link. Links liest der Workspace nicht.")
        end

        if entry[:type] == :file
            return single_file(path, entry, expect, ws)
        end
        if expect['executable'] || expect['format']
            return failed(:wrong_type, "#{code(path)} ist ein Ordner, gesucht ist eine Datei.", :read => read_parent.call)
        end

        read = show ? listing(ws, path) : nil
        return ok(:read => read) unless expect['new_files'] || expect['name'] || expect['format_matches_extension'] || expect['max_size']

        template = (check['template'] || []).to_set
        entries = ws.list(path).select { |e| e[:type] == :file }
        new_files = entries.reject { |e| template.include?(e[:name]) }
        min = (expect['new_files'] || {})['min'].to_i
        if new_files.size < min
            message = if new_files.empty? && !template.empty?
                "In #{code(path)} liegen nur Dateien aus der Vorlage (#{template.to_a.join(', ')}). Lege mindestens #{min == 1 ? 'eine eigene Datei' : "#{min} eigene Dateien"} dazu."
            else
                "In #{code(path)} liegen #{new_files.size} eigene Dateien, verlangt #{min == 1 ? 'ist eine' : "sind #{min}"}."
            end
            return failed(:no_new_files, message, :read => listing(ws, path))
        end

        max = expect['max_size'] ? TaskSpec.parse_size(expect['max_size']) : nil
        pattern = expect['name'] ? Regexp.new(expect['name'], timeout: 0.5) : nil
        new_files.each do |e|
            file = "#{path}/#{e[:name]}"
            if pattern && !pattern.match?(e[:name])
                issues = name_issues(e[:name])
                why = issues.empty? ? "passt nicht zum verlangten Muster #{code(expect['name'])}" : "enthält #{issues.join(', ')}"
                return failed(:bad_name, "Der Name #{code(e[:name])} #{why}. Gute Namen sind kurz, klein geschrieben und verwenden `-` statt Leerzeichen, zum Beispiel `mein-bild.jpg`.", :read => listing(ws, path))
            end
            if expect['format_matches_extension']
                ext = File.extname(e[:name]).delete('.').downcase
                wanted = EXTENSION_FORMATS[ext]
                actual = detect_format(ws.head(file), e[:size])
                if wanted && actual != wanted
                    return failed(:format_mismatch, "#{code(e[:name])} endet auf `.#{ext}`, ist aber #{FORMATS[actual]} (die ersten Bytes sind `#{first_bytes_hex(ws.head(file))}`). Umbenennen ändert das Format nicht.", :read => listing(ws, path))
                end
            end
            if max && e[:size] > max
                return failed(:too_big, "#{code(e[:name])} ist #{size_text(e[:size])} groß; erlaubt sind höchstens #{size_text(max)}.", :read => listing(ws, path))
            end
        end
        ok(:read => read || listing(ws, path))
    rescue WorkspaceFiles::Refused => e
        refused(e)
    end

    def self.single_file(path, entry, expect, ws)
        head = ws.head(path)
        format = detect_format(head, entry[:size])
        read = "#{code(path)}: #{size_text(entry[:size])}, #{FORMATS[format]}, #{(entry[:mode] & 0o111).zero? ? 'nicht ausführbar' : 'ausführbar'}"
        if expect['format'] && format != expect['format']
            return failed(:wrong_format, "#{code(path)} ist #{FORMATS[format]}, gesucht ist #{FORMATS[expect['format']] || expect['format']}.", :read => read)
        end
        if expect['executable'] && (entry[:mode] & 0o111).zero?
            return failed(:not_executable, "#{code(path)} gibt es, aber die Datei ist nicht ausführbar.", :read => read)
        end
        # contains: [pattern | { pattern:, say: }]: each regular expression
        # matches somewhere in the text (line by line); the read part names
        # the line where each was found.
        found = []
        text = nil
        Array(expect['contains']).each do |want|
            pattern, say = want.is_a?(Hash) ? [want['pattern'], want['say']] : [want, nil]
            text ||= ws.read_text(path)
            re = Regexp.new(pattern, timeout: 0.5)
            hit = text.each_line.with_index(1).find { |line, _| re.match?(line) }
            if hit.nil?
                read_all = ([read] + found).join("\n\n")
                return failed(:missing_text, say || "In #{code(path)} finde ich noch nichts, das zu #{code(pattern)} passt.", :read => read_all)
            end
            found << "- #{code(pattern)}: Zeile #{hit[1]}, #{code(hit[0].strip[0, 80])}"
        end
        ok(:read => ([read] + found).join("\n\n"))
    end

    # --- html -----------------------------------------------------------------

    def self.normalize(text)
        CGI.unescapeHTML(text.to_s).gsub(/\s+/, ' ').strip
    end

    def self.words(text)
        normalize(text).scan(/[[:alnum:]][[:alnum:]'’-]*/).size
    end

    def self.slide_title(slide)
        heading = slide.at('h1') || slide.at('h2') || slide.at('h3')
        heading ? normalize(heading.text) : ''
    end

    def self.html(check, ws)
        file = check['file']
        source = ws.read_text(file)
        doc = HtmlLite.parse(source)
        expect = check['expect'] || {}
        warnings = []
        read_lines = []

        if expect['parses'] && !doc.problems.empty?
            shown = doc.problems.first(MAX_SHOWN).map { |p| "- Zeile #{p.line}: #{p.message}" }
            more = doc.problems.size > MAX_SHOWN ? "\n\nUnd #{doc.problems.size - MAX_SHOWN} weitere Stellen." : ''
            return failed(:parse_error, "In #{code(file)} passen Tags nicht zusammen:\n\n#{shown.join("\n")}#{more}",
                :read => "#{code(file)}: #{lines_text(source.lines.size)}")
        end

        selector = (expect['slides'] || {})['selector'] || 'section.slide'
        slides = doc.select(selector)
        if expect['slides'] || expect['first_slide_changed'] || (check['warn'] || {})['words_per_slide']
            listing = slides.each_with_index.map do |s, i|
                title = slide_title(s)
                "#{i + 1}. Zeile #{s.line}: #{title.empty? ? '(ohne Überschrift)' : "»#{title}«"}"
            end
            read_lines << "#{slides.size} Folien (`#{selector}`) in #{code(file)}:\n\n#{listing.join("\n")}"
        end

        if (spec = expect['first_slide_changed'])
            first = slides.first
            if first.nil?
                return failed(:no_slides, "In #{code(file)} finde ich keine Folie (`#{selector}`).", :read => read_lines.join("\n\n"))
            end
            text = normalize(first.text)
            left = (spec['placeholders'] || []).select { |p| text.include?(normalize(p)) }
            unless left.empty?
                return failed(:first_slide_unchanged, "Auf der ersten Folie (ab Zeile #{first.line}) steht noch #{left.map { |p| "»#{p}«" }.join(' und ')}.", :read => read_lines.join("\n\n"))
            end
        end

        if (spec = expect['slides'])
            template = (spec['template_titles'] || []).map { |t| normalize(t) }.to_set
            own = slides.reject { |s| template.include?(slide_title(s)) }
            left = slides.select { |s| template.include?(slide_title(s)) }
            min = spec['min_own'].to_i
            if own.size < min
                still = left.first(5).map { |s| "»#{slide_title(s)}«" }.join(', ')
                note = left.empty? ? '' : " Aus der Vorlage stehen noch #{left.size} Folien drin, zum Beispiel #{still}."
                return failed(:too_few_slides, "Ich finde #{own.size} eigene Folien, verlangt #{min == 1 ? 'ist eine' : "sind #{min}"}.#{note}", :read => read_lines.join("\n\n"))
            end
            if spec['each_has']
                missing = own.find { |s| s.at(spec['each_has']).nil? }
                if missing
                    return failed(:slide_without_heading, "Die Folie ab Zeile #{missing.line} hat kein `<#{spec['each_has']}>`.", :read => read_lines.join("\n\n"))
                end
            end
            warnings << "Aus der Vorlage stehen noch #{left.size} Folien drin. Lösche die, die du nicht brauchst." unless left.empty?
        end

        if (limit = (check['warn'] || {})['words_per_slide'])
            slides.each do |s|
                n = words(s.elements.reject { |e| %w[script style].include?(e.name) }.map(&:text).join(' '))
                next if n <= limit

                title = slide_title(s)
                warnings << "Auf der Folie #{title.empty? ? "ab Zeile #{s.line}" : "»#{title}«"} stehen #{n} Wörter. Weniger Text liest sich besser (Ziel: höchstens #{limit})."
            end
        end

        if (spec = expect['img'])
            images = doc.select('img')
            base = File.dirname(file)
            lines = images.map do |img|
                "- Zeile #{img.line}: `src=\"#{img.attrs['src']}\"`#{img.attrs['alt'].to_s.strip.empty? ? ', ohne alt' : ", alt=\"#{img.attrs['alt']}\""}"
            end
            read_lines << "#{images.size} Bilder in #{code(file)}#{lines.empty? ? '.' : ":\n\n#{lines.join("\n")}"}"
            read = read_lines.join("\n\n")
            if images.size < spec['min'].to_i
                return failed(:too_few_images, "In #{code(file)} finde ich #{images.size} `<img>`, verlangt #{spec['min'].to_i == 1 ? 'ist eins' : "sind #{spec['min']}"}.", :read => read)
            end
            # local: so many pictures with a path in the Workspace, not an
            # address on the web: »ein eigenes Bild«.
            local = images.count { |img| (src = img.attrs['src'].to_s.strip) !~ %r{\A(https?:)?//}i && !src.start_with?('data:') && !src.empty? }
            if local < spec['local'].to_i
                return failed(:no_local_image, "In #{code(file)} #{images.size == 1 ? 'kommt das Bild' : 'kommen die Bilder'} aus dem Netz. Lege ein eigenes Bild neben #{code(File.basename(file))} und binde es mit einem relativen Pfad ein, zum Beispiel `src=\"mein-bild.jpg\"`.", :read => read)
            end
            images.each do |img|
                src = img.attrs['src'].to_s.strip
                if spec['alt'] == 'required' && img.attrs['alt'].to_s.strip.empty?
                    return failed(:alt_missing, "Das Bild in Zeile #{img.line} (#{code(src)}) hat keinen `alt`-Text. Beschreibe darin, was auf dem Bild zu sehen ist.", :read => read)
                end
                next unless spec['src_exists']

                if src.empty?
                    return failed(:image_missing, "Das `<img>` in Zeile #{img.line} hat kein `src`.", :read => read)
                end
                next if src =~ %r{\A(https?:)?//}i || src.start_with?('data:')

                target = resolve(base, src)
                if target.nil?
                    return failed(:image_missing, "Der Pfad #{code(src)} in Zeile #{img.line} führt aus dem Workspace heraus.", :read => read)
                end
                next if ws.entry(target)[:type] == :file

                other = ws.other_case(target)
                if other
                    return failed(:image_case, "Das Bild #{code(src)} aus Zeile #{img.line} gibt es so nicht, aber #{code(relative_to(base, other))}. Der Live Server unterscheidet Groß- und Kleinschreibung, dein Rechner zu Hause vielleicht nicht.", :read => read)
                end
                return failed(:image_missing, "Das Bild #{code(src)} aus Zeile #{img.line} gibt es nicht (gesucht: #{code(target)}).", :read => read)
            end
        end

        # elements: [{ selector: 'ul li', min: 3, attr: href, say: … }]: so
        # many elements of this kind (with this attribute, not empty).
        Array(expect['elements']).each do |want|
            found = doc.select(want['selector'])
            found = found.reject { |e| e.attrs[want['attr']].to_s.strip.empty? } if want['attr']
            min = (want['min'] || 1).to_i
            what = "`#{want['selector']}`#{want['attr'] ? " mit `#{want['attr']}`" : ''}"
            read_lines << "#{found.size} × #{what} in #{code(file)}."
            next if found.size >= min

            message = want['say'] || "In #{code(file)} finde ich #{found.size} × #{what}, verlangt #{min == 1 ? 'ist eins' : "sind #{min}"}."
            return failed(:missing_element, message, :read => read_lines.join("\n\n"))
        end

        ok(:read => read_lines.empty? ? "#{code(file)}: #{lines_text(source.lines.size)}, Tags passen zusammen." : read_lines.join("\n\n"), :warnings => warnings)
    rescue WorkspaceFiles::Refused => e
        refused(e)
    end

    # Workspace-relative path for src as seen from base, or nil if it leaves
    # the Workspace. Query and fragment are dropped, %20 decoded.
    def self.resolve(base, src)
        path = src.sub(/[?#].*\z/, '')
        path = URI::DEFAULT_PARSER.unescape(path)
        joined = path.start_with?('/') ? path.sub(%r{\A/+}, '') : (base == '.' || base.empty? ? path : "#{base}/#{path}")
        clean = Pathname.new(joined).cleanpath.to_s
        return nil if clean.start_with?('..') || clean.start_with?('/')

        clean == '.' ? '' : clean
    end

    def self.relative_to(base, path)
        return path if base.nil? || base == '.' || base.empty?

        Pathname.new(path).relative_path_from(Pathname.new(base)).to_s
    end

    # --- css ------------------------------------------------------------------

    # Only what stands inside <style>…</style>, for a .svelte or .html file;
    # everything else becomes spaces, so offsets and line numbers stay true.
    # nil if there is no style block.
    def self.style_blocks(source)
        kept = source.gsub(/[^\n]/, ' ')
        found = false
        source.scan(%r{<style\b[^>]*>(.*?)</style>}im) do
            from, to = $~.offset(1)
            kept[from...to] = source[from...to]
            found = true
        end
        found ? kept : nil
    end

    def self.css(check, ws)
        file = check['file']
        source = ws.read_text(file)
        expect = check['expect'] || {}
        if check['style_tags']
            source = style_blocks(source)
            return failed(:no_style, "In #{code(file)} steht kein `<style>`-Block. Die CSS-Regeln gehören zwischen `<style>` und `</style>`.") if source.nil?
        end
        parsed = CssLite.parse(source)
        marker_at = nil
        if check['after_marker']
            marker_at = source.index(check['after_marker'])
            if marker_at.nil?
                return failed(:marker_missing, "In #{code(file)} fehlt der Kommentar »#{check['after_marker']}«. Hast du ihn gelöscht? Deine Regeln gehören darunter.")
            end
        end
        rules = marker_at ? parsed.rules.select { |r| r.offset > marker_at } : parsed.rules
        marker_line = marker_at ? source[0, marker_at].count("\n") + 1 : nil
        read = "#{rules.size} Regeln#{marker_line ? " unter »#{check['after_marker']}« (ab Zeile #{marker_line})" : ''}#{check['style_tags'] ? ' im `<style>`-Block' : ''} in #{code(file)}" +
            (rules.empty? ? '.' : ":\n\n" + rules.first(10).map { |r| "- Zeile #{r.line}: `#{r.selector}`" }.join("\n"))
        if expect['parses'] && !parsed.problems.empty?
            shown = parsed.problems.first(MAX_SHOWN).map { |p| "- Zeile #{p.line}: #{p.message}" }
            more = parsed.problems.size > MAX_SHOWN ? "\n\nUnd #{parsed.problems.size - MAX_SHOWN} weitere Stellen." : ''
            return failed(:parse_error, "In #{code(file)} stimmt etwas nicht:\n\n#{shown.join("\n")}#{more}\n\nDer Browser überspringt fehlerhafte Regeln ohne Meldung.", :read => read)
        end
        min = expect['min_rules'].to_i
        if rules.size < min
            where = marker_line ? " unter »#{check['after_marker']}«" : ''
            return failed(:too_few_rules, "In #{code(file)} stehen#{where} #{rules.size} Regeln, verlangt #{min == 1 ? 'ist eine' : "sind #{min}"}.", :read => read)
        end
        # declares: [{ selector:, property:, value:, media:, say: }]: some
        # rule sets this property (to this value), for this selector, inside
        # an @media whose condition mentions media.
        Array(expect['declares']).each do |want|
            next if rules.any? { |rule| css_declares?(rule, want) }

            return failed(:missing_declaration, want['say'] || "In #{code(file)} fehlt #{declaration_text(want)}.", :read => read)
        end
        if (page = check['linked_from'])
            unless stylesheet_linked?(ws, page, file)
                return failed(:not_linked, "#{code(File.basename(file))} ist in #{code(page)} nicht eingebunden. Schreib in den `<head>`: `<link rel=\"stylesheet\" href=\"#{relative_to(File.dirname(page), file)}\">`", :read => read)
            end
        end
        ok(:read => read)
    rescue WorkspaceFiles::Refused => e
        refused(e)
    end

    def self.css_value(text)
        text.to_s.downcase.sub(/\s*!important\z/, '').gsub(/\s+/, ' ').strip
    end

    def self.css_declares?(rule, want)
        if want['selector']
            selectors = rule.selector.split(',').map { |s| s.strip.gsub(/\s+/, ' ') }
            return false unless selectors.include?(want['selector'].strip.gsub(/\s+/, ' '))
        end
        return false if want['media'] && !rule.media.to_s.downcase.include?(want['media'].downcase)

        rule.declarations.any? do |name, value|
            name.downcase == want['property'].downcase &&
                (want['value'].nil? || css_value(value) == css_value(want['value']))
        end
    end

    # »eine Regel für `main` mit `display: flex`«
    def self.declaration_text(want)
        decl = want['value'] ? "`#{want['property']}: #{want['value']}`" : "`#{want['property']}`"
        where = want['selector'] ? "eine Regel für `#{want['selector']}` mit #{decl}" : "eine Regel mit #{decl}"
        want['media'] ? "#{where} in einer Media Query mit `#{want['media']}`" : where
    end

    # Does page (an HTML file) load css with <link rel="stylesheet">?
    def self.stylesheet_linked?(ws, page, css)
        doc = HtmlLite.parse(ws.read_text(page))
        doc.select('link').any? do |link|
            link.attrs['rel'].to_s.downcase.split.include?('stylesheet') &&
                resolve(File.dirname(page), link.attrs['href'].to_s.strip) == css
        end
    rescue WorkspaceFiles::Refused
        false
    end

    # --- story ----------------------------------------------------------------

    # A BIF story, from the analysis BIF writes in the Workspace (StoryCheck).
    def self.story(check, ws)
        project = check['project']
        file = "#{project}/#{StoryCheck::ANALYSIS}"
        if ws.entry(project)[:type] != :dir
            return failed(:no_project, "Den Ordner #{code(project)} gibt es in deinem Workspace noch nicht. Klone zuerst die BIF-Vorlage.")
        end
        if ws.entry(file)[:type] != :file
            return failed(:no_analysis, "BIF hat deine Geschichte noch nicht gelesen: #{code(file)} fehlt. Öffne deinen Workspace und darin den Ordner #{code(project)}; BIF liest die Geschichte, sobald du eine Seite speicherst.")
        end
        analysis = begin
            JSON.parse(ws.read_text(file))
        rescue JSON::ParserError
            return failed(:no_analysis, "#{code(file)} ist unvollständig. Speichere eine Seite deiner Geschichte, dann schreibt BIF die Datei neu.")
        end
        sha256_of = lambda do |rel|
            Digest::SHA256.hexdigest(ws.read("#{project}/#{rel}"))
        rescue WorkspaceFiles::Refused
            nil
        end
        read = StoryCheck.describe(analysis, project)
        ok, problem, message = StoryCheck.evaluate(analysis, check['expect'] || {}, :project => project, :sha256_of => sha256_of)
        ok ? ok(:read => read) : failed(problem, message, :read => read)
    rescue WorkspaceFiles::Refused => e
        refused(e)
    end

    # --- references -----------------------------------------------------------

    LINKING_ATTRIBUTES = {
        'img' => %w[src], 'script' => %w[src], 'source' => %w[src], 'video' => %w[src poster],
        'audio' => %w[src], 'iframe' => %w[src], 'embed' => %w[src], 'object' => %w[data],
        'track' => %w[src], 'input' => %w[src],
    }.freeze

    # [file, line, target, kind] for everything a page or stylesheet loads.
    def self.collect_references(file, source)
        refs = []
        if file =~ /\.css\z/i
            parsed = CssLite.parse(source)
            (parsed.urls + parsed.imports).uniq.each { |target, line| refs << [file, line, target] }
        else
            doc = HtmlLite.parse(source)
            doc.root.each_element do |e|
                (LINKING_ATTRIBUTES[e.name] || []).each do |attr|
                    refs << [file, e.line, e.attrs[attr]] if e.attrs[attr]
                end
                if e.name == 'link' && e.attrs['href'] && e.attrs['rel'].to_s.downcase !~ /\A\s*(canonical|alternate|author|license|next|prev|me)\s*\z/
                    refs << [file, e.line, e.attrs['href']]
                end
                if e.attrs['srcset']
                    e.attrs['srcset'].split(',').each { |part| refs << [file, e.line, part.strip.split(/\s+/).first.to_s] }
                end
                if e.name == 'style' || e.attrs['style']
                    css = e.name == 'style' ? e.text : e.attrs['style']
                    css.scan(/url\(\s*["']?([^"')]+)["']?\s*\)/) { refs << [file, e.line, $1] }
                end
            end
        end
        refs.reject { |_, _, target| target.to_s.strip.empty? || target =~ /\A(data:|#|mailto:|javascript:|about:)/i }
    end

    def self.references(check, ws)
        root = check['root']
        expect = check['expect'] || {}
        refs = []
        (check['scan'] || []).each do |name|
            file = root.to_s.empty? ? name : "#{root}/#{name}"
            refs.concat(collect_references(file, ws.read_text(file)))
        end
        external = refs.select { |_, _, t| t =~ %r{\A(https?:)?//}i }
        local = refs - external
        read = "#{refs.size} Verweise in #{(check['scan'] || []).map { |s| code(s) }.join(', ')}: #{local.size} auf Dateien im Ordner, #{external.size} ins Netz." +
            (refs.empty? ? '' : "\n\n" + refs.first(15).map { |f, l, t| "- #{File.basename(f)}, Zeile #{l}: `#{t}`" }.join("\n"))
        if expect['external'] == 'none' && !external.empty?
            f, l, t = external.first
            more = external.size > 1 ? " (und #{external.size - 1} weitere)" : ''
            return failed(:external, "#{code(File.basename(f))} lädt in Zeile #{l} #{code(t)} aus dem Netz#{more}. Ohne Netz fehlt das dann. Lade die Datei herunter und lege sie in #{code(root)}.", :read => read)
        end
        if expect['local_exist']
            local.each do |f, l, t|
                target = resolve(File.dirname(f), t)
                if target.nil? || !(target == root || target.start_with?("#{root}/"))
                    return failed(:outside_root, "#{code(t)} in #{code(File.basename(f))}, Zeile #{l}, zeigt aus #{code(root)} heraus. Alles, was die Seite braucht, gehört in diesen Ordner.", :read => read)
                end
                next if ws.entry(target)[:type] == :file

                other = ws.other_case(target)
                hint = other ? " Es gibt aber #{code(relative_to(File.dirname(f), other))}; Groß- und Kleinschreibung zählen." : ''
                return failed(:local_missing, "#{code(t)} aus #{code(File.basename(f))}, Zeile #{l}, gibt es nicht.#{hint}", :read => read)
            end
        end
        ok(:read => read)
    rescue WorkspaceFiles::Refused => e
        refused(e)
    end
end
