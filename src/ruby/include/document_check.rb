require_relative 'document_dates'
require 'cgi'
require_relative 'office_client'

# Checks a document a student uploaded (check type document,
# docs/plan-textverarbeitung.md): the paragraphs the office service read
# (OfficeClient) against the task's rules. Each rule is one point of the
# checklist on the card, ticked or not, and points at the places on the
# student's page where it fails (marks, in PDF points from the top left of
# the page).
#
#   check:
#     type: document
#     file: formatting-marks/01 Einladung.docx   # the starting file, below src/static
#     rules:
#       - text                 # every word of the starting file is still there
#       - spaces               # no two spaces in a row
#       - empty_paragraphs     # no empty paragraphs (but at the end)
#       - line_breaks          # no line breaks by hand (Shift+Enter)
#       - { align: center, paragraph: 'Einladung zum Elternabend' }
#       - { gaps: 6pt, before: ['Liebe Eltern', 'im Mai'] }
#       - { fonts: 2 }         # at most two fonts
#       - { size: [11, 12], paragraphs: ['Am Donnerstag', …] }   # their text in this range (pt)
#       - { bold: ['14. März', 'vor der Mensa'] }   # these pieces of text bold
#       - { sparing: ['Am Donnerstag', …] }   # these paragraphs not bold as a whole
#       - no_underline         # nothing underlined
#       - no_caps              # no capitals set by formatting
#       - { list: ['Schlafsack', …] }   # these paragraphs a real list, no »- « typed
#       - { tabs: ['Busfahrt', …], align: [right, decimal] }   # one tab each, at one tab stop of this kind
#       - { table: ['Montag', 'Dienstag', …], columns: 3, header: 'Tag' }   # these in the cells of one table
#       - no_tabs              # no tab anywhere
#       - alt_text             # every picture has an alt text that says what is on it
#       - ratio                # no picture stretched or squeezed
#       - captions             # every picture has a caption whose number is a field
#       - { wrap: 1, side: right }   # the starting file's first picture has text beside it, at the right
#       - { headings: ['Montag', …], level: 1 }   # the paragraphs beginning so are headings of this level
#       - levels               # no level of heading skipped
#       - no_direct            # no heading or title formatted by hand (bold, size, colour, font)
#       - { style_changed: 'Heading 1' }   # the style itself looks other than in the starting file
#       - { page_breaks: ['Montag:', …] }   # these paragraphs begin a new page by a page break
#       - page_numbers         # the page number in a header or footer is a field, none typed
#       - toc                  # a table of contents the program made, up to date, none typed
#       - { words: 200 }       # an own text: at least so many words
#       - { heading_count: 2 } # at least so many headings with a heading style
#       - { picture_count: 1 } # at least so many pictures
#       - typed_lists          # no list marker typed by hand (»- «, »1. «) outside a real list
#
# The rules about pictures may carry names: (how the messages call the
# pictures, in the order of the starting file: ['das Foto', 'die Karte']).
# A picture of the upload is the starting file's picture with the same
# number of pixels.
#
# Every rule may carry label: (the point on the checklist) and say: (what
# the card says when it fails, instead of the built-in sentence).
module DocumentCheck
    RULES = %w[text spaces empty_paragraphs line_breaks align gaps fonts size bold sparing no_underline no_caps list tabs table no_tabs alt_text ratio captions wrap headings levels no_direct style_changed page_breaks page_numbers toc words heading_count picture_count typed_lists pdf pdf_fonts pdf_words].freeze
    # a list marker typed by hand at the start of a paragraph
    TYPED_MARKER = /\A\s*(?:[-–•*·]|\d{1,2}[.)])\s/
    TYPED_TOC_LINE = /(?:\.{4,}|…{2,}|\t)\s*\d+\s*\z/
    EDGE_PUNCT = /\A[[:punct:]»«„“”‚‘’–•·]+|[[:punct:]»«„“”‚‘’–•·]+\z/
    # a caption typed by hand or written by the program
    CAPTION_TEXT = /\A\s*(?:Abbildung|Abb\.|Bild|Grafik|Foto|Figure|Fig\.|Illustration)\s*(\d+)/i
    WRAP_NAMES = { 'none' => '»Oben und unten«', 'through' => '»Hinter den Text« oder »Vor den Text«' }.freeze
    TAB_ALIGN = { 'right' => 'rechtsbündig', 'decimal' => 'dezimal', 'center' => 'zentriert', 'left' => 'linksbündig' }.freeze
    ALIGN = { 'left' => 'linksbündig', 'center' => 'zentriert', 'right' => 'rechtsbündig', 'justify' => 'im Blocksatz' }.freeze
    MAX_MARKS = 20
    # LibreOffice's own names of the styles a student meets, as German Word
    # and LibreOffice show them (the service runs without a German UI)
    STYLE_NAMES = {
        'Standard' => 'Standard', 'Default Paragraph Style' => 'Standard', 'Normal' => 'Standard',
        'Title' => 'Titel', 'Subtitle' => 'Untertitel', 'Text body' => 'Textkörper', 'Body Text' => 'Textkörper',
        'List Paragraph' => 'Listenabsatz', 'Caption' => 'Beschriftung', 'Header' => 'Kopfzeile', 'Footer' => 'Fußzeile',
        'Table Contents' => 'Tabelleninhalt', 'Quotations' => 'Zitat', 'Quote' => 'Zitat',
    }.merge((1..9).to_h { |n| ["Heading #{n}", "Überschrift #{n}"] }).freeze

    # number: the open points' numbers, 1, 2, … in the order of the
    # checklist; every place of a point carries its point's number
    Item = Struct.new(:rule, :label, :ok, :message, :marks, :number, keyword_init: true)

    # Problems with a check's definition in tasks.yaml (TaskSpec).
    def self.spec_errors(check)
        errors = []
        file = check['file']
        # a check of an exported PDF needs no starting file
        errors << 'accept must be pdf (or left out)' if check.key?('accept') && check['accept'] != 'pdf'
        unless check['accept'] == 'pdf' && file.nil?
            errors << 'file: the starting document below src/static' unless file.is_a?(String) && !file.empty? && !file.start_with?('/') && !file.include?('..')
        end
        rules = check['rules']
        return errors + ['rules must be a list'] unless rules.is_a?(Array) && !rules.empty?

        rules.each do |rule|
            name = rule_name(rule)
            if name.nil?
                errors << "unknown rule #{rule.inspect} (#{RULES.join(', ')})"
                next
            end
            next unless rule.is_a?(Hash)
            known = %w[label say] + case name
                when 'align' then %w[align paragraph]
                when 'gaps' then %w[gaps before]
                when 'size' then %w[size paragraphs]
                when 'tabs' then %w[tabs align]
                when 'table' then %w[table columns header]
                when 'wrap' then %w[wrap side names]
                when 'headings' then %w[headings level]
                when 'alt_text', 'ratio', 'captions' then [name, 'names']
                else [name]
            end
            extra = rule.keys - known
            errors << "unknown keys for rule #{name}: #{extra.join(', ')}" unless extra.empty?
            errors << "align must be one of #{ALIGN.keys.join(', ')}" if name == 'align' && !ALIGN.key?(rule['align'])
            errors << 'align needs paragraph: (a piece of its text)' if name == 'align' && !(rule['paragraph'].is_a?(String) && !rule['paragraph'].empty?)
            errors << 'gaps must look like 6pt' if name == 'gaps' && pt(rule['gaps']) <= 0
            errors << 'gaps needs before: (pieces of the paragraphs)' if name == 'gaps' && !(rule['before'].is_a?(Array) && rule['before'].all? { |b| b.is_a?(String) })
            errors << 'fonts must be a number' if name == 'fonts' && !rule['fonts'].is_a?(Integer)
            errors << 'size must be [min, max] in pt' if name == 'size' && !(rule['size'].is_a?(Array) && rule['size'].size == 2 && rule['size'].all? { |v| v.is_a?(Numeric) })
            %w[bold sparing list tabs table page_breaks].each do |k|
                errors << "#{k} needs a list of pieces of text" if name == k && !(rule[k].is_a?(Array) && rule[k].all? { |b| b.is_a?(String) })
            end
            errors << 'wrap must be the number of a picture of the starting file (1, 2, …)' if name == 'wrap' && !(rule['wrap'].is_a?(Integer) && rule['wrap'].positive?)
            errors << 'side must be left or right' if name == 'wrap' && rule.key?('side') && !%w[left right].include?(rule['side'])
            errors << 'headings needs a list of pieces of text and level: (1, 2, …)' if name == 'headings' && !(rule['headings'].is_a?(Array) && rule['level'].is_a?(Integer) && rule['level'].positive?)
            errors << 'style_changed needs the name of a paragraph style (Heading 1)' if name == 'style_changed' && !(rule['style_changed'].is_a?(String) && !rule['style_changed'].empty?)
            %w[words heading_count picture_count pdf_words].each do |k|
                errors << "#{k} must be a positive number" if name == k && !(rule[k].is_a?(Integer) && rule[k].positive?)
            end
            errors << 'names must be a list of text' if rule.key?('names') && !(rule['names'].is_a?(Array) && rule['names'].all? { |n| n.is_a?(String) })
            errors << 'size needs paragraphs: (pieces of their text)' if name == 'size' && !(rule['paragraphs'].is_a?(Array) && rule['paragraphs'].all? { |b| b.is_a?(String) })
            %w[label say].each { |k| errors << "#{k} must be text" if rule.key?(k) && !rule[k].is_a?(String) }
        end
        errors
    end

    # align: is a rule and a key of tabs: the other rule wins
    def self.rule_name(rule)
        return rule if rule.is_a?(String)

        found = RULES & rule.keys
        found.size > 1 ? (found - ['align']).first : found.first
    end

    # model: what the office service answered for the upload; starter: the
    # same for the task's starting file (for the rule text), or nil.
    # Returns [status, items, message].
    def self.evaluate(check, model, starter)
        paragraphs = (model['paragraphs'] || []).map { |p| p.merge('plain' => squish(p['text'])) }
        body = paragraphs.select { |p| (p['where'] || {}).empty? }
        items = (check['rules'] || []).map do |rule|
            item = send("rule_#{rule_name(rule)}", rule.is_a?(Hash) ? rule : {}, body, starter, paragraphs, model)
            item.label = rule['label'] if rule.is_a?(Hash) && rule['label']
            item.message = rule['say'] if rule.is_a?(Hash) && rule['say'] && !item.ok
            item
        end
        n = 0
        items.each do |item|
            next item.marks = [] if item.ok

            item.number = (n += 1)
            item.marks = item.marks.first(MAX_MARKS).each_with_index.map { |m, i| m.merge(:n => item.number, :i => i) }
        end
        done = items.count(&:ok)
        message = if done == items.size
            "Alle #{items.size} Punkte erfüllt."
        else
            "#{done} von #{items.size} Punkten erfüllt."
        end
        [done == items.size ? :correct : :wrong, items, message]
    end

    # What the card shows: the checklist with its marks and the pages.
    def self.report(items, model)
        marks = items.flat_map(&:marks)
        {
            :items => items.map { |i| { :label => i.label, :ok => i.ok, :message => i.message, :marks => i.marks, :n => i.number } },
            :pages => (model['pages'] || []).each_with_index.map do |pg, i|
                { :width => pg['width'], :height => pg['height'], :src => "data:image/svg+xml;base64,#{pg['svg']}",
                  :crop => crop(model, marks, i + 1, pg) }
            end,
            :page_count => model['page_count'],
            # an exported PDF: its own pages, not drawn by LibreOffice
            :pdf => model['pdf'] ? true : nil,
        }.compact
    end

    # The part of a page the card shows: from a little above the first
    # paragraph to a little below the last one and every mark, between the
    # margins and room for the numbers beside them (points). left: where
    # the text begins, so that a mark there gets its number in the margin.
    def self.crop(model, marks, page, pg)
        boxes = (model['paragraphs'] || []).map { |p| p['box'] }.compact.select { |b| b['page'] == page }
        ys = boxes.flat_map { |b| [b['top'], b['bottom']] } + marks.select { |m| m[:page] == page }.flat_map { |m| [m[:y], m[:y] + m[:h]] }
        margins = model['margins'] || {}
        left = margins['left'] || 0
        right = margins['right'] || pg['width']
        top = ys.empty? ? 0 : [ys.min - 18, 0].max
        bottom = ys.empty? ? pg['height'] : [ys.max + 18, pg['height']].min
        { :x => [left - 30, 0].max, :y => top, :w => [right + 30, pg['width']].min - [left - 30, 0].max, :h => bottom - top, :left => left }
    end

    # The paragraphs as the check read them, for »Was der Workspace gelesen
    # hat«: spaces as ·, line breaks as ↵, tabs as →, the end as ¶.
    def self.xray(model)
        if model['pdf']
            text = model['text'].to_s.gsub(/[ \t]+/, ' ').gsub(/\n{3,}/, "\n\n").strip[0, 3000]
            fonts = (model['fonts'] || []).map { |f| "#{f['name']}#{f['embedded'] ? '' : ' (nicht eingebettet)'}" }.uniq
            return "So hat der Server dein PDF gelesen: #{model['page_count']} #{model['page_count'] == 1 ? 'Seite' : 'Seiten'}, " \
                "Schriften: #{CGI.escapeHTML(fonts.join(', ').then { |f| f.empty? ? 'keine' : f })}.\n\n" \
                "<pre class='document-xray'><code>#{CGI.escapeHTML(text)}</code></pre>"
        end
        lines = (model['paragraphs'] || []).first(80).map do |p|
            text = p['text'].to_s.gsub(' ', '·').gsub("\t", '→').gsub("\n", "↵\n  ")
            "#{style_name(p)}, #{ALIGN[p['align']] || p['align']}\n  #{text}¶"
        end
        "So hat LibreOffice deine Absätze gelesen (· Leerzeichen, ↵ Zeilenumbruch, → Tab, ¶ Absatzende):\n\n" \
            "<pre class='document-xray'><code>#{CGI.escapeHTML(lines.join("\n"))}</code></pre>"
    end

    # A short answer for the class overview: which points are open
    # (items: those of report).
    def self.summary(filename, items)
        open = items.reject { |i| i[:ok] }.map { |i| i[:label] }
        base = "#{filename}: #{items.count { |i| i[:ok] }} von #{items.size}"
        return base if open.empty?
        open.size <= 2 ? "#{base}, offen: #{open.join(', ')}" : "#{base}, #{open.size} Punkte offen"
    end

    def self.style_name(p)
        STYLE_NAMES[p['style']] || STYLE_NAMES[p['style_name']] || p['style_name'] || p['style']
    end

    def self.squish(text)
        text.to_s.gsub(/\s+/, ' ').strip
    end

    def self.empty?(p)
        p['plain'].empty? && (p['portions'] || []).none? { |q| q['type'] != 'Text' }
    end

    def self.short(p, words = 4)
        # a line of a typed table of contents without its dots and page
        w = p['plain'].sub(/\s*(?:\.{4,}|…{2,})\s*\d*\s*\z/, '').split(' ')
        q(w.size > words ? "#{w.first(words).join(' ')} …" : w.join(' '))
    end

    # Text from the student's document inside a message: neither HTML nor
    # Markdown (TaskHtml filters the HTML anyway; this keeps a * or _ in a
    # document from turning the message bold).
    def self.q(text)
        CGI.escapeHTML(text.to_s).gsub(/([\\`*_\[\]#])/) { "\\#{$1}" }
    end

    def self.pt(value)
        value.to_s[/[\d.,]+/].to_s.tr(',', '.').to_f
    end

    # ---- the rules

    def self.rule_text(_rule, body, starter, all = nil, _model = nil)
        item = Item.new(:rule => 'text', :label => 'Der Text ist noch vollständig', :ok => true, :marks => [])
        return item if starter.nil? || starter['paragraphs'].nil?

        # typed list markers (»-«, »•«) are no words: a real list has none
        # nor punctuation at the edges of a word (»1:« of a caption the
        # program wrote as »1«)
        # any date is the same word: the starting file's dates were filled
        # in on the day of its download (DocumentDates)
        words = ->(ps) { ps.flat_map { |p| DocumentDates.normalize(p['text']).split(/\s+/) }.map { |w| w.gsub(EDGE_PUNCT, '') }.reject(&:empty?) }
        # the whole document, the cells of tables too
        have = words.(all || body).tally
        # lines of a table of contents typed in the starting file (dots or a
        # tab, then a page number) are replaced by a real one
        theirs = starter['paragraphs'].reject { |p| p['text'].to_s.match?(TYPED_TOC_LINE) }
        missing = words.(theirs).tally.flat_map { |w, n| [w] * [n - have[w].to_i, 0].max }
        return item if missing.empty?

        item.ok = false
        item.message = "Aus der Vorlage fehlen #{missing.size == 1 ? 'ein Wort' : "#{missing.size} Wörter"}: " \
            "»#{q(missing.first(6).join(' '))}«#{missing.size > 6 ? ' …' : ''}. Hier änderst du nur, wie der Text gebaut ist, nicht den Text selbst."
        item
    end

    def self.rule_spaces(_rule, body, _starter, _all = nil, _model = nil)
        item = Item.new(:rule => 'spaces', :label => 'Keine Leerzeichen hintereinander', :ok => true, :marks => [])
        places = []
        body.each do |p|
            p['text'].to_s.to_enum(:scan, / {2,}/).each do
                m = Regexp.last_match
                before = p['text'][0...m.begin(0)].split(/\s+/).reject(&:empty?).size
                places << [p, before, m[0].size]
            end
        end
        return item if places.empty?

        item.ok = false
        item.marks = places.map { |p, before, count| gap_mark(p, before, count).merge(:title => "#{count} Leerzeichen", :count => count) }
        examples = places.first(3).map do |p, before, count|
            words = p['plain'].split(' ')
            where = before.zero? ? "vor »#{q(words.first(3).join(' '))}«" : "nach »#{q(words[[before - 1, 0].max])}«"
            "#{where} (#{count})"
        end
        item.message = "An #{places.size == 1 ? 'einer Stelle stehen' : "#{places.size} Stellen stehen"} Leerzeichen hintereinander: " \
            "#{examples.join(', ')}#{places.size > 3 ? " und #{places.size - 3} weitere" : ''}. Leerzeichen trennen Wörter; für alles andere gibt es Ausrichtung und Abstand."
        item
    end

    def self.rule_empty_paragraphs(_rule, body, _starter, _all = nil, _model = nil)
        item = Item.new(:rule => 'empty_paragraphs', :label => 'Keine leeren Absätze', :ok => true, :marks => [])
        # empty paragraphs at the very end change nothing on the page; the
        # one right before a page break is what Word leaves of Ctrl+Enter
        last = body.rindex { |p| !empty?(p) } || -1
        holds_break = body.each_index.select { |i| empty?(body[i]) && body[i + 1] && page_break?(body[i + 1]) && (i.zero? || !empty?(body[i - 1])) }.map { |i| body[i] }
        groups = body[0..last].slice_when { |a, b| empty?(a) != empty?(b) }.select { |g| empty?(g.first) }
            .map { |g| g - holds_break }.reject(&:empty?)
        return item if groups.empty?

        item.ok = false
        item.marks = groups.map do |g|
            a = g.first['box']
            z = g.last['box']
            next nil if a.nil? || z.nil?
            { :page => a['page'], :x => a['left'], :y => a['top'], :w => a['right'] - a['left'],
              :h => (z['page'] == a['page'] ? z['bottom'] : a['bottom']) - a['top'], :kind => 'blank',
              :title => g.size == 1 ? '1 leerer Absatz' : "#{g.size} leere Absätze" }
        end.compact
        count = groups.sum(&:size)
        first = body.index(groups.first.first)
        after = first && first.positive? ? " zum Beispiel nach »#{short(body[first - 1])}«" : ''
        item.message = "#{count == 1 ? 'Ein leerer Absatz macht' : "#{count} leere Absätze machen"} Abstand,#{after}. " \
            'Den Abstand gibst du dem Absatz selbst (»Abstand nach«); dann bleibt er, auch wenn sich die Schrift ändert.'
        item
    end

    def self.rule_line_breaks(_rule, body, _starter, _all = nil, _model = nil)
        item = Item.new(:rule => 'line_breaks', :label => 'Keine Zeilenumbrüche von Hand', :ok => true, :marks => [])
        found = body.select { |p| p['text'].to_s.include?("\n") }
        return item if found.empty?

        item.ok = false
        found.each do |p|
            p['text'].to_enum(:scan, /\n/).each do
                at = Regexp.last_match.begin(0)
                before = p['text'][0...at].split(/\s+/).reject(&:empty?).size
                item.marks << gap_mark(p, before, 1, :at_end => true).merge(:kind => 'spot', :title => 'Zeilenumbruch')
            end
        end
        breaks = found.sum { |p| p['text'].count("\n") }
        item.message = "»#{short(found.first)}« hat #{breaks == 1 ? 'einen Zeilenumbruch' : "#{breaks} Zeilenumbrüche"} von Hand " \
            '(Shift+Enter). Innerhalb eines Absatzes bricht das Programm die Zeilen selbst um; ein Umbruch von Hand steht auch dann noch, wenn die Zeile längst woanders endet.'
        item
    end

    def self.rule_align(rule, body, _starter, _all = nil, _model = nil)
        want = rule['align'].to_s
        name = rule['paragraph'].to_s
        label = DocumentDates.date?(name) ? 'Das Datum' : "»#{name}«"
        item = Item.new(:rule => 'align', :label => "#{label} ist #{ALIGN[want]}", :ok => false, :marks => [])
        # a date with whatever date the student's copy got (DocumentDates)
        found = DocumentDates.pattern(name)
        p = body.find { |q| q['plain'].match?(found) }
        if p.nil?
            item.message = DocumentDates.date?(name) ? 'Das Datum finde ich nicht mehr.' : "Den Absatz »#{name}« finde ich nicht mehr."
            return item
        end
        name = p['plain'][found]
        item.ok = p['align'] == want
        return item if item.ok

        item.marks = [para_mark(p).merge(:title => ALIGN[p['align']])]
        item.message = if p['text'].to_s.start_with?('  ') && p['align'] == 'left'
            "»#{name}« steht nur mit Leerzeichen #{want == 'right' ? 'rechts' : 'in der Mitte'}. Lösche sie und mach den Absatz #{ALIGN[want]}."
        else
            "»#{name}« ist #{ALIGN[p['align']]}, soll aber #{ALIGN[want]} sein."
        end
        item
    end

    def self.rule_gaps(rule, body, _starter, _all = nil, _model = nil)
        min = pt(rule['gaps'])
        item = Item.new(:rule => 'gaps', :label => 'Abstand zwischen den Absätzen', :ok => true, :marks => [])
        problems = []
        blank = []
        Array(rule['before']).each do |name|
            i = body.index { |q| q['plain'].include?(name.to_s) }
            if i.nil?
                problems << "Den Absatz »#{name}« finde ich nicht mehr."
                next
            end
            p = body[i]
            j = (0...i).to_a.reverse.find { |k| !empty?(body[k]) }
            next if j.nil?
            q = body[j]
            # the empty paragraphs between have marks of their own
            if j < i - 1
                blank << p
                next
            end
            gap = [q['space_after'].to_f, p['space_before'].to_f].max
            gap = 0 if q['context_margin'] && p['context_margin'] && q['style'] == p['style']
            # 90 %: LibreOffice counts in cm, and 0.2 cm are 5.7 pt
            next if gap >= min * 0.9 - 0.01

            problems << "Zwischen »#{short(q, 3)}« und »#{short(p, 3)}« ist #{gap.zero? ? 'kein Abstand' : "nur #{gap.round} pt Abstand"}."
            item.marks << between_mark(q, p).merge(:kind => 'tight', :title => "#{gap.round} pt Abstand")
        end
        if blank.any?
            problems.unshift(blank.size == 1 ? "Vor »#{short(blank.first)}« kommt der Abstand noch von leeren Absätzen." :
                "An #{blank.size} Stellen kommt der Abstand noch von leeren Absätzen, zum Beispiel vor »#{short(blank.first)}«.")
        end
        return item if problems.empty?

        item.ok = false
        item.message = "#{problems.first(3).join(' ')}#{problems.size > 3 ? " (und #{problems.size - 3} weitere Stellen)" : ''} " \
            "Gib dem oberen Absatz »Abstand nach« (mindestens #{min.round} pt) oder dem unteren »Abstand vor«."
        item
    end

    # ---- type: fonts, sizes, emphasis

    # The character ranges of a paragraph's portions: [[from, to, portion]].
    def self.ranges(p)
        at = 0
        (p['portions'] || []).map do |q|
            t = q['text'].to_s
            range = [at, at + t.size, q]
            at += t.size
            range
        end
    end

    def self.visible?(q)
        !q['text'].to_s.strip.empty?
    end

    # The places of a range of characters: the PDF's words that overlap it,
    # one mark per line (the whole paragraph where the words do not match).
    def self.range_marks(p, from, to, title)
        words = p['words'] || []
        tokens = p['text'].to_s.to_enum(:scan, /\S+/).map { [Regexp.last_match.begin(0), Regexp.last_match.end(0)] }
        return [para_mark(p).merge(:title => title)] if words.empty? || words.size != tokens.size

        hit = tokens.each_index.select { |i| tokens[i][0] < to && tokens[i][1] > from }.map { |i| words[i] }
        hit.chunk_while { |a, b| a[0] == b[0] && (a[2] - b[2]).abs < 2 }.map do |line|
            x0 = line.map { |w| w[1] }.min
            y0 = line.map { |w| w[2] }.min
            { :page => line[0][0], :x => x0, :y => y0, :w => line.map { |w| w[3] }.max - x0,
              :h => line.map { |w| w[4] }.max - y0, :kind => 'word', :title => title }
        end
    end

    def self.find_paragraphs(body, pieces)
        Array(pieces).filter_map { |piece| body.find { |p| p['plain'].include?(piece) } }.uniq
    end

    def self.names(list)
        list.size == 1 ? list.first : "#{list[0..-2].join(', ')} und #{list[-1]}"
    end

    def self.rule_fonts(rule, body, _starter, _all = nil, _model = nil)
        most = rule['fonts'].to_i
        item = Item.new(:rule => 'fonts', :label => most == 1 ? 'Eine Schriftart' : "Höchstens #{most} Schriftarten", :ok => true, :marks => [])
        used = Hash.new { |hash, k| hash[k] = [] }
        body.each { |p| ranges(p).each { |from, to, q| used[q['font']] << [p, from, to] if visible?(q) } }
        return item if used.size <= most

        item.ok = false
        # the fonts used most stay; the others are marked
        order = used.sort_by { |_, places| -places.sum { |_, a, b| b - a } }
        order.drop(most).each do |font, places|
            places.each { |p, from, to| item.marks.concat(range_marks(p, from, to, font)) }
        end
        item.message = "Im Dokument stehen #{used.size} Schriftarten: #{q(names(order.map(&:first)))}. " \
            "Für ein Blatt reichen zwei: eine für die Überschrift, eine für den Text."
        item
    end

    def self.rule_size(rule, body, _starter, _all = nil, _model = nil)
        low, high = rule['size']
        item = Item.new(:rule => 'size', :label => "Text in #{low.to_s.sub(/\.0\z/, '')} bis #{high.to_s.sub(/\.0\z/, '')} pt", :ok => true, :marks => [])
        bad = find_paragraphs(body, rule['paragraphs']).flat_map do |p|
            ranges(p).select { |_, _, q| visible?(q) && (q['size'] < low - 0.05 || q['size'] > high + 0.05) }.map { |from, to, q| [p, from, to, q] }
        end
        return item if bad.empty?

        item.ok = false
        bad.each { |p, from, to, q| item.marks.concat(range_marks(p, from, to, "#{q['size'].to_s.sub(/\.0\z/, '')} pt")) }
        sizes = bad.map { |_, _, _, q| "#{q['size'].to_s.sub(/\.0\z/, '')} pt" }.uniq
        item.message = "Im Text stehen auch #{names(sizes)}. Eine Größe für den ganzen Text liest sich ruhiger; " \
            "#{low.to_s.sub(/\.0\z/, '')} oder #{high.to_s.sub(/\.0\z/, '')} pt sind auf Papier gut lesbar."
        item
    end

    def self.rule_bold(rule, body, _starter, _all = nil, _model = nil)
        pieces = Array(rule['bold'])
        label = "#{names(pieces.map { |x| DocumentDates.date?(x) ? 'das Datum' : "»#{x}«" })} fett"
        item = Item.new(:rule => 'bold', :label => label.sub(/\A\p{Ll}/, &:upcase), :ok => true, :marks => [])
        problems = []
        pieces.each do |piece|
            # a date with whatever date the student's copy got (DocumentDates)
            found = DocumentDates.pattern(piece)
            p = body.find { |x| x['text'].to_s.match?(found) }
            if p.nil?
                problems << "»#{piece}« finde ich nicht mehr."
                next
            end
            m = p['text'].match(found)
            from = m.begin(0)
            to = m.end(0)
            piece = m[0]
            plain = ranges(p).select { |a, b, q| a < to && b > from && visible?(q) && !q['bold'] }
            next if plain.empty?

            problems << "»#{piece}« ist nicht fett."
            item.marks.concat(range_marks(p, from, to, 'nicht fett'))
        end
        return item if problems.empty?

        item.ok = false
        item.message = "#{problems.join(' ')} Fett hebt hervor, was jemand im Vorbeigehen lesen soll: wann und wo."
        item
    end

    def self.rule_sparing(rule, body, _starter, _all = nil, _model = nil)
        item = Item.new(:rule => 'sparing', :label => 'Fett nur, wo es wichtig ist', :ok => true, :marks => [])
        whole = find_paragraphs(body, rule['sparing']).select do |p|
            seen = ranges(p).select { |_, _, q| visible?(q) }
            total = seen.sum { |a, b, _| b - a }
            total.positive? && seen.select { |_, _, q| q['bold'] }.sum { |a, b, _| b - a } >= total * 0.8
        end
        return item if whole.empty?

        item.ok = false
        item.marks = whole.map { |p| para_mark(p).merge(:title => 'ganz fett') }
        item.message = "»#{short(whole.first)}«#{whole.size > 1 ? " und #{whole.size - 1} weitere Absätze sind" : ' ist'} ganz fett. " \
            'Ist alles fett, sticht nichts mehr heraus.'
        item
    end

    def self.rule_no_underline(_rule, body, _starter, _all = nil, _model = nil)
        portion_rule(body, 'no_underline', 'Nichts unterstrichen', 'unterstrichen', ->(q) { q['underline'] },
            'Unterstrichen sieht aus wie ein Link und macht die Buchstaben unten schwer lesbar. Zum Hervorheben gibt es fett.')
    end

    def self.rule_no_caps(_rule, body, _starter, _all = nil, _model = nil)
        portion_rule(body, 'no_caps', 'Keine Großbuchstaben als Format', 'Großbuchstaben', ->(q) { q['caps'] },
            'Text nur aus Großbuchstaben liest man langsamer, weil die Wörter ihre Form verlieren. Die Einstellung heißt »Großbuchstaben« oder »Versalien«.')
    end

    def self.portion_rule(body, name, label, title, test, why)
        item = Item.new(:rule => name, :label => label, :ok => true, :marks => [])
        found = body.flat_map { |p| ranges(p).select { |_, _, q| visible?(q) && test.(q) }.map { |a, b, _| [p, a, b] } }
        return item if found.empty?

        item.ok = false
        found.each { |p, a, b| item.marks.concat(range_marks(p, a, b, title)) }
        piece = found.first[0]['text'][found.first[1]...found.first[2]].split(' ').reject(&:empty?)
        piece = piece.size > 5 ? "#{piece.first(5).join(' ')} …" : piece.join(' ')
        item.message = "»#{q(piece)}« ist #{title == 'Großbuchstaben' ? 'in Großbuchstaben gesetzt' : title}. #{why}"
        item
    end

    # ---- lists and tab stops

    def self.rule_list(rule, body, _starter, _all = nil, _model = nil)
        item = Item.new(:rule => 'list', :label => 'Eine echte Liste', :ok => true, :marks => [])
        problems = []
        Array(rule['list']).each do |piece|
            p = body.find { |x| x['plain'].include?(piece) }
            if p.nil?
                problems << [:missing, "»#{piece}« finde ich nicht mehr."]
                next
            end
            typed = p['text'].to_s.match?(TYPED_MARKER)
            next if p['numbering'] && !typed

            item.marks << para_mark(p).merge(:title => typed ? 'Zeichen von Hand' : 'kein Listenpunkt')
            problems << [typed ? :typed : :plain, p]
        end
        return item if problems.empty?

        item.ok = false
        said = problems.group_by(&:first).map do |kind, list|
            next list.map(&:last).join(' ') if kind == :missing
            first = short(list.first.last, 2)
            case kind
            when :typed then list.size == 1 ? "Vor »#{first}« steht ein Zeichen, das jemand getippt hat." : "Vor #{list.size} Punkten steht ein Zeichen, das jemand getippt hat, zum Beispiel vor »#{first}«."
            else list.size == 1 ? "»#{first}« ist kein Punkt einer Liste." : "#{list.size} Zeilen sind keine Punkte einer Liste, zum Beispiel »#{first}«."
            end
        end
        item.message = "#{said.join(' ')} " \
            'Eine Liste, die das Programm führt, rückt jeden Punkt gleich ein, auch wenn er über zwei Zeilen geht, und zählt von selbst weiter.'
        item
    end

    def self.rule_tabs(rule, body, _starter, _all = nil, _model = nil)
        kinds = Array(rule['align'] || %w[right decimal]).map(&:to_s)
        item = Item.new(:rule => 'tabs', :label => 'Preise an einem Tabstopp', :ok => true, :marks => [])
        problems = []
        stops = []
        Array(rule['tabs']).each do |piece|
            p = body.find { |x| x['plain'].include?(piece) }
            if p.nil?
                problems << [:missing, "»#{piece}« finde ich nicht mehr."]
                next
            end
            count = p['text'].to_s.count("\t")
            stop = (p['tabs'] || []).find { |t| kinds.include?(t['align']) }
            if count != 1
                problems << (count.zero? ? [:none, p] : [:many, p])
            elsif stop.nil?
                problems << [:stop, p]
            else
                stops << [p, stop['at']]
                next
            end
            item.marks << para_mark(p).merge(:title => count == 1 ? 'kein Tabstopp' : "#{count} Tabs")
        end
        # all at the same place
        if problems.empty? && stops.map { |_, at| at.round }.uniq.size > 1
            problems << [:missing, 'Die Tabstopps stehen an verschiedenen Stellen, so stehen die Preise nicht untereinander.']
            item.marks.concat(stops.map { |p, at| para_mark(p).merge(:title => "Tabstopp bei #{(at / 72 * 2.54).round(1).to_s.tr('.', ',')} cm") })
        end
        return item if problems.empty?

        item.ok = false
        stop_kind = "#{kinds.map { |k| TAB_ALIGN[k] }.join(' oder ')}en Tabstopp"
        said = problems.group_by(&:first).map do |kind, list|
            next list.map(&:last).join(' ') if kind == :missing
            first = short(list.first.last, 2)
            n = list.size
            case kind
            when :none then n == 1 ? "In »#{first}« steht kein Tab." : "In #{n} Zeilen steht kein Tab, zum Beispiel in »#{first}«."
            when :many then n == 1 ? "In »#{first}« stehen mehrere Tabs; einer reicht." : "In #{n} Zeilen stehen mehrere Tabs, zum Beispiel in »#{first}«; einer reicht."
            else n == 1 ? "»#{first}« hat keinen #{stop_kind}." : "#{n} Zeilen haben keinen #{stop_kind}, zum Beispiel »#{first}«."
            end
        end
        item.message = "#{said.join(' ')} " \
            'Ein Tab springt zum nächsten Tabstopp; ein rechtsbündiger Tabstopp am Rand stellt die Preise genau untereinander, in jeder Schrift.'
        item
    end

    # ---- tables

    def self.rule_table(rule, body, _starter, all = nil, model = nil)
        all ||= body
        pieces = Array(rule['table'])
        item = Item.new(:rule => 'table', :label => 'Eine Tabelle', :ok => true, :marks => [])
        found = pieces.map { |piece| [piece, all.find { |p| p['plain'].include?(piece) }] }
        outside = found.select { |_, p| p && (p['where'] || {})['table'].nil? }
        missing = found.select { |_, p| p.nil? }.map(&:first)
        held = found.filter_map { |_, p| p && (p['where'] || {})['table'] }.uniq
        table = held.size == 1 ? ((model || {})['tables'] || []).find { |t| t['name'] == held.first } : nil
        problems = []
        problems << "#{q(names(missing.map { |m| "»#{m}«" }))} finde ich nicht mehr." unless missing.empty?
        unless outside.empty?
            first = short(outside.first[1], 3)
            problems << (outside.size == pieces.size ? "Die Zeilen stehen noch nicht in einer Tabelle, zum Beispiel »#{first}«." :
                "#{outside.size} Zeilen stehen außerhalb der Tabelle, zum Beispiel »#{first}«.")
            outside.each { |_, p| item.marks << para_mark(p).merge(:title => 'nicht in der Tabelle') }
        end
        problems << 'Die Zeilen stehen in mehreren Tabellen; eine reicht.' if held.size > 1
        if problems.empty? && table
            if rule['columns'] && table['cols'] && table['cols'] != rule['columns'].to_i
                problems << "Die Tabelle hat #{table['cols']} Spalten; Tag, Zeit und Programm brauchen #{rule['columns']}."
            end
            # the header among the cells of this table, the same text first
            cells = all.select { |p| (p['where'] || {})['table'] == held.first }
            header = rule['header'] && (cells.find { |p| p['plain'] == rule['header'].to_s } || cells.find { |p| p['plain'].include?(rule['header'].to_s) })
            if header && (header['where'] || {})['cell'].to_s !~ /\A[A-Z]+1\z/
                problems << "»#{q(rule['header'])}« steht nicht in der ersten Zeile; die Überschriften der Spalten gehören nach oben."
            elsif header && !table['repeat_header']
                problems << 'Die erste Zeile ist noch keine Überschrift, die sich auf jeder Seite wiederholt.'
                item.marks << para_mark(header).merge(:title => 'Überschrift wiederholen')
            end
        end
        return item if problems.empty?

        item.ok = false
        item.message = "#{problems.join(' ')} In einer Tabelle hat jede Angabe ihre Zelle; wird ein Eintrag länger, wächst seine Zeile, und alles bleibt in seiner Spalte."
        item
    end

    def self.rule_no_tabs(_rule, body, _starter, all = nil, _model = nil)
        all ||= body
        item = Item.new(:rule => 'no_tabs', :label => 'Keine Tabs mehr', :ok => true, :marks => [])
        found = all.select { |p| p['text'].to_s.include?("\t") }
        return item if found.empty?

        item.ok = false
        item.marks = found.map { |p| para_mark(p).merge(:title => "#{p['text'].count("\t")} Tabs") }
        item.message = "In #{found.size == 1 ? "»#{short(found.first, 3)}«" : "#{found.size} Absätzen, zum Beispiel »#{short(found.first, 3)}«,"} stehen noch Tabs. In der Tabelle trennen die Spalten, nicht die Tabs."
        item
    end

    # ---- pictures

    # The upload's pictures, each with its number in the starting file
    # (the one with the same pixels) and the name the rule gives it.
    def self.pictures(rule, model, starter)
        mine = ((model || {})['pictures'] || []).sort_by { |p| [(p['box'] || {})['page'] || 99, (p['box'] || {})['y'] || 0] }
        theirs = ((starter || {})['pictures'] || []).sort_by { |p| [(p['box'] || {})['page'] || 99, (p['box'] || {})['y'] || 0] }
        free = theirs.each_with_index.to_a
        names = Array(rule['names'])
        mine.map do |pic|
            hit = free.find { |q, _| q['pixels'] && q['pixels'] == pic['pixels'] }
            free.delete(hit) if hit
            n = hit ? hit[1] : nil
            name = n && names[n] ? names[n] : (n ? "das #{n + 1}. Bild" : 'ein Bild')
            pic.merge('number' => n && n + 1, 'called' => name)
        end
    end

    def self.cap(text)
        text.sub(/\A./) { |c| c.upcase }
    end

    def self.picture_mark(pic, title)
        b = pic['box']
        return nil if b.nil?
        { :page => b['page'], :x => b['x'], :y => b['y'], :w => b['w'], :h => b['h'], :kind => 'box', :title => title }
    end

    def self.alt_problem(pic)
        alt = squish(pic['description'].to_s.empty? ? pic['title'] : pic['description'])
        return :empty if alt.empty?
        return :file if alt.match?(/\.(jpe?g|png|gif|bmp|webp|tiff?|heic|svg)\z/i) || (alt.match?(/\d/) && !alt.include?(' ') && alt.match?(/[_\-.]/))
        return :auto if alt.match?(/automatisch generiert|automatically generated/i)
        return :short if alt.split(' ').size < 3
        nil
    end

    def self.rule_alt_text(rule, _body, starter, _all = nil, model = nil)
        item = Item.new(:rule => 'alt_text', :label => 'Jedes Bild hat einen Alternativtext', :ok => true, :marks => [])
        bad = pictures(rule, model, starter).filter_map { |pic| (k = alt_problem(pic)) && [pic, k] }
        return item if bad.empty?

        item.ok = false
        item.marks = bad.filter_map { |pic, k| picture_mark(pic, { :empty => 'kein Alternativtext', :file => 'Dateiname', :auto => 'geraten', :short => 'zu kurz' }[k]) }
        said = bad.first(2).map do |pic, k|
            alt = q(squish(pic['description'].to_s.empty? ? pic['title'] : pic['description']))
            case k
            when :empty then "#{cap(pic['called'])} hat keinen Alternativtext."
            when :file then "Der Alternativtext für #{pic['called']} ist ein Dateiname: »#{alt}«."
            when :auto then "Den Alternativtext für #{pic['called']} hat das Programm geraten: »#{alt.sub(/\s*Automatisch generierte Beschreibung\.?\z/i, '').sub(/[.\s]+\z/, '')}«. Er sagt nicht, was darauf zu sehen ist."
            else "»#{alt}« sagt zu wenig über #{pic['called']}."
            end
        end
        item.message = "#{said.join(' ')}#{bad.size > 2 ? " (und #{bad.size - 2} weitere Bilder)" : ''} " \
            'Ein Alternativtext sagt in einem Satz, was auf dem Bild zu sehen ist. Ein Screenreader liest ihn vor, wenn jemand das Bild nicht sehen kann.'
        item
    end

    # shown width/height against the picture's own (without what is cropped)
    def self.stretch(pic)
        return nil unless pic['width'].to_f.positive? && pic['height'].to_f.positive?
        w, h = pic['actual'] || pic['pixels']
        return nil unless w.to_f.positive? && h.to_f.positive?
        l, r, t, b = pic['actual'] ? (pic['crop'] || [0, 0, 0, 0]) : [0, 0, 0, 0]
        own = (w - l - r).to_f / (h - t - b)
        return nil unless own.positive?
        (pic['width'] / pic['height']) / own
    end

    def self.rule_ratio(rule, _body, starter, _all = nil, model = nil)
        item = Item.new(:rule => 'ratio', :label => 'Kein Bild verzerrt', :ok => true, :marks => [])
        bad = pictures(rule, model, starter).filter_map { |pic| (f = stretch(pic)) && (f - 1).abs > 0.03 && [pic, f] }
        return item if bad.empty?

        item.ok = false
        item.marks = bad.filter_map { |pic, f| picture_mark(pic, f > 1 ? 'in die Breite gezogen' : 'in die Höhe gezogen') }
        said = bad.first(2).map do |pic, f|
            "#{cap(pic['called'])} ist um #{((f > 1 ? f : 1 / f) - 1).*(100).round} % in die #{f > 1 ? 'Breite' : 'Höhe'} gezogen."
        end
        item.message = "#{said.join(' ')} Ziehst du an einer Seite, wird das Bild verzerrt; zieh an einer Ecke, " \
            'dann bleibt das Seitenverhältnis. Ein verzerrtes Bild setzt du in seinen Eigenschaften auf die Originalgröße zurück.'
        item
    end

    # A caption near a picture: a paragraph with a number range or one that
    # starts like a caption, just below or above it.
    def self.caption_for(pic, all)
        b = pic['box']
        return nil if b.nil?
        near = all.select do |p|
            pb = p['box']
            next false if pb.nil? || pb['page'] != b['page'] || p['plain'].empty?
            next false unless caption?(p)
            below = pb['top'] - (b['y'] + b['h'])
            above = b['y'] - pb['bottom']
            overlap = [pb['right'], b['x'] + b['w']].min - [pb['left'], b['x']].max
            overlap > 10 && ((-6..40).cover?(below) || (-6..30).cover?(above))
        end
        near.min_by { |p| [(p['box']['top'] - (b['y'] + b['h'])).abs, (b['y'] - p['box']['bottom']).abs].min }
    end

    def self.sequence?(p)
        (p['portions'] || []).any? { |q| q['sequence'] }
    end

    def self.caption?(p)
        sequence?(p) || p['plain'].match?(CAPTION_TEXT)
    end

    def self.rule_captions(rule, _body, starter, all = nil, model = nil)
        all ||= []
        item = Item.new(:rule => 'captions', :label => 'Bildunterschriften, die selbst zählen', :ok => true, :marks => [])
        missing = []
        typed = []
        pictures(rule, model, starter).each do |pic|
            c = caption_for(pic, all)
            if c.nil?
                missing << pic
                mark = picture_mark(pic, 'keine Bildunterschrift')
                item.marks << mark if mark
            elsif !sequence?(c)
                typed << c
            end
        end
        # typed captions that stand elsewhere
        typed |= all.select { |p| !sequence?(p) && p['plain'].match?(CAPTION_TEXT) }
        return item if missing.empty? && typed.empty?

        item.ok = false
        typed.each do |p|
            m = p['text'].to_s.match(CAPTION_TEXT)
            item.marks.concat(m ? range_marks(p, m.begin(1), m.end(1), 'getippt') : [para_mark(p).merge(:title => 'getippt')])
        end
        said = []
        said << (typed.size == 1 ? "Die Nummer in »#{short(typed.first, 3)}« ist getippt." :
            "#{typed.size} Bildunterschriften haben getippte Nummern, zum Beispiel »#{short(typed.first, 3)}«.") unless typed.empty?
        said << "#{cap(names(missing.map { |pic| pic['called'] }))} #{missing.size == 1 ? 'hat' : 'haben'} keine Bildunterschrift direkt darunter." unless missing.empty?
        item.message = "#{said.join(' ')} Fügst du die Beschriftung mit dem Programm ein, ist ihre Nummer ein Feld, das die Bilder zählt; " \
            'nach dem Aktualisieren stimmt sie wieder, auch wenn ein Bild dazukommt oder weiter nach oben wandert.'
        item
    end

    def self.rule_wrap(rule, _body, starter, _all = nil, model = nil)
        n = rule['wrap'].to_i
        side = rule['side']
        pics = pictures(rule, model, starter)
        pic = pics.find { |p| p['number'] == n }
        called = Array(rule['names'])[n - 1] || "das #{n}. Bild"
        item = Item.new(:rule => 'wrap', :label => "#{cap(called)} steht #{side == 'left' ? 'links' : side == 'right' ? 'rechts' : ''} neben dem Text".squeeze(' '), :ok => true, :marks => [])
        if pic.nil?
            item.ok = false
            item.message = "#{cap(called)} finde ich nicht mehr."
            return item
        end
        place = pic['frame'] || pic
        margins = (model || {})['margins'] || {}
        text_width = margins['right'].to_f - margins['left'].to_f
        width = (pic['frame'] || {})['width'] || pic['width']
        b = pic['box']
        problem = if place['anchor'] == 'as_character'
            "#{cap(called)} steht in der Zeile wie ein großer Buchstabe; daneben kann kein Text stehen."
        elsif !%w[parallel dynamic left right].include?(place['wrap'])
            "#{cap(called)} hat den Textumbruch #{WRAP_NAMES[place['wrap']] || place['wrap']}; der Text fließt nicht daneben."
        elsif text_width.positive? && width.to_f > text_width * 0.7
            "#{cap(called)} ist so breit, dass daneben kaum Platz für Text bleibt."
        elsif side && b && text_width.positive? && ((b['x'] + b['w'] / 2.0 > margins['left'] + text_width / 2.0) != (side == 'right'))
            "#{cap(called)} steht #{side == 'right' ? 'links' : 'rechts'}; es soll #{side == 'right' ? 'rechts' : 'links'} neben dem Text stehen."
        end
        return item if problem.nil?

        item.ok = false
        mark = picture_mark(pic, place['anchor'] == 'as_character' ? 'in der Zeile' : 'Textumbruch')
        item.marks << mark if mark
        item.message = "#{problem} Stell in Word den Textumbruch »Quadrat« ein, in LibreOffice die Verankerung »Am Absatz« und den Umlauf »Parallel«; " \
            'dann fließt der Text neben dem Bild, und du schiebst es an den Rand.'
        item
    end

    # ---- headings and styles

    DIRECT = { 'CharWeight' => 'fett', 'CharHeight' => 'Größe', 'CharColor' => 'Farbe', 'CharFontName' => 'Schriftart',
               'CharPosture' => 'kursiv', 'CharUnderline' => 'unterstrichen', 'CharCaseMap' => 'Großbuchstaben' }.freeze

    def self.level(p)
        p['outline'].to_i
    end

    def self.style_label(name)
        STYLE_NAMES[name] || name
    end

    def self.rule_headings(rule, body, _starter, _all = nil, _model = nil)
        want = rule['level'].to_i
        item = Item.new(:rule => 'headings', :label => "Überschriften der Ebene #{want}", :ok => true, :marks => [])
        plain = []
        other = []
        missing = []
        Array(rule['headings']).each do |piece|
            found = body.select { |p| p['plain'].start_with?(piece.to_s) && !p['text'].to_s.match?(TYPED_TOC_LINE) }
            missing << piece if found.empty?
            found.each do |p|
                next if level(p) == want
                (level(p).zero? ? plain : other) << p
                item.marks << para_mark(p).merge(:title => level(p).zero? ? style_name(p) : "Ebene #{level(p)}")
            end
        end
        return item if plain.empty? && other.empty? && missing.empty?

        item.ok = false
        said = []
        said << "#{q(names(missing.map { |m| "»#{m}«" }))} finde ich nicht mehr." unless missing.empty?
        unless plain.empty?
            said << (plain.size == 1 ? "»#{short(plain.first)}« ist ein normaler Absatz (#{q(style_name(plain.first))}), der nur wie eine Überschrift aussieht." :
                "#{plain.size} Zeilen sind normale Absätze, die nur wie Überschriften aussehen, zum Beispiel »#{short(plain.first)}«.")
        end
        other.first(2).each { |p| said << "»#{short(p)}« ist eine Überschrift der Ebene #{level(p)}, soll aber Ebene #{want} sein." }
        item.message = "#{said.join(' ')} Gib ihnen die Formatvorlage »Überschrift #{want}«: Dann weiß das Programm, dass sie Überschriften sind, " \
            'zeigt sie in seiner Übersicht und kann ein Inhaltsverzeichnis daraus machen.'
        item
    end

    def self.rule_levels(_rule, body, _starter, _all = nil, _model = nil)
        item = Item.new(:rule => 'levels', :label => 'Keine Ebene übersprungen', :ok => true, :marks => [])
        before = nil
        jumps = []
        body.select { |p| level(p).positive? }.each do |p|
            limit = before ? level(before) + 1 : 1
            jumps << [before, p] if level(p) > limit
            before = p
        end
        return item if jumps.empty?

        item.ok = false
        item.marks = jumps.map { |_, p| para_mark(p).merge(:title => "Ebene #{level(p)}") }
        a, b = jumps.first
        item.message = (a ? "Nach »#{short(a)}« (Ebene #{level(a)}) kommt »#{short(b)}« als Ebene #{level(b)}." :
            "Die erste Überschrift, »#{short(b)}«, ist schon Ebene #{level(b)}.") +
            ' Überschriften gehen Stufe für Stufe hinunter wie die Kapitel eines Buchs: Unter Ebene 1 kommt Ebene 2, nicht gleich 3.'
        item
    end

    def self.rule_no_direct(_rule, body, _starter, _all = nil, _model = nil)
        item = Item.new(:rule => 'no_direct', :label => 'Überschriften nicht von Hand formatiert', :ok => true, :marks => [])
        found = body.select { |p| level(p).positive? || %w[Title Titel].include?(p['style']) }.filter_map do |p|
            set = (p['portions'] || []).select { |q| visible?(q) }.flat_map { |q| q['direct'] || [] }.uniq & DIRECT.keys
            [p, set] unless set.empty?
        end
        return item if found.empty?

        item.ok = false
        item.marks = found.map { |p, set| para_mark(p).merge(:title => set.map { |k| DIRECT[k] }.join(', ')) }
        p, set = found.first
        item.message = "#{found.size == 1 ? "»#{short(p)}« ist" : "#{found.size} Überschriften sind, zum Beispiel »#{short(p)}«,"} noch von Hand formatiert " \
            "(#{set.map { |k| DIRECT[k] }.join(', ')}). Was du von Hand einstellst, gewinnt gegen die Formatvorlage: Änderst du die Vorlage, bleibt diese Überschrift, wie sie ist. " \
            'Markier sie und lösch die Formatierung (Word: Strg+Leertaste, LibreOffice: Strg+M).'
        item
    end

    STYLE_LOOK = %w[font size bold italic color].freeze

    def self.rule_style_changed(rule, _body, starter, _all = nil, model = nil)
        name = rule['style_changed'].to_s
        item = Item.new(:rule => 'style_changed', :label => "Die Formatvorlage »#{style_label(name)}« geändert", :ok => true, :marks => [])
        mine = ((model || {})['styles'] || {})[name]
        theirs = ((starter || {})['styles'] || {})[name]
        return item if mine.nil? || theirs.nil? ? false : STYLE_LOOK.any? { |k| mine[k] != theirs[k] }

        item.ok = false
        item.message = "Die Formatvorlage »#{style_label(name)}« sieht noch aus wie am Anfang. Ändere sie einmal, zum Beispiel ihre Farbe: " \
            'Alle Überschriften mit dieser Vorlage ändern sich mit, auf einmal.'
        item
    end

    # ---- pages and the table of contents

    def self.page_break?(p)
        %w[PAGE_BEFORE PAGE_BOTH].include?(p['break'].to_s)
    end

    def self.rule_page_breaks(rule, body, _starter, _all = nil, _model = nil)
        item = Item.new(:rule => 'page_breaks', :label => 'Neue Seiten mit Seitenumbruch', :ok => true, :marks => [])
        missing = []
        without = []
        Array(rule['page_breaks']).each do |piece|
            # the paragraph itself, not its line in a typed table of contents
            i = body.index { |p| p['plain'].start_with?(piece.to_s) && !p['text'].to_s.match?(TYPED_TOC_LINE) }
            if i.nil?
                missing << piece
                next
            end
            p = body[i]
            before = i.positive? ? body[i - 1] : nil
            next if page_break?(p) || (before && %w[PAGE_AFTER PAGE_BOTH].include?(before['break'].to_s))

            without << p
            item.marks << para_mark(p).merge(:title => 'kein Seitenumbruch')
        end
        return item if missing.empty? && without.empty?

        item.ok = false
        said = []
        said << "#{q(names(missing.map { |m| "»#{m}«" }))} finde ich nicht mehr." unless missing.empty?
        unless without.empty?
            said << (without.size == 1 ? "Vor »#{short(without.first)}« steht kein Seitenumbruch." :
                "Vor #{without.size} Überschriften steht kein Seitenumbruch, zum Beispiel vor »#{short(without.first)}«.")
        end
        item.message = "#{said.join(' ')} Ein Seitenumbruch (Strg+Enter) beginnt eine neue Seite, egal wie viel Text davor steht; " \
            'leere Absätze schieben nur so lange, bis sich davor etwas ändert.'
        item
    end

    def self.rule_page_numbers(_rule, _body, _starter, _all = nil, model = nil)
        item = Item.new(:rule => 'page_numbers', :label => 'Seitenzahlen, die das Programm zählt', :ok => true, :marks => [])
        parts = ((model || {})['page_parts'] || []).flat_map { |x| [x['header'], x['footer']].compact }
        field = parts.any? { |part| (part['portions'] || []).any? { |q| %w[PageNumber].include?(q['field']) } }
        typed = parts.select do |part|
            (part['portions'] || []).any? { |q| q['type'] == 'Text' && q['text'].to_s.match?(/\d/) && q['text'].to_s.match?(/Seite|^\s*\d+\s*$/) }
        end
        return item if field && typed.empty?

        item.ok = false
        item.marks = ((model || {})['footer_boxes'] || []).map { |b| b.transform_keys(&:to_sym).merge(:kind => 'box', :title => field ? 'Zahl getippt' : 'keine Seitenzahl') }
        item.message = if !typed.empty?
            "In der Fußzeile steht »#{q(squish(typed.first['portions'].map { |x| x['text'] }.join))}«, getippt: Die Fußzeile ist auf jeder Seite dieselbe, also steht dort überall dieselbe Zahl. " \
                'Füge die Seitenzahl als Feld ein; dann zählt das Programm auf jeder Seite selbst.'
        else
            'Keine Seite hat eine Seitenzahl. Füge sie als Feld in die Fußzeile ein; dann zählt das Programm auf jeder Seite selbst.'
        end
        item
    end

    def self.rule_toc(_rule, body, _starter, all = nil, model = nil)
        all ||= body
        item = Item.new(:rule => 'toc', :label => 'Ein Inhaltsverzeichnis, das das Programm führt', :ok => true, :marks => [])
        index = ((model || {})['indexes'] || []).find { |x| x['kind'] == 'content' }
        typed = body.select { |p| p['text'].to_s.match?(TYPED_TOC_LINE) }
        problems = []
        unless typed.empty?
            problems << (typed.size == 1 ? "»#{short(typed.first, 3)}« ist eine Zeile eines Inhaltsverzeichnisses, die jemand getippt hat." :
                "#{typed.size} Zeilen sind ein Inhaltsverzeichnis, das jemand getippt hat, zum Beispiel »#{short(typed.first, 3)}«.")
            item.marks.concat(typed.map { |p| para_mark(p).merge(:title => 'getippt') })
        end
        if index.nil?
            problems << 'Ein Inhaltsverzeichnis, das das Programm aus den Überschriften macht, fehlt noch.'
        elsif index['up_to_date'] == false
            gone = Array(index['entries']) - Array(index['current'])
            problems << "Das Inhaltsverzeichnis ist nicht mehr aktuell#{gone.empty? ? '' : ": Es nennt noch »#{q(gone.first)}«"}. Aktualisiere es (Rechtsklick hinein)."
            item.marks.concat(all.select { |p| (p['where'] || {})['index'] == index['name'] }.first(1).map { |p| para_mark(p).merge(:title => 'nicht aktuell') })
        end
        return item if problems.empty?

        item.ok = false
        item.message = "#{problems.join(' ')} Ein Inhaltsverzeichnis, das das Programm einfügt, kennt die Überschriften und ihre Seiten; " \
            'nach dem Aktualisieren stimmt es wieder, auch wenn sich Seiten verschoben haben.'
        item
    end

    # ---- an own text (the hand-in): counts and lists

    def self.rule_words(rule, _body, _starter, all = nil, _model = nil)
        want = rule['words'].to_i
        count = (all || []).sum { |p| p['plain'].split(' ').count { |w| w.match?(/[[:alnum:]]/) } }
        item = Item.new(:rule => 'words', :label => "Mindestens #{want} Wörter", :ok => count >= want, :marks => [])
        item.message = "Dein Text hat #{count} #{count == 1 ? 'Wort' : 'Wörter'}; es sollen mindestens #{want} sein." unless item.ok
        item
    end

    # ---- a document exported as PDF (accept: pdf; the service reads it
    # with poppler: its text, its fonts, its pages)

    def self.rule_pdf(_rule, _body, _starter, _all = nil, model = nil)
        pages = model['page_count'].to_i
        item = Item.new(:rule => 'pdf', :label => 'Eine PDF-Datei', :ok => pages.positive?, :marks => [])
        item.message = 'In diesem PDF finde ich keine Seite.' unless item.ok
        item
    end

    # every font in the file: one that is not would look different on a
    # computer without it (Word and LibreOffice embed them when they export)
    def self.rule_pdf_fonts(_rule, _body, _starter, _all = nil, model = nil)
        missing = (model['fonts'] || []).reject { |f| f['embedded'] }.map { |f| f['name'] }.uniq
        item = Item.new(:rule => 'pdf_fonts', :label => 'Die Schriften sind im PDF', :ok => missing.empty?, :marks => [])
        item.message = "Die Schrift#{missing.size == 1 ? '' : 'en'} #{names(missing.first(3).map { |n| "»#{q(n)}«" })} " \
            "#{missing.size == 1 ? 'steckt' : 'stecken'} nicht im PDF; auf einem anderen Computer sieht es dann anders aus. " \
            'Exportiere noch einmal mit *Datei* und *Exportieren*, nicht über einen PDF-Drucker.' unless item.ok
        item
    end

    def self.rule_pdf_words(rule, _body, _starter, _all = nil, model = nil)
        want = rule['pdf_words'].to_i
        count = model['text'].to_s.split(/\s+/).count { |w| w.match?(/[[:alnum:]]/) }
        item = Item.new(:rule => 'pdf_words', :label => "Dein Artikel, mindestens #{want} Wörter", :ok => count >= want, :marks => [])
        item.message = "In diesem PDF stehen #{count} #{count == 1 ? 'Wort' : 'Wörter'}; dein Artikel hat mindestens #{want}. Ist es das PDF deines Artikels?" unless item.ok
        item
    end

    def self.rule_heading_count(rule, body, _starter, _all = nil, _model = nil)
        want = rule['heading_count'].to_i
        count = body.count { |p| level(p).positive? }
        item = Item.new(:rule => 'heading_count', :label => "Mindestens #{want} Überschriften mit Formatvorlage", :ok => count >= want, :marks => [])
        return item if item.ok

        # lines that look like headings but are not: short, bold as a whole
        looks = body.select { |p| level(p).zero? && p['plain'].split(' ').size.between?(1, 8) && !%w[Title Titel].include?(p['style']) && ranges(p).select { |_, _, q| visible?(q) }.all? { |_, _, q| q['bold'] } && !p['plain'].empty? }
        item.marks = looks.map { |p| para_mark(p).merge(:title => style_name(p)) }
        item.message = "Dein Text hat #{count == 0 ? 'keine Überschrift' : count == 1 ? 'eine Überschrift' : "#{count} Überschriften"} mit einer Formatvorlage; es sollen mindestens #{want} sein." +
            (looks.empty? ? '' : " »#{short(looks.first)}« sieht aus wie eine, ist aber ein normaler Absatz.") +
            ' Gib Zwischenüberschriften die Formatvorlage »Überschrift 1« oder »Überschrift 2«.'
        item
    end

    def self.rule_picture_count(rule, _body, _starter, _all = nil, model = nil)
        want = rule['picture_count'].to_i
        count = ((model || {})['pictures'] || []).size
        item = Item.new(:rule => 'picture_count', :label => want == 1 ? 'Mindestens ein Bild' : "Mindestens #{want} Bilder", :ok => count >= want, :marks => [])
        item.message = "In deinem Text #{count.zero? ? 'ist noch kein Bild' : count == 1 ? 'ist ein Bild' : "sind #{count} Bilder"}; es #{want == 1 ? 'soll mindestens eines' : "sollen mindestens #{want}"} sein: ein Foto oder eine Zeichnung, die zum Text passt." unless item.ok
        item
    end

    def self.rule_typed_lists(_rule, body, _starter, _all = nil, _model = nil)
        item = Item.new(:rule => 'typed_lists', :label => 'Listen, die das Programm führt', :ok => true, :marks => [])
        # a dash or dot alone is a list; a number only with a neighbour that
        # has one too (»14. Mai fuhren wir …« begins a paragraph, no list)
        numbered = ->(p) { p && p['text'].to_s.match?(/\A\s*\d{1,2}[.)]\s/) && !p['numbering'] }
        typed = body.each_with_index.select do |p, i|
            next false if p['numbering'] || !p['text'].to_s.match?(TYPED_MARKER)
            next true unless numbered.(p)
            numbered.(body[i - 1]) && i.positive? || numbered.(body[i + 1])
        end.map(&:first)
        return item if typed.empty?

        item.ok = false
        item.marks = typed.map { |p| para_mark(p).merge(:title => 'Zeichen von Hand') }
        item.message = "#{typed.size == 1 ? "Vor »#{short(typed.first, 3)}« steht ein Listenzeichen, das jemand getippt hat." : "Vor #{typed.size} Zeilen stehen Listenzeichen, die jemand getippt hat, zum Beispiel vor »#{short(typed.first, 3)}«."} " \
            'Eine Liste, die das Programm führt, rückt jeden Punkt gleich ein und zählt von selbst weiter.'
        item
    end

    # ---- where on the page

    def self.para_mark(p)
        b = p['box']
        return {} if b.nil?
        { :page => b['page'], :x => b['left'], :y => b['top'], :w => b['right'] - b['left'], :h => b['bottom'] - b['top'], :kind => 'box' }
    end

    def self.between_mark(q, p)
        a = q['box']
        b = p['box']
        return para_mark(p) if a.nil? || b.nil? || a['page'] != b['page']
        top = a['bottom'] - 2
        { :page => b['page'], :x => b['left'], :y => top, :w => b['right'] - b['left'], :h => [b['top'] - top + 2, 4].max, :kind => 'blank' }
    end

    # The place of count spaces (or a line break: at_end) after the
    # before-th word of a paragraph: between two words of the PDF, before
    # the first or after the last. Falls back to the whole paragraph when
    # the words of the PDF do not match those of the text.
    def self.gap_mark(p, before, count, at_end: false)
        b = p['box']
        words = p['words'] || []
        tokens = p['text'].to_s.split(/\s+/).reject(&:empty?)
        return para_mark(p) if b.nil? || words.empty? || words.size != tokens.size

        width = [count * 3.0, 6].max
        if before.zero?
            w = words.first
            x0 = b['left']
            return { :page => w[0], :x => x0, :y => w[2], :w => [w[1] - x0, 4].max, :h => w[4] - w[2], :kind => 'gap' }
        end
        a = words[before - 1]
        c = words[before]
        if c && !at_end && c[0] == a[0] && (c[2] - a[2]).abs < 2
            return { :page => a[0], :x => a[3], :y => a[2], :w => [c[1] - a[3], 4].max, :h => a[4] - a[2], :kind => 'gap' }
        end
        { :page => a[0], :x => a[3], :y => a[2], :w => width, :h => a[4] - a[2], :kind => 'gap' }
    end
end
