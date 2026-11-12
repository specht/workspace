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
#
# Every rule may carry label: (the point on the checklist) and say: (what
# the card says when it fails, instead of the built-in sentence).
module DocumentCheck
    RULES = %w[text spaces empty_paragraphs line_breaks align gaps].freeze
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
        errors << 'file: the starting document below src/static' unless file.is_a?(String) && !file.empty? && !file.start_with?('/') && !file.include?('..')
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
                else [name]
            end
            extra = rule.keys - known
            errors << "unknown keys for rule #{name}: #{extra.join(', ')}" unless extra.empty?
            errors << "align must be one of #{ALIGN.keys.join(', ')}" if name == 'align' && !ALIGN.key?(rule['align'])
            errors << 'align needs paragraph: (a piece of its text)' if name == 'align' && !(rule['paragraph'].is_a?(String) && !rule['paragraph'].empty?)
            errors << 'gaps must look like 6pt' if name == 'gaps' && pt(rule['gaps']) <= 0
            errors << 'gaps needs before: (pieces of the paragraphs)' if name == 'gaps' && !(rule['before'].is_a?(Array) && rule['before'].all? { |b| b.is_a?(String) })
            %w[label say].each { |k| errors << "#{k} must be text" if rule.key?(k) && !rule[k].is_a?(String) }
        end
        errors
    end

    def self.rule_name(rule)
        rule.is_a?(String) ? rule : (RULES & rule.keys).first
    end

    # model: what the office service answered for the upload; starter: the
    # same for the task's starting file (for the rule text), or nil.
    # Returns [status, items, message].
    def self.evaluate(check, model, starter)
        paragraphs = (model['paragraphs'] || []).map { |p| p.merge('plain' => squish(p['text'])) }
        body = paragraphs.select { |p| (p['where'] || {}).empty? }
        items = (check['rules'] || []).map do |rule|
            item = send("rule_#{rule_name(rule)}", rule.is_a?(Hash) ? rule : {}, body, starter)
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
        }
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
        w = p['plain'].split(' ')
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

    def self.rule_text(_rule, body, starter)
        item = Item.new(:rule => 'text', :label => 'Der Text ist noch vollständig', :ok => true, :marks => [])
        return item if starter.nil? || starter['paragraphs'].nil?

        words = ->(ps) { ps.flat_map { |p| p['text'].to_s.split(/\s+/) }.reject(&:empty?) }
        have = words.(body).tally
        missing = words.(starter['paragraphs']).tally.flat_map { |w, n| [w] * [n - have[w].to_i, 0].max }
        return item if missing.empty?

        item.ok = false
        item.message = "Aus der Vorlage fehlen #{missing.size == 1 ? 'ein Wort' : "#{missing.size} Wörter"}: " \
            "»#{q(missing.first(6).join(' '))}«#{missing.size > 6 ? ' …' : ''}. Hier änderst du nur, wie der Text gebaut ist, nicht den Text selbst."
        item
    end

    def self.rule_spaces(_rule, body, _starter)
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

    def self.rule_empty_paragraphs(_rule, body, _starter)
        item = Item.new(:rule => 'empty_paragraphs', :label => 'Keine leeren Absätze', :ok => true, :marks => [])
        # empty paragraphs at the very end change nothing on the page
        last = body.rindex { |p| !empty?(p) } || -1
        groups = body[0..last].slice_when { |a, b| empty?(a) != empty?(b) }.select { |g| empty?(g.first) }
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

    def self.rule_line_breaks(_rule, body, _starter)
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
            '(Umschalt+Enter). Innerhalb eines Absatzes bricht das Programm die Zeilen selbst um; ein Umbruch von Hand steht auch dann noch, wenn die Zeile längst woanders endet.'
        item
    end

    def self.rule_align(rule, body, _starter)
        want = rule['align'].to_s
        name = rule['paragraph'].to_s
        item = Item.new(:rule => 'align', :label => "»#{name}« ist #{ALIGN[want]}", :ok => false, :marks => [])
        p = body.find { |q| q['plain'].include?(name) }
        if p.nil?
            item.message = "Den Absatz »#{name}« finde ich nicht mehr."
            return item
        end
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

    def self.rule_gaps(rule, body, _starter)
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
            next if gap >= min - 0.05

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
