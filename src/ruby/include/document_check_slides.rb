# The rules for a presentation (docs/aufgaben.md, »document«, Präsentationen;
# docs/plan-praesentationen.md). The office service reads the slides
# (src/office/slides.py): each slide's layout, its placeholders and text
# boxes with their letters (size, font, colour, how far the text runs over),
# pictures, notes, transitions and animations; each slide is drawn, and the
# marks of these rules stand on the shapes.
#
# Sizes are compared as on PowerPoint's 16:9 slide (33,867 cm, 960 pt
# wide): a slide of another size has its letters scaled to that width, and
# text a box shrinks to fit (»Autofit«) counts at the size it is shown.
module DocumentCheck
    SLIDE_RULES = %w[titles title_words min_size max_words no_overflow notes facts notes_words slide_count placeholders].freeze
    # the shapes that carry the slide's text (not its notes, footer or number)
    SLIDE_TEXT_KINDS = %w[title subtitle body text shape].freeze
    SLIDE_WIDTH = 960.0

    def self.slides?(model)
        model.is_a?(Hash) && model['kind'] == 'slides'
    end

    def self.slides_check?(check)
        check['file'].to_s.match?(/\.(pptx|odp|ppt)\z/i)
    end

    def self.slides_of(model)
        model['slides'] || []
    end

    # where a shape stands in the drawing of its slide
    def self.shape_mark(slide, shape, title)
        { :page => slide['index'] + 1, :x => shape['x'], :y => shape['y'], :w => shape['w'], :h => shape['h'],
          :kind => 'box', :title => title }
    end

    def self.slide_mark(model, slide, title)
        { :page => slide['index'] + 1, :x => 0, :y => 0, :w => model['width'].to_f, :h => model['height'].to_f,
          :kind => 'box', :title => title }
    end

    def self.text_shapes(slide)
        (slide['shapes'] || []).select { |s| SLIDE_TEXT_KINDS.include?(s['kind']) && !s['text'].to_s.strip.empty? }
    end

    def self.title_shape(slide)
        (slide['shapes'] || []).find { |s| s['kind'] == 'title' && s['placeholder'] && !s['text'].to_s.strip.empty? }
    end

    def self.slide_words(slide)
        text_shapes(slide).sum { |s| s['text'].to_s.scan(/[[:alnum:]][[:alnum:]'’-]*/).size }
    end

    # the size of a run of letters as it is shown, on a 960 pt wide slide
    def self.shown_size(model, shape, run)
        scale = (shape['font_scale'] || 1.0).to_f
        width = model['width'].to_f.positive? ? model['width'].to_f : SLIDE_WIDTH
        run['size'].to_f * scale * SLIDE_WIDTH / width
    end

    def self.slide_label(slide)
        "Folie #{slide['index'] + 1}"
    end

    # ---- the rules

    # every slide has a title, in the layout's title placeholder: the
    # outline, the presenter view and a screen reader find it there
    def self.rule_titles(rule, _body, _starter, _all = nil, model = nil)
        item = Item.new(:rule => 'titles', :label => 'Jede Folie hat einen Titel', :ok => true, :marks => [])
        missing = slides_of(model).reject { |s| s['hidden'] || title_shape(s) }
        return item if missing.empty?

        item.ok = false
        item.marks = missing.map { |s| slide_mark(model, s, 'kein Titel') }
        first = missing.first
        boxed = text_shapes(first).find { |s| !s['placeholder'] }
        item.message = "#{slide_label(first)}#{missing.size > 1 ? " und #{missing.size - 1} weitere" : ''} " \
            "#{missing.size > 1 ? 'haben' : 'hat'} keinen Titel im Titelplatzhalter." +
            (boxed ? " Ihr Text #{q(boxed['text'].to_s.lines.first.to_s.strip[0, 40])} steht in einem Textfeld, das jemand von Hand daraufgesetzt hat." : '') +
            ' Ein Titel gehört in den Platzhalter »Titel hinzufügen« des Layouts.'
        item
    end

    # the title of every slide with content says something: at least so
    # many words (»Kraniche ziehen im Herbst nach Süden«, not »Zug«); the
    # title slide and slides without a body are left out
    def self.rule_title_words(rule, _body, _starter, _all = nil, model = nil)
        min = rule['title_words'].to_i
        item = Item.new(:rule => 'title_words', :label => 'Jeder Titel sagt, worum es auf der Folie geht', :ok => true, :marks => [])
        short = slides_of(model).select do |s|
            next false if s['hidden'] || (s['shapes'] || []).none? { |sh| sh['kind'] == 'body' && !sh['text'].to_s.strip.empty? }

            title = title_shape(s)
            title && title['text'].to_s.scan(/[[:alnum:]][[:alnum:]'’-]*/).size < min
        end
        return item if short.empty?

        item.ok = false
        item.marks = short.map { |s| shape_mark(s, title_shape(s), 'Titel aus einem Wort') }
        first = title_shape(short.first)
        item.message = "Der Titel #{q(first['text'].to_s.strip)} auf #{slide_label(short.first)} nennt nur ein Thema. " \
            'Sag im Titel, was man sich von der Folie merken soll, in einem kurzen Satz.'
        item
    end

    # no letters on a slide smaller than this (pt, on PowerPoint's slide)
    def self.rule_min_size(rule, _body, _starter, _all = nil, model = nil)
        min = rule['min_size'].to_f
        item = Item.new(:rule => 'min_size', :label => "Alle Schrift ist mindestens #{number(min)} pt groß", :ok => true, :marks => [])
        small = []
        slides_of(model).each do |slide|
            text_shapes(slide).each do |shape|
                sizes = (shape['paragraphs'] || []).flat_map { |p| p['runs'] || [] }.map { |r| shown_size(model, shape, r) }
                next if sizes.empty? || sizes.min >= min - 0.25

                small << [slide, shape, sizes.min]
            end
        end
        return item if small.empty?

        item.ok = false
        item.marks = small.map { |slide, shape, size| shape_mark(slide, shape, "#{number(size.round(1))} pt") }
        slide, shape, size = small.min_by { |_, _, s| s }
        shrunk = shape['font_scale'] && shape['font_scale'].to_f < 0.99
        item.message = "Auf #{slide_label(slide)} ist Schrift nur #{number(size.round)} pt groß" +
            (shrunk ? ', weil das Programm den Text verkleinert hat, damit er in seinen Rahmen passt' : '') +
            ". Aus der letzten Reihe liest man das nicht. Weniger Text, dafür größer: mindestens #{number(min)} pt."
        item
    end

    # at most this many words on a slide (its title included)
    def self.rule_max_words(rule, _body, _starter, _all = nil, model = nil)
        max = rule['max_words'].to_i
        item = Item.new(:rule => 'max_words', :label => "Auf keiner Folie stehen mehr als #{max} Wörter", :ok => true, :marks => [])
        full = slides_of(model).map { |s| [s, slide_words(s)] }.select { |_, n| n > max }
        return item if full.empty?

        item.ok = false
        item.marks = full.map { |s, n| slide_mark(model, s, "#{n} Wörter") }
        slide, words = full.max_by { |_, n| n }
        item.message = "Auf #{slide_label(slide)} stehen #{words} Wörter. Wer so viel lesen muss, hört dir nicht mehr zu. " \
            "Auf die Folie gehören Stichwörter, höchstens #{max} Wörter; die ganzen Sätze sagst du, sie stehen in den Notizen."
        item
    end

    # no text runs past its box or off the slide
    def self.rule_no_overflow(rule, _body, _starter, _all = nil, model = nil)
        item = Item.new(:rule => 'no_overflow', :label => 'Kein Text läuft aus seinem Rahmen', :ok => true, :marks => [])
        height, width = model['height'].to_f, model['width'].to_f
        over = []
        slides_of(model).each do |slide|
            text_shapes(slide).each do |shape|
                bottom = shape['y'].to_f + shape['h'].to_f + shape['overflow'].to_f
                over << [slide, shape] if shape['overflow'].to_f > 3 || bottom > height + 3 || shape['x'].to_f + shape['w'].to_f > width + 3
            end
        end
        return item if over.empty?

        item.ok = false
        item.marks = over.map { |slide, shape| shape_mark(slide, shape, 'läuft über') }
        slide, shape = over.first
        item.message = "Auf #{slide_label(slide)} läuft der Text #{q(shape['text'].to_s.lines.first.to_s.strip[0, 40])} aus seinem Rahmen" +
            (shape['y'].to_f + shape['h'].to_f + shape['overflow'].to_f > height + 3 ? ' und über den Rand der Folie' : '') +
            '. Kürze ihn, statt ihn kleiner zu machen.'
        item
    end

    # these sentences (pieces of them) are in the notes of some slide: the
    # speaker's text, moved off the slides
    def self.rule_notes(rule, _body, _starter, _all = nil, model = nil)
        pieces = Array(rule['notes'])
        item = Item.new(:rule => 'notes', :label => 'Die ganzen Sätze stehen in den Notizen', :ok => true, :marks => [])
        notes = slides_of(model).map { |s| squish(s['notes']).downcase }.join(' ')
        missing = pieces.reject { |p| notes.include?(squish(p).downcase) }
        return item if missing.empty?

        item.ok = false
        on_slide = slides_of(model).find { |s| text_shapes(s).any? { |sh| squish(sh['text']).downcase.include?(squish(missing.first).downcase) } }
        item.marks = on_slide ? [slide_mark(model, on_slide, 'gehört in die Notizen')] : []
        item.message = "Der Satz #{q(missing.first)} steht in keiner Notiz" +
            (on_slide ? ", noch auf #{slide_label(on_slide)}" : '') +
            (missing.size > 1 ? " (und #{missing.size - 1} weitere)" : '') +
            '. Was du sagen willst, steht in den Notizen unter der Folie: Dort liest du es, das Publikum nicht.'
        item
    end

    # the Referat's facts are still there, on a slide or in its notes, in
    # whatever words: each piece of the list (alternatives with |) somewhere
    def self.rule_facts(rule, _body, _starter, _all = nil, model = nil)
        pieces = Array(rule['facts'])
        item = Item.new(:rule => 'facts', :label => 'Was du erzählen willst, ist noch da', :ok => true, :marks => [])
        all = slides_of(model).flat_map { |s| text_shapes(s).map { |sh| sh['text'].to_s } + [s['notes'].to_s] }
        text = squish(all.join(' ')).downcase
        missing = pieces.reject { |p| p.split('|').any? { |alt| text.include?(squish(alt).downcase) } }
        return item if missing.empty?

        item.ok = false
        item.message = "Von #{q(missing.first.split('|').first)} steht nichts mehr da, weder auf einer Folie noch in den Notizen" +
            (missing.size > 1 ? " (und #{missing.size - 1} weitere)" : '') +
            '. Was nicht auf die Folie kommt, merkst du dir als Stichpunkt in den Notizen.'
        item
    end

    # the notes are a cue card, not a text to read out: at most so many
    # words under a slide
    def self.rule_notes_words(rule, _body, _starter, _all = nil, model = nil)
        max = rule['notes_words'].to_i
        item = Item.new(:rule => 'notes_words', :label => "Die Notizen sind ein Spickzettel, höchstens #{max} Wörter pro Folie", :ok => true, :marks => [])
        long = slides_of(model).map { |s| [s, s['notes'].to_s.scan(/[[:alnum:]][[:alnum:]'’-]*/).size] }.select { |_, n| n > max }
        return item if long.empty?

        item.ok = false
        item.marks = long.map { |s, n| slide_mark(model, s, "#{n} Wörter in den Notizen") }
        slide, words = long.max_by { |_, n| n }
        item.message = "Unter #{slide_label(slide)} stehen #{words} Wörter in den Notizen. Das ist ein Text zum Ablesen; " \
            'wer abliest, schaut auf den Bildschirm statt ins Publikum. Ein paar Stichpunkte reichen, die dich erinnern, was du erzählen willst.'
        item
    end

    # so many slides: [min, max]
    def self.rule_slide_count(rule, _body, _starter, _all = nil, model = nil)
        min, max = Array(rule['slide_count']).map(&:to_i)
        n = slides_of(model).reject { |s| s['hidden'] }.size
        label = min == max ? "#{min} Folien" : "#{min} bis #{max} Folien"
        item = Item.new(:rule => 'slide_count', :label => label, :ok => n >= min && n <= max, :marks => [])
        item.message = "Die Präsentation hat #{n} #{n == 1 ? 'Folie' : 'Folien'}, gewünscht sind #{label}." unless item.ok
        item
    end

    # no text outside the layout's placeholders: no text boxes put on by hand
    def self.rule_placeholders(rule, _body, _starter, _all = nil, model = nil)
        item = Item.new(:rule => 'placeholders', :label => 'Text steht in den Platzhaltern des Layouts', :ok => true, :marks => [])
        boxes = slides_of(model).flat_map { |s| text_shapes(s).reject { |sh| sh['placeholder'] }.map { |sh| [s, sh] } }
        return item if boxes.empty?

        item.ok = false
        item.marks = boxes.map { |s, sh| shape_mark(s, sh, 'Textfeld') }
        slide, shape = boxes.first
        item.message = "Auf #{slide_label(slide)} steht #{q(shape['text'].to_s.lines.first.to_s.strip[0, 40])} in einem Textfeld, das von Hand daraufgesetzt ist. " \
            'Nimm ein Layout mit Platzhaltern und schreib den Text dort hinein.'
        item
    end
end
