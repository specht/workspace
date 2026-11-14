# The bold line of an open point on a document card: what is wrong, as
# short as the line of a point met (»Keine leeren Absätze« / »Leere
# Absätze machen Abstand«). The card used the first sentence of the
# message, which for many rules is long (»An 5 Stellen stehen Leerzeichen
# hintereinander: …«); now the message stays whole below the line. A rule
# that knows the exact place (the cells of a spreadsheet) sets its own
# (Item#problem, document_check_sheet.rb); for the others this table,
# worded to fit one place as well as several. A first sentence that is
# short already (»Den Absatz »Ort« finde ich nicht mehr.«) stays the line.
# Plain Ruby, tested in document_problems_test.rb.
module DocumentProblems
    SHORT = 45

    # rule => the line, from the item (its label, its message)
    LINES = {
        'text' => ->(_) { 'Am Text hat sich etwas geändert' },
        'contains' => ->(_) { 'Es fehlt noch Text' },
        'same_as' => ->(i) { i.label.sub(/\AWie /, 'Nicht wie ') },
        'scripts' => ->(_) { 'Hoch- und Tiefstellen fehlt' },
        'spaces' => ->(_) { 'Leerzeichen hintereinander' },
        'empty_paragraphs' => ->(_) { 'Leere Absätze machen Abstand' },
        'line_breaks' => ->(_) { 'Zeilenumbrüche von Hand' },
        'align' => ->(i) { i.label.sub(/ ist /, ' ist nicht ') },
        'gaps' => ->(_) { 'Der Abstand zwischen den Absätzen fehlt' },
        'fonts' => ->(_) { 'Zu viele Schriftarten' },
        'size' => ->(_) { 'Text in anderen Größen' },
        'bold' => ->(_) { 'Nicht alles Wichtige ist fett' },
        'sparing' => ->(_) { 'Ganze Absätze fett' },
        'no_underline' => ->(_) { 'Text unterstrichen' },
        'no_caps' => ->(_) { 'Großbuchstaben als Format' },
        'list' => ->(_) { 'Die Liste stimmt noch nicht' },
        'table_sorted' => ->(i) { "Nicht #{i.label.sub(/\ANach/, 'nach').sub(/,.*\z/, '')}" },
        'crop' => ->(i) { "Nicht #{i.label.sub(/\A\p{Lu}/, &:downcase)}" },
        'order' => ->(_) { 'Die Reihenfolge stimmt nicht' },
        'own_style' => ->(_) { 'Noch keine eigene Formatvorlage' },
        'landscape' => ->(_) { 'Noch keine Seite quer' },
        'title_page' => ->(_) { 'Kopf- oder Fußzeile auf der Titelseite' },
        'tabs' => ->(i) { i.label.sub(/ an einem /, ' nicht an einem ') },
        'table' => ->(_) { 'Die Tabelle stimmt noch nicht' },
        'no_tabs' => ->(_) { 'Noch Tabs im Text' },
        'alt_text' => ->(_) { 'Bilder ohne Alternativtext' },
        'ratio' => ->(_) { 'Ein Bild ist verzerrt' },
        'captions' => ->(_) { 'Bildunterschriften zählen nicht selbst' },
        'wrap' => ->(i) { i.label.sub(/ steht /, ' steht nicht ') },
        'headings' => ->(i) { "Überschriften ohne »Überschrift #{i.label[/\d+/]}«" },
        'levels' => ->(_) { 'Eine Ebene übersprungen' },
        'no_direct' => ->(_) { 'Überschriften von Hand formatiert' },
        'style_changed' => ->(i) { "#{i.label[/»[^«]*«/]} noch wie am Anfang" },
        'styled' => ->(_) { 'Ohne die passende Formatvorlage' },
        'plain_text' => ->(_) { 'Text von Hand formatiert' },
        'span' => ->(_) { 'Die Überschrift geht nicht über beide Spalten' },
        'box' => ->(_) { 'Der Kasten stimmt noch nicht' },
        'columns' => ->(i) { i.label.sub(/\ADer Text in/, 'Noch nicht in') },
        'margins' => ->(_) { 'Der Seitenrand ist zu breit' },
        'justify' => ->(_) { 'Nicht im Blocksatz' },
        'hyphenation' => ->(_) { 'Silbentrennung aus' },
        'first_line_indent' => ->(_) { 'Absätze ohne Einzug' },
        'line_spacing' => ->(_) { 'Zeilenabstand nicht einzeilig' },
        'page_breaks' => ->(_) { 'Neue Seiten ohne Seitenumbruch' },
        'page_numbers' => ->(i) { i.message.to_s.start_with?('Die Seitenzahl zählt schon') ? 'Die Seitenanzahl fehlt' : 'Seitenzahlen von Hand' },
        'toc' => ->(_) { 'Kein Inhaltsverzeichnis des Programms' },
        'words' => ->(_) { 'Zu wenige Wörter' },
        'spelling' => ->(_) { 'Wörter nicht im Wörterbuch' },
        'pdf_fonts' => ->(_) { 'Schriften fehlen im PDF' },
        'pdf_words' => ->(_) { 'Zu wenige Wörter im PDF' },
        'header_text' => ->(i) { i.label.sub(/ in der Kopfzeile\z/, ' fehlt in der Kopfzeile') },
        'header_date' => ->(_) { 'Kein Datum als Feld in der Kopfzeile' },
        'footnotes' => ->(_) { 'Zu wenige Fußnoten' },
        'heading_count' => ->(_) { 'Zu wenige Überschriften' },
        'picture_count' => ->(_) { 'Zu wenige Bilder' },
        'typed_lists' => ->(_) { 'Getippte Listenzeichen' },
        # presentations
        'titles' => ->(_) { 'Folien ohne Titel' },
        'title_words' => ->(_) { 'Ein Titel sagt nicht, worum es geht' },
        'min_size' => ->(_) { 'Schrift zu klein' },
        'max_words' => ->(_) { 'Zu viele Wörter auf einer Folie' },
        'no_overflow' => ->(_) { 'Text läuft aus seinem Rahmen' },
        'notes' => ->(_) { 'Ganze Sätze auf den Folien' },
        'facts' => ->(_) { 'Es fehlt, was du erzählen willst' },
        'notes_words' => ->(_) { 'Zu viel in den Notizen' },
        'slide_count' => ->(_) { 'Die Zahl der Folien stimmt nicht' },
        'placeholders' => ->(_) { 'Text außerhalb der Platzhalter' },
    }.freeze

    # the first sentence of a message, as the card cuts it (document_task.js,
    # firstSentence): at . ! ? outside quotes, before a capital
    def self.first_sentence(message)
        depth = 0
        text = message.to_s
        text.each_char.with_index do |c, i|
            break if i >= text.size - 2

            if c == '»' || c == '„'
                depth += 1
            elsif (c == '«' || c == '“') && depth.positive?
                depth -= 1
            elsif depth.zero? && '.!?'.include?(c) && text[i + 1] == ' ' && text[i + 2].to_s.match?(/[A-ZÄÖÜ»„<]/)
                return text[0..i]
            end
        end
        text
    end

    # the line for an open point, or nil (the card then takes the first
    # sentence, short enough)
    def self.line(item)
        return nil if item.ok || item.message.to_s.empty?
        return item.problem if item.respond_to?(:problem) && item.problem
        return nil if first_sentence(item.message).size <= SHORT

        maker = LINES[item.rule]
        maker ? maker.call(item) : nil
    end
end
