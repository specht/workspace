# The line of the curriculum each activity carries (meta curriculum:), which
# only logged-in teachers see: on its catalog card, at the top of its page and
# in the Kata strip. The names are those of the Berlin-Brandenburg plans,
# without numbers or course codes (docs/lehrplan.md):
#
#   Sek I: Algorithmisches Problemlösen (algorithmische Grundstrukturen); Oberstufe: Algorithmen und Datenstrukturen
#
# A part for Sek I, one for the Oberstufe or both, in this order, divided by
# »; «. In a part names divided by », «, each with details in brackets if
# wanted; only the details are free.
module Curriculum
    # Rahmenlehrplan Teil C Informatik, Wahlpflichtfach 7–10 (2015): the
    # Themenfelder 3.1 to 3.10, the last five Wahlthemenfelder.
    SEK_I = [
        'Standardsoftware',
        'Informatiksysteme',
        'Leben in und mit vernetzten Systemen',
        'Information und Daten',
        'Algorithmisches Problemlösen',
        'Geschichte der Informatik',
        'Datenbanken',
        'Projektmanagement',
        'Physical Computing',
        'Digitale Bilder und Visualisierung',
    ].freeze

    # Rahmenlehrplan für die gymnasiale Oberstufe, Teil C Informatik (Berlin):
    # the four Themenbereiche of the Einführungsphase; the contents of the
    # Kurshalbjahre (Datenbanken und Softwareentwicklung, Grundlagen der
    # Informatik, Softwareprojekt); the nine Vertiefungsgebiete.
    OBERSTUFE = [
        'Grundlagen informatischen Arbeitens',
        'Algorithmen und Softwareentwicklung',
        'Datenbanken und Datenschutz',
        'Computernetze',
        'Datenbanken',
        'Algorithmen und Datenstrukturen',
        'objektorientierte Modellierung',
        'objektorientierte Programmierung',
        'Syntax und Semantik',
        'Rechner und Netze',
        'Sprachen und Automaten',
        'Softwareprojekt',
        'Vertiefung Applikative Programmierung',
        'Vertiefung Kryptologie und Datensicherheit',
        'Vertiefung Computergrafik',
        'Vertiefung Computer-Netze',
        'Vertiefung Künstliche Intelligenz',
        'Vertiefung Technische Informatik',
        'Vertiefung Maschinennahe Programmierung',
        'Vertiefung Informatik und Gesellschaft',
        'Vertiefung Theoretische Informatik',
    ].freeze

    LEVELS = { 'Sek I' => SEK_I, 'Oberstufe' => OBERSTUFE }.freeze

    # A right line as its entries, in its order: [level, name, details]
    # (details nil without brackets). [] for a line that is not right.
    def self.entries(line)
        return [] unless problems(line).empty?

        split(line.to_s.strip, ';').flat_map do |part|
            level = part[/\A(Sek I|Oberstufe): /, 1]
            split(part.delete_prefix("#{level}: "), ',').map do |item|
                name = item.sub(/\s*\(.*\)\z/m, '')
                details = item[/\((.*)\)\z/m, 1]
                [level, name, details]
            end
        end
    end

    # What is wrong with a line, as messages; [] when it is right.
    def self.problems(line)
        text = line.to_s.strip
        return ['no line'] if text.empty?

        parts = split(text, ';')
        levels = parts.map { |part| part[/\A(Sek I|Oberstufe): /, 1] }
        return ["each part begins with »Sek I: « or »Oberstufe: «: #{text}"] if levels.include?(nil)
        return ["Sek I before Oberstufe, each once: #{text}"] unless levels == levels.uniq && levels == LEVELS.keys & levels

        parts.zip(levels).flat_map do |part, level|
            split(part.delete_prefix("#{level}: "), ',').map { |item| item.sub(/\s*\(.*\)\z/m, '') }.filter_map do |name|
                "not a name of the plan for #{level}: »#{name}«" unless LEVELS[level].include?(name)
            end
        end
    end

    # text split at »<mark> « outside brackets, each piece stripped: the
    # parts of a line at »; «, the names of a part at », «. A bracket left
    # open keeps the rest in one piece, so that it is no name.
    def self.split(text, mark)
        pieces = ['']
        depth = 0
        text.each_char.with_index do |c, i|
            depth += 1 if c == '('
            depth -= 1 if c == ')'
            if depth.zero? && c == mark && text[i + 1] == ' '
                pieces << ''
            else
                pieces[-1] += c
            end
        end
        pieces.map(&:strip)
    end
end
