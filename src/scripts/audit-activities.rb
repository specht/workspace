#!/usr/bin/env ruby
# The mechanical part of docs/checkliste.md for every activity in the catalog,
# as a table: what can be counted is counted, the rest needs reading.
#
#   src/scripts/audit-activities.rb             # every activity in the catalog
#   src/scripts/audit-activities.rb routenplaner calculator
#
# Columns: Aufgaben (tasks in tasks.yaml; not for the pages in TASKLESS),
# Gliederung (## and ### headings; »flach« when a chapter of more than
# LONG_CHAPTER words of prose has no ###), Sprachen (languages of the page),
# Notiz (lehrkraft.md opens with »Lernende«), generiert / von Hand (pictures
# from recipes and others; »von Hand« counts photos and diagrams too),
# dunkel (dark variants among the generated pictures), Karte (whether the card
# picture comes from a recipe and has a NAME-dark version). A »!« marks what
# the checklist asks for.
require 'json'
require 'yaml'

ROOT = File.expand_path('../..', __dir__)
CONTENT = File.join(ROOT, 'src/content')
# what the Workspace is and how to work in it: pages to read, nothing to check
TASKLESS = %w[about basics].freeze
LONG_CHAPTER = 800

def catalog
    YAML.load_file(File.join(CONTENT, 'sections.yaml')).reject { |section| section['hidden'] }
        .flat_map { |section| section['entries'] || [] }
        .map { |entry| entry.to_s.delete_prefix('.') }
end

def local_images(markdown)
    markdown.gsub(/^[ \t]*```.*?^[ \t]*```/m, '')
            .scan(/<(?:img|video|source)\b[^>]*\bsrc=['"]([^'"]+)['"]/i).flatten
            .map { |src| src.split(/[?#]/, 2).first }
            .reject { |src| src.empty? || src =~ %r{\A(?:[a-z]+:|//|/)}i }.uniq
end

def audit(slug)
    dir = File.join(CONTENT, slug)
    page = File.join(dir, "#{slug}.md")
    return nil unless File.file?(page)

    markdown = File.read(page)
    meta = markdown[%r{<div class='meta'>(.*?)</div>}m, 1].to_s
    kind = meta[/^kind:\s*(\S+)/, 1]
    return nil if kind == 'steckbrief'

    tasks_file = File.join(dir, 'tasks.yaml')
    tasks = File.file?(tasks_file) ? ((YAML.load_file(tasks_file) || {})['tasks'] || []) : []
    # what a reader reads: no code, no widget scripts, no recipes
    prose = markdown.gsub(/^[ \t]*```.*?^[ \t]*```/m, '').gsub(%r{<(script|style)\b.*?</\1>}m, '').gsub(/<!--.*?-->/m, '')
    h2 = prose.scan(/^## /).size
    h3 = prose.scan(/^### /).size
    flat = prose.split(/^(?=## )/).any? do |chapter|
        chapter.start_with?('## ') && !chapter.match?(/^### /) && chapter.gsub(/<[^>]+>/, ' ').split.size > LONG_CHAPTER
    end
    languages = meta[/^languages:\s*\[(.*?)\]/, 1]&.split(',')&.map(&:strip) || [meta[/^language:\s*(.+)$/, 1].to_s.strip].reject(&:empty?)
    note = File.join(dir, 'lehrkraft.md')
    note_ok = File.file?(note) && File.read(note).strip.split(/\n\s*\n/).first.to_s.include?('Lernende')
    manifest = File.join(dir, '.tutorial-screenshots.json')
    generated = File.file?(manifest) ? (JSON.parse(File.read(manifest))['screenshots'] || {}).keys : []
    images = local_images(markdown)
    manual = images.reject { |src| generated.include?(src) }
    dark = generated.count { |name| File.file?(File.join(dir, name.sub(/(\.\w+)\z/, '-dark\1'))) }
    card = meta[/^image:\s*([^:\s]+)/, 1]
    {
        slug: slug, kind: kind.to_s,
        tasks: tasks.size,
        outline: "#{h2}/#{h3}#{flat ? ' flach!' : ''}",
        languages: languages.join(', '),
        note: note_ok ? 'ja' : 'fehlt!',
        generated: generated.size, manual: manual.size,
        dark: dark,
        card: if card.nil?
                  'fehlt!'
              else
                  dark_card = File.file?(File.join(dir, card.sub(/(\.\w+)\z/, '-dark\1')))
                  (generated.include?(card) ? 'Rezept' : 'kein Rezept') + (dark_card ? '' : ', nicht dunkel')
              end
    }
end

slugs = ARGV.empty? ? catalog : ARGV
rows = slugs.filter_map { |slug| audit(slug) }
columns = { slug: 'Aktivität', kind: 'Art', tasks: 'Aufgaben', outline: 'Gliederung', languages: 'Sprachen',
            note: 'Notiz', generated: 'generiert', manual: 'von Hand', dark: 'dunkel', card: 'Karte' }
widths = columns.to_h { |key, title| [key, ([title] + rows.map { |row| row[key].to_s }).map(&:length).max] }
line = ->(cells) { cells.map { |key, value| value.to_s.ljust(widths[key]) }.join('  ') }
puts line.(columns)
puts line.(columns.to_h { |key, _| [key, '-' * widths[key]] })
rows.each do |row|
    row = row.merge(tasks: row[:tasks].zero? && !TASKLESS.include?(row[:slug]) ? '0!' : row[:tasks])
    puts line.(row)
end
puts
puts "#{rows.size} Aktivitäten. »!« heißt: Das verlangt docs/checkliste.md. »von Hand« zählt auch Fotos und Zeichnungen;"
puts 'ob eine Aufgabe vom Text verraten wird oder ein Bild ein Rezept bräuchte, sagt nur das Lesen.'
