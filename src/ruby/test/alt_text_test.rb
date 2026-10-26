require 'minitest/autorun'
require 'yaml'

# Every picture has an alt text that says what is to be seen (AGENTS.md).
# An <img> in inline code or a code block is an example, not a picture.
# Drafts and pages outside the catalogs (sirds, anaglyphs) are not checked.
class AltTextTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)
    UNPUBLISHED = %w[_drafts/ sirds/ anaglyphs/].freeze
    WITHOUT_ALT = /<img(?![^>]*\balt=)[^>]*>|<img[^>]*\balt=(''|"")[^>]*>/

    def test_every_picture_has_an_alt_text
        found = Dir[File.join(CONTENT, '**', '*.md')].flat_map do |file|
            name = file.sub("#{CONTENT}/", '')
            next [] if UNPUBLISHED.any? { |prefix| name.start_with?(prefix) }

            text = File.read(file, encoding: 'UTF-8')
            prose = text.split(/^```.*?^```[ \t]*$/m).map { |piece| piece.gsub(/`[^`\n]*`/, '') }.join
            prose.scan(WITHOUT_ALT).map { "#{name}: #{$~[0][0, 100]}" }
        end
        assert_empty found, "A picture without an alt text:\n#{found.join("\n")}"
    end

    # The picture on an activity's card in the catalog (meta »image«) says
    # what it shows (»image_alt«), unless the page has the same picture with
    # an alt text (docs/aufgaben.md). Only cards: the logos of the language
    # tiles stand next to their names, hidden sections show nothing.
    def test_every_card_picture_has_an_alt_text
        # .slug: a page shown in development only (main.rb)
        cards = YAML.load_file(File.join(CONTENT, 'sections.yaml')).reject { |s| s['tiles'] || s['hidden'] }
            .flat_map { |s| s['entries'] }.map { |slug| slug.delete_prefix('.') }
        found = cards.filter_map do |slug|
            file = Dir[File.join(CONTENT, '**', "#{slug}.md")].find { |f| File.basename(File.dirname(f)) == slug }
            next "#{slug}: no page" unless file

            text = File.read(file, encoding: 'UTF-8')
            meta = text[%r{<div class='meta'>(.*?)</div>}m, 1].to_s
            image = meta[/^image:\s*(\S+)/, 1]
            next unless image
            next if meta.match?(/^image_alt:\s*\S/)

            basename = File.basename(image.split(':').first)
            on_page = text.scan(/<img[^>]*>/).any? { |img| img.include?(basename) && img.match?(/\balt=(['"])[^'"]+\1/) }
            slug unless on_page
        end
        assert_empty found, "A card picture without image_alt:\n#{found.join("\n")}"
    end
end
