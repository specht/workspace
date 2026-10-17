require 'minitest/autorun'

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
end
