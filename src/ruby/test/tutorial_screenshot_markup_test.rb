require 'minitest/autorun'

# A recipe (<!-- tutorial-screenshot … -->) has to start a block of its own.
# Directly under an image or a line of text, Markdown puts the comment into
# that paragraph and prints it on the page as text.
class TutorialScreenshotMarkupTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    # Lines a recipe may follow directly: nothing, the end of a code block,
    # the end of another comment, or an HTML block such as the figures of a
    # screen gallery.
    def starts_a_block?(line)
        line.strip.empty? || line.strip.start_with?('```') || line.strip.end_with?('-->') ||
            line.strip.match?(%r{\A</?(div|figure|section|table|details|ul|ol|li)\b}i)
    end

    def test_every_recipe_starts_a_block_of_its_own
        problems = []
        Dir[File.join(CONTENT, '**', '*.md')].sort.each do |path|
            next if path.include?('/_drafts/')
            lines = File.readlines(path, chomp: true)
            lines.each_with_index do |line, index|
                next unless line.start_with?('<!-- tutorial-screenshot', '<!-- tutorial-step') && index > 0
                next if starts_a_block?(lines[index - 1])
                problems << "#{path.delete_prefix(CONTENT + '/')}:#{index + 1}: blank line missing after »#{lines[index - 1].strip[0, 60]}«"
            end
        end
        assert_empty problems, "A recipe directly under this line would appear on the page as text:\n#{problems.join("\n")}"
    end

    def test_the_rule_catches_an_image_directly_above
        refute starts_a_block?("<img class='full' src='a.webp' alt=''>")
        refute starts_a_block?('Ein Satz im Text.')
        assert starts_a_block?('')
        assert starts_a_block?('```')
        assert starts_a_block?('<figure>')
        assert starts_a_block?('<!-- Screenshot: eine Notiz -->')
    end
end
