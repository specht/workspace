require 'minitest/autorun'

# A <!-- tutorial-scope --> marker on the line right after a picture or a line
# of text belongs to that paragraph for Markdown, which then prints it as text
# (»<!-- /tutorial-scope -->« on the page). After a blank line, a block tag
# (</figure>, </div>), a code fence or another comment it stays a comment.
class ScopeMarkersTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)
    MARKER = /\A<!-- \/?tutorial-scope -->\z/
    BLOCK_BEFORE = %r{\A(</?(figure|div|video|table|section|details)\b|<!--|-->|```)}

    def test_markers_do_not_follow_inline_lines
        found = Dir[File.join(CONTENT, '**', '*.md')].flat_map do |file|
            lines = File.readlines(file, encoding: 'UTF-8').map(&:strip)
            lines.each_with_index.select do |line, i|
                i > 0 && line =~ MARKER && !lines[i - 1].empty? && lines[i - 1] !~ BLOCK_BEFORE
            end.map { |line, i| "#{file.sub(CONTENT + '/', '')}:#{i + 1}: after »#{lines[i - 1][0, 50]}«" }
        end
        assert_empty found, "Put a blank line before these scope markers:\n#{found.join("\n")}"
    end
end
