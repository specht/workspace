require 'minitest/autorun'

# Redcarpet ends a block of HTML only at a closing tag at the start of a
# line. A tag that runs over several lines (a long data-query) and closes on
# its last one, …;"></div>, keeps the block open: the page after it, up to
# the next </div> at the start of a line, comes out as raw Markdown. So the
# closing tag of such an element goes on a line of its own (AGENTS.md).
class HtmlBlockTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def test_a_tag_over_several_lines_closes_on_a_line_of_its_own
        found = Dir[File.join(CONTENT, '**', '*.md')].flat_map do |file|
            name = file.sub("#{CONTENT}/", '')
            lines = File.read(file, encoding: 'UTF-8').lines
            in_code = false
            open_at = nil
            lines.each_with_index.filter_map do |line, i|
                in_code = !in_code if line.start_with?('```')
                next if in_code

                if open_at.nil? && line =~ /\A<(div|section|figure)\b(?![^>]*>)/
                    open_at = i
                    next
                end
                next unless open_at

                closes_tag = line.include?('>')
                hit = closes_tag && line =~ %r{></(div|section|figure)>\s*\z} ? "#{name}:#{i + 1}: #{line.strip[-60..] || line.strip}" : nil
                open_at = nil if closes_tag
                hit
            end
        end
        assert_empty found, "Put the closing tag on a line of its own:\n#{found.join("\n")}"
    end
end
