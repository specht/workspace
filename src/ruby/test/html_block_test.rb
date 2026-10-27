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

    # Inside a block of HTML (a hint box, a table, a figure) Redcarpet leaves
    # the text alone: **bold** shows its asterisks. There it is <strong>.
    def test_no_markdown_bold_inside_html
        found = Dir[File.join(CONTENT, '**', '*.md')].flat_map do |file|
            tag = nil
            depth = 0
            fence = false
            File.readlines(file).each_with_index.filter_map do |line, i|
                if line.start_with?('```')
                    fence = !fence
                    next
                end
                next if fence
                tag = Regexp.last_match(1) if tag.nil? && line =~ /\A<(div|table|section|figure|details|aside)\b/
                next unless tag

                depth += line.scan(/<#{tag}\b/).size - line.scan(%r{</#{tag}>}).size
                text = line.gsub(%r{<code>.*?</code>|`[^`]*`}, '')
                hit = text =~ /\*\*[^*\s][^*]*\*\*/ ? "#{file.sub("#{CONTENT}/", '')}:#{i + 1}: #{line.strip[0, 70]}" : nil
                tag = nil if depth <= 0
                depth = 0 if tag.nil?
                hit
            end
        end
        assert_empty found, "Inside HTML, write <strong> instead of **:\n#{found.join("\n")}"
    end
end
