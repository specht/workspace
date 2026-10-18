require 'minitest/autorun'

# The site's Markdown has no pipe tables: »| a | b |« with »| --- |« below
# comes out as one paragraph of bars. Tables in content are HTML
# (<table class='table'>), with </table> on a line of its own (AGENTS.md).
class NoPipeTablesTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def test_no_pipe_tables_in_pages
        found = Dir[File.join(CONTENT, '**', '*.md')].flat_map do |file|
            text = File.read(file, encoding: 'UTF-8').gsub(/^```.*?^```/m) { |block| "\n" * block.count("\n") }
            text.lines.each_with_index.select { |line, _| line =~ /\A\|\s*:?-{3,}/ }
                .map { |_, i| "#{file.sub(CONTENT + '/', '')}:#{i + 1}" }
        end
        assert_empty found, "Pipe tables (write an HTML table instead):\n#{found.join("\n")}"
    end
end
