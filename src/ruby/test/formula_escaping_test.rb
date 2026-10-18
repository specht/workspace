require 'minitest/autorun'

# Formulas in Markdown are written \\( … \\), because Markdown eats one
# backslash. Raw HTML (a table row) is not run through Markdown, so there the
# formula is \( … \); a doubled backslash shows up on the page as \\(n^2\\).
class FormulaEscapingTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def test_no_doubled_backslash_in_html_table_cells
        found = Dir[File.join(CONTENT, '**', '*.md')].flat_map do |file|
            File.readlines(file, encoding: 'UTF-8').each_with_index
                .select { |line, _| line =~ /<t[dhr][\s>]/ && line =~ /\\\\[()\[\]]/ }
                .map { |line, i| "#{file.sub(CONTENT + '/', '')}:#{i + 1}: #{line.strip[0, 80]}" }
        end
        assert_empty found, "Formulas in HTML table cells take one backslash, \\( … \\):\n#{found.join("\n")}"
    end
end
