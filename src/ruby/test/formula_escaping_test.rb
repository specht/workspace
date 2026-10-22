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

    # An underscore in a Markdown formula is \_ (a subscript, q\_0) or, inside
    # \text{…}, \\\_. A doubled backslash right before it leaves the underscore
    # bare, and Markdown turns the rest of the paragraph into emphasis.
    def test_no_bare_underscore_after_doubled_backslash_in_formulas
        found = Dir[File.join(CONTENT, '**', '*.md')].flat_map do |file|
            File.readlines(file, encoding: 'UTF-8').each_with_index
                .select { |line, _| line.scan(/\\\(.*?\\\)/).any? { |f| f =~ /(?<!\\)\\\\_/ } }
                .map { |line, i| "#{file.sub(CONTENT + '/', '')}:#{i + 1}: #{line.strip[0, 80]}" }
        end
        assert_empty found, "Inside \\( … \\) an underscore is \\_ or, in \\text{…}, \\\\\\_:\n#{found.join("\n")}"
    end
end
