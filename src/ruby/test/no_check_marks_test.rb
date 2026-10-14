require 'minitest/autorun'

# No Unicode check marks on the website: a check mark is an icon from
# Bootstrap Icons (<i class='bi bi-check-lg'>), which fits the other icons and
# every font. Code spans and blocks in the content are left alone; there a
# check mark is what a terminal really prints (the Git prompt in git.md).
class NoCheckMarksTest < Minitest::Test
    SRC = File.expand_path('../..', __dir__)
    MARKS = /[☑✅✓✔\u{1F5F8}]/
    THIRD_PARTY = %r{/(monaco|bootstrap-[\d.]+|node_modules|vendor)/|\.min\.(js|css)\z}

    def without_code(text)
        text.gsub(/^```.*?^```/m, '').gsub(/`[^`\n]*`/, '')
    end

    def test_no_check_marks_in_pages_and_content
        files = Dir[File.join(SRC, 'static', '**', '*.{js,html,css}')] +
                Dir[File.join(SRC, 'ruby', '{main.rb,include/*.rb}')] +
                Dir[File.join(SRC, 'content', '**', '*.{md,yaml,html}')]
        found = files.reject { |f| f =~ THIRD_PARTY }.flat_map do |file|
            text = File.read(file, encoding: 'UTF-8')
            text = without_code(text) if file =~ /\.(md|yaml)\z/
            text.lines.each_with_index.select { |line, _| line =~ MARKS }
                .map { |line, i| "#{file.sub(SRC + '/', '')}:#{i + 1}: #{line.strip[0, 80]}" }
        end
        assert_empty found, "Unicode check marks; use <i class='bi bi-check-lg'></i>:\n#{found.join("\n")}"
    end
end
