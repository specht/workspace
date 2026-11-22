require 'minitest/autorun'

# A page in several languages (languages: in the meta block) shows the
# chosen one: prose outside <!-- language: … --> blocks does not name
# another language's variant in brackets (»`read_ways` (in Dart `readWays`)«,
# »(in Python `None`)«) or ask the reader to swap a word (»Statt `ruby` …«).
# A name inside a sentence goes into inline variants instead:
# <span class='code-language' data-language='ruby python'><code>read_ways</code></span>
# <span class='code-language' data-language='dart javascript java'><code>readWays</code></span>
# (docs/checkliste.md, the maintainer, November 2026).
class LanguageProseTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)
    LANGUAGE = 'Ruby|Python|Dart|JavaScript|Java'
    PATTERNS = [
        /\((?:in|bei) (?:#{LANGUAGE})(?:,| und| oder|\b)/,
        /Statt `(?:ruby|python|dart|javascript|java)`/,
    ].freeze

    def prose_lines(file)
        in_block = in_code = in_comment = in_meta = false
        File.readlines(file, encoding: 'UTF-8').each_with_index.filter_map do |line, i|
            stripped = line.strip
            if stripped.start_with?('```')
                in_code = !in_code
                next
            end
            next if in_code
            in_meta = true if stripped == "<div class='meta'>"
            if in_meta
                in_meta = false if stripped == '</div>'
                next
            end
            in_block = true if stripped.start_with?('<!-- language:')
            if in_block
                in_block = false if stripped == '<!-- /language -->'
                next
            end
            in_comment = true if stripped.start_with?('<!-- tutorial-')
            if in_comment
                in_comment = false if stripped.end_with?('-->')
                next
            end
            [line, i + 1]
        end
    end

    def test_prose_follows_the_chosen_language
        found = Dir[File.join(CONTENT, '*', '*.md')].flat_map do |file|
            next [] unless File.read(file, encoding: 'UTF-8') =~ /^languages: \[[^\]]*,/
            prose_lines(file).select { |line, _| PATTERNS.any? { |p| line =~ p } }
                             .map { |line, n| "#{file.sub(CONTENT + '/', '')}:#{n}: #{line.strip[0, 90]}" }
        end
        assert_empty found, "Use inline variants (<span class='code-language' data-language='…'>) instead:\n#{found.join("\n")}"
    end
end
