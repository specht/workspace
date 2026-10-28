require 'minitest/autorun'
require 'yaml'

# Wording rules of docs/checkliste.md that a search can check, in the prose
# of every page (activities, their tasks and teacher notes, the Handbuch);
# code blocks and inline code are left out.
class WordingTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)
    STATIC = File.expand_path('../../static', __dir__)

    def files
        Dir.glob(File.join(CONTENT, '**', '*.{md,yaml}')).reject { |path| path.include?('/_drafts/') || File.basename(path) == 'task-examples.yaml' }
    end

    # the text without code, each code block kept as its empty lines, so
    # that a line number still points into the file
    def prose(text)
        text.gsub(/^[ \t]*```.*?^[ \t]*```/m) { |block| "\n" * block.count("\n") }
            .gsub(/`[^`\n]*`/, '').gsub(/<code\b.*?<\/code>/m) { |code| "\n" * code.count("\n") }
    end

    # [file:line, line] for every line of prose matching pattern
    def hits(pattern, paths = files)
        paths.flat_map do |path|
            prose(File.read(path)).lines.each_with_index.filter_map do |line, i|
                "#{path.sub("#{CONTENT}/", '')}:#{i + 1}: #{line.strip[0, 120]}" if line.match?(pattern)
            end
        end
    end

    # it makes a class laugh; what is meant has a name
    def test_no_ding
        assert_empty hits(/\bDing(e|en|es|s)?\b/)
    end

    # a sentence that ends on a bare »eine« leaves out what it is about
    # (»Dein Modell ist eine.«); the noun is said again. Across line breaks
    # too, so the whole prose is searched, not line by line. An adjective
    # without its noun (»Du baust heute ein winziges.«) is the same mistake,
    # but a search cannot tell it from a verb (»einen vorher.«): read for it.
    def test_no_sentence_ending_on_a_bare_article
        pattern = /\b(ist|sind|bleibt|bleiben|wird|werden|war|waren)\s+(k?eine|k?einer|k?eines|k?eins)\s*[.!?]/
        found = files.flat_map do |path|
            text = prose(File.read(path))
            text.to_enum(:scan, pattern).map do
                match = Regexp.last_match
                line = text[0, match.begin(0)].count("\n") + 1
                "#{path.sub("#{CONTENT}/", '')}:#{line}: #{match[0].gsub(/\s+/, ' ')}"
            end
        end
        assert_empty found
    end

    # »Rosette« reads as crude slang in a class (the maintainer); a figure of
    # shapes turned round a point is a »Blume«. The engraver E. Rosette in a
    # picture's credit line is a name.
    def test_no_rosette
        assert_empty hits(/Rosette/).reject { |hit| hit.include?('person-credit') }
    end

    def test_multiplizieren_not_malnehmen
        assert_empty hits(/\bmal\s?(nehmen|genommen|nimmt|nimm)\b/i)
    end

    # » · « between words reads as if an AI wrote it; a comma, »und«, a
    # colon or parentheses instead. The dot stays a multiplication sign:
    # between numbers (»3 · 4«), in LaTeX, and between variables in lower
    # case or single letters (»dir · n«, »plane · camera_x«, »S · V«).
    # A word on one side and a number on the other (»Bytes · 4 Takte«)
    # slips through: read for it.
    def test_no_middle_dot_as_separator
        found = files.flat_map do |path|
            text = prose(File.read(path)).gsub(/\$\$.*?\$\$|\$[^$\n]+\$|\\\(.*?\\\)|\\\[.*?\\\]/m) { |maths| "\n" * maths.count("\n") }
            text.lines.each_with_index.flat_map do |line, i|
                line.gsub(/<[^>\n]*>/, '').scan(/(\S+) ?· ?(\S+)/).filter_map do |left, right|
                    words = [left[/([\p{L}_]+)[)»«"'.,:;!?]*\z/, 1], right[/\A[»„"']?(\p{L}[\p{L}_]*)/, 1]]
                    next if words.any?(&:nil?) || words.all? { |word| word.match?(/\A([a-z_]+|\p{L})\z/) }
                    "#{path.sub("#{CONTENT}/", '')}:#{i + 1}: #{left} · #{right}"
                end
            end
        end
        assert_empty found
    end

    # Stack, Queue, Priority Queue, Hash Table stay English
    def test_english_data_structure_names
        assert_empty hits(/Warteschlange|Vorrangwarteschlange|Prioritätswarteschlange|Hashtabelle|Stapelspeicher/)
    end

    # the server opens links to other sites in a new tab (ExternalLinks)
    # The paragraph right after <div class='hint …'> is HTML: Markdown in it
    # stays as it is, and backticks show up on the page. Code there is <code>.
    def test_no_backticks_in_html_boxes
        found = Dir.glob(File.join(CONTENT, '**', '*.md')).flat_map do |path|
            lines = File.readlines(path)
            lines.each_index.select { |i| lines[i] =~ /\A<div class='hint[^']*'[^>]*>\s*\z/ }.filter_map do |i|
                para = lines[(i + 1)..].take_while { |l| !l.strip.empty? && !l.start_with?('<!--', '</div>') }
                "#{path.sub("#{CONTENT}/", '')}:#{i + 2}" if para.any? { |l| l.include?('`') }
            end
        end
        assert_empty found
    end

    def test_no_target_on_external_links
        assert_empty hits(/<a\b[^>]*href=['"]https?:\/\/[^>]*\btarget=|<a\b[^>]*\btarget=[^>]*href=['"]https?:\/\//)
    end

    # The activities of the section `hidden` are found, not advertised: no
    # page outside them names their addresses, and no public file of the
    # site names them either.
    def test_the_hidden_section_stays_hidden
        sections = YAML.load_file(File.join(CONTENT, 'sections.yaml'))
        hidden = sections.find { |s| s['key'] == 'hidden' }
        skip 'no hidden section' unless hidden
        slugs = hidden['entries'].map { |e| e.to_s.delete_prefix('.') }
        outside = files.reject { |path| slugs.any? { |slug| path.include?("/#{slug}/") } || File.basename(path) == 'sections.yaml' }
        # the 404 page is the way in (noclip): it names the first address
        public = Dir.glob(File.join(STATIC, '**', '*.{js,css,html}')).reject do |path|
            path.include?('/include/bootstrap') || path.include?('/tic80/') || File.basename(path) == '_not_found.html'
        end
        slugs.each do |slug|
            found = (outside + public).select { |path| File.read(path).include?(slug) }
            assert_empty found, "#{slug} is named outside its section"
        end
        # the series by the name in its first page's title, without the article
        series = File.read(File.join(CONTENT, slugs.first, "#{slugs.first}.md"))[/^# (.+)$/, 1].to_s.split(/[:·]/).first.to_s.strip
        series = series.sub(/\A(Die|Der|Das)\s+/, '')
        unless series.empty?
            named = (outside + public).select { |path| File.read(path).include?(series) }
            assert_empty named, "the series »#{series}« is named outside its section"
        end
    end
end
