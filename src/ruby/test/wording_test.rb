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

    def prose(text)
        text.gsub(/^[ \t]*```.*?^[ \t]*```/m, '').gsub(/`[^`\n]*`/, '').gsub(/<code\b.*?<\/code>/m, '')
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

    def test_multiplizieren_not_malnehmen
        assert_empty hits(/\bmal\s?(nehmen|genommen|nimmt|nimm)\b/i)
    end

    # Stack, Queue, Priority Queue, Hash Table stay English
    def test_english_data_structure_names
        assert_empty hits(/Warteschlange|Vorrangwarteschlange|Prioritätswarteschlange|Hashtabelle|Stapelspeicher/)
    end

    # the server opens links to other sites in a new tab (ExternalLinks)
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
