require 'minitest/autorun'
require 'yaml'

# Every »Jetzt bist du dran« box (docs/checkliste.md) names with data-check a
# task of its own page that checks the student's code, so that nobody is
# left without knowing whether it is right.
class OwnBoxChecksTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def test_every_box_names_a_task_of_its_page
        missing = []
        Dir.glob(File.join(CONTENT, '*', '*.md')).each do |page|
            dir = File.dirname(page)
            next unless File.basename(page, '.md') == File.basename(dir)

            text = File.read(page)
            tasks_file = File.join(dir, 'tasks.yaml')
            ids = File.file?(tasks_file) ? ((YAML.load_file(tasks_file) || {})['tasks'] || []).map { |t| t['id'] } : []
            text.to_enum(:scan, /<div class=['"]hint own\b[^>]*>/).each do
                tag = Regexp.last_match[0]
                line = text[0, Regexp.last_match.begin(0)].count("\n") + 1
                id = tag[/data-check=['"]([\w-]+)['"]/, 1]
                where = "#{page.sub("#{CONTENT}/", '')}:#{line}"
                if id.nil?
                    missing << "#{where}: no data-check"
                elsif !ids.include?(id)
                    missing << "#{where}: data-check='#{id}' is no task of the page"
                end
            end
        end
        assert_empty missing
    end
end
