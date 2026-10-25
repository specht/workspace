require 'minitest/autorun'
require 'yaml'
require_relative '../include/after_task'

# <!-- after: id --> … <!-- /after -->: what gives away a task's answer waits
# for it (AfterTask, docs/didaktik.md »Vermuten vor dem Ausprobieren«).
class AfterTaskTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def pages
        Dir[File.join(CONTENT, '*', '*.md')].select do |path|
            File.basename(path, '.md') == File.basename(File.dirname(path))
        end
    end

    def test_markers_name_a_task_above_them_and_are_closed
        wrong = []
        pages.each do |path|
            markdown = File.read(path, encoding: 'utf-8')
            tasks_path = File.join(File.dirname(path), 'tasks.yaml')
            ids = File.exist?(tasks_path) ? (YAML.load_file(tasks_path)['tasks'] || []).map { |t| t['id'] } : []
            prose = AfterTask.prose(markdown).join("\n")
            open = nil
            prose.each_line do |line|
                if (m = line.match(AfterTask::START))
                    id = m[1]
                    wrong << "#{File.basename(path)}: after #{id} inside after #{open}" if open
                    wrong << "#{File.basename(path)}: after #{id} is no task in tasks.yaml" unless ids.include?(id)
                    above = prose[0...prose.index(line)]
                    wrong << "#{File.basename(path)}: after #{id} comes before _task(#{id})" unless above.include?("_task(#{id})")
                    open = id
                elsif line.match?(AfterTask::FINISH)
                    wrong << "#{File.basename(path)}: /after without after" unless open
                    open = nil
                end
            end
            wrong << "#{File.basename(path)}: after #{open} not closed" if open
        end
        assert_empty wrong
    end

    def test_mark_leaves_fenced_code_alone
        markdown = "```markdown\n<!-- after: x -->\n```\n\n<!-- after: x -->\nText\n<!-- /after -->\n"
        marked = AfterTask.mark(markdown)
        assert_includes marked, "```markdown\n<!-- after: x -->\n```"
        assert_includes marked, "<div class='after-task-start' data-after='x'></div>"
        assert_includes marked, "<div class='after-task-end'></div>"
        assert_equal ['x'], AfterTask.ids(markdown)
    end

    def test_wrap
        begin
            require 'nokogiri'
        rescue LoadError
            skip 'nokogiri only in the Ruby container'
        end
        root = Nokogiri::HTML::DocumentFragment.parse(
            "<p>Vorher</p><div class='after-task-start' data-after='x'></div><p>Bild</p><p>Text</p>" \
            "<div class='after-task-end'></div><p>Nachher</p>")
        AfterTask.wrap(root)
        box = root.at_css('div.after-task')
        assert_equal 'x', box['data-after']
        assert_equal AfterTask::WAIT, box.at_css('p.after-task-wait').text
        assert_equal %w[Bild Text], box.css('div.after-task-body > p').map(&:text)
        assert_equal 'Nachher', box.next_element.text
    end
end
