require 'minitest/autorun'
require 'yaml'

# The page's code in a »Jetzt bist du dran« box is the last step after its
# tips (hint_steps.js, docs/didaktik.md »Tipps in Stufen«): a solution
# without tips before it could be opened at once. And data-check on a box
# names the task that checks the student's own code.
class SolutionHintsTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def pages
        Dir[File.join(CONTENT, '*', '*.md')].select do |path|
            File.basename(path, '.md') == File.basename(File.dirname(path))
        end
    end

    # The lines of a page outside fenced code blocks, with their numbers.
    def prose_lines(path)
        fence = false
        File.readlines(path, encoding: 'utf-8').each_with_index.filter_map do |line, i|
            if line =~ /^\s*```/
                fence = !fence
                next
            end
            [line, i + 1] unless fence
        end
    end

    OPEN = /^\s*<!--\s*language:\s*([^>]*?)\s*-->\s*$/
    CLOSE = /^\s*<!--\s*\/language\s*-->\s*$/

    # The last line with text before a solution. Tips shared by several
    # languages (<!-- language: dart, java -->) may come before the
    # solution's own block and the blocks of the other languages, so
    # language markers and whole blocks without the solution's language are
    # passed over.
    def line_before(lines)
        own = nil
        depth = 0
        lines.reverse_each do |l|
            if l =~ CLOSE
                depth += 1
            elsif l =~ OPEN
                if depth.zero?
                    own ||= $1.split(/\s*,\s*/)
                else
                    depth -= 1
                end
            end
        end
        index = lines.size - 1
        while index >= 0
            l = lines[index]
            if l =~ CLOSE
                start = (index - 1).downto(0).find { |j| lines[j] =~ OPEN }
                if start && own && (lines[start][OPEN, 1].split(/\s*,\s*/) & own).empty?
                    index = start - 1
                    next
                end
            elsif l.strip != '' && l !~ OPEN
                return l
            end
            index -= 1
        end
        nil
    end

    def test_every_solution_has_tips_before_it
        missing = []
        pages.each do |path|
            lines = prose_lines(path)
            lines.each_with_index do |(line, number), i|
                next unless line =~ /^\s*<!--\s*solution\s*-->\s*$/

                before = line_before(lines[0...i].map(&:first))
                missing << "#{File.basename(path)}:#{number}" unless before.to_s =~ /^\s*<!--\s*\/hint\s*-->\s*$/
            end
        end
        assert_empty missing, 'a solution needs tips right before it (<!-- hint --> … <!-- /hint -->)'
    end

    def test_data_check_names_a_task_of_the_activity
        wrong = []
        pages.each do |path|
            tasks_path = File.join(File.dirname(path), 'tasks.yaml')
            ids = File.exist?(tasks_path) ? (YAML.load_file(tasks_path)['tasks'] || []).map { |t| t['id'] } : []
            prose_lines(path).each do |line, number|
                line.scan(/data-check='([^']*)'/).flatten.each do |id|
                    wrong << "#{File.basename(path)}:#{number} #{id}" unless ids.include?(id) && line.include?("class='hint own'")
                end
            end
        end
        assert_empty wrong, "data-check on a <div class='hint own'> names a task id in tasks.yaml"
    end
end
