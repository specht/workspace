require 'minitest/autorun'
require_relative '../include/task_examples'

# Every check accepts its examples and rejects its counterexamples
# (task-examples.yaml next to each tasks.yaml, docs/aufgaben.md). Choice and
# number tasks are checked against their own correct answer. ruby and python
# checks run with the local interpreters.
class TaskExamplesTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def activity_dirs
        Dir[File.join(CONTENT, '**', 'tasks.yaml')].map { |f| File.dirname(f) }.sort
    end

    def test_every_example_gets_the_expected_verdict
        failures = []
        activity_dirs.each do |dir|
            outcomes, problems = TaskExamples.run(dir)
            failures.concat(problems.map { |p| "#{File.basename(dir)}: #{p}" })
            failures.concat(outcomes.reject(&:ok).map { |o| "#{o.label}: #{o.detail}" })
        end
        assert_empty failures, "\n" + failures.join("\n")
    end

    # Activities whose file and program checks all have examples. A new
    # task with such a check in one of them needs an example too.
    COMPLETE = %w[automata bobby_tables huffman hugo latex lzw pixelwand raylib rle webserver wetter].freeze

    def test_complete_activities_stay_complete
        missing = COMPLETE.flat_map { |slug| TaskExamples.missing(File.join(CONTENT, slug)) }
        assert_empty missing, "These checks have no example that must pass:\n" + missing.join("\n")
    end
end
