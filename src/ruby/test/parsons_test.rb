require 'minitest/autorun'
require_relative '../include/parsons'
require_relative '../include/task_card'

# Parsons puzzles: the card shows the lines sorted and without indentation,
# never in the order of the solution; the solution is the lines joined.
class ParsonsTest < Minitest::Test
    LINES = ['def sum_even(n)', '  total = 0', '  (1..n).each do |i|', '    total += i if i.even?', '  end', '  total', 'end'].freeze

    def task(extra = {})
        { 'id' => 'p', 'title' => 'P', 'prompt' => 'Setz zusammen.', 'parsons' => LINES,
          'check' => { 'type' => 'function', 'language' => 'ruby', 'cases' => [{ 'call' => 'sum_even(2)', 'expect' => '2' }] } }.merge(extra)
    end

    def test_code_shown_and_indent
        assert_equal LINES.join("\n") + "\n", Parsons.code(LINES)
        assert_equal LINES.map(&:strip).sort, Parsons.shown(LINES)
        assert_equal 2, Parsons.indent(LINES)
        assert_equal 4, Parsons.indent(['a', 'b'])
        assert_includes Parsons.shown(LINES, ['  total = 1']), 'total = 1'
    end

    def test_errors
        assert_empty Parsons.errors(task)
        assert_empty Parsons.errors(task('parsons_unused' => ['  total = 1']))
        assert Parsons.errors(task('check' => { 'type' => 'answer' })).any? { |e| e.include?('only works with function') }
        assert Parsons.errors(task('parsons' => %w[a b])).any? { |e| e.include?('3 to') }
        assert Parsons.errors(task('parsons' => ['a', '  b', '   c'])).any? { |e| e.include?('steps of 2') }
        assert Parsons.errors(task('parsons_unused' => ['  total = 0'])).any? { |e| e.include?('also a line of the solution') }
        assert Parsons.errors(task('starter' => 'def x')).any? { |e| e.include?('no starter') }
    end

    # the page never holds the solution: lines sorted, no indentation
    def test_card_hides_the_order
        html = TaskCard.html(task('parsons_unused' => ['  total = 1']), activity: 'kata-loops', number: 1, render: ->(md) { md })
        shown = html.scan(/data-text='([^']*)'/).flatten.map { |t| CGI.unescapeHTML(t) }
        assert_equal Parsons.shown(LINES, ['  total = 1']), shown
        refute_equal LINES.map(&:strip), shown
        assert_includes html, "data-indent='2'"
        assert_includes html, "data-move='skip'"
        assert_includes html, "class='form-control task-input task-code parsons-answer'"
        refute_includes html, '  total = 0'
    end
end
