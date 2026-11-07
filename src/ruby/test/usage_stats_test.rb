require 'minitest/autorun'
require_relative '../include/usage_stats'

# /statistik without names (UsageStats): the numbers, and above all that
# nothing resting on fewer than five students shows.
class UsageStatsTest < Minitest::Test
    NOW = Time.utc(2026, 11, 7, 12).to_i
    DAY = 86_400

    def row(person, school, task, status, days_ago = 1)
        { :person => person, :school => school, :activity => task.split('/').first, :task => task, :status => status, :ts => NOW - days_ago * DAY }
    end

    # six students at school a solve turtle-1/square, the first ones after
    # wrong tries; two students at school b try it once and fail
    def rows
        list = []
        6.times do |p|
            p.times { list << row(p, 'a', 'turtle-1/square', 'wrong') }
            list << row(p, 'a', 'turtle-1/square', 'correct')
        end
        list << row(10, 'b', 'turtle-1/square', 'wrong') << row(11, 'b', 'turtle-1/square', 'wrong')
        list << row(12, 'b', 'kata-loops/sum', 'correct')
        list
    end

    def summary(days = 30) = UsageStats.summary(rows, :days => days, :now => NOW)

    def test_a_school_with_fewer_than_five_students_shows_nothing_but_that
        b = summary[:schools].find { |s| s[:school] == 'b' }
        assert_equal({ :school => 'b', :people => nil }, b)
        a = summary[:schools].find { |s| s[:school] == 'a' }
        assert_equal 6, a[:people]
        assert_equal 21, a[:checks]
    end

    def test_a_task_counts_who_tried_solved_and_how_many_checks_it_took
        task = summary[:activities].find { |x| x[:activity] == 'turtle-1' }[:tasks].first
        assert_equal 8, task[:people]
        assert_in_delta 6 / 8.0, task[:solved]
        assert_in_delta 17 / 23.0, task[:wrong]
        # checks up to the first correct one: 1, 2, 3, 4, 5, 6
        assert_in_delta 3.5, task[:median]
    end

    def test_an_activity_of_fewer_than_five_is_hidden_with_its_tasks
        loops = summary[:activities].find { |x| x[:activity] == 'kata-loops' }
        assert_equal({ :activity => 'kata-loops', :people => nil }, loops)
        assert(summary[:stumbling].none? { |t| t[:activity] == 'kata-loops' })
    end

    def test_the_window_leaves_out_older_attempts
        old = rows.map { |r| r.merge(:ts => NOW - 40 * DAY) }
        s = UsageStats.summary(old, :days => 30, :now => NOW)
        assert_nil s[:people]
        assert_empty s[:schools]
    end

    def test_the_page_names_no_small_number_and_no_activity_of_a_small_school
        html = UsageStats.html(summary, :titles => { 'turtle-1' => ['Turtle: Bilder aus Befehlen', 'turtle-1'], 'kata-loops' => ['Katas: Schleifen', 'kata-loops'] },
                                        :tasks => { 'turtle-1/square' => 'Ein Quadrat' }, :schools => { 'a' => 'Schule A', 'b' => 'Schule B' })
        assert_includes html, 'Schule B <span class=\'text-muted fw-normal\'>&lt; 5 Lernende</span>'
        assert_includes html, 'Ein Quadrat'
        # school a has only turtle-1 with five or more; nothing else is named there
        a = html[/Schule A.*?<\/table>/m]
        refute_includes a, 'Katas: Schleifen'
        refute_match(/>[1-4]<\/td>/, html.gsub(/<td class='text-end'>\d+ %<\/td>/, ''))
    end
end
