require 'minitest/autorun'
require_relative '../include/usage_stats'
require_relative '../include/workspace_usage'

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

    # a day with one to four students is :few, never its number; the year
    # goes by calendar weeks from a Monday
    def test_days_and_weeks_show_no_small_number
        days = summary(7)[:periods]
        assert_equal 7, days.size
        yesterday = days[-2]
        assert_equal 9, yesterday[:people]
        assert_equal 0, days.last[:people]
        few = UsageStats.summary(rows.first(3), :days => 7, :now => NOW)[:periods][-2]
        assert_equal :few, few[:people]
        assert_nil few[:checks]
        weeks = summary(365)[:periods]
        assert weeks.first[:from].monday?
        assert(weeks[0...-1].all? { |w| (w[:to] - w[:from]).to_i == 6 })
    end

    def test_the_rhythm_counts_students_per_weekday_and_hour
        rhythm = summary[:rhythm]
        t = Time.at(NOW - DAY)
        assert_equal 9, rhythm[(t.wday + 6) % 7][t.hour]
        assert_equal 1, rhythm.flatten.count { |n| n != 0 }
        lone = UsageStats.summary(rows.first(3), :days => 7, :now => NOW)[:rhythm]
        assert_equal [:few], lone.flatten.uniq - [0]
    end

    def test_the_page_draws_bars_and_names_no_small_number_in_them
        html = UsageStats.html(UsageStats.summary(rows.first(3) + rows.map { |r| r.merge(:ts => NOW - 3 * DAY) }, :days => 7, :now => NOW),
                               :titles => {}, :tasks => {}, :schools => {})
        assert_includes html, "usage-fill usage-few"
        assert_includes html, ': 9 Lernende, 24 Prüfungen'
        refute_match(/data-tip='[^']*: [1-4] Lernende/, html)
    end

    # classes on »Stolpersteine«: named from five who tried the task, with
    # their own share; a school only where none of its classes is named;
    # smaller classes only counted
    def test_a_stumbling_task_names_its_classes_from_five_on
        list = []
        6.times { |p| list << row(p, 'a', 'turtle-1/square', p < 3 ? 'wrong' : 'correct').merge(:groups => ['10a']) }
        3.times { |p| list << row(20 + p, 'a', 'turtle-1/square', 'wrong').merge(:groups => ['10b']) }
        5.times { |p| list << row(30 + p, 'b', 'turtle-1/square', 'correct').merge(:groups => ["k#{p}"]) }
        s = UsageStats.summary(list, :days => 30, :now => NOW, :group_school => { '10a' => 'a', '10b' => 'a' }.merge((0..4).to_h { |p| ["k#{p}", 'b'] }))
        task = s[:stumbling].first
        assert_equal [['10a', 'a', 6, 0.5], [nil, 'b', 5, 1.0]], task[:places].map { |p| [p[:group], p[:school], p[:people], p[:solved]] }
        assert_equal 6, task[:small_places]
        html = UsageStats.html(s, :titles => {}, :tasks => {}, :schools => { 'a' => 'Schule A', 'b' => 'Schule B' })
        assert_includes html, "10a, Schule A: 6 Lernende, 50 % gelöst"
        refute_includes html, '10b'
        assert_includes html, '6 Klassen mit weniger als 5'
    end

    # classes that worked recently: from five students on, the last day on
    # which five of them worked and on what; smaller classes only counted
    def test_classes_are_named_from_five_with_their_last_lesson
        list = []
        6.times { |p| list << row(p, 'a', 'turtle-1/square', 'correct', 5).merge(:groups => ['10a']) }
        5.times { |p| list << row(p, 'a', 'kata-loops/sum', 'correct', 2).merge(:groups => ['10a']) }
        list << row(0, 'a', 'jpeg/dct', 'wrong', 1).merge(:groups => ['10a'])
        3.times { |p| list << row(20 + p, 'a', 'turtle-1/square', 'wrong', 1).merge(:groups => ['10b']) }
        s = UsageStats.summary(list, :days => 30, :now => NOW, :group_school => { '10a' => 'a', '10b' => 'a' })
        assert_equal 1, s[:classes][:list].size
        c = s[:classes][:list].first
        assert_equal ['10a', 'a', 6], [c[:group], c[:school], c[:people]]
        # the day before yesterday five worked on the loops; yesterday only one
        assert_equal UsageStats.day_of(NOW - 2 * DAY), c[:last]
        assert_equal 'kata-loops', c[:activity]
        assert_equal 1, s[:classes][:small]
        html = UsageStats.html(s, :titles => { 'kata-loops' => ['Katas: Schleifen', 'kata-loops'] }, :tasks => {}, :schools => { 'a' => 'Schule A' })
        assert_includes html, '10a'
        assert_includes html, 'vor 2 Tagen'
        assert_includes html, 'Katas: Schleifen'
        refute_includes html, '10b'
        assert_includes html, '1 Klasse mit weniger als 5'
    end

    # Running Workspaces: who it was only within the hour, two numbers out
    def test_the_tally_counts_different_workspaces_and_the_most_at_once
        tally = WorkspaceUsage::Tally.new
        hour = NOW - NOW % 3600
        tally.add(%w[a b], hour + 60)
        tally.add(%w[b c d], hour + 360)
        assert_equal({ :hour => hour, :running => 4, :peak => 3 }, tally.add(%w[a], hour + 660))
        assert_equal({ :hour => hour + 3600, :running => 1, :peak => 1 }, tally.add(%w[e], hour + 3660))
    end

    def test_workspaces_per_day_are_the_busiest_hour_and_small_numbers_hide
        hour = NOW - NOW % 3600
        hours = [{ :hour => hour - DAY, :running => 7, :peak => 4 }, { :hour => hour - DAY + 3600, :running => 9, :peak => 6 },
                 { :hour => hour - 2 * DAY, :running => 3, :peak => 2 }]
        days = UsageStats.workspace_periods(hours, 7, NOW, 5)
        yesterday = days.find { |d| d[:from] == UsageStats.day_of(hour - DAY) }
        assert_equal 9, yesterday[:people]
        assert_equal 6, yesterday[:peak]
        assert_equal :few, days.find { |d| d[:from] == UsageStats.day_of(hour - 2 * DAY) }[:people]
        refute days.find { |d| d[:from] == UsageStats.day_of(hour - 3 * DAY) }[:counted]
    end

    def test_the_page_says_when_counting_began_or_that_it_has_not
        empty = UsageStats.html(UsageStats.summary([], :days => 7, :now => NOW), :titles => {}, :tasks => {}, :schools => {})
        assert_includes empty, 'Noch keine Zahlen'
        hour = NOW - NOW % 3600
        hours = (0..30).map { |i| { :hour => hour - i * 3600, :running => 6, :peak => 5 } }
        html = UsageStats.html(UsageStats.summary([], :days => 7, :now => NOW, :hours => hours), :titles => {}, :tasks => {}, :schools => {})
        assert_includes html, 'Laufende Workspaces'
        assert_includes html, 'höchstens 6 Workspaces in einer Stunde, 5 gleichzeitig'
        refute_includes html, 'Noch keine Zahlen'
    end

    def test_the_pins_of_each_class
        pins = [{ :group => '11-d', :school => 'hhg', :pages => [{ :title => 'Fair, barrierefrei, sicher', :href => '/fair-web', :ts => Time.new(2026, 11, 3).to_i }] }]
        html = UsageStats.pins_html(pins, ->(key) { key == 'hhg' ? 'Heinrich-Hertz-Gymnasium' : 'ohne Schule' })
        assert_includes html, 'Angepinnt'
        assert_includes html, "<a href='/fair-web'>Fair, barrierefrei, sicher</a>"
        assert_includes html, 'seit 3.11.'
        assert_includes html, 'Heinrich-Hertz-Gymnasium'
        assert_includes UsageStats.pins_html([], ->(_) { '' }), 'Keine Klasse hat etwas angepinnt.'
    end
end
