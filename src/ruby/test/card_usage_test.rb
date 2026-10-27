require 'minitest/autorun'
require_relative '../include/card_usage'

class CardUsageTest < Minitest::Test
    TEACHERS = { 't@x' => ['9a', '9b', 'Test t'], 'u@x' => ['10c'] }.freeze
    MEMBERS = {
        '9a' => ['a@x', 'b@x', 't@x'],
        '9b' => ['b@x', 'c@x', 'u@x'],
        '10c' => ['d@x'],
        'Test t' => ['t+test@x'],
    }.freeze

    # A teacher counts the members of their own groups: each once, never
    # themselves, other teachers, admins, test accounts or test groups.
    def test_students_of
        students = CardUsage.students_of('t@x', :teachers => TEACHERS, :group_members => MEMBERS,
                                         :skip => ['Test t'], :excluded => ['u@x', 't+test@x'])
        assert_equal Set['a@x', 'b@x', 'c@x'], students
        assert_equal Set['d@x'], CardUsage.students_of('u@x', :teachers => TEACHERS, :group_members => MEMBERS)
        assert_empty CardUsage.students_of('nobody@x', :teachers => TEACHERS, :group_members => MEMBERS)
    end

    def test_level
        assert_equal 0, CardUsage.level(0, 10)
        assert_equal 1, CardUsage.level(1, 10)
        assert_equal 3, CardUsage.level(5, 10)
        assert_equal 5, CardUsage.level(10, 10)
        assert_equal 5, CardUsage.level(12, 10), 'never beyond the darkest'
        assert_equal 5, CardUsage.level(3, 0), 'no top: as if one'
    end

    # A teacher's band: steps by the share of their students, titles say
    # how many of how many, the summary too.
    def test_teacher_band
        html = CardUsage.band_html(['Die erste Seite', 'Die Adresse deiner Seite'], [17, 6], :solved => 23, :people => 17, :of => 17, :css_class => 'card-usage')
        assert_includes html, "class='usage-bar usage-level-5' title='1. Die erste Seite: 17 von 17 deiner Lernenden gelöst'"
        assert_includes html, "class='usage-bar usage-level-2' title='2. Die Adresse deiner Seite: 6 von 17 deiner Lernenden gelöst'"
        assert_includes html, "aria-label='Deine Lernenden: 17 von 17 haben hier etwas gelöst, zusammen 23 Aufgaben'"
        assert_includes html, '17/17'
    end

    # Admins: steps next to the most solved task, as before
    def test_admin_band
        html = CardUsage.band_html(['A', 'B &amp; C', 'D'], [8, 4, 0], :solved => 12, :people => 9, :css_class => 'card-usage')
        assert_includes html, "usage-level-5' title='1. A: 8-mal gelöst'"
        assert_includes html, "usage-level-3' title='2. B &amp;amp; C: 4-mal gelöst'"
        assert_includes html, "usage-level-0' title='3. D: 0-mal gelöst'"
        assert_includes html, 'ohne Lehrkräfte, Admins und Testkonten'
    end

    # A student's own: a segment per core task in the pinned card's colours
    def test_own_band
        html = CardUsage.own_html(%w[Eins Zwei Drei], [:done, :wrong, :open], :css_class => 'card-usage')
        assert_includes html, "class='shc-mark shc-done' title='1. Eins: gelöst'"
        assert_includes html, "class='shc-mark shc-wrong' title='2. Zwei: noch nicht gelöst'"
        assert_includes html, "class='shc-mark shc-open' title='3. Drei: noch offen'"
        assert_includes html, '1/3'
        assert_includes html, "aria-label='Du hast 1 von 3 Aufgaben gelöst'"
    end
end
