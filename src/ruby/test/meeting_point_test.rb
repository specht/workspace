require 'minitest/autorun'
require_relative '../include/meeting_point'

class MeetingPointTest < Minitest::Test
    TASKS = [
        { :id => 'a', :title => 'Erste' },
        { :id => 'bonus', :title => 'Für Schnelle', :extra => true },
        { :id => 'b', :title => 'Zweite' },
        { :id => 'c', :title => 'Dritte' },
    ].freeze

    def test_extras_do_not_count
        assert_equal %w[a b], MeetingPoint.needed(TASKS, 'b')
        assert MeetingPoint.arrived?(TASKS, 'b', { 'a' => { :status => 'correct' }, 'b' => { :status => 'sent' } })
        refute MeetingPoint.arrived?(TASKS, 'b', { 'a' => { :status => 'correct' }, 'b' => { :status => 'wrong' } })
        refute MeetingPoint.arrived?(TASKS, 'c', { 'a' => { :status => 'correct' }, 'b' => { :status => 'correct' } })
    end

    def test_a_point_after_a_task_not_on_the_page_is_nobodys
        assert_nil MeetingPoint.needed(TASKS, 'gone')
        refute MeetingPoint.arrived?(TASKS, 'gone', {})
    end

    def test_two_groups_the_earlier_point_counts
        assert_equal ['10b', 'b'], MeetingPoint.earliest({ '10a' => 'c', '10b' => 'b' }, TASKS)
        assert_equal ['10a', 'c'], MeetingPoint.earliest({ '10a' => 'c', '10b' => 'gone' }, TASKS)
        assert_nil MeetingPoint.earliest({}, TASKS)
    end

    def test_the_board_counts_who_has_arrived
        members = [{ :email => 'z@x', :name => 'Zoe' }, { :email => 'a@x', :name => 'Ali' }, { :email => 'm@x', :name => 'Mia' }]
        states = {
            'z@x' => { 'a' => { :status => 'correct' }, 'b' => { :status => 'correct' } },
            'a@x' => { 'a' => { :status => 'correct' } },
        }
        status = MeetingPoint.status(TASKS, 'b', members, states)
        assert_equal 2, status[:index]
        assert_equal ['Zoe'], status[:arrived]
        assert_equal %w[Ali Mia], status[:waiting]
        html = MeetingPoint.status_html(status)
        assert_includes html, 'Treffpunkt nach Aufgabe 3 (Zweite)'
        assert_includes html, '1 von 3 da'
        assert_includes html, 'Noch nicht da: 2'
    end
end
