require 'minitest/autorun'
require 'set'
require_relative '../include/exam_people_classes'

class ExamPeopleClassesTest < Minitest::Test
    GROUPS = {
        'LK Informatik' => %w[a b c d e],
        'Q3 Tutorium 1' => %w[a b x y],
        'Q3 Tutorium 2' => %w[c d z],
        # a course two of them share besides their class
        'GK Musik' => %w[a b c],
        'Testgruppe' => %w[a],
    }.freeze

    def test_members_by_their_class
        # GK Musik holds three of them, more than either Tutorium, but it
        # overlaps both: the two Tutorien hold four together
        split = ExamPeopleClasses.split('LK Informatik', GROUPS['LK Informatik'], GROUPS, skip: ['Testgruppe'])
        assert_equal({ 'a' => 'Q3 Tutorium 1', 'b' => 'Q3 Tutorium 1', 'c' => 'Q3 Tutorium 2', 'd' => 'Q3 Tutorium 2', 'e' => nil }, split)
    end

    def test_a_group_holding_everyone_is_no_class
        groups = GROUPS.merge('Q3' => %w[a b c d e x y z])
        split = ExamPeopleClasses.split('LK Informatik', GROUPS['LK Informatik'], groups, skip: ['Testgruppe'])
        assert_equal 'Q3 Tutorium 1', split['a']
    end

    def test_class_beats_a_small_course
        groups = GROUPS.merge('GK Musik' => %w[a])
        split = ExamPeopleClasses.split('LK Informatik', GROUPS['LK Informatik'], groups, skip: ['Testgruppe'])
        assert_equal({ 'a' => 'Q3 Tutorium 1', 'b' => 'Q3 Tutorium 1', 'c' => 'Q3 Tutorium 2', 'd' => 'Q3 Tutorium 2', 'e' => nil }, split)
    end

    def test_a_class_of_one_is_not_named
        groups = GROUPS.merge('Q3 Tutorium 3' => %w[e w])
        split = ExamPeopleClasses.split('LK Informatik', GROUPS['LK Informatik'], groups, skip: ['Testgruppe'])
        assert_nil split['e']
    end

    def test_nothing_when_all_from_one_class
        groups = { 'LK' => %w[a b], '10a' => %w[a b x] }
        assert_equal({}, ExamPeopleClasses.split('LK', %w[a b], groups))
    end

    def test_a_class_itself_is_not_split
        groups = { '10a' => %w[a b c], 'Informatik 10' => %w[a b c d] }
        assert_equal({}, ExamPeopleClasses.split('10a', %w[a b c], groups))
    end
end
