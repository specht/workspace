require 'minitest/autorun'
require 'set'
require_relative '../include/group_visibility'

class GroupVisibilityTest < Minitest::Test
    # Lena is in Klasse 12 and in the LK, Tom only in Klasse 11.
    MEMBERS = {
        'Klasse 11' => %w[tom@example.org],
        'Klasse 12' => %w[lena@example.org],
        'LK Informatik' => %w[lena@example.org mia@example.org],
    }.freeze
    ORDER = ['Klasse 11', 'Klasse 12', 'LK Informatik'].freeze

    def lines
        [
            { :email => 'lena@example.org', :code => '111111', :expires_at => 1 },
            { :email => 'tom@example.org', :code => '222222', :expires_at => 1 },
            { :email => 'mia@example.org', :code => '333333', :expires_at => 1 },
        ]
    end

    def test_a_student_is_visible_in_every_group_not_only_the_first
        assert_equal ['Klasse 12', 'LK Informatik'], GroupVisibility.groups_of('lena@example.org', MEMBERS)
        assert GroupVisibility.visible?('lena@example.org', :group_members => MEMBERS, :viewer_groups => Set['LK Informatik'])
        assert GroupVisibility.visible?('lena@example.org', :group_members => MEMBERS, :viewer_groups => Set['Klasse 12'])
        refute GroupVisibility.visible?('tom@example.org', :group_members => MEMBERS, :viewer_groups => Set['LK Informatik'])
        assert GroupVisibility.visible?('tom@example.org', :group_members => MEMBERS, :viewer_groups => Set[], :admin => true)
    end

    # The LK teacher sees Lena's and Mia's codes, for the LK only, and has
    # nothing to choose from.
    def test_login_codes_of_a_teacher_with_one_group
        shown = GroupVisibility.login_codes_for(lines, :group_members => MEMBERS, :group_order => ORDER,
            :viewer_groups => Set['LK Informatik'])
        assert_equal %w[111111 333333], shown[:lines].map { |l| l[:code] }
        assert_equal [['LK Informatik'], ['LK Informatik']], shown[:lines].map { |l| l[:groups] }
        assert_equal ['LK Informatik'], shown[:groups]
    end

    # A teacher of Klasse 12 and the LK chooses between them; Lena's code
    # belongs to both, in file order.
    def test_login_codes_of_a_teacher_with_several_groups
        shown = GroupVisibility.login_codes_for(lines, :group_members => MEMBERS, :group_order => ORDER,
            :viewer_groups => Set['LK Informatik', 'Klasse 12'])
        assert_equal ['Klasse 12', 'LK Informatik'], shown[:groups]
        lena = shown[:lines].find { |l| l[:email] == 'lena@example.org' }
        assert_equal ['Klasse 12', 'LK Informatik'], lena[:groups]
        refute(shown[:lines].any? { |l| l[:email] == 'tom@example.org' })
    end

    # An administrator sees every code and chooses among the groups that
    # have one.
    def test_login_codes_of_an_administrator
        lonely = lines + [{ :email => 'gast@example.org', :code => '444444', :expires_at => 1 }]
        shown = GroupVisibility.login_codes_for(lonely, :group_members => MEMBERS, :group_order => ORDER,
            :viewer_groups => Set[], :admin => true)
        assert_equal 4, shown[:lines].size
        assert_equal [], shown[:lines].last[:groups]
        assert_equal ORDER, shown[:groups]
    end
end
