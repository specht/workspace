require 'minitest/autorun'
require_relative '../include/test_accounts'

# Every teacher exactly one test account in a test group of its own, and
# nobody sees another teacher's test group (docs/teacher-access.md).
class TestAccountsTest < Minitest::Test
    INVITATIONS = {
        'specht@schule.de' => { :name => 'Herr Specht' },
        'neumann@schule.de' => { :name => 'Frau Neumann' },
        'mueller@schule.de' => { :name => 'Herr Müller' },
        'mueller@andere-schule.de' => { :name => 'Herr Müller' },
        'lena@schule.de' => { :name => 'Lena' },
    }.freeze

    def test_one_test_account_for_every_teacher
        plan = TestAccounts.plan(%w[specht@schule.de neumann@schule.de], INVITATIONS)
        assert_equal %w[neumann+test@schule.de specht+test@schule.de], plan.keys.sort
        assert_equal({ :teacher => 'specht@schule.de', :group => 'Testgruppe (Herr Specht)', :name => 'Testkonto (Herr Specht)' },
            plan['specht+test@schule.de'])
    end

    def test_a_real_account_at_the_test_address_is_left_alone
        invitations = INVITATIONS.merge('specht+test@schule.de' => { :name => 'Ein echtes Konto' })
        assert_empty TestAccounts.plan(%w[specht@schule.de], invitations)
    end

    def test_two_teachers_of_the_same_name_get_two_groups
        plan = TestAccounts.plan(%w[mueller@schule.de mueller@andere-schule.de], INVITATIONS)
        assert_equal 2, plan.values.map { |a| a[:group] }.uniq.size
    end

    def test_each_teacher_sees_its_own_test_group_and_no_other
        groups = ['10a', '10b', 'WP 10', 'Testgruppe (Herr Specht)', 'Testgruppe (Frau Neumann)']
        tests = Set['Testgruppe (Herr Specht)', 'Testgruppe (Frau Neumann)']
        # An administrator: every real group, its own test group last
        assert_equal ['10a', '10b', 'WP 10', 'Testgruppe (Herr Specht)'],
            TestAccounts.visible_groups(groups, :admin => true, :own => Set[], :test_groups => tests,
                :own_test_group => 'Testgruppe (Herr Specht)')
        # A group administrator (+): its groups and its test group
        assert_equal ['10b', 'Testgruppe (Frau Neumann)'],
            TestAccounts.visible_groups(groups, :admin => false, :own => Set['10b', 'Testgruppe (Frau Neumann)'],
                :test_groups => tests, :own_test_group => 'Testgruppe (Frau Neumann)')
        # A teacher with * only: just its test group
        assert_equal ['Testgruppe (Frau Neumann)'],
            TestAccounts.visible_groups(groups, :admin => false, :own => Set['Testgruppe (Frau Neumann)'],
                :test_groups => tests, :own_test_group => 'Testgruppe (Frau Neumann)')
    end
end
