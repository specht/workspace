require 'minitest/autorun'
require_relative '../include/teacher_roles'

class TeacherRolesTest < Minitest::Test
    def test_normalizes_markers_with_angle_brackets_and_case
        assert_equal 'teacher@example.org', TeacherRoles.normalize_email(' <Teacher@Example.Org> ')
        assert_nil TeacherRoles.normalize_email('not an email')
    end

    def test_explicit_teacher_has_no_implicit_group_administration
        group_admins = { 'admin@example.org' => Set['Klasse 9'] }
        eligible = TeacherRoles.resolve(
            :invited_emails => %w[teacher@example.org admin@example.org student@example.org],
            :administrators => [],
            :group_administrators => group_admins.keys,
            :designated_teachers => Set['teacher@example.org'],
        )
        assert_includes eligible, 'teacher@example.org'
        assert_includes eligible, 'admin@example.org'
        refute_includes eligible, 'student@example.org'
        refute group_admins.key?('teacher@example.org')
    end

    def test_uninvited_marker_does_not_grant_access
        eligible = TeacherRoles.resolve(
            :invited_emails => ['student@example.org'],
            :administrators => ['admin@example.org'],
            :group_administrators => [],
            :designated_teachers => ['not-invited@example.org'],
        )
        assert_empty eligible
    end
end
