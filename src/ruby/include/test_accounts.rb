require 'set'
require_relative 'look_in'

# Every teacher has exactly one test account, adresse+test@domain, alone in
# a test group of its own (docs/teacher-access.md, »Testkonto«): to try
# tutorials, hand-ins, galleries and exams as a student would, and to reset
# it without touching anyone else. A teacher sees its own test group and no
# other: an administrator sees every real group, but not the test groups of
# the other teachers. Plain Ruby, tested on its own.
module TestAccounts
    # teachers: the addresses of all teachers (administrators, + and *)
    # invitations: { email => { :name } } of the invitation files
    # Returns { test_email => { :teacher, :group, :name } }. A teacher whose
    # test address is invited as a real account gets none.
    def self.plan(teachers, invitations)
        names = {}
        teachers.to_a.sort.each_with_object({}) do |teacher, out|
            test_email = LookIn.test_account_email(teacher)
            next if test_email.nil? || invitations.key?(test_email)

            name = (invitations[teacher] || {})[:name].to_s
            # Invited with the address alone: its part before the @
            name = teacher.split('@').first if name.empty? || name.include?('@')
            group = "Testgruppe (#{name})"
            # Two teachers of the same name: the address keeps the groups apart
            group = "Testgruppe (#{name}, #{teacher})" if names.key?(group)
            names[group] = true
            out[test_email] = { :teacher => teacher, :group => group, :name => "Testkonto (#{name})" }
        end
    end

    # The groups a teacher may see on the class page and in lists of groups,
    # in the order of the invitation files with its test group last.
    # groups: all groups in order; own: the groups it administers (+);
    # test_groups: all test groups; own_test_group: its own, or nil.
    def self.visible_groups(groups, admin:, own:, test_groups:, own_test_group:)
        real = groups.reject { |g| test_groups.include?(g) }
        visible = admin ? real : real.select { |g| own.include?(g) }
        visible += [own_test_group] if own_test_group && groups.include?(own_test_group)
        visible
    end
end
