require 'set'

# Teaching-material access is independent from group-administrator privileges.
# The '+' markers retain their existing administrative meaning; '*' only marks
# an invited account as a teacher, with no additional permissions.
module TeacherRoles
    def self.normalize_email(raw)
        email = raw.to_s.strip.delete_prefix('<').delete_suffix('>').downcase
        return nil unless email.match?(/\A[^\s<>@]+@[^\s<>@]+\z/)
        email
    end

    def self.resolve(invited_emails:, administrators:, group_administrators:, designated_teachers:)
        invited = Set.new(invited_emails.map { |email| normalize_email(email) }.compact)
        candidates = administrators.to_a + group_administrators.to_a + designated_teachers.to_a
        Set.new(candidates.map { |email| normalize_email(email) }.compact) & invited
    end
end
