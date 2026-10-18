require 'set'
require_relative '../../include/teacher_roles'

module LegacyInvitations
    # The loader as it was in main.rb before the roster (load_invitations_locked
    # at 5eb3a71f), verbatim but for reading texts instead of files: the
    # reference that Roster must match exactly, so that the import changes
    # nothing for anyone.
    def self.load(texts, admins)
        invitations = {}
        user_groups = {}
        user_group_order = []
        explicit_teacher_emails = Set.new
        group_members = {}
        current_group = 'Administrator'
        admins.each do |email|
            user_group_order << current_group unless user_group_order.include?(current_group)
            invitations[email] = { :group => current_group, :name => email }
            user_groups[current_group] ||= []
            user_groups[current_group] << email
        end
        current_group = '(keine Gruppe)'
        group_admins = {}
        texts.each do |text|
            text.each_line do |line|
                next if line.strip.empty?
                next if line.strip[0] == '#'
                if line[0] == '>'
                    current_group = line[1, line.size - 1].strip
                    group_admins[current_group] ||= []
                    user_group_order << current_group unless user_group_order.include?(current_group)
                elsif line[0] == '+'
                    email = TeacherRoles.normalize_email(line[1..])
                    (group_admins[current_group] ||= []) << email if email
                elsif line[0] == '*'
                    email = TeacherRoles.normalize_email(line[1..])
                    explicit_teacher_emails << email if email
                else
                    parts = line.strip.split(' ')
                    email = parts.last.delete_prefix('<').delete_suffix('>').downcase
                    members = (group_members[current_group] ||= [])
                    members << email unless members.include?(email)
                    unless invitations[email]
                        user_groups[current_group] ||= []
                        user_groups[current_group] << email
                        invitations[email] = { :group => current_group }
                    end
                    if parts.size > 1
                        name = parts[0, parts.size - 1].join(' ')
                        invitations[email][:name] = name
                    else
                        invitations[email][:name] ||= email
                    end
                end
            end
        end
        teachers = {}
        group_admins.each_pair do |group, emails|
            emails.each do |email|
                teachers[email] ||= Set.new()
                teachers[email] << group
            end
        end
        teacher_emails = TeacherRoles.resolve(
            :invited_emails => invitations.keys,
            :administrators => admins,
            :group_administrators => teachers.keys,
            :designated_teachers => explicit_teacher_emails,
        )
        {
            :invitations => invitations, :user_groups => user_groups, :user_group_order => user_group_order,
            :group_members => group_members, :teachers => teachers, :teacher_emails => teacher_emails,
        }
    end
end
