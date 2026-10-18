require 'set'
require_relative 'teacher_roles'

# Who is invited, to which groups, and who teaches them: the »roster«, from
# which Main builds @@invitations, @@group_members, @@teachers and the rest
# (load_invitations_locked). Plain Ruby, tested on its own.
#
# Until an administrator imports them (/personen, »Einladungslisten
# importieren«), the invitation files under /invitations are the roster, as
# before. After the import it lives in Neo4j; files whose name starts with _
# are still read, because tools write them (the e2e tests, the screenshot
# generator).
module Roster
    # One group: its name, teachers [emails]; declared: it has a »>« line
    # (the old loader listed only those in the group order). Members are a
    # list of their own, [group name, email, name or nil], in the order read,
    # because a person's first group is the first line it appears in.
    Group = Struct.new(:name, :teachers, :declared, keyword_init: true)

    NO_GROUP = '(keine Gruppe)'

    # Invitation files, in the order read, as groups and the designated
    # teachers (* lines), with the old loader's rules: a file without »>«
    # continues the previous file's group; lines before any »>« belong to
    # »(keine Gruppe)«.
    def self.parse_files(texts)
        groups = {}
        members = []
        designated = []
        current = NO_GROUP
        group = lambda do |name, declared|
            g = (groups[name] ||= Group.new(:name => name, :teachers => [], :declared => false))
            g.declared ||= declared
            g
        end
        texts.each do |text|
            text.each_line do |line|
                next if line.strip.empty? || line.strip[0] == '#'

                if line[0] == '>'
                    current = line[1..].strip
                    group.call(current, true)
                elsif line[0] == '+'
                    email = TeacherRoles.normalize_email(line[1..])
                    group.call(current, false).teachers << email if email
                elsif line[0] == '*'
                    email = TeacherRoles.normalize_email(line[1..])
                    designated << email if email && !designated.include?(email)
                else
                    parts = line.strip.split(' ')
                    email = parts.last.delete_prefix('<').delete_suffix('>').downcase
                    name = parts.size > 1 ? parts[0..-2].join(' ') : nil
                    group.call(current, false)
                    members << [current, email, name]
                end
            end
        end
        { :groups => groups.values, :members => members, :designated => designated }
    end

    # Addresses as a teacher pastes them into /gruppen, one person a line:
    # »Lena Muster <lena@example.org>«, »lena@example.org« or »lena@example.org
    # Lena Muster«, also separated by commas, semicolons or tabs as from a
    # spreadsheet. Returns { people: [[email, name or nil]], rejected: [line] }.
    def self.parse_member_lines(text)
        people = []
        rejected = []
        text.to_s.each_line do |raw|
            line = raw.strip
            next if line.empty?

            email = nil
            name = nil
            if (m = line.match(/\A(.*?)\s*<([^<>\s]+)>\s*\z/))
                name, email = m[1], m[2]
            else
                parts = line.split(/[\s,;]+/)
                address = parts.find { |p| p.include?('@') }
                email = address
                name = (parts - [address]).join(' ') if address
            end
            email = TeacherRoles.normalize_email(email)
            if email.nil?
                rejected << line
                next
            end
            name = name.to_s.gsub(/[,;\t"]+/, ' ').strip.squeeze(' ')
            name = nil if name.empty?
            existing = people.index { |e, _| e == email }
            if existing
                people[existing][1] ||= name
            else
                people << [email, name]
            end
        end
        { :people => people, :rejected => rejected }
    end

    # --- The roster as records, as Neo4j keeps it --------------------------
    #
    # :groups       [{ name:, school: key, approved: true/false }]
    # :invitations  [{ email:, name: or nil, teacher: true/false, school: key or nil }]
    # :memberships  [[email, group name]]
    # :teaching     [[email, group name]]
    #
    # Group order does not matter: groups and members come sorted by name.

    TEACHERS_GROUP = 'Lehrkräfte'

    # The records for parsed invitation files (the import). Each person gets
    # the name the old loader showed; a name that is only the address is none.
    # Teachers belong to the school; a »+« line for someone not invited had
    # no effect (they could not log in) and is left out.
    def self.records_from_files(parsed, school:)
        built = build(:groups => parsed[:groups], :members => parsed[:members], :designated => parsed[:designated], :admins => [])
        designated = Set.new(parsed[:designated])
        emails = parsed[:members].map { |_, email, _| email }.uniq
        invited = Set.new(emails)
        teaching = parsed[:groups].flat_map { |g| g.teachers.map { |email| [email, g.name] } }.uniq.select { |email, _| invited.include?(email) }
        teachers = designated + teaching.map(&:first)
        {
            :groups => parsed[:groups].map { |g| { :name => g.name, :school => school, :approved => true } },
            :invitations => emails.map do |email|
                name = built[:invitations][email][:name]
                { :email => email, :name => name == email ? nil : name, :teacher => designated.include?(email),
                  :school => teachers.include?(email) ? school : nil }
            end,
            :memberships => parsed[:members].map { |group, email, _| [email, group] }.uniq,
            :teaching => teaching,
        }
    end

    # What Roster.build needs, from the records. Students of a group whose
    # school is not approved yet are left out, so they cannot log in; teachers
    # always count. A teacher in no group is listed under »Lehrkräfte«, as the
    # files had it, so that it is invited.
    def self.from_records(records)
        approved = records[:groups].to_h { |g| [g[:name], g[:approved]] }
        names = records[:invitations].to_h { |i| [i[:email], i[:name]] }
        teaching = records[:teaching].select { |_, group| approved.key?(group) }
        teachers = Set.new(records[:invitations].select { |i| i[:teacher] }.map { |i| i[:email] }) + teaching.map(&:first)
        members = records[:memberships].select do |email, group|
            approved.key?(group) && (approved[group] || teachers.include?(email))
        end
        members = members.sort_by { |email, group| [group, email] }.map { |email, group| [group, email, names[email]] }
        members = members.select { |_, email, _| names.key?(email) }
        lonely = teachers.select { |email| names.key?(email) }.sort - members.map { |_, email, _| email }
        members += lonely.map { |email| [TEACHERS_GROUP, email, names[email]] }
        group_names = (approved.keys + (lonely.empty? ? [] : [TEACHERS_GROUP])).uniq.sort
        groups = group_names.map do |name|
            Group.new(:name => name, :teachers => teaching.select { |_, g| g == name }.map(&:first).sort, :declared => true)
        end
        designated = records[:invitations].select { |i| i[:teacher] }.map { |i| i[:email] }.sort
        { :groups => groups, :members => members, :designated => designated }
    end

    # What a roster means, without order: who is invited and with which
    # name, who is in which group, who teaches which group, who is a teacher.
    # The import is right when this is the same before and after.
    def self.content(built)
        {
            :names => built[:invitations].to_h { |email, i| [email, i[:name]] },
            :groups => built[:group_members].transform_values { |m| Set.new(m) }.reject { |_, m| m.empty? },
            # Group rights of someone not invited have no effect.
            :teaching => built[:teachers].select { |email, _| built[:invitations].key?(email) }.transform_values { |g| Set.new(g) },
            :teacher_emails => Set.new(built[:teacher_emails]),
        }
    end

    # The differences between two contents, as lines for a person to read.
    def self.differences(before, after)
        lines = []
        (before[:names].keys | after[:names].keys).sort.each do |email|
            a, b = before[:names][email], after[:names][email]
            lines << "#{email}: #{a.inspect} → #{b.inspect}" unless a == b
        end
        (before[:groups].keys | after[:groups].keys).sort.each do |group|
            a, b = before[:groups][group] || Set.new, after[:groups][group] || Set.new
            lines << "Gruppe #{group}: fehlt #{(a - b).to_a.sort.join(', ')}" unless (a - b).empty?
            lines << "Gruppe #{group}: zusätzlich #{(b - a).to_a.sort.join(', ')}" unless (b - a).empty?
        end
        (before[:teaching].keys | after[:teaching].keys).sort.each do |email|
            a, b = before[:teaching][email] || Set.new, after[:teaching][email] || Set.new
            lines << "#{email} betreut: #{a.to_a.sort.join(', ')} → #{b.to_a.sort.join(', ')}" unless a == b
        end
        a, b = before[:teacher_emails], after[:teacher_emails]
        lines << "Lehrkräfte: fehlt #{(a - b).to_a.sort.join(', ')}" unless (a - b).empty?
        lines << "Lehrkräfte: zusätzlich #{(b - a).to_a.sort.join(', ')}" unless (b - a).empty?
        lines
    end

    # The structures Main keeps, from the groups, the members and the
    # administrators. A person's group is the first it appears in, its name
    # the last one given (without any, the address). Groups come in the order
    # given; teachers are merged by group name.
    def self.build(groups:, members:, designated:, admins:)
        invitations = {}
        user_groups = {}
        user_group_order = []
        group_members = {}
        teachers = {}

        admins.each do |email|
            user_group_order << 'Administrator' unless user_group_order.include?('Administrator')
            invitations[email] = { :group => 'Administrator', :name => email }
            (user_groups['Administrator'] ||= []) << email
        end

        groups.each do |group|
            user_group_order << group.name if group.declared && !user_group_order.include?(group.name)
            group.teachers.each { |email| (teachers[email] ||= Set.new) << group.name }
        end
        members.each do |group_name, email, name|
            list = (group_members[group_name] ||= [])
            list << email unless list.include?(email)
            unless invitations[email]
                (user_groups[group_name] ||= []) << email
                invitations[email] = { :group => group_name }
            end
            if name && !name.empty?
                invitations[email][:name] = name
            else
                invitations[email][:name] ||= email
            end
        end

        teacher_emails = TeacherRoles.resolve(
            :invited_emails => invitations.keys,
            :administrators => admins,
            :group_administrators => teachers.keys,
            :designated_teachers => designated,
        )
        {
            :invitations => invitations,
            :user_groups => user_groups,
            :user_group_order => user_group_order,
            :group_members => group_members,
            :teachers => teachers,
            :teacher_emails => teacher_emails,
        }
    end
end
