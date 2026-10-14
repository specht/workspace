# Which of a student's groups a teacher may see (docs/aufgaben.md, invitation
# files). A student can be listed in several groups, Klasse 12 and LK
# Informatik for example; every teacher who administers one of them sees the
# student there. Administrators see all groups. Plain Ruby, tested on its own.
module GroupVisibility
    # The groups an address is listed in, in file order.
    # group_members: { 'Klasse 12' => [emails], ... } in file order
    def self.groups_of(email, group_members)
        group_members.select { |_, members| members.include?(email) }.keys
    end

    # The student's groups the viewer may see: all of them for an
    # administrator, otherwise those the viewer administers.
    def self.shared_groups(email, group_members:, viewer_groups:, admin: false)
        groups = groups_of(email, group_members)
        admin ? groups : groups & viewer_groups.to_a
    end

    def self.visible?(email, group_members:, viewer_groups:, admin: false)
        return true if admin

        !shared_groups(email, :group_members => group_members, :viewer_groups => viewer_groups).empty?
    end

    # Login codes for one viewer: each with the groups it belongs to for this
    # viewer, codes of students outside the viewer's groups left out. The
    # groups the viewer can choose from come along, in file order: those it
    # administers, or for an administrator those that have a code.
    # lines: [{ :email, :code, :expires_at }]
    def self.login_codes_for(lines, group_members:, group_order:, viewer_groups:, admin: false)
        shown = lines.filter_map do |line|
            groups = shared_groups(line[:email], :group_members => group_members,
                :viewer_groups => viewer_groups, :admin => admin)
            next nil if groups.empty? && !admin

            line.merge(:groups => groups)
        end
        choices = admin ? shown.flat_map { |line| line[:groups] }.uniq : viewer_groups.to_a
        { :lines => shown, :groups => group_order.select { |g| choices.include?(g) } }
    end
end
