require 'cgi'
require_relative 'group_visibility'

# A teacher looks into a student's account: as the student (impersonation)
# or in the student's workspace. Both need a short reason, which the student
# finds calmly the next time it logs in: »Herr Beispiel hat sich am 14.10. um
# 10:12 deinen Workspace angesehen: Ich sehe mir deine Abgabe an.« Grades
# never go here. Plain Ruby, tested on its own.
module LookIn
    # stop and cleanup: the administration stopped the Workspace (a runaway
    # program) or deleted folders that a tool makes again (»Platz
    # schaffen«); details says which, with the command.
    KINDS = %w[impersonate workspace stop cleanup].freeze
    MAX_REASON = 300
    MIN_REASON = 5

    # May viewer look into target's account? An administrator into anyone's
    # but its own; a teacher into a student of one of its groups, never into
    # another teacher's or an administrator's.
    def self.allowed?(viewer, target, admins:, teachers:, group_members:)
        return false if viewer.nil? || target.nil? || viewer == target
        return true if admins.include?(viewer)
        return false unless teachers.include?(viewer)
        return false if admins.include?(target) || teachers.include?(target)

        GroupVisibility.visible?(target, :group_members => group_members, :viewer_groups => teachers[viewer])
    end

    # A teacher's test account: its own address with +test, so that with most
    # mail providers the login codes reach the teacher's inbox.
    # lehrer@schule.de -> lehrer+test@schule.de
    def self.test_account_email(teacher)
        local, domain = teacher.to_s.downcase.split('@', 2)
        return nil if local.to_s.empty? || domain.to_s.empty? || local.end_with?('+test')

        "#{local}+test@#{domain}"
    end

    # The reason as stored: one line, at most MAX_REASON characters. Returns
    # nil when a teacher gave none worth showing; an administrator may look
    # in without one (support), and then the student sees nothing.
    # Administrators too (the maintainer's decision, 2026-10-19): whoever
    # opens a Workspace or imitates someone says why, and the person sees it.
    # admin: kept for callers, it no longer changes anything.
    def self.reason(text, admin: false)
        line = text.to_s.gsub(/\s+/, ' ').strip[0, MAX_REASON]
        return line if line.size >= MIN_REASON

        nil
    end

    # The notice a student sees, as HTML: who, when, what, and why.
    # look: { :by_name, :ts (epoch seconds), :kind, :reason, :details }
    def self.notice_html(look, now: Time.now)
        time = Time.at(look[:ts].to_i).localtime
        day = time.to_date == now.to_date ? 'heute' : "am #{time.strftime('%d.%m.')}"
        who = "<strong>#{CGI.escapeHTML(look[:by_name].to_s)}</strong>"
        why = "»#{CGI.escapeHTML(look[:reason].to_s)}«"
        at = "#{day} um #{time.strftime('%H:%M')} Uhr"
        case look[:kind]
        when 'stop'
            "#{who} hat #{at} deinen Workspace gestoppt: #{why} Deine Dateien sind alle noch da; »Workspace öffnen« startet ihn wieder."
        when 'cleanup'
            "#{who} hat #{at} in deinem Workspace Platz geschaffen: #{why} Gelöscht wurden Ordner, die ein Befehl wieder anlegt: " \
                "#{CGI.escapeHTML(look[:details].to_s)}."
        else
            what = look[:kind] == 'workspace' ? 'deinen Workspace angesehen' : 'dein Konto angesehen'
            "#{who} hat sich #{at} #{what}: #{why}"
        end
    end
end
