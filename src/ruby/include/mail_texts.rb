require 'cgi'

# The texts of the mails the Workspace sends besides login codes: the
# confirmation of a deletion and the welcome mails (to students from their
# teacher, to teachers from the administration). Plain Ruby, tested on its
# own; each returns [subject, html]. Students are addressed as »du«, as on
# the rest of the site.
module MailTexts
    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    def self.link(url)
        "<a href='#{h(url)}'>#{h(url)}</a>"
    end

    def self.closing(signature)
        "<p>Viele Grüße<br>#{h(signature)}</p>"
    end

    # Step two of deleting one's own account: the code, by mail, so that
    # nobody else at an unlocked computer can do it.
    def self.deletion_code(code:, minutes:, signature:)
        body = +"<p>Hallo!</p>\n"
        body << "<p>Du möchtest dein Konto im Workspace löschen. Der Code dafür lautet:</p>\n"
        body << "<p style='font-size: 200%;'>#{h(code)}</p>\n"
        body << "<p>Er gilt #{minutes} Minuten. Gelöscht wird alles: dein Workspace mit allen Dateien, deine Datenbanken, " \
            "deine Antworten und Abgaben. Wenn du das nicht wolltest, tu einfach nichts; dann bleibt alles, wie es ist.</p>\n"
        body << closing(signature)
        ["Code zum Löschen deines Kontos: #{code}", body]
    end

    # To the administration, after someone deleted its own account: the
    # address may still have to come off an invitation list.
    def self.account_deleted_by_owner(email:, name:, groups:, still_invited:, signature:)
        body = +"<p>#{h(name || email)} (#{h(email)}) hat das eigene Konto im Workspace gelöscht.</p>\n"
        body << "<p>Gruppen: #{groups.empty? ? 'keine' : h(groups.join(', '))}.</p>\n"
        body << if still_invited
            '<p>Die Adresse steht noch in einer Einladungsliste; wer sie herausnimmt, verhindert, dass sie sich wieder anmeldet. Ist es so gewollt, ist nichts zu tun.</p>'
        else
            '<p>Die Adresse steht in keiner Einladungsliste mehr; es ist nichts zu tun.</p>'
        end
        ["Konto gelöscht: #{email}", body + "\n" + closing(signature)]
    end

    # The answer to »Bitte lösch mein Konto«.
    def self.account_deleted(web_root:, signature:, still_invited:)
        body = +"<p>Hallo!</p>\n"
        body << "<p>Dein Konto im Workspace (#{link(web_root)}) ist gelöscht, mit allem, was dazugehört: " \
            "dein Workspace mit allen Dateien, deine Datenbanken, deine Antworten und Abgaben und deine Sitzungen.</p>\n"
        body << if still_invited
            "<p>Du bist weiter eingeladen. Meldest du dich wieder an, beginnst du mit einem leeren Konto.</p>\n"
        else
            "<p>Du bist nicht mehr eingeladen und kannst dich nicht mehr anmelden.</p>\n"
        end
        body << "<p>Aus den verschlüsselten Sicherungen verschwinden die Daten, wenn diese ablaufen; wird eine Sicherung zurückgespielt, wird dein Konto sofort wieder gelöscht.</p>\n"
        body << closing(signature)
        ['Dein Konto im Workspace ist gelöscht', body]
    end
end
