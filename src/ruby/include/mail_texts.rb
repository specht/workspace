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

    # Before the administration deletes an account: the date, what the
    # person can do until then, and that doing nothing is fine. Replies go
    # to the contact address.
    def self.deletion_notice(web_root:, date:, signature:)
        day = date.strftime('%d.%m.%Y')
        body = +"<p>Hallo!</p>\n"
        body << "<p>Dein Konto im Workspace (#{link(web_root)}) wird am <strong>#{day}</strong> gelöscht, mit allem, was dazugehört: " \
            'dein Workspace mit allen Dateien, deine Datenbanken, deine Antworten und Abgaben.</p>' + "\n"
        body << "<p>Bis dahin kannst du:</p>\n<ul>\n"
        body << "<li>alle deine Daten herunterladen: Melde dich an und öffne im Profil »Meine Daten« (#{link("#{web_root}/profil#my-data")}). " \
            'Du bekommst ein ZIP mit deinem Workspace, deinen Abgaben und deinen Datenbanken.</li>' + "\n"
        body << '<li>dein Konto behalten: Antworte auf diese E-Mail. Wer nicht mehr zur Schule gehört, kann den Workspace als Gast weiter nutzen; ' \
            'dann gilt die Nutzungsordnung.</li>' + "\n"
        body << "</ul>\n"
        body << "<p>Wenn du nichts davon möchtest, musst du nichts tun: Am #{day} wird das Konto gelöscht.</p>\n"
        body << closing(signature)
        ["Dein Konto im Workspace wird am #{day} gelöscht", body]
    end

    # From a teacher to the people in its group, so that they know the
    # Workspace exists and how to log in. Replies go to the teacher.
    def self.welcome_student(web_root:, group:, teacher:, email:, privacy_url: nil)
        body = +"<p>Hallo!</p>\n"
        body << "<p>#{h(teacher)} hat dich in die Gruppe <strong>#{h(group)}</strong> im Workspace eingeladen. " \
            'Der Workspace ist eine Programmierumgebung im Browser für den Unterricht: Du bekommst einen eigenen Editor, ' \
            'Aufgaben, die der Workspace selbst prüft, und ein Handbuch.</p>' + "\n"
        body << "<p>So meldest du dich an: Öffne #{link(web_root)}, gib deine E-Mail-Adresse <strong>#{h(email)}</strong> ein " \
            'und dann den Code, den du per E-Mail bekommst. Danach bleibst du angemeldet. Es gibt kein Passwort.</p>' + "\n"
        body << "<p>Was der Workspace speichert und wer es sieht, steht in der Datenschutzerklärung: #{link(privacy_url || "#{web_root}/impressum#datenschutz")}. " \
            'Fragen beantwortet deine Lehrkraft; du kannst einfach auf diese E-Mail antworten.</p>' + "\n"
        body << closing(teacher)
        ["Willkommen im Workspace: #{group}", body]
    end

    # From the administration to a new teacher. school_approved: whether the
    # school has agreed; if not, what covers the students until then.
    def self.welcome_teacher(web_root:, school:, school_approved:, email:, signature:, documents_url:, cost_note: nil)
        body = +"<p>Hallo!</p>\n"
        body << "<p>Du hast jetzt ein Konto als Lehrkraft im Workspace (#{link(web_root)}), für <strong>#{h(school)}</strong>. " \
            "Anmelden kannst du dich mit deiner Adresse #{h(email)}; den Code dafür bekommst du per E-Mail.</p>\n"
        body << "<p>Für den Anfang:</p>\n<ul>\n"
        body << "<li>#{link("#{web_root}/lehrmaterial-einstieg")}: was du für die erste Stunde brauchst und wie du mit deinem Testkonto alles selbst ausprobierst.</li>\n"
        body << "<li>#{link("#{web_root}/gruppen")}: Hier legst du Gruppen an, trägst die Adressen deiner Schülerinnen und Schüler ein (Namen sind freiwillig) und schickst ihnen eine Willkommensmail.</li>\n"
        body << "<li>#{link(documents_url)}: die Unterlagen für deine Schule, zum Drucken: eine Seite mit der Entscheidung für die Schulleitung, ein Elternbrief und mehr.</li>\n"
        body << "</ul>\n"
        body << if school_approved
            "<p>Deine Schule hat dem Einsatz zugestimmt; deine Schülerinnen und Schüler können sich anmelden, sobald du sie einträgst.</p>\n"
        else
            "<p>Deine Schule hat dem Einsatz noch nicht zugestimmt. Am besten unterschreibt die Schulleitung die Entscheidung " \
                "und die Vereinbarung zur Auftragsverarbeitung aus den Unterlagen; schick sie dann an die Administration. " \
                "Bis dahin genügt für eine Schülerin oder einen Schüler ein schriftliches Einverständnis (der Elternbrief mit Einverständniserklärung ist in den Unterlagen); " \
                "das bewahrst du auf und vermerkst es unter Gruppen. Anmelden können sich deine Schülerinnen und Schüler schon jetzt.</p>\n"
        end
        body << "<p>#{h(cost_note)}</p>\n" if cost_note && !cost_note.strip.empty?
        body << closing(signature)
        ['Willkommen im Workspace', body]
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
