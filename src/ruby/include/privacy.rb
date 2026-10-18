require 'cgi'
require 'date'
require_relative 'authentication'

# The Impressum and the Datenschutzerklärung (/impressum) and the documents
# for the school (/datenschutz/…, privacy_documents.rb), all from the facts
# below and the settings in credentials.rb, so that nothing on one page
# contradicts another (docs/datenschutz.md). Plain Ruby, tested on its own:
# every function takes the settings as a Hash (Privacy.settings).
#
# The facts were checked in the code on 2026-10-18. Whoever changes what is
# stored, who sees it or which outside service is used changes it here too.
module Privacy
    # When the facts were last checked against the code; the page says so.
    CHECKED = Date.new(2026, 10, 18)

    LEGAL_BASES = [:school, :consent].freeze

    # The settings the pages use, each optional (credentials.template.rb).
    STRING_SETTINGS = %w[
        OPERATOR_NAME OPERATOR_ROLE OPERATOR_SCHOOL OPERATOR_ADDRESS CONTACT_EMAIL
        DATA_PROTECTION_OFFICER HOSTING MAIL_PROVIDER SCHOOL_LAW SUPERVISORY_AUTHORITY
    ].freeze

    # The settings as a Hash with symbol keys: strings stripped, an empty or
    # missing one nil; legal_basis :school, :consent or nil.
    def self.settings(scope = Object)
        values = STRING_SETTINGS.to_h do |name|
            value = scope.const_defined?(name) ? scope.const_get(name).to_s.strip : ''
            [name.downcase.to_sym, value.empty? ? nil : value]
        end
        basis = scope.const_defined?('LEGAL_BASIS') ? scope.const_get('LEGAL_BASIS').to_s.strip.to_sym : nil
        values[:legal_basis] = LEGAL_BASES.include?(basis) ? basis : nil
        values[:parental_consent_required] =
            scope.const_defined?('PARENTAL_CONSENT_REQUIRED') && scope.const_get('PARENTAL_CONSENT_REQUIRED') == true
        values
    end

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    def self.lines_html(text)
        text.to_s.split("\n").map { |line| h(line.strip) }.reject(&:empty?).join('<br>')
    end

    def self.email_link(s)
        return nil unless s[:contact_email]

        "<a href='mailto:#{h(s[:contact_email])}'>#{h(s[:contact_email])}</a>"
    end

    # »an Michael Specht (kontakt@…)«, as far as it is configured.
    def self.write_to_html(s)
        name = s[:operator_name] ? h(s[:operator_name]) : nil
        link = email_link(s)
        return "an #{name} (#{link})" if name && link
        return "an #{link}" if link
        return "an #{name}" if name

        'an die Lehrkraft, die dich eingeladen hat'
    end

    # Who is responsible (Verantwortlicher): the school when it uses the
    # Workspace by a decision of the Schulleitung, otherwise the operator.
    def self.school_is_controller?(s)
        s[:legal_basis] == :school && !s[:operator_school].nil?
    end

    def self.operator_line(s)
        parts = [s[:operator_name], s[:operator_role]].compact
        parts << "am #{s[:operator_school]}" if s[:operator_school]
        parts.empty? ? nil : h(parts.join(', ')).sub(', am ', ' am ')
    end

    # What happens to the reader's login code: the code and the tag.
    def self.login_minutes
        Authentication::LOGIN_REQUEST_TTL_SECONDS / 60
    end

    def self.qr_minutes
        Authentication::QR_LOGIN_REQUEST_TTL_SECONDS / 60
    end

    def self.session_days
        Authentication::SESSION_LIFETIME_DAYS
    end

    # How long anything is kept when no-one deletes it earlier.
    ACCOUNT_END = 'bis das Konto gelöscht wird (Löschregeln unten)'

    # What is stored about a person: data, purpose, who sees it, how long.
    # »Lehrkräfte« are always those of the person's own groups.
    def self.records
        [
            {
                :data => 'E-Mail-Adresse, Name und Gruppe (Klasse oder Kurs)',
                :source => 'aus der Einladungsliste der Lehrkraft',
                :purpose => 'Anmeldung; Zuordnung zur Gruppe',
                :who => 'du, Lehrkräfte, Administration',
                :how_long => ACCOUNT_END,
            },
            {
                :data => 'Anmeldeanfragen: Code und eine zufällige Kennung des Browsers',
                :purpose => 'Anmeldung',
                :who => 'niemand; der Code steht in deiner E-Mail',
                :how_long => "#{login_minutes} Minuten (Anmeldung per QR-Code: #{qr_minutes} Minuten) oder bis zur Anmeldung",
            },
            {
                :data => 'Sitzungen: eine zufällige Kennung je angemeldetem Browser',
                :purpose => 'angemeldet bleiben',
                :who => 'niemand',
                :how_long => "#{session_days} Tage oder bis zur Abmeldung",
            },
            {
                :data => 'Einstellungen: Helligkeit, Programmiersprache, Sprache der Seiten',
                :purpose => 'Darstellung',
                :who => 'du',
                :how_long => ACCOUNT_END,
            },
            {
                :data => 'Dein Workspace: deine Dateien und Programme, der Verlauf im Terminal, installierte Pakete und Erweiterungen',
                :purpose => 'Programmieren im Unterricht und zu Hause',
                :who => 'du; Lehrkräfte, wenn sie hineinsehen (siehe unten); Administration',
                :how_long => ACCOUNT_END,
            },
            {
                :data => 'Deine Datenbanken (MySQL, Neo4j) mit allem, was du darin anlegst',
                :purpose => 'Aktivitäten zu Datenbanken',
                :who => 'du; Administration',
                :how_long => ACCOUNT_END,
            },
            {
                :data => 'Aufgaben: deine letzte Antwort, Ergebnis der Prüfung, Zahl der Versuche und Zeitpunkte',
                :purpose => 'Rückmeldung für dich; Überblick für die Lehrkraft',
                :who => 'du, Lehrkräfte',
                :how_long => 'bis du sie auf der Aufgabenkarte löschst, sonst ' + ACCOUNT_END,
            },
            {
                :data => 'Abgaben: eine Kopie der Dateien, die du bei einer Aufgabe abgibst',
                :purpose => 'Abgabe an die Lehrkraft',
                :who => 'du, Lehrkräfte',
                :how_long => 'bis du sie löschst oder neu abgibst, sonst ' + ACCOUNT_END,
            },
            {
                :data => 'Hinweise, wer in deinen Workspace oder dein Konto gesehen hat: wer, wann, warum',
                :purpose => 'damit du es erfährst',
                :who => 'du; Administration',
                :how_long => ACCOUNT_END,
            },
            {
                :data => 'Links, mit denen du deinen Workspace oder eine laufende Web-App teilst',
                :purpose => 'zeigen, was du gebaut hast; Hilfe bekommen',
                :who => 'wer den Link hat (siehe »Teilen«)',
                :how_long => 'bis du den Link abschaltest',
            },
            {
                :data => 'Protokolle des Servers: welche Seite wann mit welchem Ergebnis abgerufen wurde, ohne IP-Adresse und ohne Namen; Meldungen der Programme, in denen bei manchen Vorgängen (Anmeldung, Start eines Workspace) die E-Mail-Adresse steht',
                :purpose => 'Betrieb und Fehlersuche',
                :who => 'Administration',
                :how_long => 'Zugriffsprotokolle 7 Tage; Meldungen der Programme, bis sie überschrieben werden (höchstens 3 × 10 MB je Dienst)',
            },
        ]
    end

    # Hosts outside the server that the code or the activities use, and the
    # name the page gives each (privacy_test.rb checks that the page names
    # each one and that the hosts in the code are here).
    OUTSIDE_HOSTS = {
        'api.open-meteo.com' => 'Open-Meteo',
        'geocoding-api.open-meteo.com' => 'Open-Meteo',
        'open-vsx.org' => 'Open VSX',
        'git.nhcham.org' => 'git.nhcham.org',
        'github.com' => 'GitHub',
    }.freeze

    # Services outside the server that see something. kind: :processor
    # (works for the operator), :workspace (what the server or a Workspace
    # fetches, with no data about the person), :own (what a student's own
    # program or page contacts).
    def self.services(s)
        hosting = s[:hosting] ? h(s[:hosting]) : 'einem Rechenzentrum'
        mail = s[:mail_provider] ? " (#{h(s[:mail_provider])})" : ''
        [
            {
                :name => 'Rechenzentrum',
                :kind => :processor,
                :html => "Der Server läuft bei #{hosting}. Dort liegen alle Daten aus der Tabelle oben; das Rechenzentrum stellt die Technik bereit.",
            },
            {
                :name => 'E-Mail',
                :kind => :processor,
                :html => "Anmeldecodes und Einladungen verschickt ein E-Mail-Dienst#{mail}. Er sieht deine Adresse, die Uhrzeit und den Code; der Code allein meldet niemanden an (siehe »Anmeldung per Code«).",
            },
            {
                :name => 'Paketquellen und GitHub',
                :kind => :workspace,
                :html => 'Vorlagen und Programmbibliotheken holt dein Workspace von GitHub und den üblichen Paketquellen (RubyGems, PyPI, npm und andere), wenn eine Aktivität es sagt oder du es selbst tust. Diese Dienste sehen die Adresse des Servers, nicht deine.',
            },
            {
                :name => 'Open-Meteo',
                :kind => :workspace,
                :html => 'Für die Aktivität »Daten aus dem Netz« gibt es einen Spiegel des Wetterdienstes Open-Meteo auf dem Server: Fragt dein Programm dort, sieht Open-Meteo nur den Server.',
            },
            {
                :name => 'GitLab',
                :kind => :own,
                :html => 'Manche Lernende nutzen auf eigenen Wunsch zusätzlich den Git-Server git.nhcham.org. Er gehört nicht zum Workspace, und keine Aktivität braucht ihn. Wer ihn nutzt, hat dort ein eigenes Konto; der Workspace überträgt nichts dorthin, nur was du selbst hochlädst.',
            },
            {
                :name => 'Open VSX',
                :kind => :own,
                :html => 'Suchst du im Editor nach einer Erweiterung oder installierst eine, fragt der Editor bei Open VSX (Eclipse Foundation) nach.',
            },
            {
                :name => 'Eigene Programme',
                :kind => :own,
                :html => 'Was deine eigenen Programme und Webseiten abrufen (eine Wetter-API, ein Bild von Wikipedia, eine Schrift von Google Fonts), bestimmst du selbst. Öffnest du eine solche Seite im Browser, sieht der fremde Dienst die Adresse deines Internetanschlusses, wie bei jeder anderen Webseite.',
            },
        ]
    end

    # Technical and organisational measures in place (TOM), by the headings
    # of Art. 32 DSGVO. Each one is true of the code as it is.
    def self.measures(s)
        {
            'Zugang' => [
                'Anmelden kann sich nur, wer von einer Lehrkraft eingeladen wurde.',
                "Anmeldung mit einem Code per E-Mail, der nur zusammen mit dem Browser gilt, der ihn angefordert hat; der Code gilt #{login_minutes} Minuten und höchstens 5 Versuche lang. Es gibt keine Passwörter, die erraten oder wiederverwendet werden könnten.",
                'Anfragen zur Anmeldung sind je E-Mail-Adresse und je Internetanschluss begrenzt; die Zählung steht nur im Arbeitsspeicher und wird nirgends gespeichert.',
                'Sitzungs-Cookies sind HttpOnly, Secure und SameSite=Lax.',
            ],
            'Trennung' => [
                'Jede Person hat einen eigenen Workspace in einem eigenen Container und eigene Datenbanken mit eigenen Zugangsdaten.',
                'Die Workspaces erreichen einander im Netz nicht; eine Firewall lässt nur einen Port frei, den die Aktivitäten zu Netzwerken brauchen.',
                'Lehrkräfte sehen nur die Lernenden ihrer eigenen Gruppen, nie die Konten anderer Lehrkräfte.',
            ],
            'Transparenz' => [
                'Öffnet eine Lehrkraft deinen Workspace oder sieht die Seite so, wie du sie siehst, gibt sie einen Grund an (5 bis 300 Zeichen), und du siehst beim nächsten Besuch, wer, wann und warum.',
                'Noten werden im Workspace nicht gespeichert; die Prüfung von Aufgaben ist eine Rückmeldung, keine Bewertung.',
            ],
            'Sparsamkeit' => [
                'Kein Tracking, keine Analyse-Werkzeuge, keine Werbung.',
                'Schriften, Skripte und Symbole kommen vom Server selbst; die Seiten laden nichts von fremden Servern.',
                'Die Zugriffsprotokolle enthalten keine IP-Adressen und keine Namen; geheime Teile von Adressen (Links zum Teilen, Anmeldelinks) werden unkenntlich gemacht. Sie werden nach 7 Tagen gelöscht.',
                'Von Aufgaben bleibt nur die letzte Antwort; Versuche werden ohne Text gezählt.',
            ],
            'Übertragung' => [
                'Alle Verbindungen zur Seite und zu den Workspaces sind verschlüsselt (HTTPS, Zertifikat von Let’s Encrypt).',
            ],
            'Verfügbarkeit und Löschung' => [
                'Lernende löschen ihre Antworten und Abgaben selbst, wann sie wollen.',
                'Die Administration löscht ein Konto mit allem, was dazugehört (Workspace, Datenbanken, Aufgaben, Abgaben, Sitzungen), in einem Schritt.',
                'Der Quelltext ist öffentlich; jede Aussage hier lässt sich am Code prüfen.',
            ],
        }
    end

    # The rule for deleting accounts (Löschkonzept), as one sentence for the
    # page and the documents.
    def self.deletion_rule_html(s)
        'Ein Konto wird mit allem, was dazugehört, gelöscht, sobald es nicht mehr gebraucht wird: ' \
            'spätestens zum Ende des Schuljahres, in dem dein Kurs endet oder du die Schule verlässt, ' \
            "und jederzeit früher, wenn du (oder deine Eltern) darum bitten. Schreib dafür #{write_to_html(s)}."
    end

    def self.legal_basis_html(s)
        case s[:legal_basis]
        when :school
            law = s[:school_law] ? h(s[:school_law]) : 'dem Schulgesetz des Landes'
            text = "<p>Die Schule setzt den Workspace als digitales Lernmittel im Informatikunterricht ein; die Schulleitung hat das schriftlich entschieden. " \
                "Rechtsgrundlage ist Art. 6 Abs. 1 Buchstabe e DSGVO in Verbindung mit #{law}. " \
                'Eine Einwilligung ist dafür nicht nötig.</p>'
            if s[:parental_consent_required]
                text += '<p>Zusätzlich wird ein Konto erst freigeschaltet, wenn die Eltern ihr Einverständnis gegeben haben; Lernende ab 16 Jahren geben es selbst.</p>'
            end
            text + others_html
        when :consent
            "<p>Rechtsgrundlage ist deine Einwilligung (Art. 6 Abs. 1 Buchstabe a DSGVO); bist du jünger als 16, die deiner Eltern (Art. 8 DSGVO). " \
                'Ein Konto wird erst freigeschaltet, wenn die Einwilligung vorliegt. ' \
                'Die Teilnahme ist freiwillig: Wer nicht teilnimmt, hat dadurch keinen Nachteil und bekommt eine andere Möglichkeit, die Aufgaben zu bearbeiten. ' \
                "Die Einwilligung kann jederzeit widerrufen werden, mit einer E-Mail #{write_to_html(s)}; das Konto wird dann gelöscht.</p>"
        else
            "<p>Die Rechtsgrundlage ist für diese Installation noch nicht eingetragen. Fragen dazu beantwortet #{s[:contact_email] ? email_link(s) : 'die Lehrkraft, die dich eingeladen hat'}.</p>"
        end
    end

    # Who is not covered by the school's decision: adults from elsewhere and
    # the classes of other schools.
    def self.others_html
        '<p>Für Erwachsene von außerhalb der Schule (etwa Lehrkräfte anderer Schulen) ist die Grundlage die Vereinbarung, ' \
            'mit der sie ein Konto erhalten: die Nutzungsordnung (Art. 6 Abs. 1 Buchstabe b DSGVO). ' \
            'Lernende einer anderen Schule nutzen den Workspace nur, wenn ihre Schule das ebenso entschieden hat; ' \
            'für sie ist dann ihre Schule verantwortlich.</p>'
    end

    def self.controller_html(s)
        address = s[:operator_address] ? "<br>#{lines_html(s[:operator_address])}" : ''
        if school_is_controller?(s)
            out = "<p>Verantwortlich ist die Schule: <strong>#{h(s[:operator_school])}</strong>, vertreten durch die Schulleitung#{address}</p>"
            if s[:operator_name]
                out += "<p>Den Workspace betreibt im Auftrag der Schule #{operator_line(s)}. " \
                    "Für alle Fragen zum Datenschutz schreib #{write_to_html(s)}.</p>"
            end
        else
            who = operator_line(s) || 'die Lehrkraft, die den Workspace betreibt'
            out = "<p>Verantwortlich ist #{who}#{address}</p>"
            out += "<p>E-Mail: #{email_link(s)}</p>" if s[:contact_email]
        end
        if s[:data_protection_officer]
            out += "<p>Die oder der Datenschutzbeauftragte der Schule:<br>#{lines_html(s[:data_protection_officer])}</p>"
        end
        out
    end

    # Art. 20 (portability) holds only for consent; Art. 21 (objection) only
    # for a public task.
    def self.rights_html(s)
        out = 'Du (und deine Eltern) habt das Recht auf Auskunft, welche Daten über dich gespeichert sind (Art. 15 DSGVO), ' \
            'auf Berichtigung (Art. 16), auf Löschung (Art. 17) und auf Einschränkung der Verarbeitung (Art. 18). '
        if s[:legal_basis] == :consent
            out + 'Ihr habt das Recht auf Datenübertragbarkeit (Art. 20) und könnt die Einwilligung jederzeit widerrufen (Art. 7 Abs. 3).'
        else
            out + 'Ihr könnt der Verarbeitung widersprechen (Art. 21).'
        end
    end

    def self.supervisory_html(s)
        if s[:supervisory_authority]
            "Du kannst dich bei der Aufsichtsbehörde beschweren:<br>#{lines_html(s[:supervisory_authority])}"
        else
            'Du kannst dich bei der Datenschutz-Aufsichtsbehörde deines Bundeslandes beschweren.'
        end
    end

    def self.records_table_html(rows = records)
        out = +"<table class='table privacy-table'>\n<thead><tr><th>Daten</th><th>Wozu</th><th>Wer sieht sie</th><th>Wie lange</th></tr></thead>\n<tbody>\n"
        rows.each do |r|
            source = r[:source] ? "<br><span class='text-body-secondary'>#{h(r[:source])}</span>" : ''
            out << "<tr><td>#{h(r[:data])}#{source}</td><td>#{h(r[:purpose])}</td><td>#{h(r[:who])}</td><td>#{h(r[:how_long])}</td></tr>\n"
        end
        out << "</tbody>\n</table>\n"
        out
    end

    def self.measures_html(s)
        measures(s).map do |heading, items|
            "<h4>#{h(heading)}</h4>\n<ul>\n" + items.map { |m| "<li>#{h(m)}</li>" }.join("\n") + "\n</ul>"
        end.join("\n")
    end

    def self.impressum_html(s)
        address = s[:operator_address] ? "<br>#{lines_html(s[:operator_address])}" : ''
        operator = operator_line(s)
        out = +"<h1>Impressum</h1>\n"
        out << "<p>#{operator ? "<strong>#{operator}</strong>" : 'Die Angaben zur Person, die den Workspace betreibt, sind in dieser Installation noch nicht eingetragen.'}#{address}</p>\n"
        out << "<p>E-Mail: #{email_link(s)}</p>\n" if s[:contact_email]
        out << "<p>Der Workspace ist ein nicht-kommerzielles Angebot für den Informatikunterricht, betrieben von einer Lehrkraft. " \
            "Es gibt keine Werbung, nichts zu kaufen und keine Auswertung der Daten für andere Zwecke.</p>\n"
        out << "<p>Quelltext: <a href='https://github.com/specht/workspace'>https://github.com/specht/workspace</a></p>\n"
        out
    end

    # The whole /impressum page body.
    def self.page_html(s)
        out = +"<div class='privacy-page'>\n"
        out << impressum_html(s)
        out << <<~END_OF_HTML
            <h1 id='datenschutz'>Datenschutzerklärung</h1>
            <p>
            Hier steht genau, welche Daten der Workspace speichert, wozu, wer sie sieht und wann sie gelöscht werden.
            Die Seite ist für Lernende und ihre Eltern geschrieben. Der Workspace ist eine Programmierumgebung:
            Was hier entsteht, sind vor allem Programme, Dateien und Übungen.
            </p>

            <h2>Wer verantwortlich ist</h2>
            #{controller_html(s)}

            <h2>Wozu</h2>
            <p>
            Für den Informatikunterricht: Du programmierst in deinem eigenen Workspace, bearbeitest Aufgaben, lässt sie prüfen
            und gibst Ergebnisse ab; deine Lehrkraft sieht, wie weit die Gruppe ist, und hilft, wenn etwas nicht läuft.
            Für nichts anderes: keine Noten, keine Werbung, keine Profile, keine Weitergabe für fremde Zwecke.
            </p>

            <h2>Rechtsgrundlage</h2>
            #{legal_basis_html(s)}

            <h2>Was gespeichert wird</h2>
            #{records_table_html}
            <p>
            Alles davon liegt auf dem Server des Workspace. Eine Kopie der Daten für andere Zwecke gibt es nicht.
            </p>

            <h2>Anmeldung per Code</h2>
            <p>
            Wenn du dich anmeldest, merkt sich dein Browser eine zufällige Kennung, und du bekommst einen Code per E-Mail.
            Angemeldet wirst du nur mit beidem: dem Code und dem Browser, der ihn angefordert hat. Der E-Mail-Dienst sieht
            den Code, aber mit dem Code allein kann sich niemand anmelden. Der Code gilt #{login_minutes} Minuten und höchstens 5 Versuche lang.
            Wer im Unterricht nicht an seine E-Mails kommt, kann den Code auch von der Lehrkraft bekommen: Sie sieht auf ihrer Klassenseite
            die Codes ihrer Gruppe, solange jemand auf einen wartet, kann sich damit aber selbst nicht anmelden.
            </p>

            <h2>Was Lehrkräfte sehen</h2>
            <p>
            Lehrkräfte deiner Gruppen sehen in der Klassenübersicht, welche Aufgaben du bearbeitet hast und mit welchem Ergebnis,
            und deine Abgaben. Um dir zu helfen, können sie deinen Workspace öffnen oder die Seite so sehen, wie du sie siehst.
            Dafür geben sie einen kurzen Grund an, und du siehst beim nächsten Besuch einen Hinweis, zum Beispiel:
            »Frau Berger hat sich am 14.10. um 10:12 deinen Workspace angesehen: Ich sehe mir deine Abgabe an.«
            In deinem Profil stehen die letzten dieser Hinweise.
            </p>
            <p>
            Wer den Server betreibt (die Administration), kann wie bei jedem Server alle Daten darauf einsehen und tut das nur,
            um den Workspace zu betreiben und bei Problemen zu helfen.
            </p>

            <h2>Teilen</h2>
            <p>
            Du kannst deinen Workspace teilen: Dann bekommst du einen Link, mit dem jede Person, die ihn hat, deinen Workspace öffnen kann,
            auch ohne Anmeldung. Eine Web-App, die in deinem Workspace läuft, kannst du ebenfalls teilen; diesen Link öffnen nur Personen,
            die im Workspace angemeldet sind. Beides gilt, bis du es abschaltest. Teile nur, was du zeigen möchtest.
            </p>

            <h2>Cookies und Speicher im Browser</h2>
            <p>
            Vor der Anmeldung setzt die Seite keine Cookies. Nach der Anmeldung sind es technisch notwendige: einer, damit du angemeldet bleibst,
            und einer, mit dem dein Browser deinen Workspace öffnen darf (für eine Lehrkraft, die in einen Workspace sieht, ein dritter).
            Daneben merkt sich dein Browser ein paar Einstellungen der Seite (lokaler Speicher), etwa ob ein Hinweis schon ausgeblendet ist,
            und der Editor merkt sich dort, welche Dateien offen waren. Das verlässt deinen Browser nicht.
            </p>

            <h2>Dienste außerhalb des Servers</h2>
            <p>
            Die Seiten des Workspace laden nichts von fremden Servern: Schriften, Skripte und Symbole kommen vom Server selbst.
            Diese Dienste sind trotzdem beteiligt:
            </p>
            <ul>
            #{services(s).map { |svc| "<li>#{svc[:html]}</li>" }.join("\n")}
            </ul>

            <h2>Übermittlung in Länder außerhalb der EU</h2>
            <p>
            Der Workspace übermittelt keine Daten über dich in Länder außerhalb der Europäischen Union.
            Manche Paketquellen und GitHub sitzen in den USA; von dort holt der Server Vorlagen und Bibliotheken, ohne Daten über dich mitzuschicken.
            Was du selbst aufrufst oder hochlädst (siehe oben), entscheidest du.
            </p>

            <h2>Wie lange</h2>
            <p>#{deletion_rule_html(s)}</p>
            <p>
            Was kürzer gespeichert wird, steht in der Tabelle oben: Anmeldecodes #{login_minutes} Minuten, Sitzungen #{session_days} Tage,
            Zugriffsprotokolle 7 Tage. Deine Antworten zu Aufgaben und deine Abgaben löschst du selbst, wann du willst.
            </p>

            <h2>Musst du Daten angeben?</h2>
            <p>
            Für ein Konto braucht es deine E-Mail-Adresse und deinen Namen; beides trägt die Lehrkraft ein, die dich einlädt.
            Alles andere entsteht erst durch deine Arbeit im Workspace.
            </p>

            <h2>Keine automatisierten Entscheidungen</h2>
            <p>
            Die automatische Prüfung von Aufgaben sagt dir, ob deine Lösung schon passt. Sie ist eine Rückmeldung, keine Note,
            und entscheidet nichts über dich. Es gibt kein Profiling.
            </p>

            <h2>So sind deine Daten geschützt</h2>
            #{measures_html(s)}

            <h2>Deine Rechte</h2>
            <p>
            #{rights_html(s)}
            #{supervisory_html(s)}
            </p>

            <h2 id='nicht-einverstanden'>Wenn du nicht einverstanden bist</h2>
            <p>
            Schreib #{write_to_html(s)}, mit welcher E-Mail-Adresse du eingeladen wurdest und was du möchtest:
            eine Auskunft, eine Berichtigung, oder dass dein Konto gelöscht wird. Gelöscht wird dann alles, was oben steht:
            dein Workspace, deine Datenbanken, deine Antworten und Abgaben, deine Sitzungen und dein Konto.
            Deine Dateien kannst du vorher selbst sichern: im Explorer des Workspace mit Rechtsklick › »Herunterladen …«.
            Du bekommst eine Antwort, sobald es erledigt ist, spätestens innerhalb eines Monats.
            Im Unterricht bekommst du dann eine andere Möglichkeit, die Aufgaben zu bearbeiten.
            </p>

            <p class='text-body-secondary'>Stand: #{CHECKED.strftime('%d.%m.%Y')}</p>
        END_OF_HTML
        out << "</div>\n"
        out
    end
end
