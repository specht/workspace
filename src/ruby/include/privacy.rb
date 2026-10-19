require 'cgi'
require 'date'
require_relative 'authentication'

# The Impressum and the Datenschutzerklärung (/impressum) and the documents
# for the school (/datenschutz/…, privacy_documents.rb), all from the facts
# below and the settings in credentials.rb, so that nothing on one page
# contradicts another (docs/datenschutz.md). Plain Ruby, tested on its own:
# every function takes the settings as a Hash (Privacy.settings).
#
# The texts are formal and in the third person (no »du«): the readers who
# matter most are parents, the Schulleitung and the data protection officer.
# Each heading names the point of Art. 13/14 DSGVO it answers, so that a
# reviewer can tick them off.
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
        DATA_PROTECTION_OFFICER HOSTING MAIL_PROVIDER SCHOOL_LAW SUPERVISORY_AUTHORITY BACKUP COST_NOTE
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
        months = scope.const_defined?('BACKUP_RETENTION_MONTHS') ? scope.const_get('BACKUP_RETENTION_MONTHS').to_i : 0
        values[:backup_retention_months] = months > 0 ? months : nil
        values[:web_root] = scope.const_defined?('WEB_ROOT') ? scope.const_get('WEB_ROOT').to_s : nil
        values[:parental_consent_required] =
            scope.const_defined?('PARENTAL_CONSENT_REQUIRED') && scope.const_get('PARENTAL_CONSENT_REQUIRED') == true
        # The school that is responsible and whom to write to: the operator's
        # own school and the operator, unless for_school replaces them.
        values[:school_name] = values[:operator_school]
        values[:school_address] = values[:operator_address]
        values[:contacts] = [{ :name => values[:operator_name], :email => values[:contact_email] }]
        values
    end

    # The settings for the package of a partner school (/datenschutz?schule=KEY):
    # that school is the controller, with name, address, data protection
    # officer, law and authority from its School node (/personen); the operator
    # stays the processor; its teachers are the contact, and the one who is
    # logged in signs the Elternbrief. school: the node's properties, string
    # or symbol keys; teachers: [{ name:, email: }].
    def self.for_school(s, school, teachers: [], signer: nil)
        school = school.transform_keys(&:to_sym)
        return s if school[:home] || school[:guests]

        field = ->(key) { text = school[key].to_s.strip; text.empty? ? nil : text }
        teachers = teachers.map { |t| { :name => t[:name].to_s.strip.empty? ? nil : t[:name].strip, :email => t[:email] } }
        s.merge(
            :partner => true,
            :school_key => school[:key],
            :school_name => field.call(:name),
            :school_address => field.call(:address),
            :data_protection_officer => field.call(:data_protection_officer),
            :school_law => field.call(:school_law),
            :supervisory_authority => field.call(:supervisory_authority),
            :legal_basis => :school,
            :parental_consent_required => false,
            :contacts => teachers,
            :signer => signer || (teachers.map { |t| t[:name] }.compact.join(', ') if teachers.any? { |t| t[:name] }),
        )
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

    # Whom to write to: »Dr. Erika Muster (muster@…)«, as far as configured;
    # for a partner school its teachers.
    def self.contact_html(s)
        parts = (s[:contacts] || []).map do |c|
            name = c[:name] ? h(c[:name]) : nil
            link = c[:email] ? "<a href='mailto:#{h(c[:email])}'>#{h(c[:email])}</a>" : nil
            next "#{name} (#{link})" if name && link

            link || name
        end.compact
        parts.empty? ? 'die Lehrkraft, die zum Workspace eingeladen hat' : parts.join(', ')
    end

    # Who is responsible (Verantwortlicher): the school when it uses the
    # Workspace by a decision of the Schulleitung, otherwise the operator.
    def self.school_is_controller?(s)
        s[:legal_basis] == :school && !s[:school_name].nil?
    end

    def self.operator_line(s)
        parts = [s[:operator_name], s[:operator_role]].compact
        parts << "am #{s[:operator_school]}" if s[:operator_school]
        parts.empty? ? nil : h(parts.join(', ')).sub(', am ', ' am ')
    end

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
    ACCOUNT_END = 'bis zur Löschung des Kontos (siehe Speicherdauer)'

    # The categories of § 2 Abs. 5 DigLLV (Berlin), which also serve as the
    # categories of data in the VVT.
    CATEGORIES = {
        :master => 'Stammdaten',
        :account => 'Account-Daten',
        :assignment => 'Zuordnungsdaten',
        :content => 'nutzergenerierte Inhalte',
        :traffic => 'Verkehrs- und Verbindungsdaten',
    }.freeze

    # What is stored about a person: data, category, purpose, who sees it,
    # how long. »Lehrkräfte« are always those of the person's own groups.
    # How long a backup is kept, as the page says it.
    def self.backup_retention(s)
        months = s[:backup_retention_months]
        months ? "höchstens #{months} #{months == 1 ? 'Monat' : 'Monate'}" : 'wie lange, ist in dieser Installation noch nicht eingetragen'
    end

    # With s, the backups too, when the installation names them (BACKUP).
    def self.records(s = {})
        base_records + (s[:backup] ? [backup_record(s)] : [])
    end

    def self.backup_kept_sentence(s)
        s[:backup_retention_months] ? "Sicherungen werden #{backup_retention(s)} aufbewahrt." : 'Wie lange Sicherungen aufbewahrt werden, ist noch nicht eingetragen.'
    end

    def self.backup_record(s)
        {
            :data => 'Sicherungen: eine verschlüsselte Kopie aller Daten oben, auch gelöschter, bis die Sicherung abläuft',
            :categories => CATEGORIES.keys,
            :purpose => 'Wiederherstellung nach einem Ausfall',
            :who => 'Administration, nur für eine Wiederherstellung',
            :how_long => backup_retention(s),
        }
    end

    def self.base_records
        [
            {
                :data => 'Einladungslisten: E-Mail-Adresse, Name (freiwillig) und Gruppe aller eingeladenen Personen, auch derer, die sich nie anmelden; ' \
                    'gegebenenfalls das Datum eines schriftlichen Einverständnisses und wer es vermerkt hat',
                :source => 'von der Lehrkraft eingetragen',
                :categories => [:master, :assignment],
                :purpose => 'festlegen, wer sich anmelden kann',
                :who => 'Lehrkräfte der Gruppe, Administration',
                :how_long => 'bis die Lehrkraft die Person oder die Gruppe aus der Liste nimmt',
            },
            {
                :data => 'Konto: E-Mail-Adresse, Name (freiwillig) und Gruppe (Klasse oder Kurs); entsteht, wenn die Person zum ersten Mal einen Anmeldecode anfordert',
                :source => 'aus der Einladungsliste der Lehrkraft',
                :categories => [:master, :assignment],
                :purpose => 'Anmeldung; Zuordnung zur Lerngruppe',
                :who => 'die Person selbst, Lehrkräfte der Gruppe, Administration',
                :how_long => ACCOUNT_END,
            },
            {
                :data => 'Protokoll der Zugriffe der Administration: wann jemand aus der Administration welchen Workspace geöffnet oder welches Konto nachgeahmt hat, und mit welchem Grund',
                :categories => [:traffic],
                :purpose => 'Nachvollziehbarkeit von Zugriffen der Administration',
                :who => 'Administration',
                :how_long => 'ein Jahr',
            },
            {
                :data => 'Gegebenenfalls eine Sperre des Zugangs: seit wann, wer sie eingetragen hat und warum (etwa ein Widerspruch, der geprüft wird)',
                :categories => [:account],
                :purpose => 'den Zugang anhalten, ohne Daten zu löschen',
                :who => 'Administration',
                :how_long => 'bis die Sperre aufgehoben oder das Konto gelöscht wird',
            },
            {
                :data => 'Bei Gästen: wann sie der Nutzungsordnung zugestimmt haben, und welcher Fassung',
                :categories => [:account],
                :purpose => 'Nachweis der Vereinbarung, auf der ihr Konto beruht',
                :who => 'Administration',
                :how_long => ACCOUNT_END,
            },
            {
                :data => 'Tag des letzten Besuchs (nur das Datum, ohne Uhrzeit; nicht, während jemand anderes in das Konto sieht)',
                :categories => [:traffic],
                :purpose => 'erkennen, welche Konten nicht mehr genutzt werden, damit sie gelöscht werden können',
                :who => 'Administration',
                :how_long => 'wird bei jedem Besuch überschrieben, bis zur Löschung des Kontos (siehe Speicherdauer)',
            },
            {
                :data => 'Anmeldeanfragen: Code und eine zufällige Kennung des Browsers',
                :categories => [:account],
                :purpose => 'Anmeldung',
                :who => 'die Person selbst (Code per E-Mail); Lehrkräfte der Gruppe, solange die Anfrage offen ist',
                :how_long => "#{login_minutes} Minuten (Anmeldung per QR-Code: #{qr_minutes} Minuten) oder bis zur Anmeldung",
            },
            {
                :data => 'Sitzungen: eine zufällige Kennung je angemeldetem Browser',
                :categories => [:account],
                :purpose => 'angemeldet bleiben',
                :who => 'niemand',
                :how_long => "#{session_days} Tage oder bis zur Abmeldung",
            },
            {
                :data => 'Einstellungen: Helligkeit, Programmiersprache, Sprache der Seiten',
                :categories => [:account],
                :purpose => 'Darstellung',
                :who => 'die Person selbst',
                :how_long => ACCOUNT_END,
            },
            {
                :data => 'Workspace: eigene Dateien und Programme, Verlauf im Terminal, installierte Pakete und Erweiterungen',
                :categories => [:content],
                :purpose => 'Programmieren im Unterricht und zu Hause',
                :who => 'die Person selbst; Lehrkräfte der Gruppe, wenn sie hineinsehen; Administration',
                :how_long => ACCOUNT_END,
            },
            {
                :data => 'Eigene Datenbanken (MySQL, Neo4j) mit ihrem Inhalt',
                :categories => [:content],
                :purpose => 'Unterricht zu Datenbanken',
                :who => 'die Person selbst; Administration',
                :how_long => ACCOUNT_END,
            },
            {
                :data => 'Aufgaben: letzte Antwort, Ergebnis der automatischen Prüfung, Zahl und Zeitpunkte der Versuche',
                :categories => [:content],
                :purpose => 'Rückmeldung an die Person; Überblick für die Lehrkraft',
                :who => 'die Person selbst, Lehrkräfte der Gruppe',
                :how_long => 'bis die Person sie selbst löscht, sonst ' + ACCOUNT_END,
            },
            {
                :data => 'Abgaben: eine Kopie der Dateien, die bei einer Aufgabe abgegeben werden',
                :categories => [:content],
                :purpose => 'Abgabe an die Lehrkraft',
                :who => 'die Person selbst, Lehrkräfte der Gruppe',
                :how_long => 'bis die Person sie löscht oder neu abgibt, sonst ' + ACCOUNT_END,
            },
            {
                :data => 'Leistungsüberprüfungen: ein eigener Workspace und eigene Datenbanken für die Dauer einer Leistungsüberprüfung, mit allem, was darin entsteht',
                :categories => [:content],
                :purpose => 'Leistungsüberprüfung im Unterricht; die Lehrkraft lädt die Ergebnisse zur Korrektur als PDF herunter',
                :who => 'die Person selbst während der Überprüfung; die Lehrkraft, der die Überprüfung gehört',
                :how_long => 'bis die Lehrkraft die Leistungsüberprüfung löscht, sonst ' + ACCOUNT_END,
            },
            {
                :data => 'Hinweise, wer in Workspace oder Konto gesehen hat: wer, wann, mit welcher Begründung',
                :categories => [:traffic],
                :purpose => 'Transparenz gegenüber der Person',
                :who => 'die Person selbst; Administration',
                :how_long => ACCOUNT_END,
            },
            {
                :data => 'Links zum Teilen des eigenen Workspace oder einer laufenden Web-App',
                :categories => [:account],
                :purpose => 'eigene Arbeit zeigen; Hilfe bekommen',
                :who => 'wer den Link hat (siehe »Teilen«)',
                :how_long => 'Link zum Workspace eine Woche (verlängerbar), sonst bis die Person den Link abschaltet',
            },
            {
                :data => 'Protokolle des Servers: welche Seite wann mit welchem Ergebnis abgerufen wurde, ohne IP-Adresse und ohne Namen; Meldungen der Programme, in denen bei manchen Vorgängen (Anmeldung, Start eines Workspace) die E-Mail-Adresse steht',
                :categories => [:traffic],
                :purpose => 'Betrieb, Sicherheit und Fehlersuche',
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
        'github.com' => 'GitHub',
    }.freeze

    # Services outside the server. kind: :processor (processes data for the
    # controller: Empfänger), :server (what the server or a Workspace
    # fetches, with no data about the person), :own (what a person's own
    # program, page or choice contacts).
    def self.services(s)
        hosting = s[:hosting] ? h(s[:hosting]) : 'einem Rechenzentrum'
        mail = s[:mail_provider] ? " (#{h(s[:mail_provider])})" : ''
        [
            {
                :name => 'Rechenzentrum',
                :kind => :processor,
                :html => "Der Server läuft bei #{hosting}. Dort liegen alle oben genannten Daten; das Rechenzentrum stellt die Technik bereit." +
                    (s[:backup] ? " Verschlüsselte Sicherungen liegen #{h(s[:backup])}; der Schlüssel liegt nur beim Betreiber, das Rechenzentrum kann sie nicht lesen." : ''),
            },
            {
                :name => 'E-Mail',
                :kind => :processor,
                :html => "Anmeldecodes und Einladungen verschickt ein E-Mail-Dienst#{mail}. Er sieht die Adresse, die Uhrzeit und den Code; der Code allein ermöglicht keine Anmeldung (siehe »Anmeldung per Code«).",
            },
            {
                :name => 'DNS',
                :kind => :server,
                :html => 'Die Namen der Seite (DNS) verwaltet Cloudflare. Über Cloudflare laufen keine Verbindungen: Die Namen zeigen direkt auf den Server, und Cloudflare sieht nur Anfragen von DNS-Servern, keine Inhalte und keine Daten über Personen.',
            },
            {
                :name => 'Paketquellen und GitHub',
                :kind => :server,
                :html => 'Vorlagen und Programmbibliotheken holt der Workspace von GitHub und den üblichen Paketquellen (RubyGems, PyPI, npm und andere), wenn eine Aktivität es vorsieht. Diese Dienste sehen die Adresse des Servers, keine Daten über Personen.',
            },
            {
                :name => 'Open-Meteo',
                :kind => :server,
                :html => 'Für die Aktivität »Daten aus dem Netz« gibt es auf dem Server einen Spiegel des Wetterdienstes Open-Meteo. Fragt ein Programm dort, sieht Open-Meteo nur den Server.',
            },
            {
                :name => 'Open VSX',
                :kind => :own,
                :html => 'Wer im Editor nach einer Erweiterung sucht oder eine installiert, ruft sie bei Open VSX (Eclipse Foundation) ab.',
            },
            {
                :name => 'Webhosting',
                :kind => :own,
                :html => 'Wer eine fertige Website dauerhaft veröffentlichen möchte, kann eine eigene Subdomain von hackschule.de bekommen. Das ist ein eigenes Angebot mit eigener Nutzungsvereinbarung; die Website ist öffentlich.',
            },
            {
                :name => 'Eigene Programme',
                :kind => :own,
                :html => 'Was eigene Programme und Webseiten abrufen (eine Wetter-API, ein Bild aus Wikipedia, eine Schrift von Google Fonts), bestimmt die Person, die sie schreibt. Öffnet sie eine solche Seite im Browser, sieht der fremde Dienst die Adresse ihres Internetanschlusses, wie bei jeder anderen Webseite.',
            },
        ]
    end

    # Technical and organisational measures in place (TOM, Art. 32 DSGVO).
    # Each one is true of the code as it is.
    def self.measures(s)
        {
            'Zugangskontrolle' => [
                'Anmelden kann sich nur, wer von einer Lehrkraft eingeladen wurde.',
                "Anmeldung mit einem Code per E-Mail, der nur zusammen mit dem Browser gilt, der ihn angefordert hat; der Code gilt #{login_minutes} Minuten und höchstens 5 Versuche lang. Es gibt keine Passwörter, die erraten oder wiederverwendet werden könnten.",
                'Anfragen zur Anmeldung sind je E-Mail-Adresse und je Internetanschluss begrenzt; die Zählung steht nur im Arbeitsspeicher und wird nirgends gespeichert.',
                'Sitzungs-Cookies sind HttpOnly, Secure und SameSite=Lax.',
            ],
            'Trennung und Berechtigungen' => [
                'Jede Person hat einen eigenen Workspace in einem eigenen Container und eigene Datenbanken mit eigenen Zugangsdaten.',
                'Die Workspaces erreichen einander im Netz nicht; eine Firewall lässt nur einen Port frei, den die Aktivitäten zu Netzwerken brauchen.',
                'Lehrkräfte sehen nur die Schülerinnen und Schüler ihrer eigenen Gruppen und nehmen nur diese in eine Leistungsüberprüfung auf; die Konten anderer Lehrkräfte sehen sie nie.',
            ],
            'Transparenz' => [
                'Öffnet eine Lehrkraft einen Workspace oder sieht die Seite so, wie die Schülerin oder der Schüler sie sieht, gibt sie eine Begründung an (5 bis 300 Zeichen); die Person sieht beim nächsten Besuch, wer, wann und warum.',
                'Noten werden im Workspace nicht gespeichert; die automatische Prüfung von Aufgaben ist eine Rückmeldung, keine Bewertung.',
                'Der Quelltext ist öffentlich; jede Angabe hier lässt sich am Code prüfen.',
            ],
            'Datensparsamkeit' => [
                'Kein Tracking, keine Analyse-Werkzeuge, keine Werbung.',
                'Schriften, Skripte und Symbole kommen vom Server selbst; die Seiten des Workspace laden nichts von fremden Servern.',
                'Die Zugriffsprotokolle enthalten keine IP-Adressen und keine Namen; geheime Teile von Adressen (Links zum Teilen, Anmeldelinks) werden unkenntlich gemacht. Sie werden nach 7 Tagen gelöscht.',
                'Von Aufgaben wird nur die letzte Antwort gespeichert; Versuche werden ohne Text gezählt.',
            ],
            'Verschlüsselung' => [
                'Alle Verbindungen zur Seite und zu den Workspaces sind verschlüsselt (HTTPS mit Zertifikat von Let’s Encrypt).',
            ] + (s[:backup] ? ["Sicherungen sind verschlüsselt, bevor sie den Server verlassen (#{s[:backup]}); den Schlüssel hat nur der Betreiber."] : []),
            'Löschung' => [
                'Schülerinnen und Schüler löschen ihre Antworten und Abgaben selbst, jederzeit, und ebenso ihr ganzes Konto.',
                'Alle Daten eines Kontos lassen sich im Profil als ein Archiv herunterladen.',
                'Die Administration löscht ein Konto mit allem, was dazugehört (Workspaces, Datenbanken, Aufgaben, Abgaben, Sitzungen), in einem Schritt, und sieht jederzeit, welche Konten keine Einladung mehr haben.',
                'Mit einer Leistungsüberprüfung löscht die Lehrkraft auch die Workspaces und Datenbanken aller, die sie geschrieben haben.',
            ] + (s[:backup] ? ["#{backup_kept_sentence(s)} Jede Löschung wird ohne Adresse vermerkt (ein Hashwert und das Datum); nach dem Zurückspielen einer Sicherung werden alle seither gelöschten Konten erneut gelöscht."] : []) + [
            ],
        }
    end

    def self.deletion_rule_html(s)
        'Ein Konto wird mit allen zugehörigen Daten gelöscht, sobald es nicht mehr gebraucht wird: ' \
            'spätestens zum Ende des Schuljahres, in dem der Kurs endet oder die Schülerin oder der Schüler die Schule verlässt, ' \
            'und jederzeit früher auf Wunsch: im Profil unter »Meine Daten« oder per E-Mail an ' + contact_html(s) + '. ' \
            'Kündigt die Administration eine Löschung per E-Mail an, wird das Konto zwei Wochen später gelöscht; bis dahin lassen sich alle Daten herunterladen. ' \
            'Wer die Schule verlässt und das Konto behalten möchte, kann es auf eigenen Wunsch weiterführen; es gilt dann die Nutzungsordnung ' \
            '(bei Minderjährigen mit Unterschrift der Erziehungsberechtigten), und verantwortlich ist nicht mehr die Schule, sondern der Betreiber.'
    end

    def self.legal_basis_html(s)
        case s[:legal_basis]
        when :school
            law = s[:school_law] ? h(s[:school_law]) : 'dem Schulgesetz des Landes'
            text = '<p>Die Schule setzt den Workspace als digitales Lehr- und Lernmittel im Informatikunterricht ein; die Schulleitung hat das schriftlich entschieden. ' \
                "Rechtsgrundlage ist Art. 6 Abs. 1 Buchst. e DSGVO in Verbindung mit #{law}. " \
                'Eine Einwilligung ist dafür nicht erforderlich; die Verarbeitung ist auf das beschränkt, was der Unterricht braucht.</p>'
            if s[:parental_consent_required]
                text += '<p>Zusätzlich holt die Lehrkraft das schriftliche Einverständnis der Erziehungsberechtigten ein (ab 16 Jahren das der Schülerinnen und Schüler selbst) und vermerkt es im Workspace.</p>'
            end
            text + others_html
        when :consent
            '<p>Rechtsgrundlage ist die Einwilligung der Person (Art. 6 Abs. 1 Buchst. a DSGVO), bei Personen unter 16 Jahren die der Erziehungsberechtigten (Art. 8 DSGVO). ' \
                'Ein Konto wird erst freigeschaltet, wenn die Einwilligung vorliegt. ' \
                'Die Teilnahme ist freiwillig: Wer nicht teilnimmt, hat dadurch keinen Nachteil und erhält eine andere Möglichkeit, die Aufgaben zu bearbeiten. ' \
                "Die Einwilligung kann jederzeit mit Wirkung für die Zukunft widerrufen werden, per E-Mail an #{contact_html(s)}; das Konto wird dann gelöscht.</p>"
        else
            "<p>Die Rechtsgrundlage ist für diese Installation noch nicht eingetragen. Auskunft gibt #{contact_html(s)}.</p>"
        end
    end

    # Who is not covered by the school's decision: adults from elsewhere and
    # the classes of other schools.
    def self.others_html
        '<p>Für Erwachsene von außerhalb der Schule (Gäste, etwa Lehrkräfte anderer Schulen, die den Workspace kennenlernen) und für ehemalige Schülerinnen und Schüler, ' \
            'die ihr Konto behalten, ist Rechtsgrundlage die Nutzungsordnung, mit der sie das Konto erhalten oder weiterführen ' \
            '(Art. 6 Abs. 1 Buchst. b DSGVO); sie stimmen ihr bei der ersten Anmeldung zu, und verantwortlich ist dafür der Betreiber, nicht die Schule. ' \
            'Gäste sind Erwachsene; Minderjährige nehmen nur über ihre Schule teil. ' \
            'Schülerinnen und Schüler einer anderen Schule nutzen den Workspace nur, wenn ihre Schule das ebenso entschieden hat; ' \
            'verantwortlich ist dann ihre Schule.</p>'
    end

    def self.controller_html(s)
        if school_is_controller?(s)
            address = s[:school_address] ? "<br>#{lines_html(s[:school_address])}" : ''
            out = "<p>Verantwortlich ist die Schule: <strong>#{h(s[:school_name])}</strong>, vertreten durch die Schulleitung#{address}</p>"
            if s[:partner]
                operator_address = s[:operator_address] ? ", #{h(s[:operator_address].split("\n").map(&:strip).reject(&:empty?).join(', '))}" : ''
                out += "<p>Den Workspace betreibt für die Schule als Auftragsverarbeiter (Art. 28 DSGVO) #{operator_line(s) || 'eine Lehrkraft einer anderen Schule'}#{operator_address}. " \
                    "Ansprechperson an der Schule für alle Fragen zum Datenschutz: #{contact_html(s)}.</p>"
            elsif s[:operator_name]
                out += "<p>Den Workspace betreibt im Auftrag der Schule #{operator_line(s)}. " \
                    "Ansprechperson für alle Fragen zum Datenschutz: #{contact_html(s)}.</p>"
            end
        else
            address = s[:operator_address] ? "<br>#{lines_html(s[:operator_address])}" : ''
            who = operator_line(s) || 'die Lehrkraft, die den Workspace betreibt'
            out = "<p>Verantwortlich ist #{who}#{address}</p>"
            out += "<p>E-Mail: #{email_link(s)}</p>" if s[:contact_email]
        end
        if s[:data_protection_officer]
            out += "<p>Datenschutzbeauftragte Stelle der Schule:<br>#{lines_html(s[:data_protection_officer])}</p>"
        end
        out
    end

    # Art. 20 (portability) holds only for consent and contract; Art. 21
    # (objection) only for a public task.
    def self.rights_html(s)
        out = 'Betroffene Personen, bei Minderjährigen auch ihre Erziehungsberechtigten, haben das Recht auf Auskunft über die gespeicherten Daten (Art. 15 DSGVO), ' \
            'auf Berichtigung (Art. 16), auf Löschung (Art. 17) und auf Einschränkung der Verarbeitung (Art. 18). '
        if s[:legal_basis] == :consent
            out + 'Sie haben das Recht auf Datenübertragbarkeit (Art. 20) und können die Einwilligung jederzeit widerrufen (Art. 7 Abs. 3).'
        else
            out + 'Sie können der Verarbeitung widersprechen (Art. 21).'
        end
    end

    def self.supervisory_html(s)
        if s[:supervisory_authority]
            "Beschwerden sind bei der Aufsichtsbehörde möglich:<br>#{lines_html(s[:supervisory_authority])}"
        else
            'Beschwerden sind bei der Datenschutz-Aufsichtsbehörde des Bundeslandes möglich.'
        end
    end

    def self.categories_text(record)
        record[:categories].map { |c| CATEGORIES[c] }.join(', ')
    end

    def self.records_table_html(rows = records)
        out = +"<table class='table privacy-table'>\n<thead><tr><th>Daten</th><th>Zweck</th><th>Wer sie sieht</th><th>Speicherdauer</th></tr></thead>\n<tbody>\n"
        rows.each do |r|
            source = r[:source] ? "<br><span class='text-body-secondary'>#{h(r[:source])}</span>" : ''
            out << "<tr><td>#{h(r[:data])}#{source}<br><span class='text-body-secondary'>Kategorie: #{h(categories_text(r))}</span></td>" \
                "<td>#{h(r[:purpose])}</td><td>#{h(r[:who])}</td><td>#{h(r[:how_long])}</td></tr>\n"
        end
        out << "</tbody>\n</table>\n"
        out
    end

    def self.measures_html(s)
        measures(s).map do |heading, items|
            "<h4>#{h(heading)}</h4>\n<ul>\n" + items.map { |m| "<li>#{h(m)}</li>" }.join("\n") + "\n</ul>"
        end.join("\n")
    end

    # A heading in plain words, with the term and article a reviewer looks
    # for underneath.
    def self.heading(title, ref, id: nil)
        id_attr = id ? " id='#{id}'" : ''
        "<h2#{id_attr}>#{h(title)}<span class='privacy-ref'>#{h(ref)}</span></h2>"
    end

    def self.impressum_html(s)
        address = s[:operator_address] ? "<br>#{lines_html(s[:operator_address])}" : ''
        operator = operator_line(s)
        out = +"<h1>Impressum</h1>\n"
        out << "<p>#{operator ? "<strong>#{operator}</strong>" : 'Die Angaben zur Person, die den Workspace betreibt, sind in dieser Installation noch nicht eingetragen.'}#{address}</p>\n"
        out << "<p>E-Mail: #{email_link(s)}</p>\n" if s[:contact_email]
        out << '<p>Der Workspace ist ein nicht-kommerzielles Angebot für den Informatikunterricht, betrieben von einer Lehrkraft. ' \
            "Es gibt keine Werbung, keine kostenpflichtigen Funktionen und keine Auswertung der Daten für andere Zwecke.</p>\n"
        out << "<p>Quelltext: <a href='https://github.com/specht/workspace'>https://github.com/specht/workspace</a></p>\n"
        out
    end

    # The schools whose Schulleitung decided the same: each has its own
    # version of this page. partners: [{ key:, name: }], approved only.
    # Partner schools are not named on the public page; their students get
    # their school's version of this notice as a PDF with the invitation.
    def self.partners_html(partners)
        return '' if partners.nil? || partners.empty?

        '<p>Auch andere Schulen setzen den Workspace ein. Für ihre Schülerinnen und Schüler ist die jeweilige Schule verantwortlich, ' \
            'und es gilt deren Fassung dieser Erklärung; sie kommt mit der Einladung als PDF.</p>'
    end

    # The whole /impressum page body; partners: the approved partner schools
    # (Main#partner_schools), listed under the legal basis.
    def self.page_html(s, partners: [])
        out = +"<div class='privacy-page'>\n"
        out << impressum_html(s)
        out << <<~END_OF_HTML
            <h1 id='datenschutz'>Datenschutzerklärung</h1>
            <p>
            Diese Erklärung beschreibt, welche personenbezogenen Daten der Workspace verarbeitet, zu welchem Zweck,
            wer sie sieht und wann sie gelöscht werden. Sie folgt Art. 13 und 14 DSGVO; unter jeder Überschrift steht,
            welche Angabe sie enthält. Der Workspace ist eine Programmierumgebung für den Unterricht: Was dort entsteht,
            sind vor allem Programme, Dateien und Übungen.
            </p>

            #{heading('Wer verantwortlich ist', 'Verantwortlicher und Kontakt · Art. 13 Abs. 1 Buchst. a, b DSGVO')}
            #{controller_html(s)}

            #{heading('Wozu die Daten verarbeitet werden', 'Zwecke · Art. 13 Abs. 1 Buchst. c DSGVO')}
            <p>
            Für den Unterricht, vor allem im Fach Informatik: Schülerinnen und Schüler programmieren in ihrem eigenen Workspace, bearbeiten Aufgaben,
            lassen sie automatisch prüfen und geben Ergebnisse ab; die Lehrkraft sieht den Stand der Lerngruppe und hilft bei Problemen.
            Die Daten werden für keinen anderen Zweck verarbeitet: nicht für Noten, nicht für Werbung, nicht für Profile,
            nicht für Forschung, und sie werden an niemanden für eigene Zwecke weitergegeben.
            </p>

            #{heading('Rechtsgrundlage', 'Art. 13 Abs. 1 Buchst. c DSGVO')}
            #{legal_basis_html(s)}
            #{partners_html(partners)}

            #{heading('Welche Daten gespeichert werden', 'Kategorien, Herkunft, Empfänger und Speicherdauer · Art. 13 Abs. 1 Buchst. e, Abs. 2 Buchst. a; Art. 14 Abs. 1 Buchst. d, Abs. 2 Buchst. f DSGVO')}
            #{records_table_html(records(s))}
            <p>
            Alle diese Daten liegen auf dem Server des Workspace. E-Mail-Adresse, Gruppe und, wenn gewünscht, den Namen trägt die
            Lehrkraft aus ihrer Einladungsliste ein; alles andere entsteht durch die Arbeit im Workspace.
            </p>

            #{heading('Anmeldung per Code', 'Sicherheit der Anmeldung · Art. 32 DSGVO')}
            <p>
            Bei der Anmeldung merkt sich der Browser eine zufällige Kennung, und ein Code wird per E-Mail verschickt.
            Angemeldet wird nur, wer beides hat: den Code und den Browser, der ihn angefordert hat. Der E-Mail-Dienst sieht den Code,
            mit dem Code allein ist aber keine Anmeldung möglich. Der Code gilt #{login_minutes} Minuten und höchstens 5 Versuche lang.
            Damit sich Schülerinnen und Schüler im Unterricht auch ohne Zugriff auf ihre E-Mails anmelden können, sieht die Lehrkraft
            die offenen Codes ihrer Gruppe auf ihrer Klassenseite; anmelden kann sie sich damit selbst nicht.
            </p>

            #{heading('Was Lehrkräfte sehen', 'Empfänger innerhalb der Schule · Art. 13 Abs. 1 Buchst. e DSGVO')}
            <p>
            Lehrkräfte einer Gruppe sehen in der Klassenübersicht, welche Aufgaben ihre Schülerinnen und Schüler bearbeitet haben
            und mit welchem Ergebnis, und deren Abgaben. Um zu helfen, können sie den Workspace einer Schülerin oder eines Schülers öffnen
            oder die Seite so sehen, wie diese Person sie sieht. Dafür geben sie eine kurze Begründung an, und die Person sieht beim
            nächsten Besuch einen Hinweis, zum Beispiel: »Frau Berger hat sich am 14.10. um 10:12 deinen Workspace angesehen:
            Ich sehe mir deine Abgabe an.« Im Profil stehen die letzten dieser Hinweise.
            </p>
            <p>
            Die Administration des Servers kann, wie bei jedem Server, alle Daten darauf einsehen; sie tut das nur für den Betrieb
            und um bei Problemen zu helfen. Öffnet sie einen Workspace oder sieht sie die Seite als eine Person, gibt sie wie eine
            Lehrkraft einen Grund an, und die Person sieht den Hinweis; zusätzlich wird der Zugriff intern protokolliert.
            </p>

            #{heading('Teilen', 'Weitergabe durch die Person selbst')}
            <p>
            Wer den eigenen Workspace teilt, erhält einen Link, mit dem jede Person, die ihn hat, diesen Workspace öffnen kann,
            auch ohne Anmeldung. Eine Web-App, die im Workspace läuft, lässt sich ebenfalls teilen; diesen Link öffnen nur Personen,
            die im Workspace angemeldet sind. Der Link zum Workspace gilt eine Woche (erneutes Teilen verlängert ihn) oder bis er abgeschaltet wird, der zur Web-App bis zum Abschalten. Die Seite weist beim Teilen darauf hin.
            </p>

            #{heading('Cookies und Speicher im Browser', 'Endgeräte · § 25 TDDDG')}
            <p>
            Vor der Anmeldung setzt die Seite keine Cookies. Nach der Anmeldung setzt sie nur technisch notwendige: einen für die Sitzung
            und einen, mit dem der Browser den eigenen Workspace öffnen darf (für eine Lehrkraft, die in einen Workspace sieht, einen dritten).
            Daneben speichert der Browser einige Einstellungen der Seite (lokaler Speicher), etwa ob ein Hinweis ausgeblendet ist,
            und der Editor speichert dort, welche Dateien geöffnet waren. Diese Angaben verlassen den Browser nicht.
            </p>

            #{heading('Dienste außerhalb des Servers', 'Empfänger · Art. 13 Abs. 1 Buchst. e DSGVO')}
            <p>
            Die Seiten des Workspace laden nichts von fremden Servern: Schriften, Skripte und Symbole kommen vom Server selbst.
            Beteiligt sind:
            </p>
            <ul>
            #{services(s).map { |svc| "<li>#{svc[:html]}</li>" }.join("\n")}
            </ul>

            #{heading('Übermittlung in Länder außerhalb der EU', 'Drittländer · Art. 13 Abs. 1 Buchst. f DSGVO')}
            <p>
            Der Workspace übermittelt keine personenbezogenen Daten in Länder außerhalb der Europäischen Union.
            Manche Paketquellen und GitHub haben ihren Sitz in den USA; von dort holt der Server Vorlagen und Bibliotheken,
            ohne Daten über Personen mitzuschicken. Was eine Person selbst aufruft oder hochlädt (siehe oben), entscheidet sie selbst.
            </p>

            #{heading('Speicherdauer', 'Art. 13 Abs. 2 Buchst. a DSGVO')}
            <p>#{deletion_rule_html(s)}</p>
            <p>
            Kürzer gespeichert werden: Anmeldecodes #{login_minutes} Minuten, Sitzungen #{session_days} Tage, Zugriffsprotokolle 7 Tage.
            #{s[:backup] ? "Sicherungen werden nicht nachträglich verändert: Gelöschte Daten verschwinden aus ihnen, wenn die Sicherung abläuft (#{backup_retention(s)}). Wird eine Sicherung zurückgespielt, werden alle seither gelöschten Konten sofort erneut gelöscht." : ''}
            Antworten zu Aufgaben und Abgaben löschen Schülerinnen und Schüler selbst, wann sie wollen.
            </p>

            #{heading('Pflicht zur Angabe von Daten', 'Art. 13 Abs. 2 Buchst. e DSGVO')}
            <p>
            Erforderlich ist nur eine E-Mail-Adresse, an die Anmeldecodes gehen können; sie muss den Namen nicht enthalten.
            Ein Name ist freiwillig; ohne ihn zeigt der Workspace die Adresse. Beides trägt die einladende Lehrkraft ein.
            Weitere Angaben werden nicht verlangt.
            </p>

            #{heading('Keine automatisierten Entscheidungen', 'Art. 13 Abs. 2 Buchst. f, Art. 22 DSGVO')}
            <p>
            Die automatische Prüfung von Aufgaben zeigt an, ob eine Lösung die Anforderungen erfüllt. Sie ist eine Rückmeldung,
            keine Note, und hat keine rechtliche oder ähnlich erhebliche Wirkung. Es findet kein Profiling statt.
            </p>

            #{heading('Schutzmaßnahmen', 'Technische und organisatorische Maßnahmen · Art. 25, 32 DSGVO')}
            #{measures_html(s)}

            #{heading('Rechte der betroffenen Personen', 'Art. 13 Abs. 2 Buchst. b, c, d DSGVO')}
            <p>
            #{rights_html(s)}
            #{supervisory_html(s)}
            </p>

            #{heading('Wer nicht einverstanden ist', 'Ausübung der Rechte · Art. 12 DSGVO', :id => 'nicht-einverstanden')}
            <p>
            Im Profil unter »Meine Daten« lässt sich alles, was zum Konto gespeichert ist, als Archiv herunterladen
            (Workspace, Abgaben, MySQL- und Neo4j-Datenbanken, Kontodaten), und das Konto lässt sich dort selbst löschen.
            Ebenso genügt eine E-Mail an #{contact_html(s)}, mit der Adresse, mit der die Person eingeladen wurde, und dem Anliegen:
            Auskunft, Berichtigung, Widerspruch oder Löschung des Kontos. Bei einer Löschung wird alles oben Genannte gelöscht:
            Workspaces, Datenbanken, Antworten und Abgaben, Sitzungen und das Konto. Die Antwort kommt, sobald das Anliegen erledigt ist,
            spätestens innerhalb eines Monats. Im Unterricht erhalten Schülerinnen und Schüler dann eine andere Möglichkeit,
            die Aufgaben zu bearbeiten: mit ausgedruckten Unterlagen, auf Papier oder am eigenen Rechner.
            </p>

            <p class='text-body-secondary'>Stand: #{CHECKED.strftime('%d.%m.%Y')}</p>
        END_OF_HTML
        out << "</div>\n"
        out
    end
end
