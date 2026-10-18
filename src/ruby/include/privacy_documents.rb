require_relative 'privacy'

# The documents for the school (/datenschutz): what the Schulleitung,
# the data protection officer and the parents get, so that agreeing is one
# decision and nothing has to be examined from scratch. Built from the same
# facts as the Datenschutzerklärung (Privacy), so none contradicts another.
# Formal, in the third person or Sie-Form, never »du«.
module PrivacyDocuments
    # slug => [title, who it is for]
    DOCUMENTS = {
        'schulleitung' => ['Der Workspace im Informatikunterricht: Information und Entscheidung', 'für die Schulleitung'],
        'pruefuebersicht' => ['Prüfübersicht Datenschutz', 'für die Schulleitung und die Datenschutzbeauftragte Stelle'],
        'datenschutzerklaerung' => ['Datenschutzerklärung', 'für Schülerinnen und Schüler, Eltern und Lehrkräfte'],
        'vvt' => ['Eintrag für das Verzeichnis von Verarbeitungstätigkeiten', 'für die Akten der Schule (Art. 30 DSGVO)'],
        'tom' => ['Technische und organisatorische Maßnahmen', 'Anlage zum Verzeichnis und zur Vereinbarung (Art. 32 DSGVO)'],
        'loeschkonzept' => ['Löschkonzept', 'für die Schule und die Lehrkräfte'],
        'auftragsverarbeitung' => ['Vereinbarung zur Auftragsverarbeitung (Entwurf)', 'zwischen der Schule und der Person, die den Workspace betreibt (Art. 28 DSGVO)'],
        'elternbrief' => ['Elternbrief', 'für die Erziehungsberechtigten'],
        'nutzungsordnung' => ['Nutzungsordnung', 'für alle, die ein Konto haben'],
        'webhosting' => ['Nutzungsvereinbarung zum Webhosting', 'für eine eigene, öffentliche Website auf einer Subdomain'],
    }.freeze

    def self.h(text)
        Privacy.h(text)
    end

    # The school that is responsible: the operator's own, or a partner
    # school (Privacy.for_school).
    def self.school(s)
        s[:school_name] ? h(s[:school_name]) : 'die Schule'
    end

    # The links between the documents keep the school (?schule=KEY).
    def self.path(s, slug = nil)
        base = slug ? "/datenschutz/#{slug}" : '/datenschutz'
        s[:school_key] ? "#{base}?schule=#{h(CGI.escape(s[:school_key].to_s))}" : base
    end

    # Where the Datenschutzerklärung of this school is, as a printed address.
    def self.privacy_url(s)
        "#{h(s[:web_root].to_s)}#{s[:partner] ? path(s, 'datenschutzerklaerung') : '/impressum'}"
    end

    # Who signs the Elternbrief: the teacher of the partner school, otherwise
    # the operator.
    def self.signer(s)
        s[:signer] ? h(s[:signer]) : operator_name(s)
    end

    def self.operator(s)
        Privacy.operator_line(s) || 'die Lehrkraft, die den Workspace betreibt'
    end

    def self.operator_name(s)
        s[:operator_name] ? h(s[:operator_name]) : 'die Lehrkraft, die den Workspace betreibt'
    end

    def self.hosting(s)
        s[:hosting] ? h(s[:hosting]) : 'einem Rechenzentrum (in den Einstellungen noch nicht eingetragen)'
    end

    def self.digllv?(s)
        s[:school_law].to_s.include?('DigLLV')
    end

    def self.list(items)
        "<ul>\n" + items.map { |item| "<li>#{item}</li>" }.join("\n") + "\n</ul>\n"
    end

    def self.table(head, rows)
        out = +"<table>\n<thead><tr>#{head.map { |c| "<th>#{c}</th>" }.join}</tr></thead>\n<tbody>\n"
        rows.each { |row| out << "<tr>#{row.map { |c| "<td>#{c}</td>" }.join}</tr>\n" }
        out << "</tbody>\n</table>\n"
        out
    end

    def self.signature(*labels)
        "<div class='signatures'>" + labels.map { |label| "<div class='signature'><div class='line'></div>#{label}</div>" }.join + "</div>\n"
    end

    def self.body(slug, s)
        send("#{slug}_html", s)
    end

    # --- Eine Seite für die Schulleitung --------------------------------

    def self.schulleitung_html(s)
        <<~END_OF_HTML
            <p>
            Der Workspace ist eine Programmierumgebung im Browser für den Informatikunterricht. Jede Schülerin und jeder Schüler
            erhält einen eigenen Editor (Visual Studio Code) mit Terminal und eigenen Datenbanken, dazu Aktivitäten, ein Handbuch
            und Aufgaben, die der Workspace selbst prüft. Es muss nichts installiert werden, und es funktioniert auf jedem Gerät
            mit einem Browser, in der Schule wie zu Hause.
            </p>
            <p>
            Betrieben wird der Workspace von #{operator(s)}, nicht-kommerziell, ohne Werbung und ohne Tracking.
            Der Server läuft bei #{hosting(s)}. Der Quelltext ist öffentlich (github.com/specht/workspace).
            #{s[:partner] ? "Für #{school(s)} ist der Betreiber Auftragsverarbeiter; verantwortlich ist die Schule selbst. Ansprechperson an der Schule: #{Privacy.contact_html(s)}." : ''}
            </p>

            <h2>Welche Daten anfallen</h2>
            #{list([
                'E-Mail-Adresse, Name und Lerngruppe, aus der Einladungsliste der Lehrkraft; der Name ist nicht zwingend. Ein Konto entsteht erst bei der ersten Anmeldung.',
                'Was im Unterricht entsteht: Programme, Dateien, Datenbanken, Antworten auf Aufgaben, Abgaben.',
                'Technisch Nötiges: Anmeldecodes, Sitzungen, Protokolle ohne IP-Adressen.',
            ])}
            <p>Keine Noten, keine Fotos oder Gesundheitsdaten, keine Profile, keine Weitergabe für fremde Zwecke.</p>

            <h2>Wie sie geschützt sind</h2>
            #{list([
                'Nur eingeladene Personen können sich anmelden, mit einem Code per E-Mail, der nur im anfragenden Browser gilt.',
                'Jede Person hat einen eigenen, abgeschotteten Workspace; Lehrkräfte sehen nur ihre eigenen Lerngruppen.',
                'Sieht eine Lehrkraft in den Workspace einer Schülerin oder eines Schülers, nennt sie einen Grund, und die Person erfährt es.',
                'Alle Daten liegen auf einem Server in der EU; Verbindungen sind verschlüsselt.',
                'Konten werden spätestens zum Ende des Schuljahres gelöscht, in dem der Kurs endet, auf Wunsch jederzeit früher.',
            ])}

            <h2>Worum die Schule gebeten wird</h2>
            <p>
            Um eine schriftliche Entscheidung, dass der Workspace im Unterricht, vor allem im Fach Informatik, als digitales
            Lehr- und Lernmittel eingesetzt werden darf. Damit ist die Schule verantwortlich und der Einsatz rechtlich auf die Aufgabe der Schule gestützt
            (Art. 6 Abs. 1 Buchst. e DSGVO#{s[:school_law] ? " in Verbindung mit #{h(s[:school_law])}" : ''});
            eine Einwilligung jeder einzelnen Familie ist dann nicht nötig. Für die Schule ist die Nutzung kostenlos, und es ist nichts zu installieren.
            Die Entscheidung kann jederzeit widerrufen werden; dann werden alle Konten der Schule gelöscht.
            </p>
            #{cost_note_html(s)}
            <p>Alle weiteren Unterlagen liegen fertig vor:</p>
            #{list(DOCUMENTS.reject { |slug, _| slug == 'schulleitung' }.map { |_, (title, who)| "#{h(title)}, #{h(who)}" })}
            <p>Aufgaben für die Schule: die Entscheidung unterschreiben, die Vereinbarung zur Auftragsverarbeitung unterschreiben,
            den Eintrag ins Verzeichnis von Verarbeitungstätigkeiten aufnehmen und der Datenschutzbeauftragten Stelle zur Kenntnis geben,
            den Elternbrief verteilen.</p>

            <div class='decision'>
            <h2>Entscheidung der Schulleitung</h2>
            <p>
            Der Workspace darf im Unterricht, vor allem im Fach Informatik, als digitales Lehr- und Lernmittel eingesetzt werden. Verantwortlich für den Betrieb ist #{operator(s)}.
            Die beigefügte Datenschutzerklärung, das Löschkonzept und die technischen und organisatorischen Maßnahmen gelten.
            Die Entscheidung kann jederzeit widerrufen werden.
            </p>
            #{signature('Ort, Datum', 'Schulleitung')}
            </div>
        END_OF_HTML
    end

    # Voluntary contributions to the server's cost (COST_NOTE), on the page
    # for the Schulleitung; the use never depends on them, and the page says
    # so itself, whatever the note says.
    def self.cost_note_html(s)
        return '' unless s[:cost_note]

        "<p>#{h(s[:cost_note])} Ein Beitrag ist keine Bedingung: Die Nutzung hängt nicht davon ab, ob eine Schule beitragen kann.</p>\n"
    end

    # --- Prüfübersicht ---------------------------------------------------

    def self.pruefuebersicht_rows(s)
        rows = [
            ['Rechtsgrundlage', 'Art. 6 Abs. 1 Buchst. e DSGVO' + (s[:school_law] ? ", #{h(s[:school_law])}" : ''), 'Entscheidung der Schulleitung (Unterlage »Information und Entscheidung«)'],
            ['Zweckbindung', 'Art. 5 Abs. 1 Buchst. b DSGVO' + (digllv?(s) ? '; § 2 Abs. 1 DigLLV' : ''), 'Nur Unterricht; keine Werbung, keine Forschung, keine Noten, keine Profile (Datenschutzerklärung, »Wozu«)'],
            ['Datenminimierung', 'Art. 5 Abs. 1 Buchst. c, Art. 25 DSGVO', 'Name nicht zwingend; Protokolle ohne IP-Adressen; von Aufgaben nur die letzte Antwort; kein Tracking (TOM)'],
            ['Datenkategorien', digllv?(s) ? '§ 2 Abs. 5 DigLLV' : 'Art. 30 Abs. 1 Buchst. c DSGVO', 'Stammdaten, Account-Daten, Zuordnungsdaten, nutzergenerierte Inhalte, Verkehrsdaten; jede Datenart ist zugeordnet (VVT)'],
            ['Speicherbegrenzung', 'Art. 5 Abs. 1 Buchst. e DSGVO' + (digllv?(s) ? '; § 2 Abs. 6 DigLLV' : ''), 'Löschung spätestens zum Ende des Schuljahres, Löschkonzept; Löschfunktion für die Administration'],
            ['Information', 'Art. 12–14 DSGVO', 'Datenschutzerklärung nach Art. 13/14, öffentlich unter /impressum; Elternbrief'],
            ['Rechte der Betroffenen', 'Art. 15–21 DSGVO', 'Eine E-Mail genügt; Antwort binnen eines Monats; Löschung in einem Schritt'],
            ['Auftragsverarbeitung', 'Art. 28 DSGVO' + (digllv?(s) ? '; § 2 Abs. 3 DigLLV' : ''), 'Vereinbarung Schule – Betreiber (Entwurf); Rechenzentrum und E-Mail-Dienst als Unterauftragnehmer mit deren Standardverträgen'],
            ['Serverstandort', digllv?(s) ? '§ 2 Abs. 3 Satz 2 DigLLV' : 'Art. 44 ff. DSGVO', "EU: #{hosting(s)}; keine Übermittlung in Drittländer"],
            ['Verzeichnis', 'Art. 30 DSGVO', 'Eintrag fertig zum Ablegen (VVT)'],
            ['Sicherheit', 'Art. 32 DSGVO', 'TOM: Zugang nur mit Einladung und Code, Trennung der Workspaces, Verschlüsselung, Transparenz bei Einsicht'],
            ['Endgeräte', '§ 25 TDDDG', 'Nur technisch notwendige Cookies, erst nach der Anmeldung'],
            ['Prüfung durch die Schule', digllv?(s) ? '§ 2 Abs. 2 Satz 3 DigLLV' : 'Art. 24 DSGVO', 'Diese Unterlagen; Quelltext öffentlich; automatische Tests gleichen die Datenschutzerklärung mit dem Code ab'],
        ]
        rows
    end

    def self.pruefuebersicht_html(s)
        <<~END_OF_HTML
            <p>
            Jede Anforderung, die beim Einsatz eines digitalen Lernmittels zu prüfen ist, mit der Stelle, an der sie erfüllt ist.
            Alle Angaben beschreiben den Workspace, wie er ist (Stand #{Privacy::CHECKED.strftime('%d.%m.%Y')}), und lassen sich am öffentlichen Quelltext nachprüfen.
            </p>
            #{table(%w[Anforderung Vorschrift Erfüllt\ durch], pruefuebersicht_rows(s))}
            <h2>Datenschutz-Folgenabschätzung (Art. 35 DSGVO)</h2>
            <p>
            Zur Einschätzung, ob eine Folgenabschätzung nötig ist: Der Workspace verarbeitet keine besonderen Kategorien von Daten
            (Art. 9 DSGVO), bewertet und benotet nicht, erstellt keine Profile und trifft keine automatisierten Entscheidungen.
            Lehrkräfte sehen den Stand ihrer Lerngruppen; eine Einsicht in einzelne Workspaces geschieht mit Begründung und wird der
            Person angezeigt. Betroffen sind Minderjährige. Ob danach eine Folgenabschätzung erforderlich ist, entscheidet die Schule;
            die Unterlagen hier enthalten die dafür nötigen Angaben.
            </p>
        END_OF_HTML
    end

    # --- Datenschutzerklärung -------------------------------------------

    def self.datenschutzerklaerung_html(s)
        Privacy.page_html(s).sub(/\A<div class='privacy-page'>\n<h1>Impressum<\/h1>.*?(?=<h1 id='datenschutz'>)/m, "<div class='privacy-page'>\n")
            .sub("<h1 id='datenschutz'>Datenschutzerklärung</h1>", '')
    end

    # --- VVT --------------------------------------------------------------

    def self.vvt_html(s)
        school_controller = s[:legal_basis] == :school || s[:school_name].nil?
        controller = school_controller ? "#{school(s)}, vertreten durch die Schulleitung" : operator(s)
        address_text = school_controller ? s[:school_address] : s[:operator_address]
        address = address_text ? "<br>#{Privacy.lines_html(address_text)}" : ''
        dsb = s[:data_protection_officer] ? Privacy.lines_html(s[:data_protection_officer]) : 'die Datenschutzbeauftragte Stelle der Schule'
        records = Privacy.records(s).map do |r|
            [h(r[:data]), h(Privacy.categories_text(r)), h(r[:who]), h(r[:how_long])]
        end
        processors = Privacy.services(s).select { |svc| svc[:kind] == :processor }.map { |svc| svc[:html] }
        <<~END_OF_HTML
            #{table(%w[Angabe Inhalt], [
                ['Bezeichnung', 'Workspace: Programmierumgebung für den Informatikunterricht'],
                ['Verantwortlicher (Art. 30 Abs. 1 Buchst. a)', "#{controller}#{address}"],
                ['Datenschutzbeauftragte Stelle', dsb],
                ['Ansprechperson', Privacy.contact_html(s)],
                ['Zwecke (Buchst. b)', 'Durchführung des Informatikunterrichts: Programmieren in einer eigenen Umgebung, Bearbeitung und automatische Prüfung von Aufgaben, Abgaben, Leistungsüberprüfungen, Begleitung des Lernstands durch die Lehrkraft.'],
                ['Rechtsgrundlage', 'Art. 6 Abs. 1 Buchst. e DSGVO' + (s[:school_law] ? " in Verbindung mit #{h(s[:school_law])}" : '') + '; für Erwachsene von außerhalb der Schule Art. 6 Abs. 1 Buchst. b DSGVO'],
                ['Betroffene Personen (Buchst. c)', 'Schülerinnen und Schüler, die eine Lehrkraft eingeladen hat; Lehrkräfte. Nicht in der Verantwortung der Schule: Erwachsene von außerhalb der Schule und ehemalige Schülerinnen und Schüler, die ihr Konto behalten (Nutzungsordnung)'],
                ['Empfänger (Buchst. d)', 'Lehrkräfte der jeweiligen Lerngruppe; die Administration des Servers; als Auftragsverarbeiter: ' + processors.join(' ')],
                ['Drittländer (Buchst. e)', 'keine'],
                ['Löschfristen (Buchst. f)', Privacy.deletion_rule_html(s) + ' Einzelheiten: Löschkonzept.'],
                ['Technische und organisatorische Maßnahmen (Buchst. g)', 'siehe Anlage »Technische und organisatorische Maßnahmen«'],
            ])}
            <h2>Kategorien personenbezogener Daten (Art. 30 Abs. 1 Buchst. c)</h2>
            #{table(['Daten', 'Kategorie', 'Zugriff', 'Speicherdauer'], records)}
        END_OF_HTML
    end

    # --- TOM --------------------------------------------------------------

    def self.tom_html(s)
        <<~END_OF_HTML
            <p>Die Maßnahmen, die im Workspace umgesetzt sind (Stand #{Privacy::CHECKED.strftime('%d.%m.%Y')}). Jede lässt sich am öffentlichen Quelltext prüfen.</p>
            #{Privacy.measures(s).map { |heading, items| "<h2>#{h(heading)}</h2>\n" + list(items.map { |m| h(m) }) }.join}
            <h2>Verfügbarkeit und Belastbarkeit</h2>
            #{list([
                "Der Server läuft bei #{hosting(s)}; Rechenzeit und Speicher jedes Workspace sind begrenzt, damit ein Workspace die anderen nicht stören kann.",
                'Workspaces, die niemand benutzt, werden angehalten; die Dateien bleiben erhalten.',
            ])}
            <h2>Regelmäßige Überprüfung (Art. 32 Abs. 1 Buchst. d)</h2>
            #{list([
                'Automatische Tests im Quelltext prüfen, dass die Datenschutzerklärung jeden Dienst außerhalb des Servers nennt, dass die Seiten nichts von fremden Servern laden und dass das Löschen eines Kontos alles erfasst, was zu einer Person gespeichert wird.',
                'Ändert sich, was gespeichert wird, werden diese Angaben mitgeändert; das Datum oben zeigt den letzten Abgleich mit dem Code.',
            ])}
        END_OF_HTML
    end

    # --- Löschkonzept ---------------------------------------------------

    def self.loeschkonzept_html(s)
        rows = Privacy.records(s).map { |r| [h(r[:data]), h(r[:how_long])] }
        <<~END_OF_HTML
            <p>
            #{digllv?(s) ? 'Nach § 2 Abs. 6 DigLLV sind die Daten zu löschen, sobald kein Verarbeitungszweck mehr besteht, in der Regel spätestens zum Ende eines Schuljahres; dieses Löschkonzept legt die Fristen fest. ' : ''}#{Privacy.deletion_rule_html(s)}
            </p>
            <h2>Ablauf zum Ende des Schuljahres</h2>
            #{list([
                'Die Lehrkräfte nehmen Lerngruppen, deren Kurs endet, und Schülerinnen und Schüler, die die Schule verlassen, aus ihren Einladungslisten.',
                'Die Administration ruft unter »Personen und Schulen › Aufräumen › Konten ohne Einladung« die Konten auf, deren Adresse in keiner Einladungsliste mehr steht, und löscht sie bis zum letzten Tag des Schuljahres.',
                'Wer im nächsten Schuljahr weiter Informatik hat, bleibt eingeladen; der Zweck besteht fort, das Konto bleibt.',
                'Wer die Schule verlässt und das Konto behalten möchte, sagt das vorher; die Administration führt es dann in einer eigenen Gruppe (etwa »Ehemalige«) unter der Nutzungsordnung weiter, und die Person unterschreibt sie.',
                'Statt sofort zu löschen, kann die Administration die Löschung per E-Mail ankündigen: Das Konto wird dann zwei Wochen später von selbst gelöscht. Bis dahin lädt die Person ihre Daten herunter oder bittet darum, das Konto als Gast weiterzuführen; wer nichts tut, dessen Konto wird gelöscht.',
            ])}
            <h2>Löschung auf Wunsch</h2>
            <p>
            Jede Person kann ihr Konto im Profil unter »Meine Daten« selbst löschen und vorher alle ihre Daten als Archiv herunterladen.
            Ebenso genügt eine E-Mail an #{Privacy.contact_html(s)}: Die Administration löscht das Konto mit allem, was dazugehört, in einem Schritt,
            binnen eines Monats, und bestätigt es. Antworten zu Aufgaben und Abgaben löschen Schülerinnen und Schüler jederzeit selbst.
            </p>
            <h2>Fristen je Datenart</h2>
            #{table(['Daten', 'Gelöscht'], rows)}
            <h2>Was mit einem Konto gelöscht wird</h2>
            <p>
            Workspace mit allen Dateien, die Workspaces der Leistungsüberprüfungen, die eigenen MySQL- und Neo4j-Datenbanken,
            Antworten, Abgaben, Hinweise zur Einsicht, Sitzungen, Anmeldeanfragen, geteilte Links und das Konto selbst.
            Ein automatischer Test stellt sicher, dass nichts, was zu einer Person gespeichert wird, dabei vergessen wird.
            </p>
        END_OF_HTML
    end

    # --- Auftragsverarbeitung -------------------------------------------

    def self.auftragsverarbeitung_html(s)
        processors = Privacy.services(s).select { |svc| svc[:kind] == :processor }
        address = s[:operator_address] ? ", #{h(s[:operator_address].split("\n").map(&:strip).join(', '))}" : ''
        <<~END_OF_HTML
            <p class='draft'>Entwurf. Vor der Unterzeichnung von der Schule und ihrer Datenschutzbeauftragten Stelle zu prüfen.</p>
            <p>
            zwischen <strong>#{school(s)}</strong>, vertreten durch die Schulleitung (Verantwortlicher),
            und <strong>#{operator(s)}</strong>#{address} (Auftragsverarbeiter).
            </p>
            <h2>§ 1 Gegenstand und Dauer</h2>
            <p>
            Der Auftragsverarbeiter betreibt für den Verantwortlichen den Workspace, eine Programmierumgebung für den Informatikunterricht,
            auf einem Server bei #{hosting(s)}. Die Vereinbarung gilt, solange die Schule den Workspace einsetzt; sie endet mit dem Widerruf
            der Entscheidung der Schulleitung.
            </p>
            <h2>§ 2 Art, Zweck und Daten</h2>
            <p>
            Zweck, Art der Daten und Kreis der Betroffenen ergeben sich aus dem Eintrag im Verzeichnis von Verarbeitungstätigkeiten,
            der Teil dieser Vereinbarung ist. Eine Verarbeitung zu anderen Zwecken findet nicht statt.
            </p>
            <h2>§ 3 Pflichten des Auftragsverarbeiters (Art. 28 Abs. 3 DSGVO)</h2>
            #{list([
                'Er verarbeitet die Daten nur auf dokumentierte Weisung des Verantwortlichen; als Weisung gelten diese Vereinbarung, die Datenschutzerklärung und das Löschkonzept (Buchst. a).',
                'Wer Zugang zu den Daten hat (Administration), ist zur Vertraulichkeit verpflichtet (Buchst. b).',
                'Er trifft die Maßnahmen nach Art. 32 DSGVO, die in der Anlage »Technische und organisatorische Maßnahmen« beschrieben sind, und ändert sie nur so, dass das Schutzniveau nicht sinkt (Buchst. c).',
                'Er unterstützt den Verantwortlichen bei Anfragen betroffener Personen, insbesondere bei Auskunft und Löschung (Buchst. e), und bei den Pflichten nach Art. 32 bis 36 DSGVO (Buchst. f).',
                'Nach Ende der Vereinbarung löscht er alle Daten der Schule, soweit keine Pflicht zur Aufbewahrung besteht, und bestätigt es (Buchst. g).',
                'Er stellt alle Informationen zum Nachweis bereit und ermöglicht Überprüfungen; der Quelltext ist öffentlich (Buchst. h).',
                'Eine Verletzung des Schutzes personenbezogener Daten meldet er dem Verantwortlichen unverzüglich (Art. 33 Abs. 2 DSGVO).',
            ])}
            <h2>§ 4 Unterauftragsverarbeiter (Art. 28 Abs. 2 und 4 DSGVO)</h2>
            <p>Der Verantwortliche ist mit folgenden Unterauftragsverarbeitern einverstanden; mit ihnen bestehen Verträge zur Auftragsverarbeitung nach deren Standardbedingungen:</p>
            #{list(processors.map { |svc| svc[:html] })}
            <p>Über einen Wechsel oder einen weiteren Unterauftragsverarbeiter informiert der Auftragsverarbeiter vorher; der Verantwortliche kann widersprechen.</p>
            <h2>§ 5 Ort der Verarbeitung</h2>
            <p>Die Daten werden ausschließlich in der Europäischen Union verarbeitet.</p>
            <h2>§ 6 Vergütung</h2>
            <p>Der Auftragsverarbeiter erbringt die Leistung unentgeltlich.#{s[:cost_note] ? ' Freiwillige Beiträge zu den Kosten des Servers sind möglich; sie sind keine Gegenleistung und keine Bedingung.' : ''}</p>
            #{signature('Ort, Datum, Schulleitung', 'Ort, Datum, Auftragsverarbeiter')}
        END_OF_HTML
    end

    # --- Elternbrief ----------------------------------------------------

    # The address of the Workspace for the first paragraph of the letter:
    # parents can look at every activity without an account.
    def self.website_link(s)
        return '' unless s[:web_root]

        url = h(s[:web_root])
        " (<a href='#{url}'>#{url.sub(%r{\Ahttps?://}, '')}</a>; alle Aktivitäten und das Handbuch lassen sich dort ohne Anmeldung ansehen)"
    end

    def self.elternbrief_html(s)
        consent = s[:parental_consent_required] || s[:legal_basis] == :consent
        out = <<~END_OF_HTML
            <p>Liebe Eltern und Erziehungsberechtigte,</p>
            <p>
            im Informatikunterricht arbeitet Ihr Kind mit dem Workspace#{website_link(s)}, einer Programmierumgebung im Browser. Ihr Kind erhält dort
            einen eigenen Editor mit Terminal und Datenbanken, Aufgaben, die der Workspace selbst prüft, und ein Handbuch.
            Es muss nichts installiert werden; der Workspace funktioniert in der Schule wie zu Hause.
            </p>
            <h2>Welche Daten gespeichert werden</h2>
            <p>
            Die E-Mail-Adresse, der Name und die Lerngruppe Ihres Kindes, sowie was im Unterricht entsteht: Programme, Dateien,
            Antworten auf Aufgaben und Abgaben. Noten werden im Workspace nicht gespeichert. Es gibt kein Tracking und keine Werbung,
            und die Daten werden für nichts anderes als den Unterricht verwendet. Alle Daten liegen auf einem Server in der EU.
            </p>
            <h2>Wer die Daten sieht</h2>
            <p>
            Ihr Kind selbst und die Informatik-Lehrkräfte seiner Lerngruppe. Sieht eine Lehrkraft in den Workspace Ihres Kindes,
            etwa um bei einem Fehler zu helfen, gibt sie einen Grund an, und Ihr Kind sieht beim nächsten Besuch, wer, wann und warum.
            </p>
            <h2>Wann die Daten gelöscht werden</h2>
            <p>#{Privacy.deletion_rule_html(s)}</p>
            <h2>Ihre Rechte</h2>
            <p>
            #{Privacy.rights_html(s)} Für alle Fragen und Anliegen schreiben Sie bitte an #{Privacy.contact_html(s)}.
            Die vollständige Datenschutzerklärung steht unter #{privacy_url(s)}.
            </p>
            <p>Mit freundlichen Grüßen<br>#{signer(s)}</p>
        END_OF_HTML
        if consent
            out += <<~END_OF_HTML
                <div class='decision'>
                <h2>Einverständnis</h2>
                <p>
                Ich bin damit einverstanden, dass mein Kind den Workspace wie beschrieben nutzt. Das Einverständnis ist freiwillig
                und kann jederzeit mit Wirkung für die Zukunft widerrufen werden; das Konto wird dann gelöscht.
                </p>
                #{signature('Name des Kindes, Lerngruppe', 'Ort, Datum, Unterschrift einer erziehungsberechtigten Person')}
                </div>
            END_OF_HTML
        end
        out
    end

    # --- Nutzungsordnung ------------------------------------------------

    def self.nutzungsordnung_html(s)
        <<~END_OF_HTML
            <p>
            Der Workspace ist eine Programmierumgebung für den Informatikunterricht. Diese Ordnung gilt für alle, die dort ein Konto haben.
            Für Erwachsene von außerhalb der Schule und für ehemalige Schülerinnen und Schüler, die ihr Konto behalten, ist sie die
            Vereinbarung mit #{operator(s)}, mit der sie ihr Konto erhalten oder weiterführen; sie endet, wenn eine Seite es wünscht.
            </p>
            <h2>Konto und Anmeldung</h2>
            #{list([
                'Jede Person nutzt nur ihr eigenes Konto und gibt Anmeldecodes nicht weiter.',
                'Der Name ist nicht zwingend; die E-Mail-Adresse muss nur Anmeldecodes empfangen können.',
            ])}
            <h2>Was in den Workspace gehört</h2>
            #{list([
                'Was für den Unterricht entsteht: Programme, Dateien, Datenbanken, Projekte. Der Workspace ist kein Speicher für Privates.',
                'Keine personenbezogenen Daten anderer (Namen, Fotos, Adressen) in Projekten, außer mit deren Einverständnis; für Übungsdaten erfundene Namen verwenden.',
                'Keine rechtswidrigen, beleidigenden oder fremden urheberrechtlich geschützten Inhalte; keine Programme, die andere stören oder schädigen.',
                'Sicherheitslücken oder Fehler bitte melden, nicht ausnutzen.',
            ])}
            <h2>Teilen</h2>
            <p>
            Ein geteilter Link auf den eigenen Workspace öffnet ihn für jede Person mit dem Link, auch ohne Anmeldung.
            Geteilt wird nur, was gezeigt werden soll, und der Link wird danach wieder abgeschaltet.
            </p>
            <h2>Was Lehrkräfte sehen</h2>
            <p>
            Lehrkräfte sehen den Stand ihrer Lerngruppen bei den Aufgaben und die Abgaben. Sie können einen Workspace öffnen, um zu helfen;
            dafür nennen sie einen Grund, und die Person erfährt es. Die Administration kann für Betrieb und Hilfe alle Daten einsehen.
            </p>
            <h2>Ende</h2>
            <p>
            Das Konto wird nach dem Löschkonzept gelöscht: #{Privacy.deletion_rule_html(s)}
            Vorher lassen sich alle Daten im Profil unter »Meine Daten« herunterladen.
            Bei einem Verstoß gegen diese Ordnung kann ein Konto gesperrt werden.
            </p>
        END_OF_HTML
    end

    # --- Webhosting -----------------------------------------------------

    # Replaces docs/Nutzungsvereinbarung-Webhosting.pdf: the same offer, with
    # what is stored and when it is deleted, and the operator from the
    # settings.
    def self.webhosting_html(s)
        address = s[:operator_address] ? "<br>#{Privacy.lines_html(s[:operator_address])}" : ''
        <<~END_OF_HTML
            <p>
            zwischen <strong>#{operator(s)}</strong>#{address}<br>(im Folgenden »Betreiber«)
            </p>
            <p>und</p>
            #{signature('Name', 'Lerngruppe')}
            <p>(im Folgenden »Nutzerin oder Nutzer«)</p>

            <h2>§ 1 Gegenstand</h2>
            <p>
            Der Betreiber stellt kostenlos eine Subdomain (zum Beispiel meineseite.hackschule.de) und Speicherplatz für eine statische
            Website zur Verfügung, die im Informatikunterricht entsteht: HTML, CSS, JavaScript und Medien wie Bilder, Audio und Video.
            Serverseitige Programme (etwa PHP oder Python) und Datenbanken gibt es nicht. Die Technik, einschließlich der Zertifikate
            für HTTPS, verwaltet der Betreiber.
            </p>
            <p>
            <strong>Die Website ist öffentlich:</strong> Jede Person im Internet kann sie aufrufen, ohne Anmeldung, und Suchmaschinen
            können sie finden. Das gilt, anders als im Workspace, auch für alle, die nicht zur Schule gehören.
            </p>

            <h2>§ 2 Pflichten</h2>
            <p>Die Nutzerin oder der Nutzer verpflichtet sich insbesondere,</p>
            #{list([
                'keine rechtswidrigen Inhalte zu veröffentlichen, insbesondere keine fremden Bilder, Texte oder Musik ohne Erlaubnis;',
                'keine beleidigenden, rassistischen oder jugendgefährdenden Inhalte zu verbreiten;',
                'die Website nicht kommerziell zu nutzen und keine Werbung zu schalten;',
                'personenbezogene Daten anderer (etwa Namen oder Fotos) nur mit deren ausdrücklichem Einverständnis zu verwenden;',
                'von sich selbst nur zu zeigen, was öffentlich sein soll: keine Anschrift, keine Telefonnummer; statt des vollen Namens genügt ein Vorname oder ein Pseudonym;',
                'keine Analyse-, Tracking- oder Werbedienste einzubinden, die Daten der Besucherinnen und Besucher erheben;',
                'nichts zu veröffentlichen, was Besucherinnen und Besucher gefährdet (Schadcode, Phishing, unerwünschte Weiterleitungen);',
                'Sicherheitslücken oder technische Probleme dem Betreiber unverzüglich zu melden.',
            ])}

            <h2>§ 3 Verantwortung für Inhalte</h2>
            <p>
            Für die veröffentlichten Inhalte ist die Nutzerin oder der Nutzer verantwortlich. Der Betreiber überwacht die Websites
            nicht laufend; Stichproben sind möglich. Verstößt eine Website gegen diese Vereinbarung oder erfährt der Betreiber von
            einem rechtswidrigen Inhalt, schaltet er die Website ohne Vorwarnung ab.
            </p>

            <h2>§ 4 Zugang</h2>
            <p>
            Dateien werden ausschließlich per SSH, SFTP oder rsync mit einem Schlüsselpaar hochgeladen. Den privaten Schlüssel
            gibt die Nutzerin oder der Nutzer nicht weiter.
            </p>

            <h2>§ 5 Datenschutz</h2>
            <p>
            Gespeichert werden: Name und Lerngruppe (auf dieser Vereinbarung), die Subdomain, der Benutzername, der öffentliche
            SSH-Schlüssel und die hochgeladenen Dateien. Sie dienen nur dem Betrieb der Website (Art. 6 Abs. 1 Buchst. b DSGVO)
            und werden mit dem Ende der Vereinbarung gelöscht. Im Übrigen gilt die Datenschutzerklärung des Workspace;
            Ansprechperson ist #{Privacy.contact_html(s)}.
            </p>

            <h2>§ 6 Dauer und Ende</h2>
            <p>
            Die Vereinbarung gilt bis zum Ende der Schulzeit der Nutzerin oder des Nutzers, wenn sie nicht verlängert wird.
            Beide Seiten können sie jederzeit beenden; eine E-Mail genügt. Mit dem Ende schaltet der Betreiber die Website ab und
            löscht binnen eines Monats alle Dateien, den Schlüssel und die Subdomain.
            </p>

            <h2>Wunsch-Subdomain</h2>
            #{signature('Gewünschte Subdomain', 'Ort, Datum')}
            #{signature('Unterschrift Nutzerin oder Nutzer', 'Unterschrift einer erziehungsberechtigten Person (bei Minderjährigen)', 'Unterschrift Betreiber')}
        END_OF_HTML
    end

    # The index of /datenschutz.
    def self.index_html(s)
        items = DOCUMENTS.map do |slug, (title, who)|
            consent = slug == 'elternbrief' ? " · <a href='#{path(s, slug)}#{s[:school_key] ? '&amp;' : '?'}einverstaendnis=1'>mit Einverständniserklärung</a> (solange die Schule nicht zugestimmt hat)" : ''
            "<li><a href='#{path(s, slug)}'>#{h(title)}</a>#{consent}<br><span class='who'>#{h(who)}</span></li>"
        end
        partner = s[:partner] ? "<p>Die Unterlagen für <strong>#{school(s)}</strong>: Verantwortlich ist die Schule, #{operator(s)} betreibt den Workspace für sie als Auftragsverarbeiter. " \
            "Was hier fehlt (Anschrift, Datenschutzbeauftragte Stelle, Vorschrift, Aufsichtsbehörde), trägt die Administration unter »Personen › Schulen« ein.</p>" : ''
        <<~END_OF_HTML
            <p>
            Alles, was eine Schule braucht, um den Workspace im Unterricht einzusetzen, aus denselben Angaben wie die
            <a href='/impressum#datenschutz'>Datenschutzerklärung</a>. Jede Unterlage lässt sich drucken oder als PDF speichern.
            </p>
            #{partner}
            <ol class='documents'>
            #{items.join("\n")}
            </ol>
        END_OF_HTML
    end

    STYLE = <<~END_OF_CSS
        body { font-family: 'IBM Plex Sans', sans-serif; max-width: 48em; margin: 2em auto; padding: 0 16px; line-height: 1.5; color: #222; background: #fff; }
        h1 { font-size: 1.6rem; margin-bottom: 0.1em; }
        .who { color: #666; margin-top: 0; }
        h2 { font-size: 1.15rem; margin: 1.4em 0 0.4em 0; }
        h4 { font-size: 1rem; margin: 1em 0 0.3em 0; }
        table { border-collapse: collapse; width: 100%; margin: 0.5em 0 1em 0; font-size: 0.92rem; }
        th, td { border: 1px solid #bbb; padding: 0.35em 0.5em; text-align: left; vertical-align: top; }
        th { background: #f0f0f0; }
        .privacy-ref { display: block; font-size: 0.8rem; font-weight: normal; color: #666; }
        .text-body-secondary { color: #666; }
        .decision { border: 1px solid #888; padding: 0.2em 1em 1em 1em; margin-top: 2em; break-inside: avoid; }
        .signatures { display: flex; gap: 3em; margin-top: 3em; }
        .signature { flex: 1; font-size: 0.85rem; color: #444; }
        .signature .line { border-bottom: 1px solid #333; height: 2.5em; margin-bottom: 0.3em; }
        .draft { border-left: 4px solid #c90; padding-left: 0.8em; color: #750; }
        .documents li { margin-bottom: 0.6em; }
        .toolbar { margin-bottom: 1.5em; font-size: 0.9rem; }
        .toolbar a, .toolbar button { margin-right: 1em; }
        @media print { .toolbar { display: none; } body { margin: 0; max-width: none; } a { color: inherit; text-decoration: none; } }
    END_OF_CSS

    # A whole printable page; light only, as it is meant for paper.
    def self.page(slug, s)
        title, who = slug ? DOCUMENTS.fetch(slug) : ['Datenschutz: Unterlagen für die Schule', 'für Schulleitung, Datenschutzbeauftragte Stelle und Eltern']
        content = slug ? body(slug, s) : index_html(s)
        toolbar = slug ? "<div class='toolbar'><a href='#{path(s)}'>Alle Unterlagen</a><button onclick='window.print()'>Drucken</button></div>" : ''
        <<~END_OF_HTML
            <!DOCTYPE html>
            <html lang='de'>
            <head>
            <meta charset='utf-8'>
            <meta name='viewport' content='width=device-width, initial-scale=1'>
            <title>#{h(title)}</title>
            <link href='/include/fonts.css' rel='stylesheet'>
            <style>#{STYLE}</style>
            </head>
            <body>
            #{toolbar}
            <h1>#{h(title)}</h1>
            <p class='who'>#{h(who)}#{s[:school_name] ? " · #{h(s[:school_name])}" : ''} · Stand #{Privacy::CHECKED.strftime('%d.%m.%Y')}</p>
            #{content}
            </body>
            </html>
        END_OF_HTML
    end
end
