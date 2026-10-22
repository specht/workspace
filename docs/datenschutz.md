# Datenschutz: Impressum, Datenschutzerklärung und Unterlagen für die Schule

Für Betreibende einer Installation und für alle, die am Code ändern, was
gespeichert wird.

## Was es gibt

- `/impressum`: Impressum und Datenschutzerklärung, gegliedert nach Art. 13
  und 14 DSGVO; unter jeder Überschrift steht klein, welchen Punkt sie
  erfüllt.
- `/datenschutz`: die Unterlagen für die Schule, jede zum Drucken und als
  PDF (`NAME.pdf`), alle zusammen als ZIP (`unterlagen.zip`): Information
  und Entscheidung für die Schulleitung, Prüfübersicht,
  Datenschutzerklärung, Eintrag für das Verzeichnis von
  Verarbeitungstätigkeiten, technische und organisatorische Maßnahmen,
  Löschkonzept, Rollen- und Berechtigungskonzept, Risikoanalyse, Entwurf
  einer Datenschutz-Folgenabschätzung (diese drei für den Fall, dass die
  Schule den Workspace als Lernmanagementsystem nach § 3 DigLLV einordnet;
  die Einschätzungen darin stammen vom Betreiber und sind von der Schule zu
  prüfen), Entwurf der Vereinbarung zur Auftragsverarbeitung,
  Elternbrief (auch mit Einverständniserklärung), Nutzungsordnung,
  Nutzungsvereinbarung zum Webhosting. Nicht öffentlich
  (`Main#document_access!`): Datenschutzerklärung, Nutzungsordnung und
  Webhosting für alle, die angemeldet sind (die Fassung einer Partnerschule
  nur für deren Leute), alles andere für Lehrkräfte der Schule und die
  Administration. Die Unterlagen gehen als Dateien oder ausgedruckt an die
  Schulleitung. Öffentlich ist nur `/impressum`. Die PDFs macht
  `weasyprint` im Ruby-Image, mit nur den Schnitten von IBM Plex Sans
  (`Main.pdf_font_css`; alle 48 Schriften kosteten neun Sekunden je PDF).
- `/personen` (»Alle Personen«, »Aufräumen«): ein Konto mit allem, was
  dazugehört, löschen; Einladungen ohne Konto und Konten ohne Einladung.
  Unter »Schulen« der Stand jeder Schule in der Übergangszeit (unten).

Die Rechtslage in Berlin (§ 64 Abs. 11 Schulgesetz, DigLLV), die Wege, auf
denen eine Schule den Workspace nutzen kann, und was mit der
Datenschutzbeauftragten Stelle noch zu klären ist, stehen in
`docs/handoff-datenschutz.md`.

Alles kommt aus denselben Angaben: `src/ruby/include/privacy.rb` (die Fakten:
was gespeichert wird, wer es sieht, wie lange, welche Dienste beteiligt sind,
welche Schutzmaßnahmen es gibt) und `privacy_documents.rb` (die Unterlagen).
Die Texte sind förmlich, ohne »du«; ein Test prüft das.

## Einstellungen in `credentials.rb`

Alle freiwillig; fehlt eine, sagt die Seite, dass die Angabe fehlt.

- `OPERATOR_NAME`, `OPERATOR_ROLE`, `OPERATOR_SCHOOL`, `OPERATOR_ADDRESS`:
  wer den Workspace betreibt, mit einer Anschrift, unter der Briefe ankommen
  (die der Schule genügt). Name, Rolle und Schule ergeben die Zeile
  »Name, Rolle am Schule« (`Privacy.operator_line`) im Impressum, in der
  Datenschutzerklärung, der Vereinbarung zur Auftragsverarbeitung, der
  Entscheidung für die Schulleitung und der Nutzungsordnung; ohne Rolle
  »Name am Schule«. Den Elternbrief unterschreibt der Name allein.
- `CONTACT_EMAIL`: an wen man schreibt, auch für Auskunft und Löschung.
- `DATA_PROTECTION_OFFICER`: die Datenschutzbeauftragte Stelle der Schule.
- `HOSTING`, `MAIL_PROVIDER`: Rechenzentrum (nur eines in der EU, denn die
  Seiten sagen, dass keine Daten die EU verlassen) und E-Mail-Dienst.
- `LEGAL_BASIS`: `:school`, sobald die Schulleitung schriftlich entschieden
  hat; dann ist die Schule verantwortlich. `:consent` für ein eigenes Angebot
  mit Einwilligung. Ohne die Zeile sagt die Seite, dass die Rechtsgrundlage
  noch nicht eingetragen ist.
- `SCHOOL_LAW`: die Vorschrift, auf die sich die Schule stützt. Steht darin
  `DigLLV`, nennen Prüfübersicht und Löschkonzept die Berliner Regeln.
- `SUPERVISORY_AUTHORITY`: die Aufsichtsbehörde für Beschwerden.
- `BACKUP`, `BACKUP_RETENTION_MONTHS`: wo die verschlüsselten Sicherungen
  liegen und wie viele Monate sie höchstens aufbewahrt werden. Ohne `BACKUP`
  erwähnen die Seiten keine Sicherungen; die Zahl muss zu borgmatic passen
  (`keep_daily`, `keep_weekly`, `keep_monthly`, kein `keep_yearly`).
- `PARENTAL_CONSENT_REQUIRED = true`: für eine Installation, deren Schule ein
  Einverständnis verlangt. Der Elternbrief bekommt dann immer die
  Einverständniserklärung, und die Datenschutzerklärung sagt, dass die
  Lehrkraft es einholt und unter `/gruppen` vermerkt. Gesperrt wird ohne
  Einverständnis niemand (teacher-access.md, »Zustimmung der Schule,
  Einverständnis, ungeklärt«).
- `DEDICATED_SERVER = true`: wenn auf dem Server nichts anderes läuft als
  der Workspace. Dann sagen es die technischen Maßnahmen, die Risikoanalyse
  und die Folgenabschätzung; ohne die Zeile sagen sie nichts darüber.
- `COST_NOTE`: was der Server kostet und wie eine Schule beitragen kann;
  steht auf der Seite für die Schulleitung, in der Vereinbarung (§ 6) und in
  der Willkommensmail an Lehrkräfte. Die Seiten sagen dazu immer, dass die
  Nutzung nicht davon abhängt. Geht Geld an eine Privatperson, sind Steuer
  und Nebentätigkeit zu bedenken; einfacher zahlt ein Förderverein oder der
  Schulträger den Server.

## Partnerschulen

Sobald die Einladungen in der Datenbank sind (teacher-access.md,
»Einladungen importieren«), legt die Administration unter `/personen`
weitere Schulen an, mit Anschrift, Datenschutzbeauftragter Stelle, Vorschrift
und Aufsichtsbehörde. `/datenschutz?schule=KEY` (und jede Unterlage darunter)
ist dann das Paket für diese Schule (`Privacy.for_school`): Sie ist die
Verantwortliche, der Betreiber ihr Auftragsverarbeiter, ihre Lehrkräfte sind
die Ansprechpersonen, und den Elternbrief unterschreibt die Lehrkraft, die
angemeldet ist (sonst stehen alle Lehrkräfte der Schule darunter). Fehlt
eine Angabe der Schule, fehlt sie in den Unterlagen, statt dass die der
eigenen Schule dort stünde. Die Schule der Installation und die Gäste
bekommen die gewöhnlichen Unterlagen. Lehrkräfte finden den Link unter
`/gruppen`, die Administration unter `/personen`.

Hat die Schulleitung entschieden und die Vereinbarung unterschrieben, trägt
die Administration beide Daten unter `/personen` ein; dann nennt die
Datenschutzerklärung unter `/impressum` die Schule mit einem Link auf ihre
Fassung.

## Die Übergangszeit

Bis zum Stichtag, dem 12. April 2027 (`TRANSITION_DEADLINE`), sollen alle
Schulen ihre Unterlagen beisammen haben. Bis dahin sperrt die Rechtslage
einer Schule niemanden, und am Stichtag ändert sich nichts von selbst: Was
danach geschieht, entscheidet der Betreiber (Vorschlag in
`docs/handoff-datenschutz.md`). Der Stichtag ist ein Termin des Projekts,
keine Ausnahme von der DSGVO.

Unter `/personen` › Schulen hält die Administration fest, wie weit jede
Schule ist: Stand des Gesprächs, letzter Kontakt, wann nachgefasst wird, die
Schritte (Schulleitung angesprochen, Rechtsgrundlage geklärt,
Datenschutzbeauftragte Stelle beteiligt, Entscheidung, Vereinbarung), offene
Fragen und eine Notiz ohne Namen. Darüber stehen der Stichtag, die Tage bis
dahin, was gerade ansteht, und wie viele Schulen fertig, in Arbeit, noch
nicht angesprochen oder fällig sind. »Nachricht vorbereiten« füllt einen
Text für die Schule aus, zum Kopieren in eine eigene E-Mail; der Workspace
verschickt nichts. Der Stand ist kein Nachweis: Zugestimmt hat eine Schule
nur mit den beiden Daten.

Ein schriftliches Einverständnis je Schülerin oder Schüler (Elternbrief mit
Einverständniserklärung, `?einverstaendnis=1`, von der Lehrkraft unter
`/gruppen` vermerkt) trägt nur eine freiwillige Nutzung. Für den Unterricht
taugt es nicht als Grundlage: Wer am Unterricht teilnehmen muss, kann nicht
frei einwilligen (Erwägungsgrund 43 DSGVO). Dort ist der Weg die
Entscheidung der Schule. Gesperrt wird ohne beides während der
Übergangszeit niemand (teacher-access.md, »Zustimmung der Schule,
Einverständnis, ungeklärt«); ein ausdrücklicher Widerspruch oder ein
Widerruf wird sofort bearbeitet (Sperre unter `/personen`).

## Der Weg über die Schule

In Berlin: § 64 Abs. 11 des Schulgesetzes und § 2 DigLLV. Eine Einwilligung
jeder einzelnen Familie braucht es dann nicht, wohl aber eine Prüfung durch
die Schule selbst. Für den Lernraum Berlin und Videokonferenzen hat die
Berliner Datenschutzbeauftragte das am 17. September 2021 ausdrücklich
gesagt; für andere digitale Lehr- und Lernmittel gilt dieselbe Vorschrift.

1. Die Einstellungen eintragen, `LEGAL_BASIS` noch nicht.
2. Der Schulleitung den Link `/datenschutz` geben, oder die Unterlagen
   gedruckt.
3. Die Schule prüft mit ihrer Datenschutzbeauftragten Stelle
   (§ 2 Abs. 2 DigLLV; dafür die Prüfübersicht); über den Einsatz im
   Unterricht entscheiden die Konferenzen (Gesamtkonferenz für die
   Grundsätze, § 79 Abs. 3 Nr. 8, Fachkonferenz für die Einführung, § 16
   Abs. 2 Schulgesetz).
4. Die Schulleitung unterschreibt die Entscheidung und die Vereinbarung zur
   Auftragsverarbeitung; der Eintrag für das Verzeichnis geht an die
   Datenschutzbeauftragte Stelle.
5. `LEGAL_BASIS = :school` eintragen, den Elternbrief verteilen (zur
   Information, nicht zur Einwilligung).

Für die Vereinbarung braucht es Verträge zur Auftragsverarbeitung mit dem
Rechenzentrum und dem E-Mail-Dienst; die meisten Anbieter bieten sie als
Standardvertrag an (bei Hetzner in der Console).

## Löschen

- Zum Ende des Schuljahres nehmen die Lehrkräfte beendete Kurse und
  Abgänger aus ihren Gruppen; die Administration löscht unter
  »Personen und Schulen › Aufräumen › Konten ohne Einladung« alles, was dort
  steht. Daneben zeigen »Einladungen ohne Konto« je Gruppe, wie viele ein
  Konto haben und wann sie zuletzt da waren.
- Auf Wunsch oder beim Aufräumen: unter »Alle Personen« oder »Konten ohne
  Einladung« beim Konto auf den Papierkorb (oder mehrere auswählen). Vorgabe
  ist »Per E-Mail ankündigen«: `User.deletion_at` bekommt den Tag in
  `DELETION_NOTICE_DAYS` (14) Tagen, die Person eine E-Mail
  (`MailTexts.deletion_notice`, Antworten an `CONTACT_EMAIL`), das Profil
  zeigt den Tag, und `run_scheduled_deletions` (stündlich, eigener Thread)
  löscht dann mit einer kurzen Bestätigung. Bis dahin: »Löschung abbrechen«
  oder »Als Gast weiterführen« (in eine Gruppe der Gäste). »Jetzt löschen«
  verlangt die Adresse zur Bestätigung. Steht die Adresse noch in einer
  Gruppe, dort herausnehmen.
- Gelöscht wird: der Workspace, die Workspaces der Leistungsüberprüfungen
  (und bei Lehrkräften deren Vorschau), die MySQL- und Neo4j-Datenbanken mit
  ihren Logins, die zusätzlichen MySQL-Datenbanken, Abgaben, und in Neo4j
  alles, was am Konto hängt (`include/account_deletion.rb`).
- Abgelaufene Sitzungen löscht der Server selbst, Zugriffsprotokolle
  logrotate nach 7 Tagen (`bootstrap/03-setup-storage.rb`), die Ausgaben der
  Dienste Docker nach 3 × 10 MB.
- Eine Sitzung (`Session`) hält neben der Kennung den Tag der Anmeldung
  (`created_on`), den Tag der letzten Nutzung (`last_used_on`, höchstens
  einmal am Tag geschrieben, nicht beim Nachahmen) und eine grobe Angabe des
  Geräts (`device`, etwa »Firefox auf Windows«), die bei der Anmeldung aus
  dem User-Agent gebildet wird (`include/login_sessions.rb`); den User-Agent
  selbst und die IP-Adresse speichert der Workspace nicht. Die Person sieht
  die Liste im Profil unter »Konto & Darstellung« und beendet dort einzelne
  Sitzungen; die Seite kennt sie nur unter einem Hashwert der Kennung. Die
  Angaben gehen mit der Sitzung: bei der Abmeldung, nach einem Jahr, mit dem
  Konto. Sitzungen von vor dieser Änderung haben sie nicht und sagen das.
- Sicherungen werden nicht nachträglich verändert; gelöschte Daten
  verschwinden aus ihnen, wenn sie ablaufen. Jede Löschung steht deshalb
  ohne Adresse (Hashwert und Datum) in `/internal/deleted-accounts.jsonl`.
  Nach dem Zurückspielen einer Sicherung: die aktuelle Datei behalten (vorher
  wegkopieren, wenn `/internal` mit zurückgespielt wird) und unter
  »Administration › Nach einer Wiederherstellung« mit dem Datum der Sicherung
  »Löschungen wiederholen«.
- Traefik vor dem Workspace (`docker/traefik/docker-compose.yaml`) schreibt
  bewusst kein Zugriffsprotokoll (kein `--accesslog`): Die
  Datenschutzerklärung sagt, dass die Protokolle keine IP-Adressen enthalten.
- Selbst: Im Profil unter »Meine Daten« lädt jede Person alle ihre Daten als
  Archiv herunter (`include/account_export.rb`) und löscht ihr Konto selbst.
  Wer die Schule verlässt und das Konto behalten möchte, kommt in eine eigene
  Gruppe (etwa »Ehemalige«) und unterschreibt die Nutzungsordnung.

## Wer am Code ändert, was gespeichert wird

Dann ändern sich auch die Fakten in `privacy.rb`, und `Privacy::CHECKED`
bekommt das Datum des Abgleichs. Die Tests helfen dabei:

- `privacy_test.rb`: Die Seite rendert ohne Einstellungen, nennt jeden
  Dienst außerhalb des Servers, den der Code benutzt, und keine Seite unter
  `src/static` lädt etwas von fremden Servern.
- `account_deletion_test.rb`: Jede Art von Knoten, die `main.rb` an einen
  `User` hängt, löscht `AccountDeletion` mit, oder sie steht ausdrücklich in
  `NOT_OWNED`.
- `privacy_documents_test.rb`: Jede Unterlage rendert, nennt alle Daten und
  Maßnahmen aus `privacy.rb` und sagt nirgends »du«.

Nicht in `privacy.rb` steht, was eine Gruppe als Einstellung trägt und
nichts über eine Person sagt: ihre Programmiersprache, die angehefteten
Aktivitäten und die Adressen ihrer Klassen-Repositorys (`GroupSetting`,
`Pin`; docs/teacher-access.md, »Klassen-Repository«). Eine solche Adresse
kann den Benutzernamen der Lehrkraft auf ihrem Git-Server enthalten; sie
ist eine Angabe der Lehrkraft für ihren Unterricht, wie ein Link im
Arbeitsblatt, und geht mit der Gruppe. Den Git-Server selbst betreibt der
Workspace nicht: Er schickt ihm nichts, legt dort keine Konten an, und die
Aufgaben lesen nur das Repository im Workspace. Wer dort ein Konto
bekommt, entscheidet die Lehrkraft mit ihrer Schule, wie bei jedem anderen
Dienst, den sie im Unterricht einsetzt.
