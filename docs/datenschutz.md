# Datenschutz: Impressum, Datenschutzerklärung und Unterlagen für die Schule

Für Betreibende einer Installation und für alle, die am Code ändern, was
gespeichert wird.

## Was es gibt

- `/impressum`: Impressum und Datenschutzerklärung, gegliedert nach Art. 13
  und 14 DSGVO; unter jeder Überschrift steht klein, welchen Punkt sie
  erfüllt.
- `/datenschutz`: die Unterlagen für die Schule, jede zum Drucken oder als
  PDF: Information und Entscheidung für die Schulleitung, Prüfübersicht,
  Datenschutzerklärung, Eintrag für das Verzeichnis von
  Verarbeitungstätigkeiten, technische und organisatorische Maßnahmen,
  Löschkonzept, Entwurf der Vereinbarung zur Auftragsverarbeitung,
  Elternbrief, Nutzungsordnung, Nutzungsvereinbarung zum Webhosting. Die
  Seiten sind öffentlich, damit ein Link an die Schulleitung gehen kann.
- `/personen` (»Alle Personen«, »Aufräumen«): ein Konto mit allem, was
  dazugehört, löschen; Einladungen ohne Konto und Konten ohne Einladung.

Alles kommt aus denselben Angaben: `src/ruby/include/privacy.rb` (die Fakten:
was gespeichert wird, wer es sieht, wie lange, welche Dienste beteiligt sind,
welche Schutzmaßnahmen es gibt) und `privacy_documents.rb` (die Unterlagen).
Die Texte sind förmlich, ohne »du«; ein Test prüft das.

## Einstellungen in `credentials.rb`

Alle freiwillig; fehlt eine, sagt die Seite, dass die Angabe fehlt.

- `OPERATOR_NAME`, `OPERATOR_ROLE`, `OPERATOR_SCHOOL`, `OPERATOR_ADDRESS`:
  wer den Workspace betreibt, mit einer Anschrift, unter der Briefe ankommen
  (die der Schule genügt).
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
- `PARENTAL_CONSENT_REQUIRED = true`: der Elternbrief bekommt eine
  Einverständniserklärung, die Datenschutzerklärung sagt, dass Konten erst
  danach freigeschaltet werden. Das Sperren selbst ist noch nicht gebaut.
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
Fassung. Bis dahin vermerken die Lehrkräfte ein schriftliches Einverständnis
je Schülerin oder Schüler (Elternbrief mit Einverständniserklärung,
`?einverstaendnis=1`). Gesperrt wird ohne beides niemand
(teacher-access.md, »Zustimmung der Schule, Einverständnis, ungeklärt«).

## Der Weg über die Schule

1. Die Einstellungen eintragen, `LEGAL_BASIS` noch nicht.
2. Der Schulleitung den Link `/datenschutz` geben, oder die Unterlagen
   gedruckt.
3. Die Schulleitung unterschreibt die Entscheidung und die Vereinbarung zur
   Auftragsverarbeitung; der Eintrag für das Verzeichnis geht an die
   Datenschutzbeauftragte Stelle.
4. `LEGAL_BASIS = :school` eintragen, den Elternbrief verteilen.

Für die Vereinbarung braucht es Verträge zur Auftragsverarbeitung mit dem
Rechenzentrum und dem E-Mail-Dienst; die meisten Anbieter bieten sie als
Standardvertrag an (bei Hetzner in der Console).

## Löschen

- Zum Ende des Schuljahres nehmen die Lehrkräfte beendete Kurse und
  Abgänger aus ihren Gruppen; die Administration löscht unter
  »Personen und Schulen › Aufräumen › Konten ohne Einladung« alles, was dort
  steht. Daneben zeigen »Einladungen ohne Konto« je Gruppe, wie viele ein
  Konto haben und wann sie zuletzt da waren.
- Auf Wunsch: unter »Alle Personen« beim Konto auf den Papierkorb, die
  Adresse zur Bestätigung eingeben. Steht sie noch in einer Gruppe, dort
  herausnehmen.
- Gelöscht wird: der Workspace, die Workspaces der Leistungsüberprüfungen
  (und bei Lehrkräften deren Vorschau), die MySQL- und Neo4j-Datenbanken mit
  ihren Logins, die zusätzlichen MySQL-Datenbanken, Abgaben, und in Neo4j
  alles, was am Konto hängt (`include/account_deletion.rb`).
- Abgelaufene Sitzungen löscht der Server selbst, Zugriffsprotokolle
  logrotate nach 7 Tagen (`bootstrap/03-setup-storage.rb`), die Ausgaben der
  Dienste Docker nach 3 × 10 MB.
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
