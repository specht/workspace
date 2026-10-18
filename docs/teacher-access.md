# Lehrkräfte, Gruppen und Einladungen

Die Einladungslisten liegen unter `data/invitations/*.txt` (im Container:
`/invitations/*.txt`). Die Dateien werden in alphabetischer Reihenfolge
gelesen.

```text
> Klasse 12
Lena Muster <lena@example.org>
Tom Beispiel <tom@example.org>
+ specht@example.org

> LK Informatik
lena@example.org
Mia Test <mia@example.org>
+ specht@example.org

> Lehrkräfte
Dr. Michael Specht <specht@example.org>
Lea Lehrerin <lea@example.org>
* lea@example.org
```

- `> Name` beginnt eine Gruppe.
- Normale Zeilen laden eine Person ein und machen sie zum Mitglied der Gruppe.
  Eine Person darf in mehreren Gruppen stehen (Klasse 12 und LK). Ihr Name
  muss nur in einer Zeile stehen; eine Zeile mit nur der Adresse behält ihn.
- `+ email` macht die Person zur Lehrkraft, die diese Gruppe betreut. Sie
  muss außerdem irgendwo als normale Zeile eingeladen sein, sonst kann sie
  sich nicht anmelden.
- `* email` kennzeichnet eine eingeladene Person als Lehrkraft ohne Gruppe:
  Sie sieht die Notizen für Lehrkräfte, betreut aber niemanden.
- `ADMIN_USERS` in `credentials.rb` sind Administratorinnen und
  Administratoren: Sie sehen alles.

## Einladungen importieren

Bis zum Import sind die Dateien die Einladungslisten, wie oben beschrieben.
Danach stehen Gruppen, Einladungen und Schulen in Neo4j
(`include/roster_store.rb`); Lehrkräfte verwalten ihre Gruppen dann im
Browser. Dateien, deren Name mit `_` beginnt (`_e2e.txt`,
`_tutorial_screenshots.txt`), schreiben Werkzeuge; sie werden weiter gelesen.

So geht es auf einer laufenden Installation:

1. Eine Kopie von `/invitations` auf den eigenen Rechner holen und prüfen:
   `src/scripts/check-invitation-import.rb ORDNER`. Das Skript liest die
   Dateien mit dem alten und dem neuen Leser und prüft, dass die Datensätze
   des Imports dasselbe ergeben: wer eingeladen ist, mit welchem Namen, in
   welcher Gruppe, wer was betreut, wer Lehrkraft ist. Die Reihenfolge der
   Gruppen zählt nicht. `INVITATIONS_DIR=ORDNER ruby
   src/ruby/test/roster_test.rb` macht dasselbe als Test.
2. Unter »Administration › Einladungslisten« die Vorschau ansehen: Dateien,
   Gruppen mit der Zahl ihrer Mitglieder, Einladungen, Lehrkräfte.
3. »Einladungslisten importieren«. Der Import schreibt in einer Transaktion,
   liest zurück und vergleicht mit den Dateien; bei einer Abweichung wird
   nichts übernommen, und die Abweichungen stehen auf der Seite. Die Schule
   der Installation (`OPERATOR_SCHOOL`) wird als freigegeben angelegt.
4. Die Dateien bleiben liegen. »Zurück zu den Dateien« (mit `zurück`
   bestätigen) löscht Gruppen, Einladungen und Schulen wieder aus der
   Datenbank; was seit dem Import im Browser geändert wurde, geht dabei
   verloren, die Seite sagt, ob es das gibt.

Ein Konto entsteht weiter erst bei der ersten Anmeldung; eine Einladung ist
kein Konto. Gehört eine Gruppe zu einer Schule, die noch nicht freigegeben
ist, können sich ihre Schülerinnen und Schüler noch nicht anmelden; ihre
Lehrkräfte schon.

## Gruppen im Browser (`/gruppen`)

Nach dem Import verwaltet jede Lehrkraft ihre Gruppen selbst
(`include/groups_page.rb`, `static/gruppen.html`):

- Eine Gruppe anlegen; sie gehört zur Schule der Lehrkraft. Namen müssen
  frei sein; `Administrator`, `Lehrkräfte`, die Testgruppen und Gruppen aus
  den `_`-Dateien sind reserviert.
- Adressen eintragen, eine pro Zeile, mit oder ohne Namen
  (`Roster.parse_member_lines`, auch aus einer Tabelle kopiert); ein Name,
  der später kommt, wird ergänzt, ein vorhandener nicht überschrieben.
- Lehrkräfte derselben Schule in eine Gruppe aufnehmen; eine andere Schule
  wird abgelehnt. Nur die Administration ordnet Lehrkräfte einer Schule zu.
- Willkommensmails an die Mitglieder, die noch keine bekommen haben
  (`MailTexts.welcome_student`); Antworten gehen an die Lehrkraft
  (`welcomed_at` an der Einladung). Begrenzt auf 600 je Lehrkraft und Stunde.
- Die Gruppe löschen (Name zur Bestätigung). Wer danach in keiner Gruppe
  mehr steht und keine Lehrkraft ist, ist nicht mehr eingeladen; ein Konto
  löscht das nicht (Administration › Konten ohne Einladung).
- Der Link zu den Unterlagen für die eigene Schule (datenschutz.md,
  »Partnerschulen«).

Administratorinnen und Administratoren sehen alle Gruppen und wählen beim
Anlegen die Schule, auch »Gäste«.

## Personen und Schulen (`/personen`, Administration)

`include/people_page.rb`, `static/personen.html`:

- Schulen: Name, Anschrift, Datenschutzbeauftragte Stelle, Vorschrift,
  Aufsichtsbehörde (für die Unterlagen der Schule) und die beiden Daten, die
  eine Schule freigeben: Entscheidung der Schulleitung und Vereinbarung zur
  Auftragsverarbeitung (`decision_at`, `agreement_at`; `approved_at`, sobald
  beide da sind). Die Schule der Installation ist beim Import freigegeben.
  »Gäste anlegen« macht die Schule `guests` für Erwachsene von außerhalb,
  immer freigegeben; ihre Gruppen entstehen unter `/gruppen`.
- Lehrkräfte: Adresse, Name (freiwillig) und Schule (Pflicht); eine
  Willkommensmail (`MailTexts.welcome_teacher`) mit den Links zum Einstieg,
  zu `/gruppen` und zu den Unterlagen ihrer Schule und, wenn `COST_NOTE`
  gesetzt ist, dem Hinweis zu den Kosten. »Keine Lehrkraft mehr« nimmt
  Schule und betreute Gruppen weg.
- Alle Personen mit Rolle, Schule, Gruppen und ob sie ein Konto haben.

Auf der Klassenseite stehen die Gruppen unter ihrer Schule, sobald es mehr
als eine gibt; Testgruppen und Gruppen ohne Schule zuletzt.

## Wer was sieht

| | Lernende | Lehrkraft (`*`) | Lehrkraft (`+`) | Admin |
| --- | --- | --- | --- | --- |
| Notizen für Lehrkräfte (auf den Aktivitäten) | | ✓ | ✓ | ✓ |
| Klassenseite `/klasse` für die eigenen Gruppen | | | ✓ | ✓ alle |
| Anmelde-Codes der eigenen Gruppen (Klassenseite) | | | ✓ | ✓ |
| Workspace öffnen, Nachahmen | | | ✓ eigene Gruppen | ✓ alle |
| Leistungsüberprüfungen | | | ✓ eigene Gruppen | ✓ |
| Verwaltungsseite `/admin` (Server, alle Workspaces) | | | | ✓ |

Eine Lehrkraft sieht eine Person, sobald sie **eine** ihrer Gruppen betreut,
nicht nur die erste der Person (`GroupVisibility`). Andere Lehrkräfte und
Admins kann sie weder öffnen noch ansehen (`LookIn.allowed?`).

## Hineinschauen

Auf der Klassenseite stehen bei jeder Person zwei Knöpfe:

- **Workspace öffnen** öffnet ihren Workspace in VS Code.
- **Nachahmen** zeigt die Website so, wie die Person sie sieht. Eine Leiste
  oben sagt das und führt mit »Zurück zu deinem Konto« zurück.

Vorher fragt ein Dialog nach einem Satz, warum (»Ich sehe mir deine Abgabe zu
Aufgabe 3 an.«). Der letzte Satz ist vorausgefüllt; beim Durchsehen einer
ganzen Klasse genügt er also einmal. Die Person findet den Satz bei der
nächsten Anmeldung in einer ruhigen Leiste (»Verstanden«) und danach in
ihrem Profil. Noten gehören nie hierher. Admins dürfen ohne Satz
hineinschauen (Support); dann erfährt die Person nichts.

## Testkonto

Jede Lehrkraft hat genau ein Testkonto, `adresse+test@domain`, ob mit `+`,
mit `*` oder als Admin, allein in einer eigenen **Testgruppe (Name)**. Nur
sie sieht diese Gruppe; sie steht in der Gruppenauswahl der Klassenseite
zuletzt, und auch Admins sehen die Testgruppen der anderen nicht. Damit
probiert sie aus, was Lernende sehen: Aufgaben, Abgaben, die Galerie,
Leistungsüberprüfungen; im Profil kann sie das Testkonto einer eigenen
Leistungsüberprüfung zuweisen. »Nachahmen« braucht dort keinen Satz.

**Testkonto zurücksetzen** (Klassenseite, Reiter »Lernende« der Testgruppe)
leert den Workspace des Testkontos und seine Workspaces in
Leistungsüberprüfungen, legt seine Datenbanken neu an und löscht seine
Antworten, Versuche und Abgaben. Zugewiesen bleibt es; andere Konten ändern
sich nicht. Wer sich direkt als Testkonto anmeldet, bekommt den Anmelde-Code an die
eigene Adresse ohne `+test`; nicht jedes Postfach nimmt Adressen mit `+` an.

## Einstieg für Lehrkräfte

`/lehrmaterial-einstieg` (`src/content/lehrmaterial/lehrmaterial-einstieg/`)
erklärt neuen Lehrkräften die erste Stunde: Anmeldung mit den Codes am Beamer,
die Reiter der Klassenseite, Abgaben ansehen, das Testkonto und »Nachahmen«,
die Notiz für Lehrkräfte und was der Server speichert. Die Klassenseite weist
mit »Neu hier?« darauf hin, bis die Lehrkraft den Hinweis ausblendet (in
diesem Browser); der Link neben der Überschrift bleibt. `/lehrmaterial`
führt ebenfalls dorthin. Die Bilder (16:9, doppelte Pixeldichte)
nimmt ein Skript mit den E2E-Konten unter erfundenen Namen auf und räumt
danach wieder auf; nach Änderungen an der Klassenseite neu aufnehmen:

```bash
cd e2e && HS_PICTURES=1 npx playwright test --project=pictures
```
