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
   der Installation (`OPERATOR_SCHOOL`) wird angelegt, aber nicht als
   zugestimmt: Das ist sie erst, wenn unter `/personen` beide Daten stehen.
4. Der Import ist endgültig: Einen Weg zurück zu den Dateien gibt es nicht,
   nur die Sicherung von Neo4j. Die Dateien bleiben liegen, werden aber
   nicht mehr gelesen. Auf einer Kopie (Entwicklung) lässt sich der Import
   proben, indem man die Datenbank leert und den Dump neu einspielt
   (docs/entwicklung.md, »Datenbanken«).

Ein Konto (`User`) entsteht, sobald jemand zum ersten Mal einen Anmelde-Code
anfordert (`ensure_user_account` in `/api/request_login`), nicht erst mit
der Anmeldung; wer den Code nie eingibt, erscheint unter `/personen` als
»nicht bekannt«. Eine Einladung ist
kein Konto.

## Zustimmung der Schule, Einverständnis, ungeklärt

Wer eingeladen ist, kann sich anmelden; ob die Schule zugestimmt hat,
sperrt niemanden (Entscheidung des Betreibers: Während die Rechtslage
geklärt wird, bleibt der Unterricht, wie er ist). Die Seiten zeigen, was
offen ist:

1. Die Schule hat zugestimmt: Die Lehrkraft schickt der Administration die
   unterschriebene Entscheidung der Schulleitung und die Vereinbarung zur
   Auftragsverarbeitung; die Administration trägt beide Daten unter
   `/personen` ein. Dann ist für die ganze Schule alles geklärt.
2. Bis dahin: Liegt für eine Schülerin oder einen Schüler ein schriftliches
   Einverständnis vor (Elternbrief mit Einverständniserklärung,
   `/datenschutz/elternbrief?einverstaendnis=1`), bewahrt die Lehrkraft es
   auf und trägt das Datum unter `/gruppen` ein (`consent_at`, `consent_by`
   an der Einladung). Dafür ist sie verantwortlich; die Administration hat
   damit nichts zu tun.
3. Weder noch: geduldet, aber sichtbar (»ungeklärt« auf `/personen`, »Einverständnis
   für x von y« an der Gruppe), damit es geklärt wird.

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
  löscht das nicht (»Personen und Schulen › Aufräumen«).
- Der Link zu den Unterlagen für die eigene Schule (datenschutz.md,
  »Partnerschulen«).

Administratorinnen und Administratoren sehen alle Gruppen und wählen beim
Anlegen die Schule, auch »Gäste«. Sie finden `/gruppen` und `/personen` im
Menü »Administration«; Lehrkräfte ohne Administration haben einen eigenen
Knopf »Gruppen«. Alle Routen unter `/api/groups/*` verlangen eine Lehrkraft
oder die Administration (`teacher_or_admin!`, sonst 403) und prüfen danach
die Gruppe (`managed_group!`) oder die Person (`not_your_student`); die unter
`/api/people/*` nur die Administration (`admin_only!`, 403).

- Jede Gruppe ist eine geschlossene Zeile (Mitglieder, mit Konto,
  Einverständnis, Lehrkräfte; für die Administration zuletzt da und
  Speicher); ein Klick öffnet sie. Gruppen stehen unter ihrer Schule, die
  eigene zuerst, Gäste zuletzt; ein Suchfeld findet Gruppen, Namen und
  Adressen.
- Umbenennen (Lehrkräfte der Gruppe und Administration): Der Name ist auch
  Schlüssel der angehefteten Aktivitäten (`Pin`), der Programmiersprache
  der Gruppe (`GroupSetting`) und der Auswahl auf der Klassenseite
  (`User.class_prefs`); alles zieht in einer Transaktion mit
  (`/api/groups/rename`).
- Hinzufügen mit Vorschlägen beim Tippen, nach Name oder Adresse
  (`/api/groups/suggest`): für Lehrkräfte nur Eingeladene aus Gruppen ihrer
  Schule und Lehrkräfte ihrer Schule, für die Administration alle. Eine
  Schülerin oder ein Schüler, die schon in einer Gruppe einer anderen Schule
  stehen, werden nicht aufgenommen (wie bei »Schule wechseln«); Gruppen der
  Gäste zählen dabei nicht als andere Schule, dort dürfen dieselben Personen
  auch stehen (Workshops, Firmen, Veranstaltungen).
- Namen lassen sich in der Tabelle ändern, auch unter `/personen`.

**Schule wechseln** (nur Administration, an jeder Gruppe): für Gruppen, die
beim Import unter der Schule der Installation gelandet sind, aber zu einer
anderen gehören. Die Zielschule muss es unter `/personen` schon geben. Eine
Vorschau zeigt beide Schulen mit ihrem Stand, die Zahl der Mitglieder und
was zu entscheiden ist; erst die Bestätigung ändert etwas, und dann nur die
Schule der Gruppe (`moved_from`, `moved_at`, `moved_by` an der Gruppe, eine
Zeile im Log). Mitglieder, Namen, Konten, Workspaces, Aufgaben und Abgaben
bleiben.

- Lehrkräfte der Gruppe, die zu einer anderen Schule gehören: entweder mit
  zur Zielschule nehmen (geht nur, wenn sie keine Gruppe einer anderen
  Schule betreuen) oder aus der Gruppe nehmen. Ihre Mitgliedschaften in
  anderen Gruppen (etwa einem Informatiktag) bleiben. Die Administration
  bleibt, wie sie ist.
- Steht eine Schülerin oder ein Schüler auch in einer Gruppe einer anderen
  Schule, verweigert der Server den Wechsel: Eine Lehrkraft der Zielschule
  sähe sonst jemanden einer anderen Schule (ein Konto je Adresse). Erst aus
  einer der Gruppen nehmen.
- Ein vermerktes Einverständnis bleibt beim Wechsel erhalten.

**Gäste** (Gruppen der Schule `guests`) sind Erwachsene: Teilnehmende an
Workshops, Firmen, Ehemalige, die ihr Konto behalten. Minderjährige nehmen
nur über ihre Schule teil. Ihre Grundlage ist die Nutzungsordnung: Bei der
ersten Anmeldung zeigt die Seite eine Leiste (`Main#terms_bar_html`), und
der eigene Workspace startet erst nach »Ich bin einverstanden«
(`/api/accept_terms`, `User.terms_accepted_at` und `terms_version`).
Administration und Testkonten sind ausgenommen; wer in einen Workspace
hineinsieht, ist nicht betroffen.

## Personen und Schulen (`/personen`, Administration)

`include/people_page.rb`, `static/personen.html`:

- Schulen: Name, Anschrift, Datenschutzbeauftragte Stelle, Vorschrift,
  Aufsichtsbehörde (für die Unterlagen der Schule) und die beiden Daten, mit
  denen eine Schule zugestimmt hat: Entscheidung der Schulleitung und
  Vereinbarung zur Auftragsverarbeitung (`decision_at`, `agreement_at`;
  `approved_at`, sobald beide da sind), für die Schule der Installation
  genauso. Daneben, wie viele Schülerinnen und Schüler ein Einverständnis
  haben und wie viele ungeklärt sind. »Gäste anlegen« macht die Schule
  `guests` für Erwachsene von außerhalb (Nutzungsordnung); ihre Gruppen
  entstehen unter `/gruppen`.
- Lehrkräfte ohne Konto (»Aufräumen«): eingeladene Lehrkräfte ohne Konto,
  mit Schule und betreuten Gruppen (allein betreute hervorgehoben);
  entfernen nimmt die ganze Einladung heraus (`/api/people/remove_unused_teachers`,
  bestätigt mit der Zahl). Administration nie.
- Einladungen ohne Konto: Gruppen mit Eingeladenen, zu deren Adresse es kein
  Konto gibt (etwa aus der Liste der ganzen Schule); Lehrkräfte zählen nie
  dazu. Ausgewählte Gruppen lassen sich davon leeren, bestätigt mit der Zahl
  der Personen; wer danach in keiner Gruppe steht, ist nicht mehr
  eingeladen. Ein Konto wird dabei nicht gelöscht, es gibt keins.
- Lehrkräfte: Adresse, Name (freiwillig) und Schule (Pflicht); eine
  Willkommensmail (`MailTexts.welcome_teacher`) mit den Links zum Einstieg,
  zu `/gruppen` und zu den Unterlagen ihrer Schule und, wenn `COST_NOTE`
  gesetzt ist, dem Hinweis zu den Kosten. »Keine Lehrkraft mehr« nimmt
  Schule und betreute Gruppen weg.
- Alle Personen mit Rolle, Schule, Gruppen, wann sie zuletzt da waren und
  dem Speicher ihres Workspace (`Main#workspace_disk_usage`, aus der Messung
  von `housekeeping.rb`; hervorgehoben ab dem Fünffachen des Medians und
  mindestens 200 MB), filterbar nach Schule und Rolle, sortierbar nach »am
  längsten nicht da« und »meister Speicher«. Namen lassen sich dort ändern;
  eine einzelne Einladung ohne Konto lässt sich entfernen.
  »Zuletzt da« ist `User.last_seen` (der Tag des letzten Besuchs, seit
  2026-10-18 erfasst, höchstens ein Schreibzugriff am Tag, nicht beim
  Hineinsehen); fehlt er, die letzte Anmeldung (Ablauf der neuesten Sitzung
  minus `SESSION_LIFETIME_DAYS`) oder die letzte Antwort auf eine Aufgabe,
  was später war (`Main#activity_by_email`).

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
ihrem Profil. Noten gehören nie hierher. Für Admins gilt dasselbe (auch auf
`/admin`): Sie geben einen Grund an, und die Person sieht den Hinweis;
zusätzlich steht jeder Zugriff der Administration ein Jahr lang in
`/internal/admin-access.jsonl`. Nur das eigene Testkonto braucht keinen Satz.

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
