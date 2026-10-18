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
