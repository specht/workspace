# Changelog

Alle nennenswerten Änderungen am Hackschule Workspace stehen hier. Andere
Schulen mit eigener Installation folgen getaggten Releases statt `main`.

Versionen folgen [Semantic Versioning](https://semver.org/lang/de/):

- **Major** (`2.0.0`): Eine bestehende Installation braucht einen Handgriff
  beim Update, zum Beispiel neue Pflichtwerte in der Konfiguration oder eine
  Datenmigration, die nicht von selbst läuft.
- **Minor** (`1.1.0`): Neue Funktionen und Inhalte; Update ohne Handgriff.
  Neue Funktionen sind zunächst abgeschaltet.
- **Patch** (`1.0.1`): Fehlerkorrekturen.

Jeder Eintrag sagt, was sich für Lernende, Lehrkräfte und Betreibende ändert.

## Unreleased

### Hinzugefügt

- Format für Aufgaben, die der Workspace selbst prüft: `docs/aufgaben.md`.
  Noch nicht aktiv; Seiten binden noch keine Aufgaben ein.
- `src/ruby/include/task_spec.rb` prüft `tasks.yaml`-Dateien;
  `src/ruby/test/task_spec_test.rb` prüft alle Aufgaben im Inhaltsordner.
- Erste Aufgabendateien für TCP/IP (12), Anaglyphen (12) und shower.js (11).
- Handbuch: eigener Katalog in `src/content/reference/`, Übersicht unter
  `/reference`, Seiten unter `/reference/<slug>`. Erste Seiten: »Workspace:
  Grundgriffe« und »HTML-Folien gestalten«.
- Platzhalter `_include(seite#teil)` und `_uses(teil, …)` sowie Teile in
  Handbuch-Seiten (`<!-- part: name -->`), siehe `docs/aufgaben.md`.
- Notizen für Lehrkräfte: `lehrkraft.md` neben einer Seite erscheint für
  Lehrkräfte als aufklappbare Notiz über der Aktivität.
- Neue, freiwillige Felder im Meta-Block: `kind`, `topic`, `duration`,
  `requires`, `reference`, `template`.
- Aktivitäten und Handbuch-Seiten sehen unterschiedlich aus: Über einer
  Aktivität stehen Art, Dauer und die Handbuch-Seiten, die sie nutzt; über
  einer Handbuch-Seite stehen »Handbuch › Abschnitt« und die
  Aktivitäten, die sie verwenden. Inhalte aus dem Handbuch sind türkis markiert.
- Menüpunkt »Handbuch« für alle, auch ohne Anmeldung.
- Aufgabenkarten: `_task(id)` setzt eine Aufgabe aus der `tasks.yaml` in die
  Seite. Der Workspace prüft Antworten, Auswahl, Zahlen und Reflexionen
  (`answer`, `choice`, `number`, `reflection`) und merkt sich das Ergebnis.
  Lernende können ihre Antwort jederzeit selbst löschen. Erste Aufgaben in
  »TCP/IP-Verbindungen« (5 Stück).
- Aufgaben sind für Lernende ausgeschaltet, bis eine Installation
  `TASKS_ENABLED = true` in `credentials.rb` setzt. Lehrkräfte sehen sie
  vorher schon als Vorschau.
- Aufgaben, die im Workspace nachsehen: `files` (Dateien, Formate,
  Dateinamen, Größen), `html` (Tags passen zusammen, Folien, Bilder mit
  `alt`), `css` (Klammern, Doppelpunkte, Semikolons, eigene Regeln),
  `references` (nichts aus dem Netz, alle Pfade stimmen) und `git`
  (committet und gepusht?). Die Rückmeldung nennt Datei und Zeile und zeigt,
  was gelesen wurde. Der Server folgt dabei keinen symbolischen Links und
  führt Git nur im Workspace der Person aus, nur lesend.
- Abgaben (`submit`): eine Kopie der genannten Dateien als `.tar.gz` unter
  `/internal/submissions/`; Lernende laden sie herunter, geben neu ab oder
  löschen sie.
- Neue Aktivitäten »Mit der Data Structures Sandbox arbeiten«, »Mit der
  Sorting Sandbox arbeiten« und »Mit der CPU Sandbox arbeiten« unter »Terminal
  & Git«: begleiten Einrichtung, Klonen und Veröffentlichen und prüfen am Ende
  jeder Stunde, ob alles committet und gepusht ist. Die Adresse des
  Klassen-Repositorys steht jeweils im Meta-Block (`class_repo`).
- Handbuch-Seite »Git und GitLab«: SSH-Schlüssel, Name und E-Mail für Git, was
  tun, wenn ein Push abgelehnt wird. Die drei Sandbox-Seiten binden sie ein.
- Alle elf Aufgaben in »Eine Präsentation in HTML erstellen« und die
  2048-Aufgabe in »TCP/IP-Verbindungen« stehen jetzt auf der Seite.
- Klassenübersicht unter `/klasse` (Menüpunkt »Klassen« für Lehrkräfte mit
  Gruppe): Lernende × Aufgaben einer Aktivität, wer hängt, was häufig falsch
  ist, Antworten zum Nachlesen und Abgaben zum Herunterladen. »Jetzt in den
  Workspaces nachsehen« prüft eine Workspace-Aufgabe sofort für die ganze
  Gruppe, ohne etwas zu speichern.
- Eine Adresse kann in mehreren Gruppen stehen (Klasse und Kurs); die
  Klassenübersicht zeigt sie in jeder. Für alles andere zählt wie bisher die
  erste Gruppe.
- Lehrmaterial listet die Aktivitäten mit einer Notiz für Lehrkräfte.
- Tabler Icons (MIT) als gebündeltes Sprite in `src/static/include/tabler/`,
  zunächst für die Klassenübersicht; Liste in `icons.txt`, neu bauen mit
  `src/ruby/tools/build_tabler_sprite.rb`.
- Code Bites wird zu Übungen: »Programmieren üben« im Katalog mit drei
  Übungen (Zahlen, Arrays, Strings) und 17 Aufgabenkarten der Prüfart `code`.
  Gelöst wird weiter im Code-Editor; wer dort alle Tests besteht, ist auf der
  Karte und in der Klassenübersicht abgehakt. Der Knopf »Code Bites« in der
  Menüleiste und sein Schalter im Profil entfallen; `/codebites` bleibt
  erreichbar.
- Startseite: angemeldete Lernende sehen »Weitermachen« mit ihren
  angefangenen Aktivitäten und der nächsten offenen Aufgabe. Katalogkarten
  zeigen die Art der Aktivität und den eigenen Stand.
- Prüfarten `measure` und `command`: Messwerte vergleicht der Server mit
  einer eigenen Messung im Workspace der Person (ping, traceroute, curl);
  Befehle laufen im eigenen Workspace mit Zeitlimit, die Ausgabe wird
  geprüft. Alle TCP/IP-Aufgaben außer dem Codewort stehen jetzt auf der Seite.
- Neue Neo4j-Knoten `TaskState` und `TaskAttempt` (Constraint
  `TaskState/key`, Index `TaskAttempt/task`); sie werden beim Start von selbst
  angelegt.

### Geändert

- »Eine Präsentation in HTML erstellen« ist kürzer: Der Referenzteil steht
  jetzt unter `/reference/html-folien`. Alte Links auf Abschnitte der Seite
  führen dorthin weiter. Die Mini-Folien liegen in
  `src/content/reference/html-folien/screenshots/`; `screenshot-range.mjs`
  schreibt dorthin.
- Eine Seite ist immer `<slug>.md` aus ihrem Ordner. Bisher war es die erste
  `.md`-Datei; bei allen vorhandenen Seiten ist das dieselbe.

### Für Betreibende

- Keine Änderung nötig.
