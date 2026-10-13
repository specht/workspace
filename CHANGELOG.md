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
- Nachschlagen: eigener Katalog in `src/content/reference/`, Übersicht unter
  `/reference`, Seiten unter `/reference/<slug>`. Erste Seiten: »Workspace:
  Grundgriffe« und »HTML-Folien gestalten«.
- Platzhalter `_include(seite#teil)` und `_uses(teil, …)` sowie Teile in
  Nachschlage-Seiten (`<!-- part: name -->`), siehe `docs/aufgaben.md`.
- Notizen für Lehrkräfte: `lehrkraft.md` neben einer Seite erscheint für
  Lehrkräfte als aufklappbare Notiz über der Aktivität.
- Neue, freiwillige Felder im Meta-Block: `kind`, `topic`, `duration`,
  `requires`, `reference`, `template`.

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
