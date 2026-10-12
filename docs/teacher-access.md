# Lehrkräfte und Lehrmaterial

Die Einladungsliste unter `data/invitations/*.txt` (im Container:
`/invitations/*.txt`) trennt zwei Dinge, die bisher gekoppelt waren:
Lehrkraftstatus und Gruppenadministration.

```text
> Kurs 9a
Max Mustermann <max@example.org>
Lea Lehrerin <lea@example.org>
* lea@example.org
+ max@example.org
```

- Normale Zeilen laden Benutzer ein und ordnen sie ihrer ersten Gruppe zu.
- `* email@example.org` (alternativ `* <email@example.org>`) kennzeichnet
  einen **bereits eingeladenen** Benutzer als Lehrkraft. Dies gewährt nur
  Zugriff auf die Rubrik **Lehrmaterial**, keine administrativen Rechte.
- `+ email@example.org` ist unverändert: Die Person verwaltet die aktuelle
  Gruppe und gilt ebenfalls als Lehrkraft. `*` ist dann überflüssig.
- Die in `ADMIN_USERS` eingetragenen Benutzer gelten ebenfalls als Lehrkräfte.
- Markierungen ohne passende Einladung berechtigen nicht zur Anmeldung.

Der Button **Lehrmaterial** steht für Lehrkräfte rechts im Kopfmenü. Unter
`/lehrmaterial` findet sich eine eigene Übersicht mit denselben Tutorial-Karten
wie auf der Startseite. Die normale Startseite zeigt ausschließlich den
Schülerkatalog aus `src/content/sections.yaml`.

Die Lehrerübersicht wird unabhängig davon über
`src/content/lehrmaterial/sections.yaml` gepflegt. Die dort aufgeführten
Markdown-Artikel liegen unter `src/content/lehrmaterial/<slug>/<slug>.md`.
Der Slug beginnt mit `lehrmaterial-` und ist weltweit eindeutig. Neue Rubriken
und Artikel erscheinen automatisch im Lehrerbereich; die gewohnten
Markdown-, Inhaltsverzeichnis-, Bild- und Screenshot-Mechanismen werden geteilt.

Nicht angemeldete Benutzer werden bei geschützten Seiten zur Anmeldung
weitergeleitet; andere angemeldete Benutzer erhalten HTTP 403. Die bisherigen
Verwaltungsrechte und Funktionen für Leistungsüberprüfungen bleiben
unverändert an `+` bzw. `ADMIN_USERS` gebunden.

Die Artikel verwenden das vorhandene `<!-- tutorial-screenshot ... -->`-System.
Jeder Artikel erzeugt seinen eigenen Screenshot im Entwicklungsmodus; der
entsprechende Screenshot dient nach erfolgreicher Erzeugung auch als Kachelbild.
Der Snapshot-Dienst benötigt nach Änderungen an `server.js` einen Neubau:

```bash
./config.rb up -d --build tutorial_screenshots
```

Die Sandbox-Beispiele verwenden die Ports 18080, 18081 und 18765 im
**isolierten Screenshot-Workspace**, nicht auf dem Server-Host. Die erzeugten
`.webp`-Dateien und `.tutorial-screenshots.json` müssen nach Prüfung ins
Repository aufgenommen werden. Bei noch fehlenden Screenshots erscheint
vorübergehend ein neutrales Kartenbild.
