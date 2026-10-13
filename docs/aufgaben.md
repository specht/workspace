# Aktivitäten, Aufgaben und Handbuch

Dieses Dokument beschreibt, wie Inhalte im Workspace aufgebaut sind und wie
Aufgaben geschrieben werden, die der Workspace selbst prüft. Es richtet sich an
alle, die Inhalte schreiben, auch an Lehrkräfte anderer Schulen mit eigener
Installation.

**Stand:** Format 1, Meilenstein M1. Es gibt das Aufgabenformat und seine
Prüfung (`src/ruby/include/task_spec.rb`), Handbuch-Seiten unter
`/reference`, die Platzhalter `_include()` und `_uses()` und Notizen für
Lehrkräfte. Seiten binden noch keine Aufgaben ein, und geprüft wird noch
nichts. Was ab wann läuft, steht in der Tabelle der Prüfarten.

## Die fünf Bausteine

| Baustein | Was es ist | Wo es liegt |
| --- | --- | --- |
| Aktivität | Text zum Lesen und Mitmachen: Projekt, Labor, Grundlagen oder Galerie | `src/content/<slug>/<slug>.md` |
| Aufgabe | Ein Schritt in einer Aktivität, den der Workspace prüft | `src/content/<slug>/tasks.yaml` |
| Handbuch | Kurz und vollständig, ohne Geschichte; von vielen Aktivitäten verlinkt | `src/content/reference/<slug>/<slug>.md`, Adresse `/reference/<slug>` |
| Werkzeug | Etwas, womit man arbeitet: Workspace, Datenbanken, ER-Werkstatt, die Sandboxes, das 2D Game Studio | eigene Seiten oder eigene Projekte |
| Klasse | Gruppen aus den Einladungslisten; eine Person kann in mehreren sein | `data/invitations/*.txt` |

Die Sandboxes und das 2D Game Studio bleiben eigenständige Projekte. Der
Workspace verlinkt sie und kann lesen, was in einem Workspace liegt, ändert
aber nichts an ihnen.

## Fünf Regeln

Der Workspace soll Einzelheiten nicht verstecken: echte Dateien, echte Pfade,
speichern, Ergebnis ansehen. Aufgaben und Prüfungen halten sich deshalb an
diese Regeln.

1. **Die Prüfung liest nur.** Sie schaut sich die Dateien der Lernenden an wie
   eine Lehrkraft über die Schulter. Sie schreibt, verschiebt und repariert
   nichts und verändert keine Git-Repositories.
2. **Jede Rückmeldung zeigt, was wirklich da ist:** Dateiname, Zeile, Größe,
   Format, Pfad, Git-Stand. Nicht nur »richtig« oder »falsch«.
3. **Ergebnisse sind Dateien im eigenen Ordner.** Eine Bauaufgabe nennt die
   Datei, in der gearbeitet wird. Eine Abgabe ist eine Kopie, die die
   Lernenden sehen, ersetzen und löschen können.
4. **Beim ersten Mal ausführlich, danach eine Zeile.** Klonen, Live Server,
   `git push`: einmal Schritt für Schritt im Handbuch, in der Aktivität
   dann ein Satz mit Link. Wer etwas noch nie gemacht hat, bekommt die lange
   Fassung angezeigt.
5. **Adressen und Werkzeuge bleiben offen.** Vorschau mit URL, Terminal,
   Explorer. Nichts davon verschwindet hinter einer eigenen Oberfläche.

## Dateien einer Aktivität

```text
src/content/showerjs/
├── showerjs.md        Aktivität: Text mit Platzhaltern für Aufgaben
├── tasks.yaml         Aufgaben mit ihren Prüfungen
├── lehrkraft.md       Notiz für Lehrkräfte (nur für Lehrkräfte sichtbar)
├── lehrkraft/         Lösungsdateien (werden nie ausgeliefert)
└── *.webp             Bilder, auch aus Screenshot-Rezepten

src/content/reference/
├── sections.yaml      Katalog der Handbuch-Seiten
└── html-folien/
    └── html-folien.md
```

Dateien im Inhaltsordner werden nur ausgeliefert, wenn eine Seite sie als Bild
oder Video einbindet. Lösungen können deshalb gefahrlos neben der Aktivität
liegen.

`lehrkraft.md` wird wie die Seite gerendert und erscheint für Lehrkräfte als
aufklappbare »Notiz für Lehrkräfte« über der Aktivität. Andere sehen sie nicht.
Eine Seite ist immer `<slug>.md`; `lehrkraft.md` wird nie zur Seite.

## Metadaten

Der YAML-Block am Anfang einer Seite bekommt neue, freiwillige Felder. Seiten
ohne sie funktionieren unverändert weiter.

```html
<div class='meta'>
kind: labor
topic: networks
duration: 2 Doppelstunden
requires: [working-with-files]
reference: [netzwerk]
template: https://git.nhcham.org/specht/shower.js.git
image: tcpip.webp
</div>
```

| Feld | Bedeutung |
| --- | --- |
| `kind` | `projekt`, `labor`, `grundlagen`, `galerie` oder `reference` |
| `topic` | Schlüssel eines Abschnitts aus `sections.yaml`, für die Sortierung nach Thema |
| `duration` | Freier Text, so wie er auf der Karte stehen soll |
| `requires` | Aktivitäten, die vorher sinnvoll sind |
| `reference` | Handbuch-Seiten, auf die `_uses()` verweist |
| `template` | Adresse der Vorlage zum Klonen; andere Schulen können hier einen Spiegel eintragen |

Über jeder Seite steht ein schmaler Streifen, der sagt, was sie ist:

- **Aktivität:** die Art aus `kind` (ohne `kind`: »Aktivität«), die Dauer aus
  `duration` und Links zu den Handbuch-Seiten aus `reference` und aus
  jedem `_include()` der Seite.
- **Handbuch-Seite:** »Handbuch › Abschnitt« und »Wird verwendet in«
  mit allen Aktivitäten, die die Seite nennen oder Teile daraus einbinden.

Alles, was aus dem Handbuch stammt, ist türkis markiert: Handbuch-Seiten
selbst und die eingebetteten Kästen in Aktivitäten. Im Menü führt
»Handbuch« zur Übersicht.

## Platzhalter im Text

Wie die vorhandenen `_include_file()` und `_include_svg()` werden diese
Platzhalter ersetzt, bevor Redcarpet den Text liest.

| Platzhalter | Ergebnis | Ab |
| --- | --- | --- |
| `_task(adresse)` | Die Aufgabe `adresse` aus `tasks.yaml` an dieser Stelle | M2 |
| `_include(workspace#repository-klonen)` | Der Teil `repository-klonen` der Handbuch-Seite `workspace`, eingebettet in einem Kasten mit Link | M1 |
| `_uses(ifconfig, ping)` | Kleine Marken, die zu den Teilen `ifconfig` und `ping` der Handbuch-Seiten aus `reference` führen | M1 |

### Teile einer Handbuch-Seite

Eine Handbuch-Seite gliedert sich in Teile. Ein Teil beginnt mit einer
Markierung und endet am nächsten Teil, an `<!-- /part -->` oder am Ende der
Datei:

```markdown
<!-- part: repository-klonen -->
## Repository klonen

Klicke auf »Clone Repository« und füge die Adresse der Vorlage ein:

    ${template}
<!-- /part -->
```

- Auf der Handbuch-Seite selbst wird die Markierung zu einem Sprungziel:
  `/reference/workspace#repository-klonen`.
- Bei `_include()` fällt die erste Überschrift des Teils weg, weil die
  Aktivität ihre eigene schreibt. Relative Bildpfade werden passend
  umgeschrieben.
- `${template}` wird durch das Feld `template` der einbindenden Seite ersetzt,
  auf der Handbuch-Seite selbst durch »[Adresse der Vorlage]«. Andere
  `${…}`, etwa in JavaScript-Beispielen, bleiben unverändert.
- Ein fehlender Teil bricht nichts: Die Seite zeigt »Baustein fehlt«, und das
  Server-Log nennt Datei und Platzhalter.

## tasks.yaml

```yaml
format: 1
activity: tcpip            # muss dem Ordnernamen entsprechen
tasks:
  - id: adresse            # a–z, 0–9 und -, eindeutig in der Aktivität
    title: Deine Adresse
    prompt: |
      Wie lautet die IPv4-Adresse deines Adapters `eth0`?
    check:
      type: answer
      expect: ${workspace.ip}
    hint: Suche unter `eth0` die Zeile, die mit `inet` beginnt.
    feedback:
      - when: { matches: '^127\.' }
        say: Das ist die Adresse von `lo`. Gesucht ist die von `eth0`.
```

### Felder einer Aufgabe

| Feld | Pflicht | Bedeutung |
| --- | --- | --- |
| `id` | ja | Kennung; zusammen mit der Aktivität weltweit eindeutig: `tcpip/adresse` |
| `title` | ja | Kurzer Name, erscheint in Listen und im Klassenboard |
| `prompt` | ja | Die Aufgabe in Markdown |
| `check` | ja | Wie geprüft wird; alles, was über richtig und falsch entscheidet, steht hier |
| `file` | nein | Die Datei, in der gearbeitet wird, relativ zu `/workspace`; wird bei der Aufgabe angezeigt |
| `hint` | nein | Hinweis, den man aufklappen kann |
| `feedback` | nein | Eigene Rückmeldungen für typische Fehler |
| `prediction` | nein | `true`: Vorhersage; nur der erste Versuch zählt fürs Klassenboard, danach darf ausprobiert werden |
| `solution` | nein | Lösung; nur Lehrkräfte sehen sie |
| `exam` | nein | `false`: nie in einer Leistungsüberprüfung; Prüfarten mit Netz sind dort ohnehin gesperrt |

### Variablen

In `expect` können Werte stehen, die der Server pro Person kennt.

| Variable | Wert |
| --- | --- |
| `${workspace.ip}` | IP-Adresse des eigenen Workspace |
| `${workspace.live_url}` | Adresse des eigenen Live Servers, z. B. `https://q7k2m-5500.workspace.hackschule.de` |
| `${person.codeword}` | Ein Codewort, das für jede Person anders ist |

### Rückmeldungen

Jede Prüfung bringt eigene Rückmeldungen mit, die zeigen, was sie gelesen hat.
Mit `feedback` kommen Sätze für typische Fehler dazu. Die Bedingungen in
`when` hängen von der Prüfart ab: `answer` kennt `equals` und `matches`,
`number` kennt `equals` und `between`, `choice` kennt `picked`, `command`
kennt `output_matches`, `scene` kennt `found`, `missing` und
`source_matches`.

### Versionen

Jede Aufgabe hat eine Version, die aus ihrem `check` berechnet wird. Ändert
sich die Bewertung, ändert sich die Version, und frühere Ergebnisse erscheinen
als »geändert« statt als stilles Häkchen. Tippfehler in `title`, `prompt` oder
`hint` ändern die Version nicht.

## Prüfarten

| Art | Prüft | Liest | Läuft | Ab |
| --- | --- | --- | --- | --- |
| `answer` | Text, Adresse oder regulären Ausdruck | die Eingabe | Server | M2 |
| `choice` | Auswahl aus Antworten | die Eingabe | Server | M2 |
| `number` | Zahl mit Toleranz | die Eingabe | Server | M2 |
| `reflection` | nichts; Freitext geht ans Klassenboard | die Eingabe | Server | M2 |
| `files` | ob es Dateien gibt, Namen, Größe, Format nach den ersten Bytes, ausführbar | Dateien in `/workspace` | Server | M3 |
| `html` | Folien, Überschriften, offene Tags, Bildpfade, alt-Texte | eine HTML-Datei | Server | M3 |
| `css` | eigene Regeln, offene Klammern | eine CSS-Datei | Server | M3 |
| `references` | Adressen in `src`, `href`, `url()`, `@import`: extern oder vorhanden | HTML- und CSS-Dateien | Server | M3 |
| `git` | Remote vorhanden, alles committet, alles gepusht | `git status` eines Repositories | Workspace | M3 |
| `submit` | Abgabe: Kopie von Dateien, optional für die Galerie | Dateien in `/workspace` | Server | M3 |
| `measure` | Messwert der Lernenden gegen eine eigene Messung | Ausgabe eines Befehls | Workspace | später |
| `command` | Ausgabe eines Befehls der Lernenden | Ausgabe | Workspace | später |
| `scene` | Szene des Anaglyph-Viewers, zu einem oder mehreren Zeitpunkten | eine `.ini`-Datei | Workspace | später |
| `code` | Code-Bites-Aufgabe | Code | Code-Bites-Judge | später |
| `sql` | Ergebnis einer Abfrage | Abfrage | Datenbank | später |

»Server« heißt: Der Server liest die Datei als Text und wertet sie aus, ohne
etwas auszuführen. »Workspace« heißt: Ein festes Prüfskript läuft per
`docker exec` im Workspace der Person, mit ihren Rechten und einer Zeitgrenze.
Alles, was etwas ausführt oder dekodiert, läuft dort, nie auf dem Server: Git
(eine `.git/config` kann Befehle auslösen), Bilder, die Ausdrücke des
Anaglyph-Viewers.

### answer

```yaml
check:
  type: answer
  compare: url                 # text (Standard), url oder regex
  expect: ${workspace.live_url}/pictures/fuji.jpg
```

`expect` darf eine Liste sein. Groß- und Kleinschreibung zählt nur mit
`case_sensitive: true`; bei `url` zählt sie im Pfad immer.

### choice, number, reflection

```yaml
check: { type: choice, options: [A, B, C], correct: 1 }      # gezählt ab 0
check: { type: number, expect: 111, tolerance: 6, unit: ms }
check: { type: reflection, min_words: 20 }
```

### files

```yaml
check:
  type: files
  path: shower.js/pictures
  template: [fuji.jpg, monkey.jpg]   # Dateien der Vorlage zählen nicht als neu
  show: listing                      # gelesene Liste immer mit anzeigen
  expect:
    new_files: { min: 1 }
    name: '^[a-z0-9-]+\.(jpe?g|png|webp)$'
    format_matches_extension: true
    max_size: 500kB
```

Für eine einzelne Datei: `exists`, `executable` und `format` (z. B. `elf`).

### html, css, references

```yaml
check:
  type: html
  file: shower.js/index.html
  expect:
    parses: true
    slides: { selector: section.slide, min_own: 5, each_has: h2, template_titles: [...] }
    img: { min: 1, src_exists: case_sensitive, alt: required }
  warn: { words_per_slide: 60 }
```

`css` kennt `after_marker`, `parses` und `min_rules`. `references` kennt
`root`, `scan`, `external: none` und `local_exist: true`.

### git

```yaml
check:
  type: git
  repo: data-structures-sandbox
  expect:
    has_remote: true
    committed: true
    pushed: true
```

Liest nur: `git status --porcelain --branch` im Workspace. Die Rückmeldung
nennt den Stand und den nächsten Schritt mit den echten Dateinamen, zum
Beispiel: »3 Dateien geändert, aber nicht committet: `structures/lena/…`.
Im Terminal: `git add -A` und `git commit -m "…"`. Danach `git push`.«
`pushed` vergleicht mit dem zuletzt bekannten Stand des Remotes; die Prüfung
holt nichts aus dem Netz.

### submit

```yaml
check:
  type: submit
  paths: [shower.js]           # oder pick: anaglyph/*.ini für eine einzelne Datei
  exclude: [shower.js/.git]
  max_size: 20MB
  gallery: false
  requires: { html_parses: true }
```

Der Server kopiert die Dateien beim Abgeben. Lernende sehen ihre Abgabe, geben
jederzeit neu ab und können sie löschen. Repositories werden dabei nicht
angefasst.

### measure, command, scene

Beispiele stehen in `src/content/tcpip/tasks.yaml` und
`src/content/anaglyph/tasks.yaml`. `measure` vergleicht eine Eingabe mit
einer Messung, die der Server im Workspace selbst macht (`extract`:
`ping_rtt_avg`, `ping_hops`, `ping_reply_country`, `traceroute_hops`,
`http_header:<Name>`). `scene` braucht das Werkzeug `scene-dump` aus dem
Anaglyph-Projekt.

## Beispiel: Begleiter für eine Sandbox

Die Sandboxes bleiben eigenständig. Eine kleine Aktivität im Workspace kann
trotzdem begleiten, besonders wenn neue Lernende Git noch nicht kennen: Sie
liest nur, was im eigenen Workspace liegt, und sagt, was als Nächstes fehlt.

```yaml
activity: datenstrukturen-start
tasks:
  - id: geklont
    title: Die Sandbox ist da
    prompt: Klone dein Klassen-Repository nach `/workspace/data-structures-sandbox`.
    check:
      type: files
      path: data-structures-sandbox/structures
      expect: { exists: true }

  - id: abgegeben
    title: Committet und gepusht
    prompt: Dein Stand ist committet und gepusht, damit deine Lehrkraft ihn in »Class progress« sieht.
    check:
      type: git
      repo: data-structures-sandbox
      expect: { has_remote: true, committed: true, pushed: true }
```

Im Klassenboard sieht die Lehrkraft dann nicht nur, wer gepusht hat, sondern
auch, wer Änderungen hat und nie gepusht hat, und warum.

## Aufgaben auf der Seite

`_task(id)` auf einer eigenen Zeile setzt die Aufgabe `id` aus der
`tasks.yaml` neben der Seite als Karte an diese Stelle. Die Karten werden in
der Reihenfolge der Seite nummeriert (»Aufgabe 3«, bei `prediction: true`
»Vermutung 3«). In Codeblöcken bleibt `_task(...)` stehen. Fehlt die Aufgabe in
der `tasks.yaml`, steht an ihrer Stelle ein roter Hinweis.

Die Karte enthält nie, wogegen geprüft wird: `expect`, `correct` und
`solution` bleiben auf dem Server. Die Lösung kommt erst nach einer richtigen
Antwort mit der Rückmeldung.

Seit M2 prüft der Workspace `answer`, `choice`, `number` und `reflection`.
Karten anderer Arten zeigen bis M3 »Zeig dein Ergebnis deiner Lehrkraft«.

### Einschalten

Aufgaben sind für Lernende zunächst ausgeschaltet. Lehrkräfte sehen sie
trotzdem, mit dem Vermerk »Vorschau für Lehrkräfte«, und können sie
ausprobieren. Für alle eingeschaltet werden sie in `credentials.rb`:

```ruby
TASKS_ENABLED = true
```

Fehlt die Zeile, bleibt alles wie bisher.

### Schnittstelle

| Aufruf | Daten | Antwort |
| --- | --- | --- |
| `POST /api/task/check` | `task` (`tcpip/adresse`), `answer` | `status`, `message`, `solution`, `state` |
| `POST /api/task/states` | `activity` | gespeicherte Antworten der angemeldeten Person |
| `POST /api/task/delete` | `task` | löscht Antwort und Versuche der angemeldeten Person |

`status` ist `correct`, `wrong`, `sent` (Reflexion gespeichert), `invalid`
(nicht lesbar, z. B. keine Zahl; zählt nicht) oder `unavailable` (gerade nicht
prüfbar, z. B. Workspace aus; zählt nicht).

## Was gespeichert wird

In Neo4j, nur für Antworten, die zählen:

- `(:User)-[:HAS_TASK_STATE]->(:TaskState)` pro Person und Aufgabe: Aufgabe,
  Version der Prüfung, Status, letzte Eingabe, Zahl der Versuche, Status des
  ersten Versuchs, Zeitpunkt der ersten richtigen Antwort, letzte Änderung.
  Der Schlüssel ist ein Hash aus E-Mail-Adresse und Aufgabe.
- `(:TaskAttempt)-[:BY]->(:User)` pro Versuch: Aufgabe, Version, Status und
  Zeitpunkt, ohne den Text der Antwort. Daraus entsteht in M4 die Übersicht
  für die Klasse.
- Ab M3: Abgaben als Kopie der genannten Dateien.

Sehen können das nur die Person selbst und ihre Lehrkräfte (ab M4 in der
Übersicht der Klasse). Lernende löschen
ihre Antwort zu einer Aufgabe auf der Karte (»Meine Antwort löschen«), wann sie
wollen, auch wenn Aufgaben wieder ausgeschaltet sind. Wer ein Konto von Hand
entfernt, löscht die Aufgabendaten mit:

```cypher
MATCH (u:User {email: $email})
OPTIONAL MATCH (u)-[:HAS_TASK_STATE]->(s:TaskState)
OPTIONAL MATCH (a:TaskAttempt)-[:BY]->(u)
DETACH DELETE s, a;
```

Ändert sich die Prüfung einer Aufgabe (eine neue Version), zeigt die Karte
»Aufgabe geändert, bitte neu prüfen«, und die Zählung beginnt neu.

## Eine tasks.yaml prüfen

```bash
cd src/ruby && ruby test/task_spec_test.rb && ruby test/task_card_test.rb
```

Die Tests lesen alle `tasks.yaml` unter `src/content` und melden jede
Abweichung mit Aufgabe und Feld. Außerdem prüfen sie, dass jedes `_task()` auf
einer Seite eine Aufgabe in der `tasks.yaml` daneben hat.
