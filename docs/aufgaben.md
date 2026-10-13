# Aktivitäten, Aufgaben und Handbuch

Dieses Dokument beschreibt, wie Inhalte im Workspace aufgebaut sind und wie
Aufgaben geschrieben werden, die der Workspace selbst prüft. Es richtet sich an
alle, die Inhalte schreiben, auch an Lehrkräfte anderer Schulen mit eigener
Installation.

**Stand:** Format 1, Meilenstein M6. Aufgabenkarten auf den Seiten, alle
Prüfarten außer `scene` und `sql`, Abgaben, die Klassenübersicht, Übungen aus
Code Bites und die Startseite für Lernende. Was ab wann läuft, steht in der
Tabelle der Prüfarten.

Wer eine eigene Aktivität schreiben will, fängt mit
[aktivitaet-schreiben.md](aktivitaet-schreiben.md) an; dieses Dokument ist
das Nachschlagewerk dazu.

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
| `kind` | `projekt`, `labor`, `grundlagen`, `galerie`, `uebung` oder `reference`; steht als Etikett auf der Karte im Katalog |
| `topic` | Schlüssel eines Abschnitts aus `sections.yaml`, für die Sortierung nach Thema |
| `duration` | Freier Text, so wie er auf der Karte stehen soll |
| `requires` | Aktivitäten, die vorher sinnvoll sind |
| `reference` | Handbuch-Seiten, auf die `_uses()` verweist |
| `template` | Adresse der Vorlage zum Klonen; andere Schulen können hier einen Spiegel eintragen |
| `class_repo` | Adresse des Klassen-Repositorys, zum Beispiel für die Sandboxes; im Text `${class_repo}` |

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
| `measure` | Messwert der Lernenden gegen eine eigene Messung | Ausgabe eines Befehls | Workspace | M6 |
| `command` | Ausgabe eines Befehls der Lernenden | Ausgabe | Workspace | M6 |
| `scene` | Szene des Anaglyph-Viewers, zu einem oder mehreren Zeitpunkten | eine `.ini`-Datei | Workspace | später |
| `code` | Code-Bites-Aufgabe | Code | Code-Bites-Judge | M5 |
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

Liest nur, und zwar im laufenden Workspace der Person, als diese Person:
`git status --porcelain --branch`, `git remote` und die Zahl der Commits
(`src/ruby/include/git_check.rb`). `GIT_OPTIONAL_LOCKS=0` verhindert, dass
dabei auch nur der Index neu geschrieben wird. Der Server selbst führt nie Git
auf einem Repository von Lernenden aus. Die Rückmeldung nennt den Stand und den
nächsten Schritt mit den echten Dateinamen, zum Beispiel: »3 Dateien sind
geändert, aber nicht committet: `lena/stack.py`, … Im Terminal, im Ordner
`structures`: `git add -A` und `git commit -m "…"`. Danach `git push`.«
`pushed` vergleicht mit dem zuletzt bekannten Stand des Remotes; die Prüfung
holt nichts aus dem Netz. Ist der Workspace aus, sagt die Karte das, statt zu
raten. `min_commits` zählt nur die Commits der Person selbst, erkannt an der
E-Mail-Adresse aus `git config user.email`; ist keine eingetragen, sagt die
Karte, wie man das nachholt. Ist `repo` nur ein Ordner in einem anderen
Repository, sagt die Karte auch das.

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
angefasst. Bei `pick` wählen Lernende auf der Karte eine der passenden Dateien
aus ihrem Workspace. `requires: { html_parses: true }` lehnt eine Abgabe ab,
deren HTML-Dateien nicht zusammenpassende Tags haben, und nennt die Zeile.
`scene_loads` und `min_objects` prüft der Workspace erst mit `scene`; bis dahin
nimmt er die Abgabe an und sagt das dazu. Die Galerie (`gallery: true`) kommt
mit der Klassenübersicht.

### measure, command, scene

Beispiele stehen in `src/content/tcpip/tasks.yaml` und
`src/content/anaglyph/tasks.yaml`.

`measure` (seit M6): Lernende tragen einen Messwert ein, für jedes Feld einen.
Danach führt der Server den Befehl aus `run` selbst aus, im laufenden
Workspace der Person, als diese Person, und liest den Wert mit `extract` aus:
`ping_rtt_avg`, `ping_hops` (aus der TTL der Antwort), `ping_reply_country`
(aus der Endung des Rechners, der antwortet), `traceroute_hops` oder
`http_header:<Name>`. Zahlen vergleicht er mit `tolerance` oder
`tolerance_percent`; Werte unter 1 gelten als gleich, wenn beide unter 1
liegen (Loopback). Texte vergleicht er ohne Groß- und Kleinschreibung,
`nginx` passt zu `nginx/1.24.0`, `Belgium` zu `Belgien`. Die Rückmeldung
nennt den eigenen Messwert: »8.8.8.8: Du hast 40 ms eingetragen, meine Messung
ergibt gerade 10,2 ms.« Der Befehl kommt immer aus der Aufgabe, nie von den
Lernenden. Im Prüfungsmodus gibt es kein Netz; deshalb ist `measure` dort
gesperrt.

`command` (seit M6): Lernende tippen einen Befehl. Er läuft in ihrem eigenen
Workspace, als sie selbst, in `/workspace`, mit dem Zeitlimit aus `timeout`
(höchstens 30 Sekunden), so als hätten sie ihn im Terminal eingegeben. Die
Ausgabe (höchstens 64 kB) wird mit `output` verglichen: `lines`, `matching`,
`not_matching`; Rückmeldungen mit `when: { output_matches: … }`. Unter »Was
der Workspace gelesen hat« steht der Befehl mit seiner Ausgabe.

`scene` braucht das Werkzeug `scene-dump` aus dem Anaglyph-Projekt und kommt
später.

## Beispiel: Begleiter für eine Sandbox

Die Sandboxes bleiben eigenständig. Eine kleine Aktivität im Workspace kann
trotzdem begleiten, besonders wenn neue Lernende Git noch nicht kennen: Sie
liest nur, was im eigenen Workspace liegt, und sagt, was als Nächstes fehlt.
Fertige Seiten gibt es für alle drei Sandboxes: `sandbox-datenstrukturen`,
`sandbox-sortieren` und `sandbox-cpu` unter `src/content/`. Was alle drei
brauchen (SSH-Schlüssel, `git config`, abgelehnter Push), steht einmal im
Handbuch unter `reference/gitlab` und wird mit `_include()` eingebunden. Hier
der Kern der Aufgaben:

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
      repo: data-structures-sandbox/structures
      expect: { has_remote: true, committed: true, pushed: true }
```

`repo` ist das Klassen-Repository `structures` im Sandbox-Ordner, nicht die
Sandbox selbst. Die Prüfung hilft nur, wenn die Lernenden im Workspace
arbeiten; auf dem eigenen Rechner kann der Workspace nichts lesen.

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

Der Workspace prüft `answer`, `choice`, `number` und `reflection` (seit M2)
sowie `files`, `html`, `css`, `references`, `git` und `submit` (seit M3).
Karten der übrigen Arten zeigen »Zeig dein Ergebnis deiner Lehrkraft«.

Karten, die im Workspace nachsehen, nennen unter dem Text die echten Pfade, die
sie lesen (»Liest `/workspace/shower.js/index.html`«). Die Rückmeldung nennt
Datei und Zeile, und unter »Was der Workspace gelesen hat« steht, was die
Prüfung gefunden hat: eine Liste der Dateien mit Größe und Format, die Folien
mit ihren Überschriften, die Bilder mit `src` und `alt`, die Ausgabe von
`git status`.

### Wie Dateien gelesen werden

Der Server liest den Workspace-Ordner direkt (`/user/<tag>/workspace`), auch
wenn der Workspace aus ist, über `src/ruby/include/workspace_files.rb`:

- Pfade dürfen nicht absolut sein und kein `..` enthalten.
- Symbolische Links werden nie verfolgt, auch nicht in Ordnern auf dem Weg.
  Eine Karte sagt dann: »… ist ein symbolischer Link. Links liest der
  Workspace nicht.«
- Nach dem Öffnen fragt der Server den Kernel, welche Datei er wirklich
  geöffnet hat, und liest sie nur, wenn sie im Workspace liegt.
- Dateien werden höchstens bis 2 MB gelesen. Bilder werden nicht dekodiert;
  ihr Format erkennt der Server an den ersten Bytes.

HTML und CSS liest ein eigener, kleiner Parser (`html_lite.rb`,
`css_lite.rb`). Er ist absichtlich strenger als ein Browser und nennt die
Zeile, in der Tags oder Klammern nicht zusammenpassen.

### Problem-Codes

Mit `feedback: [{ when: { problem: <code> }, say: … }]` ersetzt eine Aufgabe
die eingebaute Rückmeldung für einen Fall.

| Art | Codes |
| --- | --- |
| `files` | `missing`, `symlink`, `wrong_type`, `wrong_format`, `not_executable`, `no_new_files`, `bad_name`, `format_mismatch`, `too_big` |
| `html` | `missing`, `parse_error`, `no_slides`, `first_slide_unchanged`, `too_few_slides`, `slide_without_heading`, `too_few_images`, `alt_missing`, `image_case`, `image_missing` |
| `css` | `missing`, `marker_missing`, `parse_error`, `too_few_rules` |
| `references` | `missing`, `external`, `outside_root`, `local_missing` |
| `git` | `no_repo`, `not_git`, `no_remote`, `no_identity`, `uncommitted`, `too_few_commits`, `detached`, `no_upstream`, `not_pushed` |

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

## Übungen aus Code Bites

Die Aufgaben von Code Bites stehen als Übungen im Katalog unter »Programmieren
üben« (`uebung-zahlen`, `uebung-arrays`, `uebung-strings`). Jede Karte hat die
Prüfart `code`:

```yaml
check:
  type: code
  codebite: array_sum            # Datei in src/codebites/tasks/
  languages: [python, ruby]      # freiwillig; ohne: Ruby, Python, JavaScript
```

Gelöst wird weiter im Code-Editor von Code Bites (`/codebites/<name>`), mit
seinen Tests. Besteht der Code dort alle Tests, hakt der Server jede Karte ab,
die auf diese Aufgabe zeigt, und die Klassenübersicht zeigt es. »Stand holen«
auf der Karte übernimmt Aufgaben, die vor M5 gelöst wurden. Eine neue
Code-Bites-Aufgabe erscheint als Übung, sobald sie in einer `tasks.yaml` steht.
Code Bites hat dafür keinen eigenen Knopf mehr in der Menüleiste.

## Startseite

Angemeldete Lernende sehen oben auf der Startseite »Weitermachen«: die
Aktivitäten, in denen sie schon etwas geprüft haben, mit einem Balken pro
Aufgabe (grün erledigt, gelb noch falsch, orange geändert, grau offen) und
einem Link zur nächsten offenen Aufgabe. Fertige Aktivitäten stehen darunter.
Im Katalog trägt jede Karte ihre Art (`kind`) und, wenn angefangen, den Stand
(»2/5«).

Lernende probieren viel aus; nicht alles, was sie anfangen, soll ewig oben
stehen. Mit dem Auge-Symbol blenden sie eine Aktivität aus »Weitermachen« aus.
Sie kommt von selbst zurück, sobald sie dort wieder etwas prüfen; »Wieder
zeigen« holt alle ausgeblendeten zurück.

Lehrkräfte heften in der Klassenübersicht Aktivitäten für eine Gruppe an.
Angeheftetes steht bei allen in der Gruppe ganz oben unter »Von deiner
Lehrkraft«, auch wenn sie noch nichts angefangen haben, und lässt sich nicht
ausblenden. Das geht mit jeder Aktivität aus dem Katalog, auch ohne Aufgaben.
Gespeichert wird das als `(:Pin {key, group, slug, by, ts})` und bei den
Lernenden als Liste `home_hidden` am `User`.

Die Begrüßung nimmt den Vornamen aus der Einladungsliste und lässt Titel wie
»Dr.« oder »Prof.« weg. Ohne `TASKS_ENABLED` sieht die Startseite für Lernende aus wie
bisher; nur die Etiketten der Art kommen dazu.

## Klassenübersicht

Unter `/klasse` (Menüpunkt »Klassen«) sehen Lehrkräfte für eine Gruppe und
eine Aktivität, wer welche Aufgabe gelöst hat:

| Icon (Tabler) | Bedeutung |
| --- | --- |
| `check` | gelöst |
| `send` | abgegeben oder Reflexion gespeichert |
| `x` mit Zahl | noch nicht gelöst, nach so vielen Versuchen; rot hinterlegt ab 3 Versuchen (»hängt«) |
| `refresh` | die Aufgabe hat sich geändert, die Antwort ist von vorher |
| `minus` | noch nicht bearbeitet |

Die Icons stammen aus [Tabler Icons](https://tabler.io/icons) (MIT) und liegen
als Sprite in `src/static/include/tabler/tabler.svg`; welche Icons darin sind,
steht in `icons.txt` daneben. `src/ruby/tools/build_tabler_sprite.rb` baut das
Sprite neu, `Icons.tabler('check')` (`src/ruby/include/icons.rb`) setzt eins
in eine Seite.

Die Zeile »Klasse« zählt, wie viele fertig sind und wie viele hängen; bei
Vermutungen auch, wie viele richtig vermutet haben. Eine Spalte ist gelb, wenn
mindestens ein Drittel derer, die es versucht haben, gerade falsch liegt
(»häufig falsch«), ab drei Personen. Darunter stehen die Antworten und
Reflexionen zum Nachlesen und die Abgaben zum Herunterladen.

**Jetzt in den Workspaces nachsehen** führt eine Aufgabe, die im Workspace
nachsieht (`files`, `html`, `css`, `references`, `git`), sofort für alle in
der Gruppe aus, ohne dass jemand »Prüfen« drücken muss. Das Ergebnis erscheint
blau umrandet in der Tabelle und darunter als Liste mit der Rückmeldung, die
die Person selbst sehen würde. Gespeichert wird dabei nichts; der Stand der
Lernenden bleibt, wie er ist. `git` braucht einen laufenden Workspace.

Wer was sieht:

- Administratorinnen und Administratoren sehen alle Gruppen.
- Lehrkräfte mit `+` in einer Einladungsdatei sehen diese Gruppe.
- Steht eine Adresse in mehreren Gruppen (Klasse 10b und WP 10), erscheint sie
  in jeder davon. Für alles andere im Workspace zählt weiter die erste Gruppe.
- Lehrkräfte mit `*` ohne eigene Gruppe sehen keine Klassenübersicht.

## Was gespeichert wird

In Neo4j, nur für Antworten, die zählen:

- `(:User)-[:HAS_TASK_STATE]->(:TaskState)` pro Person und Aufgabe: Aufgabe,
  Version der Prüfung, Status, letzte Eingabe, Zahl der Versuche, Status des
  ersten Versuchs, Zeitpunkt der ersten richtigen Antwort, letzte Änderung.
  Der Schlüssel ist ein Hash aus E-Mail-Adresse und Aufgabe.
- `(:TaskAttempt)-[:BY]->(:User)` pro Versuch: Aufgabe, Version, Status und
  Zeitpunkt, ohne den Text der Antwort.
- Abgaben (seit M3) als `.tar.gz` unter `/internal/submissions/`, benannt nach
  dem Schlüssel des `TaskState`. Eine neue Abgabe ersetzt die alte erst, wenn
  sie vollständig geschrieben ist. Symbolische Links werden nicht mit
  abgegeben. Lernende laden ihre Abgabe auf der Karte herunter.

Sehen können das nur die Person selbst und die Lehrkräfte ihrer Gruppen, in
der Klassenübersicht. Lernende löschen
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
