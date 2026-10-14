# Aktivitäten, Aufgaben und Handbuch

Dieses Dokument beschreibt, wie Inhalte im Workspace aufgebaut sind und wie
Aufgaben geschrieben werden, die der Workspace selbst prüft. Es richtet sich an
alle, die Inhalte schreiben, auch an Lehrkräfte anderer Schulen mit eigener
Installation.

**Stand:** Format 1, Meilenstein M6. Aufgabenkarten auf den Seiten, alle
Prüfarten, Abgaben, die Klassenübersicht, Übungen aus
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
| Werkzeug | Etwas, womit man arbeitet: Workspace, Datenbanken, ER-Werkstatt, die Sandboxes | eigene Seiten oder eigene Projekte |
| Klasse | Gruppen aus den Einladungslisten; eine Person kann in mehreren sein | `data/invitations/*.txt` |

Die Sandboxes bleiben eigenständige Projekte. Der Workspace verlinkt sie und kann lesen, was in einem Workspace liegt, ändert
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
template_name: der shower.js-Vorlage
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
| `template` | Adresse der Vorlage zum Klonen (`https://…`); andere Schulen können hier einen Spiegel eintragen. Im Text `${template}`; der Workspace sagt Lernenden, wenn es eine neue Version gibt (siehe unten) |
| `template_name` | Name der Vorlage im Genitiv, für »Es gibt eine neue Version der CPU Sandbox!«; ohne das Feld: »… von `cpu-sandbox`« |
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
| `_include(gitlab#git-name, done: git-name)` | Wie oben; ist die Aufgabe `git-name` auf dieser Seite gelöst, zeigt der Kasten beim nächsten Laden nur noch eine Zeile | M7 |
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
- Mit `done: id` faltet sich der Kasten, sobald die Aufgabe `id` gelöst ist:
  »✓ Git sagen, wer du bist: erledigt, Aufgabe 2 ist gelöst. Anzeigen«. Das
  passiert nur beim Laden der Seite, nie während jemand liest. Die Aufgabe
  muss auf derselben Seite stehen (ein Test prüft das). So gilt auch für
  eingebundene Anleitungen: beim ersten Mal ausführlich, danach eine Zeile.
- Ohne `done:` faltet sich ein Kasten, wenn die Person ihn schon kennt: wenn
  sie auf einer anderen Seite, die denselben Teil einbindet, mindestens eine
  Aufgabe gelöst hat. Statt des Kastens steht dann eine Zeile, zum Beispiel
  »Kein Ordner offen (Strg+K, dann F), Seitenleiste zu (Strg+B), Terminal auf
  (Strg+J). Kennst du schon aus »Versionsverwaltung mit Git«. Anleitung
  anzeigen«. Die Zeile schreibt der Teil selbst, direkt unter seiner
  Markierung: `<!-- short: … -->` (nur `Code` wird ausgezeichnet, `${template}`
  wird ersetzt). Ohne `short:` steht dort die Überschrift des Teils. Wer nicht
  angemeldet ist oder keine Aufgaben sieht, bekommt immer den ganzen Kasten.
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
kennt `output_matches`, `scene` kennt `found`, `missing`,
`source_matches` und `problem`.

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
| `scene` | Szene des Anaglyph-Viewers, zu einem oder mehreren Zeitpunkten | eine `.ini`-Datei | Workspace | M6 |
| `code` | Code-Bites-Aufgabe | Code | Code-Bites-Judge | M5 |
| `sql` | Ergebnis einer `SELECT`-Abfrage gegen das einer Musterabfrage | Abfrage | eigene MySQL-Datenbank, nur lesend | M6 |
| `er` | ER-Modell: Entitäten, Schlüssel, Attribute, Beziehungen, Kardinalitäten und die Tabellen, die daraus werden | eine `.er`-Datei | Workspace (ER-Werkstatt) | M7 |
| `schema` | Tabellen der eigenen Datenbank: Spalten, Primär- und Fremdschlüssel, Zeilenzahl | `information_schema` | eigene MySQL-Datenbank, nur lesend | M7 |
| `keyboard` | ein Kapitel des Keyboard-Tutorials: alle Schritte abgehakt | `/workspace/.hs-kbd-tutorial/.state.json` | Server | M7 |

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

Jede Person sieht die Optionen einer `choice`-Aufgabe in ihrer eigenen
Reihenfolge, immer derselben (Seed: E-Mail-Adresse und Aufgabe, siehe
`TaskCard.option_order`). »Die dritte Antwort« lässt sich so nicht
weitersagen. Nur die Plätze ändern sich: `correct`, `picked` in `feedback`
und die gespeicherten Antworten zählen weiter in der Reihenfolge aus
`tasks.yaml`. Wo die Reihenfolge selbst etwas bedeutet (»Alle oben
genannten«), schaltet `shuffle: false` das ab.

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

Ohne `repo` und nur mit `identity: true` fragt die Karte, wer Git zu sein
glaubt: `git config user.name` und `user.email`, gelesen in `/workspace` im
Container der Person. Fehlt eins, nennt sie die beiden Befehle; stehen dort
noch die Beispielwerte aus dem Handbuch (»Vorname Nachname«,
»deine.mail@example.org«), sagt sie das (`problem: placeholder`). Mit `repo`
zusammen wird `identity` nach dem Repository geprüft.

```yaml
check:
  type: git
  expect:
    identity: true
```

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
aus ihrem Workspace; die Liste wird beim Anklicken und beim Zurückkehren zur
Seite neu gelesen. `requires: { html_parses: true }` lehnt eine Abgabe ab,
deren HTML-Dateien nicht zusammenpassende Tags haben, und nennt die Zeile.
`requires: { scene_loads: true, min_objects: 5 }` lädt eine abgegebene Szene
mit dem Viewer der Person (wie bei `scene`) und lehnt sie ab, wenn der Viewer
Fehler meldet oder weniger Objekte findet. Läuft der Workspace nicht, nimmt der
Server die Abgabe an und sagt dazu, dass die Szene nicht geprüft wurde. Die Galerie (`gallery: true`) kommt
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
Workspace in `/workspace`, mit dem Zeitlimit aus `timeout` (höchstens 30
Sekunden), aber nicht als sie selbst, sondern als Gast (`nobody`, `HOME=/tmp`):
Er kann lesen und ins Netz, aber in `/workspace` nichts anlegen, ändern oder
löschen, und keine Prozesse der Person beenden. Ein `rm -rf ~` in der Karte
scheitert an fehlenden Rechten (Regel 1). Dateien, die nur für die Person
lesbar sind (etwa `~/.ssh`), sieht der Gast nicht. Die
Ausgabe (höchstens 64 kB) wird mit `output` verglichen: `lines`, `matching`,
`not_matching`; Rückmeldungen mit `when: { output_matches: … }`. Unter »Was
der Workspace gelesen hat« steht der Befehl mit seiner Ausgabe.

`scene` (seit M6): prüft eine Szene des Anaglyph-Viewers. Der Server liest
dafür nichts selbst aus: Er schickt ein kleines Skript (`scene_dump.js`) mit
`node` in den laufenden Workspace der Person, als diese Person. Es lädt den
Viewer, der neben der Szene liegt (`anaglyph/include/script.js`), liest die
Szene mit dessen eigenem Parser und rechnet aus, wo jedes Objekt steht, wie
groß es ist und wie es gedreht ist, zu den Zeitpunkten aus `at_times`
(Standard: nur `t = 0`). Gezeichnet wird nichts, und es ist genau der Viewer,
den die Person auch in der Vorschau sieht; Fehlermeldungen des Viewers
erscheinen wörtlich in der Rückmeldung. Ausdrücke wie `t * 30` laufen nur im
Workspace, mit Zeitlimit.

```yaml
check:
  type: scene
  file: anaglyph/sonnensystem.ini
  ignore: [grid]                # Formen, die nicht mitzählen
  at_times: [0, 1, 2, 3]
  background: black             # Farbfamilie des Hintergrunds
  expect:
    - { label: Sonne, shape: sphere, color: [yellow, orange], moves: false }
    - { label: Erde, shape: sphere, color: [blue, cyan], orbits: Sonne }
  source:
    contains: [group]           # Wörter, die im Text vorkommen müssen
```

Jeder Eintrag in `expect` wählt Objekte aus und verlangt etwas von ihnen.
Ausgewählt wird mit `shape`, mit `color` (eine Farbfamilie oder eine Liste
davon) und mit `name` für `name = …` in der Szene. `label` ist nur der Name in
den Rückmeldungen (»Ich finde keine Kugel in Gelb oder Orange für »Sonne««)
und das Ziel von `orbits`.

Farben statt Namen: Das Tutorial führt `name` nirgends ein, Farben dagegen
schon, und eine gelbe Sonne ist eine natürliche Aufgabe. `scene_dump.js`
verfolgt `fill` wie der Viewer, als Stift, der für alle folgenden Objekte gilt
und den eine Gruppe beim `end` zurücksetzt; `background` gilt für die ganze
Szene. Der Server ordnet jede Farbe einer Familie zu
(`src/ruby/include/scene_color.rb`): `white`, `grey`, `black`, `red`,
`orange`, `brown`, `yellow`, `green`, `cyan`, `blue`, `purple`, `pink`. So
zählt `gold`, `#f9c80e` und `1, 0.8, 0` gleichermaßen als gelb. Unter »Was der
Workspace gelesen hat« steht bei jedem Objekt, was geschrieben wurde und was
daraus wurde: »Farbe gold (#ffd700, Gelb)«.

| Feld | Verlangt |
| --- | --- |
| `count`, `min` | genau so viele, mindestens so viele |
| `at: [x, y, z]` | mit `count`: alle dort; sonst: eines dort |
| `size: [b, h, t]` | Größe nach dem Skalieren (Würfel 50, Kugel 100, …), 2 % Toleranz |
| `bottom_y` | Unterkante auf dieser Höhe (»liegt auf dem Gitter«) |
| `rotate_z: { spacing, modulo }` | Drehungen um z in gleichen Abständen |
| `lattice: [3, 3, 3]`, `min_spacing` | vollständiges Gitter, Mindestabstand |
| `rows` | so viele gleich lange Reihen, keine zwei Objekte an derselben Stelle |
| `moves: true/false` | bewegt sich zwischen den Zeitpunkten oder nicht |
| `orbits: <label>` | Abstand zu dem Objekt, das der Eintrag mit diesem `label` findet (oder zum Objekt mit `name = …`), bleibt gleich, die Richtung ändert sich |

Positionen haben eine Toleranz von 2 Einheiten. Unter »Was der Workspace gelesen
hat« steht jedes Objekt mit Zeile, Position und Größe. Die Karte nennt Datei
und Viewer: »Liest `/workspace/anaglyph/balken.ini` mit deinem Viewer
`/workspace/anaglyph/include/script.js`«.

### sql

Seit M6. Beispiele stehen in `src/content/mysql/tasks.yaml` (»Einführung in
SQL«, Beispieldaten `terra1.sql`).

```yaml
check:
  type: sql
  expect_query: SELECT Name, Einwohner, Hauptstadt FROM land ORDER BY Einwohner DESC
  sorted_by: 2 DESC            # Reihenfolge zählt, Gleichstände egal
  tables: { land: 197 }        # die Beispieldaten müssen so da sein
  setup: Lade zuerst die Beispieldaten, wie unter »Beispieldaten laden« beschrieben.
feedback:
  - when: { same_result_as: "SELECT COUNT(*) FROM land WHERE Kontinent = 'Europa'" }
    say: Das sind nur die Länder mit `Europa`.
  - when: { query_matches: 'order\s+by\s+einwohner\s*;?\s*$' }
    say: Sortiert ist es, aber aufsteigend.
```

Lernende schreiben eine Abfrage in ein Textfeld (<kbd>Strg</kbd>+<kbd>Enter</kbd>
prüft). Der Server führt sie in **ihrer eigenen** MySQL-Datenbank aus,
angemeldet als sie selbst, nie als `root`, und dort auch die Musterabfrage aus
`expect_query`. Verglichen werden die Ergebnisse, nicht der Text der Abfrage:
`COUNT(name)` statt `COUNT(*)`, andere Spaltennamen, eine andere Reihenfolge
der Spalten oder eine Unterabfrage statt `ORDER BY … LIMIT 1` zählen genauso.

Nur lesen (Regel 1):

- Angenommen wird genau eine Abfrage, die mit `SELECT` oder `WITH` beginnt.
  Alles andere (`DELETE`, `DROP`, zwei Befehle mit `;`, `INTO OUTFILE`,
  ausführbare Kommentare `/*! … */`) lehnt der Server ab, ohne es zu senden.
- Sie läuft in `START TRANSACTION READ ONLY` mit `max_execution_time` von
  5 Sekunden; danach wird die Verbindung geschlossen, MySQL rollt zurück.
- Höchstens 5000 Zeilen werden gelesen.

| Feld | Bedeutung |
| --- | --- |
| `expect_query` | Musterabfrage, oder eine Liste davon; eine passende reicht |
| `compare` | `same` (Standard): dieselben Zeilen. `subset`: jede Zeile ist eine der Musterzeilen, keine doppelt (»Finde drei Orte …«), mit `rows` für die Anzahl |
| `sorted_by` | Spalten des Musterergebnisses, nach denen sortiert sein muss, z. B. `2 DESC` oder `1, 3`. Die Richtung kommt aus der Musterabfrage; gleiche Werte dürfen in beliebiger Reihenfolge stehen. Ohne das Feld ist die Reihenfolge egal |
| `extra_columns` | Mehr Spalten als nötig: `wrong` (Standard: nur die gefragten Spalten), `note` (richtig mit Hinweis) oder `ok`. Wer mehrere Spaltenauswahlen gelten lassen will, gibt mehrere `expect_query` an |
| `tables` | Tabellen und ihre Zeilenzahl in den Beispieldaten (`~` für »egal«). Fehlt eine oder stimmt die Zahl nicht, wird nicht geprüft: Sonst wäre nach einem `DELETE` eine leere Antwort richtig |
| `setup` | Satz, wie man die Beispieldaten lädt; steht bei fehlenden Daten dabei |

Rückmeldungen: `same_result_as` (das Ergebnis gleicht dem einer typischen
falschen Abfrage) und `query_matches` (regulärer Ausdruck auf den Text, ohne
Groß- und Kleinschreibung; greift auch bei Fehlermeldungen von MySQL, etwa
`6,046` statt `6.046`). Sonst sagt die Karte, was nicht passt: zu viele oder
zu wenige Zeilen oder Spalten, eine Zeile, die nicht dazugehört (aus dem
eigenen Ergebnis), die Reihenfolge. Fehlermeldungen von MySQL erscheinen
wörtlich, mit einem Satz dazu für die häufigen (Syntax, unbekannte Tabelle
oder Spalte, `GROUP BY`). Unter der Rückmeldung steht immer das eigene
Ergebnis als Tabelle, die ersten 10 Zeilen, so wie MySQL es liefert;
Werte aus der Datenbank werden dabei maskiert. Lehrkräfte sehen die Abfragen
in der Klassenübersicht.

Kann die Karte nicht prüfen, sagt sie genau, woran es liegt, und klappt das
Nachgesehene auf: welche Datenbank auf welchem Server mit welcher Anmeldung,
die Meldung von MySQL (Anmeldung abgelehnt, Datenbank unbekannt, Server nicht
erreichbar), welche Tabellen es gibt und wie viele Zeilen die geforderten
haben, ob die Tabelle in einer anderen eigenen Datenbank liegt, ob der
Workspace läuft und welche Datenbank `/workspace/.my.cnf` nennt (ohne
Passwort). Die Prüfung selbst braucht keinen laufenden Workspace.

### er

Seit M7. Beispiel: `src/content/filmdatenbank/tasks.yaml` (»Filmdatenbank mit MySQL«).
Lernende schreiben ihr Modell als Text in eine `.er`-Datei; die
ER-Werkstatt in VS Code zeigt es als Diagramm und erzeugt das SQL
(`src/vscode-extensions/er/README.md`). Die Prüfung liest die Datei mit dem
Kern der ER-Werkstatt (`core/er-core.js`), derselben Datei, die auch die
Erweiterung benutzt; er läuft mit `node` im Workspace der Person. Ein Modell
bedeutet so überall dasselbe.

```yaml
check:
  type: er
  file: filmdatenbank/film.er
  entities:
    - name: movie
      key: [id]
      attributes: [title, "year: int, optional"]   # wie in der .er-Datei
  relationships:
    - between: [movie, genre]
      cardinality: n:m             # in der Reihenfolge von between
    - between: [movie, crew, job]
      table: movie_crew            # verlangt `as movie_crew`
  tables:                          # was aus dem Modell wird, wie bei schema
    - name: movie_genre
      primary_key: [movie_id, genre_id]
      foreign_keys: ["movie_id -> movie.id", "genre_id -> genre"]
```

Was nicht genannt ist, ist frei: weitere Entitäten, Attribute und Beziehungen
stören nicht. Ein Attribut wird so geschrieben wie im Modell; Typ und
Markierungen (`primary key`, `optional`, `unique`) zählen nur, wenn sie
dastehen; `key` in der Aufgabe und das ältere `key` im Modell meinen dasselbe
wie `primary key`. `key: [id]` unter einer Entität nennt die Attribute ihres
Primärschlüssels.
Eine Beziehung wird an ihren Entitäten erkannt, nicht am Namen; `1:n`
zwischen `[genre, movie]` ist dasselbe wie `n:1` zwischen `[movie, genre]`,
`m` und `n` bedeuten beide »viele«. Fehler im Modell kommen zuerst, mit
Zeilennummer, so wie die Erweiterung sie unterstreicht.

Unter der Rückmeldung steht, was im Modell steht: Entitäten mit ihren
Attributen (Schlüssel fett), Beziehungen und die Tabellen, die daraus werden.

### keyboard

```yaml
check:
  type: keyboard
  section: Text markieren
```

Das Keyboard-Tutorial (die Erweiterung in `src/vscode-extensions/keyboard-tutorial`)
hakt jeden erledigten Schritt in `~/.hs-kbd-tutorial/.state.json` ab. Die
Aufgabe ist gelöst, wenn alle Schritte des Kapitels `section` abgehakt sind;
welche das sind, steht in der `tutorial/sections.yaml` der Erweiterung, aus
der sie auch gebaut wird. Die Rückmeldung nennt die Schritte, die noch
fehlen. Der Server liest nur die Datei, der Workspace muss nicht laufen; die
Klassenübersicht kann das Kapitel deshalb auch für alle auf einmal prüfen.

### schema

Seit M7. Liest den Aufbau der eigenen Datenbank aus `information_schema`,
angemeldet als die Person, nur lesend, wie `sql`. Zeilen zählt die Prüfung
nur für Tabellen mit `rows` oder `min_rows`.

```yaml
check:
  type: schema
  tables:
    - name: movie
      columns: [id, title, year]   # müssen da sein; weitere sind erlaubt
      primary_key: [id]
      min_rows: 1                  # oder rows: genau so viele
    - name: movie_genre
      foreign_keys: ["movie_id -> movie.id", "genre_id -> genre"]
  setup: Lege die Tabellen mit `mysql < film.sql` an.
```

Ein Fremdschlüssel ohne Spalte nach dem Punkt zeigt auf den Primärschlüssel
der anderen Tabelle. `setup` steht dabei, wenn Tabellen fehlen. Unter der
Rückmeldung stehen alle Tabellen der Datenbank mit Spalten und Typen,
Primärschlüssel fett, Fremdschlüssel und, wo gezählt, die Zeilenzahl. Ist die
Datenbank nicht erreichbar, sagt die Karte dasselbe wie bei `sql`.

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
| `er` | `missing`, `errors`, `missing_entity`, `key`, `missing_attribute`, `attribute`, `missing_relationship`, `cardinality`, `missing_table`, `missing_column`, `primary_key`, `foreign_key` |
| `schema` | `no_tables`, `missing_table`, `missing_column`, `primary_key`, `foreign_key`, `rows` |

Text, der nur die Karten ersetzt (etwa eine nummerierte Aufgabenliste),
steht in einem Element mit `class='tasks-off-only'` und verschwindet, sobald
die Karten zu sehen sind. Umgekehrt erscheint `class='tasks-on-only'` nur mit
den Karten. Beides muss HTML sein, weil Markdown in HTML-Blöcken nicht
gelesen wird.

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

Über allem steht »Deine Aktivitäten«, ohne Begrüßung mit Namen. Ohne `TASKS_ENABLED` sieht die Startseite für Lernende aus wie
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

## Neue Version einer Vorlage

Sandboxes, der Anaglyph-Viewer, BIF und andere Vorlagen werden
weiterentwickelt, während Lernende mit ihrem Klon arbeiten. Auf der Seite
einer Aktivität mit `template` sieht der Workspace deshalb nach, ob der Klon
in `/workspace/<Name des Repositorys>` die neueste Version enthält. Wenn
nicht, steht unter der Einleitung:

> **Es gibt eine neue Version der CPU Sandbox!** Du solltest `git pull`
> ausführen:
>
>     cd /workspace/cpu-sandbox
>     git pull

Mit einem Knopf »Erledigt? Noch einmal nachsehen« und einem Link auf den
Handbuch-Teil `workspace#neue-version`, der erklärt, was zu tun ist, wenn
`git pull` abbricht.

Der Workspace holt nie selbst etwas. So läuft die Prüfung:

1. Der Server fragt die Adresse aus `template` nach dem aktuellen Commit, so
   wie `git ls-remote` es tut (HTTPS, `<adresse>/info/refs`). Das ist immer
   die öffentliche Adresse von der Seite, nie ein Repository von Lernenden.
   Die Antwort merkt er sich 10 Minuten, einen Fehler 2 Minuten.
2. Ein festes Skript (`TemplateFreshness::SCRIPT`) läuft im laufenden
   Workspace der Person, als diese Person: Ist der Ordner ein eigenes
   Repository, zeigt `origin` auf die Vorlage, und ist der Commit schon in
   `HEAD` enthalten? Es benutzt nur lesende Befehle (`git rev-parse`,
   `git config --get`, `git cat-file -e`, `git merge-base --is-ancestor`),
   ohne Sperrdateien (`GIT_OPTIONAL_LOCKS=0`) und ohne nachzuladen
   (`GIT_NO_LAZY_FETCH=1`).
3. Einen Hinweis gibt es nur, wenn `origin` die Vorlage ist und ihr Commit
   fehlt, auch wenn er schon geholt, aber noch nicht übernommen ist. Eigene
   Commits obendrauf zählen als aktuell. Hat jemand den Ordner auf ein
   eigenes Repository umgestellt, kein Klon, Workspace aus: kein Hinweis.

Wie die Aufgaben erscheint der Hinweis nur, wenn `TASKS_ENABLED` an ist (für
Lehrkräfte immer). In der Klassenübersicht prüft **Vorlage aktuell?** dasselbe
für die ganze Gruppe und listet, wer eine alte Version hat, wer nicht geklont
hat und wessen Workspace nicht läuft. Gespeichert wird nichts.

| Aufruf | Daten | Antwort |
| --- | --- | --- |
| `POST /api/template/status` | `slug` der Seite | `status` (`outdated`, `current`, `absent`, `other`, `unavailable`), `notice` |
| `POST /api/class/template_status` | `group`, `slug` | `results` je E-Mail-Adresse, `reachable`, `folder` |

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
