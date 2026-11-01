# Review of the pages

A reading of every page in the catalog, as a student would read it, for
what the checklist (docs/checkliste.md) cannot catch by grep:

- **Spoilers:** the answer to a task on the page before it (text, a picture,
  code, a widget that shows the result, the order of chapters).
- **Wording:** sloppy, elliptic or overlong sentences, unclear references,
  words a class stumbles over, the checklist's rules on terms.
- **Hard passages:** where a picture, a diagram or a step-through widget
  would explain better than a paragraph. These are proposals for the
  maintainer, collected below; nothing is built without a decision.
- **Correctness:** claims checked against the code, the examples or a source.

Clear fixes (spoilers, wording, wrong claims) are made right away, one
commit per page. A page is ticked with the date and that commit once it has
been read completely, its tasks.yaml and lehrkraft.md included. Started on
1 November 2026, section by section, »Software entwerfen« first.

## Fixes across pages

- 22 teacher notes named the class page's setting »Sprache«; it is »Sprachen
  der Klasse«, a ranked list (41d48ebc, kept by `wording_test.rb`).
- 11 Kata teacher notes named Ruby, Python and JavaScript only; the pages
  also offer Dart and Java (c417af20).

## Pages

### Erste Schritte

- [x] `about` (1 November 2026, 3be552c3): no tasks; out-of-date claims fixed:
  the languages whose functions tasks call (also JavaScript, Java, Dart,
  Prolog), »die Datenschutzbeauftragte Stelle«, »jede falsche Antwort« has
  feedback (only the common ones), the copy button sits after each step; the
  teacher note named »Sprache (Ruby oder Python)«. Checked: tips are kept in
  localStorage only, curricula of Berlin-Brandenburg and Baden-Württemberg.
- [x] `basics` (1 November 2026, 3be552c3): no tasks; a stray `data` attribute
  on the first picture; the teacher note said a checkpoint saves the whole
  Workspace, it saves the opened folder (and restoring removes newer files).
  Checked: Dark 2026 is the default theme, seven chapters in the keyboard
  tutorial, »Alle Checkpoints löschen…« exists.
- [x] `keyboard` (1 November 2026): read, nothing to change; the prediction
  »Kopieren ohne Markierung« comes before its after block.

### Katas

- [x] `pip-1` (1 November 2026, 4fa3a4fe): page, predictions and tips read,
  nothing to change (the predictions are answered only in after blocks, first
  tips give no solution, the Java variants say pause()); the teacher note's
  list of tasks lacked entrance and nested-steps.
- [x] `turtle-1` (1 November 2026): page, tasks and teacher note read, nothing
  to change on the page (the predictions on the triangle's corner and the
  91-degree spiral are answered only after them); the teacher note's »Sprache«
  is part of the fix across pages.
- [x] `kata-functions` (1 November 2026): intro, predictions and cards read,
  nothing to change (predictions answered in after blocks; the two-digit help
  after predict-rest comes before the cards that need it, as help, not as a
  solution).
- [x] `kata-decisions` (1 November 2026, 0a6b3070): intro, predictions and
  cards read; FizzBuzz »in Vorstellungsgesprächen für Programmierer«
  shortened; the teacher note counted one extra and listed no `bread`.
  Predictions are answered in after blocks, first tips give no solution.
- [x] `kata-loops` (1 November 2026, 19f23241): intro, predictions and cards
  read; the abstract's »Von der Summe bis eins« said the wrong end. Checked:
  Ausführen stops after 10 seconds (RUN_SECONDS), four perfect numbers up to
  10000, collatz from 6 in eight steps.
- [x] `pip-2` (1 November 2026, 8af0054b): page, tasks and teacher note read;
  »wie in den Katas zum Wiederholen« named a page that is called »Katas:
  Schleifen«, now a link; the teacher note credits Karel the Robot, the page
  now has a book box for it (Internet Archive). Checked: 1000 steps
  (PipWorld::MAX_STEPS), the first three spells are called by the check.
- [x] `kata-texts` (1 November 2026): intro, try boxes, cards and teacher note
  read, nothing to change (the try boxes ask without answering, the examples
  check out: vigenere('hallo welt', 'abc') is 'hbnlp yemv').
- [x] `kata-lists` (1 November 2026, 8386a4ee): intro, try box, cards and
  teacher note read; no spoilers. Added von Neumann's person box after the
  paragraph on Merge Sort, whose merge step he wrote down in 1945.
- [x] `kata-dicts` (1 November 2026, 9eb018d1): intro, try box, cards and
  teacher note read; no spoilers on the page (the ages in the example are
  right: Lovelace 36, Turing 41, Hopper 85). The teacher note named only Ruby,
  Python and JavaScript in three places.
- [x] `kata-objects` (1 November 2026, b6c0b904): intro, predictions, cards
  and teacher note read; predictions answered only in after blocks, each
  notation shown before a Kata needs it. The teacher note described the
  foreign shape for Ruby, Python and JavaScript only (Java uses an anonymous
  Shape, Dart none).
- [ ] `kata-bits`
- [ ] `kata-recursion`
- [ ] `turtle-2`
- [ ] `turtle-3`
- [ ] `kata-functional`
- [ ] `kata-search`
- [ ] `pip-3`
- [ ] `kata-tests`

### Grafik

- [ ] `anaglyph`
- [ ] `drawing`
- [ ] `fire`
- [ ] `pixelram`
- [ ] `raytracing`
- [ ] `glsl`

### Spiele & 3D-Welten

- [ ] `tic80`
- [ ] `raycaster`
- [ ] `raylib`

### Texte & Präsentationen

- [ ] `bif`
- [ ] `showerjs`
- [ ] `latex`

### Web

- [ ] `static-website`
- [ ] `svelte`
- [ ] `wetter`
- [ ] `hugo`
- [ ] `suchmaschine`
- [ ] `tracking`

### Künstliche Intelligenz

- [ ] `sprachmodell`
- [ ] `perzeptron`
- [ ] `naechste-nachbarn`

### Bilder und Kompression

- [ ] `rle`
- [ ] `huffman`
- [ ] `lzw`
- [ ] `gif`
- [ ] `jpeg`
- [ ] `steganography`

### Netze & Sicherheit

- [ ] `radio`
- [ ] `tcpip`
- [ ] `dns`
- [ ] `webserver`
- [ ] `pixelwand`
- [ ] `crypto`
- [ ] `totp`
- [ ] `nebenlaeufigkeit`

### Datenbanken

- [ ] `mysql`
- [ ] `sql-joins`
- [ ] `sql-index`
- [ ] `filmdatenbank`
- [ ] `neo4j-filmdatenbank`
- [ ] `bobby_tables`

### Wie Computer rechnen

- [ ] `logikgatter`
- [ ] `logikwerkstatt`
- [ ] `sandbox-cpu`

### Algorithmen und Datenstrukturen

- [ ] `sandbox-sortieren`
- [ ] `sandbox-datenstrukturen`
- [ ] `routenplaner`
- [ ] `laufzeit`

### Automaten und Sprachen

- [ ] `automata`
- [ ] `regex`
- [ ] `kellerautomat`
- [ ] `turingmaschine`
- [ ] `grammatiken`
- [ ] `calculator`
- [ ] `prolog`

### Software entwerfen

- [x] `check-digits` (1 November 2026, 3675f1bd): »Von Hand gerechnet« asked for
  the check digit the barcode at the top already shows (now a juice, `4 005999
  90002`); the abstract answered the first prediction (»Warum piept die
  Kasse«); »braucht es genauer« without a noun. Predictions, after blocks,
  numbers in the teacher note and the explanations checked; 122 examples pass.
- [x] `supermarkt` (1 November 2026, f7f0c678): no spoilers (the predictions need
  their own calculation, the simulation's numbers stand only in after blocks);
  »Eine normale Kasse ist selbst eine Kasse, sie bleibt also, wie sie ist«
  said plainly.
- [x] `testen` (1 November 2026, 356ee0ab): the text before »Die Grenzen von
  grade« named the classes of `grade` and how to test their boundaries, almost
  the answer; now explained with the age of majority (17 and 18). The rest was
  checked when the page was built the same day (examples, recipes with
  check-task in all three languages).
- [x] `textadventure` (1 November 2026, 7f34fbd7): the paragraph right before
  »Wer räumt den Apfel weg?« ended »Wer etwas von der Tasche will, bittet die
  Spielerin darum«, nearly the right option; the problem stays before the
  task, the answer (Kapselung, the bag in each language) moved into its after
  block. The other predictions need reasoning; maps and the sequence diagram
  are there, no illustration proposed.
- [x] `class-quiz` (1 November 2026, 4019405d): the abstract said »Ein Push wird
  abgelehnt« before the prediction about the push; »Mit Gewalt« counted
  »Dein Push geht durch« as right, but the Workspace's Git server refuses
  force pushes (`receive.denyNonFastForwards` in `git_server.rb`), so the task
  now asks about a server that allows it and the solution says both; the
  teacher note linked »Katas: Testen«, now »Testen«.

### Terminal & Git

- [ ] `working-with-files`
- [ ] `git`
- [ ] `terminal-mystery`

### Programmiersprachen

- [ ] `fortran`
- [ ] `cobol`
- [ ] `basic`
- [ ] `pascal`
- [ ] `c`
- [ ] `prolog-portrait`
- [ ] `smalltalk`
- [ ] `cpp`
- [ ] `clisp`
- [ ] `perl`
- [ ] `python`
- [ ] `lua`
- [ ] `ruby`
- [ ] `java`
- [ ] `js`
- [ ] `nasm`
- [ ] `cs`
- [ ] `go`
- [ ] `dart`
- [ ] `rust`

### Hidden: the Backrooms

Read like the others, with their own rules: they are never named or linked
outside the series, also not in what a page generates; a fix must not add
such a mention.

- [ ] `level-0-c1d8a2f7`
- [ ] `level-1-7e3a91c4`
- [ ] `level-2-b52d0f18`
- [ ] `level-3-4c9e6a2d`

## Illustrations proposed

What a page would explain better with a picture or a widget, for the
maintainer to decide. Per page: where, what the picture shows, why.

#### `check-digits`

- **»Reparieren mit einer Geraden«:** a small plot of the four stored values
  over their places 1 to 4: 2, 6 and 14 on a straight line, the 11 above it,
  a dotted arrow down to the 10 the line gives. The text has to say »wenn man
  den Platz der 11 mitzählt«, which a picture shows at once.
- **»Ein Gitter, das ein Bit repariert«:** the grid as a small widget: a click
  flips a bit, rows and columns with an odd number of ones turn red, their
  crossing is marked. It must not answer »zwei-bits« (two flips in one row)
  before that prediction: flipping a second bit only after it, or the
  widget placed after it.

#### `supermarkt`

- **Diagrams instead of text:** the page explains the three compartments, the
  multiplicities at the ends of a line and the hollow arrowhead of
  inheritance, but shows the school with its rooms and the vehicle with the
  bicycle only as `uml` text. Drawn as diagrams from the same text (the way
  `_include_er` draws ER models with the ER-Werkstatt's core), text and
  diagram side by side, a reader would see what the words describe.
- **After »kasse-vorhersage«:** one checkout as a timeline from 0 to 180
  seconds: the first customer from 0 to 90, the second arriving at 30, a
  waiting bar until 90, her service until 165. The text explains »ab wann sie
  wieder frei ist« in words; the card picture already draws such timelines.

#### `class-quiz`

- **After »push-vorhersage«:** the commits as a small graph: on the server
  A, B and Ida's I; in your clone A, B and your L. The server cannot take L
  without losing I; after `git pull` a merge M joins both. The text says »Dein
  Commit baut auf dem Stand davor auf«, which the graph shows; the card
  picture already draws commit graphs in the same style.
