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

## Open for the maintainer

Found while reading, not fixed, because it needs a decision or a run in the
image.

- `kata-search`, »Hunderttausend Suchen« in Java and Dart: the examples have
  no fail case. Measured on the development machine, a linear search over
  `List<Integer>` takes 8.9 seconds in Java (limit 8) and 15 in Dart
  (limit 5); Java rejects it only narrowly, and a faster machine would let it
  through. A fail example for each, and perhaps 200 000 queries in Java.

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
- [x] `kata-bits` (1 November 2026, 64f034c6): intro, try boxes, predictions,
  cards and teacher note read; both predictions are answered only in after
  blocks (the next Kata sits in the after block of the overflow), the Pac-Man
  level 256 claim matches the Dossier. The teacher note listed the switched-
  off conversions for Ruby, Python and JavaScript only.
- [x] `kata-recursion` (1 November 2026, cd4b821d): intro, try box, cards and
  teacher note read; the extra said »fibonacci(70) dauert Jahre«: measured
  here, about 7 months in Ruby, 10 in Python, a month in JavaScript, now said
  per language. The teacher note named only Ruby, Python and JavaScript for
  the recursion check (RecursionCheck reads Dart and Java too).
- [x] `turtle-2` (1 November 2026, 7bc198fc): page, level widgets' texts,
  cards and teacher note read; two sentences ended on an adjective (»wächst
  wie ein echter:«, »einen schönen:«); the teacher note put the try box with
  random trees »dort« at the trees, it is under the own tree. Dates (Koch
  1904, Hilbert 1891) checked.
- [x] `turtle-3` (1 November 2026, 983f677a): page, try boxes, cards and
  teacher note read; the prediction on pop is answered only after it, the word
  lengths in the note (11, 61, 311) check out. The teacher note called the
  Kata page »Katas: Texte«.
- [x] `kata-functional` (1 November 2026, b22849ea): intro, predictions, try
  box, cards and teacher note read; the predictions are answered only in after
  blocks, the chain's 1400 and the try box's error check out. The teacher note
  named forEach only for JavaScript.
- [x] `kata-search` (1 November 2026, ea6165ac): intro, try box, cards and
  teacher note read; the extra claimed the linear search takes hours
  (measured: about 9 minutes in Ruby, 5 in Python, 9 to 15 seconds in
  JavaScript, Java and Dart); »Wer findet, gibt den Index zurück« spoke of a
  function as a person; the Java limit is eight seconds, the note said five.
  Examples (guesses, search_steps) check out. Java's narrow margin is under
  »Open for the maintainer«.
- [x] `pip-3` (1 November 2026, 48328edf): page, widget text, cards and
  teacher note read; the prediction and the BFS tie (north before west, the
  path over the top) check out. The first and only tip of »face« was the
  solution's line in four languages, now two steps as Java had; »way« and
  »escape« were both titled »Der kürzeste Weg«, escape is now »Zur letzten
  Lampe«; find said »Stelle« where the page says Position.
- [x] `kata-tests` (1 November 2026, 4ceebf38): intro, try box, cards and
  teacher note read; the try box said »und führe wieder aus« without an
  object; the cards referred to »den Texten«, a page called Katas: Strings.
  The prompts name the kinds of faulty versions on purpose (the point is the
  boundaries), no spoiler beyond that.

### Grafik

- [x] `anaglyph` (1 November 2026, 25719b0e): page, cards and teacher note
  read; checked against the viewer (specht/anaglyph). Wrong: »scale = 1,8«
  does not drop the decimals, the comma separates values (x 1, y 8); the group
  example »nach rechts verschoben« moves 50 up. Spoiler: the after block of
  »reihenfolge« ended »(und jede Skalierung auch)«, the answer of the next
  reflection on scaling. Wording: »heisst«, »ein 4 Leerzeichen«, »kein
  Strich«, »eine neue« without noun; Reset Camera goes to the file's camera
  line (verified in camera.js).
- [x] `drawing` (1 November 2026, 0daeda59): page, widget, cards and teacher
  note read; colour 19 of zughy_32 (#3978a8) was called Indigo; rand(120) of
  180 rows was »die obere Hälfte«; the teacher note counted two own boxes,
  there are three (Drei Hügel). Predictions (fluten, maske) are answered only
  in after blocks; 57600 bytes and 319, 179 check out.
- [x] `fire` (1 November 2026, dd18c94e): page, kernel figures, cards and
  teacher note read, nothing wrong on the page: the numbers check out (28 =
  #e06000, 160/32/0, 248, 32), predictions and the »begrenzen« question come
  before their answers, the PlayStation claim matches Sanglard's article. The
  teacher note named the drawing page by an old title.
- [x] `pixelram` (1 November 2026, 7efa4338): page, cards and teacher note
  read; the explanation why a simultaneous crash is a draw stood right under
  the question, now in its after block; »älter als die Heimcomputer« made
  precise (Apple II, 1977; Blockade 1976). Numbers (4, 0, 52 pixels per wave,
  58 000 corners) and the DOOM port (ports/doom) check out.
- [x] `raytracing` (1 November 2026): page (1400 lines), cards and teacher
  note read, nothing to change: the predictions (schatten-da, akne, tiefe) are
  answered after them, the numbers check out (sx −0.8861, t 4, gamma: 128 is
  about a fifth as bright, critical angle 41.8°, r0 0.04), the history
  (Whitted 1979/1980, Cook/Porter/Carpenter 1984, Schlick 1994, Haines' hollow
  ball) is sourced. Not checked: the three album covers.
- [x] `glsl` (1 November 2026, 9d8e6379): page, cards and teacher note read;
  the bullet right before »Heller, wo sie sich treffen« gave its answer (the
  glow is added), now in the after block; »Wer näher als 0,5 ist« and »Wer
  dicht an einem Tropfen vorbeiläuft« spoke of pixels and rays as persons;
  Sierpinski without ń (also on pixelram); »einen neuen abgeben« without noun.
  Numbers (1.3333, 0.984375, 1.4, 0.15, 512, 400) and the history (RenderMan,
  GeForce 3, Shadertoy 2013, elevated 2009) check out.

### Spiele & 3D-Welten

- [x] `tic80` (1 November 2026): page, cards and teacher note read, nothing to
  change: 32640 pixels and 2.28 seconds check out, the prediction (ohne-cls)
  needs reasoning, the default program's cls(13) and transparent colour 14
  match. The duplicate task »Ein Stern fällt« (ein-stern), a looser check of the step »Fallen und fangen« checks, was removed.
- [x] `raycaster` (1 November 2026, 4c83cf4d): page, cards and teacher note
  read; »320 Strahlen für ein Bild, 60 Bilder in der Sekunde. So programmierte
  John Carmack Wolfenstein 3D« read as if Wolfenstein ran at 60 (the page
  later says 30 on a 386), now said of the student's program; »konnte ein
  386er gar nicht rechnen« (it could, in software); »ein Sprite … wo er«.
  Numbers (83°, 500 steps, 1072 ramps, 24, 1.978) check out; the predictions
  have after blocks.
- [x] `raylib` (1 November 2026, 869e7c5b): page, cards and teacher note read;
  »Grafikkarten mit Tiefenpuffer gab es für zu Hause erst ab 1996« softened to
  »wurden üblich« (cards like the S3 ViRGE had one in 1995). The link /unter-
  der-welt to the 404 page is deliberate (3f4fd48e, the way into the
  Backrooms). Numbers (55 296, 0.075, 1.28, 0.81, 0.45, seven minutes for 2000
  blocks, 40 MB) and the history (Infiniminer, Straßer and Catmull 1974, BSP
  1980) check out.

### Texte & Präsentationen

- [x] `bif` (1 November 2026, 47fe6e2a): page, cards and teacher note read;
  the paragraph introducing has_key said why the start value belongs in 1.md,
  the answer of »Warum in 1.md?«, now in its after block; one »Leser:innen«
  among »Leserinnen und Leser«; »keine allgemein gültige Liste wie die zwölf
  Prinzipien der Animation« compared with something the page never mentions.
  The prediction »Was bedeutet der Punkt?« has its after block.
- [x] `showerjs` (1 November 2026, 85d9a643): page, included parts and cards
  read; the prediction »Umbenennen ist nicht umwandeln« stood right above the
  included part html#bilddateien, which says »Benennst du bild.png in
  bild.webp um, bleibt es ein PNG«; the include now sits in the after block
  (includes are expanded before AfterTask.mark). The template on
  git.nhcham.org stays, as decided.
- [x] `latex` (1 November 2026): page, cards and teacher note read, nothing to
  change: the pictures of the finished paper wait in after blocks, 2.46 s
  checks out, the »??« question is not answered before it.

### Web

- [x] `static-website` (1 November 2026, 415ef997): page, included parts and
  cards read; the sentence right under »Was ist .credit?« (»die Regel .credit
  gilt deshalb nur für ihn«) and the one under »Wann gilt die Media Query?«
  (»höchstens 700 Pixel breit«) gave the answers away; both now in after
  blocks. The Space Jam page (1996, still at /1996) checks out.
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
