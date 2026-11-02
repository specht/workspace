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

## Where the review stands

Every page of the catalog is read, the four hidden levels last, on
2 November 2026. What is left is in »Open for the maintainer« and
»Illustrations proposed«; a page changed later is read again and its entry
updated. Answers that stand directly under or above their question go into an
`<!-- after: id -->` block (an included Handbuch part can be wrapped too);
redundant tasks are removed, since there are no student solutions yet.

## Fixes across pages

- 22 teacher notes named the class page's setting »Sprache«; it is »Sprachen
  der Klasse«, a ranked list (41d48ebc, kept by `wording_test.rb`).
- 11 Kata teacher notes named Ruby, Python and JavaScript only; the pages
  also offer Dart and Java (c417af20).
- 18 language portraits wrote »Youtube« in their closing paragraph; now
  »YouTube« (632506ee).

## Open for the maintainer

Found while reading, not fixed, because it needs a decision or a run in the
image.

Nothing at the moment.

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
  Later: »Hunderttausend Suchen« had no fail example in Java and Dart, and a
  linear search in Java took 8.9 s against a limit of 8; Java now searches
  200 000 numbers, the card is »Hunderttausende Suchen«, both languages have
  a linear search as fail example (all 169 examples pass in the image).
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
- [x] `svelte` (1 November 2026, e0c07818): page, cards and teacher note read;
  the paragraph before »Warum :global?« gave the answer (the classes come
  later through JavaScript), now in its after block; the fact the question
  needs (Svelte drops rules that match nothing) stays before. Checked: the
  current sv template keeps the adapter in vite.config.js, as the page says.
- [x] `wetter` (1 November 2026, d2cce863): page, cards and teacher note read;
  »jede Sekunde … verbraucht das Tageskontingent in gut einer Stunde« was
  wrong (3600 an hour: the hourly 5000 after about 83 minutes, the daily 10
  000 after about 2¾ hours); the sentence right before »Ein Spiegel für alle«
  (30 people, one request) and the paragraph under the API-key question gave
  the answers, now in after blocks. The mirror's claims (15 minutes,
  hit/miss/stale) checked in weather_mirror.rb; Richardson 1922, ENIAC 1950
  and the WMO codes check out.
- [x] `hugo` (1 November 2026): page, cards and teacher note read, nothing to
  change: the prediction »Ein vierter Planet« needs the loop over
  site.RegularPages and is answered after it; the try box builds on »Ein
  Doppelpunkt zu viel«; layouts follow Hugo's current template system
  (baseof.html at the top, _partials).
- [x] `suchmaschine` (1 November 2026): page, cards and teacher note read,
  nothing to change: predictions (geheim, erster) and the EU question are
  answered in after blocks; the counts (11 files, 10 found, 8 links to the
  start page, 9 with robots.txt) fit together; Brin/Page 1998, Panda 2011,
  Penguin 2012, Google Spain 2014 and the ECIR 2024 study (7392 queries; few
  reviews on the web use affiliate links, most results do) check out.
- [x] `tracking` (1 November 2026): page, cards and teacher note read, nothing
  to change: the answers (referer, zweimal, personenbezug, ablehnen-versteckt)
  wait in after blocks; the questions on § 25 TDDDG apply the rule stated
  before them; Nouwens et al. (22 to 23 points, 11.8 % of 680 banners),
  Planet49 2019, the Referer default since 2020/2021 and the DSK guidance 2024
  check out.

### Künstliche Intelligenz

- [x] `sprachmodell` (1 November 2026): page, cards and teacher note read,
  nothing to change: the predictions (naechstes-wort, immer-das-haeufigste,
  zwei-woerter, abgeschrieben, boese) are answered only in after blocks;
  Shannon 1948 and Markow 1913 check out.
- [x] `perzeptron` (1 November 2026, beda8d0e): page, cards and teacher note
  read; the Zügelpinguin was introduced twice with »(englisch Chinstrap)«.
  Predictions are answered in after blocks; 342 and 274 penguins, 43 writers
  (30 train, 13 test), McCulloch/Pitts 1943, Rosenblatt 1958, Minsky/Papert
  1969 check out.
- [x] `naechste-nachbarn` (1 November 2026): page, cards and teacher note
  read, nothing to change: every prediction (alle-nachbarn, stadtteile, ohne-
  wohnort, entfernung) is answered in an after block; Fix/Hodges 1951 and
  Gender Shades 2018 (0.8 % and 34.7 %) check out.

### Bilder und Kompression

- [x] `rle` (1 November 2026): page, widget, cards and teacher note read,
  nothing to change: the predictions (ziffern, text) are answered in after
  blocks; the widget counts two bytes per run as the text says.
- [x] `huffman` (1 November 2026): page, stepper, cards and teacher note read,
  nothing to change: the numbers fit (45 240 bytes against LZW's 28 813 and
  gzip's 20 271; 90 % needs about a seventh of a bit), Fano's term paper 1951
  and Huffman 1952 check out.
- [x] `lzw` (1 November 2026): page, both steppers, cards and teacher note
  read, nothing to change: the special case and the photo are answered in
  after blocks; LZ77/LZ78, Welch 1984 and the Unisys patent (1994, PNG 1995,
  expired 2003/2004) check out.
- [x] `gif` (2 November 2026): page, hexdumps, LZW stepper, cards and teacher
  note read, nothing to change: the bit order example (4, 1, 6), the interlace
  numbers (54 rows in pass 1, row 4 next) and the colour-table sizes check
  out; the special code is answered in its after block; Wilhite 2013 and the
  browsers' 2-hundredths rule are right.
- [x] `jpeg` (2 November 2026, 37414408): page, widgets, cards and teacher
  note read; the MCU was called »Minimal Coding Unit« (the standard says
  Minimum Coded Unit), and Y was said to have »2:2-Subsampling« though Y is
  not subsampled (factors 2 × 2, chroma 1 × 1). The DC example (11110 → 7, 7 −
  127 = −120), 10 500 MCUs for 91-baseline and the AAN claim (5
  multiplications) check out; the prediction on dequantisation has its after
  block.
- [x] `steganography` (2 November 2026, 0fe2f56c): page, widget, cards and
  teacher note read; the link to the JPEG page used the name »Ein JPEG-
  Decoder«. Predictions (sichtbar, vorbei) are answered in after blocks;
  capacity (57 596 bytes), 4.75 and almost 7 bits, Herodot and
  Westfeld/Pfitzmann 1999 check out.

### Netze & Sicherheit

- [x] `radio` (2 November 2026): page, cards and teacher note read, nothing to
  change: the discussion questions are asked without answers; Baudot (five
  keys, about 150 years ago) and Shannon 1948 check out.
- [x] `tcpip` (2 November 2026, 9e7b0e6c): page, cards and teacher note read;
  the explanation right under »Wo steht der Server wirklich?« (the ending says
  where a name is registered, not where the server is) now sits in its after
  block; all predictions already had after blocks. Berlin–Canberra (16 000 km,
  160 ms in fibre), RFC 826 (1982) and info.cern.ch (last modified December
  1992) check out.
- [x] `dns` (2 November 2026, a51081cb): page, cards and teacher note read;
  the two sentences before »Was der Root-Server sagt« gave its answer (it
  knows who is responsible for de, a referral in the AUTHORITY SECTION), now
  in its after block; the student reads it from the output first. The TTLs
  (300, 172800), Mockapetris 1983 and Kaminsky 2008 (65 536 IDs) check out.
- [x] `webserver` (2 November 2026, 72f9b665): page (Ruby and Python), cards
  and teacher note read; the paragraph introducing the lock explained the lost
  update, the answer of »Zwei auf einmal« further down; now in its after
  block, in both languages. Predictions have after blocks; RFC 2324 (1998) and
  418 kept in 2017 check out.
- [x] `pixelwand` (2 November 2026): page (Ruby and Python), cards and teacher
  note read, nothing to change: the predictions (handshake, wer-bekommt-was,
  counter) are answered after them; the RFC 6455 example key, r/place 2017
  (1000 × 1000, 16 colours, 5 to 20 minutes) and § 303b StGB check out.
- [x] `crypto` (2 November 2026): page, cards and teacher note read, nothing
  to change: every prediction (lawine, salz-vorhersage, schluesselproblem,
  geaendert, fingerabdruck) is answered in an after block; RSA sizes (27 to
  256 bytes, a bit over 200 at most), Diffie/Hellman 1976 and RSA 1977 check
  out.
- [x] `totp` (2 November 2026, 772f16fe): page (Ruby and Python), cards and
  teacher note read; the HMAC was introduced as »eine Prüfsumme wie SHA-1 aus
  Kryptographie im Terminal«, but that page uses SHA-256 and calls it a hash.
  The RFC 4226 example (75a48a19…, 287082), Base32 and RSA SecurID 2011 check
  out; predictions have after blocks.
- [x] `nebenlaeufigkeit` (2 November 2026): page (Java; Dart parallel),
  widgets, cards and teacher note read, nothing to change: every prediction is
  answered in an after block; 1680 orders for three threads, and »vier Kerne«
  (docker run --cpus=4 in workspace_runtime.rb) check out.
  Later: the dining philosophers got a picture of the table and, after the
  task, one of the deadlock as a circle; drawing it showed that the text put
  the forks in the wrong places (A takes `f1` and `f2`, which lay apart).

### Datenbanken

- [x] `mysql` (2 November 2026, d7873aaf): page, cards and teacher note read;
  the closing paragraph spoke of »im zweiten Teil unten«, but the second part
  is the page sql-joins. Predictions (reihenfolge, dichte-where, ohne-where)
  have after blocks; SQL 1974 and the seven continents (Antarktika without
  countries) check out.
- [x] `sql-joins` (2 November 2026): page, cards and teacher note read,
  nothing to change: predictions are answered in after blocks or by the
  student's own query (weiteste-berlin); Berlin–Paris 876 km, Singapur's four
  official languages and Papua-Neuguinea's one neighbour check out.
- [x] `sql-index` (2 November 2026): page, cards and teacher note read,
  nothing to change: predictions (vollstaendig, primaerschluessel, welche-
  nutzt) wait in after blocks; the arithmetic (a third of a second, three
  queries a second, 20 steps of binary search) fits.
- [x] `filmdatenbank` (2 November 2026): page, cards and teacher note read,
  nothing to change: predictions (vorhersage-n1, genre-loeschen, doppelte-
  titel, modell-verleih) are answered in after blocks; »Überlege, bevor du
  weiterliest« is followed by its answer on purpose, without a task; Nosferatu
  1922/2024 and Dune 1984/2021 check out.
- [x] `neo4j-filmdatenbank` (2 November 2026): page, cards and teacher note
  read, nothing to change: predictions (zwischentabelle, taetigkeit, ausleihe-
  graph, zweimal-create, paare) wait in after blocks; IS NODE KEY needs Neo4j
  Enterprise, which the Workspace runs (neo4j:2026.06.0-enterprise); Johnson
  and Jackson in »The Other Guys« checks out.
- [x] `bobby_tables` (2 November 2026, 29183015): page (Ruby and Python),
  cards and teacher note read; »Mit # oder -- beginnt ein Kommentar« left out
  that MySQL needs a space after --, so an injection with »--« and no space
  fails. The prediction (apostroph) has its after block; xkcd 327 credited.

### Wie Computer rechnen

- [x] `logikgatter` (2 November 2026): page, circuits, cards and teacher note
  read, nothing to change: every prediction is answered in an after block; the
  subtraction examples (5 − 3, 3 − 5 and their carry), CMOS NAND with four
  transistors and Boole/Shannon check out.
- [x] `logikwerkstatt` (2 November 2026): page, editor instructions, cards and
  teacher note read, nothing to change: each circuit's explanation waits in
  its after block; the mux and ripple-carry statements check out.
- [x] `sandbox-cpu` (2 November 2026): page (both class-repository variants),
  cards and teacher note read, nothing to change: the 6502 history (1975,
  Apple II, C64, NES, Terminator) and the overflow puzzle (0 + 255 + … + 1 ≡
  128 mod 256) check out; the think boxes ask without answering.

### Algorithmen und Datenstrukturen

- [x] `sandbox-sortieren` (2 November 2026, 7ec0b4c1): page (both class-
  repository variants), cards and teacher note read; »weiß sogar ein
  Präsident« for the 2007 Google visit, when Obama was a senator and
  candidate. Predictions and think boxes have their after blocks or stay open.
- [x] `sandbox-datenstrukturen` (2 November 2026): page (both class-repository
  variants), cards and teacher note read, nothing to change: −7 % 8 is 1 in
  Dart and Python and −7 in Java and JavaScript, as stated; the think boxes
  ask without answering.
- [x] `routenplaner` (2 November 2026): page (Ruby and Python), cards and
  teacher note read, nothing to change: the numbers fit (111 195 m per degree,
  67 km per degree of longitude in Berlin, heap height 17 for 100 000, 4! =
  24, 8! = 40 320, 7! = 5 040); the prediction on degrees has its after block.
- [x] `laufzeit` (2 November 2026): page (Ruby; the other languages parallel),
  widgets, cards and teacher note read, nothing to change: predictions wait in
  after blocks; 499 500 steps, 1,5 s · 1716 ≈ 43 minutes, Cook 1971 and the n
  log n bound check out; the 110,6 km per degree of latitude matches the code
  (110.57).

### Automaten und Sprachen

- [x] `automata` (2 November 2026): page (Ruby and Python), steppers, cards
  and teacher note read, nothing to change: 3^1000 has 1585 binary digits,
  2^30 is over a billion; the explosion prediction has its after block.
- [x] `regex` (2 November 2026): page (Ruby and Python), cards and teacher
  note read, nothing to change: every prediction is answered in an after
  block; Cloudflare 2 July 2019 (27 minutes), Ruby 3.2's memoization, Thompson
  1968 and Kleene check out.
- [x] `kellerautomat` (2 November 2026, f3302e02): page (Ruby and Python),
  cards and teacher note read; the pigeonhole step said a k-state automaton
  »war in k + 1 Zuständen«, now k + 1 visits to only k states. Predictions
  (zaehler, schubfach) wait in after blocks.
- [x] `turingmaschine` (2 November 2026): page (Ruby and Python), steppers,
  cards and teacher note read, nothing to change: every prediction waits in an
  after block; Turing 1936, Church and the halting argument are stated
  correctly.
- [x] `grammatiken` (2 November 2026): page (Ruby and Python), diagrams, cards
  and teacher note read, nothing to change: the predictions (tiefe, baeume,
  welcher-typ) wait in after blocks; Backus 1959, Naur 1960, Wirth's diagrams
  for Pascal and Chomsky 1956 check out.
- [x] `calculator` (2 November 2026, bd471779): page (Ruby and Python), tree
  stepper, cards and teacher note read; the paragraph right before »Was ein
  Automat nicht kann« gave its answer, now in its after block with a hint
  (»Denk an ((((1))))«) before. The other predictions had after blocks; ALGOL
  60 and Ruby's Rational for 2 ^ -1 check out.
- [x] `prolog` (2 November 2026): page, figures, cards and teacher note read,
  nothing to change: every prediction waits in an after block; Appel/Haken
  1976 and Prolog's origin in language processing check out.

### Software entwerfen

- [x] `check-digits` (1 November 2026, 3675f1bd): »Von Hand gerechnet« asked for
  the check digit the barcode at the top already shows (now a juice, `4 005999
  90002`); the abstract answered the first prediction (»Warum piept die
  Kasse«); »braucht es genauer« without a noun. Predictions, after blocks,
  numbers in the teacher note and the explanations checked; 122 examples pass.
  Later: the straight line as a plot (»Stelle«, not »Platz«, also in the text
  and the task), the parity grid as a widget: one flipped bit before »zwei-
  bits«, two in its after block.
- [x] `supermarkt` (1 November 2026, f7f0c678): no spoilers (the predictions need
  their own calculation, the simulation's numbers stand only in after blocks);
  »Eine normale Kasse ist selbst eine Kasse, sie bleibt also, wie sie ist«
  said plainly.
  Later: School and Room, Vehicle and Bicycle drawn under their `uml` text
  (`_uml_diagram`), the checkout as a timeline after »kasse-vorhersage«.
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
  Later: the rejected push and the merge as commit graphs; the merge picture
  stands after the conflict is resolved, since `git pull` stops at the
  conflict.

### Terminal & Git

- [x] `working-with-files` (2 November 2026, af7092fa): page, cards and
  teacher note read; the paragraph right before »Windows oder Linux?« named 0d
  0a as the Windows line end, now the student looks first and the explanation
  waits in the after block. The rm warning stays before »Und wenn rm
  danebengeht?« on purpose, it is a safety warning; the numbers (3 756 lines,
  3 002 words, 61 204 and 48 925 bytes) are the recipe's.
- [x] `git` (2 November 2026, 84fa4794): page, object graphs, cards and
  teacher note read; »Branches werden beim Push normal mit übertragen« (git
  push sends the current branch only) with an em dash, and the diff's removed
  lines were shown as »–« instead of »-«. The prediction head-zwei has its
  after block.
- [x] `terminal-mystery` (2 November 2026): page, cards and teacher note read,
  nothing to change: the case is solved by the student's own commands, the
  regex shortcut for the car stands only after the culprit is found; the
  counts (over 1200 lines, 1503 interviews, 145 Hamburg files) fit; Veltman's
  Command Line Murders credited.

### Programmiersprachen

- [x] `fortran` (2 November 2026, be941eb4): page, cards and teacher note
  read; »wie man in C Benutzereingaben verarbeitet« on the Fortran page
  (copied from the C page; the other portraits are right), and the first
  sentence made the 1957 compiler »eine der ältesten Programmiersprachen«,
  with English quotes. The overflow prediction has its after block.
- [x] `cobol` (2 November 2026, 825aff9c): page read (shared portrait
  template), nothing else to change: CODASYL 1959 and FLOW-MATIC check out.
- [x] `basic` (2 November 2026, 954561f6): page read; »BASIC ist eine
  Skriptsprache, … zur Laufzeit interpretiert« (Dartmouth BASIC was compiled),
  English quotes. The 3000000000 question was run in the image: bwBASIC prints
  the 2 nine times and stops, so the question has an answer.
- [x] `pascal` (2 November 2026): page read, nothing to change: Wirth
  1968/1970, Turbo Pascal and the FreePascal IDE are described correctly.
- [x] `c` (2 November 2026, 4df77bb5): page read, nothing else: the int
  overflow explanation (lower 32 bits, no error) is right for gcc.
- [x] `prolog-portrait` (2 November 2026): page read, nothing to change:
  Colmerauer and Roussel 1972, = versus is explained correctly.
- [x] `smalltalk` (2 November 2026): page read, nothing to change.
- [x] `cpp` (2 November 2026): page read, nothing to change: cin stores the
  largest int and sets the fail state (since C++11), 2147483647 is prime.
- [x] `clisp` (2 November 2026, 29caf3e8): SBCL was called an interpreter of a
  script language; it compiles every expression to machine code, also in the
  REPL.
- [x] `perl` (2 November 2026): page read, nothing to change.
- [x] `python` (2 November 2026): page read, nothing to change.
- [x] `lua` (2 November 2026): page read, nothing to change.
- [x] `ruby` (2 November 2026, c2698bfd): page read, only the quotes around
  the nickname.
- [x] `java` (2 November 2026): page read, nothing to change: IO.println and
  the short main are final since Java 25.
- [x] `js` (2 November 2026): page read, nothing to change.
- [x] `nasm` (2 November 2026, 1db71447): »Programmierspracen«; RollerCoaster
  Tycoon (1999) was listed among early games; the prose quoted »Hello, world!«
  where the code prints »Hello, World!«.
- [x] `cs` (2 November 2026): page read, nothing to change: .NET 10 is what
  the image installs.
- [x] `go` (2 November 2026): page read, nothing to change.
- [x] `dart` (2 November 2026): page read; only »macOS«: the analytics switch-
  off runs at every start (init-code-server-run).
- [x] `rust` (2 November 2026): page read, nothing to change.

### Hidden: the Backrooms

Read like the others, with their own rules: they are never named or linked
outside the series, also not in what a page generates; a fix must not add
such a mention.

- [x] `level-0-c1d8a2f7` (2 November 2026, 3df6bf71): »floor zehn« was eleven
  (FLOORS in level.js); the starter's comment listed six walls and five floors
  (fixed and pushed there, f20fabd); the door question checked against the
  loading queue, and the sentence above it that answered it moved behind it
  (be7390e2); the box-office claim checked.
- [x] `level-1-7e3a91c4` (2 November 2026, 9988e4f6): the code block with »slab = 0.5«
  stood visible under the question asking for it, now in an after block;
  stairs arithmetic, slab and the collision numbers (controls.js) checked.
- [x] `level-2-b52d0f18` (2 November 2026, 8ff92c29): page read; the portal
  section began by repeating the sentence before it; water depth, column
  height and the water defaults (level.js) checked.
- [x] `level-3-4c9e6a2d` (2 November 2026, 7bc36cb9): »Die Decke ist schwarz«
  stood visible under the prediction about it, now in an after block; the
  abstract and both teacher notes promised Lobby and Tiefe, finished levels
  the starter no longer has (only the Nachtschicht); 2K/4K were said to look
  the same, but the engine keeps the full texture; 7680 points, 64 passes and
  the ray count checked.

## Illustrations proposed

What a page would explain better with a picture or a widget, for the
maintainer to decide. Per page: where, what the picture shows, why.

Nothing at the moment. Done on 2 November 2026: the straight line and the
parity grid (a widget) in `check-digits`, the class diagrams (`_uml_diagram`,
also in the Handbuch) and the checkout timeline in `supermarkt`, the commit
graphs in `class-quiz`, the dining philosophers in `nebenlaeufigkeit`.
