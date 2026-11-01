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

## Pages

### Erste Schritte

- [ ] `about`
- [ ] `basics`
- [ ] `keyboard`

### Katas

- [ ] `pip-1`
- [ ] `turtle-1`
- [ ] `kata-functions`
- [ ] `kata-decisions`
- [ ] `kata-loops`
- [ ] `pip-2`
- [ ] `kata-texts`
- [ ] `kata-lists`
- [ ] `kata-dicts`
- [ ] `kata-objects`
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
- [ ] `supermarkt`
- [ ] `testen`
- [ ] `textadventure`
- [ ] `class-quiz`

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
