# What the curriculum still lacks, and what could fill it

`/lehrplan` lists, for every name of the Berlin-Brandenburg curricula
([lehrplan.md](lehrplan.md)), the activities whose `curriculum:` line names
it. This plan is about the names with none or few, as of 27 October 2026:
what could fill each, in which form, what was decided and what is open.

## The gaps

| Name | Activities | Proposal | Form | Effort | State |
| --- | --- | --- | --- | --- | --- |
| Oberstufe: Computernetze (Einführungsphase) | 0 | TCP/IP, DNS and the web server teach what it lists (the internet, the layered model, protocols as agreements); they were only filed under »Rechner und Netze« | the name added to their lines | minutes | done |
| Oberstufe: objektorientierte Modellierung | 1 | an activity: a short story becomes a class diagram, then classes (below) | activity, a class-diagram extension | large | `supermarkt`, live; `textadventure` (28 October 2026), with a small class diagram of `Item`, `Key` and `Food` |
| Oberstufe: objektorientierte Programmierung | 2 | the same activity; »Katas: Objekte« to practise the mechanics | activity, Kata set | large | `supermarkt`, live; `kata-objects` (28 October 2026); `textadventure` (28 October 2026), a project with rooms, items and a player, inheritance with `use` |
| Oberstufe: Vertiefung Künstliche Intelligenz | 3 | »Ein winziges Sprachmodell« (bigrams), »Ein Neuron lernt« (a perceptron from scratch), »Nächste Nachbarn« (classifying, and what biased data does) | section KI | large | done, live: `sprachmodell`, `perzeptron`, `naechste-nachbarn` |
| Oberstufe: Vertiefung Theoretische Informatik | 3 | pushdown automata, Turing machines and the halting problem, grammars and the Chomsky hierarchy, after »Endliche Automaten« | activities | medium | done, live: `kellerautomat`, `turingmaschine`, `grammatiken`; complexity still only in `kata-search` and the sorting sandbox |
| Oberstufe: Vertiefung Technische Informatik | 2 | a logic sandbox like the CPU sandbox: gates, half adder, full adder, a 4-bit adder, a flip-flop, ending in the ALU of `sandbox-cpu`. `radio` (light signals between two rooms, formerly `cable`) belongs nearby | an activity with a circuit widget, then one with a circuit editor (below) | large | done, live: `logikgatter`, `logikwerkstatt` (28 October 2026) |
| Oberstufe: Vertiefung Informatik und Gesellschaft | 4 | »Was eine Webseite über dich erfährt« (tracking in the students' own web server, cookie banners and dark patterns), the search engine's SEO section, the training data of the language model, a decision by neighbours | project, sections elsewhere | medium | done, live: `tracking`, `suchmaschine`, `sprachmodell`, `naechste-nachbarn` |
| Sek I: Physical Computing | 0 | the Workspace cannot reach hardware, but it can build for it: MicroPython for Calliope mini or micro:bit, downloaded as a `.hex` and dragged onto the board's USB drive (the usual way to flash them); the image needs the build tools. Or a simulated board on the website | project, image change | medium | shelved: the schools have no boards (28 October 2026) |
| Oberstufe: Vertiefung Applikative Programmierung | 2 | the planned Prolog page (TODO.md, »Later ideas«) for logic programming; a Kata strand with `map`, `filter`, `reduce` for functional programming | page, Kata strand | medium | functional done, live: `kata-functional` (»Katas: Funktionen als Werte«, 28 October 2026: lambdas, a closure, `map`, `filter`, `reduce`, chains, a loop rewritten and one where the loop stays clearer, beside the Lisp portrait); Prolog open |
| Sek I: Projektmanagement, Oberstufe: Softwareprojekt | 2, 4 | the Git collaboration and testing | activities | medium | done, live: `class-quiz` (a quiz in the class repository with real merge conflicts), `git`, `kata-tests`, `hugo` |
| Oberstufe: Datenbanken und Datenschutz | 2 | Datenschutz in `tracking`; Urheberrecht and the modelling chapter of the database sequence (TODO.md) still open | chapter | medium | in part: `mysql`, `tracking` |
| Sek I: Standardsoftware | 1 (LaTeX) | left as it is: office programs are not what the Workspace is for | | | decided |

The order proposed: Computernetze (done), object orientation, the language
model, the Turing machine, the logic sandbox, then the rest.

### Gaps inside names that have activities

A name with activities can still leave out what its plan lists. As of
27 October 2026, found by comparing the contents in [lehrplan.md](lehrplan.md)
with the details of every `curriculum:` line and a search of the pages:

| Name | Left out | Proposal |
| --- | --- | --- |
| Sek I: Leben in und mit vernetzten Systemen | Gefahren bei E-Mails (Phishing, Schadprogramme), Netiquette; Suchmaschinen and Datenschutz are covered now (`suchmaschine`, `tracking`) | »E-Mail und Phishing«: a mail server in the Workspace, a forged sender, what headers reveal |
| Sek I: Information und Daten | Sender-Empfänger-Modell, covered now by `radio` (28 October 2026: the parked `cable` back as radio, Shannon's model at its end); Strichcode and QR-Code are covered now (`check-digits`; the chapter »Wenn der Code beschädigt ist: QR-Codes«, 28 October 2026: the parts of a QR code, a code to paint over that jsQR reads in the browser, the levels L to H, Reed-Solomon as values on a line, a grid with parity bits in Ruby or Python), Rechnen mit Dualzahlen too (the chapter »Addieren« of `kata-bits`, 28 October 2026: written addition with a carry, overflow in a byte, negating in two's complement) | done |
| Sek I: Datenbanken | nothing: Datensätze einfügen, löschen, verändern are covered now (the chapter »Daten ändern« of `mysql`, 28 October 2026: `INSERT` with one and several rows, `UPDATE` with a prediction on a missing `WHERE`, `DELETE`, transactions; the check `schema` counts rows with `rows_where`), and its line names them | |
| Sek I: Geschichte der Informatik | Rechenhilfsmittel, Rechnergenerationen; technische Entwicklung der Informationsübertragung is covered now by `radio` (Morse and CW, Baudot's telegraph and his person box) | a chapter in `sandbox-cpu` |
| Sek I: Digitale Bilder und Visualisierung | Manipulation von Bildern und Filmen; Vektorgrafik and rekursive Algorithmen für Grafiken are covered now by the three Turtle activities (`turtle-1` to `turtle-3`, 29 October 2026: lines and figures from commands, recursive trees and curves, Lindenmayer systems) | an activity or chapter on manipulated pictures: filters and retouching in code, how a fake is made and noticed. The parked SVG chapter (`parked/drawing-svg`) is no longer needed for the curriculum |
| Oberstufe: Algorithmen und Softwareentwicklung | nothing: Struktogramm and Pseudocode are covered now (`check-digits`); the Katas to functions, decisions and loops and the twelve language portraits name it since 27 October 2026 | |
| Oberstufe: Syntax und Semantik | nothing: `grammatiken` and `calculator` cover it, both live; the language portraits no longer claim it (27 October 2026) | |
| Oberstufe: Sprachen und Automaten | regular expressions, only an outlook at the end of `automata` and the Handbuch page `reference/regex` | »Reguläre Ausdrücke« between automata and grammars: grep in a web server's log, groups in Ruby or Python, validating postcodes, dates and user names (and why not e-mail addresses), backtracking, regex to automaton with the Thompson construction and Kleene's theorem, no expression for brackets. Done, live: `regex` (28 October 2026), after `automata` in the section »Automaten und Sprachen« |
| Oberstufe: Datenbanken | nothing: Normalisierung is covered now by an extra chapter of `filmdatenbank` (28 October 2026): a film club's wide table built from the students' own data, the three anomalies run in transactions, first to third normal form step by step (the third written by the students, checked by `schema` with `absent_columns`), back to the activity's six tables; the definitions once in the Handbuch (`er-modelle#normalformen`) | |
| Oberstufe: Datenbanken und Datenschutz | Urheberrecht; Datenschutz is covered now (`tracking`) | a section on licences, perhaps with the picture rights of the Workspace's own pages |
| Oberstufe: Computernetze, Vertiefung Computer-Netze | nothing: LAN, WLAN and routing are covered now by the chapter »Im eigenen Netz: Ethernet, ARP und Switches« of `tcpip` (28 October 2026): the MAC address with `ip link`, the frame, ARP, `ip neigh` in pairs after a `ping` (the partner learns the asker too), `ip route` and why 8.8.8.8 never appears, a switch learning its table, WLAN as a shared medium encrypted by WPA, new frames but the same packet at every router; Robert Metcalfe and the 1976 paper; the activity is three Doppelstunden now, and its line names both | |
| Oberstufe: Softwareprojekt | Ergonomie | a chapter of the Git project below, or of `svelte` |

## Packages for the front page

On a wide screen the front page shows three cards in a row. Until
29 October 2026 every section had to hold three or six (Fireworks and Hugo
were parked for that, in eabe9ca9) and the language tiles fill rows of
four; the maintainer dropped that rule then: a section takes as many
activities as belong in it, and a row may end half full. Proposed on
27 October 2026, after the maintainer asked to close the gaps where
appropriate, also by bringing back parked activities or moving them:

| Package | Section | Cards | Closes |
| --- | --- | --- | --- |
| Software | Software entwerfen | `check-digits`, `supermarkt`, `class-quiz`: done, all live | objektorientierte Modellierung and Programmierung, Algorithmen und Softwareentwicklung, Projektmanagement, Softwareprojekt |
| Web | Web (3 → 6) | `hugo` back, `suchmaschine`, `tracking`: done, all live | Suchmaschinen, Datenschutz, Informatik und Gesellschaft |
| Sprachen | Programmiersprachen (16 → 20 tiles) | `smalltalk` and `cobol` back; Prolog (logic); Haskell (functional). Smalltalk and COBOL run in the image; Prolog (`swi-prolog`) and Haskell (`ghc`) do not yet | Applikative Programmierung; Smalltalk beside the supermarket |
| KI | a new section Künstliche Intelligenz (0 → 3) | `sprachmodell`, `perzeptron`, `naechste-nachbarn`: done, live | Vertiefung Künstliche Intelligenz, Informatik und Gesellschaft |
| Rechnen | Wie Computer rechnen (6 → 9), split on 28 October 2026 into Wie Computer rechnen (3), Algorithmen und Datenstrukturen (3), Automaten und Sprachen (6) | `kellerautomat`, `turingmaschine`, `grammatiken`, `logikgatter`, `logikwerkstatt`, `regex`: done, live; `cable` came back as `radio` in Netze & Sicherheit instead (28 October 2026) | Technische Informatik, Theoretische Informatik, Information und Daten |
| Chapters | no new cards | Normalisierung in `filmdatenbank` (done), syntax diagrams in `calculator`, LAN and WLAN in `tcpip` (done), QR codes in `check-digits` (done), an SVG chapter | the gaps inside names above |

State of the package Software (27 October 2026; out of dev-only on 28 October 2026):
`check-digits` (»Prüfziffern: erst planen, dann programmieren«: EAN-13 and
ISBN-10, a Struktogramm read and translated, then planned by the students;
Ruby and Python, with its card picture), `supermarkt`, the Handbuch pages
»Pseudocode und Struktogramme« (`struktogramm.js` draws ```` ```struktogramm ````
blocks and the playground) and »Klassendiagramme als Text«. Open: the third
card (the team project with Git), the card picture and screenshots of
`supermarkt`. The keywords in a Struktogramm are those of the Handbuch's
pseudocode (English, `while`, `for`, `until`), the labels of a decision
»wahr« and »falsch«; whether German keywords (»solange«, »wiederhole … bis«)
suit the classes better was asked: English stays, the same words as the
pseudocode beside it (27 October 2026).

The third card (decided 27 October 2026): a class quiz in the class
repository (`class_repo: group`). Each student adds questions in a folder of
their own and registers them in one shared list, so that a merge conflict
happens for real; then branches, pull before push, reviewing a classmate's
commit, a `TODO.md` as the task board. Without a class repository the page
simulates a teammate locally, as the Git activity does.

Fireworks (`feuerwerk`) could join Spiele & 3D-Welten with two more cards;
it still needs a teacher note, a try in class and its screenshots (TODO.md).
`cable` was parked on purpose (fa0f2c81) and came back on 28 October
2026 as `radio`, »Datenübertragung per Funk«, first in Netze & Sicherheit
(`/cable` redirects there). The device is a radio now, five channels keyed on
and off on a frequency everyone can hear, like Morse on shortwave (CW), which
explains listening in and jamming. Teachers give each pair its word from the
row of its frequency and say it aloud; the panel keeps which word went to
which frequency, in the teacher's browser only. Each student gets a printed
sheet (`src/static/radio/blatt.pdf`, from `src/scripts/radio-sheet.rb`): the protocol above,
eight rounds below with »Gesendet«, written by the teacher, and »Empfangen«,
written by the receiver. The code keeps the name `cable` (`cable.js`,
`/ws_cable`), so that no installation has to change anything.

## Decisions

- **Computernetze** (27 October 2026): TCP/IP, DNS and the web server carry
  the name too; no new page.
- **Object orientation** (27 October 2026): first an activity with
  modelling, in Ruby, Python, JavaScript, Dart and Java; a Kata set
  »Objekte« later. Why: the plan for the Kurshalbjahre names
  objektorientierte Modellierung (UML class diagrams) and
  objektorientierte Programmierung side by side, and »Analyse und
  Modellierung folgen dem objektorientierten Ansatz«; a Kata set practises
  the mechanics but cannot model, an activity covers both names.
- **Its diagram** (27 October 2026): an own class-diagram extension like
  the ER-Werkstatt, so that the diagram is checked like everything else.
- **Its story** (27 October 2026): first a library, then, on the
  maintainer's doubt, a simulation of the checkouts in a supermarket:
  »Lohnt sich eine Expresskasse?« Why: a library is about data that must be
  kept (books, members, loans), which is what an ER model and a database
  are for, and its behaviour is thin. Object orientation pays where objects
  live only while a program runs, change their state and act differently
  by class; simulation is where it began (Simula). The simulation answers a
  real question, and the answer surprises: with the same three cashiers,
  an express checkout makes the average wait longer. The page says how a
  class diagram and an ER model differ and that a real checkout system has
  both (an ORM between them).
- **The extension's name** (27 October 2026): the UML-Werkstatt, files
  `.uml`, so that state and sequence diagrams can join it later; class
  diagrams first. Also agreed: the outline below (then for the library),
  preparing a new Workspace image (built after asking).
- **The diagram's language** (27 October 2026): like the ER-Werkstatt's,
  indentation and a few English keywords, one member per line, `+` and `-`
  for public and private (no `#` for protected: it starts a comment), types
  as in Java (`int`, `String`, `boolean`):

  ```
  # Kassen im Supermarkt
  class Checkout
    - freeAt: int
    + serve(customer: Customer): int
  class ExpressCheckout extends Checkout
    + accepts(customer: Customer): boolean
  class Supermarket
    + Supermarket(checkouts: List<Checkout>)
  association checkouts: Supermarket 1 -- 1..* Checkout
  ```
- **Names in the diagram** (27 October 2026): as in the student's code, not
  as UML usually writes them. Why: the maintainer found `isAvailable` in the
  diagram beside `available?` in Ruby confusing. A method may end with `?`
  or `!` (Ruby); the check `classes` takes `isAvailable`, `is_available`
  and `available?` as one method (`!` makes another one). The page shows
  the diagram per language where names differ (one marker may name several
  languages, `<!-- language: javascript dart java -->`), and the general
  examples use names of one word (`toggle`, `seats`). Types stay as in
  Java. Ruby's `accepts?` is the others' `accepts` too.
- **Packages for the front page** (27 October 2026): close the gaps where
  appropriate; parked activities may come back and activities may move.
  Full rows are no longer required (29 October 2026). Software first (it lets `supermarkt`
  go public); `cable` may come back (it did, as `radio`).
- **Overriding in the diagram** (27 October 2026): since a class has what
  it inherits, a task can no longer see whether a subclass overrides a
  method; `overrides` in the check `classes` asks for it in the class's
  own box and in a class above it.

- **The next four** (28 October 2026): Künstliche Intelligenz, the Turing
  machine, the logic sandbox and Informatik und Gesellschaft, in packages
  that keep the rows of the front page full. Physical Computing is shelved:
  the schools have no boards.
  - **KI first**, a section of its own with three cards (below), dev-only
    until all three exist.
  - **Informatik und Gesellschaft both ways**: inside the KI section (what
    biased data does to nearest neighbours, where a language model's text
    comes from) and as »Was eine Webseite über dich erfährt« in Web, which
    then takes `hugo` back and »Eine Suchmaschine« to reach six.
  - **Wie Computer rechnen** grows to nine: the logic sandbox, the Turing
    machine, and `cable` back, reframed as CW radio (it went to Netze & Sicherheit as `radio`).

## The KI section (outline)

Three projects in Ruby and Python, written from scratch without libraries,
so that students write the small functions themselves; each in about two
Doppelstunden, built on Katas: Dictionaries and Listen.

1. **»Ein winziges Sprachmodell«** (`sprachmodell`). A text (fairy tales of
   the Brothers Grimm, public domain, as a dataset; or one the class
   chooses) is split into words; a dictionary counts which word follows
   which; the most likely next word, then text generated word by word,
   drawn by its counts. Then two words of context instead of one: the text
   gets better and starts to copy whole sentences of the original, which is
   the trade-off of every language model. Shannon did this by hand in 1948
   (»A Mathematical Theory of Communication«, a person box and a paper box),
   Markov with the letters of »Eugen Onegin« in 1913. For Gesellschaft: the
   model knows only its text (whose texts, with whose consent), repeats what
   the text says often, and writes fluently without knowing whether it is
   true.
2. **»Ein Neuron lernt«** (`perzeptron`). A perceptron from scratch: weights,
   a sum, a threshold; the learning rule on points of two classes with two
   features, drawn after every pass; then 8 × 8 pixel digits (a free
   dataset), its 64 weights shown as a picture of what it looks for. XOR it
   cannot learn (Minsky and Papert, 1969), and why two layers can. Frank
   Rosenblatt (1958) in a person box.
3. **»Nächste Nachbarn: Wer entscheidet hier?«** (`naechste-nachbarn`).
   Classifying by the k nearest examples (Fix and Hodges, 1951); accuracy on
   data held back. Then made-up decisions of the past, skewed against one
   part of a town: the classifier learns the skew and repeats it; the error
   rate per group shows it; what could be done (other data, other features,
   asking who decides), and that a person stays responsible. Joy Buolamwini's
   »Gender Shades« (2018) as the real case, in a paper box.

State: all three are written and live since 27 October 2026 (`sprachmodell`, `perzeptron`, `naechste-nachbarn`); Rosenblatt, Gorman and Buolamwini are on the Personen page.

#### »Ein Neuron lernt« (`perzeptron`), outline

Real data first: the Palmer penguins (Gorman, Williams and Fraser 2014;
CC0), measured on the Antarctic Peninsula. Adélie and Gentoo are told apart
by two measurements of the bill, its length and its depth: with the raw
millimetres, zero weights, rate 1 and the file's order, a perceptron gets
all 274 right after 12 passes, and its weights can be read (29.1 · length −
74.0 · depth − 1 > 0: Gentoo has a long, flat bill). Adélie against
Chinstrap is never error-free: a single neuron's limit in real data,
before XOR.

1. The neuron by hand: inputs, weights, a sum, a threshold; McCulloch and
   Pitts (1943), Rosenblatt's Mark I Perceptron (1958).
2. `predict(weights, bias, features)`, for any number of features (part 2
   needs 64); loading the CSV is given.
3. The learning rule, `learn(…)` for one example, `train(examples, passes)`
   counting the errors of each pass; a prediction (will it ever be
   error-free?) and the number of passes.
4. What it learned: the weights read as a rule; a given function draws the
   penguins and the line into an SVG opened in VS Code.
5. Penguins it has not seen: trained on half, tested on the rest.
6. Where one neuron stops: Adélie against Chinstrap, then XOR (Minsky and
   Papert, 1969), and why two layers can.
7. Digits: 8 × 8 pixels, »0 or 1?«, the same functions with 64 features
   (UCI »Optical Recognition of Handwritten Digits«, CC BY 4.0); the
   weights as a picture of what the neuron looks for; a drawing pad on the
   page where students paste their weights and draw (taking up the idea of
   the parked `digits-experiment`: their own handwriting).

Open: whether digits the class draws on the pad become a dataset of its
own (an extra).

## Logic gates: what exists and the next step

`logikgatter` (»Vom Gatter zum Rechenwerk«, 28 October 2026, live since the
front page's split the same day, first in »Wie Computer rechnen«) is the first
version: circuits to click
(`logic_circuit.js`, gates as in DIN EN 60617), predictions before each
circuit, and seven Aufgaben with code that may only use gates (`gates:` in the check
`function`, `GateCheck`): NOT and AND from NAND, OR and XOR from NAND, half
adder, full adder, a 4-bit adder, the D flip-flop as a function, and a 4-bit
ALU with ADD, SUB, AND, OR, XOR and the flags ZF and CF as the CPU Sandbox
sets them (CF after SUB is the borrow). George Boole, and William Eccles with
F. W. Jordan (the first electronic latch, 1918), have boxes on the Personen
page (`_person(boole)`, `_person(eccles-jordan)`).

**The circuit editor: `logikwerkstatt`** (»Logik-Werkstatt«, 28 October
2026, live: `logikwerkstatt` after `logikgatter` in »Wie Computer
rechnen«, so that the section reads logikgatter → Werkstatt → `sandbox-cpu`,
from the gate to the processor). Students build on the page, with the
drawing and the engine of `logic_circuit.js` (`circuit_editor.js`,
widgets.md):

- a palette of the six gates, switches and lamps in the free editor, and the
  person's own blocks; parts placed by a click or dropped where they belong,
  moved on a grid; wires dragged (or tapped pin after pin) from an output to
  an input, at right angles, a dot where one branches; undo, keyboard,
  touch; on a phone only to look at and switch;
- a circuit is a netlist, one line a part (`nand g1 a b @120,48`, `block HA …
  end`), the same text in the editor, in a page (`<div class='circuit-view'
  data-netlist='…'>`) and as the answer of a task;
- blocks: a circuit saved with »Als Baustein speichern« becomes a box with
  its switches as inputs and its lamps as outputs; the half adder becomes
  `HA`, two of them the full adder `VA`, four of those the 4-bit adder. A
  block is a stored sub-circuit; the engine replaces it by its gates;
- the check `circuit` (`CircuitCheck`, aufgaben.md): the server reads the
  netlist and runs it round by round like the page, every row of the truth
  table (expected outputs as expressions over the inputs) or steps one after
  another for a circuit with memory; allowed gates, a gate budget (counted
  inside blocks) and a least number of blocks; the first wrong row named;
- seven tasks in rising order: half adder, XOR from four NAND, multiplexer,
  full adder from two half adders, 4-bit adder from four full adders, a
  2-bit comparator, a D latch; a prediction about the carry running through
  the adder;
- drafts and the person's blocks are kept in the browser (localStorage, per
  account, like the tips: nothing new on the server); a checked circuit is
  the task's stored answer as with every task, and brings its blocks to
  another computer.

Decided along the way: the editor lives on the page, not as a VS Code
extension (no Workspace needed, a tablet is enough); circuits nest by name
(a block carries copies of the blocks inside it). Open: buses (four bits on
one line) and a block for the ALU, so that the CPU Sandbox's ALU could be
built in the editor too; a teacher's »Lösung einsetzen« for circuits; a
person box for the flip-flop (Eccles and Jordan, 1918) if a free photo and
a readable source turn up.

## Open questions for the maintainer

- Künstliche Intelligenz: proposed from scratch without libraries (above);
  TensorFlow in the image would let `digits-experiment` into the catalog
  later.
- The order of the rest.

## The object-oriented activity (draft)

Working title »Klassen entwerfen«. A short story (a library with books,
members and loans; a music playlist; an inventory in a small game; to be
chosen) is read for its nouns and verbs; classes with attributes and
methods come out of it, drawn as a class diagram, then written as code and
checked; then relationships: »hat ein« (association, with multiplicities)
and »ist ein« (inheritance).

What it needs, in this order (state on 27 October 2026 after each):

1. **Five languages on an activity** (done: Dart and Java in
   `CodeLanguages`, with logos and colours in the switch). `CodeLanguages` knows Ruby, Python
   and JavaScript, and an activity offers Ruby and Python (a class sets one
   of them, `GROUP_LANGUAGES`). Dart and Java join `LANGUAGES` (with names,
   the switch, syntax highlighting of code blocks and code inputs); whether
   the class setting offers them stays the maintainer's call. JavaScript is
   so far a language of the Katas only (plan-katas.md); this page would be
   the first activity with it.
2. **Checks of Java and Dart files in the Workspace** (done: check types
   `java` and `dart`, docs/aufgaben.md, tried on real classes in the
   Workspace image; and `javascript` for a file in the Workspace, which
   there was none of: the Katas' harness, run with node as the guest). The checks `ruby`,
   `python` (and `javascript`) load the person's file in their Workspace and
   evaluate each call with the language's `eval`. Java and Dart have none:
   their harness is generated as source around the calls of the task,
   compiled with the person's files and run, then prints the same result
   line (`CallCheck`, `__HS_RESULT__`). Equality needs care: `==` on Dart
   lists is identity, Java arrays need `Arrays.deepEquals`; both harnesses
   bring their own comparison and their own way to print a value.
   They run in the Workspace, which has the memory a JVM needs.
3. **Calls of several steps** (done for all five languages and the Katas,
   docs/aufgaben.md, »Aufrufe in mehreren Schritten«). An object's behaviour shows over steps:
   `a = Account(5); a.deposit(3); a.balance()`. Ruby's and JavaScript's
   `eval` return the last value already; Python's harness evaluates one
   expression and needs to run the statements before the last one first;
   Java and Dart put the steps into the generated code.
4. **A class-diagram extension**, modelled on the ER-Werkstatt
   (`src/vscode-extensions/er`, about 2,900 lines, its core 1,200): a small
   language for classes, attributes with types, methods with parameters,
   associations with multiplicities and inheritance; a preview that draws
   the UML class diagram; a core shared by the extension and a check type
   that compares the model with the task, as `er` does (`ErCheck`).
   Alternatives: Mermaid's class diagrams in a Markdown preview (an
   extension the Workspace does not have; no check), or PlantUML (Java,
   heavy). The own extension fits how the ER-Werkstatt works and is the
   only one that can be checked.
5. **The page**, with tasks in five languages, examples for every check in
   `task-examples.yaml` (Java and Dart examples wait where the machine
   lacks them, as JavaScript without Node does), `lehrkraft.md`,
   screenshots from recipes, its curriculum line (Oberstufe:
   objektorientierte Modellierung, objektorientierte Programmierung; Sek I:
   Algorithmisches Problemlösen (Grundlagen der Objektorientierung)).

### Outline of the page

Slug `supermarkt`, title »Klassen entwerfen: die Expresskasse« (shortened on 28 October 2026 from »… Lohnt sich eine Expresskasse?«),
kind `projekt`, languages `[ruby, python, javascript, dart, java]`, about
three Doppelstunden, dev-only for now. No starter repository: students make
a folder `supermarkt` with `supermarkt.uml` and their code (one file, in
Java one file per class); the simulation itself is given on the page, and
`customers.csv` (171 made-up customers of a Saturday morning, seeded) comes
from the activity's folder. Times are whole seconds, so every language
computes the same numbers (checked in all five: 69.75, 118.29, 12.44).

1. **Die Geschichte.** The branch manager's question; Simula in one
   paragraph; which nouns become classes, which an attribute (the number of
   items), which nothing (the fridges).
2. **Die erste Klasse.** `Customer` with items and arrival in the diagram,
   then in code with getters; »Klassendiagramm oder ER-Modell?« with a
   table.
3. **Zustand und Verhalten.** `Checkout` remembers only when it is free
   again; `serviceTime` (60 s + 3 s per item), `serve` returns the wait. A
   prediction first; the checks catch a start before the customer is there.
4. **Der Supermarkt.** The association `Supermarket 1 -- 1..* Checkout`
   (and why a customer as a parameter is none); `arrive` picks the
   checkout free first, the first on a tie; the average as a `double`.
5. **Expresskasse und Selbstbedienung.** `ExpressCheckout` overrides
   `accepts`, `SelfCheckout` `serviceTime` (30 s + 6 s per item);
   `arrive` asks `accepts`. A prediction on polymorphism.
6. **Der Samstag.** The simulation; three numbers; a prediction (the
   express checkout makes it worse: 70 s to 118 s; small baskets 58 to
   40, large ones 82 to 197); what the model leaves out.
7. **Wie geht es weiter?** Waits per basket size, another limit, the
   longest wait, a real Queue.

Open: a person box for Kristen Nygaard and Ole-Johan Dahl (Simula, the
first classes, made for simulations like this one), with photos and
sources; a Handbuch page on the UML-Werkstatt's language like
`er-modelle`; screenshots and the card picture from recipes; the
UML-Werkstatt in the Workspace image.

## Java and Dart for Katas later

Measured on 27 October 2026 in the Workspace image (OpenJDK 25.0.4, Dart
3.13), a class with three calls, in a container like a Kata's (no network,
read-only image, nobody):

| Run | Limits | Time |
| --- | --- | --- |
| `dart harness.dart` (the SDK's own `/opt/flutter/bin/cache/dart-sdk/bin/dart`) | 0.5 CPU, 192 MB | 1.6 s |
| `javac` both files, then `java` (`-XX:+UseSerialGC -XX:TieredStopAtLevel=1`) | 0.5 CPU, 256 MB | 2.5 s |
| the same | 1 CPU, 384 MB | 0.9 s |

- `dart` on the path is Flutter's wrapper, which tries to update its cache
  and fails on a read-only image; the Kata runner calls the SDK's binary.
- Java's launcher for source files (`java Harness.java`, several files since
  Java 22) finds another class only in a file named after it; a person's
  `Account` in `Kata.java` is not found, so it is `javac` and then `java`.
- Java Katas would need more than today's 192 MB and half a core
  (`KataRunner::MEMORY`, `CPUS`): limits per language.
