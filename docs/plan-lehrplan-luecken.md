# What the curriculum still lacks, and what could fill it

`/lehrplan` lists, for every name of the Berlin-Brandenburg curricula
([lehrplan.md](lehrplan.md)), the activities whose `curriculum:` line names
it. This plan is about the names with none or few, as of 27 October 2026:
what could fill each, in which form, what was decided and what is open.

## The gaps

| Name | Activities | Proposal | Form | Effort | State |
| --- | --- | --- | --- | --- | --- |
| Oberstufe: Computernetze (Einführungsphase) | 0 | TCP/IP, DNS and the web server teach what it lists (the internet, the layered model, protocols as agreements); they were only filed under »Rechner und Netze« | the name added to their lines | minutes | done |
| Oberstufe: objektorientierte Modellierung | 0 | an activity: a short story becomes a class diagram, then classes (below) | activity, a class-diagram extension | large | decided, next |
| Oberstufe: objektorientierte Programmierung | 0 | the same activity; later »Katas: Objekte« to practise the mechanics | activity, later a Kata set | large | decided, next |
| Oberstufe: Vertiefung Künstliche Intelligenz | 0 | »Ein winziges Sprachmodell«: a text generator from word pairs (bigrams) on a text the class chooses; builds on Katas: Dictionaries. Later a perceptron written from scratch. `digits-experiment` (a neural network with TensorFlow, in no catalog) needs TensorFlow in the image | project | medium | open |
| Oberstufe: Vertiefung Theoretische Informatik | 0 | a Turing machine as the next step after »Endliche Automaten«, in the same simulator; the halting problem as a thought experiment; complexity from the comparisons the sorting sandbox counts | extend `automata`, a new page | medium | open |
| Oberstufe: Vertiefung Technische Informatik | 0 | a logic sandbox like the CPU sandbox: gates, half adder, full adder, a 4-bit adder, a flip-flop, ending in the ALU of `sandbox-cpu`. The parked `cable` (light signals between two rooms) belongs nearby | a new sandbox widget | large | open |
| Oberstufe: Vertiefung Informatik und Gesellschaft | 0 | »Was eine Webseite über dich erfährt«: a tracking pixel and cookies in the students' own web server, what it learns about visitors, then the discussion; bias in training data as a section of the language model | project, sections elsewhere | medium | open |
| Sek I: Physical Computing | 0 | the Workspace cannot reach hardware, but it can build for it: MicroPython for Calliope mini or micro:bit, downloaded as a `.hex` and dragged onto the board's USB drive (the usual way to flash them); the image needs the build tools. Or a simulated board on the website | project, image change | medium | open: which boards do the schools have? |
| Oberstufe: Vertiefung Applikative Programmierung | 1 (the Lisp portrait) | the planned Prolog page (TODO.md, »Later ideas«) for logic programming; a Kata strand with `map`, `filter`, `reduce` for functional programming | page, Kata strand | medium | open |
| Sek I: Projektmanagement, Oberstufe: Softwareprojekt | 1, 2 | the planned Git collaboration (TODO.md, »Later ideas«) and the activity »Testen« (TODO.md, »Katas«) | activities | medium | open |
| Oberstufe: Datenbanken und Datenschutz | 1 | the planned modelling chapter of the database sequence (TODO.md) | chapter | medium | open |
| Sek I: Standardsoftware | 1 (LaTeX) | left as it is: office programs are not what the Workspace is for | | | decided |

The order proposed: Computernetze (done), object orientation, the language
model, the Turing machine, the logic sandbox, then the rest.

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
- **Its story and its diagram** (27 October 2026): a library (books,
  members, loans; a book and a DVD are both media), and an own
  class-diagram extension like the ER-Werkstatt, so that the diagram is
  checked like everything else.
- **The diagram's language** (27 October 2026): like the ER-Werkstatt's,
  indentation and a few English keywords, one member per line, `+` and `-`
  for public and private (no `#` for protected: it starts a comment), types
  as in Java (`int`, `String`, `boolean`):

  ```
  # Eine Bibliothek
  class Medium (abstract)
    - title: String
    + lend(): void
  class Book extends Medium
    - author: String
    + Book(title: String, author: String)
  class Member
    + borrow(medium: Medium): void
  association loans: Member 1 -- 0..* Medium
  ```

## Open questions for the maintainer

- Physical Computing: which boards do the schools have (Calliope mini,
  micro:bit, none)?
- Künstliche Intelligenz: TensorFlow in the image (large), so that
  `digits-experiment` can go into the catalog, or networks written from
  scratch without libraries, which suits »students write small functions
  first« better?
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
