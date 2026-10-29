# Plan: Dart and Java on the site

Where Dart and Java are offered, how the checks for them work, and what is
open. State of 29 October 2026. The maintainer's school uses Dart; other
schools teach Java, also for object orientation, so pages get both, one page
at a time (a class ranks its languages under »Sprachen der Klasse«).

## What exists

- **All Katas and Pip unter Funkelbach** in Ruby, Python, JavaScript, Dart
  and Java; how they are checked (`kata_compiled.rb`, `pip_compiled.rb`,
  `tests_compiled.rb`), the decisions and the typical mistakes per language:
  [plan-katas.md](plan-katas.md).
- **Activities in Ruby, Python, Dart and Java:** Textadventure
  (`textadventure`), Prüfziffern (`check-digits`), »Wie lange dauert das?«
  (`laufzeit`); »Supermarkt« in all five. Their code tasks use the check
  types `java` and `dart` (docs/aufgaben.md, »java, dart«), which compile the
  student's file in their Workspace.
- **Handbuch:** »Dart und Java nebeneinander« (`reference/dart-java`), and
  Dart and Java columns in »Pseudocode und Struktogramme«.
- **Language choice:** a class ranks any of the five; a person's own switch
  picks only between Ruby and Python, and a choice of JavaScript, Dart or
  Java on a Kata page is kept apart (`code_languages.rb`).

## How a page gets Dart and Java

The pattern of the three activities above:

- `languages: [ruby, python, dart, java]` and the `language:` line in the
  meta block;
- a `dart:` and a `java:` variant on every card whose prompt, check, hint or
  solution names code (camelCase names, each language's types; a prediction
  card whose code or numbers differ gets its own prompt and check);
- solutions in `loesung/` (`laufzeit.dart`, `Laufzeit.java`), every typical
  mistake of the Ruby version in both (`fehler/*.dart`, `fehler/java/`),
  plus the mistakes only one language has (Java: `==` on texts, `remove(int)`
  taking the index, `int` overflowing);
- starters in both; a Java starter that calls a method the student writes
  later stops every `java` check on the page, since the check compiles every
  `.java` file in the folder: the page gives a stand-in until then;
- `<!-- language: dart -->` and `<!-- language: java -->` blocks on the page
  (several languages in one marker, `<!-- language: dart, java -->`); a
  marker never between table rows, so a table that names code comes once per
  group of languages;
- times and numbers measured in each language, not taken over from Ruby
  (Laufzeit's ten places: 1.5 s in Ruby, 0.5 s in Dart, 1 s in Java);
- examples for both in `task-examples.yaml`, run with `javac` and `dart` on
  the machine; the teacher note.

## Open

- **Checks in a real Workspace.** The Textadventure's recipe ran its Dart
  and Java checks in a Workspace. Prüfziffern has no recipe that runs checks,
  and Laufzeit's recipe checks only Ruby and Python: `check-task: … dart`
  and `… java` lines (with `write-file:` of `loesung/laufzeit.dart` and
  `loesung/Laufzeit.java`) would run them on the next render.
- **A class at once.** In a Kata container (0.5 CPU) a Java Kata card with
  its typical wrong solutions takes about 2.3 s, a Dart card 1.4 to 2 s, a
  Pip card with three maps about 3 s (measured, plan-lehrplan-luecken.md,
  »Java and Dart for Katas«). Nobody has checked yet what thirty students pressing »Prüfen« in
  the same minute do to the host. Measure before a Java class uses the
  Katas.
- **The own switch.** Should a student be able to pick Dart or Java
  themselves for every page, not only through the class? Now only Ruby and
  Python (`OWN_LANGUAGES`). The maintainer decides.
- **The next activities.** Those without Ruby gems or Python libraries
  translate directly; by their code tasks in `tasks.yaml`:
  `automata` (12), `calculator` (12), `naechste-nachbarn` (10), `huffman`
  (8), `rle` (6), `kellerautomat` (6), `regex` (6; each language's regular
  expressions differ in details), `lzw` (4), `grammatiken` (4),
  `turingmaschine` (4). Not worth it: the activities built around a library
  or a server (web server, Pixelwand, Routenplaner, Steganographie, TOTP,
  Tracking, Suchmaschine, Wetter), where Dart or Java would mean redoing the
  activity rather than translating it; check Perzeptron and Sprachmodell
  first. The maintainer picks the order.
- **Concurrency** in Java and Dart: [plan-nebenlaeufigkeit.md](plan-nebenlaeufigkeit.md).
- **Object orientation** in five languages, with a class diagram:
  [plan-lehrplan-luecken.md](plan-lehrplan-luecken.md).
