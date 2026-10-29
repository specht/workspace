# Plan: logic programming with Prolog, and run-time complexity

Two gaps of the curriculum ([plan-lehrplan-luecken.md](plan-lehrplan-luecken.md)):
the logical half of »Oberstufe: Vertiefung Applikative Programmierung
(funktional oder logisch)«, and »Aufwand« in »Algorithmen und Datenstrukturen«
with complexity in »Vertiefung Theoretische Informatik«, which so far appear
only in `kata-search` (counting guesses) and `sandbox-sortieren` (steps in a
race). Both are built as of 29 October 2026: `src/content/prolog` (»Logisch
programmieren mit Prolog«, in »Automaten und Sprachen« after `calculator`,
with the check type `prolog`, docs/aufgaben.md) and `src/content/laufzeit`
(»Wie lange dauert das?«, in »Algorithmen und Datenstrukturen«). What is
still open is marked »Open« below; the rest describes what was built.

## Prolog: »Logisch programmieren: Wer ist mit wem verwandt?«

An activity (`prolog`, kind `projekt`, three Doppelstunden and extras) in the
section of the programming languages or beside `grammatiken`, Oberstufe, after
`kata-recursion` and `kata-lists`.

### In the image

- `swi-prolog-nox` from Debian (SWI-Prolog without the GUI, about 40 MB):
  `swipl file.pl` for an interactive top level in the terminal, and
  `swipl -q -g goal -t halt file.pl` for the checks.
- Syntax colours in VS Code: »Better Prolog Syntax« (jeff-hykin, MIT, a
  grammar without code) from Open VSX, bundled in the image like Live
  Server. It claims every `.pl` file, which Perl uses too; the default
  settings (`files.associations`) give `.pl` back to Perl except below a
  folder `prolog`, where the activity and the Prolog tile keep their files.
- On the website Rouge already colours `prolog` code blocks.

### The check: type `prolog`

A new check type, like `ruby` and `python` (it runs in the student's
Workspace as the guest that cannot change anything): the task names the file
and queries, each with the answers expected, compared as a set (order does
not matter unless `ordered: true`), with a timeout for the left recursion
that never ends.

```yaml
check:
  type: prolog
  file: prolog/familie.pl
  cases:
    - query: "grandparent(X, wilhelm_friedemann)"
      # his father's parents and his mother's (Maria Barbara's) parents
      answers: ["X = johann_ambrosius", "X = maria_elisabeth", "X = johann_michael", "X = catharina"]
    - query: "ancestor(johann_sebastian, carl_philipp_emanuel)"
      answers: [true]
```

The driver wraps each query in `findall/3` with `call_with_time_limit/2`,
prints the bindings as text and compares; a message names the query, what came
and what was expected, or that it ran into the time limit (»Steht die
rekursive Regel vor der Abbruchbedingung?«).

### The chapters

1. **Facts and questions.** A real family over five generations, the Bachs
   (Johann Sebastian, his sons Wilhelm Friedemann, Carl Philipp Emanuel and
   Johann Christian, and their ancestors), as `parent/2` facts in the
   student's file. Queries with constants, then with variables; `;` for the
   next answer. A prediction: how many answers `parent(X, Y)` has.
2. **Rules.** `grandparent/2`, `sibling/2` with `X \= Y` (and the
   prediction why without it everyone is their own sibling), `ancestor/2`
   recursively. The prediction that matters most: the same rule with the
   recursive clause first, or with the recursive goal first, never ends.
3. **How Prolog searches.** A widget (docs/widgets.md, the AVL tree as the
   model): the goal list, unification with the current fact, the choice
   points left, backtracking when a goal fails; one step per click, the
   student's query on the Bach family. Unification as its own short task:
   which pairs unify and with which bindings.
4. **Lists.** `[Head | Tail]`, `member/2` and `length/2` written by the
   students, then `append/3`, and the moment of the activity: the same
   `append` run backwards, `append(X, Y, [1, 2, 3])` lists every way to split
   the list. Reversing with an accumulator.
5. **A puzzle the computer solves.** The twelve districts of Berlin coloured
   with four colours so that no neighbours share one (generate and test):
   the students write the neighbourhood facts from a map and one rule; the
   result coloured on a map on the page (an SVG of the districts, filled from
   the student's answer, pasted into the card). Then a logic grid puzzle of a
   school's own: who teaches what in which room.
6. **Extras.** Sudoku with `library(clpfd)` in twelve lines and why it is
   fast (constraints prune before guessing); a DCG that parses German
   sentences of the grammar from `grammatiken`; routes on the BVG network of
   `routenplaner` with a list of visited stations; negation as failure and
   the cut, with the warning what they cost.

People: Alain Colmerauer and Philippe Roussel (Prolog, Marseille 1972),
Robert Kowalski (the procedural reading of logic); a book box for Clocksin
and Mellish, »Programming in Prolog«. Photos with a free licence to be found.
About twelve tasks, three predictions, the puzzle as a free task.

Prolog has a tile among the language portraits (`prolog-portrait`), and the
search widget was built (`prolog_search.js`, a stepper through the search
for one query).

## Complexity: »Wie lange dauert das?«

An activity (`laufzeit`, kind `projekt`, three Doppelstunden and extras),
Oberstufe, after `kata-dicts` and `kata-recursion`, in the section
»Algorithmen und Datenstrukturen«; in Ruby, Python, Dart and Java
(docs/plan-dart-java.md). As built:

1. **Dreimal dieselbe Frage.** How many different street names has Berlin?
   The Routenplaner's dataset (`berlin.tsv.gz`, the third column, 102 763
   lines, 11 772 names): a list of names already seen, sorting first, a set.
2. **Messen.** A prediction (twice the names, four times the time for the
   list), then the starter `messen` times all three for 1 000, 2 000, 4 000 …
   names with the factor per doubling; a question on what the measurement
   shows (four times at first, twice later, since most names are in the
   list by then); `hyperfine` in a try box.
3. **Zählen statt messen.** How often the inner line of two nested loops
   runs (499 500 for n = 1 000), the O notation and a table from O(1) to
   O(n!).
4. **Wie schnell der Aufwand wächst.** A widget with n on a slider and the
   time of each growth (`/include/growth_widget.js`); a prediction (n² and
   ten times the input).
5. **Schneller machen.** Fibonacci with a memo, a pair with a given sum with
   a set; both checked with large inputs and a short `timeout`, and `slow:`
   says what to change.
6. **Eine Rundfahrt durch Berlin.** The sights of `berlin-places.tsv`: the
   length of a round trip, the shortest by trying every order, a prediction
   for 13 places, always the nearest place next, and an open question on
   where waiting stops being worth it. The starter `rundfahrt` runs both for
   the first n sights.
7. **Schnell prüfen, langsam finden.** P and NP, Stephen Cook 1971, the
   prize of the Clay Mathematics Institute. Extra: why sorting by comparing
   needs n log n, and the halting problem, linked to `turingmaschine`.

Fourteen tasks: eight with code, four predictions, a question on the
measurement and an open question.

The checks are those of the languages (`ruby`, `python`, `dart`, `java`)
on the student's file, with large inputs, `timeout` and `slow:`. Typical
mistakes in `fehler/`: two are only too slow.

Open: the students' own measurements as a chart (a `measure` task and a
small chart against n, planned, not built); person boxes for Stephen Cook
and Richard Karp, and an amortised-cost Extra (a list that grows by
doubling), both in the first plan.
