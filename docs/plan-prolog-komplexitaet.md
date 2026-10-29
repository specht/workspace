# Plan: logic programming with Prolog, and run-time complexity

Two gaps of the curriculum ([plan-lehrplan-luecken.md](plan-lehrplan-luecken.md)):
the logical half of »Oberstufe: Vertiefung Applikative Programmierung
(funktional oder logisch)«, and »Aufwand« in »Algorithmen und Datenstrukturen«
with complexity in »Vertiefung Theoretische Informatik«, which so far appear
only in `kata-search` (counting guesses) and `sandbox-sortieren` (steps in a
race). Both are proposals as of 29 October 2026; nothing is built.

## Prolog: »Logisch programmieren: Wer ist mit wem verwandt?«

An activity (`prolog`, kind `projekt`, three Doppelstunden and extras) in the
section of the programming languages or beside `grammatiken`, Oberstufe, after
`kata-recursion` and `kata-lists`.

### In the image

- `swi-prolog-nox` from Debian (SWI-Prolog without the GUI, about 40 MB):
  `swipl file.pl` for an interactive top level in the terminal, and
  `swipl -q -g goal -t halt file.pl` for the checks.
- A VS Code extension for syntax colours: »VSC-Prolog« from Open VSX, if its
  licence allows bundling (to check), otherwise a small TextMate grammar of our
  own in `src/vscode-extensions` (the ER and UML extensions show how).
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

Open: whether Prolog also gets a tile among the language portraits (the
section has room for four more, plan-lehrplan-luecken.md); whether the
search widget is worth its effort or a printed trace with `trace/0` is
enough at first.

## Complexity: »Wie lange dauert das?«

An activity (`laufzeit`, kind `projekt`, three Doppelstunden and extras),
Oberstufe, after `kata-search` and `kata-recursion`, in the section
»Algorithmen und Datenstrukturen«; Ruby or Python.

### In the image

- `hyperfine` (Debian package) to time a whole program several times with a
  warm-up and a mean, for the first chapter.
- Nothing else: timing inside a program is in the languages
  (`Process.clock_gettime`, `time.perf_counter`), and plots are drawn on the
  website (below), not with matplotlib.

### The checks

The function checks that exist (`ruby`, `python`), with large inputs and the
`timeout` the task spec already allows: a quadratic solution is right but
too slow, and the message says so (»Für 200 000 Namen braucht dein Programm
länger als 2 Sekunden: Geht es ohne die Schleife in der Schleife?«). A
`measure` task takes the students' own measurements; a small chart on the
card draws them against n (a new widget, plain SVG, the steps per doubling
marked).

### The chapters

1. **The same answer, three times.** Are there two equal names in a list of
   pupils of all Berlin schools? A loop in a loop, sorting first, a set. The
   students time all three with `hyperfine` for 1 000, 10 000 and 100 000
   names; the first goes from instant to minutes. The doubling experiment:
   what happens to the time when n doubles (twice, four times, a little more
   than twice, the same).
2. **Counting instead of timing.** Seconds depend on the computer; steps do
   not. Counting the comparisons of each program, the O notation from the
   counts: one loop n, two nested n², halving log n, sorting n log n.
   Predictions for small programs (how many steps for n = 1 000).
3. **How fast things grow.** A widget with n on a slider and the steps of
   1, log n, n, n log n, n², 2ⁿ and n! at a million steps per second, as
   times (»2⁶⁰ Schritte: 36 000 Jahre«), and the question turned around: how
   large may n be for one second?
4. **Making slow things fast.** Fibonacci from `kata-recursion`, 2ⁿ calls, and
   the same with a dictionary of results (memoisation), n calls; two numbers
   that add up to a target, with two loops and with a set. The checks with the
   timeout make the difference matter.
5. **Problems nobody knows how to solve fast.** A class trip to eight, ten,
   twelve places in Berlin, the shortest round trip by trying every order: the
   students measure 8!, 10!, 12! and extrapolate to 20. Then a quick guess
   (always go to the nearest place next) that is fast and not always best.
   Checking a solution is easy, finding one is hard: P and NP, with a Sudoku
   that is checked in a moment; the open question and its prize. Stephen Cook
   (1971) and Richard Karp in person boxes.
6. **Extras.** Why sorting by comparing needs at least n log n steps (a
   decision tree of the possible orders); the halting problem as the step from
   »slow« to »impossible«, linked to `turingmaschine`; amortised cost of a list
   that grows by doubling.

About fourteen tasks, four predictions, two of them with timeouts, and the
students' own measurements as a chart.

Open: whether the chart widget stores the measurements (a `measure` task) or
only draws what is pasted; whether the TSP chapter uses real coordinates of
Berlin schools (a dataset from `publish-datasets.rb`) or invented places.
