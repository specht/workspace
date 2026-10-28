# Katas and Pip unter Funkelbach

**Katas** are short programming exercises typed straight into a card on the
page, in Ruby, Python or JavaScript, without terminal and files: a way to start
coding before the Workspace's tools, and to practise alongside them. They
replace Code Bites (removed on 2026-10-18; the old code is in
`git show 8a6bead5^:src/codebites/…`). **Pip unter Funkelbach** is the way
in: a story in three parts in which students steer Pip from the 2D Game
Studio through tunnels with plans.

**The name is »Kata«**: a code kata is the programmers' word for a small
exercise repeated until it is fluent; it reads the same in German, »eine Kata,
zwei Katas«. `kata` in code.

This file says what exists and why it is as it is. How to write a Kata is in
[aufgaben.md](aufgaben.md) (check types `function`, `pip`, `tests`), how runs
work in [entwicklung.md](entwicklung.md) (»Katas: Läufe im eigenen
Container«), the rules with what checks them in [checkliste.md](checkliste.md)
(»Katas und Pip«). Open work is in [TODO.md](../TODO.md), »Katas«.


## What exists

All pages are in the catalog since 2026-10-26 (before, dev-only until the
maintainer had looked at them). In catalog order, which is also the order of
the strip on the front page:

| # | Page | Idea | Special |
| --- | --- | --- | --- |
| 1 | `pip-1` Hinab in die Stollen | plans, loops, conditions, waiting; no prior knowledge | first chapter clicked (`buttons:`) |
| 2 | `kata-functions` Erste Funktionen | values in, one value out | |
| 3 | `kata-decisions` Verzweigungen | `if`, comparisons, `and`/`or` | |
| 4 | `kata-loops` Schleifen | loops, a variable that collects | the sieve as Extra, with a time limit |
| 5 | `pip-2` Bellos Spur | `while`, own functions, variables, the hand on the wall | `call`/`expect`, `generate: maze` |
| 6 | `kata-texts` Strings | strings, characters, building text | |
| 7 | `kata-lists` Listen | going through a list, building one | the merge step |
| 8 | `kata-dicts` Dictionaries | counting, grouping, looking up | |
| 9 | `kata-objects` Objekte | classes, attributes, methods, an object that guards its state, objects in objects, the text form, inheritance and `super` | calls in several steps; a shape the check makes itself shows polymorphism |
| 10 | `kata-bits` Bits und Zahlensysteme | bits and bases with `/` and `%` | |
| 11 | `kata-recursion` Rekursion | a function that calls itself | `recursive: true`, `loops: false` |
| 12 | `kata-functional` Funktionen als Werte | lambdas, closures, `map`, `filter`, `reduce`, chains | loop methods switched off with `forbid` where `map` or `filter` is the point |
| 13 | `kata-search` Suchen | linear search, halving | building blocks of the Sorting Sandbox |
| 14 | `pip-3` Der Mampfer | counting steps, the light, breadth-first search | `generate: chase`, `cave_map`, `facing` |
| 15 | `kata-tests` Tests schreiben | tests that catch every broken version | check type `tests` |

Each set has about ten core Katas at three levels (Einstieg, Übung, Knobelei)
and two Extras (`extra: true`, counting for nobody), its teacher note, and a
curriculum line (`curriculum:`) that teachers see in the strip.

The code:

- **Runs:** `kata_runner.rb` (one container per run from the Workspace image,
  no network, as `nobody`, limits), `kata_queue.rb` (one queue,
  `KATA_PARALLEL_RUNS`, one run per person), `src/scripts/measure-katas.rb`.
- **Checks:** `kata_check.rb` with the harnesses of `ruby_check.rb`,
  `python_check.rb` and `javascript_check.rb` (cases, `random` with a seed per
  person, `forbid`, `same_as` feedback, explained beginner errors);
  `recursion_check.rb` (Prism for Ruby, blanked text for Python and
  JavaScript); `tests_check.rb`; `pip_check.rb` with `pip_world.rb` (the world
  on the server, which answers every command and records the trace) and
  `pip_maps.rb` (mazes from the seed).
- **Pages:** `kata_page.rb` (tickets and polling, hand-ins), `kata_map.rb` (the
  strip), `kata_hand_ins.rb` and `kata_hand_ins.js` (the viewer without
  names), `pip_world.js` (the map, the player, the clicked plan),
  `tasks.js`, `code_input.js` (the editor).
- **Pictures:** `src/static/images/pip/make_sprites.py` (the 3/4 view, from the
  figures in `catalog/`), `make_icons.py` (the figures for the strip).
- **Tests:** `kata_*_test.rb`, `pip_*_test.rb`, `recursion_check_test.rb`,
  `tests_check_test.rb`, `e2e/tests/katas.browser.spec.ts`, and the examples
  of every page (`task_examples_test.rb`).


## Decisions and why

### Katas in the Workspace

- **A Kata is a task card** in `tasks.yaml`, shown with `_task(id)` on an
  ordinary activity page, counted on the class page, versioned by its
  `check`. No page, API family or admin page of its own: Code Bites had all
  of them and did not last.
- **Runs happen in a container per run behind one queue**, never in the
  student's Workspace or the Ruby container. One queue for everyone, first
  come, first served; capacity problems are not expected.
- **Teachers see who solved which Kata, not the code.** A student may hand a
  solution in to be shown in class (»Einreichen«, not »Meiner Lehrkraft
  zeigen« and not »Abgeben«, the label of hand-in tasks): their own choice,
  taken back at any time with »Zurücknehmen«, shown numbered and without
  names until the teacher switches them on.
- **JavaScript is a language of the Katas only.** The class setting stays Ruby
  or Python; a choice of JavaScript is stored apart (`u.kata_code_language`).
- **The catalogue follows ideas, not data types** (Code Bites sorted by
  numbers, strings, arrays), in the order of »Algorithmisches Problemlösen«
  in the Sek I plan. A class with no programming starts with Pip; nothing is
  locked, `requires:` only recommends.
- **No Kata twice:** the merge step is in Listen, the sieve the Extra of
  Wiederholen, counting comparisons the Sorting Sandbox's; Suchen gives two
  building blocks of sorting and links the sandbox.
- **Prices and other sums in cent**, so that no result depends on the order of
  adding decimals. FizzBuzz is introduced as an English counting game, »Brot«
  as the one from German primary schools.
- **Functions as values come after Rekursion** (`kata-functional`, added on
  28 October 2026): it needs lists and, for one `reduce`, dictionaries
  (`requires: [kata-dicts]`), and it is the other half of functional
  programming beside recursion, for the Oberstufe's »Vertiefung Applikative
  Programmierung«; the empty list as `reduce`'s start value is the case
  that ends a recursion. Whether a loop is used cannot always be checked:
  `forbid` switches off Ruby's `each`, `for` (which calls `each`), `times`
  and `loop` and JavaScript's `forEach`, never `while` or a `for` in Python
  and JavaScript, and in Ruby only where no `reduce` is needed, since
  Enumerable's `reduce` calls `each` and the forbidden method would fire. A
  `same_as` with a loop replaces the message »Du schreibst es gerade
  selbst«, which does not fit there. Python's List Comprehensions pass: in
  Python they are the usual way to write `map` and `filter`. One Kata
  (»Zum ersten Mal im Minus«) is there to show where a loop with `return`
  stays clearer, and the page shows its `reduce` version afterwards.
- **Examples never imitate a difficulty a student may have:** a »stutter«
  function became one putting letters in brackets.

### The card and the strip

- **One button, »Ausführen«** with a play icon: it runs a function Kata's own
  program first when it has code of its own, shows what it prints, then
  checks; at first there were two buttons, which confused.
- **The level is a word with three bars**, not dots; Extras say »Extra«.
- **»Lösung einsetzen«** beside a Kata's buttons for teachers puts the
  solution into the editor; the solution is never in the card's HTML.
- **Visitors** see a Kata's code read-only in the same editor, with colours
  and line numbers.
- **No page `/katas`:** the front page has one strip of the sets, no taller
  than a row of cards. The maintainer tried an empty ring beside it
  (discouraging), the sets as coloured pies (too busy), one bar in segments
  and stacked bars per set (too small), and dropped the total: each set
  shows its own progress.
- **The strip** is dragged with a mouse and thrown (it glides to the set it
  would come to rest at, without a snap afterwards), swiped on a phone (a set
  snaps into the middle), has arrows that fade out at its ends and fades into
  the page where more follows; text on it is not selectable. A set is a
  link like every card, without a button (the maintainer, after a button
  of its own first): the click that ends a drag opens nothing. A set
  pinned for the group is marked and centred; on hover a set casts a soft
  shadow in its hue.
- **Icons, no pictures of cards** (a solved card gives a solution away), on a
  band in the set's hue. The hue comes from the set's place in the strip,
  equally spaced from Pip's lantern amber, no exception (the maintainer).
  Pip, Bello and the Mampfer are drawn in grey and tinted; Pip and Bello
  at two and a half times, every pixel of the two alike, the Mampfer at 1.6,
  so that each is about as large as the other icons; they zoom on hover as
  one layer, which keeps the zoom smooth and the picture unchanged when it
  stops. Buttons and progress stay green, the bars striped.
- On a page with a language switch the kind label does not name the
  language (»Übung«, not »Übung (Ruby)«); the chosen language's pill slides.

### Pip

- **The story** continues the 2D Game Studio's demo game (Funkelbach, the
  Wichtel, Bello, the Glibber) and ends in itself, without a teaser. The plan
  is written first and then runs because Pip cannot hear you underground.
  Texts are drafted with the maintainer: short sentences, a little humour,
  the Wichtel's voice for instructions.
- **Plain plans:** no class, no `self`, no imports (the old wizard tasks of
  hackschule.de had students subclass `Wizard`); English command names even
  for twelve-year-olds; real loops of each language from the first loop on;
  in Ruby calls with and without parentheses work, pages write them without.
  `turn_right` is missing until students write it (Karel the Robot by
  Richard E. Pattis, named in the teacher note).
- **The first chapter is clicked, not typed:** buttons move Pip and write the
  plan, which is only watched; »Rückgängig« (not »Zurück«, which the player
  has) takes the last line back; at the lamp it is checked by itself. Pip
  starts facing down, so »left is Pip's left« is felt before a prediction
  asks it. The browser moves Pip itself, so such a map has only walls, stones
  and the lamp.
- **Turn-based and checked on the server:** each command is one step, the
  world on the server decides everything and records a trace, the browser
  only plays it back. Sensors arrive chapter by chapter, so plans react
  instead of remembering a path; several maps, some from the seed, where a
  chapter is about a general solution; the feedback names the map, the line
  and what happened.
- **Found by simulating every map** (scripts searched for maps the intended
  plan solves and the naive one does not): the Glibber maps of Teil 1 pass no
  fixed plan with up to eight waits; `on_lever` was added, since without it
  Pip cannot know where a lever is; `trail_ahead` replaced »prints here«,
  which would have meant stepping off the trail; the last chapter of Teil 2
  is a tunnel without branches, since the hand on the wall walks into every
  shaft and meets the Glibber head-on; the island Extra shows that the left
  hand circles for ever while the right one arrives.
- **The 3/4 view:** the 2D Game Studio's figures, everything around them
  drawn anew with dithering, light and shadow, mixed corners, side faces on
  left walls only, every figure in the middle of its field and behind the
  rock in front of it; spikes instead of a gate. Turns and taking a stone
  play faster than a step; the speed holds for the page and the next one.
  Pip speaks sentence by sentence in Pixelify Sans.
- **Teil 3, the Mampfer:** the monster of the old Hackschule, which always
  found the shortest way, kept as the way into breadth-first search. The
  sprite is 0x72's »big demon« from the 16x16 DungeonTileset II (CC0,
  credited on the page). After every step of Pip, a turn, `wait` and `take`
  included, it goes one tile along a shortest way: Pip standing still is
  caught (the maintainer). The way is the Handbuch's `breadth_first_search`
  from Pip, neighbours east, north, west, south, the Mampfer stepping to its
  tile's predecessor, so the widget »Was der Mampfer sieht« (the Algo
  Stepper on a map, `data-grid`) and the students' own search are the same
  algorithm. It is not blind: its eyes are made for the dark and a burning
  lamp dazzles it, so it never steps into the light, the 3 × 3 tiles round
  a lamp Pip lit earlier, not even onto Pip standing there. A plan that
  ends before Pip is at the goal does not end the run: Pip stands still and
  the Mampfer keeps coming, step by step (»nach dem Plan« in the player, no
  line), until it has Pip for breakfast (Pip vanishes under it) or cannot
  come nearer; not after a crash, which keeps its own message. A puzzle that
  depends on which of two equal ways it takes is unfair and was dropped.
  Because turns cost a step, the generated caves of the last chapter are
  checked by simulation: every shortest way escapes even turning left only,
  both wall-following plans of Teil 2 are caught, and the lamp sits at two
  thirds of the longest way, so that the way is long but not all corners.
