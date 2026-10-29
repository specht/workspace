# Katas and Pip unter Funkelbach

**Katas** are short programming exercises typed straight into a card on the
page, in Ruby, Python, JavaScript, Dart or Java, without terminal and files: a way to start
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
  `kata_compiled.rb` for Dart and Java, which compile before they run (the
  code as a library for DartCheck's harness, or as members of a class as in
  a compact source file of Java 25; one compiler run for the code and its
  typical wrong solutions); `recursion_check.rb` (Prism for Ruby, blanked
  text for Python, JavaScript, Dart and Java); `tests_check.rb`; `pip_check.rb` with `pip_world.rb` (the world
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
- **JavaScript, Dart and Java are chosen apart.** A person's own switch stays
  Ruby or Python; a choice of JavaScript, Dart or Java is stored apart
  (`u.kata_code_language`), and a class can rank all five.
- **Dart and Java have every set except Tests schreiben and Pip**, whose
  check types (`tests`, `pip`) run only in Ruby, Python and JavaScript. A
  Dart or Java card uses that language's types in its signature, its own
  forbidden built-ins and its own typical mistakes (the remainder of a
  negative number differs in all three, Java compares texts with `equals`);
  a typical wrong solution must compile, since in Java one that does not
  stops the person's own code too.
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

### Turtle

An introduction to programming beside Pip, with pictures: a turtle on a
sheet of paper that draws a line behind it (`turtle-1` and `turtle-2`, in
development since 28 October 2026). What the maintainer decided and why:

- **On the website, not in VS Code**, like the Katas: the code is typed into
  a card and kept per language until the student deletes it.
- **Dead simple:** the commands are plain functions (`forward 100`,
  `left 90`, `pen_up`), no object, no class, no import; students write
  their own functions later, never classes.
- **It runs in the browser** (`turtle_worker.js`, one module worker per
  language), live: the turtle draws while the program runs, until it ends or
  »Stopp« ends the worker; endless programs are allowed. Ruby is ruby.wasm
  without its standard library, Python is Pyodide, JavaScript runs as it is
  (`fetch-turtle-runtimes.sh`, not in git; nginx serves them gzipped and
  cached). Measured before deciding: about 6 MB for Python and 5 MB for Ruby
  compressed, loaded once and only for the language that runs; a fresh Ruby
  VM per run (40 to 230 ms) so that nothing of an earlier run is left over.
  Pip's plans stay on the server: Pip's world answers questions, the turtle
  only takes commands.
- **The speed is pixels of line per second on the screen** (the maintainer,
  at a fern that took ten minutes at »Schnell«): 100, 300 and 2000 at
  »Langsam«, »Normal« and »Schnell«, measured on the paper as it is zoomed,
  so a picture of 40000 short lines takes no longer than a square; turns
  take no time, but at »Langsam« at most a quarter second, to watch the
  turtle turn. The worker waits after each line too (a busy wait), at
  60000 steps per second, faster than the page draws at »Schnell« even
  zoomed far out: an endless program runs on for a while, and »Sofort« in
  the middle of a run draws the rest at once.
- **The check on the server:** a program that ends by itself sends its
  commands; `TurtleWorld` replays them and compares the lines with the
  task's picture by sampling both, so order, direction, pieces and rounding
  do not matter. A determined student could fake the commands; for a card
  that only says »gelöst« that is accepted. What is missing is drawn dashed,
  what is too much in red. A stopped run only keeps the code; the card's
  state stays (a solved card stays solved).
- **One set of commands per language** (`turtle.rb`, `turtle.py`,
  `turtle.js`), used by the worker and by the examples on the host
  (`TurtleCheck.record`), so that both run the same code.
- **The speed changes while a program runs** (the maintainer): the worker
  paces as above, the page draws what came at the speed chosen now;
  »Sofort« at the start sends everything at once. Shared memory
  between page and worker would let the worker read the buttons itself, but
  needs cross-origin isolation for the whole page.
- **The paper fits the picture** (the maintainer, at the Koch curve that
  used a quarter of it): centred on the picture asked for, at least ±150
  steps, zooming out around everything drawn when a drawing goes past the
  edge, gliding there in 0.6 s instead of jumping (the maintainer); the grid stays 50 steps apart and is left out where it would be
  too dense. `show:` names the call whose picture a card shows.
- **The commands at hand** above the paper, as Pip's (the maintainer); both
  widgets show them in the editor's colours, with a quiet »Befehle«.
- **Two pages** (the maintainer: the recursive parts later, when the
  students are there): `turtle-1` after Pip 1, with commands, loops, own
  functions, pen and colour, a variable that grows, stars, circles, one's
  own picture to hand in; `turtle-2` after the Katas on recursion, with
  trees (a beautiful one in brown and green, by the maintainer's wish), a
  fern and curves: Sierpiński, Koch, Hilbert as an extra; its icon a
  Hilbert curve (`images/icons/hilbert.svg`, the maintainer's idea). Two
  predictions on the first page: the turn at a triangle's corner (120, not
  60) and 91 instead of 90 degrees. A third, `turtle-3` »Pflanzen, Zufall
  und Farbe« (the maintainer: a Kata set, not an activity of its own):
  colour from a number (the hue) and mixed from two, `push` and `pop`
  (given as commands, as a canvas has `save()` and `restore()`; the
  maintainer: this is about drawing, not about building a stack), `random`,
  and Lindenmayer systems, a word drawn letter by letter and grown by a
  rule, with a level picture of the plant. Every page ends with a free
  section (the maintainer), and turtle-1 lost its weaker cards (a hexagon
  after the square loop, a pattern counted in lines, many-pointed stars).
- **Not only tracing** (the maintainer): free cards with a goal instead of
  a picture (`free:`, also with `max_lines`, which then asks for a loop):
  one's own pattern, flower, tree and picture.
- **A picture on its way is not »wrong«** (the maintainer: developing a fern
  takes many runs, and a card should not say at each one that it is wrong
  and where): a run that does not match yet gets one quiet sentence (status
  `drawn`, neutral like a note; for a function, which call differs), and
  the dashed and red differences with their sentence only on »Unterschiede
  zeigen«. The stored state is still »noch nicht gelöst«.
- **Colours to pick from** (the maintainer): the chips `color` and
  `background` above the paper open a palette, Cling (the site's palette,
  from Pixelflow and the 2D Game Studio) and some HTML names by hue; a click
  puts the colour into the program where the cursor was, as a hex code or a
  name, so that no new syntax is needed. `background` colours the paper,
  with the same colours as `color` (the grid turns lighter on a dark one);
  the comparison ignores it, a handed-in picture keeps it. Every card lists
  its commands, so that turtle-1 does not show those of turtle-3.
- **Several drawings on a free card** (the maintainer): on every card with
  `free:` a row »Bild« lists the person's drawings by name, with Neu,
  Umbenennen and Löschen; the open one is in the editor and is the card's
  answer as before (`TaskState.sketches`, `/api/turtle/sketch`,
  `TurtleSketches`). »Einreichen« hands in the open one, any number of
  them: as its code and an SVG the server draws from the commands of its
  last run, so that the class board shows pictures and no student's code
  ever runs in someone else's browser (a worker running JavaScript or
  Pyodide could call the site with the teacher's session).
- **Solved, then free** (the maintainer: experiments are not punished): once
  a card is solved, a run that draws something else keeps its code, the
  card stays solved, and the card says that nothing is checked any more,
  without the dashed and red differences (`/api/turtle/check`, status
  `free`); a run that matches again says so as before.
- **The picture asked for is a thin pencil line** under the ink (the
  maintainer found it too dark and thick); it stays visible after drawing.
- **Recursion is shown, not dictated** (the maintainer, at a Hilbert prompt
  that was a recipe to transcribe): a level widget (`<div
  class='turtle-levels' data-figure='hilbert'>`, turtle_canvas.js) draws
  each level with the parts made of the level below in colour; the cards
  then only say what to write, and the tips lead there. Each level fills
  the paper (small ones zoomed in), the view glides from one level to the
  next, and the level before stays faint behind (the maintainer); the
  Hilbert curve's connecting lines are black, grey was too close to the
  parts' colours. Trees also grow
  unevenly and hang down to the ground (the maintainer).
- **One speed for all cards of a page**, and a line on its way takes the
  new speed at once (the maintainer: it did not react for a while); the
  player draws at most about 12 ms per frame.
