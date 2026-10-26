# Plan: Katas, Pip unter Funkelbach and unit tests

A handover for the next session, written on 25 October 2026; what is built
since is under »Progress« at the end. Read [AGENTS.md](../AGENTS.md),
[didaktik.md](didaktik.md) and [aufgaben.md](aufgaben.md) first; this plan
builds on them.

**Katas** are short programming exercises typed straight into a card on the
page, in Ruby, Python or JavaScript, without terminal and files: a way to start
coding before the Workspace's tools, and to practise alongside them. They bring
back what Code Bites offered (removed on 2026-10-18), as tasks of the Workspace
instead of a system beside it. **Pip unter Funkelbach** is the way in: a story
in which students steer Pip from the 2D Game Studio through dark tunnels with
programs. **Unit tests** get an activity of their own and a check type that
judges the tests students write.

**The name is »Kata«** (decided 26 October 2026): a code kata is the
programmers' word for a small exercise repeated until it is fluent; it reads
the same in German, »eine Kata, zwei Katas«. `kata` in code.


## Decisions already made

* **A Kata is a task card**, defined in `tasks.yaml`, shown with `_task(id)` on
  an ordinary activity page, counted on the class page, versioned by its
  `check`. No page, template, API family, task format or admin page of its own:
  that is why Code Bites did not last (it had all of them; the old code is in
  `git show 8a6bead5^:src/codebites/…`).
* **Runs happen in a dedicated container, one per run, behind a global queue**,
  never in the student's Workspace and never in the Ruby container.
* **Teachers see who solved which Kata**, as for every task, not the code.
* **Students may hand in a solution** for the teacher to show and discuss in
  class: their own choice, never automatic (Datenschutz). Seeing other
  students' solutions automatically does not come back.
* **JavaScript is a language of the Katas only.** Other activities keep Ruby and
  Python, and the class setting »Sprache« stays as it is.
* **Pip's world is top-down** and tells a story.
* **Pip's programs are plain plans**: no class, no `self`, no imports; English
  command names even for twelve-year-olds; real loops from the first loop on;
  in Ruby, calls with and without parentheses are both accepted (section 2).


## 1. The catalogue

### Ordered by idea, not by data type

Code Bites sorted by data type (Zahlen, Strings, Arrays). The new catalogue
follows the ideas of programming in the order they build on each other, the
order of »Algorithmisches Problemlösen« in the Sek I plan and of
Softwareentwicklung in the Oberstufe. Numbers, texts and lists appear wherever
they serve the idea. Each set is one activity page (`kind: uebung`, `languages:
[ruby, python, javascript]`) with its own teacher note, so the class page,
pinning a set for a group (the »angeheftet« activities of `/klasse`) and
`requires:` work as for every other activity.

```
                 Pip, Teil 1 ─────────────┐
                 (no prior knowledge)      │
                                           ▼
   ┌──────────── 1 Erste Funktionen ── 2 Entscheidungen ── 3 Wiederholen ───┐
   │                                                                        │
   │                Pip, Teil 2 ◄──────────────────────────────────────────┤
   │                (after 3)                                               │
   ▼                                                                        ▼
 4 Texte ─────────── 5 Listen ─────────── 6 Dictionaries         7 Bits und Zahlensysteme
   │                   │
   └──────── 8 Rekursion ──── 9 Suchen ──────────────── 10 Tests schreiben
```

A class with no programming yet starts with Pip; a class that has programmed
starts at set 1, or wherever the teacher pins. Sets 4 to 7 are side by side:
after set 3 a teacher picks what the lesson needs. Nothing is locked: the arrows
are recommendations (`requires:`), shown on the overview, never enforced.

### Levels inside a set

Every set has the same shape, so students and teachers know what to expect:

* **Einstieg**: the idea in its simplest form, one or two lines of real work,
  with an example in the prompt that nearly gives the way. About four.
* **Übung**: the idea applied, a little thinking, edge cases. About four.
* **Knobelei**: needs a plan before code, often combines with an earlier set.
  Two or three.
* **Extras**: for the fast and the curious, optional: a harder variant, a
  constraint (»ohne Schleife«, »ohne `sort`«, »in höchstens fünf Zeilen«), a
  famous problem. One to three.

The card shows its level as a small label with a Bootstrap icon (one, two or
three `bi-circle-fill` dots, or the `bi-stars` of extra chapters for Extras),
never a Unicode star. Within a set the page lists the Katas by level; students
may jump. A set of ten to twelve core Katas is about one Doppelstunde for a
class that knows the idea from the lesson, with the Extras for those who finish.

### Extras that do not spoil the count

Today an extra chapter may not hold a task, because »x von y Aufgaben« would
never be complete for those who skip it (docs/checkliste.md,
`extra_chapters_test.rb`). Katas need optional tasks, so they get a field:
`extra: true` on a task. Such a task counts for nobody's »x von y«; the card,
the page and the class page show it apart (»8 von 10, dazu 2 Extras«, the extra
columns lighter on the class page). That is a small change to task cards, the
progress line, the class page and the rule in docs/checkliste.md and
aufgaben.md; make it before the first set, and let other activities use it too
(it may replace some `hint try` boxes that wanted to be checked).

### The overview page

A page `/katas` (the first entry of the catalog section »Katas«) shows the map
above as a picture with every set as a stop: its title, its idea in a few words,
the levels it has and, for a logged-in student, how far they are (solved of
core, extras solved), from the task states the site already has. A teacher sees
the map with the curriculum line under each set (below). The catalog section on
the start page lists the sets as cards in this order.

### The sets

For each: the idea, a few Katas per level (enough to show the shape; the
authors fill in the rest), the curriculum line for the overview and the teacher
note, and where the old material goes. Old Code Bites tasks are in
`src/codebites/tasks/*.rb` (cases, random generators, starters, forbidden
built-ins), hackschule.de tasks in that site's source; »(CB)« and »(HS)« mark
them.

**Pip unter Funkelbach, Teil 1 · Hinab** (section 2)
Sequence, loops, conditions, waiting, in a world that shows every step. Sek I:
Algorithmisches Problemlösen, Miniwelten.

**1 · Erste Funktionen**
A function takes values and gives one back; arithmetic and joining text.
* Einstieg: add (CB add_numbers), double, `greet('Ada')` → `'Hallo, Ada!'` (HS
  Persönliche Begrüßung), area of a rectangle
* Übung: Celsius to Fahrenheit, seconds to `'2:05'`, average of three numbers,
  the price of n items with a discount
* Knobelei: the time on a clock after n minutes (`%`), the last digit of a
  number
* Extras: a number's digits reversed without strings
* Sek I: Variablen, Ausdrücke, Prozeduren

**2 · Entscheidungen**
`if`, `else`, comparisons, `and`/`or`, a function that returns `true` or
`false`.
* Einstieg: even (CB), max of two (CB max_of_two), absolute value, `is_adult`
* Übung: leap year, grade from points, ticket price by age, max of three
* Knobelei: FizzBuzz for one number, a triangle from three sides (none,
  isosceles, equilateral), Pythagorean triple (CB, HS)
* Extras: rock, paper, scissors (who wins)
* Sek I: Kontrollstrukturen

**3 · Wiederholen**
Loops over numbers, a variable that collects.
* Einstieg: sum 1 to n, the numbers 1 to n as a list (HS Zahlenreihe), squares
  (HS Quadratzahlen), countdown
* Übung: factorial (CB), digit sum, Fibonacci (CB), count the divisors
* Knobelei: prime (CB), gcd (CB, with `forbid`), Collatz steps
* Extras: perfect numbers, all primes up to n (CB primegen) with a time limit
  that a naive loop misses
* Sek I: Kontrollstrukturen; Oberstufe: Algorithmen

**Pip unter Funkelbach, Teil 2 · Bellos Spur** (section 2)
`while`, functions of one's own, variables, combined conditions, an algorithm
on generated mazes. Comes after set 3, because it uses all of it.

**4 · Texte** (built, `kata-texts`)
Strings: characters, positions, building a new text.
* Einstieg: reverse (CB reverse_string), count vowels (CB count_vowels), the
  first letter of each word, to upper case without `upcase` (HS
  Großbuchstaben)
* Übung: palindrome (HS), word count, longest word, slug (`'Hello World!'` →
  `'hello-world'`)
* Knobelei: Caesar (HS Cäsar-Chiffre), anagram (HS), names formatted (`'max
  mustermann'` → `'Max Mustermann'`)
* Extras: Vigenère, password rules
* Sek I: Information und Daten (Codierung, Verschlüsselung); links to `crypto`

**5 · Listen** (built, `kata-lists`)
Going through a list, building a new one, positions (Index, not »Stelle«).
* Einstieg: sum (CB array_sum), max (CB array_max), count the even numbers,
  contains
* Übung: reverse (CB array_reverse), swap first and last (CB), rotate left
  (CB), sorted? (HS Monotonie)
* Knobelei: remove duplicates, the missing number (CB find_missing_number),
  merge two sorted lists. The sieve of Eratosthenes, meant to move here from
  set 9, stays where it already is: the Extra of set 3 (all primes up to a
  million in six seconds).
* Extras: two numbers that add up to a target (their indices, with two
  loops; the one pass with a dictionary is hinted at for set 6), chunks
* Oberstufe: Datenstrukturen; links to the Data Structures Sandbox

**6 · Dictionaries** (built, `kata-dicts`)
Keys and values: counting, grouping, looking up. A Hash in Ruby, a
Dictionary in Python, an object in JavaScript.
* Einstieg: the phone book (a missing name gives `'unbekannt'`), a tally of a
  list, the price of a shopping list (in cent, so that no sum depends on the
  order of adding decimals), the names with so many points
* Übung: how often each letter, the most frequent word, group words by
  length, two tallies together
* Knobelei: invert (each value with the list of its keys), the average per
  subject, the first number that comes again (100 000 numbers in ten
  seconds: searching a list each time is too slow)
* Extras: count the misses of a small LRU cache (a check calls one function,
  so a cache with state becomes counting), groups of anagrams
* Count words and two-sum, planned here first, are already in sets 4 and 5.
* Oberstufe: Datenstrukturen (Hash Table); links to the Handbuch page
  `hash-tables`

**7 · Bits und Zahlensysteme** (built, `kata-bits`)
Numbers as bits, other bases, only with dividing and remainders.
* Einstieg: a number in bits, bits back into a number, a single bit, how
  many bits a number needs
* Übung: count the ones, hexadecimal and back, is it a power of two
* Knobelei: two's complement of a byte, any base from 2 to 16, from one base
  into another
* Extras: Roman numerals, written and read
* Sek I: Information und Daten (Zahlensysteme); links to the Handbuch page
  `bits-und-bytes`

**8 · Rekursion** (built, `kata-recursion`)
A function that calls itself, with a case that stops. The check reads the
code once the cases pass: `recursive: true`, `loops: false`
(RecursionCheck; the maintainer asked whether static analysis could see a
recursion, 26 October 2026).
* Einstieg: factorial, the sum of a list, a countdown as a list, the length
  of a text without `length`
* Übung: power, Fibonacci, palindrome, how often a character occurs
* Knobelei: Towers of Hanoi (the moves as a list), flatten a nested list,
  all orders of a text (both with a loop beside the recursion)
* Extras: Fibonacci with a memo under a time limit, subset sum
* Oberstufe: Algorithmen (rekursive Verfahren)

**9 · Suchen** (built, `kata-search`)
Finding something in a list, and halving. Whole sorting algorithms are the
Sorting Sandbox's (`sandbox-sortieren`: written in Dart, watched, raced,
measured, checked for stability); this set gives two of their building blocks
and ends with a link there: the minimum's index is what selection sort is
made of, inserting into a sorted list what insertion sort is made of. The
merge step is set 5's (merge two sorted lists), counting comparisons the
sandbox's Analyze (decided with the maintainer, 26 October 2026).
* Einstieg: linear search (the index or `nil`), the minimum's index, the
  last occurrence, the first number above a limit
* Übung: binary search, guess the number by halving (the list of guesses),
  the first index from which a sorted list reaches a limit, the integer
  square root by halving up to a trillion
* Knobelei: insert into a sorted list keeping it sorted, is a list a
  permutation of another (counting, without sorting), where a rotated
  sorted list starts again
* Extras: binary search that counts its steps; 100 000 searches in 100 000
  numbers in five seconds
* Oberstufe: Algorithmen, Effizienz; links to `sandbox-sortieren`

**10 · Tests schreiben** (built, `kata-tests`, with the check type `tests`)
Writing tests instead of code, with the check type `tests` (section 4): for each
Kata a function with a description, the student writes test cases, broken
versions must fail them. Typed into the card, the tests are lines
`assert_equal(expected, actual)` that the harness provides, the same in the
three languages, no framework (TestsCheck, docs/aufgaben.md »tests«).
* Einstieg: tests for `even`, `max_of_two`, `absolute`
* Übung: tests for `leap_year` (the century rule), `slug`, `fizzbuzz`
* Knobelei: tests for binary search (one element, the edges, not there),
  for merging two sorted lists
* Extras: find the bug in a given `max_of_three` by writing the test that
  shows it; tests for `caesar`
* Oberstufe: Softwareentwicklung (Testen); leads to the activity »Testen«

Not in the catalogue: what needs files or the network (CSV, JSON, a URL
shortener) belongs in Workspace activities. The quine (HS) is an Extra of set 4
once a whole-program mode exists (run the program, compare its output with its
source).


The numbers here count the sets of functions only. The strip on the front
page counts in catalog order, Pip's two parts included: there Erste
Funktionen is 2, Wiederholen 4, Pip Teil 2 is 5, Texte 6, Rekursion 10,
Suchen 11 and Tests schreiben 12.


## 2. Pip unter Funkelbach

### The story

Pip is the hero of the 2D Game Studio's demo game »Pip und das letzte Licht«
(`specht/2d`, `rezepte/demo/letztes-licht.yaml`): the village Funkelbach, the
Wichtel who talks too much, Pip's dog Bello, Sigi with his shop, the Glibber in
the caves. The Katas tell a new story in the same world.

> Unter Funkelbach liegen alte Stollen. Früher haben die Leute dort
> Funkensteine gegraben, Steine, die von selbst leuchten. Eines Abends jagt
> Bello einem Glühwürmchen nach, springt in einen Schacht und ist weg. Pip will
> ihn holen, aber die Stollen sind eng, finster und voller Glibber, und unten
> hört Pip niemanden mehr. Der Wichtel hat die alten Karten der Stollen.
> »Schreib Pip einen Plan«, sagt er, »Schritt für Schritt. Pip macht genau, was
> draufsteht. Nicht mehr und nicht weniger.«

That is why the program is written first and then runs: Pip cannot hear you
underground. Funkensteine are what Pip collects; they keep the lantern
burning. Draft every text in German with the maintainer: short sentences, a
little humour, the Wichtel's voice for instructions, Bello for hints (a bark
from the right direction).

### What the old dungeon was, and what changes

The five wizard tasks of hackschule.de (»Wizard: Einführung«, »Münzsammler«,
»Münzspirale«, »Zickzack«, »Monster-Mampf«) had students write a class:

```python
from wizard import Wizard

class Fili(Wizard):
    def run(self):
        self.forward()
        self.turn_left()
        if self.coin_here():
            self.get_coin()
```

with `forward`, `turn_left`, `turn_right`, `say(message)`, `wait(seconds)`,
`coins()`, `coin_here()`, `get_coin()` and `set_speed(speed)`. The `self.` was
the price of checking (the platform called `run` on a subclass), and every
task page had to remind students to write it. `coin_here()` was the only
sensor: no walls, no monster, so a program could only be a fixed list of moves
for one known map. The monster task was solved by guessing seconds for `wait`.
As far as the pages show, each task had one map, so a hard-coded path always
passed, and the only feedback was »dein Programm ist korrekt«.

Pip keeps the pleasure of steering a figure through a dungeon and changes the
rest:

* **No boilerplate**: the card holds only the plan (below).
* **Turn-based**: each command is one step, and the Glibber moves one tile per
  step, so a run is exactly reproducible; `wait()` is one step, not seconds.
* **Sensors arrive chapter by chapter**, so programs react instead of
  remembering a path.
* **Several maps, some generated**, wherever the chapter is about a general
  solution.
* **Feedback that names what happened**, with a replay (»In Karte 2 läuft Pip in
  Zeile 6 gegen die Wand«, »Hier liegt kein Funkenstein«); in the old version
  such a mistake just failed the program.
* **Kept**: `say("…")`, now a speech bubble over Pip, because it is fun.
  `turn_right` is deliberately missing until students write it themselves in
  chapter 7, as in Karel the Robot.

### The code students write

**Decided**: English command names, even for twelve-year-olds (as all code in
the Workspace; the story, prompts and feedback are German). Real loops of each
language from the first loop on, no `repeat` helper. In Ruby, calls with and
without parentheses are both accepted (`move` and `move()`, `front_clear?` and
`front_clear?()`); pages and solutions write them without, as Ruby
programmers do.

**The card holds only the plan**: no import, no class, no `main`. The whole
solution of the first Kata:

```python
move()
move()
turn_left()
move()
take()
```

```ruby
move
move
turn_left
move
take
```

```javascript
move();
move();
turnLeft();
move();
take();
```

Each language stays idiomatic, so the habits carry over to real programs:
Ruby's `front_clear?` with a question mark and `4.times do … end`, Python's
`front_clear()` and `for i in range(4):` (`i` rather than `_`, which puzzles
beginners), JavaScript's `frontClear()` and `for (let i = 0; i < 4; i++)`.

How it is checked without `self`:

* **A prelude**, invisible to the student, defines the commands as ordinary
  global functions: in the namespace the Python code runs in, as methods of
  Ruby's top-level object, as functions in the scope of a JavaScript wrapper.
  Line numbers in errors and in the replay are shifted back by the prelude, so
  they point at the student's own lines.
* **Each command is one request**: it writes a line to the world engine and
  waits for the answer (»done«, »wall«, `true`/`false` for a sensor). The
  engine is the one on the server (»The world« below); the executor connects
  the program's stdin/stdout to it.
* **Each map runs the program from scratch**, so nothing carries over between
  maps.
* **Functions of one's own** (chapter 7) are checked like the `ruby`/`python`
  checks load files: definitions only, then the check puts Pip on a test map,
  calls `turn_right()` and compares where Pip stands and faces. A `turn_right`
  that turns left once passes the first map but not the second, and the
  feedback says so (»`turn_right()` dreht Pip nach links«).

Small rules that keep it simple: `say` and `print` cost no step (`say` is a
bubble over Pip, `print` goes to the console); sensors never change anything;
`take()` on an empty tile, `pull()` without a lever and walking into a wall end
the run with that event, never with a stack trace. A step limit stops endless
loops (»Nach 1000 Schritten angehalten: Läuft Pip im Kreis?«).

What students write over the chapters, in Python:

```python
# 3 · Die Wendeltreppe: a loop with a counter
for side in range(1, 6):
    for step in range(side):
        move()
        take()
    turn_left()

# 5 · Glibber!: a condition inside a loop
for i in range(14):
    if glibber_ahead():
        wait()
    else:
        move()

# 6 · Im Finstern: while
while front_clear():
    move()
    if on_stone():
        take()

# 7 · Eigene Zaubersprüche: a function, then one with a parameter
def turn_right():
    turn_left()
    turn_left()
    turn_left()

def walk(n):
    for i in range(n):
        move()
```

### The commands

Each comes with the chapter that needs it, never before; on pages as an HTML
table.

| Python | Ruby | JavaScript | Does | From |
| --- | --- | --- | --- | --- |
| `move()` | `move` | `move()` | one tile forward | 1 |
| `turn_left()` | `turn_left` | `turnLeft()` | a quarter turn to the left | 1 |
| `say(text)` | `say text` | `say(text)` | a speech bubble, no step | 1 |
| `take()` | `take` | `take()` | picks up the stone Pip stands on | 1 |
| `on_stone()` | `on_stone?` | `onStone()` | is there a stone here? | 4 |
| `wait()` | `wait` | `wait()` | one step standing still | 5 |
| `glibber_ahead()` | `glibber_ahead?` | `glibberAhead()` | is a Glibber on the next tile? | 5 |
| `front_clear()` | `front_clear?` | `frontClear()` | is the next tile free? | 6 |
| `on_lamp()` | `on_lamp?` | `onLamp()` | is Pip at the lamp (the goal)? | 6 |
| `pull()` | `pull` | `pull()` | pulls the lever Pip stands on | 9 |
| `on_lever()` | `on_lever?` | `onLever()` | is there a lever here? | 9 |
| `trail_ahead()` | `trail_ahead?` | `trailAhead()` | are there paw prints (or Bello) on the next tile? | 10 |
| `puddle_ahead()` | `puddle_ahead?` | `puddleAhead()` | is a puddle on the next tile? | 10 |
| `bello_here()` | `bello_here?` | `belloHere()` | is Bello on this tile? | 11 |

### Two parts, eleven chapters

Each chapter has two or three sentences of story, the map, Katas at the levels
of section 1, an Extra where it fits, and after its last Kata a line of story
that appears once it is solved (`<!-- after: id -->`).

**Teil 1 · Hinab** (the entry, no prior knowledge)

1. **Der Schacht** (from »Wizard: Einführung«). A short L-shaped tunnel with a
   lamp at the end. Einstieg: walk to the lamp with `move` and `turn_left`
   (right is three lefts). Einstieg: pick up the first stone on the way with
   `take`. Prediction before either: »Wo steht Pip nach diesem Plan?«, the
   answer a tile on the map.
2. **Funkensteine** (from »Münzsammler«). A winding tunnel with about ten
   stones. Einstieg and Übung: collect them all, as one long sequence. It is
   meant to get tedious: that is the reason for the next chapter. Extra
   (»Fehlersuche«): a given plan walks Pip into a wall; find and fix the wrong
   line.
3. **Die Wendeltreppe** (from »Münzspirale«). Einstieg: a square room, a loop
   that runs four times. Übung: a straight corridor of stones with a loop.
   Knobelei: the spiral itself, sides growing 1, 1, 2, 2, 3, 3 …, with a loop
   whose counter sets the length. The old spiral could be done as one endless
   sequence; here that is the trap the tips point out. Extra: the spiral in at
   most six lines.
4. **Die Pilzwiese** (from »Zickzack«). A meadow walked in serpentines,
   stones on some tiles only. Übung: walk the meadow with loops. Knobelei: the
   same meadow on three maps with stones in different places, so `if
   on_stone(): take()` is needed. The old task asked for a short program; now
   the maps require the condition.
5. **Glibber!** (from »Monster-Mampf«). A Glibber oozes back and forth across
   the corridor, one tile per step. Übung: one fixed map, where counting steps
   and placing `wait()` is enough. Knobelei: maps where the Glibber starts at
   different places, so the loop needs `if glibber_ahead(): wait() else:
   move()`. The old timing guesswork becomes a condition.

**Teil 2 · Bellos Spur** (after set 3)

6. **Im Finstern**. Tunnels of generated lengths. Einstieg: `while
   front_clear(): move()`. Übung: the same, collecting stones on the way.
   Knobelei: follow a winding tunnel to the lamp (`on_lamp`).
7. **Eigene Zaubersprüche**. The Wichtel's spells. Einstieg: `turn_right` as
   three lefts. Übung: `turn_around`, then `walk(n)` with a parameter.
   Knobelei: a staircase built with `walk(n)`. The check calls the student's
   functions on test maps.
8. **Das Labyrinth**. Three generated mazes per check, solved by keeping a hand
   on the wall with `front_clear` and turns. Extra: a maze with a loop around an
   island, where following the wall can circle for ever: worth thinking about.
9. **Drei Hebel**. A gate opens only after exactly three pulls, and a fourth
   closes it again; with five levers on the map, students need a counter
   variable. Extra: count the stones yourself and `say` the number at the end.
10. **Bellos Spur**. Paw prints (`on_trail`) and puddles (`puddle_ahead`) at
    junctions: `and`, `or`, `not`.
11. **Zurück ans Licht**. Generated caves with stones, a Glibber, levers and
    Bello somewhere (`bello_here`): everything together. It ends where the demo
    game begins: »Den Rest der Geschichte spielst du im 2D Game Studio.«

### What makes it fun

* **Pip moves**: walking, turning, picking up a stone, bumping into a wall (a
  little dust, »Autsch«), a Glibber that wobbles, `say` bubbles. The world is a
  widget (docs/widgets.md), and **Ausführen** plays the run step by step with the
  current line of code highlighted, at a speed the student can change (the old
  `set_speed` becomes a slider).
* **The story pays out**: every solved chapter lights a tunnel on a map at the
  top of the page, from the task states the page already has.
* **Mistakes are part of the story**: »Bello bellt von links.«, with a replay of
  the run that went wrong.

### The world

* **Maps in YAML**, one character per tile with a legend, like the scenes of
  the 2D Game Studio's recipes:

  ```yaml
  world:
    map: |
      #########
      #P..o...#
      #.###.#.#
      #...o.#o#
      #########
    facing: east
  ```

* **Several maps per task** where the idea is a general solution (chapters 4, 5,
  6, 8, 11), some generated per check from a seed (per student and task
  version): a fixed list of steps fails the second map. The card shows the
  first; the feedback names and replays the one that went wrong.
* **One engine on the server** (Ruby) runs the commands against a map and
  records a trace: every step, turn, stone, lever, Glibber move, bubble and the
  end (all stones, a wall, caught, too many steps). The check judges the trace;
  the browser only animates it, so there is one truth and nothing to cheat.
* `same_as`-style feedback recognises a typical wrong program by its trace on
  all maps (»Pip biegt immer links ab, auch wenn dort eine Wand ist.«).

### Pictures

Pip (four directions, a short walk cycle), Bello, the Glibber, stones, walls,
mushrooms, levers, gates, paw prints and puddles, drawn top-down in the 2D Game
Studio in the palette of its sprite catalogue (mostly Sweetie 16), exported as
PNG strips; the side views in `rezepte/sprites/pip/` are the model for colours
and proportions. The maintainer draws or approves them; they are the face of
the whole thing.


## 3. How a Kata works

### The card

Title, level, prompt with examples, a code editor with a starter, one button
**Ausführen** (runs the program once where it has code of its own, shows the
console, then runs the cases; first it was two buttons, »Ausführen« and
»Prüfen«), then console and feedback. The language switch of the page chooses Ruby, Python or JavaScript
(the class setting picks Ruby or Python as before; JavaScript only by the
student's switch). What has been written is stored per language as the task's
answer (`TaskStore`), there again at home, visible only to the student, who can
delete it. Stored code is new data: `src/ruby/include/privacy.rb`,
docs/datenschutz.md and the CHANGELOG change with it.

Under a solved Kata, **Einreichen** (not »Meiner Lehrkraft zeigen«, the
maintainer's choice; not »Abgeben«, the label of hand-in tasks) hands the code in as a copy
(`SubmissionStore`), which the student can take back. The teacher finds it under
»Fortschritt« like other hand-ins and opens it in the viewer, large enough for
the projector and **without the name** unless the teacher switches it on. A line
says what it is for: a solution that is short, unusual, clever or has an
interesting mistake, to discuss together; it does not have to be notable.

### Running code

* **One image** with Ruby, Python and Node, built by `build-image.sh`, a
  CHANGELOG line for operators.
* **One container per run**: no network, read-only with a small tmpfs, a
  non-root user, limits on CPU, memory, processes and time, removed afterwards.
  The code goes in on stdin, never as a file.
* **A global queue** with a configurable number of parallel runs (a value in
  `credentials.rb`; measure the server, perhaps 4), first come, first served,
  at most one run per student (a new one replaces a waiting or running one).
  The card shows the place (»Noch 2 vor dir«), then the live console over the
  page's WebSocket.
* **One protocol**: the old executors' NDJSON (`src/codebites/executor/*`) as a
  start, results in `CallCheck`'s format so the card renders feedback like
  `python` and `ruby`. Pip and `tests` use the same machinery.

### The check type `function`

Calls functions in the card's code and compares, with the `cases` format of
`python`/`ruby` (aufgaben.md), per language through »In mehreren Sprachen«
(extended by `javascript:`):

```yaml
  - id: ggt
    title: Der größte gemeinsame Teiler
    level: 3                         # 1 Einstieg, 2 Übung, 3 Knobelei
    prompt: |
      Schreib eine Funktion `gcd(a, b)`, die den größten gemeinsamen Teiler
      zweier Zahlen zurückgibt.
    starter: |
      def gcd(a, b)
      end
    check:
      type: function
      language: ruby
      forbid: [Integer#gcd, Integer#lcm]
      cases:
        - gcd(60, 48): 12
        - call: gcd(7, 7)
          expect: 7
          say: Der größte Teiler einer Zahl mit sich selbst ist sie selbst.
      random:
        count: 4
        args: [1..1000, 1..1000]
        reference: a.gcd(b)          # evaluated by the check, never shown
    feedback:
      - when: { same_as: 'def gcd(a, b) = [a, b].min' }
        say: Die kleinere Zahl ist nur manchmal ein Teiler der größeren. Probier `gcd(12, 8)` von Hand.
    hint:
      - Rechne `gcd(60, 48)` von Hand. Welche Zahlen teilen beide?
      - Euklid: Was `a` und `b` teilt, teilt auch `a - b`. Was passiert, wenn du das wiederholst?
      - |
        Ein Gerüst:

        ```ruby
        def gcd(a, b)
          while b != 0
            # hier: a und b neu belegen
          end
          a
        end
        ```
    python: { starter: …, check: { type: function, language: python, forbid: [math.gcd], cases: … } }
    javascript: { starter: …, check: { type: function, language: javascript, cases: … } }
```

* **Random cases** with a seed per student and task version: the same cases
  on a second check, and a table of answers does not pass.
* **`forbid:`** disables built-ins before the code loads; the error says which
  and why (»`Integer#gcd` gibt es in dieser Aufgabe nicht: Du schreibst sie
  gerade selbst.«).
* **A time limit per case** where efficiency is the point (the Extras of sets 3,
  8, 9), with feedback that says so.
* `task-examples.yaml` gets a solution and a starter for every language, and
  `task_examples_test.rb` runs them.

### Feedback and help (docs/didaktik.md)

* **Feedback that names the mistake**: the failing call, what came back, what
  was expected, `say` on the case. `feedback` with `when: { same_as: CODE }`,
  the counterpart of SQL's `same_result_as`: a typical wrong solution, written by
  the author and never shown, runs on the same cases, and if the student's
  results match it, its `say` replaces the generic line. One for each mistake
  classes make. Error messages that would stop a beginner (`IndentationError`,
  `undefined local variable or method`, `is not defined`) are explained, the
  original underneath.
* **Tips in steps** (`hint:` as a list): from an example by hand to the way in
  words to a scaffold, never the solution, a minute apart, kept in the browser.
  Every Kata from Übung up has them; an Einstieg needs one at most.
* **The solution after solving**, in the card's language, to compare.
* **Predictions** for Katas about reading code (»Was gibt `mystery(5)`
  zurück?«) and in Pip (»Wo steht Pip nach diesem Plan?«).


## 4. Unit tests

### The activity »Testen«

A Workspace activity with files and terminal, `languages: [ruby, python]`,
after set 10 or on its own. Students test a familiar function (`slug` from set
4), watch a test fail, find the bug their tests reveal in a second, broken
module, then write tests before the code for a new function (red, green,
refactor) and learn what makes a good test: edge cases, one idea per test, names
that say what is expected. Check that the image has `pytest` and `minitest`.

### The check type `tests`

Runs the student's tests against the correct implementation (all must pass) and
against broken versions, each with its mistake; each broken one must make at
least one test fail. The feedback names the first that slips through.

```yaml
check:
  type: tests
  file: testen/test_slug.py
  against: testen/slug.py
  mutants:
    - file: mutanten/slug_ohne_kleinbuchstaben.py
      say: Eine Fassung, die Großbuchstaben stehen lässt, besteht deine Tests.
    - file: mutanten/slug_doppelte_striche.py
      say: Bei zwei Leerzeichen hintereinander entstehen zwei Striche, und keiner deiner Tests merkt es.
```

In the activity the tests are a file and run in the Workspace like `python`;
in set 10 they are typed into the card and run in the executor.


## Order of work

1. **Executor and queue**, measured on the server.
2. **The card and `function`** for Ruby and Python: editor (extend
   `code_input.js`), storing, `cases`, `random`, `forbid`, `same_as`, levels,
   tiered hints, privacy, docs (aufgaben.md, aktivitaet-schreiben.md), tests.
3. **`extra: true`** for tasks: counting, card, class page, checkliste.
4. **Sets 1 to 3** in Ruby and Python, then **JavaScript** for the Katas.
5. **The overview page** and the catalog section.
6. **Einreichen** and the viewer without names.
7. **Pip, Teil 1**: engine, trace, commands, widget, pictures, story, five
   chapters. Then **Teil 2**.
8. **Sets 4 to 9**, as many as there is time for, Texte and Listen first.
9. **Testen** and the check type `tests`, then **set 10**.

Each step ends with the Ruby tests, `task_examples_test.rb`, a look at every new
card in light and dark and on a phone, and a CHANGELOG line where an operator
must know (a new image, a value in `credentials.rb`, what is stored).


## Next: the maintainer's wishes (26 October 2026)

Asked for while Teil 1 and 2 were looked at; not built yet unless marked.
The rules that came out of the same review are in
[checkliste.md](checkliste.md), »Katas und Pip«.

1. **Clicking before typing (Pip Teil 1, before the first plan)** (done).
   The first chapter, »Der Eingang«: buttons under the map (»Vorwärts«,
   »Links drehen«; »Nimm« exists) move Pip at once, and every click writes
   its command as a line into the plan beside the map, which can only be
   watched, not typed into (the maintainer: »only watch it fill«). A step
   into a wall or a take without a stone shows Pip's surprise and writes
   nothing; »Rückgängig« (not »Zurück«, which the player has for a step back) takes the last line back. When Pip reaches the lamp,
   the plan goes to the usual check by itself; there is no »Ausführen«. The
   browser moves Pip itself (`PipButtons` in pip_world.js), so such a map
   has only walls, stones and the lamp (`buttons:` in the check, PipCheck
   validates it). Pip starts facing down, so the first left turn goes to the
   right of the screen: »left is Pip's left« is felt before the prediction
   asks it. »Der Schacht« then says: from now on you write the plan.
2. **A chasing monster for a finale: Teil 3, »Der Mampfer«** (being built).
   The maintainer: the monster of the old Hackschule, which always found
   the shortest way; keep it, make it the way into breadth-first search.
   The sprite is that monster, 0x72's »big demon« from the 16x16
   DungeonTileset II (CC0, https://0x72.itch.io/dungeontileset-ii; credited
   anyway, as the old Impressum did), copied to `images/pip/catalog/` and
   built into `mampfer.png` by make_sprites.py, its pixels as large as
   Pip's. The rules (PipWorld):
   - After every step of Pip, a turn, `wait` and `take` included, the
     Mampfer goes one tile along a shortest way to Pip: Pip standing still
     is caught (the maintainer). `say` is no step.
   - The way is a breadth-first search from Pip, neighbours in the order
     east, north, west, south; the Mampfer steps to its tile's
     predecessor. That is the Handbuch's `breadth_first_search` exactly,
     so the widget and the students' own search are the same algorithm.
   - The lamps Pip lit in Teil 1 and 2 still burn (`*`): the Mampfer never
     steps into their light, the 3 × 3 tiles round them (tinted on the
     map), not even onto Pip standing there.
   - Pip on its tile or it on Pip's ends the run (»schnappt der Mampfer
     Pip«).
   - Two new questions, no step: `cave_map` (the map as it is now, a list of
     rows in the legend) and `facing` (»east«, »north«, »west«, »south«).
   Because turns cost a step, the shortest way in tiles is not always the
   cheapest; the generated maps of the last chapter are therefore checked
   by simulation: every shortest way escapes, counting its turns at worst
   (a right turn as three lefts), and the wall-following plans of Teil 2
   are caught.
   Chapters: (1) the Mampfer wakes: what it does, a prediction round a
   pillar, fleeing, the light as a refuge; (2) what the Mampfer knows: the
   breadth-first search on Pip's map as a widget (»Was der Mampfer
   sieht«), then function Katas on the map as a list of rows (find a
   tile, the free neighbours, the distance, the way); (3) the shortest way:
   Pip reads the map, searches and walks the way, on generated mazes with
   loops. In the catalogue after »Suchen«: it needs lists and
   dictionaries. Then there are 13 Kata pages.
3. **The front page instead of `/katas`** (done). One strip of the Kata
   sets (`carousel: true` in sections.yaml, `KataMap.carousel_html`), each
   with its picture or icon, idea, number of Katas and progress, beside it
   what there is or how far the person is (below), lower than one row of
   activity cards; a set pinned
   for the person's group is marked and scrolled into the middle. The page
   `/katas` is gone. A Kata without Pip has no picture: a solved card would
   only give a solution away.
4. **One Kata widget on the Kata pages**, instead of the general task
   cards: the task beside the code on a wide screen (like Pip's map beside
   the plan), one button (done for every Kata: »Ausführen« runs the program,
   shows what it prints, and checks), the level marks redrawn (done), and a
   UI/UX pass over all of it. Planned and shown to the maintainer before it
   is built.
5. **Solutions for teachers** (done): a small button »Lösung einsetzen«
   beside a Kata's buttons puts the solution into the editor
   (`kata_solutions_config`, tasks.js).

## Answers of the maintainer (26 October 2026)

1. The name is »Kata«.
2. The levels are called »Einstieg«, »Übung«, »Knobelei« and »Extra«.
3. No distinction between a class in a lesson and someone at home: one queue,
   first come, first served. Capacity problems are not expected.
4. Pip's texts and pictures: the session writes and draws them, from the
   sprite catalogue of `specht/2d` (substantial already), and shows them to the
   maintainer.

### Decided in the review of Pip and the sets (26 October 2026)

What the maintainer decided while looking at the pages, for a later session
to keep. The rules that follow from it, with what checks them, are in
[checkliste.md](checkliste.md), »Katas und Pip«; what is still to be built is
under »Next« above.

The cards and the strip
- One button per Kata, »Ausführen« with a play icon: it runs the code (a
  function Kata's own program first, when it has code of its own, so its
  output shows) and checks. Pip shows the result only after the replay.
- Map tabs name the call the check makes on that map; the map in the tab
  runs first and stays; the tabs wait while a run is under way.
- The level is a word with three bars, not dots.
- Teachers get »Lösung einsetzen« beside a Kata's buttons; the solution goes
  into the editor, never into the card's HTML.
- Visitors see a Kata's code in its editor, read-only, with colours and line
  numbers; a map in the page with its plan after it shows the plan beside it
  in the same editor.
- No page `/katas`: the front page has one strip of the sets, no taller
  than a row of cards, and a panel beside it (`KataMap.total_html`). No
  empty ring, which only says how much is missing: before anything is
  solved, and for visitors, the section's icon, how many Katas there are in
  how many Übungen and, for a student, »Loslegen« into the pinned set or
  the first. Once something is solved, a thin bar per set stacked in the
  strip's order, each in its set's hue and filled as far as solved (the
  maintainer, after colourful pies were too busy and one bar in segments
  too small); a bar names its set on hover and brings it into the middle
  on a click. Under them »3 Übungen geschafft · 1 angefangen«, the Katas
  solved and »Weitermachen« into the first set begun. A set opens with its
  button (»Loslegen«, »Weitermachen«, »Ansehen«), the strip is dragged with
  a mouse, swiped on a phone (a set snaps into the middle) and has arrows
  that fade out at its ends; it fades into the page where more follows. A
  set pinned for the person's group is marked and scrolled into the middle.
- Every set has an icon on a band in its own hue, no picture of a card,
  which would give a solution away. The hue comes from the set's place in
  the strip, equally spaced round the colour wheel from Pip's lantern amber,
  with no exception, not for Pip either (the maintainer; `KataMap.hue`), all
  equally light and strong: braces for Erste Funktionen, a signpost for
  Entscheidungen, a loop for Wiederholen, quotation marks for Texte, a
  numbered list for Listen, a book for Dictionaries, toggles for Bits, a
  tree for Rekursion, a magnifier for Suchen, a ticked box for Tests
  schreiben; Pip (from the side), Bello and the Mampfer for Pip's sets,
  drawn in grey and tinted in the band's hue (`images/pip/make_icons.py`,
  default.css), about as large as the other icons: Pip and Bello at two and
  a half times, every pixel of the two alike, the Mampfer, drawn larger, at
  1.6 times. Buttons and progress stay green, the bars striped. The
  section's icon is the martial arts uniform.
- A mouse drags the strip; thrown, it glides to the set it would come to
  rest at and stops there, with no snap afterwards. A click on a set calls
  out its button. Text on the sets is not selectable.
- On a page with a language switch the kind label does not name the
  language (»Übung«, not »Übung (Ruby)«); the chosen language's pill slides.

Pip
- The 3/4 view stays: the 2D Game Studio's figures, the surroundings drawn
  anew with dithering, light and shadow, mixed corners, side faces on left
  walls only, every figure in the middle of its field, and behind the rock
  in front of it. Spikes instead of a gate, long and uneven, animated.
- Several maps of a task differ, each shows what is expected (a corridor as
  long as `walk(n)` should go), and a prompt that has the check call a
  function says so and names the calls. Numbers in a prompt hold for every
  map a person can get.
- Turns and taking a stone play faster than a step; the speed holds for the
  page and the next one. Pip speaks sentence by sentence in Pixelify Sans.
- The story ends in itself, without a teaser for the 2D Game Studio.
- The first chapter is clicked, not typed (»Next«, 1). Teil 3 is the
  chasing monster, the Mampfer (»Next«, 2).

The sets
- FizzBuzz is introduced as an English counting game; »Brot« (divisible by 3
  or with a 3) is the game of German primary schools, an Extra.
- Set 9 is »Suchen« (see the catalogue); the merge step is set 5's, the
  sieve stays the Extra of set 3, counting comparisons is the Sorting
  Sandbox's. No Kata twice: what one set has, another leaves out.
- Prices and other sums in cent, not in euros with decimals, so that no
  result depends on the order of adding.
- Examples never imitate a difficulty a student may have (a »stutter«
  function became one putting letters in brackets).
- The curriculum line, shown to teachers in the strip, is wanted for every
  activity (TODO.md).


## Progress

**Step 1, executor and queue: done.** `src/ruby/include/kata_runner.rb` runs
one Kata in a container of its own from the Workspace image (named
`hs_code_kata_*`, so the Docker guard lets it through unchanged; no image of
its own: the interpreters are those of the Workspace), with no network, a
read-only image and a small `/tmp`, as `nobody`, limits on CPU, memory,
processes and output, `--init` and an alarm that ends the program even if the
website is gone. `kata_queue.rb`: one queue, `KATA_PARALLEL_RUNS` at a time
(default 4), one run per person. `src/scripts/measure-katas.rb` measures how
many pay off (docs/entwicklung.md, »Katas: Läufe im eigenen Container«).

**Step 2, the card and `function`: mostly done.** `kata_check.rb`,
`kata_page.rb`: the check reuses the harness of `python` and `ruby`; a request
gets a ticket and the card polls `/api/kata/poll` (no web server thread waits),
showing »Noch 2 vor dir«; »Ausführen« shows the output; the code is stored per
language as JSON in the task's answer, teachers see only the result;
`level:`; `starter:`; examples with `answer:`, the starter must fail. The page
`e2e/tests/katas.browser.spec.ts` uses the Kata pages themselves (a demo page `/kata-demo` was removed).
`forbid`, `random` (seed per person and task version, the reference computed
in the run, the message names the value) and `same_as` feedback (the wrong
solutions run in the same container) are done, and beginner errors get a
line of explanation under the original (`KataCheck::EXPLAINED`). The
solution after solving comes as for every task, in the card's language. A check may name
`slow:`, the message when the time is up where speed is the point. Still
open in step 2: a time limit per case, and the live console over the
WebSocket (the output now comes when the run ends).

**Step 3, `extra: true`: done.** Counts for nobody's »x von y« (start page,
class board, the card says »Extra«); extra chapters may hold extra tasks.

**Step 4, sets 1 to 3 and JavaScript: done, dev-only.** `kata-functions`,
`kata-decisions`, `kata-loops` in the catalog section »Katas« (entries with a
dot, so only development shows them until the maintainer has looked), each in
Ruby, Python and JavaScript, with card pictures from recipes. JavaScript is a
language of the Katas only: `CodeLanguages::GROUP_LANGUAGES` keeps the class
setting at Ruby and Python, and a choice of JavaScript is stored apart
(`u.kata_code_language`), so other pages keep Ruby or Python.
`JavascriptCheck` is the harness. Person boxes for Euklid, Fibonacci,
Collatz and Eratosthenes (set 3) are still to be written.

**Step 5, the overview: done, dev-only.** `/katas` (`src/content/katas`,
the first entry of the section) holds `<div class='kata-map'></div>`;
`KataMap` and `page_html` put the sets there as stops on a path, in the
order of `sections.yaml`, with `idea:` and the count per level from their
meta and tasks, the person's progress, and `curriculum:` for teachers. The
map draws only the sets that exist; Pip and sets 4 to 10 join it as they
come.

**Step 6, Einreichen: done.** Under a solved Kata »Einreichen« stores a copy
of the code in its language as the task's hand-in (`SubmissionStore.write_text`,
the same path as other hand-ins, so deleting the answer or the account and
the data export include it); »Eingereicht · Zurücknehmen« removes it. The
class board lists them per Kata as »Lösung 1, 2 …« in an order of their own
(`KataHandIns`), with the line what they are for, »Namen zeigen«, and a
viewer for the projector (`kata_hand_ins.js`).

**Step 7, Pip, Teil 1: done, dev-only, waiting for the maintainer's look at
the pictures.** `PipWorld` (`src/ruby/include/pip_world.rb`) is the world on
the server: the map, turn by turn, the Glibbers, the events, the trace.
`PipCheck` (check type `pip`, aufgaben.md) runs the plan with a prelude per
language in a Kata container; `KataRunner` got a dialog mode in which each
command line (`DIALOG_MARK`) is answered by the world on stdin. The card
draws the first map and plays a run back (`pip_world.js`, `PipPlayer`) with
the line in the editor marked (`codeInputMark`); a map in the page for a
prediction is `pip-static`. Story lines after a Kata wait until it is solved.
The page `pip-1` has five chapters, 13 Katas (two Extras) and a prediction,
in Ruby, Python and JavaScript; the Glibber maps were chosen by searching
all fixed plans with up to eight waits, none of which passes all three. The
pictures are made by `src/static/images/pip/make_sprites.py` from the 2D Game
Studio's catalogue (`catalog/`): Pip in four directions, Glibber, Bello,
stone, lamp, lever, gate, wall, floor, paw prints, puddle, Wichtel. Still
open: the tunnel map at the top of the page that lights up per chapter, and
`same_as`-style feedback by trace.

**Step 7, Pip, Teil 2: done, dev-only.** `pip-2`, six chapters, 16 Katas
(two Extras), in Ruby, Python and JavaScript. Changes from the plan, found
by simulating every map (the scripts searched for maps the intended plan
solves and the naive one does not): `on_lever` was added, since without it
Pip cannot know where a lever is on several maps; `trail_ahead` replaced
`on_trail`, since with »prints here« following a trail means stepping off
it and back, while »prints ahead« lets the hand on the wall follow it;
Bello's tile counts as trail, so the trail ends at him, and chapter 10 ends
with Bello found. Chapter 11 is a winding tunnel without branches instead of
a maze: the hand on the wall walks into every side passage, a Glibber's
shaft too, and meets the Glibber head-on; »straight on where possible«
crosses the shafts. The Extra of chapter 8 is an island: the left hand
circles the outer wall for ever, the right hand from the start reaches the
lamp. New in the engine: `call`/`expect` for functions of one's own (the
program is loaded with its commands ignored), `generate: maze` from the
seed (`PipMaps`), `dry: true`, goals `bello` and `say: stones`, the closed
gate as an event of its own; JavaScript plans run as a script
(`vm.runInThisContext`), so their functions are global. Karel the Robot
(Richard E. Pattis) is named in the teacher note; a person box waits for
his year of birth, which no source gives.

**Pip's pictures, the 3/4 view.** The map is seen from above at a slant,
as in tile games: the ground, the top of the rock with a light edge on each
open side and its front face above an open tile, about half the corners
round, what stands upright sorted by row and taller than its tile (the old
lamp on its post, the gate). Pip, Bello, the Glibber and the Wichtel stay
the figures of the 2D Game Studio, so that students know them; everything
around them is drawn by `src/static/images/pip/make_sprites.py` in one
palette, shading dithered. A light layer darkens the cave; Pip's lantern,
the Funkensteine, the lit lamp, glowing mushrooms and crystals shine. When
Pip lights the lamp (or finds Bello), it flares up, sparks fly, the cave
brightens and Pip hops. The maintainer looks at the pictures and may redraw.

