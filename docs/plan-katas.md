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
   └──────── 8 Rekursion ──── 9 Suchen und Sortieren ──── 10 Tests schreiben
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

**4 · Texte**
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

**5 · Listen**
Going through a list, building a new one, positions (Index, not »Stelle«).
* Einstieg: sum (CB array_sum), max (CB array_max), count the even numbers,
  contains
* Übung: reverse (CB array_reverse), swap first and last (CB), rotate left
  (CB), sorted? (HS Monotonie)
* Knobelei: remove duplicates, the missing number (CB find_missing_number),
  merge two sorted lists
* Extras: two numbers that add up to a target (two-sum) in one pass
* Oberstufe: Datenstrukturen; links to the Data Structures Sandbox

**6 · Dictionaries**
Keys and values: counting, grouping, looking up.
* Einstieg: look up a phone number, count words in a list
* Übung: word frequency, most frequent word, group words by length
* Knobelei: invert a dictionary, the average per subject from a list of grades
* Extras: a tiny LRU cache
* Oberstufe: Datenstrukturen (Hash Table); links to the Handbuch page
  `hash-tables`

**7 · Bits und Zahlensysteme**
Numbers as bits, other bases.
* Einstieg: binary representation (HS Binärdarstellung), from binary back
* Übung: count set bits (HS), hexadecimal, is a power of two
* Knobelei: two's complement of a byte, any base to any base
* Extras: a number in Roman numerals
* Sek I: Information und Daten (Zahlensysteme); links to `bits-und-bytes`

**8 · Rekursion**
A function that calls itself, with a case that stops.
* Einstieg: factorial again, recursively; sum of a list
* Übung: power, Fibonacci recursively, palindrome recursively
* Knobelei: Towers of Hanoi (the moves as a list), flatten a nested list
* Extras: Fibonacci with memoisation under a time limit
* Oberstufe: Algorithmen (rekursive Verfahren)

**9 · Suchen und Sortieren**
The helpers of the algorithms; the full sorting algorithms stay in the Sorting
Sandbox, which the page links.
* Einstieg: linear search (the index or `nil`), minimum's index
* Übung: binary search, insert into a sorted list, is a list a permutation
* Knobelei: the sieve of Eratosthenes, merge step of merge sort
* Extras: count comparisons of two search methods on the same list
* Oberstufe: Algorithmen, Effizienz

**10 · Tests schreiben**
Writing tests instead of code, with the check type `tests` (section 4): for each
Kata a function with a description, the student writes test cases, broken
versions must fail them.
* Einstieg: tests for `even`, for `max_of_two`
* Übung: tests for `slug`, for `leap_year` (the century rule)
* Knobelei: tests for binary search (empty list, one element, not there)
* Extras: find the bug in a given function by writing the test that shows it
* Oberstufe: Softwareentwicklung (Testen); leads to the activity »Testen«

Not in the catalogue: what needs files or the network (CSV, JSON, a URL
shortener) belongs in Workspace activities. The quine (HS) is an Extra of set 4
once a whole-program mode exists (run the program, compare its output with its
source).


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

Title, level, prompt with examples, a code editor with a starter, **Ausführen**
(runs once, shows the console) and **Prüfen** (runs the cases), then console and
feedback. The language switch of the page chooses Ruby, Python or JavaScript
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


## Answers of the maintainer (26 October 2026)

1. The name is »Kata«.
2. The levels are called »Einstieg«, »Übung«, »Knobelei« and »Extra«.
3. No distinction between a class in a lesson and someone at home: one queue,
   first come, first served. Capacity problems are not expected.
4. Pip's texts and pictures: the session writes and draws them, from the
   sprite catalogue of `specht/2d` (substantial already), and shows them to the
   maintainer.


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
`/kata-demo` (dev only) has two Katas; `e2e/tests/katas.browser.spec.ts`.
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

