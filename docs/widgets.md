# Interactive widgets

How the step-by-step widgets on Handbuch pages and activities look and
behave: the Algo Stepper (`src/static/include/algo_stepper.js`: trees, heap,
sorting, lists, hashing, graph searches), the Query Stepper, the Huffman and
LZW steppers and the like. A widget is there so that students *see* what an
algorithm does, step by step, in the order the code does it. It is not
decoration and not a quiz.

The movement between steps comes from `src/static/include/stepper_motion.js`
(loaded on every page). The AVL tree on `reference/baeume` is the model: when
in doubt, do it the way it does.

## What a widget shows

- **A widget never solves a task.** It shows other examples than the
  activity's tasks (the DEA and NEA of the automata page work on `a` and `b`,
  the tasks on digits), and where its input can be edited, typing a task's
  input must not give the task's answer.
- **Presets instead of free input** (Algo Stepper, `data-presets='gemischt:
  5, 2, 8, 1 | schon sortiert: 1, 2, 5, 8'`, for `data-values` where a
  widget has values and no operations (the sorts, Heapsort), `data-ops`
  otherwise, or the attribute `data-preset-of` names; a preset of the form
  `start=A goal=D` sets several attributes, as the graph searches do): a few
  inputs to choose above the buttons, so that a student can try what the
  page asks (Quick Sort on a sorted list, the AVL tree on sorted values,
  only even keys in a hash table). The author picks them; none may be the
  input of a task. Choosing one sets the widget up anew (`setUp`, the old
  listeners and timers go with `algoTeardown`).

- **A title says what can be seen** (`data-title`), above the buttons, short
  and not too specific: »Suchbaum: einfügen«, »AVL-Baum: entfernen«,
  »Heap: pop«, »Merge Sort«.
- **One kind of operation per widget**: insert in one, find in another
  (with `data-setup` for the tree it starts with), not both in one.
- **It starts with what it is about.** A demo of `remove` starts with the
  finished tree, not with the eight inserts that build it: `data-setup` holds
  the operations that build the start without steps of their own,
  `data-ops` the ones the widget is about.
- **One step is one line of the pseudocode,** the pseudocode the page shows.
  Beside the picture a code box shows the function that runs, with the
  current line marked. A step that does not change the picture still gets its
  line when the line matters (`return node`).
- **Short variable names** (`min`, not `smallest`): they fit in a label and
  are read at a glance.
- **Pseudocode is English, its comments German.** Names and keywords as on
  the Handbuch pages (`rotate_right(y)`, `node.left`, `if … :`, `return`);
  a remark in parentheses may be German. `←` assigns, `==` and `!=` compare,
  `<`, `≤`, `>`, `≥` order; a single `=` never appears, so that nobody
  confuses assignment and comparison; `null` is a reference to nothing (`node == null`,
  `return null`, in the picture too), `empty` is a collection without
  entries (`queue not empty`, `empty set`); words rather than symbols where
  a word is clear (`and`, `or`, `not`, `mod`). `remove_at(i)` takes out
  what stands at a position, `remove` is only for taking out a value
  (`remove(node, value)` in a tree, `remove(k)` in a hash table). The code box and the pages
  hold the same lines: a change goes into both. On the pages it is a
  ```` ```pseudocode ```` block (`pseudocode_lexer.rb`), and the code box
  colours the same tokens the same way (`highlightPseudocode` in
  `algo_stepper.js`): dark, like every other code block on the site.
- **The buttons are in the same order everywhere**: »Zurück« and »Weiter«
  together at the right edge, »Weiter« filled (the step taken most); to the
  start and to the end quieter, set apart to the left of them; play and its
  speed on the left, then the step count. On a desktop at the size of a
  mouse, the pairs joined, back and next with their words
  (`.stepper-word`, from `stepper_keys.js`); on a touch screen large for
  the thumb (default.css, `pointer: coarse`). A button that cannot be used
  is grey.
- **What is current is purple in every widget**: the code line, the table
  row, the coefficient, the clause, the next byte (`--stepper-current` in
  default.css, the Algo Stepper's `--algo-accent`). Red is for an error or
  a goal, never for »this one«. Code in a widget's message is set like the
  text around it on a faint purple ground, not in the pink of code in the
  page text, which would read like an error.
- **A variable is drawn in the picture.** An index or pointer is a label
  with an arrow to its cell; a number that points at no cell is a box with
  its name and value (`valueBox`: `capacity | 6`, `count | 1`), beside what
  it describes; `size` shows what it counts (the border after the used
  cells in the Array List, a bracket under the queue's cells in the ring
  buffer). Never a line of plain text like »size 4, capacity 6«.
- **A small legend under the picture** says what its colours mean, only
  those that occur in this widget's steps and in its words (»in der
  Queue«, »Pivot«, »wird entfernt«; `legendOf` in algo_stepper.js): gold
  is waiting, compared or new, violet done, red the goal, the pivot or what
  is removed, green the start.
- **The timeline names its parts** so that they can be told apart: the
  operations with their values (»push 5«, »pop → 1«, »dequeue → 23«), a
  sort's passes by its loop variable (»i = 0«, »end = 7«), Merge Sort's and
  Quick Sort's calls by their range.
- **The frame is a card like a task's**, in the widgets' own slate (tasks
  are blue, the Handbuch teal, a guess violet; a calm frame lets the
  widget's own colours speak), its label a pill like a task's number.
- **A timeline** under the buttons: a dot for every step, joined by a fine
  line (coloured as far as the steps are done); the dot where an
  operation starts is ringed and carries the operation's name (`remove 20`,
  `remove 40`, `remove 50`), starting at its dot. Where names would run into
  each other, the earlier ones go up a row and get a fine line down to their
  dot; no name stands in the way of such a line. The parts of an operation
  stand below the dots, smaller and quieter, each under a fine bracket over
  its steps, so that they read as part of the job above: `R 50` and `L 40`
  for a rotation, `sift up` and `sift down` in the heap, `successor` in
  remove, `merge 0–3` and `partition 0–7` in sorting (the `phase` of a
  frame). Each step has a minimum width (24 px on a touch screen, so the dots have room); where the
  timeline gets wider than the widget it scrolls sideways and keeps the
  current step in the middle by itself. The dots are evenly
  spaced; done dots coloured, the current one large. A tap or a drag
  on it goes to that step. The viewer always knows where they are and where
  each operation starts.
- **The message under the picture** says in one or two German sentences what
  this step did and why, with the values: »40 hängt nur noch an der
  Variablen x.« Plain statements, no questions. Where the picture changes
  its height from step to step (Huffman's forest with its table at the end,
  the calculator's tree that shrinks to its result), the message stands
  above it and the picture comes last: every part but the last keeps the
  height of its tallest step (stepper_keys.js), so the last one may grow
  and shrink without moving anything in the widget and without leaving a
  gap on a phone.
- **The code beside the picture only where it has room.** The Algo Stepper
  puts the calls and the code beside the picture while the widget is at
  least 56em wide (`NARROW_EM`): the picture's 34em, the gap and about 20em
  for the code, so that its lines do not break inside words. Narrower (a
  tablet upright, the Handbuch in half a screen beside VS Code, a phone),
  the widget gets `algo-narrow`: one column, the message right under the
  picture and its legend, the calls and the code last, so that picture and
  message are seen together. It is measured on the widget (ResizeObserver),
  not the window. Then every part keeps the height of its tallest step,
  the code too (stepper_keys.js counts the parts in the order they are
  seen), and the lines from the calls to their nodes are not drawn.
- **Variables are visible** when they matter: a label with the variable's
  English name (`root`, `node`, `x`, `y`, `successor`; not »Wurzel«) and an
  arrow to what it references. `root` comes in from above the tree. A temporary variable
  appears when it is assigned and disappears when its function returns; the
  moment it holds the only reference to a node is said out loud.

## How it moves

The rule: what stays the same thing moves; nothing pops in or out when it
could move. A viewer should be able to follow every element from one step to
the next with their eyes. And every movement means something: nothing moves
only to look nice.

- **What covers something does not fade.** A node or a token that appears
  over lines grows in, opaque, from nothing to its size (`data-grow`: a
  circle by its radius, a group by its scale), and shrinks away when it goes;
  a fade would let the lines behind it show through. The arrows that end on
  it follow its current size (`data-inset='auto'`). Fading is for what covers
  nothing (text, rings). A variable's box and its name are there at once
  (`data-solid`); when they go, they stay opaque until the arrow has shrunk
  back into the box, then fade together, so the arrowhead never shows through.
  What goes into something else (the token into the node it becomes) does
  not fade either: it moves along under it and is gone at the end
  (`data-merge`), so two half-transparent shapes never lie on each other.

- **A step back is the step forward played backwards.** Going back one step
  replays the movement that led there in reverse (`StepperMotion.morph(…,
  { reverse: true })`): a pulse runs back along its arrow, what came out of
  something goes back into it, what grew shrinks, a two-part step (Huffman)
  takes its parts in reverse order. Widgets without `stepper_motion.js` do
  the same by hand (GIF: the pixels of the code undone shrink away, its
  table entry lights up and goes). A jump shows the step at once.
- **Which label belongs to which arrow can be read without help**, as in a
  printed book: labels stand where they are least ambiguous, and where
  some still are the rows get more room. On top of that, pointing at an
  arrow (on a wide invisible line, `.automaton-pick`) or at its label marks
  both (`AutomatonLayout.hover`, elements of one arrow share `data-edge`):
  purple, the arrow thicker, the label bold and underlined, so that it
  does not depend on colour. Only a moving pointer marks.
- **Automatron** has the step widget too: »Ein Wort durchlaufen« under each
  drawing (ε-NEA with ε steps, DEA, smallest DEA), a word below runs through
  the smallest DEA; the expression is in the address (`?regex=…`), the
  back button goes back to the empty start. Widgets built after the page
  has loaded send `stepper-added` (stepper_keys.js).
- **A ring travels along the arrow it takes** (`data-along='edge-key'` on
  the ring, timed with the pulse on that arrow): its centre runs from the
  state's centre along the arrow into the next state's centre, a fifth
  smaller on its way; a ring that stays goes once around its loop.
- **Identity by key.** Every element that lives on between steps has a
  `data-key`: a node by an id, not by its value, wherever a value can be in
  two places (remove copies a value into another node). Elements without a
  key are redrawn at once.
- **Pointers are arrows that belong to their node** (`data-ends='owner
  target'`, keyed by owner and field, `edge-<id>-left`). An arrow that is
  assigned sprouts from its owner, its tip follows the target and snaps on
  near the end of the movement (the line is briefly thicker); one that is set
  to empty lets go with a jerk and shrinks back into its owner; one that is
  reassigned lets go of the old target, swings across and snaps onto the new
  one. The owner's end never comes loose. One assignment per step. In a step
  that assigns a pointer, the pointer goes first: it sprouts (or swings over)
  and snaps onto its target where the target stands, and only then does the
  target move into its new place, the pointer staying on it (two phases, the
  step 1.7 times as long).
- **Arrow tips never touch a ring**: an arrow to a node that wears a ring
  ends outside the ring (`data-ring` on the ring, `stepper_motion.js`), at
  rest as well as while it moves.
- **Edge labels keep clear of every edge**: the automaton places each label
  where it keeps 4 units from all arrows, states and rings, and nearer to
  its own arrow than to any other, so that it is read as its.
- **Arrows end on the border** of their node (`data-inset`), so the
  arrowhead stays visible (a curve that would come in backwards is drawn
  straight for that moment, so the head never turns over), and they **bend
  while they move**: a path whose
  middle trails behind its ends like a rubber band and straightens when the
  nodes rest. Only as far as its two ends move differently: when the whole
  picture shifts to make room, that is a change of view, and the arrows stay
  straight.
- **A pulse runs down a pointer** (`data-pulse`): the arrow itself swells a
  little and the bulge travels from the owner up to the arrowhead (not into
  it), like a peristaltic wave, in the arrow's colour; no glow, no dot. It runs on the way back up of a
  recursive function, when the pointer is assigned the subtree its call
  returned (`node.left ← remove(node.left, value)`): one pointer after
  another, the lowest first, each with a pause before the next (650 ms each,
  `PULSE_TIME`), the arrow of `root` last. A pointer that changes swings to
  its new target first and gets its pulse after. On the way down there is no
  pulse: the ring moving along the edge already shows the call.
- **An arrow whose label travels with its node** (`node`, `successor`,
  `data-rigid`) moves along rigidly instead of letting go and snapping on;
  letting go and snapping on are for real reassignments, where the owner
  stays.
- **A value on its way** is a token beside the node it is compared with, and
  it lands where it is inserted (`data-from`). A copied value flies from the
  node it is copied from.
- **The focus ring** is drawn in front of the nodes. It marks the node the running call works on, the
  parameter `node`, and the label `node` is always beside it. It moves from
  node to node, and while it fades in or out it stays on its node, even when
  that node moves (`data-follow`).
- **Nothing jumps, not even while sliding along the timeline**: every part of
  a widget keeps the height of its tallest step (`stepper_keys.js` goes
  through all steps once when the page has loaded and sets `min-height` in
  em; again when the width changes or a text in the widget is edited).
- **Nothing jumps.** The call stack keeps the height of its deepest moment
  over the whole timeline, and the start (before any operation) already shows
  the code of the first one, no line marked: the code box stays where it is
  in every step.
- **The way the stack went down is marked in the picture**: the pointers from
  `root` through every waiting call's node to the current one are purple,
  their dashes flowing in the direction of the calls, so it is always clear
  along which way the stack will come back up (`on-stack`; without the
  flowing under reduced motion). On the way back each of these pointers
  settles once its pulse has passed: solid and grey again, the lowest first.
- **The calls that run** stand above the code box, each one deeper indented
  (`remove(node: 50, value: 20)`), the current one marked. On the way back
  each shows what it returns (`→ 30`) and they return one after another, the
  deepest first, in time with the pulses on the pointers. A thin line joins
  each call to the node it holds (strong for the current one, dashed and
  faint for those that wait), behind the picture and the code: so it is
  visible how many nodes are held at once. The lines end without a dot. The
  current call's line ends at the ring, which glides from node to node, and
  the value on its way (the dashed token, the call's argument) is placed on
  that line just before the node; on a phone, without lines, it stands beside
  the node. Only when the code stands beside
  the picture; on a phone, where it stands below, there are no lines.
- **Layout keeps the structure readable at every step.** In a tree a child
  is always below every node that points to it; the column is the place of
  the value in the sorted order, which rotations keep, so nodes move up and
  down but not sideways. A node that only a variable still holds stands as a
  small tree of its own beside the node being worked on.
- **The range of a recursive call is in the picture** (merge sort, quick
  sort): a light band behind the bars of the current call's range, and above
  the bars a bracket for every call on the stack, the outer ones grey, the
  current one purple; they move with the calls, so the recursion is seen
  going down into the halves and coming back.
- **Entries that swap** (heap, sorting) glide to each other's place on a
  small arc (`data-arc`), so that they pass each other instead of
  overlapping, in every view at once (the heap's tree and array).
- **Index variables** are boxes under the places they hold, one box each.
  The boxes of one place stand side by side, and their arrows aim at one
  point inside the column near its bottom, stopping before the index number,
  so the index stays readable and the tips end apart. Groups go into rows so
  that no box overlaps another and none stands in an arrow's way (narrow
  groups first). A new box is there at once (`data-solid`), so it hides the
  start of its arrow. An arrow keeps about 24 px between box and tip, so it
  reads as an arrow and not as a lone arrowhead.
- **A count is a border, not a place.** `size` of an Array List is the line
  in the gap after the last used cell, labelled with its value
  (`size | 2`, a box with the value beside the name); right of it the cells and values are faint. The faint part is
  a copy clipped to the area right of the line, so a cell turns solid exactly
  where the moving line passes over it, never all at once.
- **null is one word in one place: where the arrow would end.** A
  variable that holds null (`head`, `root`, `node`) has the word right after
  its label, the same gap everywhere (`nullBeside`); a field drawn as a box
  (a node's `next`) has it inside; a tree's child pointer, which has no
  box, is a short stub from its node ending in the word, and so is the
  start's predecessor in a graph search. Shown where the code looks at it:
  in a tree only at the place a call reached (`find`, `remove` of a value
  that is not there), not under every leaf.
- **Graph searches** (Breitensuche, Tiefensuche, Dijkstra, A*): the ring is
  `node`, `neighbor` a label with an arrow, the edge looked along drawn
  purple; `predecessor[neighbor] ← node` is an arrow from the neighbor to
  node that sprouts and snaps on (a shorter way swings it over), the start's
  predecessor a stub to `null`. The queue, the stack or the priority queue is
  a row of tokens under the graph: a token comes out of its node and goes
  back into it when it is taken; an old entry is struck through. A number a
  variable holds is a box with its value beside the name (`new | 58`), no `=`.
  `path_back` runs step by step only in the first widget of the page; the
  others show at once what it returns. The timeline is named after the node
  that comes out.
- **Huffman** (`huffman.md`, no code box: the students write it): the trees
  stand on one line at the bottom, so a new node always appears above the
  two it joins; it grows in with its weight first, its edges sprout and snap
  onto the two roots, where they stand; only then does the forest
  close up (two movements in one step, `done` in `morph`). The trees stand
  in the order their leaves have in the finished tree, so the two that come
  together are always neighbours and no tree jumps sides (`data-lift` would
  let a tree rise over others, where passing cannot be avoided). The two
  lightest trees wear the purple ring, the counts clear of it. Only the
  roots' weights count: the numbers under a node that has been joined go
  pale while the sum appears.
- **The calculator** (`calculator.md`): an operator is a round box, a number
  a box; when a node is computed, its numbers rise into it and merge, and
  the round box turns into the result's box; back, they come out of it
  again. The node that comes next wears the purple ring.
- **A short arrow does not bend**: the rubber band of a moving arrow grows
  with its length (none under 40 px), so the head of an index arrow never
  tilts. A label that comes or goes follows its node (`data-follow` on a
  text), and what merges into an element keeps its own key.
- **Subset construction and minimisation** (Algo Stepper, `data-algo='subset'`
  and `'minimize'`, the automaton from Automatron with `data-regex` or written
  out like a drawing): the page's pseudocode beside the picture, one step a
  line. Subset construction: the NEA on top (`set` solid rings, `next`
  dashed, the arrows taken pulse, the ε arrows after them), the DEA below,
  growing, each state already where it is at the end with its set under it,
  a table beside that fills cell by cell. Minimisation: the DEA with rings on
  the pair being checked, dashed on the pair a symbol leads to, the table of
  pairs with its marks and their reasons; at the end the states that become
  one share a tint.
- **Binary search** (Algo Stepper, `data-algo='bsearch'`, `data-values` and
  `data-ops='find_position 34'`): `find_position` from `reference/listen`,
  `left` and `right` as index arrows (`right` may point behind the last
  cell), the range still in question on a band, the cells outside it faint.
- **Traversal** (`data-ops='inorder'` on a search tree): the calls go down
  with the ring and the stack as in `remove`, a null child gets its
  `if node == null: done`, and each visited node flies out of the tree into
  the row »besucht« under it.
- **Tombstones** (`data-mode='probe'`, `remove k`): the bucket keeps a dashed
  box with the word `deleted`; `find` and `insert` count it as occupied.
- **Rehashing** (`data-mode='chain' data-grow`, `data-size='4'`): `insert`
  with the fill check from `reference/hash-tables`. `old ← buckets` puts the
  label `old` above the same buckets; at `buckets ← m empty buckets` the
  keys go into a column under `old` on the right, and the loop takes them
  out one by one, each flying to its new bucket (the token is the key
  itself, `key-k`). The calls show the `insert(s)` the loop makes. The fill
  note is left out while count and m do not belong together.
- **Equal values in a sort** carry a letter in their order at the start
  (3a, 3b, 3c, a small subscript; a copy keeps the letter of what it was
  copied from), and the last step says whether they kept that order:
  stable or not. Not on Selection Sort (`data-no-letters`): a task of the
  Sorting Sandbox asks which of its lines makes it unstable.
- **Round trip** (`data-algo='tour'`, `data-mode='nearest'` or `'2opt'`,
  `data-points='x y; x y; …'`): `nearest_neighbor` and `two_opt` from
  `reference/graphen` on places in the plane (y up), the distance the
  straight line. The nearest neighbour shows dashed lines from `last` to
  every remaining place with their lengths; 2-opt starts from its order and
  shows, for each `i`, `j`, the two ways a reversal takes out (dashed red)
  and the two it puts in (dashed yellow), the sums with two decimals so
  that they add up as written. Under the places the order as cells that
  move when a piece is reversed. The `for` lines of 2-opt get no steps of
  their own: `i` and `j` stand under the order and in the message.
- **Heapsort** (`data-algo='heap' data-mode='sort'`, `data-values`):
  `heap_sort` and `sift_down` from `reference/heap`, a heap in which no child
  is greater than its parent. The heap's picture with the calls beside the
  code; `size` is a border under the array, and what lies behind it has left
  the heap: out of the tree, in the array in the colour of »fertig
  sortiert«. A leaf's two missing children are one step, not two. The
  timeline names each round by what goes to the end (»9 ans Ende«).
- **Bit planes** (`bitplane_widget.js`, steganography): a slider with two
  handles over the bits 7 to 0, the bits between them kept, all others 0;
  »verstärken« stretches the rest to 0 to 255; one pixel's red value in
  binary under the picture. The photo (CC0, credited under it) is a PNG, so
  that every bit is the photo's own, and not one of the activity's photos.
- **What grows comes first**: in a step that also sprouts a pointer, an
  element that grows in does so at the start (`early` in
  `stepper_motion.js`), so that its pointers sprout from something.
- **Automata** (`automaton_stepper.js`, data attributes for states, start,
  accepting states, transitions, the word): the word as a tape with a head
  over it, the automaton drawn by `automaton_layout.js` like every other
  automaton on the site (the drawings in pages, `<div
  class='automaton-drawing'>`, and Automatron): the states in columns by
  their distance from the start, every arrow bent to its left (forward
  above, back below), arcs within a row nested as in an arc diagram, arcs
  within a column beside it, loops on top; the gaps grow with the longest
  label; ends of arrows to different states keep the order of those states,
  so that neighbouring arrows do not cross; every arrow is checked as it is
  drawn and bends further where it would pass a state; several orders inside
  the columns are tried and the drawing with the fewest crossings, no arrows
  on top of each other and no label on an arrow is kept (Thompson's NEA also
  tries columns by the longest way along its numbering: one row, skips as
  arcs above, star loops below); a dense automaton (more than 50
  states, or more arrows than 45 and 1.4 per state) is its transition table
  instead (Automatron's DEA for ten digits); the ring for the state (a NEA: a ring for every state it can be
  in, a new one coming out of the state it is reached from), the arrow
  taken purple with a pulse; at the end the accepting state green or the
  verdict »abgelehnt«. The word can be edited. One font, the mono font of
  the other widgets: state names in its italics with a small index
  (q<sub>0</sub>, names as in mathematics), the symbols upright as on the
  tape; in the message q<sub>0</sub> as in the page's text.
  The same widget shows a **pushdown automaton** (`data-kind='pushdown'`,
  rules `state read top push next`, `ε` for pushing nothing): the word as a
  tape, the stack as a column of cells right of the drawing, `#` at the
  bottom and the top framed purple, the variables `state`, `symbol`, `top`
  and `push`; and a **Turing machine** (`data-kind='turing'`, rules `state
  read write move next`): the tape from its first to its last cell used in
  the whole run, one empty cell more on either side, the head as the purple
  frame, the cell just written purple, the variables `state`, `read`,
  `write` and `move`; it stops in a state of `data-accept` (the tape is the
  result) or where no rule fits (`data-result='accept'`: accepted or
  rejected). A label of several lines has one line per rule, and only the
  line used in the step is purple. `data-fixed` hides the input, for the
  widget at the top of »Kellerautomaten« and »Turingmaschinen«, whose word
  a task must not come from. The engine (`machine`, `trace`) runs without a
  page: `test/static/automaton_stepper.test.cjs`.
- **Circuits** (`logic_circuit.js`, »Vom Gatter zum Rechenwerk«): no step
  buttons, the reader switches the inputs (a click, a tap, Enter or the
  space bar; on a narrow screen also as buttons of 44 px under the drawing,
  and the labels grow up to 1.3). Gates as in DIN EN 60617 (rectangles with
  `&`, `≥1`, `=1`, `1`, a circle where the output is negated), as German
  school books draw them; a wire with a 1 in Cling amber (`#c98500`, dark
  `#f0a000`, the yellow of /lehrplan), with a 0 grey and thinner; buses
  (four bits on one line) thick, their value written over them; control
  lines dashed; a dot where a wire branches, none where wires only cross.
  A change runs round by round (every gate computes from the values before,
  all at once, 110 ms a round), so that a carry is seen running through the
  adder; the RS flip-flop holds its bit by the same rounds, and one that
  does not come to rest within 64 rounds says so after 12 shown. Beside a
  circuit without memory its truth table, the current row purple; a row
  sets the inputs. The circuits are a library in the script, laid out by
  hand; the engine runs without a page (`test/static/logic_circuit.test.cjs`,
  among others every ALU result against the flags of the CPU Sandbox).
- **The circuit editor** (`circuit_editor.js`, »Logik-Werkstatt«): the same
  drawing and the same engine as the circuits above (gates from
  `logic_circuit.js`, its `settle` round by round, 110 ms a round), but the
  students build. A palette of the six gates (in a task only those its check
  allows), switches and lamps in the free editor, and the blocks a person
  has saved; a click puts a part on a free place (switches on the left,
  lamps right of the rest, gates column by column), dragging puts it where
  it is dropped. Parts move on an 8 px grid and snap so that a wire becomes
  straight; wires run at right angles from an output pin (right) to an
  input pin (left), back to the left around below both parts, a dot where
  one branches; an open input is an empty ring. A wire is dragged, or on a
  touch screen tapped pin after pin. Keyboard: Tab from part to part, arrows
  move, the space bar switches, Entf deletes, V and a digit draw a wire,
  Strg+Z and Strg+Y undo and redo (100 steps). Beside it the circuit's own
  truth table up to five switches (a row sets them), a note instead when a
  wire feeds back. While building the drawing has its own size and the
  canvas scrolls; to look at (a phone, `<div class='circuit-view'
  data-netlist='…'>` in a page) it is as wide as there is room for, and on a
  phone the editor only switches and says that building needs a larger
  screen. A circuit is a netlist (`LogicCircuit.Netlist`: parse, format,
  flatten, one line a part, `block NAME … end` for a block); flatten replaces
  every block by its gates (ids `h1/g1`), so the engine runs it unchanged,
  feedback through blocks included. »Als Baustein speichern« keeps the
  circuit as a block, its switches its inputs and its lamps its outputs from
  top to bottom; placing a block copies it from the library into the
  circuit with the blocks inside it, so a circuit always carries what it
  uses. Drafts and the library stay in the browser (localStorage, per
  account like the tips); a checked circuit is the task's answer on the
  server and brings its blocks back on another computer. The netlists of
  `src/ruby/test/fixtures/circuits.json` run the same in the page
  (`test/static/logic_circuit.test.cjs`) and on the server
  (`circuit_check_test.rb`); the editor itself has
  `e2e/tests/logic-workshop.browser.spec.ts`.
- **What grows, grows from its middle** (`data-grow='22 21'` on a group
  drawn from its corner), and ends on its own transform, so that its next
  move slides instead of jumping.
- **Text grows where the picture is small** (a phone): the Algo Stepper
  measures how far its picture is scaled down and lets every text grow by
  up to 1.35 (a variable's box with its name and value as one, about its
  centre, keys `var-…`), so that labels stay readable; a turned phone draws
  the step again.
- **The same thing has the same size** across related widgets: a list's
  cells are as large in the widget that grows to 8 cells as in the one with
  4 (the picture at most as wide as drawn, `data-natural`).
- **Nothing covers anything.** Labels, tokens and rings keep at least about
  20 px from nodes and from each other; labels of the smaller and the larger
  value go on opposite sides, `successor` below its node.

## Why (decisions of the maintainer, October 2026)

- Children below their parents at every step, never a child drawn above its
  parent: a tree is read top down, and a picture that breaks that confuses
  more than it shows. The nodes move up and down with the pointers instead.
- One pointer assignment per step, and temporary variables shown: the point
  of a rotation is that the order of the assignments matters and that a node
  is held only by a variable for a moment.
- Sprouting from the owner and snapping onto the target, letting go and
  shrinking back: a pointer is a field of its node, and the picture should
  say who owns it and when it is assigned.
- Pulses on the way back only, one after another with a pause: on the way
  down the ring already moves along the edge, a pulse on top made the edge
  busy; on the way back each assignment should be seen on its own.
- The ring is the parameter `node`: it never appears without its label.
- A demo starts with what it shows: nobody should click through eight inserts
  to see a remove.
- The call stack beside the code: a recursive function goes down and comes
  back up, and the viewer should always know on which level they are.

## Timing and controls

- A step forward or back moves (550 ms, `MOTION_TIME`; a step with pulses one
  after another takes longer, `frame.duration`); a jump (first, last) shows
  the step at once. A click during a movement finishes it first.
- `prefers-reduced-motion` shows every step at once. `?motion=slow` in the
  address makes every movement six times slower, to look at it closely.
- Four buttons with `data-go` (first, prev, next, last) and ← → through
  `stepper_keys.js` (`data-stepper` on the widget); on a touch screen a
  sideways swipe across the picture steps it too (not on the timeline,
  `data-no-swipe`). On a touch screen (tablet, phone) the controls sit on
  the right, where the thumb is: back and next large and finger-sized
  (44 px) at the edge, next on the very edge; to the start and to the end
  narrow, outlined instead of filled and set apart to their left, so that a
  finger meant for next does not jump to the end (outlined, not paler: pale
  reads as disabled); the step count on the left, on a phone on a line
  of its own (`default.css`). Where the first picture is the input before
  anything has happened (the Algo Stepper, the Automatron, Huffman, the
  calculator), it is »Schritt 0«, and the count says how many steps the
  algorithm takes; where the first picture is already a step (the Query
  Stepper's `FROM`, LZW's first byte, the iDCT's first basis function), the
  count starts at 1.
- **Play** (`data-play`, beside the step buttons): one step after another
  by itself, each movement and then a pause to read the message
  (`READ_TIME`); the speed button beside it cycles 1×, 2× and ½× and changes
  both. A step by hand (button, key, swipe, timeline) stops it; play at the
  end starts again from the beginning. On a touch screen play and its speed
  sit beside the step count. Every other widget with step buttons (Huffman,
  LZW, GIF, JPEG, the calculator, the Query Stepper, the iDCT) gets the same
  play from `stepper_keys.js`, which presses next until it is disabled, and
  the Algo Stepper's timeline (dots, the done ones purple, tap or drag to a
  step) where the widget keeps `data-step` and `data-steps` and offers
  `stepperGo(step)` (Huffman, LZW, GIF, the calculator, the iDCT).
  Pip's player in a Kata card (`pip_world.js`) has these controls and the
  timeline too, with its own play (a step takes 450 ms at 1×, a plan has
  many), and no frame, label or lightbox of its own: the task card is the
  frame. The current line of the plan is marked purple in the editor, its
  number brighter.
- **A lightbox for a demo**: every widget has a square button at its top
  right (`bi-fullscreen`, `stepper_keys.js`) that opens it larger in a
  lightbox on a black background, for the big screen. The keys and the
  sideways swipe step it as before; the arrows at the bottom right, Page Up
  and Page Down, or a swipe up and down go to the previous or next widget of
  the page; Esc, the button again or the browser's back button close it.
  Everything fits without scrolling, as large as the window allows (up to
  32 px), the code nearly as large as the text: the font size is found by
  halving (`stepper_keys.js`, `fit`) for three layouts, the code beside the
  picture 24em or 32em wide or under it, and the layout with the larger text
  wins (a tablet upright puts the code under the picture). The picture gives
  up height only while its labels stay at least 70 % of the text size;
  beside the code it keeps at least 60 % of its size across the whole width.
  The size is measured when the picture rests, at the first, the last and
  the current step (`stepperGo`), so that a widget that grows (the LZW
  table) has one size from the start; it is kept from step to step and
  only made smaller when a step does not fit, so the text does not jump; a
  new window size or another widget fits afresh. The content stands in the
  middle of the window, for the classroom: moved down by half the room its
  tallest step leaves, so it does not move up and down from step to step.
  On a window too small for that even at 12 px (a phone), the lightbox
  scrolls instead (`stepper-scroll`): the text at 15 px, the picture across
  the whole width and at most 85 % of the window high, the code beside it
  only where the picture keeps its labels readable (as above), otherwise
  everything in one column with the step message between picture and code.
  The close button and the arrows to the other widgets stand in a strip of
  their own below the lightbox (upright) or to its right (sideways), so they
  cover nothing.
- Colours come from the widget's CSS variables, so light and dark mode both
  work; no fixed colours in the drawing.

## How it is built

- **The engine is apart from the drawing.** A trace function computes every
  frame up front from the operations, as plain data (the nodes as they are in
  memory, the marks, the code line, the message). A draw function renders one
  frame into a detached SVG, and `StepperMotion.morph(visible, drawn)` turns
  the visible picture into it. The trace never touches the DOM.
- **Checks.** The engine gets node tests in `src/static-tests/` where it can
  be tested on its own; the motion layer's interpolation has
  `stepper_motion.test.cjs`. The Playwright test
  `e2e/tests/algo-stepper-motion.browser.spec.ts` steps through the tree
  widgets forward and back and measures, several times during each movement,
  that every arrow starts on its owner and ends on a node's border once the
  movement is over.
- **Caching.** In development every file the page template loads gets a
  fresh address per request, so a changed widget script shows after a
  reload; a page's own scripts get one through `main.rb`.
