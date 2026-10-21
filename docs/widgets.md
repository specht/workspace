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
  the Handbuch pages (`rotate_right(y)`, `node.left`, `if … :`, `return`,
  `empty`); a remark in parentheses may be German. On the pages it is a
  ```` ```pseudocode ```` block (`pseudocode_lexer.rb`), and the code box
  colours the same tokens the same way (`highlightPseudocode` in
  `algo_stepper.js`): dark, like every other code block on the site.
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
  Variablen x.« Plain statements, no questions.
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
  nothing (text, rings); a variable's box is there at once (`data-solid`).
  What goes into something else (the token into the node it becomes) does
  not fade either: it moves along under it and is gone at the end
  (`data-merge`), so two half-transparent shapes never lie on each other.

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
  start of its arrow.
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
  narrow, paler and set apart to their left, so that a finger meant for next
  does not jump to the end; the step count on the left, on a phone on a line
  of its own (`default.css`).
- **A lightbox for a demo**: every widget has a square button at its top
  right (`bi-fullscreen`, `stepper_keys.js`) that opens it larger in a
  lightbox on a black background, for the big screen. The keys and the
  sideways swipe step it as before; the arrows at the bottom right, Page Up
  and Page Down, or a swipe up and down go to the previous or next widget of
  the page; Esc, the button again or the browser's back button close it.
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
