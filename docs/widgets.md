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

- **It starts with what it is about.** A demo of `remove` starts with the
  finished tree, not with the eight inserts that build it: `data-setup` holds
  the operations that build the start without steps of their own,
  `data-ops` the ones the widget is about.
- **One step is one line of the pseudocode,** the pseudocode the page shows
  (in English, as on the page; comments in it may be German). Beside the
  picture a code box shows the function that runs, with the current line
  marked. A step that does not change the picture still gets its line when
  the line matters (`return node`).
- **The operations in their order** stand above the picture (`insert 50 ·
  insert 40 · …`): done ones faded, the current one marked, what comes
  next visible. The viewer always knows where they are.
- **The message under the picture** says in one or two German sentences what
  this step did and why, with the values: »40 hängt nur noch an der
  Variablen x.« Plain statements, no questions.
- **Variables are visible** when they matter: a label with the variable's
  English name (`root`, `node`, `x`, `y`, `successor`) and an arrow to what
  it references. `root` comes in from above the tree. A temporary variable
  appears when it is assigned and disappears when its function returns; the
  moment it holds the only reference to a node is said out loud.

## How it moves

The rule: what stays the same thing moves; nothing pops in or out when it
could move. A viewer should be able to follow every element from one step to
the next with their eyes.

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
  one. The owner's end never comes loose. One assignment per step.
- **Arrows end on the border** of their node (`data-inset`), so the
  arrowhead stays visible, and they **bend while they move**: a path whose
  middle trails behind its ends like a rubber band and straightens when the
  nodes rest.
- **A pulse runs down a pointer** (`data-pulse`) when it is followed into a
  recursive call, and on the way back up when each pointer is assigned the
  subtree its call returned (the lowest first).
- **A value on its way** is a token beside the node it is compared with, and
  it lands where it is inserted (`data-from`). A copied value flies from the
  node it is copied from.
- **The focus ring** marks the node the running call works on, the
  parameter `node`, and the label `node` is always beside it. It moves from
  node to node, and while it fades in or out it stays on its node, even when
  that node moves (`data-follow`).
- **The calls that run** stand above the code box, each one deeper indented
  (`remove(node: 50, value: 20)`), the current one marked. On the way back
  each shows what it returns (`→ 30`) and they return one after another, the
  deepest first, in time with the pulses on the pointers.
- **Layout keeps the structure readable at every step.** In a tree a child
  is always below every node that points to it; the column is the place of
  the value in the sorted order, which rotations keep, so nodes move up and
  down but not sideways. A node that only a variable still holds stands as a
  small tree of its own beside the node being worked on.
- **Nothing covers anything.** Labels, tokens and rings keep at least about
  20 px from nodes and from each other; labels of the smaller and the larger
  value go on opposite sides, `successor` below its node.

## Timing and controls

- A step forward or back moves (550 ms, `MOTION_TIME`); a jump (first, last)
  shows the step at once. A click during a movement finishes it first.
- `prefers-reduced-motion` shows every step at once. `?motion=slow` in the
  address makes every movement six times slower, to look at it closely.
- Four buttons with `data-go` (first, prev, next, last) and ← → through
  `stepper_keys.js` (`data-stepper` on the widget). On a narrow screen the
  buttons share the whole width and the step count goes on its own line
  (`default.css`).
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
