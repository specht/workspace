# UML-Werkstatt

UML diagrams as text (`.uml`), drawn beside the editor, as the VS Code
extension `hackschule-uml`. For now the only kind is the class diagram;
state and sequence diagrams may follow in the same extension.

In VS Code, a `.uml` file gets:

- highlighting, and problems underlined while typing (German messages with a
  hint what to write instead, also in »Probleme«); errors in red, warnings
  (a lowercase class name, an unknown type, an association name used twice)
  in yellow;
- »Klassendiagramm anzeigen« (button in the editor title, <kbd>Ctrl</kbd>+<kbd>K</kbd>
  <kbd>V</kbd>): the class diagram in a preview beside the editor, updated
  while typing. With errors it shows the last state without errors, faded,
  and lists the errors (a click goes to the line). Zoom with −, + and
  »Einpassen« or Ctrl + mouse wheel; typing never rescales the view. Light and
  dark theme follow VS Code;
- »Diagramm als SVG speichern«: `supermarkt.svg` next to `supermarkt.uml`, on
  white. An SVG the UML-Werkstatt did not write is only replaced after asking.

Build: `npm run package` runs the tests and writes `hackschule-uml-<version>.vsix`
with `pack.cjs` (no npm packages or network needed).

`core/uml-core.js` is one plain JavaScript module without dependencies and
without a DOM (a UMD module: `require` in node, `window.UmlCore` in the
preview). The extension and the Workspace checks both use it, so a diagram
means the same everywhere.

```
# Kassen im Supermarkt
class Customer
  - items: int
  - arrival: int
  + Customer(items: int, arrival: int)

class Checkout
  - freeAt: int
  + Checkout()
  + accepts(customer: Customer): boolean
  + serviceTime(customer: Customer): int
  + serve(customer: Customer): int

class ExpressCheckout extends Checkout
  + accepts(customer: Customer): boolean

class SelfCheckout extends Checkout
  + serviceTime(customer: Customer): int

class Supermarket
  + Supermarket(checkouts: List<Checkout>)
  + arrive(customer: Customer): int
  + averageWait(): double

association checkouts: Supermarket 1 -- 1..* Checkout
```

- `#` starts a comment, to the end of the line; blank lines do not matter.
- A class: `class Name`, then optionally `extends Superclass`, then
  optionally `(abstract)`. Names start with a letter; a class name in lower
  case is a warning.
- Members, indented under their class, one per line, each with its
  visibility `+` (public) or `-` (private). There is no `#` (protected): `#`
  starts a comment.
  - attribute: `- items: int`
  - method: `+ serve(customer: Customer): int`; the return type is required
    (`void` when nothing is returned)
  - constructor: named like its class, without return type: `+ Customer(items: int, arrival: int)`
- Types as in Java: `int`, `double`, `long`, `float`, `boolean`, `char`,
  `byte`, `short`, `String`, `Object`, the wrapper classes (`Integer` …),
  `void` (only as a return type), the classes of the file, generics
  (`List<Checkout>`, `ArrayList`, `LinkedList`, `Set`, `HashSet`, `TreeSet`,
  `Queue`, `Deque`, `ArrayDeque`, `Stack`, `Map<String, Checkout>`, `HashMap`,
  `TreeMap`) and arrays (`int[]`). Anything else is a warning asking whether
  a class is missing or the name is misspelled.
- Associations: `association name: A mult -- mult B`, or `->` when only A
  knows B (navigable from A to B):
  `association queue: Checkout 1 -> 0..* Customer`. Multiplicities: `1`, `*`, `0..1`, `0..*`,
  `1..*`, a number, a range `m..n` with m ≤ n.
- Errors: a class or a member declared twice (methods: same name and
  parameter types), `extends` an unknown class or a cycle of `extends`, an
  association to an unknown class, members outside a class, any line that
  matches nothing.

The diagram is a UML class diagram: name, attributes, methods in three
compartments (abstract: «abstract», name in italics); a hollow triangle at
the superclass; associations as lines with their name near the middle and
the multiplicities beside the line at the class they belong to, an open
arrowhead for `->`. Lines aim at the centres of the boxes and end on their
borders. Superclasses are above their subclasses; classes linked only by
associations stand beside the classes they are linked with, unless that
would draw a line through a box or make the picture far too wide.

Command line, used by the checks and for exports:

```bash
node core/uml-core.js --json supermarkt.uml    # classes, associations, problems
node core/uml-core.js --svg supermarkt.uml > supermarkt.svg
```

`--json` prints one object:

```json
{
  "version": "0.1.0",
  "errors":   [{ "line": 3, "from": 2, "to": 8, "msg": "…" }],
  "warnings": [{ "line": 5, "from": 0, "to": 4, "msg": "…" }],
  "classes": [
    { "name": "Customer", "abstract": false, "extends": null, "line": 2,
      "attributes": [{ "name": "items", "type": "int", "visibility": "-" },
                     { "name": "arrival", "type": "int", "visibility": "-" }],
      "methods": [{ "name": "Customer", "constructor": true, "returns": null, "visibility": "+",
                    "params": [{ "name": "items", "type": "int" }, { "name": "arrival", "type": "int" }] }] },
    { "name": "ExpressCheckout", "abstract": false, "extends": "Checkout", "line": 14, "attributes": [],
      "methods": [{ "name": "accepts", "constructor": false, "returns": "boolean", "visibility": "+",
                    "params": [{ "name": "customer", "type": "Customer" }] }] }
  ],
  "associations": [
    { "name": "checkouts", "from": "Supermarket", "to": "Checkout", "fromMult": "1", "toMult": "1..*", "directed": false, "line": 25 }
  ]
}
```

Lines are 1-based, columns (`from`, `to`) 0-based. `extends` is `null`
without a superclass; types are written as in the file with spaces
normalised (`Map<String, Checkout>`). A missing file gives
`{ "version": …, "missing": true, "errors": [{ "line": 0, "msg": "Die Datei … gibt es nicht." }] }`.
`--svg` exits with 1 and the errors on stderr when the file has errors.

Tests: `npm test` in this folder.
