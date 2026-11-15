# Routenplaner in five languages

Plan of 15 November 2026, decided with the maintainer: the Routenplaner
(`src/content/routenplaner`, starter `specht/routenplaner-starter`) keeps one
server, in Ruby, which students never edit, and the graph functions can be
written in Ruby, Python, Dart, JavaScript or Java: the languages of the data
structures sandbox, so that a group that built its structures there in Dart
(or JavaScript, Java, Python) keeps its language and its semantics. The
sandbox classes themselves do not carry over unchanged (they store only
`int` values through the sandbox's memory objects, the array queue holds
eight); what carries over is the language. First used by an LK on
24 November 2026.

## How it works

- `./run [language]` starts `server.rb` (as `./run` starts the sandbox).
  Without a language: the one whose folder was changed last. `AREA=…` as
  before.
- One folder per language in the starter: `ruby/routes.rb`,
  `python/routes.py`, `dart/routes.dart`, `javascript/routes.js`,
  `java/Routes.java`, each with its bridge (`bridge.rb`, `bridge.py`,
  `bridge.dart`, `bridge.js`, `Bridge.java`), and for Dart and Java the
  types the functions use (`graph.dart`, `Types.java`: Point, Way, Graph,
  Edge, Search). Java needs its own folder anyway: its check compiles every
  `.java` file in the folder.
- The server starts the bridge as a child process and talks to it over a
  TCP socket on 127.0.0.1 (port in `ROUTES_PORT`), one JSON object per line;
  stdout and stderr of the bridge go to the terminal, so a student's `print`
  is seen and never breaks the protocol.
- Before each request, and once a second, the server compares the times of
  the files in the language's folder; after a change it restarts the bridge
  (Java: `javac` first). Compile errors and crashes appear in the terminal
  and on the page instead of a route.
- Requests: `graph` (mode `length`, `foot`, `bike`, `car`: the bridge reads
  the ways once, builds and keeps the graph, sends it to the server, which
  needs it for clicks, the graph view and the cost of a path), `search`
  (`breadth_first`, `dijkstra`, `a_star` by mode, start and goal; the bridge
  measures the time of the search itself, so the comparison stays fair),
  `call` (the tour functions with their arguments). Answers carry `missing`
  (a function not written yet: Ruby and Python `NotImplementedError`, Dart
  `UnimplementedError`, JavaScript `notYet(name)`, Java
  `UnsupportedOperationException`) or `error`.
- Watching the search unfold on the map is the point of the activity (the
  maintainer): the browser plays a search back from `visited` (the nodes in
  the order of the search) and `edges` (which the server derives from it),
  from yellow to red, at the chosen speed. So every bridge returns `path`
  and `visited` exactly as the Ruby functions do, and the answer of
  `/route` is the same in every language; the e2e test checks it per
  language.
- Names: Ruby and Python `read_ways`, `build_graph`, `breadth_first`, …;
  Dart, JavaScript and Java `readWays`, `buildGraph`, `breadthFirst`,
  `aStar`, `buildTravelGraph`, `tourLength`, `bestTour`, `nearestTour`,
  `improveTour`. The server speaks the Ruby names; each bridge translates.
- `server.py` goes; Python runs through its bridge like the others.

## Steps

1. Starter: `run`, `server.rb` with the bridge, the five bridges and
   starters; tested with the solutions in all five languages.
2. Page and tasks: the cards up to Dijkstra (fusswege, lesen, grad, abstand,
   graph, weg, breitensuche, kanten, dijkstra) with `dart`, `javascript` and
   `java` variants (check types exist), solutions and typical mistakes in
   `loesung/`, examples, `<!-- language: … -->` blocks, »Einrichten« with
   `./run`, the teacher note, docs/dart-java.md.
3. A run in a real Workspace with all five languages, e2e.
4. Later: heap, Dijkstra with heap, A*, travel graphs and tours in the
   three new languages.

The starter is pushed (directly, as allowed for it) only together with the
page's deploy, so that nobody clones a starter the page does not describe.
