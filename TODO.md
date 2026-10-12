# Workspace TODO

Where the Hackschule Workspace is going, how it gets there, and the open
maintenance work. Rewritten in October 2026 after a review of all teaching
material; the review document has the evidence for the findings below.


## Direction

The Workspace supports a teacher who is in the room. The teacher explains,
talks with students and decides what happens next. The Workspace takes over the
work that does not need a person:

1. **Setup** – nothing to install, the same environment at school and at home.
2. **Making the invisible visible** – one instrument per concept (Sorting,
   Data Structures and CPU sandboxes, the ER workshop, later a query stepper).
3. **Telling right from wrong** – checks that judge behaviour, not form, so any
   correct solution passes.
4. **Showing the teacher the room** – who is done, who is stuck, on which task.

Principles:

* Everything runs in the browser and continues at home.
* Everything is generated reproducibly: screenshots, diagrams, datasets.
* Checks accept every correct solution (as the Data Structures sandbox does with
  hash functions); a check that rejects a valid answer is a bug.
* Shared data across topics: the same movies and music appear in ER modelling,
  SQL, Neo4j and small applications.


### Content model: prose, reference, tasks

Today every page is called a tutorial and mixes three different kinds of content.
Split them, so each can do its own job:

| Kind | What it is | Who uses it how | Examples today |
| --- | --- | --- | --- |
| **Prose** | the story of an activity: why, what we build, what to notice | read with the teacher, in order | most of BIF, Anaglyph, PixelRAM |
| **Reference** | facts to look up: syntax, commands, options, fonts | searched while working, in any order | shower.js "Referenz", LaTeX fonts, SQL building blocks table, the language gallery |
| **Tasks** | something to do, with a check | done by the student, seen by the teacher | `hint task` boxes, Code Bites, sandbox checks |

* [ ] **Pages become activities.** Prose that embeds tasks and links reference
  entries. Decide the name shown to students (candidates: *Projekt* for pages that
  end with a product, *Labor* for pages that explore a phenomenon with an
  instrument, *Grundlagen* for skills like files, Git and first SQL).
* [ ] **Reference as its own layer.** One searchable reference area; entries are
  small Markdown files (`reference/sql/join.md`, `reference/latex/fonts.md`) that
  pages link with a macro and that open beside the editor. Move the long reference
  parts out of shower.js and LaTeX first.
* [ ] **Tasks as files.** A task is a small file with a statement, optional hints
  and a check, included in a page with a macro (`_task(sql-terra-07)`), shown as an
  interactive block when the student is logged in (they usually are), and as a plain
  task box otherwise. Existing `hint task` boxes are the first candidates.
* [ ] **Check types**, smallest first:
  * answer: a number or short text compared with the expected answer
  * SQL: a query run against a fixture database, result set compared (ordered only
    if the task asks for `ORDER BY`), plus a shuffled second fixture so hard-coded ids fail
  * code: the Code Bites judge (Python, JavaScript, Ruby)
  * workspace state: a file, a commit, a table or a running server exists, as the
    keyboard tutorial already detects student actions
  * reflection: free text without a check, collected for the teacher
* [ ] **One progress model** for page tasks, Code Bites and the sandboxes' Check,
  so the teacher view can show all of them.
* [ ] **Page metadata** in the existing `<div class='meta'>` YAML: `duration`,
  `level`, `requires`, `next`; shown on catalog cards and as a generated
  "Wie geht es weiter?" box at the end of a page.
* [ ] **Teacher notes per page** in the Lehrmaterial area: goals, time, typical
  mistakes, solutions to the page's tasks.


## Roadmap

A writing and development order, not a prerequisite chain for students. Each phase
unblocks the next.


### Phase 1 – Datasets

Both collectors now write the same files through `src/scripts/lib/classroom_dataset.rb`:
`mysql.sql`, `mysql-schema.sql`, `mysql-data.sql`, `mysql-data-<part>.sql`,
`neo4j.dump`, plus JSON Lines and a generated README.

* [x] Remove duplicate credits in `collect-movies` (IMDb lists a person once per character; `actress` → `actor` adds more)
* [x] Refuse to write a table with a duplicate primary key (the old `videothek-data.sql` stopped on import with ERROR 1062)
* [x] Readable string literals instead of hex, hex only for strings with backslashes or control characters
* [x] Schema-only, data-only and per-part data dumps (`movies` then `crew`; `albums`, `tracks`, `members`)
* [x] Prefer the German title IMDb displays in Germany; drop impossible death years (Homer: born 850, died 800)
* [x] Print a sanity summary after every build
* [ ] Rebuild the movie dataset with `collect-movies.rb` and publish it to `specht/videothek`; review the diff first, do not auto-commit
* [ ] Decide where the generated music dataset lives (a `specht/musikdatenbank` repository next to `specht/videothek`?) and publish it
* [ ] A student command `load-dataset movies|music [--schema|--data|--part NAME] [--neo4j]` that loads into the student's database (or a named extra database)
* [ ] Refresh both datasets periodically rather than letting them go stale


### Phase 2 – Database sequence

> query an existing database → model → query related tables → the same data as a graph → SQL safely from code

Movies and music run in parallel from the modelling chapter on: half the class can
work with Die Ärzte while the other half works with Nosferatu – same skills,
different answers, and students choose what they care about. Bobby Tables stays a
small, isolated security lab with its own throwaway database. Students can create
several MySQL databases but have one Neo4j database, so Neo4j uses reset/reload.

#### SQL introduction (`/mysql`)

Done in August 2026: single quotes, `COUNT`/`LIKE`/`DISTINCT` before the exercises,
the population-density claim removed, the chapter stops before `JOIN`.

* [ ] Shorten the opening further (folder, terminal) and link the Workspace basics
* [ ] Turn the 9 exercises into checked SQL tasks
* [ ] Fix the alt texts of the OR and COUNT…WHERE screenshots; the OR example shows `SELECT name`, its recipe runs `SELECT *`
* [ ] Delete the leftover `mysql/mysql.new.md.txt`

#### ER workshop (prototype: "ER-Werkstatt")

A text DSL for ER models; the Chen diagram, a crow's-foot diagram (conceptual or as
tables), the relational schema and MySQL DDL are generated, with every translation
step explained. Keywords are strictly English:

```text
entity Film
  id: int, key
  titel: text
  bewertung: decimal(3,1), optional

entity Exemplar
  nummer: int, key

relationship hat: Film 1:n Exemplar
relationship leiht: Kunde n:m Exemplar
  ausleihe: datetime, key
  rückgabe: datetime, optional
```

* [ ] Bring it into the Workspace as an `.er` file type with a live preview pane (like the BIF and Pixelflow previews)
* [ ] `_include_er(model.er, chen|crowsfoot|tables)` for pages, so ER diagrams are generated and never go stale
* [ ] Min–max cardinalities (`Film (1,1) : (0,n) Exemplar`) → `NULL`/`NOT NULL` and crow's-foot circle and bar
* [ ] Requirement checks against the generated schema: "a film can have several genres" (two inserts succeed), "a loan needs an existing copy" (insert fails)
* [ ] Later, if the curriculum needs them: weak entities, generalisation

#### Modelling chapter (new, short)

* [ ] From a short story to an ER model in the DSL; Chen and crow's foot
* [ ] Translation rules for 1:1, 1:n and n:m; relationship attributes
* [ ] One flat table with update, insert and delete anomalies as the reason for splitting tables

#### Filmdatenbank / Musikdatenbank (draft: `_drafts/filmdatenbank.md`)

Keep from the draft: start from a real JSON record, one `JOIN … ON …` per relationship,
`GROUP BY` with real questions, the actor-pair queries that motivate Neo4j.

* [x] Use the generated dumps (`mysql-data-movies.sql`, `mysql-data-crew.sql`); fix the clone recipe's dialog text
* [ ] Create the schema from the student's ER model instead of hand-drawn ERD Editor tables and manual screenshots
* [ ] Add the 1:n case: `Film 1:n Exemplar`, `Kunde leiht Exemplar` with loan and return date → relationship attributes, `NULL`, `LEFT JOIN` ("which copies are out?", "which films were never rented?")
* [ ] Integrity experiments: insert a `movie_genre` row for a missing film, delete a genre still in use, then `ON DELETE`
* [ ] The first Nosferatu queries filter by title only and mix the 1922 and 2024 films: make it the lesson "a title is not a key"
* [ ] Music twin of the chapter on the Discogs dataset
* [ ] Bridge to Neo4j: the same films, people and genres, relationships as first-class graph relationships
* [ ] Replace `/videothek` and retire its Ruby/TTY importer and crashing app; keep any application example small and focused on parameterized queries

#### Neo4j (draft: `_drafts/neo4j-filmdatenbank.md`)

Teach why graph databases exist, not only Cypher syntax.

* [x] Load `neo4j.dump` from the dataset (file renamed in both collectors)
* [ ] Start with a tiny manually created movie graph: nodes, labels, properties, relationships, `MATCH`
* [ ] Reset/reload as the documented clean-start workflow
* [ ] SQL `JOIN` and Cypher side by side; graph modelling choices instead of converting every table to a node
* [ ] Paths, variable-length paths, a shortest-path exercise on actor/movie connections
* [ ] Aggregation only after traversal is understood
* [ ] Show `cypher-shell` briefly; query from Ruby with the `neo4j_bolt` wrapper (decide whether it belongs in the student image)
* [ ] The Discogs music graph (`MEMBER_OF`, `RELEASED`, `HAS_TRACK`) as the second dataset

#### SQL Injection (`/bobby_tables`)

* [ ] Own throwaway database, no hard-coded `db_1234` in `login.rb`
* [ ] Add the missing `gem install mysql2 tty-prompt` (not in the student image)
* [ ] Keep attack → observe → prepared statement → retry; mark the tasks as tasks (`hint books` today)
* [ ] Say that the plain-text passwords exist only for the lab; keep the legal boundary short
* [ ] Spelling: "heisst", "Schluß"


### Phase 3 – Checked tasks

* [ ] Task file format and `_task()` macro (see Content model)
* [ ] SQL executor for Code Bites: fixture database + expected rows; MySQL in a throwaway schema keeps checks consistent with the pages (SQLite accepts `GROUP BY` queries MySQL rejects with `ONLY_FULL_GROUP_BY`)
* [ ] First users: the SQL introduction, the Filmdatenbank, SQL Injection
* [ ] Workspace-state checks for setup steps (repository cloned, server running)
* [ ] Link the matching Code Bites tasks from each programming-language page


### Phase 4 – Teacher view

* [ ] One live view per class across page tasks, Code Bites and sandboxes: who is done, who is stuck where
* [ ] Teacher notes with solutions for every page, starting with the database chapter
* [ ] Show `duration`, `level`, `requires` on catalog cards


### Phase 5 – New instruments and pages

Prefer pages that introduce genuinely new ideas rather than more installed technology.
Prioritise creative, visible material that also works with younger students; the order
below is a writing priority, not a prerequisite chain.

1. raylib + WebAssembly
2. Hugo / static site generators
3. Flutter
4. Web server
5. WebSockets
6. Cryptography

Plus, from the review:

* [ ] **Query stepper / database sandbox**: FROM → JOIN → WHERE → GROUP BY → HAVING → SELECT → ORDER BY → LIMIT with the intermediate table after each step; class repository and Class progress like the other sandboxes
* [ ] **JPEG decoder project**: the interactive Huffman section is excellent, but iDCT, MCU and colour conversion get one paragraph each and there is nothing to implement; a staged decoder with a check per stage


#### 1. Web server

Continue directly from the TCP/IP page and write the server in **Ruby** using raw TCP sockets.

> TCP → HTTP → browser → web server

* [ ] Connect explicitly to the TCP/IP page and port 1234
* [ ] Begin with the smallest `TCPServer` that accepts a connection
* [ ] Inspect a browser/`curl` HTTP request as plain text
* [ ] Send a minimal valid HTTP response manually
* [ ] Handle `GET /`, parse the request line into method/path/version, add routes, return `404 Not Found`
* [ ] Serve HTML and a small set of static files
* [ ] Read query parameters; optionally handle one small `POST` request
* [ ] Finish by sharing the running server through Shared Live Apps
* [ ] Take over the useful part of the Hades page (HTTP POST, JSON, a session header) against a practice API inside the Workspace

Do not introduce Sinatra/Rack/Node/Flask here. The point is to discover that a web server is initially just a TCP server that speaks HTTP.

#### 2. WebSockets

Ruby server + browser JavaScript client, so the networking progression stays continuous.

* [ ] Show why repeated HTTP request/response is awkward for a real-time application
* [ ] Inspect the upgrade request and `101 Switching Protocols` at a useful level, without a full protocol implementation
* [ ] Messages browser → server and server → browser; several clients; broadcast state changes
* [ ] Build a genuinely collaborative project rather than stopping at chat; share via Shared Live Apps

Preferred project: **shared pixel canvas**. Alternatives: multiplayer Pong, collaborative cursors, classroom voting, a tiny multiplayer world. If manual frame parsing becomes distracting, provide a tiny framing helper after the handshake is understood.

#### 3. Cryptography in the terminal

A laboratory around four questions:

> Has this data changed? → hashes
>
> Can somebody else read this data? → encryption
>
> Who signed/created this data? → digital signatures
>
> How can a server recognize me without receiving my password? → public-key authentication

* [ ] `sha256sum`, change one byte, compare copies; what a hash can and cannot prove
* [ ] Why plain SHA-256 is not password storage; salts and dedicated password hashing conceptually
* [ ] Encrypt/decrypt a file symmetrically with OpenSSL; try the wrong password
* [ ] Generate and inspect an Ed25519 key pair; sign, verify, modify, fail
* [ ] Distinguish secrecy (encryption) from authenticity/integrity (signature)
* [ ] Connect public keys to SSH and the Git material; end with a key fingerprint (back to hashes)

Before finalizing signing, test which tool gives the cleanest classroom workflow (`ssh-keygen -Y`, OpenSSL or GPG). Keep mathematics and cipher internals optional.

#### 4. Flutter

> Develop in the browser, then turn the same project into a real installable app.

* [ ] Smallest project with web **and Android** targets; hot reload; widgets and layout; button presses
* [ ] A Dart list rendered as widgets; changing it without rebuilding motivates state; `StatefulWidget` and `setState`
* [ ] A small ranking/favorites app: text field, sorting and filtering, one custom widget, maybe one implicit animation
* [ ] Share the browser version through Shared Live Apps
* [ ] End with `flutter build apk --debug` and where to find the APK; install on a phone

```text
HTML/JavaScript: find an element and change it.
Flutter:         change the state and rebuild the interface.
```

Android build support, before broad rollout:

* [ ] Measure memory and PID usage of `flutter build apk --debug` with the resource profiler
* [ ] Re-check student container limits against the Android/Gradle workload
* [ ] Check whether the pinned CMake/NDK components are needed and remove them if not

APK rather than App Bundle for the introduction; no release signing or Play Store accounts (mention `flutter build appbundle` as a follow-up). iOS: the same source can target iOS, but builds need macOS, Xcode and Apple signing; mention a Mac or a cloud build service such as Codemagic; keep App Store details optional.

#### 5. raylib + WebAssembly

The higher-level C graphics/game route; PixelRAM stays the low-level framebuffer route.

```text
PixelRAM: you get pixels and implement drawing/rendering ideas yourself.
raylib:   you get shapes, input, textures/sound primitives and build the game.
```

Preferred project: **Breakout**, asset-free if possible.

* [ ] Window + one primitive; the game loop `input → update → draw → repeat`; movement per second with `GetFrameTime()`
* [ ] Keyboard and mouse input, paddle, ball with a `Vector2` velocity, wall/paddle collision
* [ ] A grid of bricks, score, win/lose/restart
* [ ] Only then: the Emscripten build behind `make`, `C → .wasm + browser glue`, why the browser owns the event loop
* [ ] Hide backend-specific Emscripten details in the Makefile; publish the web build on a Hackschule subdomain

Do not turn PixelRAM into a second raylib by adding drawing primitives for convenience.

#### 6. Hugo / static site generators

> One HTML page is easy to maintain by hand. What happens when 20 or 50 pages all need the same navigation, header and footer?

* [ ] Begin with the duplication problem; a minimal site without a downloaded theme
* [ ] First page in Markdown, `hugo server`, minimal front matter (`title`, `date`, `draft`)
* [ ] A shared base layout with `.Title` and `.Content`; several pages; a list/home template that discovers new content
* [ ] Only the Go-template subset students need; their own CSS; tags as one example of generated navigation
* [ ] Source vs output: run `hugo`, inspect `public/`, publish it to the student's webspace

No third-party themes, Hugo Modules, multilingual sites, custom output formats, complex shortcodes or deployment pipelines in the introduction.


## Existing pages: findings from the October 2026 review

| Page | Action |
| --- | --- |
| Pixelflow drawing, Fire | add tasks (own scene; blue flame, wind direction) |
| shower.js | split into a short activity and a reference section |
| LaTeX | rewrite around writing one short paper from scratch (sections, environments, references, tables, a formula, BibTeX); today it compiles templates; typos "compilierung", "Bereicht", `litatur.bib` |
| Svelte | fix markup/editorial issues, clean up event-listener teardown, remove the draft outline comment at the end |
| Hades | only useful for one school's Greek app and needs a real e-mail login; script uses `agr.nhcham.org`, the text links `agr.gymnasiumsteglitz.de`; move its ideas into the web-server page and retire it |
| Ziffernerkennung | collect handwriting in the browser instead of print + 300 dpi scan; predict-then-run tasks; discuss overfitting (training loss 10⁻⁴); a shared preinstalled environment instead of 500 MB download and 2 GB per student; typos "boostrap", "Kenrel", "beboachten", "des die" |
| TCP/IP | rework as the first half of TCP → HTTP → web server: layers, ports, handshake; the Peru server (14.5 ms) is presented as far away next to Google (5.3 ms); remove the commented-out SSH block; broken `<code>-s</code an`; typos "einigt sich", "gar keinen Fall", "Anschließen", "in diesem Beispiele", "Umso kleiner … desto" |
| Git/GitHub | mark low-level internals as optional/advanced; correct the implication that `git mv` is required for rename detection |
| Files and directories | remove the stray `</td></tr>` |
| Programming languages | fact-check history, creator/date and lineage claims; one convention for the year in headings |

Platform-wide:

* [ ] A macro for the repeated opening (close folder, clone, open) with generated screenshots; 37 pages repeat it by hand
* [ ] Convert manual screenshots to recipes when a page is touched (about 800 manual images, 45 generated)
* [ ] E2E coverage for the database sequence, Git and TCP/IP (8 pages are covered today)
* [ ] Review `_template_codebites.html` language metadata and remaining manually authored homepage image alt attributes
* [ ] On narrow screens, consider left-aligning long paragraphs instead of global justified text
* [ ] A small `prefers-reduced-motion` pass for Workspace-owned transitions and animations
* [ ] Mention Shared Live Apps in BIF for peer-testing unfinished stories, and wherever sharing a running app helps
* [ ] Revisit BIF after the teacher workshop and incorporate classroom feedback
* [ ] Link Automatron from formal-language material and teacher notes; add a short introduction if students use it alone


### Homepage organisation

Reorganize `sections.yaml` by conceptual area as new pages land, together with the
page kinds from the content model:

1. Einführung – About → Basics
2. Grafik & Spiele – Pixelflow Canvas → Anaglyph 3D → Fire → PixelRAM → raylib + WebAssembly
3. Texte & Präsentationen – BIF → shower.js → LaTeX
4. Websites & Apps – HTML/CSS → Hugo → Svelte → Flutter
5. Terminal, Netzwerk & Sicherheit – Files → Git → TCP/IP → Web Server → WebSockets → Cryptography
6. Datenbanken – SQL → Modellieren → Filmdatenbank/Musikdatenbank → Neo4j → SQL Injection
7. Experimente – Digit Recognition → JPEG
8. Programmiersprachen – keep the gallery

Keep Einführung first. Keep related progressions adjacent, but do not present the
homepage as a mandatory course; pages link their prerequisites and follow-ups.


## Infrastructure and operations

### Operational logging

* [ ] Audit command/error logging so secrets and credentials cannot appear in logs
* [ ] Prefer safe human-readable operation descriptions over dumping complete commands when commands fail

### Dependency and image verification

Improve supply-chain reproducibility gradually rather than rewriting the Docker build at once.

* [ ] Checksum verification for important downloaded release archives where upstream checksums exist
* [ ] Consider digest pinning for important base images

### Continuous integration

Keep tutorial tests focused on whether the documented student workflow actually works.

* [ ] A small, fast CI gate for cheap checks
* [ ] Heavier browser/toolchain tests manually, nightly or on a suitable self-hosted runner
* [ ] E2E coverage when important pages are changed or newly written

### Deployment visibility

* [ ] Show the deployed short Git commit SHA in the admin area

### Ruby cleanup

Continue extracting coherent pieces from `main.rb` incrementally.

* [ ] Remove helper/setup definitions duplicated in `helper.rb` and `main.rb`
* [ ] Decide whether the remaining login/session routes move into a dedicated module
* [ ] Workspace lifecycle / nginx management as a future extraction candidate


## Later ideas

* **Prolog** – facts, rules, queries, pattern matching, backtracking, recursive rules, a small knowledge base, a logic puzzle
* **Digital audio** – oscillators, frequency and amplitude, envelopes, mixing, waveform/spectrum views, a tiny synthesizer or drum machine (headphones needed)
* **WebGPU / parallel computing** – a minimal WGSL compute shader, CPU vs GPU, particles, cellular automata, image processing or fractals
* **Git collaboration** – branches, merge, conflicts, pull requests, working with another student, basic CI
* **Data formats** – text vs binary, CSV, JSON, encodings, hex dumps; connects JPEG and network protocols
* **PixelRAM projects** – plasma, Game of Life, Mandelbrot, raycaster, raytracer, a software rasterizer; do not turn PixelRAM into a second raylib
