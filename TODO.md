# Workspace TODO

Open infrastructure, maintenance and teaching work for the Hackschule Workspace.


## Infrastructure and Hardening

### Operational logging

* [ ] Audit command/error logging so secrets and credentials cannot accidentally appear in logs
* [ ] Prefer safe human-readable operation descriptions over dumping complete commands when commands fail


### Dependency and image verification

Improve supply-chain reproducibility gradually rather than trying to rewrite the complete Docker build at once.

* [ ] Add checksum verification for important downloaded release archives where upstream checksums are available
* [ ] Consider digest pinning for important base images


## Testing and Operations

### Continuous integration

The Playwright suite now covers a growing number of real student workflows. Keep tutorial tests focused on whether the documented student workflow actually works, and add automated execution without making every push run the most expensive tests.

* [ ] Add a small, fast CI gate for cheap checks
* [ ] Run heavier browser/toolchain tests manually, nightly or on a suitable self-hosted runner
* [ ] Add E2E coverage when important tutorials are changed or newly written


### Deployment visibility

* [ ] Show the deployed short Git commit SHA somewhere in the admin area

This should make it immediately obvious which revision is running in production.


### Ruby cleanup

Continue extracting coherent pieces from `main.rb` incrementally rather than attempting a rewrite.

* [ ] Remove duplicated helper/setup definitions that now exist both in `helper.rb` and `main.rb`
* [ ] Decide whether the remaining login/session routes should move into a dedicated module now that the authentication helpers are extracted
* [ ] Consider workspace lifecycle / nginx management as another future extraction candidate


## Existing Tutorial Cleanup

Do small editorial and technical fixes as the affected tutorials are touched.

* [ ] Svelte: fix remaining markup/editorial issues and clean up event-listener teardown
* [ ] Git/GitHub: mark low-level internals as optional/advanced and correct the implication that `git mv` is required for rename detection
* [ ] Fact-check programming-language history, creator/date and lineage claims and use a consistent convention for the year shown in headings
* [ ] Remove the stray `</td></tr>` in `working-with-files.md`
* [ ] Review `_template_codebites.html` language metadata and remaining manually authored homepage image alt attributes
* [ ] On narrow screens, consider left-aligning long tutorial paragraphs instead of using global justified text
* [ ] Add a small `prefers-reduced-motion` pass for Workspace-owned transitions/animations


### Videothek source data

The Videothek dataset should become the common reference dataset for the relational MySQL material and the new Neo4j tutorial.

* [ ] Locate the existing IMDb data collection/conversion script
* [ ] Turn refreshing `movies.txt`, `genres.txt` and `crew.txt` into one reproducible command
* [ ] Keep the generated files deterministic enough that changes can be reviewed sensibly in Git
* [ ] Print useful sanity statistics after a refresh (movie/person/genre counts and similar checks)
* [ ] Refresh the teaching dataset periodically rather than letting it become visibly stale
* [ ] Do not auto-commit refreshed data; inspect the diff before replacing teaching data and screenshots
* [ ] Extend the generated relational output so schema and data can also be imported separately while retaining one convenient complete SQL dump

The SQL and Neo4j tutorials should consume the same underlying source data so that their modelling differences are real rather than contrived.



### Database tutorial sequence

Rework the existing database material as one coherent sequence **before starting the other new tutorial projects below**.

Main progression:

> query an existing relational database → model relational data → revisit the same domain as a graph → use SQL safely from application code

The relational movie tutorial and the Neo4j tutorial should use the same curated IMDb-derived dataset. Bobby Tables should deliberately remain a small, isolated security lab rather than becoming part of the movie database.

Students can create multiple MySQL databases in the Workspace. Use that capability to keep exercises isolated where useful, especially Bobby Tables. Students have one Neo4j database, so the Neo4j tutorial should use reset/reload of that database rather than assume that several independent Neo4j databases can be created.

#### SQL introduction (`/mysql`)

Keep the existing Terra dataset and the successful progression from simple `SELECT` queries to independent exercises, but get students to SQL much faster.

* [ ] Remove most duplicated shell/terminal teaching from the opening and link to the Workspace basics where appropriate
* [ ] Keep the dataset download/import short so `SHOW TABLES`, `DESCRIBE` and the first `SELECT` arrive early
* [ ] Use single quotes consistently for SQL string literals
* [ ] Fix/remove the population-density claim unless the query actually calculates population density
* [ ] Introduce `COUNT`, `LIKE` and `DISTINCT` explicitly before exercises that require them
* [ ] Keep the final independent query exercises and tighten wording/screenshots as needed
* [ ] Stop before `JOIN`; let relationships between tables become the conceptual payoff of the movie tutorial

#### Filmdatenbank mit MySQL (`/videothek`)

Refocus the current Videothek chapter on relational modelling instead of making a Ruby console application the destination. Consider renaming the chapter from **Videothek mit MySQL** to **Filmdatenbank mit MySQL** so it pairs naturally with the Neo4j chapter.

Keep the strongest existing ideas:

* [ ] Start by inspecting a real movie JSON record and identify the entities/relationships hidden in it
* [ ] Keep the ERD Editor, but make the modelling problem the lesson and the editor merely the tool used to express the solution
* [ ] Build the first model from `movie`, `genre` and `movie_genre`
* [ ] Make the n:m relationship and the composite `(movie_id, genre_id)` primary key explicit conceptual milestones
* [ ] Export the ERD as SQL and inspect the generated `CREATE TABLE`, primary-key and foreign-key statements
* [ ] Load the movie data directly from a data-only SQL dump after students have created the schema

Make `JOIN` the central SQL idea of the chapter:

* [ ] Develop the first movie/genre join incrementally instead of presenting a finished multi-table query
* [ ] Use one `JOIN ... ON ...` step per relationship so the SQL mirrors the relational model clearly
* [ ] Add meaningful movie questions that naturally introduce `GROUP BY`, `COUNT` and useful aggregations
* [ ] Prefer questions students might actually ask of the dataset: films by genre, genres of a film, top-rated films, films per genre and similar examples

Extend the model once, deliberately:

* [ ] Introduce people and jobs only after the movie/genre model is understood
* [ ] Let students reason about why `crew`, `job` and `movie_crew` are needed before revealing/completing the larger ERD
* [ ] Load the complete dataset without requiring a second hand-written Ruby importer or a second tutorial-specific data pipeline
* [ ] End with relational questions about actors/directors/collaborations whose increasing JOIN complexity creates a genuine reason to try a graph database next

Remove accidental complexity from the core path:

* [ ] Remove `gem install mysql2`, `gem install tty` and the Ruby import script from the core database-learning flow
* [ ] Remove the deliberately crashing TTY Videothek application as the chapter's main payoff
* [ ] Do not ask students to replace the importer and reset/rebuild the database halfway through the tutorial
* [ ] If an application example remains, make it small and focused on parameterized queries rather than TTY/menu abstractions or Ruby metaprogramming

Finish with an explicit bridge to the Neo4j tutorial: the same films, genres, people and jobs will be modelled again, but relationships become first-class graph relationships instead of join tables.

#### Bobby Tables / SQL Injection

Keep this as a compact experiment after the modelling tutorials: build a vulnerable query, exploit it, then fix the same program and repeat the attack.

* [ ] Give the lab its own throwaway MySQL database; students can create multiple databases and should not need to disturb their movie database
* [ ] Keep the current attack → observe → prepared-statement fix → retry progression
* [ ] Remove any hard-coded generated database name such as `db_1234` from the starter workflow
* [ ] Make parameterized/prepared statements the explicit security lesson and connect them back to any earlier application/database code
* [ ] State clearly that the deliberately plain-text passwords are only for the SQL-injection lab and are not a model for real password storage
* [ ] Keep the legal/ethical boundary clear but concise: only attack the supplied lab or systems for which students have explicit permission


## Tutorials

The infrastructure is now mature enough that the main growth area should again be teaching material. Prefer tutorials that introduce genuinely new ideas rather than simply more installed technology.

The seven tutorials below are the current main teaching projects. Complete the database tutorial pass above first; Neo4j is part of that pass, while the other new tutorial projects can follow afterwards.


### 1. Web Server

Continue directly from the existing TCP/IP tutorial and write the server in **Ruby** using raw TCP sockets.

Main idea:

> TCP → HTTP → browser → web server

Ruby is used here because the socket code stays close to the conceptual model and does not hide HTTP behind a framework.

* [ ] Connect the tutorial explicitly to the TCP/IP tutorial and port 1234
* [ ] Begin with the smallest `TCPServer` that accepts a connection
* [ ] Inspect a browser/`curl` HTTP request as plain text
* [ ] Send a minimal valid HTTP response manually
* [ ] Handle `GET /`
* [ ] Parse the request line into method/path/version
* [ ] Add multiple routes
* [ ] Return `404 Not Found`
* [ ] Serve HTML
* [ ] Serve a small set of static files
* [ ] Read query parameters
* [ ] Optionally handle one small `POST` request
* [ ] Finish by sharing the running server through Shared Live Apps

Do not introduce Sinatra/Rack/Node/Flask here. The point is to discover that a web server is initially just a TCP server that speaks HTTP.


### 2. WebSockets

Follow the web-server tutorial with persistent, bidirectional communication.

Use a **Ruby server + browser JavaScript client**. Keep Ruby on the server so the networking progression remains continuous; introduce JavaScript because it is the language running in the browser.

* [ ] Show why repeated HTTP request/response is awkward for a real-time application
* [ ] Inspect the WebSocket upgrade request and `101 Switching Protocols` response at a useful level
* [ ] Explain the handshake without turning the tutorial into a complete protocol implementation exercise
* [ ] Establish a WebSocket connection from browser JavaScript
* [ ] Send messages browser → Ruby server
* [ ] Send messages Ruby server → browser
* [ ] Support several simultaneously connected browsers
* [ ] Broadcast state changes to all clients
* [ ] Build a genuinely collaborative project rather than stopping at chat
* [ ] Use Shared Live Apps so classmates can join the running project

Preferred project: **shared pixel canvas**. Other possible follow-ups include multiplayer Pong, collaborative cursors, classroom voting or a tiny multiplayer world.

If manual frame parsing becomes distracting, provide a tiny framing helper after students have understood the opening handshake.


### 3. Neo4j

Teach **why graph databases exist**, not merely Cypher syntax.

Reuse the **Videothek** data from the MySQL tutorial. Students should encounter the same domain twice: first as normalized tables/foreign keys/join tables, then as nodes/relationships/paths.

Main contrast:

```text
MySQL:
movie ← movie_genre → genre
crew  ← movie_crew → movie
job   ← movie_crew

Neo4j:
(Movie)-[:IN_GENRE]->(Genre)
(Person)-[:ACTOR|DIRECTOR|WRITER|...]->(Movie)
```

This chapter should follow the reworked **Filmdatenbank mit MySQL** tutorial directly. Students should already know the domain and the relational model before they see it represented as a graph.

* [ ] Start in Neo4j Browser with a tiny manually created movie graph
* [ ] Introduce nodes, labels and properties immediately
* [ ] Add relationships and visualize the first graph
* [ ] Teach `MATCH` by asking questions about the tiny graph
* [ ] Load the familiar IMDb-derived movie dataset into the student's existing Neo4j database
* [ ] Make reset/reload the documented clean-start workflow because students do not create several independent Neo4j databases
* [ ] Compare equivalent SQL `JOIN` and Cypher relationship queries side by side
* [ ] Discuss graph modelling choices rather than mechanically converting every SQL table to a node
* [ ] Introduce paths and variable-length paths
* [ ] Use actor/movie connections for a meaningful shortest-path exercise
* [ ] Use repeated collaboration/co-star questions to show where graph traversal is substantially more natural than increasingly deep relational joins
* [ ] Add aggregation only after traversal is understood
* [ ] Show `cypher-shell` briefly so students see that Browser is only one Neo4j client
* [ ] Query the same database from Ruby with the new `neo4j_bolt` wrapper

Before writing the Ruby part:

* [ ] Decide whether `neo4j_bolt` belongs in the student image or should be installed explicitly as part of the tutorial

The programming part should stay small: parameterized `neo4j_query`, simple result handling, and perhaps a movie/person/path explorer. Do not make driver/session plumbing the lesson.

The existing Discogs music collector remains useful as an **optional larger graph dataset/project** after the Videothek comparison is understood.


### 4. Cryptography in the Terminal

Make this feel like a laboratory: perform an operation, change something, and observe what cryptography does.

Organize the tutorial around four questions:

> Has this data changed? → hashes
>
> Can somebody else read this data? → encryption
>
> Who signed/created this data? → digital signatures
>
> How can a server recognize me without receiving my password? → public-key authentication

* [ ] Hash a file with `sha256sum`
* [ ] Change one byte and observe that the digest changes completely
* [ ] Compare identical copies and explain what a hash can/cannot prove
* [ ] Explain why plain SHA-256 is not an appropriate password-storage scheme
* [ ] Introduce salts and dedicated password hashing conceptually; do not invent a home-grown password scheme
* [ ] Encrypt and decrypt a real file symmetrically with OpenSSL
* [ ] Try decryption with the wrong password
* [ ] Generate and inspect an Ed25519 key pair
* [ ] Sign a small text/document
* [ ] Verify the signature using the public key
* [ ] Modify the signed data and observe verification fail
* [ ] Clearly distinguish **secrecy** (encryption) from **authenticity/integrity** (signature)
* [ ] Connect the public/private-key idea to SSH authentication and the existing Git material
* [ ] Show a public-key fingerprint and connect the ending back to hashes

Before finalizing the signing section, test which installed command-line tool gives the cleanest classroom workflow (`ssh-keygen -Y`, OpenSSL or GPG). Keep the mathematics and cipher internals optional.


### 5. Flutter

Write a compact Flutter tutorial that starts in the browser but makes clear that Flutter is a genuine cross-platform application framework, not merely another way to build web pages.

Main idea:

> Develop in the browser, then turn the same project into a real installable app.

Use the existing Dart material as background knowledge. The Bubblesort example is useful only as a short bridge into Flutter's state model; the finished project should feel like an actual app.

Suggested teaching sequence:

* [ ] Create the smallest Flutter project with web **and Android** targets enabled
* [ ] Run it in the browser and introduce hot reload
* [ ] Explain widgets and simple layout
* [ ] Handle button presses
* [ ] Reuse a Dart list and render it as widgets
* [ ] Change the list without rebuilding the UI and use the result to motivate state
* [ ] Introduce `StatefulWidget` and `setState`
* [ ] Build a small ranking / favorites app rather than stopping at a sorting demo
* [ ] Add entries with a text field
* [ ] Add sorting and filtering
* [ ] Extract at least one custom widget
* [ ] Add one small implicit animation if useful
* [ ] Run and share the browser version through Shared Live Apps

Useful conceptual contrast:

```text
HTML/JavaScript:
Find an element and change it.

Flutter:
Change the state and rebuild the interface.
```

#### Android build support

Android compilation is a supported Workspace workflow. Before the tutorial is rolled out broadly, finish the remaining operational checks:

* [ ] Measure memory and PID usage of `flutter build apk --debug` with the resource profiler
* [ ] Re-check the planned student container limits against the Android/Gradle workload
* [ ] Verify whether the currently pinned CMake/NDK components are needed by the normal tutorial build and remove them if not

The tutorial should end with:

```bash
flutter build apk --debug
```

and explain where the generated APK can be found.

Students with Android devices should then be able to:

```text
build APK in Workspace
        ↓
download app-debug.apk
        ↓
allow installation from that browser/file manager
        ↓
install the APK
        ↓
launch their own app from the phone
```

This is intentionally different from Play Store publishing. For the introductory tutorial:

* use an APK because it can be installed directly on a device
* explain that an Android App Bundle (`.aab`) is normally used for store publishing
* do not require students to create release signing keys or Play Store accounts
* mention release signing and `flutter build appbundle` only as a follow-up for students who want to publish an app more formally

#### iOS / iPhone

Explain that the **same Flutter source code can also target iOS**, but the build and installation path is different.

The Workspace itself is Linux-based, so it should not pretend to offer local iOS compilation.

* [ ] Explain that normal Flutter iOS builds require macOS, Xcode and Apple code signing/provisioning
* [ ] Explain that installing an app on an iPhone is not the same simple APK-sideloading workflow available on Android
* [ ] Mention that students with access to a Mac can continue the same Flutter project there
* [ ] Mention a cloud macOS build service such as Codemagic as an optional route for building iOS versions
* [ ] Make clear that signed iOS distribution still requires the appropriate Apple developer/signing setup
* [ ] Keep App Store / alternative EU distribution details optional; they are deployment topics, not part of the introductory Flutter lesson

The main classroom payoff remains:

> Build in the browser, then install the same project as a real Android app.

The iOS section should show that Flutter is genuinely cross-platform while being transparent about Apple's additional build and signing requirements.


### 6. raylib + WebAssembly

Use raylib as the **higher-level C graphics/game route** and keep PixelRAM as the separate low-level framebuffer route.

Core contrast:

```text
PixelRAM:
You get pixels and implement drawing/rendering ideas yourself.

raylib:
You get shapes, input, textures/sound primitives and build the game.
```

The tutorial should be mostly about making a small game; WebAssembly is the final explanation of how the same C program becomes browser software.

Preferred project: **Breakout**.

* [ ] Start with the smallest raylib program: window + one primitive
* [ ] Explain the game loop as `input → update state → draw → repeat`
* [ ] Move an object automatically
* [ ] Use `GetFrameTime()` so movement is expressed per second rather than per frame
* [ ] Add keyboard and mouse input
* [ ] Build the Breakout paddle
* [ ] Add a moving ball with a `Vector2` velocity
* [ ] Add wall/paddle collision
* [ ] Add an array/grid of bricks
* [ ] Destroy bricks and track game state/score
* [ ] Add win/lose/restart behaviour

Keep the core game asset-free if possible: rectangles, circles, lines, text and color are enough. Sounds/textures/particles can be optional polish.

Only after the game works:

* [ ] Reveal the Emscripten build command behind the supplied/simple `make` workflow
* [ ] Explain `C → Emscripten → .wasm + browser glue` at a high level
* [ ] Explain why the browser owns the event loop and how the per-frame callback differs from a native `while` loop
* [ ] Avoid backend-specific or obscure Emscripten details in student code; hide them in the project Makefile where possible
* [ ] Publish the finished static web build on a Hackschule subdomain

Do not turn PixelRAM into a second raylib by adding drawing primitives simply for convenience.


### 7. Hugo / Static Site Generators

Frame this as **build and publish your own multi-page website or blog**, not as a tour of Hugo features.

Connect directly to the existing static HTML/CSS tutorial:

> One HTML page is easy to maintain by hand. What happens when 20 or 50 pages all need the same navigation, header and footer?

* [ ] Begin with the duplication problem: repeated HTML across several pages
* [ ] Create a minimal Hugo site without relying on a downloaded theme
* [ ] Write the first article/page in Markdown
* [ ] Use `hugo server` for live local preview
* [ ] Introduce minimal front matter: `title`, `date`, `draft`
* [ ] Build a shared base HTML layout
* [ ] Insert `.Title` and `.Content`
* [ ] Add several pages/posts
* [ ] Build a list/home template that automatically discovers new content
* [ ] Keep Go-template syntax to the small subset students actually need
* [ ] Add a shared CSS file and let students design the site themselves
* [ ] Introduce tags as one useful example of generated navigation/grouping
* [ ] Make the source/output distinction explicit: Hugo is a build tool, not something that runs on the web server
* [ ] Run `hugo` and inspect the generated `public/` directory
* [ ] Publish **`public/`** to the student's Hackschule webspace/subdomain

Possible projects include a personal homepage, gaming/music/film blog, portfolio, recipe collection, project diary or review site.

Do not require third-party themes, Hugo Modules, multilingual sites, custom output formats, complex shortcodes or deployment pipelines in the introductory tutorial.


## Tutorial Follow-ups

### Homepage / tutorial organization

As the new tutorials land, reorganize `sections.yaml` by **conceptual area** rather than creating a separate “new tutorials” section.

Target headings and homepage order:

1. Einführung
2. Grafik & Spiele
3. Texte & Präsentationen
4. Websites & Apps
5. Terminal, Netzwerk & Sicherheit
6. Datenbanken
7. Experimente
8. Programmiersprachen

Keep **Einführung** first so new students can always find orientation and the basic Workspace workflow. Immediately after that, prefer creative and visible projects that also work well with younger students before moving into the more technical infrastructure-oriented material.

Suggested order inside the main sections:

```text
Einführung
  About → Basics

Grafik & Spiele
  Pixelflow Canvas → Anaglyph 3D → Fire → PixelRAM → raylib + WebAssembly

Texte & Präsentationen
  BIF → shower.js → LaTeX

Websites & Apps
  HTML/CSS → Hugo → Svelte → Flutter

Terminal, Netzwerk & Sicherheit
  Working with Files → Git → TCP/IP → Web Server → WebSockets → Hades → Cryptography

Datenbanken
  MySQL → Videothek → Neo4j → Bobby Tables

Experimente
  Digit Recognition → JPEG

Programmiersprachen
  Keep the existing language gallery.
```

Keep strongly related progressions adjacent, but do not present the homepage as a mandatory linear course. Individual tutorials should link explicitly to useful prerequisites and natural follow-ups.


### Shared Live Apps in existing tutorials

Shared Live Apps is infrastructure, not a separate tutorial.

* [ ] Mention it in BIF as an option for peer-testing unfinished stories
* [ ] Add it to other tutorials where sharing a running application is genuinely useful


### BIF workshop feedback

* [ ] Revisit the BIF tutorial after the teacher workshop and incorporate useful classroom feedback


### Automatron / Formal Languages

Keep Automatron as a contextual teaching tool rather than adding every specialist tool to the main navigation.

* [ ] Link it directly from relevant formal-language / automata material
* [ ] Mention the direct URL in teacher-facing lesson notes where useful
* [ ] Add a short introduction if students are expected to use it independently


## Later Tutorial Ideas

These are worthwhile directions, but not all need to become tutorials soon.


### Prolog

Expose students to a genuinely different programming paradigm.

* facts, rules and queries
* variables and pattern matching
* backtracking
* recursive rules
* a small knowledge base
* a search/logic problem such as route finding, a mystery or a logic puzzle


### Digital Audio

Browser-based sound synthesis would add a new creative computing area without extra server infrastructure.

* oscillators and waveforms
* frequency/pitch and amplitude
* envelopes
* mixing
* waveform/spectrum visualization
* a tiny synthesizer, sequencer or drum machine

Practical requirement: headphones in the classroom.


### WebGPU / Parallel Computing

If classroom browser support is reliable, focus primarily on compute rather than introducing yet another graphics API.

* smallest useful WGSL compute shader
* moving data between JavaScript and the GPU
* CPU vs. GPU comparison
* a highly parallel workload such as particles, cellular automata, image processing or fractals


### Git Collaboration

Extend the existing Git material when collaborative workflows become useful.

* branches
* merge
* merge conflicts
* pull requests
* working with another student
* basic CI/testing


### Data Formats

Possible compact tutorial or collection connecting several existing topics.

* text vs. binary data
* CSV
* JSON
* character encodings
* hex dumps
* connections to JPEG and network protocols


### PixelRAM: Further Projects

The introductory PixelRAM tutorial is complete. Future additions should be optional projects for students who want to go deeper.

Possible projects:

* plasma / classic demoscene effect
* Conway's Game of Life
* Mandelbrot set
* raycaster
* raytracer
* simple software triangle/rasterizer
* advanced game-port demonstrations

Important principle:

> Do not turn PixelRAM into a second raylib. Drawing and rendering algorithms are valuable precisely because students can implement them themselves on top of the framebuffer.


## Suggested Order for Remaining Major Tutorial Work

Prioritize the more enticing, creative material first, especially projects that can also work with younger students. The infrastructure-heavy topics remain important, but they do not need to be the first new material students encounter.

1. raylib + WebAssembly
2. Hugo / Static Site Generators
3. Flutter
4. Web Server
5. WebSockets
6. Cryptography
7. Neo4j

This is a **writing/development priority**, not a prerequisite chain.

* raylib adds another highly visual project path and complements PixelRAM without replacing it.
* Hugo gives students a real website/blog they can keep publishing and connects naturally to the existing HTML/CSS material.
* Flutter adds a polished app-building route with immediate visual feedback.
* Web Server and WebSockets should then be written together as one continuous sequence after TCP/IP.
* Cryptography adds a practical security lab using real command-line tools.
* Neo4j can come after the Videothek refresh/import path and `neo4j_bolt` student setup are ready, allowing the SQL-vs-graph comparison to be done properly rather than rushed.

The homepage order remains independent of this development priority: **Einführung first, then Grafik & Spiele, Texte & Präsentationen, Websites & Apps, Terminal/Netzwerk/Sicherheit, Datenbanken, Experimente and Programmiersprachen**.
