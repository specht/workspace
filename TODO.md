# Workspace TODO

Where the Hackschule Workspace is going, how it gets there, and the open
maintenance work. Rewritten in October 2026 after a review of all teaching
material and cleaned up on 16 October 2026: what is done is gone from this
list; `docs/aufgaben.md` describes what exists.


## Direction

The Workspace supports a teacher who is in the room. The teacher explains,
talks with students and decides what happens next. The Workspace takes over the
work that does not need a person:

1. **Setup** – nothing to install, the same environment at school and at home.
2. **Making the invisible visible** – one instrument per concept (Sorting,
   Data Structures and CPU sandboxes, the ER-Werkstatt, later a query stepper).
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


### Content model

Activities, Handbuch pages and tasks with checks are in place, as are the class
page and the start page (`docs/aufgaben.md`).
Still open:

* [ ] A generated »Wie geht es weiter?« box at the end of a page, from a new `next` field
* [ ] Teacher notes for the language pages (Steckbriefe): every activity in a catalog has a `lehrkraft.md`, they have none
* [ ] Workspace-state check for a running server (a port answers), for the setup steps of web activities


## Roadmap

A writing and development order, not a prerequisite chain for students.


### Datasets

Countries, movies and music are published in `specht/workspace-files`
(`country-archive/`, `movie-archive/`, `music-archive/`), built with
`src/scripts/collect-countries`, `collect-movies` and `collect-music` and
published with `src/scripts/publish-datasets.rb`.

* [ ] A student command `load-dataset countries|movies|music [--schema|--data|--part NAME] [--neo4j]` that loads into the student's database (or a named extra database); the pages use `wget` and `mysql <` today
* [ ] Refresh the datasets periodically rather than letting them go stale (countries: the tasks and screenshots of the SQL introduction follow the row counts)
* [ ] After the school year (August 2027): delete the movie files under their old names (`mysql*.sql`, `neo4j.dump` in `movie-archive/`) and `old_files` in `publish-datasets.rb`


### Database sequence

> query an existing database → model → query related tables → the same data as a graph → SQL safely from code

Done: the SQL introduction with checked tasks, the ER-Werkstatt in the
Workspace, »Filmdatenbank mit MySQL« built from the student's ER model, and
»Filmdatenbank als Graph mit Neo4j« with Cypher tasks.

#### SQL introduction (`/mysql`)

* [ ] Shorten the opening (close folder, new folder, terminal) and link the Workspace Handbuch instead

#### ER-Werkstatt

* [ ] `_include_er(model.er, chen|crowsfoot|tables)` for pages, so ER diagrams on the website are generated from the model instead of screenshots
* [ ] Min–max cardinalities (`movie (1,1) : (0,n) copy`) → `NULL`/`NOT NULL` and crow's-foot circle and bar
* [ ] Requirement checks that run inserts against the generated schema: "a film can have several genres" (two inserts succeed), "a loan needs an existing copy" (insert fails)
* [ ] Later, if the curriculum needs them: weak entities, generalisation

#### Modelling chapter (new, short)

* [ ] From a short story to an ER model in the DSL; Chen and crow's foot
* [ ] Translation rules for 1:1, 1:n and n:m; relationship attributes
* [ ] One flat table with update, insert and delete anomalies as the reason for splitting tables
* [ ] The borders of the country dataset as an example of deliberate redundancy: every pair twice, what goes wrong if one direction is missing, how to store it once

#### Filmdatenbank

Then delete `_drafts/filmdatenbank.md`, which still holds these ideas.

* [ ] Add the 1:n case: `movie 1:n copy`, `customer` borrows `copy` with loan and return date → relationship attributes, `NULL`, `LEFT JOIN` ("which copies are out?", "which films were never rented?")
* [ ] Integrity experiments: insert a `movie_genre` row for a missing film, delete a genre still in use, then `ON DELETE`
* [ ] "A title is not a key": two films with the same title (Nosferatu 1922 and 2024)
* [ ] Music twin of the chapter on the Discogs dataset

#### Neo4j

* [ ] Start with a tiny graph created by hand (`CREATE`, labels, properties, relationships) before loading the dump
* [ ] Reset/reload as the documented clean-start workflow
* [ ] Graph modelling choices instead of converting every table to a node
* [ ] The Discogs music graph (`MEMBER_OF`, `RELEASED`, `HAS_TRACK`) as the second dataset

### Written, not yet tried in class

Activities written in October 2026, with tasks, teacher notes and
generated screenshots:

* »Eine Facharbeit mit LaTeX« (`src/content/latex`) and Handbuch »Dokumente mit LaTeX«
* »Ein eigener Webserver« (`src/content/webserver`), a Ruby `TCPServer` in six stages (the last a poll with `POST` and `303`), linked from TCP/IP
* »Eine Pixelwand für alle« (`src/content/pixelwand`), WebSockets with faye-websocket, a shared 64 × 40 canvas through Shared Live Apps, a chapter on threads and a lock
* »Eine Website mit Hugo« (`src/content/hugo`), a small encyclopedia of the planets with own layouts, published from `public/`
* »Kryptographie im Terminal« (`src/content/crypto`), hashes, salted password hashes, AES, RSA, Ed25519 signatures with `ssh-keygen -Y`, a partner part over port 1234
* »Daten aus dem Netz« (`src/content/wetter`), the Open-Meteo weather API with `curl`/`jq`, Ruby with a cache, and a page with `fetch`
* »Eine Klötzchenwelt in 3D« (`src/content/raylib`), raylib in 3D with Handbuch »raylib«; the starter is `specht/raylib-starter`, the same files as `vorlage/`
* »Geheime Botschaften in Bildern« (`src/content/steganography`), Ruby/Python with ChunkyPNG/Pillow: bit planes, LSB with a length header, the chi-square test, compressed versus plain messages, colour classes with a mixed-radix number in RGB
* »Ein Krimi im Terminal« (`src/content/terminal-mystery`), a theft solved with `grep`, `head | tail`, `sort | uniq -d` and `git log`/`git show`; with the case server (`CASE_SERVER`) a last chapter with `ssh`, `ssh -J` and `scp`
* »Ein Rechner für Ausdrücke« (`src/content/calculator`), Ruby/Python: tokens, recursive descent, evaluation, errors, `^` and variables, with a tree drawer that steps through the evaluation

* [ ] Try each in class and adjust pace and tasks (LaTeX 3, web server 2, pixel wall 2, Hugo 2, weather 2, crypto 2, raylib 5, steganography 3, crime story 2, calculator 2 double lessons; TCP/IP 2 after its rework)
* [ ] Case server: try the SSH chapter with a whole class (1000 accounts, per-account limits, everyone on `museum.bitstein` at once) before switching it on elsewhere
* [ ] Pixel wall: check how it behaves with a whole class painting on one Workspace
* [ ] raylib: an icon of its own for the Handbuch section (now the star)

Do not turn PixelRAM into a second raylib by adding drawing primitives for convenience.

### Students write first (October 2026)

»Jetzt bist du dran« boxes with tips in steps, predictions that wait for
the answer, explanations after solving (docs/didaktik.md). Open from that pass:

* [ ] Try boxes and tips in class: is a minute between tips right (`HintSteps::DELAY_SECONDS`), do students work with a tip or wait for the next?
* [ ] raylib's five `c` checks ran only against raylib 6.0's headers outside a Workspace; confirm them in a real one (the recipes on stages 2 to 5 run them with `check-task`)
* [ ] Web server »Eine Umfrage« still looks for words in `server.rb`; a `command` check against the running server (`curl -si -d choice=curry localhost:8000/vote`, expect `303` and `Location: /results`) needs an output mode where one line has to match, not all
* [ ] Checks that only look for words where running the code is not possible yet: LaTeX, TIC-80 (no Lua in the Workspace), raylib's light, torches and textures, Pixelwand (a WebSocket client in the check), Wetter (network)
* [ ] GLSL: no check looks at a shader; a compile check (`glslangValidator` in the Workspace) would at least catch errors before the preview does


### New instruments and pages

Prefer pages that introduce genuinely new ideas rather than more installed technology.
Prioritise creative, visible material that also works with younger students; the order
below is a writing priority, not a prerequisite chain.

1. Flutter

Plus, from the review:

* [ ] **The gaps on `/lehrplan`** ([docs/plan-lehrplan-luecken.md](docs/plan-lehrplan-luecken.md)): object orientation, the language model, the Turing machine and the logic sandbox are built; open are Urheberrecht (a section on licences), Rechenhilfsmittel and Rechnergenerationen (a chapter in `sandbox-cpu`), manipulated pictures and Ergonomie

* [ ] **Concurrency in Java and Dart** ([docs/plan-nebenlaeufigkeit.md](docs/plan-nebenlaeufigkeit.md)): a step-through widget for lost updates, locks and deadlock; Java threads, `synchronized` and the monitor; Dart's races at `await` and isolates. First the `dart` check must await a `Future`; place and name are the maintainer's
* [ ] **Dart and Java on more pages** ([docs/plan-dart-java.md](docs/plan-dart-java.md)): run Prüfziffern's and Laufzeit's Dart and Java checks in a real Workspace (recipes); measure a whole class on the Java Katas; the maintainer picks the next activities (automata, calculator, nearest neighbours, Huffman …) and whether the own switch offers Dart and Java
* [ ] **Query stepper, further**: subqueries and `UNION`; the music data (Discogs) once it has a chapter
* [ ] **»Wie sicher ist ein Passwort?«**: not written; »Zwei Faktoren« fills the place in Netze & Sicherheit. The local branch `passwords` keeps solutions, starters, a list and tasks from a first attempt
* [ ] **Person boxes waiting for a visible page**: Béla Julesz and Christopher Tyler (autostereograms, `sirds`), Yann LeCun (`digits-experiment` is in no catalog); Turner Whitted has his place in Raytracing
* [ ] **JPEG, further**: a widget for the MCU (four Y blocks, Cb and Cr enlarged, the colours) and for the colour conversion; a check that runs the student's decoder instead of asking for numbers

#### Flutter

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

Flutter, the Android SDK and the Gradle cache left the image on 28 October 2026
(no activity used them; 8.6 GB of 16.6 GiB). For this activity they come back:
the stages `flutter`, `android-sdk` and `flutter-android` and the test
`e2e/tests/flutter.toolchain.spec.ts` are in the Git history before that date.
Then, before broad rollout:

* [ ] Measure memory and PID usage of `flutter build apk --debug` with the resource profiler
* [ ] Re-check student container limits against the Android/Gradle workload
* [ ] Check whether the pinned CMake/NDK components are needed and remove them if not
* [ ] `sdkmanager` is deprecated in favour of the Android CLI; accepting the licences and unzipping the NDK took most of that stage's time
* [ ] Dart comes on its own now (`DART_VERSION`); Flutter brings its own, so one of the two goes

APK rather than App Bundle for the introduction; no release signing or Play Store accounts (mention `flutter build appbundle` as a follow-up). iOS: the same source can target iOS, but builds need macOS, Xcode and Apple signing; mention a Mac or a cloud build service such as Codemagic; keep App Store details optional.

### Katas

What exists and why: [docs/plan-katas.md](docs/plan-katas.md).

* [ ] **One Kata widget** instead of the general task card on Kata pages: the prompt beside the code on a wide screen, as Pip's map beside the plan, and a UI/UX pass over all of it; planned and shown to the maintainer before it is built
* [ ] The output of a run while it runs (live, over the page's WebSocket); now it comes when the run ends
* [ ] A time limit per case where speed is the point (now `timeout` is for all cases together; `slow:` words the message)
* [ ] Pip: a map of the tunnels at the top of each Pip page that lights up chapter by chapter; `same_as`-style feedback by trace (»Pip biegt immer links ab, auch wenn dort eine Wand ist.«)
* [ ] Person boxes for Euklid, Fibonacci, Collatz and Eratosthenes (Schleifen); Richard E. Pattis (Karel the Robot) once a source gives his year of birth
* [ ] »Software entwerfen« (`check-digits`, `supermarkt`, `class-quiz`, live since 28 October 2026): screenshots of the UML-Werkstatt and of the Git server's key box from recipes, a person box for Dahl and Nygaard (Simula) in `supermarkt`, a try in class. The other packages for the curriculum's gaps: docs/plan-lehrplan-luecken.md
* [ ] `/images/white.webp`, the picture of a card without one, is missing (404)
* [ ] The activity »Testen«: tests as files in the Workspace (`pytest`, `minitest`), red-green-refactor, with the check type `tests` reading files (`file:`, `against:`, mutants as files)
* [ ] A whole-program mode (run the program, compare its output), for a quine as an Extra of Texte

## Existing pages

Open findings from the October 2026 review:

| Page | Action |
| --- | --- |
| Fire | add tasks (blue flame, wind direction) |
| Ziffernerkennung | collect handwriting in the browser instead of print + 300 dpi scan; predict-then-run tasks; discuss overfitting (training loss 10⁻⁴); a shared preinstalled environment instead of 500 MB download and 2 GB per student; typos "boostrap", "Kenrel", "beboachten", "des die" |
| Git/GitHub | mark low-level internals as optional/advanced |
| Programming languages | fact-check history, creator/date and lineage claims; one convention for the year in headings |

Platform-wide:

* [ ] Use the Handbuch part for the opening (close folder, clone, open) on the 24 pages that still repeat it by hand
* [ ] Convert manual screenshots to recipes when a page is touched: 155 manual, 812 generated; most left on Ziffernerkennung (44, retired), Anaglyph (22), Handbuch Farbräume (14), Steganographie (8)
* [ ] A dark card picture for the activities that have none (`audit-activities.rb`, column »Karte«: »nicht dunkel«), from `variants: dark` or `render-svg.rb`
* [ ] Small pictures stretched by `class='full'` (100 % wide, at least 1000 px): `gif/dithering.webp` (920 px wide) and the 1350-px PixelRAM shots; render them larger, or show a drawing at its own size with `img.drawing` and a recipe with `zoom: 2`, as the calculator and the automata pages do
* [ ] E2E coverage for TCP/IP (the Git checks run in its screenshot recipes)
* [ ] When a page is touched: English names in its code (variables, functions, files), as in the raylib, web server and pixel wall activities; comments stay German
* [ ] On narrow screens, consider left-aligning long paragraphs instead of global justified text
* [ ] A small `prefers-reduced-motion` pass for Workspace-owned transitions and animations
* [ ] Mention Shared Live Apps in BIF for peer-testing unfinished stories, and wherever sharing a running app helps
* [ ] Review the remaining manually authored homepage image alt attributes
* [ ] Revisit BIF after the teacher workshop and incorporate classroom feedback
* [ ] Link Automatron from formal-language material and teacher notes; add a short introduction if students use it alone
* [ ] Find a section for new pages as they land (Flutter under Web & Schnittstellen); keep related progressions adjacent


## Infrastructure and operations

### Operational logging

* [ ] Audit command/error logging so secrets and credentials cannot appear in logs
* [ ] Prefer safe human-readable operation descriptions over dumping complete commands when commands fail

### TeX Live


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


### Ruby cleanup

Continue extracting coherent pieces from `main.rb` (about 7000 lines) incrementally.

* [ ] Decide whether the remaining login/session routes move into a dedicated module
* [ ] Workspace lifecycle / nginx management as a future extraction candidate


## Later ideas

* **Digital audio** – oscillators, frequency and amplitude, envelopes, mixing, waveform/spectrum views, a tiny synthesizer or drum machine (headphones needed)
* **WebGPU / parallel computing** – a minimal WGSL compute shader, CPU vs GPU, particles, cellular automata, image processing or fractals
* **Git collaboration, further** – pull requests and basic CI on top of the class quiz (`class-quiz` has branches, merges, conflicts and a review)
* **Data formats** – text vs binary, CSV, JSON, encodings, hex dumps; connects JPEG and network protocols
* **PixelRAM projects** – plasma, Game of Life, Mandelbrot, raycaster, raytracer, a software rasterizer; do not turn PixelRAM into a second raylib
* **PostgreSQL** – next to MySQL, with the exercises of [practical-sql](https://github.com/anthonydb/practical-sql)
* **LDAP for schools** – a simple directory for logins, for example [glauth](https://github.com/glauth/glauth) with a config file
