# Workspace TODO

Where the Hackschule Workspace is going, how it gets there, and the open
maintenance work. Rewritten in October 2026 after a review of all teaching
material and cleaned up on 15 October 2026: what is done is gone from this
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
page, the start page and practice sets from Code Bites (`docs/aufgaben.md`).
Still open:

* [ ] A generated »Wie geht es weiter?« box at the end of a page, from a new `next` field
* [ ] Teacher notes with solutions for every activity; 15 have a `lehrkraft.md`, the SQL introduction and the language pages have none
* [ ] Workspace-state check for a running server (a port answers), for the setup steps of web activities
* [ ] Link the matching Code Bites exercises from each programming-language page


## Roadmap

A writing and development order, not a prerequisite chain for students.


### Datasets

Movies and music are published in `specht/workspace-files` (`movie-archive/`,
built with `src/scripts/collect-movies` and `collect-music`).

* [ ] A student command `load-dataset movies|music [--schema|--data|--part NAME] [--neo4j]` that loads into the student's database (or a named extra database); the pages use `wget` and `mysql <` today
* [ ] Refresh both datasets periodically rather than letting them go stale


### Database sequence

> query an existing database → model → query related tables → the same data as a graph → SQL safely from code

Done: the SQL introduction with checked tasks, the ER-Werkstatt in the
Workspace, »Filmdatenbank mit MySQL« built from the student's ER model, and
»Filmdatenbank als Graph mit Neo4j« with Cypher tasks.

#### SQL introduction (`/mysql`)

* [ ] Shorten the opening (close folder, new folder, terminal) and link the Workspace Handbuch instead
* [ ] Delete the leftover `mysql/mysql.new.md.txt`

#### ER-Werkstatt

* [ ] `_include_er(model.er, chen|crowsfoot|tables)` for pages, so ER diagrams on the website are generated from the model instead of screenshots
* [ ] Min–max cardinalities (`movie (1,1) : (0,n) copy`) → `NULL`/`NOT NULL` and crow's-foot circle and bar
* [ ] Requirement checks that run inserts against the generated schema: "a film can have several genres" (two inserts succeed), "a loan needs an existing copy" (insert fails)
* [ ] Later, if the curriculum needs them: weak entities, generalisation

#### Modelling chapter (new, short)

* [ ] From a short story to an ER model in the DSL; Chen and crow's foot
* [ ] Translation rules for 1:1, 1:n and n:m; relationship attributes
* [ ] One flat table with update, insert and delete anomalies as the reason for splitting tables

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

#### SQL Injection (`/bobby_tables`, hidden from the catalog)

* [ ] Own throwaway database (one of the student's extra MySQL databases), no hard-coded `db_1234` in `login.rb`
* [ ] Add the missing `gem install mysql2 tty-prompt` (not in the student image)
* [ ] Keep attack → observe → prepared statement → retry; tasks as `_task()` cards
* [ ] Say that the plain-text passwords exist only for the lab; keep the legal boundary short
* [ ] Spelling: "heisst", "Schluß"
* [ ] Back into the catalog


### New instruments and pages

Prefer pages that introduce genuinely new ideas rather than more installed technology.
Prioritise creative, visible material that also works with younger students; the order
below is a writing priority, not a prerequisite chain.

1. raylib + WebAssembly (drafted as a 3D activity, see below)
2. Hugo / static site generators
3. Flutter
4. Web server (drafted)
5. WebSockets (drafted)
6. Cryptography

Plus, from the review:

* [ ] **Query stepper / database sandbox**: FROM → JOIN → WHERE → GROUP BY → HAVING → SELECT → ORDER BY → LIMIT with the intermediate table after each step; class repository and Class progress like the other sandboxes
* [ ] **JPEG decoder project**: the interactive Huffman section is excellent, but iDCT, MCU and colour conversion get one paragraph each and there is nothing to implement; a staged decoder with a check per stage


#### 1. and 2. Web server and WebSockets

Drafted as »Ein eigener Webserver« (`src/content/webserver`, a Ruby `TCPServer` in five stages up to files, 404, parameters and a counter with a mutex) and »Eine Pixelwand für alle« (`src/content/pixelwand`, handshake written by the students, frames in a commented `websocket.rb`, messages to everyone, a shared 64 × 40 canvas, shared through Shared Live Apps). Both without a template, linked from TCP/IP.

* [ ] Try both in class; check how the shared pixel wall behaves with a whole class on one Workspace
* [ ] Take over the useful part of the Hades page (HTTP POST, JSON) into the web server page, then retire Hades

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

#### 5. raylib: »Eine Klötzchenwelt in 3D«

Drafted in `src/content/raylib` (activity, tasks, teacher note, stages `stufen/1.c`–`8.c`) with the Handbuch page `reference/raylib`. A block world in first person: camera from two angles, walking with `dt`, gravity and collision, a ray to aim, building and removing, own faces with normals, a GLSL light shader, day and night, torches as point lights, fog, textures from `texturen.png`, drawn in the 2D Game Studio. The starter is `specht/raylib-starter` (same files as `vorlage/`). It sets raylib apart from Anaglyph (scenes) and TIC-80 (2D) by real-time 3D interaction and lighting. The page around the program is PixelRAM's shell, fetched by the starter's Makefile.

* [ ] Apply and deploy `2d-sprite-export.patch` (Game Studio: Funktionen › Sprite › Als Bild speichern), which the texture chapter relies on
* [ ] Render the screenshots (the recipes write the starter files themselves, no clone needed) and check them
* [ ] Try it in class; adjust the pace (4 double lessons) and the tasks
* [ ] An icon of its own for the Handbuch section (now the star)

Do not turn PixelRAM into a second raylib by adding drawing primitives for convenience.

#### 6. Hugo / static site generators

> One HTML page is easy to maintain by hand. What happens when 20 or 50 pages all need the same navigation, header and footer?

* [ ] Begin with the duplication problem; a minimal site without a downloaded theme
* [ ] First page in Markdown, `hugo server`, minimal front matter (`title`, `date`, `draft`)
* [ ] A shared base layout with `.Title` and `.Content`; several pages; a list/home template that discovers new content
* [ ] Only the Go-template subset students need; their own CSS; tags as one example of generated navigation
* [ ] Source vs output: run `hugo`, inspect `public/`, publish it to the student's webspace

No third-party themes, Hugo Modules, multilingual sites, custom output formats, complex shortcodes or deployment pipelines in the introduction.


## Existing pages

Open findings from the October 2026 review:

| Page | Action |
| --- | --- |
| Fire | add tasks (blue flame, wind direction) |
| Hades | only useful for one school's Greek app and needs a real e-mail login; script uses `agr.nhcham.org`, the text links `agr.gymnasiumsteglitz.de`; move its ideas into the web-server page and retire it |
| Ziffernerkennung | collect handwriting in the browser instead of print + 300 dpi scan; predict-then-run tasks; discuss overfitting (training loss 10⁻⁴); a shared preinstalled environment instead of 500 MB download and 2 GB per student; typos "boostrap", "Kenrel", "beboachten", "des die" |
| TCP/IP | rework as the first half of TCP → HTTP → web server: layers, ports, handshake; the Peru server is presented as far away next to Google |
| Git/GitHub | mark low-level internals as optional/advanced; correct the claim that `git mv` is needed so Git notices a rename |
| Programming languages | fact-check history, creator/date and lineage claims; one convention for the year in headings |

Platform-wide:

* [ ] Use the Handbuch part for the opening (close folder, clone, open) on the 24 pages that still repeat it by hand
* [ ] Convert manual screenshots to recipes when a page is touched: 214 manual, 593 generated; most left on Ziffernerkennung (44), Anaglyph (33), Fire (15), Handbuch HTML (12), Hades (11)
* [ ] E2E coverage for Git and TCP/IP
* [ ] On narrow screens, consider left-aligning long paragraphs instead of global justified text
* [ ] A small `prefers-reduced-motion` pass for Workspace-owned transitions and animations
* [ ] Mention Shared Live Apps in BIF for peer-testing unfinished stories, and wherever sharing a running app helps
* [ ] Review `_template_codebites.html` language metadata and remaining manually authored homepage image alt attributes
* [ ] Revisit BIF after the teacher workshop and incorporate classroom feedback
* [ ] Link Automatron from formal-language material and teacher notes; add a short introduction if students use it alone
* [ ] Find a section for new pages as they land (Hugo and Flutter under Web & Schnittstellen); keep related progressions adjacent


## Infrastructure and operations

### Operational logging

* [ ] Audit command/error logging so secrets and credentials cannot appear in logs
* [ ] Prefer safe human-readable operation descriptions over dumping complete commands when commands fail

### TeX Live

* [ ] Add `pgfplots` and `csquotes` to the TeX Live toolchain (`docker/code/Dockerfile`, the `tlmgr install` step); the LaTeX activity draws its diagram with TikZ meanwhile, and biblatex works without csquotes

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

Continue extracting coherent pieces from `main.rb` (about 7000 lines) incrementally.

* [ ] Decide whether the remaining login/session routes move into a dedicated module
* [ ] Workspace lifecycle / nginx management as a future extraction candidate


## Later ideas

* **Prolog** – facts, rules, queries, pattern matching, backtracking, recursive rules, a small knowledge base, a logic puzzle
* **Digital audio** – oscillators, frequency and amplitude, envelopes, mixing, waveform/spectrum views, a tiny synthesizer or drum machine (headphones needed)
* **WebGPU / parallel computing** – a minimal WGSL compute shader, CPU vs GPU, particles, cellular automata, image processing or fractals
* **Git collaboration** – branches, merge, conflicts, pull requests, working with another student, basic CI
* **Data formats** – text vs binary, CSV, JSON, encodings, hex dumps; connects JPEG and network protocols
* **PixelRAM projects** – plasma, Game of Life, Mandelbrot, raycaster, raytracer, a software rasterizer; do not turn PixelRAM into a second raylib
