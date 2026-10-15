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
* [ ] Teacher notes with solutions for every activity; 23 have a `lehrkraft.md`, the SQL introduction and the language pages have none
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


### Written, not yet tried in class

Five activities written in October 2026, with tasks, teacher notes and
generated screenshots:

* »Eine Facharbeit mit LaTeX« (`src/content/latex`) and Handbuch »Dokumente mit LaTeX«
* »Ein eigener Webserver« (`src/content/webserver`), a Ruby `TCPServer` in six stages (the last a poll with `POST` and `303`), linked from TCP/IP
* »Eine Pixelwand für alle« (`src/content/pixelwand`), WebSockets with faye-websocket, a shared 64 × 40 canvas through Shared Live Apps, a chapter on threads and a lock
* »Eine Website mit Hugo« (`src/content/hugo`), an aquarium encyclopedia with own layouts, published from `public/`
* »Kryptographie im Terminal« (`src/content/crypto`), hashes, salted password hashes, AES, RSA, Ed25519 signatures with `ssh-keygen -Y`, a partner part over port 1234
* »Daten aus dem Netz« (`src/content/wetter`), the Open-Meteo weather API with `curl`/`jq`, Ruby with a cache, and a page with `fetch`
* »Eine Klötzchenwelt in 3D« (`src/content/raylib`), raylib in 3D with Handbuch »raylib«; the starter is `specht/raylib-starter`, the same files as `vorlage/`

* [ ] Try each in class and adjust pace and tasks (LaTeX 3, web server 2, pixel wall 2, Hugo 2, weather 2, crypto 2, raylib 5 double lessons; TCP/IP 2 after its rework)
* [ ] Pixel wall: check how it behaves with a whole class painting on one Workspace
* [ ] raylib: an icon of its own for the Handbuch section (now the star)

Do not turn PixelRAM into a second raylib by adding drawing primitives for convenience.


### New instruments and pages

Prefer pages that introduce genuinely new ideas rather than more installed technology.
Prioritise creative, visible material that also works with younger students; the order
below is a writing priority, not a prerequisite chain.

1. Flutter
2. Cryptography

Plus, from the review:

* [ ] **Query stepper / database sandbox**: FROM → JOIN → WHERE → GROUP BY → HAVING → SELECT → ORDER BY → LIMIT with the intermediate table after each step; class repository and Class progress like the other sandboxes
* [ ] **JPEG decoder project**: the interactive Huffman section is excellent, but iDCT, MCU and colour conversion get one paragraph each and there is nothing to implement; a staged decoder with a check per stage

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

Android build support, before broad rollout:

* [ ] Measure memory and PID usage of `flutter build apk --debug` with the resource profiler
* [ ] Re-check student container limits against the Android/Gradle workload
* [ ] Check whether the pinned CMake/NDK components are needed and remove them if not

APK rather than App Bundle for the introduction; no release signing or Play Store accounts (mention `flutter build appbundle` as a follow-up). iOS: the same source can target iOS, but builds need macOS, Xcode and Apple signing; mention a Mac or a cloud build service such as Codemagic; keep App Store details optional.

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
* [ ] Convert manual screenshots to recipes when a page is touched: 213 manual, 637 generated; most left on Ziffernerkennung (44), Anaglyph (33), Fire (15), Handbuch HTML (12)
* [ ] E2E coverage for Git and TCP/IP
* [ ] When a page is touched: English names in its code (variables, functions, files), as in the raylib, web server and pixel wall activities; comments stay German
* [ ] On narrow screens, consider left-aligning long paragraphs instead of global justified text
* [ ] A small `prefers-reduced-motion` pass for Workspace-owned transitions and animations
* [ ] Mention Shared Live Apps in BIF for peer-testing unfinished stories, and wherever sharing a running app helps
* [ ] Review `_template_codebites.html` language metadata and remaining manually authored homepage image alt attributes
* [ ] Revisit BIF after the teacher workshop and incorporate classroom feedback
* [ ] Link Automatron from formal-language material and teacher notes; add a short introduction if students use it alone
* [ ] Find a section for new pages as they land (Flutter under Web & Schnittstellen); keep related progressions adjacent


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
* **PostgreSQL** – next to MySQL, with the exercises of [practical-sql](https://github.com/anthonydb/practical-sql)
* **LDAP for schools** – a simple directory for logins, for example [glauth](https://github.com/glauth/glauth) with a config file
