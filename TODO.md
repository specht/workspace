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


## Roadmap

A writing and development order, not a prerequisite chain for students.


### Datasets

Countries, movies and music are published in `specht/workspace-files`
(`country-archive/`, `movie-archive/`, `music-archive/`), built with
`src/scripts/collect-countries`, `collect-movies` and `collect-music` and
published with `src/scripts/publish-datasets.rb`. The music data stays a
dataset for teachers to use as they see fit (a project, an exam); no
activity of its own is planned for it.
Students load the data with `wget` and `mysql <` (or `neo4j_bolt load`), as
they would outside the Workspace; no command of our own hides that.

* [ ] Refresh the datasets periodically rather than letting them go stale (countries: the tasks and screenshots of the SQL introduction follow the row counts)
* [ ] After the school year (August 2027): delete the movie files under their old names (`mysql*.sql`, `neo4j.dump` in `movie-archive/`) and `old_files` in `publish-datasets.rb`


### Database sequence

> query an existing database → model → query related tables → the same data as a graph → SQL safely from code

Done: the SQL introduction with checked tasks, the ER-Werkstatt in the
Workspace, »Filmdatenbank mit MySQL« built from the student's ER model, and
»Filmdatenbank als Graph mit Neo4j« with Cypher tasks.

#### ER-Werkstatt

* [ ] Later, if the curriculum needs them: weak entities, generalisation

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
* »Nebenläufigkeit« (`src/content/nebenlaeufigkeit`), Java or Dart: a step-through widget for the lost update, a lock and a deadlock (check type `interleaving`), then threads, `synchronized`, locks in a fixed order and a monitor in Java, or the race at `await`, a lock of `Future`s and isolates in Dart

* [ ] Try each in class and adjust pace and tasks (LaTeX 3, web server 2, pixel wall 2, Hugo 2, weather 2, crypto 2, raylib 5, steganography 3, crime story 2, calculator 2, concurrency 3 double lessons; TCP/IP 2 after its rework)
* [ ] Case server: try the SSH chapter with a whole class (1000 accounts, per-account limits, everyone on `museum.bitstein` at once) before switching it on elsewhere
* [ ] Pixel wall: check how it behaves with a whole class painting on one Workspace

Do not turn PixelRAM into a second raylib by adding drawing primitives for convenience.

### Students write first (October 2026)

»Jetzt bist du dran« boxes with tips in steps, predictions that wait for
the answer, explanations after solving (docs/didaktik.md). Open from that pass:

* [ ] Try boxes and tips in class: is a minute between tips right (`HintSteps::DELAY_SECONDS`), do students work with a tip or wait for the next?
* [ ] Checks that only look for words where running the code is not possible yet: LaTeX, TIC-80 (no Lua in the Workspace), raylib's light, torches and textures, Pixelwand (a WebSocket client in the check), Wetter (network)


### New instruments and pages

Prefer pages that introduce genuinely new ideas rather than more installed technology.
Prioritise creative, visible material that also works with younger students; the order
below is a writing priority, not a prerequisite chain. Flutter is not
coming back into the image (the maintainer, 2 November 2026): too large and
too slow for the Workspace.

* [ ] **The gaps on `/lehrplan`** ([docs/plan-lehrplan-luecken.md](docs/plan-lehrplan-luecken.md)): open are Urheberrecht (a section on licences), Rechenhilfsmittel and Rechnergenerationen (a chapter in `sandbox-cpu`), manipulated pictures and Ergonomie

* [ ] **Dart and Java on more pages** ([docs/dart-java.md](docs/dart-java.md)): run Prüfziffern's and Laufzeit's Dart and Java checks in a real Workspace (recipes); measure a whole class on the Java Katas; the maintainer picks the next activities (automata, calculator, nearest neighbours, Huffman …) and whether the own switch offers Dart and Java
* [ ] **»Wie sicher ist ein Passwort?«**: not written; »Zwei Faktoren« fills the place in Netze & Sicherheit. The branch `passwords` with a first attempt no longer exists (31 October 2026, neither local nor on GitHub); stored passwords with salt are already in »Kryptographie im Terminal«, so this page would be about guessing
* [ ] **Person boxes waiting for a visible page**: Béla Julesz and Christopher Tyler (autostereograms, `sirds`), Yann LeCun (`digits-experiment` is in no catalog); Turner Whitted has his place in Raytracing
* [ ] **Small things left on built pages:** Laufzeit: the students' own measurements as a chart (a `measure` task), person boxes for Stephen Cook and Richard Karp, an Extra on amortised cost (a list that grows by doubling); Nebenläufigkeit: Per Brinch Hansen's box (on the Personen page already) and a `.book` box (Goetz, »Java Concurrency in Practice«); Logik-Werkstatt: buses (four bits on one line), a block for the ALU, »Lösung einsetzen« for circuits; Perzeptron: digits the class draws on the pad as a dataset of its own (an Extra)
* [ ] **JPEG, further**: `decoder` runs the whole decoder; the number tasks before it could go once it has been tried in class
* [ ] **Gaussian Splatting** (`.gaussian-splatting`, development only, November 2026): train a scene ourselves with Brush (the cash register of the DX.GL sets, or a Montreal building of Heritage3DMtl, CC BY 4.0) once a permission rule lets Brush run, and only then let the Extra »Eine eigene Szene« promise more than Brush's README (its CLI exports `.ply` with `--export-every`, read in `--help`); view-dependent colour in the viewer (spherical harmonics, files two to four times bigger), then a chapter »Spiegel und Glas« with the shop windows of the street; a Gauß person box
* [ ] **Fractal image compression** (the maintainer's idea, November 2026): in the compression section, Python with numpy on a small grey picture: range and domain blocks, the eight orientations, contrast and brightness by least squares, a brute-force search, and the decoder that iterates from noise to the picture (Banach's fixed point; a prediction: a contrast above 1). Why it went nowhere (slow encoding, patents, JPEG and wavelets) and what used it (check: Encarta, »Genuine Fractals«) before the page claims it
* [ ] **WebGL and Gaussian Splatting public** (both `.`-pages, development only, November 2026): when they enter the catalog, link them in each other's »Wie geht es weiter?« (`next_steps_test.rb` refuses links to development-only pages until then) and rebuild the graph of `/lehrplan`

### Katas

What exists and why: [docs/katas.md](docs/katas.md).

* [ ] Kata cards: what else a card needs, from the maintainer's use of them (the prompt beside the code from 60em on is done)
* [ ] Pip: more typical wrong plans (`same_as`, by the way Pip walks) where classes show them, after a try in class
* [ ] Person box for Richard E. Pattis (Karel the Robot) once a source gives his year of birth
* [ ] »Software entwerfen« (`check-digits`, `supermarkt`, `class-quiz`, live since 28 October 2026): a try in class; a picture of the Git server's key box only where the live box does not show (students see the box itself on the page)
* [ ] »Testen« (`testen`, in the catalog since 1 November 2026): a try in class
* [ ] A whole-program mode (run the program, compare its output), for a quine as an Extra of Texte

## Existing pages

Open findings from the October 2026 review:

| Page | Action |
| --- | --- |
| Ziffernerkennung | collect handwriting in the browser instead of print + 300 dpi scan; predict-then-run tasks; discuss overfitting (training loss 10⁻⁴); a shared preinstalled environment instead of 500 MB download and 2 GB per student |

Platform-wide:

* [ ] Convert manual screenshots to recipes when a page is touched: 155 manual, 812 generated; most left on Ziffernerkennung (44, retired), Anaglyph (22), Handbuch Farbräume (14), Steganographie (8)
* [ ] When a page is touched: English names in its code (variables, functions, files), as in the raylib, web server and pixel wall activities; comments stay German
* [ ] Revisit BIF after the teacher workshop and incorporate classroom feedback
* [ ] Find a section for new pages as they land; keep related progressions adjacent


## Infrastructure and operations

### Dependency and image verification

Improve supply-chain reproducibility gradually rather than rewriting the Docker build at once.

* [ ] Pin the running services' images in `config.rb` (mysql, neo4j, phpmyadmin, ruby alpine) to the digests that run on the server: their tags were republished since, so pinning to today's digest would change what runs; the maintainer reads them there (`docker image inspect --format '{{join .RepoDigests " "}}' mysql:8.4.11`). The Dockerfiles' base images are pinned. Not checked: the TeX Live installer (no versions, tlmgr checks its packages) and pwndbg (its installer takes the latest)

### Continuous integration

Keep tutorial tests focused on whether the documented student workflow actually works.

* [ ] Heavier browser/toolchain tests manually, nightly or on a suitable self-hosted runner
* [ ] E2E coverage when important pages are changed or newly written

### Ruby cleanup

`main.rb` went from 10648 lines (31 October 2026) to about 4200; what is
left there stays: the TCP-Konsole socket, TIC-80 files, the Anaglyph viewer,
the Terminal-Krimi downloads, the Open-Meteo proxy, and the configure block
with the before filter. New routes go into `include/` by topic.

## Later ideas

* **Digital audio** – oscillators, frequency and amplitude, envelopes, mixing, waveform/spectrum views, a tiny synthesizer or drum machine (headphones needed)
* **WebGPU / parallel computing** – a minimal WGSL compute shader, CPU vs GPU, particles, cellular automata, image processing or fractals
* **Git collaboration, further** – pull requests and basic CI on top of the class quiz (`class-quiz` has branches, merges, conflicts and a review)
* **Data formats** – text vs binary, CSV, JSON, encodings, hex dumps; connects JPEG and network protocols
* **PixelRAM projects** – plasma, Game of Life, Mandelbrot, raycaster, raytracer, a software rasterizer; do not turn PixelRAM into a second raylib
* **PostgreSQL** – next to MySQL, with the exercises of [practical-sql](https://github.com/anthonydb/practical-sql)
* **LDAP for schools** – a simple directory for logins, for example [glauth](https://github.com/glauth/glauth) with a config file
