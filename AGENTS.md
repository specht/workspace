# AGENTS.md — Hackschule Workspace

A browser-based development environment for computer science lessons:
students get VS Code (code-server) in a container, plus a website with
activities, a Handbuch and tasks the Workspace checks itself. Content and
user-facing text are German; code and these notes are English.

## Where things are

| Path | What |
| --- | --- |
| `src/content/<slug>/<slug>.md` | an activity: page, `tasks.yaml`, `task-examples.yaml` (what each check must accept and reject), `lehrkraft.md` (teacher note), pictures, example files |
| `src/content/reference/<slug>/` | a Handbuch page; parts marked `<!-- part: name -->` … `<!-- /part -->`, included with `_include(slug#part)` |
| `src/content/sections.yaml`, `reference/sections.yaml` | the catalogs |
| `src/ruby/main.rb`, `src/ruby/include/` | the Sinatra app; content is read on every request in development; who may do what is in `include/access.rb` |
| `src/static/` | CSS, JS, page template (`_template.html`) |
| `src/tutorial-screenshots/` | the screenshot and video generator (its own service) |
| `docker/code/` | the student Workspace image |
| `e2e/` | Playwright tests; read [e2e/AGENTS.md](e2e/AGENTS.md) before writing one |

The development site is `http://workspace.test:8025`; log in with any admin
address from `credentials.rb` and the code `123456`.

## Documentation

- [README.md](README.md): what the Workspace is, installation
- [docs/entwicklung.md](docs/entwicklung.md): what to rebuild after a change, tests, screenshots, extensions, image
- [docs/aktivitaet-schreiben.md](docs/aktivitaet-schreiben.md): writing an activity, with a template in `docs/vorlage-aktivitaet/`
- [docs/didaktik.md](docs/didaktik.md): how activities make students think instead of copy, and the decisions
  behind it (»Jetzt bist du dran«, changes instead of whole files, checks of behaviour, predictions, tips in steps)
- [docs/aufgaben.md](docs/aufgaben.md): every meta field, task type and placeholder
- [docs/datenschutz.md](docs/datenschutz.md): Impressum, Datenschutzerklärung, the documents for the school, deleting accounts;
  change `src/ruby/include/privacy.rb` whenever what is stored changes
- [docs/handoff-datenschutz.md](docs/handoff-datenschutz.md): the state of data protection for the next session:
  the transition period up to 12 April 2027, the legal situation in Berlin with sources, the ways a school
  may use the Workspace, what the DSB and the maintainer still have to decide
- [docs/plan-datenschutz-schulen.md](docs/plan-datenschutz-schulen.md): the plan in German for non-specialists
  (Schulleitung, DSB, an attorney): who is approached when, which document goes to whom, the open questions
- [docs/widgets.md](docs/widgets.md): how the step-by-step widgets look, move and are built (the AVL tree is the model)
- [docs/teacher-access.md](docs/teacher-access.md): roles, groups, the roster import, /personen, what teachers see
- [docs/test-workspace-packages.md](docs/test-workspace-packages.md): Workspace packages for Leistungsüberprüfungen
- [docs/checkliste.md](docs/checkliste.md): every rule an activity must meet, why, and what checks it;
  go through it for every new or changed activity. `src/scripts/audit-activities.rb` counts the countable part
- [src/tutorial-screenshots/README.md](src/tutorial-screenshots/README.md): the recipe language for generated pictures
- [CHANGELOG.md](CHANGELOG.md): per release, what an operator of another installation must know
- [TODO.md](TODO.md): direction and open work; delete what is done instead of ticking it off

## Rules for content

- Names in code are English (variables, functions, constants, files, routes);
  comments and program output may be German. In German prose, technical terms
  developers say in English stay English (Handshake, Lock, String, Header).
- Every picture has an `alt` text that says what is to be seen.
- Pictures side by side are a gallery: `<div class='gallery'>` (optionally
  `columns-2`/`-3`/`-4`, `portrait`, `tiles`, `centered`, `wide-narrow`) with one `<figure>` and
  `<figcaption>` each, as single files; labels go in the caption, never into
  the picture (docs/aktivitaet-schreiben.md). `gallery_markup_test.rb` checks it.
- Screenshots of the Workspace come from recipes (`<!-- tutorial-screenshot -->`
  before the `<img>`), never by hand. Render with
  `src/scripts/recreate-screenshots.rb PAGE`, check `--status`, and look at
  every new picture before committing it with its `.tutorial-screenshots.json`.
- Diagrams: an edge aims at the centres of the nodes it joins (drawn from
  centre to centre under the nodes, or ending on the border on that line), and
  an arrowhead points along its edge, its tip on the node's border. A curved
  arrow arrives on the line towards the centre: its last control point lies on
  that line, so the head follows the curve (`dns/dns-tree.svg`,
  `sql-index/b-tree.svg`).
  Labels read like the page: formulas typeset by LaTeX (cos α, n⃗),
  code as code (`dot`, `cross`, `hit.y = 1.35` with a decimal point), both
  from `src/scripts/diagram_labels.py`, which also draws the German right
  angle (a quarter arc with a dot). Axes as everywhere: in 3D x to the
  right, y up, z towards the viewer; in 2D x to the right, y up, except
  on a screen or a map drawn on it (the raycaster), where y goes down as in
  the code; pure maths (a cosine curve) keeps y up. Keep the
  script that generates a diagram next to it (`bilder/`), not in a scratch
  folder.
- Check marks are Bootstrap icons (`<i class='bi bi-check-lg'></i>`), never
  Unicode characters; `no_check_marks_test.rb` enforces this.
- Icons come from what the site already has, Bootstrap Icons
  (`src/static/include/bootstrap-icons-1.11.3`, `bi-chevron-right`, `bi-x-lg`
  and so on), before drawing an SVG or using a character like › or ×. An
  icon alone in a button must sit in its middle: the site gives `button .bi` a
  right margin for icons beside a label, so reset it (`margin: 0`,
  `::before { vertical-align: 0 }`) and centre with flex, and look at it.
- Code blocks name their language. A copy button (`_copy`) only for what is no
  use to type and for big blocks (about 15 lines and more) that go into a file.
  New code in the middle of a file gets context lines (`~ `, faint, not
  copied) around it, as the file is at that point of the page;
  `src/scripts/code-context.rb PAGE.md` proposes them from the recipes'
  stage files (read each proposal before `--apply`).
- Formulas: KaTeX renders `\(…\)` and `$$…$$`, not single `$…$`. In Markdown write
  `\\(`…`\\)` and `\_` for an underscore inside them (`\\\_` inside `\text{…}`): Markdown eats a single
  backslash before a bracket and turns two underscores into emphasis.
  Raw HTML (table cells) is not Markdown: there it is a single `\(`…`\)`
  (`formula_escaping_test.rb`).
- Tables are HTML (`<table class='table'>`); the site's Markdown has no pipe
  tables (`no_pipe_tables_test.rb`). In HTML tables inside Markdown,
  `</table>` and `</div>` go on lines of their own, otherwise the rest of the
  page renders as raw text. A Handbuch table (`table table-sm reference-table`
  in `<div class='scroll-x'>`) has `<thead>` and `<tbody>`: on a phone it
  becomes a list, labelled with the head (`prepare_reference_tables` in
  `_template.html`). Its first column does not wrap, so long commands and
  error messages go into a code block or a list instead. A comparison with
  three or more columns of code (Ruby beside Python) gets
  `reference-table-wide`, a list already below 992px, and a `<th>` at the
  start of each row, so that every value carries its column's name.
- Every activity has a teacher note (`lehrkraft.md`) whose first paragraph says
  in one or two sentences what the students do (»Lernende …«); then a table of
  facts (Voraussetzungen, Ablauf with one item per Doppelstunde, Dateien,
  Prüfung …), lists instead of long cells and HTML, not Markdown, in the
  cells; then `####` sections (Was schiefgeht, Die Aufgaben) as lists
  (docs/aktivitaet-schreiben.md, `docs/vorlage-aktivitaet/lehrkraft.md`);
  `teacher_note_test.rb` checks the first paragraph and the cells.
- Interactive widgets (steppers) follow [docs/widgets.md](docs/widgets.md): they start with
  what they show, one step per line of the page's pseudocode with the code beside it, and
  everything that stays the same thing moves instead of popping. A widget never solves a task of its
  activity: it shows other examples than the tasks ask about, and where its
  input can be edited, the task's input must not give the answer either.
- A person who made what a page or one of its sections is about (a
  construction, a format, a language, an algorithm) gets a box there; a name
  mentioned in passing does not, and neither does a person no page is about.
  The box is a part of the Handbuch page `reference/personen` (heading,
  `<div class='person'>`, a free photo with a credit line, two or three
  sentences that tie the person to what students know from the Workspace,
  »Du suchst mit `grep`? Das hat er geschrieben.«), placed with
  `_person(name)` after the paragraph that names the person or the thing,
  never before a task it would give away. Without a free photo the box is
  text only, no silhouette. The Personen page is sorted by year of birth;
  docs/aktivitaet-schreiben.md.
- People, books and papers belong wherever they are relevant, in activities
  and Handbuch pages alike. A paper only where it is the historical first
  description of the technique a section is about and a good match for it
  (Whitted's raytracing 1980, Blinn's blobby objects 1982, Perlin's noise
  1985), not a paper for every topic; a book only where it goes on exactly
  where a section stops (Abrash's *Graphics Programming Black Book*). Both
  get a `<div class='book'>` box with `bi-file-earmark-text` or `bi-book`:
  title, author, year, what is in it, a checked link
  (docs/aktivitaet-schreiben.md, »Bücher und Aufsätze zum Weiterlesen«).
  Look for them whenever a page is written or changed.
- A standard or document a page names (an RFC, an ISO standard, a paper,
  a specification) is a checked link to it, in prose, task prompts and
  teacher notes alike; RFCs at the RFC Editor
  (`https://www.rfc-editor.org/rfc/rfc6238.html`, with an anchor such as
  `#appendix-B` where one part is meant). Links to other sites open in a
  new tab; the server sets that (`ExternalLinks`), so no `target` in the
  Markdown (docs/aktivitaet-schreiben.md).
- Invite optional experiments: where students have just got something
  working, a box `<div class='hint try'>` (»Ausprobieren:«) suggests a small
  change and what to watch for, so they see how it works by changing it.
  Not a task, nothing is checked; the page continues with the unchanged code.
- Let students write code before showing it: where the page has just
  explained what a small function does (often with its formula), a box
  `<div class='hint own'>` (»Jetzt bist du dran:«) says what to write and where,
  and the page's code follows between `<!-- solution -->` and
  `<!-- /solution -->` on lines of their own (`SolutionBlocks`). Tips in
  steps come before it, and the solution is their last step: it opens only
  after every tip and the pause after the last, never at once
  (`solution_hints_test.rb`). Small, self-contained steps only; the page
  continues with that code. Where possible a task under the solution checks
  the student's own version (check types ruby, python, c), named by
  `data-check='id'` on the box; solving it opens the solution at once
  (docs/aktivitaet-schreiben.md, »Jetzt bist du dran«).
- What gives away the answer to a prediction (a screenshot of the result,
  the paragraph that explains it) goes between `<!-- after: id -->` and
  `<!-- /after -->` after `_task(id)` (`AfterTask`): students see it once
  they have answered; teachers and visitors see it at once
  (docs/aktivitaet-schreiben.md, `after_task_test.rb`).
- Tips in steps where students can get stuck: `hint` as a list in
  tasks.yaml, or `<!-- hint -->` … `<!-- /hint -->` blocks before the
  solution of a »Jetzt bist du dran« box (`HintSteps`). From understanding
  to strategy to a scaffold, never the complete solution; one step at a time with a pause
  of a minute, the next step hidden until then and no countdown; nothing is stored on the server and no teacher sees who used
  which tip (docs/didaktik.md, »Tipps in Stufen«).
- A Handbuch page holds the basics and the most important things, not
  everything, and links to the official reference or manual for the rest
  (specification, official docs, cheatsheet), in its parts or a closing
  section »Zum Nachschlagen«. Check every link before adding it. Activities
  link to the Handbuch page instead of repeating its official links.

## Working on the code

- Never claim that something comes back, restores or recreates itself
  (»legt es wieder an«, »von selbst«, »jederzeit«) unless the code that
  does it has been read or the behaviour tested. Otherwise say what the
  person has to do, and under which conditions it works. When unsure,
  offer less: an unticked box, a question instead of an answer.

- A new or changed check gets examples in `task-examples.yaml` (docs/aufgaben.md,
  »Beispiele für die Prüfungen«): a solution it must accept, a starting point it
  must reject.
- Run the Ruby tests in the Ruby container, which has the gems the pages
  need (`./config.rb exec ruby bash -c 'for f in test/*_test.rb; do echo "$f: $(ruby "$f" 2>&1 | tail -1)"; done'`);
  outside it, only those without nokogiri, redcarpet or rouge run, and only
  with `LANG=C.UTF-8` (docs/entwicklung.md, »Tests«). For the generator,
  `node --test src/tutorial-screenshots/test/*.test.js`.
- After a change to `src/tutorial-screenshots/server.js`, rebuild the service:
  `./config.rb build tutorial_screenshots && ./config.rb up -d tutorial_screenshots`.
- CHANGELOG: a short German line under »Unreleased« only for what an
  operator of another installation must know (new or changed values in
  `credentials.rb`, migrations or steps on update, features that start
  switched off, changes to what is stored); not every change.

## Git

- Work on a branch; commit in coherent pieces. Merge into `main` and push only
  when the maintainer asks for it.
- Releases are tags `vX.Y.Z` on `main` (Semantic Versioning as in the
  CHANGELOG header), made when the maintainer asks: »Unreleased« becomes the
  version's section, a fresh »Unreleased« goes on top.
- Other repositories (starter templates such as `specht/raylib-starter`, the
  2D Game Studio) get a patch for `git am`, checked with `git apply --check`;
  never commit or push there directly. Exceptions the maintainer allowed:
  `specht/routenplaner-starter`, `specht/backrooms-starter` and the datasets
  in `specht/workspace-files` (`src/scripts/publish-datasets.rb … --push`)
  are committed and pushed directly.
