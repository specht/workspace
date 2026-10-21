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
  `columns-2`/`-3`/`-4`, `portrait`, `tiles`, `centered`) with one `<figure>` and
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
  `\\(`…`\\)` and `\_` for an underscore inside them: Markdown eats a single
  backslash before a bracket and turns two underscores into emphasis.
  Raw HTML (table cells) is not Markdown: there it is a single `\(`…`\)`
  (`formula_escaping_test.rb`).
- Tables are HTML (`<table class='table'>`); the site's Markdown has no pipe
  tables (`no_pipe_tables_test.rb`). In HTML tables inside Markdown,
  `</table>` and `</div>` go on lines of their own, otherwise the rest of the
  page renders as raw text.
- Every activity has a teacher note (`lehrkraft.md`) whose first paragraph says
  what the students do (»Lernende …«); `teacher_note_test.rb` checks it.
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
- Invite optional experiments: where students have just got something
  working, a box `<div class='hint try'>` (»Ausprobieren:«) suggests a small
  change and what to watch for, so they see how it works by changing it.
  Not a task, nothing is checked; the page continues with the unchanged code.
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
- Run the Ruby tests (`cd src/ruby; for f in test/*_test.rb; do ruby "$f" | tail -1; done`)
  and, for the generator, `node --test src/tutorial-screenshots/test/*.test.js`.
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
