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
- [docs/plan-katas.md](docs/plan-katas.md): Katas (code typed into a card; check types `function`, `pip`,
  `tests`), Pip unter Funkelbach in three parts: what exists, the maintainer's decisions and why, what is open
- [docs/lehrplan.md](docs/lehrplan.md): the `curriculum:` line every activity carries for teachers:
  its form and the only names allowed, from the Berlin-Brandenburg curricula
- [docs/plan-bundeslaender.md](docs/plan-bundeslaender.md): the curricula of other states: topics per
  activity (`topics:`), one plan per file in `src/content/curricula/`, the teacher's state in the profile;
  Baden-Württemberg as a draft for a teacher there to check
- [docs/plan-lehrplan-luecken.md](docs/plan-lehrplan-luecken.md): the names of the curricula with no
  or few activities, what could fill each, what is decided (object orientation as an activity with
  modelling, in five languages) and what is open
- [docs/plan-git-gruppen.md](docs/plan-git-gruppen.md): Git repositories for groups kept by the Workspace
  itself (a mounted directory or an SSH container), browsable on the site; a plan, open questions
- [docs/plan-treffpunkt.md](docs/plan-treffpunkt.md): a meeting point a teacher sets in an activity for
  a group, live, soft (nothing hidden); what is decided and how it could be built
- `/picture-rights` (`src/ruby/include/picture_rights.rb`, `src/content/picture-rights.yaml`): pictures that are
  someone else's without a free licence, with a letter asking for permission; off the pages until granted
- [docs/checkliste.md](docs/checkliste.md): every rule an activity must meet, why, and what checks it;
  go through it for every new or changed activity. `src/scripts/audit-activities.rb` counts the countable part
- [src/tutorial-screenshots/README.md](src/tutorial-screenshots/README.md): the recipe language for generated pictures
- [CHANGELOG.md](CHANGELOG.md): per release, what an operator of another installation must know
- [TODO.md](TODO.md): direction and open work; delete what is done instead of ticking it off

## Rules for content

[docs/checkliste.md](docs/checkliste.md) holds every rule for activities and
Handbuch pages, with the reason and what checks it; go through it for every
new or changed page, and add a rule there when the maintainer gives one (with
a test where a grep can check it, `wording_test.rb`). The ones broken most
often:

- Names in code are English; German prose keeps English technical terms
  (Handshake, String, Stack, Queue). »Index«, not »Stelle«; no »Ding«.
- Screenshots come from recipes (`<!-- tutorial-screenshot -->`), rendered
  with `src/scripts/recreate-screenshots.rb PAGE`; look at every new picture.
- Tables are HTML, icons are Bootstrap Icons, every picture has an `alt`
  text, pictures side by side are a gallery, every code block names its
  language.
- A task's answer is not on the page before it; what gives a prediction away
  goes after it (`<!-- after: id -->`). Students write small functions first
  (»Jetzt bist du dran«), with tips in steps; try boxes say what to watch for.
- Activities of a hidden section are never named or linked outside it.
- The /lehrplan graph is laid out once in development and stored: whenever an
  activity is added, removed or renamed, its `requires:` changes or
  `sections.yaml` changes, run `src/scripts/curriculum-graph.rb` on the host
  (about 3 minutes, in the background) and commit
  `src/ruby/include/curriculum-graph.json` with the change.

## Working on the code

- Never touch the backups: no borgmatic, no `docker system prune`, no
  cleanup on the machine without asking first; production dumps never go
  into the repository.
- Run renders, Playwright and anything longer than a minute in the
  background and keep working. Run scripts you write (generators,
  searches, simulations) as `(ulimit -v 4000000; timeout 120 …)`, give every
  loop a bound, and run every heavy command (Playwright, task examples,
  compiling) in a scope of its own with a memory limit
  (`systemd-run --user --scope -q -p MemoryMax=6G …`), so that a runaway
  job is killed alone. At most three agents that execute code at once. The
  machine froze three times before (an unbounded loop; three agents running
  C checks beside a Playwright run); since then it has an early OOM killer
  (earlyoom at 5 % free memory) and compressed swap in RAM (zram).
- The development site serves the branch that is checked out, and the
  maintainer reviews it live: stack new work on the newest branch instead of
  checking out an older one.
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
- No real names of people in commits, docs, tests or plans: name the role;
  real names and addresses belong in `credentials.rb` only.
- Releases are tags `vX.Y.Z` on `main` (Semantic Versioning as in the
  CHANGELOG header), made when the maintainer asks: »Unreleased« becomes the
  version's section, a fresh »Unreleased« goes on top.
- Other repositories (starter templates such as `specht/raylib-starter`, the
  2D Game Studio) get a patch for `git am`, checked with `git apply --check`;
  never commit or push there directly. Exceptions the maintainer allowed:
  `specht/routenplaner-starter`, `specht/backrooms-starter` and the datasets
  in `specht/workspace-files` (`src/scripts/publish-datasets.rb … --push`)
  are committed and pushed directly.
