# AGENTS.md — Hackschule Workspace

A browser-based development environment for computer science lessons:
students get VS Code (code-server) in a container, plus a website with
activities, a Handbuch and tasks the Workspace checks itself. Content and
user-facing text are German; code and these notes are English.

## Where things are

| Path | What |
| --- | --- |
| `src/content/<slug>/<slug>.md` | an activity: page, `tasks.yaml`, `lehrkraft.md` (teacher note), pictures, example files |
| `src/content/reference/<slug>/` | a Handbuch page; parts marked `<!-- part: name -->` … `<!-- /part -->`, included with `_include(slug#part)` |
| `src/content/sections.yaml`, `reference/sections.yaml` | the catalogs |
| `src/ruby/main.rb`, `src/ruby/include/` | the Sinatra app; content is read on every request in development |
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
- [src/tutorial-screenshots/README.md](src/tutorial-screenshots/README.md): the recipe language for generated pictures
- [CHANGELOG.md](CHANGELOG.md): every notable change goes under »Unreleased«
- [TODO.md](TODO.md): direction and open work; delete what is done instead of ticking it off

## Rules for content

- Names in code are English (variables, functions, constants, files, routes);
  comments and program output may be German. In German prose, technical terms
  developers say in English stay English (Handshake, Lock, String, Header).
- Every picture has an `alt` text that says what is to be seen.
- Screenshots of the Workspace come from recipes (`<!-- tutorial-screenshot -->`
  before the `<img>`), never by hand. Render with
  `src/scripts/recreate-screenshots.rb PAGE`, check `--status`, and look at
  every new picture before committing it with its `.tutorial-screenshots.json`.
- Check marks are Bootstrap icons (`<i class='bi bi-check-lg'></i>`), never
  Unicode characters; `no_check_marks_test.rb` enforces this.
- Code blocks name their language. A copy button (`_copy`) only for what is no
  use to type and for big blocks (about 15 lines and more) that go into a file.
- Formulas: KaTeX renders `\(…\)` and `$$…$$`, not single `$…$`.
- In HTML tables inside Markdown, `</table>` and `</div>` go on lines of their
  own, otherwise the rest of the page renders as raw text.

## Working on the code

- Run the Ruby tests (`cd src/ruby; for f in test/*_test.rb; do ruby "$f" | tail -1; done`)
  and, for the generator, `node --test src/tutorial-screenshots/test/*.test.js`.
- After a change to `src/tutorial-screenshots/server.js`, rebuild the service:
  `./config.rb build tutorial_screenshots && ./config.rb up -d tutorial_screenshots`.
- Add a CHANGELOG entry for every notable change, in German, saying what
  changes for students, teachers and operators.

## Git

- Work on a branch; commit in coherent pieces. Merge into `main` and push only
  when the maintainer asks for it.
- Other repositories (starter templates such as `specht/raylib-starter`, the
  2D Game Studio) get a patch for `git am`, checked with `git apply --check`;
  never commit or push there directly.
