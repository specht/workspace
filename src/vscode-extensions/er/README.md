# ER-Werkstatt

ER models as text (`.er`), their translation into tables, MySQL and diagrams
(Chen and crow's foot), as the VS Code extension `hackschule-er` in the
Workspace image.

In VS Code, a `.er` file gets:

- highlighting, and problems underlined while typing (German messages, also in
  »Probleme«);
- »ER-Diagramm anzeigen« (button in the editor title, <kbd>Ctrl</kbd>+<kbd>K</kbd>
  <kbd>V</kbd>): a preview beside the editor with Chen, Krähenfuß and Tabellen,
  one line per entity type (Chen, Krähenfuß) or table (Tabellen) with the key
  underlined and ↑ for a foreign key, »Kopieren« for the lines as text
  (the key underlined with U+0332), and »So wurde übersetzt«. Zoom with −, + and
  »Einpassen« or Ctrl + mouse wheel; typing never rescales the view, and
  boxes stay where they are while you type. Krähenfuß and Tabellen are laid
  out in layers, as Mermaid draws them (what is referenced above, join tables
  below, few crossings, curves square to the box edges); Chen keeps a layout
  of its own. Boxes can be dragged; their
  positions are kept in the hidden `.film.er.layout.json` next to the model
  (delete it, or press »Neu anordnen«, for a fresh layout);
- »SQL erzeugen«: writes `film.sql` next to the model. A file the ER-Werkstatt
  did not write is only replaced after asking. Students run it themselves:
  `mysql < film.sql`;
- »Diagramm als SVG speichern«: `film-chen.svg`, `film-crowfoot.svg` or
  `film-tables.svg`.

Build: `npm run package` runs the tests and writes `hackschule-er-<version>.vsix`
with `pack.cjs` (no npm packages or network needed). `build-image.sh` copies
it into the image; `svc-code-server-run` installs it.

`core/er-core.js` is one plain JavaScript module without dependencies and
without a DOM. The extension, the website and the Workspace checks all use it,
so a model means the same everywhere.

```
# comments start with #
entity movie
  id: int, primary key
  title: text
  german_title: text, optional

entity genre
  id: int, primary key
  name: text, unique

relationship has_genre: movie n:m genre              # table movie_genre
relationship credit: movie, crew, job as movie_crew  # three-way, own table
relationship loan: customer n:m copy                 # attributes of a relationship
  borrowed: date
```

- Keywords and identifiers are English and lower case; messages for students
  are German.
- Types: `int`, `text` (VARCHAR(255)), `varchar(n)`, `decimal(p,s)`, `float`,
  `date`, `datetime`, `bool`. Flags: `primary key`, `optional`, `unique`
  (`key` still works and gets a warning suggesting `primary key`).
- Cardinalities: `1:1`, `1:n`, `n:1`, `n:m`. A three-way relationship lists its
  entities with commas. Indented lines under a relationship are its attributes;
  they end up in the table that holds the relationship.
- n:m and three-way relationships get their own table, named after the
  entities (`movie_genre`) unless `as` names it. A relationship of an entity
  with itself uses its own name.
- The generated SQL starts with `DROP TABLE IF EXISTS` (foreign-key checks off
  while dropping), so a changed model is built again with one command.

Command line, used by the checks and for exports:

```bash
node core/er-core.js --json film.er                      # model, tables, problems
node core/er-core.js --sql film.er > film.sql
node core/er-core.js --svg chen|crowfoot|tables film.er > film.svg
```

Tests: `npm test` in this folder.
