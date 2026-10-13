# ER-Werkstatt

ER models as text (`.er`), their translation into tables, MySQL and diagrams
(Chen and crow's foot). This folder will hold the VS Code extension
`hackschule-er`; for now it holds its core.

`core/er-core.js` is one plain JavaScript module without dependencies and
without a DOM. The extension, the website and the Workspace checks all use it,
so a model means the same everywhere.

```
# comments start with #
entity movie
  id: int, key
  title: text
  rating: decimal(3,1), optional

entity genre
  id: int, key
  name: text, unique

relationship has_genre: movie n:m genre              # table movie_genre
relationship credit: movie, crew, job as movie_crew  # three-way, own table
```

- Keywords and identifiers are English and lower case; messages for students
  are German.
- Types: `int`, `text` (VARCHAR(255)), `varchar(n)`, `decimal(p,s)`, `float`,
  `date`, `datetime`, `bool`. Flags: `key`, `optional`, `unique`.
- Cardinalities: `1:1`, `1:n`, `n:1`, `n:m`. A three-way relationship lists its
  entities with commas.
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

Tests: `node --test src/vscode-extensions/er/core/`.
