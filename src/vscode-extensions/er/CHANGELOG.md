# Changelog

## 0.3.0

- Krähenfuß and Tabellen in layers, as Mermaid draws them: a table that
  others point to sits above them, join tables at the bottom; the order in
  each layer keeps crossings few, and a line over several layers goes round
  the boxes in between. Lines are curves that leave a box at a right angle,
  through a crow's foot or bar that sits square on the edge; several lines
  on one side get places of their own (two foreign keys to the same table
  are two lines). Kept positions win; a new box moves aside instead of
  landing on one. Chen keeps its own layout.
- Boxes in three columns, type | name | key, every other row shaded.
- A relationship with itself: a wider loop whose crow's foot and bar each
  have room, with room for it in the layout.
- Chen: a box or diamond moves off a line that would run through it, and a
  cardinality that would sit on another moves further out along its line.
- Under the diagram one line per entity type (Chen, Krähenfuß) or per table
  (Tabellen); »Kopieren« puts them on the clipboard as text. On the command
  line: `--schema entities|tables`.

## 0.2.0

- Generated SQL names the right version (the core still said 0.1.0).
- The diagram's styles apply only to the diagram (`.er-diagram`), so an SVG
  embedded in a page no longer restyles the page.
- `primary key` marks the key, as in SQL; `key` alone still works and gets a
  warning suggesting `primary key`.

- Chen: attributes are spread evenly around their entity, in the gaps
  between its relationship lines, in the order they are written (clockwise
  from the top). The two cardinalities of a relationship with itself no
  longer overlap.
- Preview: zoom with −, + and »Einpassen« or Ctrl + mouse wheel. Typing and
  dragging never rescale or move the view; boxes stay where they are while
  you type, only new ones are placed (»Neu anordnen« lays everything out
  again).

## 0.1.0

- `.er` files: highlighting, problems while typing (German), preview with Chen, crow's foot and tables, "SQL erzeugen" and "Diagramm als SVG speichern".
