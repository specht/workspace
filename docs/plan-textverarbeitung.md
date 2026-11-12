# Office: Textverarbeitung, Tabellen, Präsentationen

The section Office (`sections.yaml`, key `office`) teaches word processing,
later spreadsheets and presentations, in the program the school has: Word,
LibreOffice or Nextcloud Office. Students work in that program; the website
checks the file and shows them on their own page what is still missing.
Started on 12 November 2026 for an ITG class in year 7, where worksheets
handed out on paper got no feedback because nobody could check them all.

## What exists

- **The service `office`** (`docker/office`, `src/office/server.py`,
  `config.rb`): LibreOffice without a window, Python with UNO, poppler. One
  LibreOffice runs the whole time; each document is loaded with macros off
  and links not updated, read, given a bookmark at the start and end of every
  paragraph in memory, exported as PDF and closed. The named destinations of
  the PDF (`pdfinfo -dests`) say where each paragraph landed, the words of
  `pdftotext -bbox` where each word is; each page comes back as SVG
  (`pdftocairo -svg`). Nothing is stored. On a network of its own with the
  backend only, without the internet; one document at a time, killed after
  40 seconds. Fonts: Microsoft's core fonts (installed while the image is
  built), Carlito and Caladea for Calibri and Cambria (same widths).
- **The check type `document`** (`DocumentCheck`, docs/aufgaben.md): rules
  `text`, `spaces`, `empty_paragraphs`, `line_breaks`, `align`, `gaps`. Each
  is a point of the checklist; its places are marks on the page, in points.
  Examples in `task-examples.yaml` with `document:`.
- **The card** (`document_task.js`): the starting file, saved through the
  page in Chrome and Edge (`showSaveFilePicker`), so the page keeps hold of
  it; a document chosen or dropped is kept the same way. While the page is
  visible it looks at the file every two seconds and checks it after each
  save. Firefox and Safari choose or drop the file for each check; the card
  says so and why Chrome or Edge shorten the loop.
- **The result:** the checklist beside the page, both in one window, at half
  a desktop (the size this is made for: Word on one half, the page on the
  other) the page above the list. One number per open point, in the left
  margin beside its lines; on the page the formatting marks in orange (·, ¶,
  ↵). Pointing at a point brings out its places; a click on its number
  travels to the next place. Drag, wheel, pinch, buttons and keys zoom and
  move; the view stays where it was from one check to the next. Points met
  since the last check say »neu erfüllt«; when all are met, the points tick
  one after the other, a check mark draws itself, a stamp lands on the page.
- **Office programs as a choice** (`CodeLanguages::OFFICE`): a page offers
  `languages: [word, libreoffice, nextcloud]` and marks what differs with
  `<!-- language: word -->`. Students switch on the page (`u.office_suite`),
  teachers rank them for a class (»Office-Programme der Klasse«,
  `g.office_suites`), apart from the programming languages.
- **Worksheet 1, »Was das Auge nicht sieht«** (`formatting-marks`, in
  development): an invitation to a parents' evening, centred with spaces,
  gaps of empty paragraphs, lines broken by hand. A prediction (font one
  step larger), formatting marks, alignment, spacing, search and replace,
  the check. `dokumente.py` makes the starting file, the solution and the
  pictures (`src/scripts/office`).
- **Tests:** `e2e/tests/document-task.browser.spec.ts` (at 960 × 1040, then
  full width and a phone), the examples, `task_examples.rb`.

## Decisions

- **Upload, not Office in the browser.** Students learn the program they
  will meet at school and at home; the website only checks. Nextcloud Office
  through WebDAV (no upload at all) and a Word add-in (a pane beside the
  document, checking what is on screen) are later ways in to the same
  check.
- **What is checked is how the document is built, not how it looks.** The
  point of the series: two printouts can be the same while one breaks as
  soon as anything changes.
- **LibreOffice draws the page.** A file from Word may break a line one word
  earlier; the card says so. The check does not depend on it.
- **Vector pages.** SVG keeps the page sharp at any zoom; it is shown as
  `<img>`, so nothing in a student's document runs.
- **Nothing of the document is stored**, only the result and a line for the
  class overview (`privacy.rb`). The hand-in at the end of the series will
  store the file and needs its own line there.

## The series (worksheets)

The thread: the class trip of the 7b to the Kranichsee. Each worksheet
starts from a file »typed in a hurry« that looks right when printed and is
built by hand; each is a page of a series of its own, about one lesson.

1. **Was das Auge nicht sieht** (built): formatting marks, alignment,
   spacing instead of empty paragraphs, line breaks.
2. **Schrift mit Absicht:** at most two fonts, body 11 to 12 pt, emphasis on
   date, time and place, not bold, underline and capitals at once.
3. **Listen und Tabstopps:** a packing list with »- «, a price list lined up
   with spaces; real lists, a right-aligned tab stop with dot leaders.
4. **Tabellen:** the trip's schedule in tabs; a table with a header row.
5. **Bilder im Text:** text wrapping, a caption with a numbering field, an
   alt text that is not the file name.
6. **Formatvorlagen** (two lessons): ten headings formatted by hand; heading
   styles, the style changed once, no level skipped.
7. **Seiten und Verzeichnisse:** page number as a field in the footer, page
   breaks, an automatic table of contents that is up to date.
8. **The hand-in:** an article of one's own for the trip newspaper, every
   earlier point checked at once, the file handed in for the teacher to grade
   (stored like a `submit`, viewed with its marked pages on the class page).

## Open

- **For worksheets 6 to 8:**
  - **Several pages in the viewer:** a page display, jumping between pages,
    the page of each place in the list.
  - **Reading more of the document:** headers and footers (with their
    fields), the table of contents through UNO (`DocumentIndexes`, compared
    with the headings), styles and their changes, tables, pictures with alt
    text and wrapping.
- **Try on a school PC:**
  - whether the page sees the file after Word saved it (Word writes a new
    file and swaps it in), and whether it can read it while Word has it open;
  - whether a page reload keeps the file (the handle in IndexedDB, with the
    browser asking again).
- **Aptos** (Word's default since 2024): Microsoft offers it for download but
  wants a licence of its own for using it on a server. Without it, a
  document begun in a new Word breaks lines elsewhere on the drawn page. The
  starting files set Calibri. The maintainer decides.
- **Pictures of the program** (where a button is): Collabora Online in a
  container, driven by the screenshot generator with Playwright, for
  LibreOffice and Nextcloud Office; Word by hand, a few per worksheet. Not
  tried yet.
- **Spreadsheets:** the same service with Calc; drawn as a grid from the
  cells (value, formula, format), marks on the cells, charts as pictures.
  The check changes an input cell, recalculates and compares, the way the
  function checks run code.
- **Presentations:** Impress, one SVG per slide, marks on the shapes
  (placeholders, text running over, pictures without alt text, the master).
- **The Word add-in:** the worksheet in a pane beside the document, checking
  what is on screen without saving; needs the school's IT to install it.
- **Card picture and title** of worksheet 1, and the word »Office« as the
  section's name: the maintainer decides.
