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
  (`pdftocairo -svg`). The service stores nothing. On a network of its own with the
  backend only, without the internet; one document at a time, killed after
  40 seconds. Fonts: Microsoft's core fonts (installed while the image is
  built), Carlito and Caladea for Calibri and Cambria (same widths).
- **The check type `document`** (`DocumentCheck`, docs/aufgaben.md): rules
  for text and spacing, type, lists and tabs, tables, pictures. Each
  is a point of the checklist; its places are marks on the page, in points.
  Examples in `task-examples.yaml` with `document:`.
- **The card** (`document_task.js`): the starting file as a plain download
  (in the browser's downloads, opened from there). In Chrome and Edge on
  https the document chosen once (the dialog opens in Downloads) or dropped
  is kept; while the page is visible it looks at the file every two seconds
  and checks it after each save, and the line under the button says which
  file it watches and when it was saved last; »Erneut prüfen« on a file
  unchanged since its last check says so (LibreOffice's question about the
  format, a second file) instead of checking again. Firefox and Safari
  choose or drop the file for each check; the card says so and why Chrome or
  Edge shorten the loop. Saving the starting file through the page
  (`showSaveFilePicker`) was dropped on 12 November 2026: Chrome does not
  count such a file as a download, so it was not in the downloads to open,
  and LibreOffice could easily open another copy than the one the page
  watched.
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
  full width and a phone with full screen), the examples, `task_examples.rb`.
- **The series in the class overview** (`class_series_html`): each worksheet
  is an activity of its own, pinned on its own week by week; »Reihe: …« in
  the list of activities shows the whole series, a row per student, a column
  per worksheet, each cell how many tasks are solved and, in its title, what
  a document check left open.

## Decisions

- **No Aptos** (the maintainer, 12 November 2026): Word's default font since
  2024 is not installed; Microsoft wants a licence of its own for using it on
  a server. The starting files set Calibri, whose widths Carlito has. A
  document begun in a new Word is drawn with another font and may break
  its lines elsewhere; the checks do not depend on that.

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
- **The last checked version is kept** (the maintainer, 12 November 2026),
  as the person's hand-in of the task: to go on with at home or on another
  computer (»Deine letzte Fassung«), for the group's teacher to download;
  replaced by the next check, gone with »Ergebnis löschen« and the account
  (`privacy.rb`). The hand-in at the end of the series is that version.

## Requirements for every series of the section (the maintainer)

These hold for word processing and for the series still to come
(spreadsheets, presentations):

- **45 minutes a sheet, everything included** (12 November 2026): reading,
  predictions, trying, the check. A sheet that is shorter gets more to do,
  not a second sheet.
- **Help where the program is hard to find one's way in:** every step says
  where the function is, for the program the student chose (Word,
  LibreOffice, Nextcloud Office), and the hints go from »what« to »where
  exactly«. Students get stuck in the program's menus more than in the idea.
- **Illustrations where a page explains something visual** (the site's
  rule; the maintainer named serifs on »Schrift mit Absicht«): a drawing of
  its own, as SVG, not only a sentence.
- **The teacher note suggests a demonstration** of the sheet's most important
  functions before the class starts, at the projector, in the program the
  class uses; its ways and shortcuts are those of the program chosen at the
  top of the page (`<!-- language: word -->` and so on in `lehrkraft.md`,
  as on the page).
- **Exporting as PDF belongs to the curriculum:** sheet 8 ends with the
  article exported as PDF, a check of its own (`accept: pdf`: a PDF, its
  fonts embedded, the article's words), read by poppler.
- **Grading:** the last checked version is the hand-in; the teacher sees it
  with its marked pages on the class page (Ansehen, Herunterladen). How a
  grade comes out of it is still being decided (see »Open«).
- **The size for which everything is made:** half a Full HD screen (960 px
  wide, Word beside the browser); full width and a phone must work too.
- **Dates in starting files** without year; a date that stands for today or
  for something soon is filled in on download (`dates:`, docs/aufgaben.md
  »document«; the maintainer, 12 November 2026, after first deciding on
  fixed dates). The pictures on the pages keep the dates of the file in
  `src/static`.
- **Predictions whose answer is the same in every program:** a question about
  what happens in Word may have another answer in LibreOffice (applying a
  style to a paragraph formatted by hand); such a prediction is dropped.
- **Nothing limited per IP address:** a class at school shares one (the
  Telefonbuch's line had one call per address; it would have thwarted lessons).

## Findings of the word processing series

- **The loop:** the starting file as a plain download (in the browser's
  downloads, opened from there); chosen once with »Prüfen«, Chrome and Edge
  keep it (on https only) and check after each save. The card says which file
  it watches and when it was saved; »Erneut prüfen« on an unchanged file says
  so. What went wrong in the first lesson-like test: LibreOffice saved into
  another file (its question about the format, or a second copy in the
  downloads), and the page watched the first one. School computers keep the
  downloads on a network drive (H:\), where reading takes a moment.
- **The viewer:** the wheel scrolls the website, Ctrl and the wheel zoom (one
  target, eased); fits with names; full screen keeps new results; the busy
  state on the »prüfen« button beside the page; ragged-right messages; the
  pages by address, not inside the answer.
- **What the check must allow:** Word's Ctrl+Enter leaves an empty paragraph
  before a page break; LibreOffice counts in cm (0.2 cm are 5.7 pt, so
  gaps from 90 %); a caption may be in a frame (LibreOffice) or a text box
  (Word); a page break in the style »Überschrift 1« also moves Word's
  heading of the table of contents.
- **Generating documents:** python-docx for what students start from, UNO in
  LibreOffice for what the program itself builds (fields, frames, indexes):
  a number range's master needs its name before its type, removing a
  paragraph joins the next one into it, a killed LibreOffice leaves lock
  files. Pictures for the pages as SVG from LibreOffice's PDF
  (`src/office/page_picture.py`).

## The series (worksheets)

The thread: the class trip of the 7b to the Kranichsee. Each worksheet
starts from a file »typed in a hurry« that looks right when printed and is
built by hand; each is a page of a series of its own, about one lesson.

1. **Was das Auge nicht sieht** (built): formatting marks, alignment,
   spacing instead of empty paragraphs, line breaks.
2. **Schrift mit Absicht** (built, `character-formatting`): the 7b's flyer
   for a cake sale in seven fonts; two fonts, the text in 11 to 12 pt, bold
   only on date, time and place, nothing underlined, no capitals as format;
   deleting the formatting as a prediction.
3. **Listen und Tabstopps** (built, `lists-and-tabs`): a packing list with
   »- « typed, the costs lined up with spaces counted for Calibri 12 pt (they
   break in any other font: the prediction); a real list, one tab per line, a
   right-aligned tab stop with dot leaders.
4. **Tabellen** (built, `word-tables`): the trip's programme at two tab
   stops (a longer entry starts again at the left margin: the prediction);
   text into a table, three columns, a header row repeated on every page;
   the service reports tables (rows, columns, header row).
5. **Bilder im Text** (built, `pictures-in-text`): an article about the
   cranes for the trip newspaper; the photo stretched to the width of the
   text, captions typed, no alt text (the map's guessed by Word). The
   picture back in its own proportions, text beside it, captions with a
   numbering field, alt texts; moving the map up as the prediction. The
   service reports pictures (alt text, size and pixels, anchor, wrapping,
   the frame or Word text box they are in, where they landed through
   `pdftohtml -xml`) and the paragraphs in frames and text boxes. The photo
   is from Wikimedia Commons (CC BY-SA 4.0, credited in its caption); the
   map is drawn (`bilder/karte.py`). The solution is built by LibreOffice
   through UNO (a frame with picture and caption, saved as a Word text box).
6. **Formatvorlagen** (built, `paragraph-styles`, 45 minutes): the 7b's
   trip diary with nine headings formatted by hand (bold, larger, blue,
   Wednesday a point smaller); the prediction: how many entries the
   program's list of headings shows (none); the days in »Überschrift 1«,
   their parts in »Überschrift 2«, nothing by hand on them, »Überschrift 1«
   changed once. The rules headings, levels, no_direct, style_changed; the
   service reports the definitions of the styles used. A prediction about a
   style turned green and a heading blue by hand was dropped: whether
   applying a style keeps formatting by hand may differ between Word and
   LibreOffice.
7. **Seiten und Verzeichnisse** (built, `pages-and-contents`): the travel
   report, six pages: each day pushed onto its page with empty paragraphs
   (the generator presses Enter until the heading jumps, through the
   service's layout), a table of contents typed with dots, »Seite 1« typed
   in the footer. Page breaks, the page number as a field, a table of
   contents from the headings. The service reports indexes (updated in
   memory after the pages are drawn, to tell whether the student's was up
   to date), headers and footers with their fields and where the footer is
   on each page; the viewer shows the page in view and goes from page to
   page. Found on the way: a page break in the style »Überschrift 1« also
   moves Word's heading of the table of contents, which is based on it; the
   solution sets the break on each day.
8. **The hand-in:** an article of one's own for the trip newspaper, every
   earlier point checked at once, the file handed in for the teacher to grade
   (stored like a `submit`, viewed with its marked pages on the class page).

## Open

- **Grading** (the maintainer asked how; to decide): see the proposal in the
  conversation of 12 November 2026, summed up here once decided.
- **The curriculum graph:** a series (the eight sheets, the Backrooms levels)
  as one node (the maintainer, 12 November 2026); until then the stored
  layout lacks sheet 8 (production orders quickly, the graph test is red).
- **Every sheet to 45 minutes**, »Formatvorlagen« first.
- **Illustrations where the sheets only explain** (the maintainer, 12 November
  2026; the site's rule): every sheet looked through for passages that say
  in words what a drawing would show. Named so far: the serifs on »Schrift
  mit Absicht«; on »Seiten und Verzeichnisse« the table of contents (how the
  headings with their styles and their pages become its lines, the dots to
  the page number), the page break against empty paragraphs, the footer
  that is the same on every page with the field that is not. The finished
  title page after the check does not count: it comes after the
  explanation.
- **Hints per program on every task** (the maintainer, 12 November 2026):
  sheet 1's document task has them (`word:`, `libreoffice:`, `nextcloud:`
  under the task, each with its own `hint:` list, from »what« to »where
  exactly«); every other task of sheets 1 to 8 still has only the hints for
  all programs. Menu names from the programs' German translations (for
  LibreOffice the `.po` files of its translations), not from memory.

- **For worksheet 8:** the page of each place in the list (the viewer
  shows the page in view and goes to a place's page already).
- **Keeping hold of the file needs https:** Chrome and Edge offer the File
  System Access API only in a secure context (https or localhost). On the
  development site (`http://workspace.test:8030`) Chrome chooses the file
  again for each check, like Firefox, unless the address is entered at
  `chrome://flags/#unsafely-treat-insecure-origin-as-secure` (as the
  Playwright tests start Chrome). The tests hand files to the input, without
  a handle, so the automatic check after saving has not run in a test; try
  it on the https site or with that flag.
- **Try on a school PC:**
  - whether the page sees the file after Word saved it (Word writes a new
    file and swaps it in), and whether it can read it while Word has it open;
  - whether a page reload keeps the file (the handle in IndexedDB, with the
    browser asking again).
- **Try a picture from Word:** a picture with »Quadrat« and a caption Word
  inserted (Word puts it in a text box of its own). The check finds a
  caption by where it stands, so it should pass; built by hand for the
  examples, LibreOffice laid such a text box elsewhere than Word would, so
  that case has no example yet.
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
