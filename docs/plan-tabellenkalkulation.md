# Spreadsheets: a second series of Office worksheets

The second part of the section Office (docs/plan-textverarbeitung.md): seven
worksheets on spreadsheets, in LibreOffice Calc, Excel or Nextcloud Office,
written on 12 November 2026 after the word processing series, with all its
requirements (»Requirements for every series of the section« there): 45
minutes a sheet with everything, hints per program from »what« to »where
exactly«, illustrations where a page explains something visual, a
demonstration in every teacher note, the menus' names from the programs'
translations, predictions whose answer is the same in every program, history
and people where they belong, fixed or downloaded dates, half a Full HD
screen, nothing limited per address.

## The story

The 7b's class trip again, now its money: what the trip costs, what each
family pays, who has paid, what the class thought of it. Each sheet starts
from a table someone made in a hurry, the way people do without knowing the
program: results worked out on a calculator and typed, amounts typed with
their € as text, a formula copied down that breaks, a list sorted by one
column alone.

## The worksheets

1. **Zellen, die rechnen** (`spreadsheet-cells`): the trip's costs with
   every result typed. Cells and addresses, formulas instead of results,
   filling down, SUMME. Prediction: a price changes, what happens to the sum.
2. **Zahlen in Form** (`number-formats`): amounts typed as »12,50 €«, so
   SUMME leaves them out. Number formats: currency, percent, date, decimals.
3. **Feste Bezüge** (`absolute-references`): each family's share and a
   discount from one cell; copying a formula down without $ breaks it.
4. **Auswerten** (`spreadsheet-functions`): the class's survey after the
   trip: MITTELWERT, MIN, MAX, ANZAHL, and WENN.
5. **Sortieren, Filtern, Hervorheben** (`sort-and-filter`): the list of
   payments sorted by one column alone (the names no longer fit the
   amounts), a filter, conditional formatting for what is still open.
6. **Diagramme** (`spreadsheet-charts`): which chart answers which
   question; titles, axis titles, the right range.
7. **Die Abrechnung der Klassenfahrt** (`trip-accounts`): the hand-in: a
   table of one's own with every point of the series, exported as PDF.

## How the check reads a spreadsheet

The office service (`src/office/sheet.py`) opens the workbook in Calc:
every cell with its value, formula (English function names, as the file
stores them; the messages translate them), the text as shown, its number
format, bold, alignment and colour; the sheet's column widths and rows
(hidden ones too, which a filter hides), conditional formats, autofilters
and charts (type, title, axis titles, ranges, where they stand). Each sheet
is drawn as an SVG grid with column letters and row numbers, the charts as
pictures where they stand (PNG: Calc exports a chart's shape as an empty
SVG), and the marks of the check stand on cells, so the card and the
hand-in work as for text documents.

**Does it calculate?** The rule `recalc` sends probes with the upload: the
service changes input cells in its copy, recalculates and reads the
results, then puts the cells back. A table whose results are typed shows
the old numbers. This is the check of code (run it with other input) for a
spreadsheet.

Rules (docs/aufgaben.md, »document«): `formulas`, `values`, `recalc`,
`functions`, `absolute`, `same_data`, `number_format`, `text_numbers`,
`bold_cells`, `sorted`, `autofilter`, `conditional`, `chart`, `no_errors`.

## Decisions

- **The same three programs**, the same choice: the key `word` stands for
  Microsoft's suite; on a page with `office: sheet` the switch names it
  Excel and shows the spreadsheet icons (`CodeLanguages::KIND_NAMES`).
- **Files as .xlsx**, which every program opens and saves; .ods works too.
- **The office image** needed `libreoffice-calc-nogui`.

## Open

- Try a file saved by Excel (number formats, charts) once a class uses it.
- Nextcloud Office: its menus as far as the pages name them are LibreOffice's;
  to check in the program.
