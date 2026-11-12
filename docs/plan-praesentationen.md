# Presentations: a third series of Office worksheets

**Shelved** (the maintainer, 12 November 2026), after sheet 1: word processing
and spreadsheets are the start. Sheet 1 (`good-slides`) is finished and
in the repository, listed nowhere (taken off the development site's front
page too); to show it on the development site again, add `.good-slides` to
the Office section in sections.yaml. The
office service, the check's rules for slides and the card's handling of
.pptx and .odp are in place for the sheets to come.

The third part of the section Office (docs/plan-textverarbeitung.md, with all
its »Requirements for every series of the section«): worksheets on
presentations in LibreOffice Impress, PowerPoint or Nextcloud Office, and on
what makes a presentation good. Written on 12 November 2026; the curriculum
below is for the maintainer to look at before the sheets are written.

## What the series teaches

Two things side by side, on every sheet:

- **The program:** layouts and placeholders, the outline, the master,
  pictures and charts, notes and the presenter view, PDF.
- **A good presentation:** the slide supports the talk, it does not replace
  it. One idea per slide, a title that says it, few words in large type, a
  picture instead of a paragraph, honest charts, sources named, a beginning
  that makes people listen and an end that says what stays. Each rule has
  its reason on the page (reading and listening at once does not work; the
  back row; a projector in a bright room), and the check sees what can be
  seen in the file.

The thread: a Referat. Someone in the 7b put together a Referat about the
cranes of the Kranichsee in a hurry, the way people do without knowing
better: sentences copied from the trip newspaper's article onto the slides,
text boxes everywhere, every slide coloured by hand. Each sheet improves one
side of it; the last one is the student's own short talk.

## The worksheets (45 minutes each)

1. **Eine Folie, ein Gedanke** (`good-slides`): the Referat as walls of
   text, 18 pt, eighty words a slide. What a slide is for. Predictions: how
   long it takes to read one such slide (a timer on the page), and what the
   audience does meanwhile (reads, and stops listening). The sentences move
   into the notes, the slide keeps keywords; a title that states the
   message (»Kraniche ziehen im Herbst nach Spanien« instead of »Zug«);
   24 pt at least. Check: every slide a title, at most 25 words a slide,
   no letters under 24 pt, nothing running over, the sentences in the
   notes. The Columbia slide of 2003 was taken out again (the maintainer,
   12 November 2026: a tragic accident, and a confusing example for the
   class); a lighter story in its place, if one is found.
2. **Layouts und Platzhalter** (`slide-layouts`): the Referat built from
   text boxes put on by hand. Prediction: how many titles the outline shows
   (none). Layouts (title slide, title and content, two contents, title
   only), placeholders, the outline and the slide sorter, a new slide from
   a layout, reordering. Check: titles in the title placeholders, no text
   outside placeholders, the first slide a title slide. History: why it is
   called a Folie (overhead transparencies); PowerPoint 1987, first called
   Presenter.
3. **Der Folienmaster** (`slide-master`): every slide coloured and set in
   its own font by hand, one in yellow on white. Prediction: the master's
   title font changed, which slides follow (only those not formatted by
   hand; the same in both programs). Contrast on a projector in a bright
   room (a widget washes the colours out as a projector does), at most two
   fonts, slide numbers. Check: nothing formatted by hand on the slides,
   contrast 4.5 : 1 at least, at most two fonts, slide numbers on, the
   master changed.
4. **Bilder, Diagramme, Quellen** (`slide-pictures`): a photo stretched, a
   table of numbers read aloud, a picture from the internet without a
   source. A picture large instead of a paragraph, proportions and
   cropping, the chart from the spreadsheet series instead of the table
   (»Diagramme«), whose picture may be used: licences (CC BY, CC BY-SA),
   what a credit line names, alt text. Check: no picture squeezed, alt
   texts, a credit for each photo, a chart. Person box: Hans Rosling
   (charts that move, TED 2006). This sheet covers the curriculum's
   Urheberrecht gap for Sek I in part (docs/plan-lehrplan-luecken.md).
5. **Vortragen** (`presenting`): notes for every slide, the presenter view,
   rehearsing with a timer (about one slide a minute), animation only where
   it shows an order (bullet by bullet), no flying effects, transitions
   none or one. A beginning that makes people listen (a question, a
   picture) and an end with what stays instead of »Danke für die
   Aufmerksamkeit«. Prediction: how long the Referat takes when read
   aloud. Check: notes on every slide, only appearing or fading in, one
   kind of transition at most, the last slide not »Danke«/»Fragen?«.
   History: PechaKucha (Tokyo 2003, 20 slides of 20 seconds).
6. **Dein Kurzvortrag** (`short-talk`): the hand-in, and taking the
   presentation elsewhere (the maintainer, 12 November 2026): a prediction
   (your presentation on a computer without your font: another font stands
   in, lines break elsewhere, text runs over its box); fonts embedded in
   the file (PowerPoint's save options, LibreOffice's document properties;
   a font's licence may forbid it); exporting: a PDF embeds every font and
   looks the same everywhere, but has no animations and no notes, a slide
   show file opens straight into the show; the check of embedded PDF fonts
   (`pdf_fonts`) exists. A link to »Eine Präsentation in HTML erstellen«
   (`showerjs`): slides as a web page, in any browser. The hand-in itself: a topic of one's own,
   5 to 8 slides for five minutes, every earlier rule checked at once, then
   exported as PDF for a handout. The talk itself is given in class; the
   teacher note brings a feedback sheet for it (what the check cannot see:
   speaking freely, eye contact, time).

The series gets one card (»Präsentieren Schritt für Schritt«), as the other
two.

## Notes are a cue card (the maintainer, 12 November 2026)

Students are not to read their talk from the notes. The sentences leave the
slide and are told freely; the notes keep a few cues of what to tell. The
check on sheet 1: the Referat's facts are still there, on a slide or in its
notes (`facts`, in any words), and no slide has more than 30 words of notes
(`notes_words`); the sentences moved whole into the notes fail as a text to
read out.

## A good slide has a picture (the maintainer, 12 November 2026)

The model of a good slide in this series is a picture with minimal text;
keywords alone are a step on the way (sheet 1 takes the text off, sheet 4
adds the pictures). The card pictures of the sheets show such a slide,
front and centre with a slight perspective, thrown onto a wall by a beamer
hanging from the ceiling outside the picture, its light a frustum with four
faces (`good-slides/bilder/teaser.py` renders the scene through a camera;
camera and lens set by EYE, TARGET, FOCAL and LENS). Sheet 1's slide is
drawn there (cranes in a wedge over the lake at dusk), free of licences.

## Accessibility (the maintainer, 12 November 2026)

Everywhere it fits: information never by colour alone (about one boy in
twelve cannot tell red from green), titles in placeholders (a screen reader
and the outline find them), alt texts, contrast, large type, a sensible
reading order. Sheet 3 has a slide that marks something important by colour
only, to be told by a word, a symbol or a place as well; sheet 4 labels the
lines and bars of a chart directly instead of a legend in colours. The
information of such an example is real and matters, never made up for the
exercise: numbers about the cranes from a source that can be cited (how
many pairs breed in Germany over the decades, NABU or the count of
Kranichschutz Deutschland), researched before the sheet is written.

## Starting files

Each starting file exists twice, from one description (`src/office/deck.py`):
`.pptx` written by python-pptx as PowerPoint writes it, and `.odp` written by
LibreOffice. The card offers the `.pptx` to PowerPoint and the `.odp` to
LibreOffice and Nextcloud Office (`TaskCard`, `language`). LibreOffice opens
a `.pptx` with every letter formatted by hand, so its slides would not
follow a change of the master there (tried with LibreOffice 25.2); an `.odp`
keeps the styles. The check reads both.

## The check

The office service reads a presentation (`src/office/slides.py`) and draws
each slide; the rules are in `document_check_slides.rb` (built so far:
`titles`, `min_size`, `max_words`, `no_overflow`, `notes`, `slide_count`,
`placeholders`). Sizes count as on PowerPoint's 16:9 slide (960 pt wide);
text a box shrinks to fit counts at the size it is shown. Still to build:
`no_direct` (for a `.pptx` read from its XML: LibreOffice's import marks
every letter as formatted by hand), `contrast`, `fonts`, `slide_numbers`,
`master_changed`, `ratio` and `alt_text` for slides, `credits`, `chart`,
`notes_every`, `animations`, `transitions`, `last_slide`.

## Curriculum

`Sek I: Standardsoftware (Präsentation)` and, as for word processing,
`Medienbildung: Präsentieren (Medienspezifische Gestaltungsprinzipien;
Durchführung einer Präsentation)`.

## Decisions

- **The cranes** (the maintainer, 12 November 2026): there are no photos
  of a class trip to use, and cranes are great; the Referat reuses the
  facts of the trip newspaper's article.
- **A timer on sheet 1** as a widget: how long reading one slide of text
  takes, against how long saying it takes.
- **Feedback from classmates on sheet 6**: a page of its own besides the
  teacher's feedback sheet (what it stores goes into privacy.rb first).
