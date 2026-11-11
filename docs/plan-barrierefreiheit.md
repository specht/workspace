# Plan: an activity on accessibility (»Eine Seite für alle«)

The Handbuch page `reference/accessibility` explains the rules and shows
three of them (`a11y_demo.js`: only colour, contrast, what a screen reader
reads out). An activity would let students meet the barriers themselves on
a page that has them, and take them out one by one, each step checked. The
maintainer asked for it on 11 November 2026 (»if this becomes like an
activity we should make an activity out of it or at least plan it«). Not
built.

## The idea

Students get a small website that looks fine and is hard to use: the sign-up
page of a school club (Hacker-AG) with a picture, a form, a list of dates,
buttons and a short animation. It has the barriers of the Handbuch page,
each on purpose and each found again in real sites:

- pictures without `alt`, one with `alt="bild1.jpg"`;
- no `lang` on `<html>`;
- fields without `<label>`, an error shown only by a red frame;
- a »button« that is a `<div onclick>`, an icon button without text, links
  that say »hier klicken«;
- light grey text on white (about 2.5 : 1), a link only by its colour;
- headings chosen by size (`h4` after `h1`), no `<main>` or `<nav>`;
- `outline: none` on everything, an animation that never stops.

First they **use** the page as others do, then they **fix** it, then they
**check** it with tools and write down what is left.

## Chapters

1. **Einrichten:** clone the starter, open the live preview (Handbuch
   `workspace`). Builds on `static-website`.
2. **Die Seite benutzen wie andere:** only the keyboard (a prediction
   first: how many presses of Tab to the button?), zoom to 200 %, the
   colour demo of the Handbuch on screenshots of the page, and the screen
   reader of their own phone or computer (VoiceOver, TalkBack, NVDA).
   Students list what goes wrong (a reflection task).
3. **Bilder und Sprache:** `alt` that says what a picture shows, `alt=""`
   for decoration, `lang="de"`. A choice task: which of four alt texts is
   good for a given picture, and why.
4. **Formulare:** `<label for>`, errors with a sign and a sentence,
   `autocomplete`.
5. **Knöpfe und Links:** `<div onclick>` to `<button>`, a text or
   `aria-label` for the icon button, link texts that say where they lead.
6. **Farbe und Kontrast:** students write the contrast ratio themselves
   (relative luminance, `(L1 + 0.05) / (L2 + 0.05)`) as a function in
   Python or Ruby (»Jetzt bist du dran«, check type `function`), run it on
   the page's colours and change the CSS until every pair reaches 4.5 : 1.
   This is the one place with a little mathematics and the strongest link
   to computer science.
7. **Struktur:** headings in order, `<main>`, `<nav>`, a skip link.
8. **Fokus und Bewegung:** `:focus-visible`, `prefers-reduced-motion`.
9. **Prüfen:** Lighthouse in Chrome, the accessibility inspector of
   Firefox; what tools cannot find (whether an alt text fits). Students
   write a short »Erklärung zur Barrierefreiheit« for their page, as the
   BITV asks of public bodies: what is accessible, what is not yet.

About four double lessons; chapters 3 to 8 are independent of each other,
so a class can stop after any of them.

## Checks

What exists (`docs/aufgaben.md`, »html, css, references«):

- `html` with `img: { alt: required }`: an `alt` on every picture;
- `html` with `elements`: `{ selector: html, attr: lang }`, `{ selector: main, min: 1 }`,
  `{ selector: nav, min: 1 }`, `{ selector: button, min: 2 }`;
- `css` with `declares`: a rule for `:focus-visible`, a media query with
  `prefers-reduced-motion`;
- `function` (Python, Ruby) for the contrast ratio.

What would be new in the `html` and `css` checks, each small:

- `elements` with `max` (`{ selector: 'div[onclick]', max: 0 }`, and no
  `img` whose `alt` ends in `.jpg`);
- `labels: required`: every `<input>` (except buttons and hidden fields)
  has a `<label for>` with its `id`, or sits inside a `<label>`;
- `headings: in_order`: no heading level skipped downwards;
- `contrast: { min: 4.5 }` in `css`: for every rule that declares both
  `color` and `background-color` (and for `color` against the page's
  background), the ratio after WCAG; the card names the selector and the
  ratio it found.

An automatic test of the whole page (axe-core in a headless browser) would
find more, but needs a browser in the Workspace or on the server; the
small checks above are enough for the barriers the starter has, and each
gives a clear sentence on the card.

## Open for the maintainer

- **Title and place:** »Eine Seite für alle« or »Barrierefreie Webseiten«;
  in the section of `static-website`, after it.
- **Kind:** Labor (find and fix) or Projekt (own page made accessible at the
  end).
- **The screen reader:** students would listen on their own device, which
  needs the live preview reachable from a phone or a second browser; the
  preview is a subdomain of its own, and whether a phone can open it has
  to be tried first. Alternative: the reader demo of the Handbuch for the
  page's parts, and a screen reader only where the school has one.
- **The contrast function:** Python and Ruby both, as other activities do,
  or only the language the class uses (the page's language switch).
- **axe-core:** leave it out (the plan above) or add it to the Workspace
  image for a `command` check.
- **Curriculum line:** which of the names allowed in `docs/lehrplan.md`
  fits; not chosen yet.
