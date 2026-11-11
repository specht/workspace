# Plan: an activity on accessibility, fair design and phishing (»Eine Seite für alle«)

The Handbuch page `reference/accessibility` explains the rules and shows
three of them (`a11y_demo.js`: only colour, contrast, what a screen reader
reads out). An activity would let students meet the barriers themselves on
a page that has them, and take them out one by one, each step checked. The
maintainer asked for it on 11 November 2026 (»if this becomes like an
activity we should make an activity out of it or at least plan it«), and
the same day: the activity should cover dark patterns too, and the demos of
both Handbuch pages (`a11y_demo.js`, `dark_patterns_demo.js`) move into it,
with tasks; the Handbuch keeps the rules. Later the same day: a part on
phishing mails as well (»a real problem at our school«), all of it one
activity. Being built from 11 November 2026.

## State (11 November 2026)

Built, in development only (`- .fair-web` in `sections.yaml`), as
`src/content/fair-web/`: one Markdown file with `parts: true`, three pages
(Eine Seite für alle, Fair statt trickreich, Phishing); the closing »Wie
geht es weiter?« ends the last part (`ActivityParts.fold_closing`). Each
part starts with »Erkennen« (demos, predictions, choices) and goes on with
building (HTML checks, functions in the card). The starter is
`src/static/fair-web/` (fetched with `wget`), the finished page
`src/content/fair-web/stufen/`; `fair_web_checks_test.rb` and
`task-examples.yaml` run every check against both. The mail viewer is
`src/static/include/mail_demo.js` (five mails, two phishing, `.example`
only). Functions: `luminance`, `contrast_ratio`, `host`, `real_domain`,
`looks_like`, in Ruby and Python. Still open: a card picture, the final
title, whether the demos leave the Handbuch pages (they are on both now),
a read of the page by the maintainer.

## Decided (the maintainer, 11 November 2026)

- **One activity in three parts:** accessibility, dark patterns, phishing,
  on one made-up club site; each part usable on its own.
- **Labor:** find and fix on a prepared site, checked step by step.
- **Phishing:** hunting clues in made-up mails (a widget), a link checker
  of one's own (the real domain of a link against what its text claims,
  look-alike letters included), and what to do after a click.
- **Python and Ruby** for the two functions (contrast ratio, link
  checker), with the page's language switch.

## Two layers (the maintainer, 11 November 2026)

The topic matters for everyone, younger students included, and the
technology behind it is computer science. Each part has two layers:

- **Erkennen, for everyone:** the demos and widgets (the mail viewer, the
  banner, the colour simulation) with predictions and choice tasks, no
  code; from about grade 5.
- **Verstehen und bauen, for computer science:** the link checker, the
  contrast function, the HTML repaired; marked as optional chapters
  (»Extra«) or the other way round, decided when the page stands.

What others already provide well (rules of behaviour, the »Surfschein« of
Internet-ABC, klicksafe, the BSI's pages for citizens) is linked, not
copied; the Workspace shows the technology behind it, with something to try
on every topic.

## More topics later, in the same two layers

- **Passwörter:** how long guessing takes (a widget), why a site must not
  store them readable (hashing, salt), password managers, two factors.
- **Cookies und Tracking:** what HTTP really sends; links the existing
  activity »Was eine Webseite über dich erfährt«.
- **Schadsoftware:** viruses, worms, trojans, ransomware; a worm spreading
  through a network as a simulation; how a scanner recognises malware
  (signatures as fingerprints, behaviour); the double extension
  (`.pdf.exe`); Morris worm 1988 and WannaCry 2017 as history boxes. The
  maintainer's observation: many laptops carry a pre-installed security
  suite that slows them down, while Windows has a protection program of
  its own; to be checked against the BSI before it goes on a page, and
  said without naming a product.
- **Manipulierte Bilder** and **Urheberrecht:** both still missing in the
  curriculum lines (`docs/plan-lehrplan-luecken.md`).

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
9. **Fair statt trickreich:** the club page also has a cookie banner, a
   newsletter box and a sign-up with fees that appear late, each a dark
   pattern of the Handbuch page. Students first predict how many clicks
   declining takes, count them in the banner demo, then rebuild the banner
   with two equal buttons, the box without the double negation and
   unticked, the price whole from the start. Tasks: a prediction (clicks),
   a choice (which wording is a dark pattern, and which), and checks of the
   HTML (no `checked` on the box, two buttons of the same class in the
   banner).
10. **Phishing:** the club's mailbox has five mails, two of them phishing
   (a made-up school portal: »Ihr Postfach wird in 24 Stunden gelöscht«).
   In a mail viewer (a widget, like the demos) students click what looks
   wrong: the sender's address with a look-alike letter, a greeting
   without a name, the haste, a link whose text and target differ (shown
   on hover, as in a real mail program), an attachment `Rechnung.pdf.exe`.
   Each clue found is explained; the real mails have none. Then the link
   checker of their own: `real_domain(url)` (the registrable domain of a
   link, `login.schul-portal.de.example.net` → `example.net`) and
   `looks_like(a, b)` (two domains that differ only in look-alike
   characters, `l` and `I`, `rn` and `m`, `0` and `o`), checked with
   function tasks. Last, what to do: a prediction (what can someone do with
   a stolen password?), a choice (what to do after a click: tell someone,
   change the password, sign out everywhere), two-factor sign-in as the
   answer. No real service or organisation is named or imitated.
11. **Prüfen:** Lighthouse in Chrome, the accessibility inspector of
   Firefox; what tools cannot find (whether an alt text fits). Students
   write a short »Erklärung zur Barrierefreiheit« for their page, as the
   BITV asks of public bodies: what is accessible, what is not yet.

About six double lessons; chapters 3 to 10 are independent of each other,
so a class can stop after any of them. The demos of both Handbuch pages go
into their chapters (the colour demo into 6, the screen reader into 3 and
5, the banner, the wording, the countdown and the till into 9), each with
a task about what it shows; the Handbuch pages then link the activity for
trying.

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
