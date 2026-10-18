# Hand-off: Impressum and Datenschutzerklärung

Written 2026-10-18 at the end of a long session, for the next session (a
different account, same machine). Branch `datenschutz`, made from `main`
at `fd5b6e14`; nothing on it is pushed. Read AGENTS.md first, as always.

## What the maintainer asked for

Michael Specht (the maintainer) wants:

1. **His name on the page, as the person responsible**, but **the school's
   address** (Gymnasium Steglitz), not his private one.
2. **Configurable**, because other teachers at other schools run their own
   Workspace: name, address, school, e-mail come from `credentials.rb`, with
   placeholders in `credentials.template.rb`, like `CONTACT_EMAIL` now.
3. The Datenschutzerklärung should **list what actually happens to data**,
   precisely, not in general phrases.
4. **What students and parents can do if they do not agree** (objection,
   deletion, who to write to).
5. It must say plainly: **a non-profit teaching platform run by one teacher**.
6. **Think about parental consent**: in future an account might only be
   unlocked once parents have agreed. Design for it, do not build it yet
   unless he says so.
7. He believes the **login codes by e-mail are fine** (the mail provider
   sees the code) because the code is only half of the secret. Confirmed in
   the code, see below; say so on the page.

## Where things are now

- The page: `src/static/impressum.html` (a Ruby template: `#{…}` is
  evaluated, so `\(` and other backslashes need care). The route is
  `/impressum`; the footer links it as »Impressum und Datenschutzerklärung«
  (`src/static/_template.html`, around line 1043).
- It hard-codes »Michael Specht am Gymnasium Steglitz« and says vague, partly
  wrong things: »wird von der Schule betrieben«, »keine Einbindung externer
  Dienste« (see the list below: there are some, server-side), »Alle Daten
  bleiben auf dem schulischen Server« (the server is rented, not at school;
  check where it is hosted: README »Shared vCPU x86«).
- Settings: `src/ruby/credentials.rb` (ignored by git) and
  `src/ruby/credentials.template.rb`. `CONTACT_EMAIL` is read in `main.rb`
  (around line 1258, `contact_email` / `contact_email_link` around 5216).
  New settings belong beside it, each with a German comment like the others,
  and an empty or missing value must not break the page (»Ohne diese Zeile
  fehlt …«, as for `CONTACT_EMAIL`).

Suggested settings (names English, as all code):

```ruby
OPERATOR_NAME = 'Michael Specht'          # who is responsible (Verantwortlicher)
OPERATOR_ROLE = 'Lehrer für Informatik'   # optional
OPERATOR_SCHOOL = 'Gymnasium Steglitz'
OPERATOR_ADDRESS = "Heesestraße 15\n12169 Berlin"   # check the real address
# CONTACT_EMAIL exists already
SCHOOL_DATA_PROTECTION_OFFICER = '…'      # optional: name/e-mail of the school's DSB
HOSTING = 'Hetzner Online GmbH, Rechenzentrum in Deutschland'  # ask the maintainer
```

Check the school's address with the maintainer; do not trust the one above.

## Facts for the Datenschutzerklärung (checked in the code)

**Login.** `POST /api/request_login` (main.rb ~2838) works only for invited
addresses; it creates a `LoginRequest` with a random **tag**, which stays in
the browser, and a **code**, which goes out by e-mail. `POST
/api/complete_login` (~2979) needs **both** tag and code. So the mail
provider sees a code that is useless without the browser that asked for it.
Code valid 10 minutes, at most 5 attempts, a new one after 60 s
(`include/authentication.rb`). There is also a QR login (5 minutes).
Sessions last **365 days** (`SESSION_LIFETIME_DAYS`). Requests are
rate-limited by client IP, in memory only.

**Mail.** Sent through the SMTP server in `credentials.rb` (`SMTP_SERVER`,
in production a provider such as udag). The provider sees the address, the
time and the code.

**Who can log in.** Only addresses in `/invitations/*.txt`, one group (class,
course) per file section, with e-mail and name (`load_invitations_locked`,
main.rb ~2081). `+` and `*` mark teachers. Names come from those files.

**Stored per person** (Neo4j, `User` node and related):
e-mail, name, group(s), sessions, login requests, preferences (`brightness`,
`code_language`, `preferred_language`, `class_prefs`), the Workspace's
server tag, database logins (`db_login`), share tags; tasks
(`TaskState`, `TaskAttempt`: answers to tasks and when), `Solved`,
`Submission` (handed-in files, `include/submission_store.rb`), `Pin`,
`LookIn` records, live-app shares (`LiveAppShare`).

**The Workspace itself.** Every person gets a VS Code container with a home
directory (`/workspace`): all their files, terminal history, installed
packages. Plus their own MySQL, PostgreSQL and Neo4j databases
(`include/database_provisioning.rb`). These live on the server's disk.

**What teachers see.** The class board (`include/class_board.rb`):
progress on tasks per student. Submissions (`include/submission_view.rb`).
**Look In** (`include/look_in.rb`): a teacher can open a student's account or
Workspace, but only with a reason of 5 to 300 characters, and the student
sees a notice the next time (»Herr Specht hat sich am 14.10. um 10:12 deinen
Workspace angesehen: …«). Grades are never stored here.

**Shared live apps** (`include/live_apps.rb`): a student can share a running
web app of their Workspace under a link; check in the code who can open such
a link (logged-in only, or anyone with the link) and say so.

**Logs.** nginx logs requests with method, URI, status and whether it was a
share (`log_format custom`, main.rb ~792): **no IP addresses, no user
names**. Check the Ruby/Puma logs and `logs/` for what else is written, and
how long anything is kept (`src/ruby/housekeeping.rb`).

**Outside services**, so the old sentence »keine externen Dienste« needs
correcting. None of them run in the reader's browser; fonts, KaTeX and icons
are served from the site itself (check `_template.html` once more):
- the mail provider (login codes, invitations)
- `api.open-meteo.com` for the weather activity, through a server-side
  mirror (`include/weather_mirror.rb`): the provider sees the server, not
  the student
- `git.nhcham.org`, a GitLab the maintainer runs, where some activities have
  students push their code (sandboxes, `reference/gitlab`): students have
  accounts there; that needs its own sentence (or its own notice)
- GitHub and other hosts from which templates are cloned and packages
  installed (gem, pip, npm) from inside the Workspace
- the map data of the route planner is OpenStreetMap, but prepared in
  advance (`specht/workspace-files`), not loaded from OSM by the browser

**Cookies.** Check the current statement (»vor der Anmeldung keine
Cookies«) against the code: the session cookie is host-only, `hs_server_sid`
is domain-scoped for the Workspace subdomains (live_apps.rb ~321), and the
brightness preference may be a cookie or stored on the user.

**Gaps found.** There is **no way to delete an account** (no code for it was
found: `grep -n "DETACH DELETE u"`). The page will promise deletion on
request; either build an admin action »Konto löschen« (user node, sessions,
tasks, submissions, Workspace volume, databases) or describe the manual way
honestly. Also: what happens to accounts at the end of a school year.

## The page to write

German, plain, Du-Form for students where it addresses them, but it is also
for parents. Suggested outline:

1. **Impressum**: operator name, »Lehrer am …«, school address, e-mail;
   »Ein nicht-kommerzielles Angebot für den Unterricht, betrieben von einer
   Lehrkraft«; source code link (exists).
2. **Wer verantwortlich ist** (Verantwortlicher), and the school's data
   protection officer if configured.
3. **Was gespeichert wird und warum**: the facts above, as a table
   (Daten · wozu · wer sieht es · wie lange).
4. **Anmeldung per Code**: why the mailed code alone is useless.
5. **Was Lehrkräfte sehen**, including Look In with its notice.
6. **Dienste außerhalb**: the list above.
7. **Wie lange**: sessions 365 days, codes 10 minutes, accounts until …
   (decide with the maintainer).
8. **Deine Rechte** (Auskunft, Berichtigung, Löschung, Widerspruch,
   Beschwerde bei der Aufsichtsbehörde: in Berlin the Berliner Beauftragte
   für Datenschutz und Informationsfreiheit) and **who to write to**:
   CONTACT_EMAIL. Make »Wenn ihr nicht einverstanden seid« concrete: an
   e-mail, and what then happens (account locked or deleted, work exported
   as a download first if wanted; check whether a download exists).

Keep the facts in one place in code if possible (a Ruby hash or YAML in
`src/ruby/include/`), so a test can check that the page names every outside
service and that nothing on it contradicts the configuration. A test that
`impressum.html` renders with all new settings missing is a must.

## The aim: honest, and calm for data protection officers and lawyers

The maintainer's words: honest, but such that data protection officers and
lawyers stay calm. He does not expect real help from the school's data
protection officer. These go together: what worries a reviewer is vagueness
and promises the system cannot keep (the old »keine externen Dienste«); what
reassures is a familiar structure, precise facts and no overstatement.

- **Follow DSGVO Art. 13 point by point**, with the usual headings, so a
  reviewer can tick it off: Verantwortlicher; Kontakt (and the school's
  Datenschutzbeauftragter if configured); Zwecke; Rechtsgrundlage;
  Empfänger; Übermittlung in Drittländer (say whether any of the outside
  services is outside the EU: GitHub, package registries; only the server
  contacts them); Speicherdauer; Rechte; Beschwerderecht bei der
  Aufsichtsbehörde; ob die Angabe der Daten erforderlich ist; keine
  automatisierte Entscheidung, kein Profiling.
- **Say only what is true and checked**, in the present tense, without
  softeners (»grundsätzlich«, »in der Regel«). Where something is not built
  (account deletion), say how it is done today, not what will be.
- **Name the technical and organisational measures** that already exist,
  since they show care: invitation only, login code plus browser tag,
  rate limits, no tracking and no analytics, no IP addresses in the logs,
  fonts and scripts from the site itself, Workspaces isolated from each
  other on the network, Look In only with a reason and a notice to the
  student, encryption in transit (check: HTTPS in production). A short
  section »So schützen wir eure Daten«.
- **The legal basis is the one point not to guess.** »Art. 6 Abs. 1 lit. e
  DSGVO in Verbindung mit dem Schulgesetz« is only true if the school uses
  the Workspace officially; otherwise it is consent. The cleanest way that
  does not depend on the data protection officer: a short written decision
  by the Schulleitung that the Workspace is used in lessons. Then the page
  can name the school as the place of use and him as the teacher who runs
  it. Make the sentence configurable (`LEGAL_BASIS = :school | :consent`) so
  each installation states its own case; the consent design below is the
  other branch.
- **Offer the documents reviewers ask for**, generated from the same facts:
  a Verzeichnis der Verarbeitungstätigkeiten (VVT) entry and a list of the
  technical and organisational measures (TOM), as a page or a download for
  admins. Handing those over unprompted is what usually ends a discussion.
- For questions the school cannot answer: the Berliner Beauftragte für
  Datenschutz und Informationsfreiheit publishes guidance for schools, and
  the Senatsverwaltung for digital teaching tools; the page should not cite
  rules the session has not looked up.

## Parental consent: things to put to the maintainer

Not legal advice; the next session should say so and suggest he asks the
school's data protection officer (behördlicher Datenschutzbeauftragter)
before relying on any of it.

- The legal basis matters most. If the **school** officially uses the
  Workspace as a teaching tool (decided by the Schulleitung, entered in the
  school's records of processing, with an agreement with the hoster), the
  basis is the school's public task, and consent is not the basis. If it is
  **his own private offer**, consent is usually needed, and for children
  under 16 from the parents (DSGVO Art. 8 for online services). Which of the
  two it is decides whether consent must be built at all.
- A design that keeps both doors open: an invitation can carry a flag
  »braucht Einverständnis«; such an account logs in but sees only a page
  »Dein Konto wird freigeschaltet, sobald …«, with a printable form or a link
  for parents; a teacher (or an admin page) marks the consent as given, with
  the date. Store who confirmed it and when. Off by default
  (`PARENTAL_CONSENT_REQUIRED = false`), like `TASKS_ENABLED`.
- Configurable per installation, since another school may decide
  differently.

## Working agreements to keep

- Branch `datenschutz`; merge into `main` and push only when the maintainer
  asks (AGENTS.md »Git«).
- CHANGELOG entry in German under »Unreleased« (»für Betreibende: neue
  Einträge in credentials.rb …« — a new required setting is a **major**
  change by the versioning rules at the top of CHANGELOG.md, so make them
  optional with sensible output when missing).
- Run the Ruby tests (`cd src/ruby; for f in test/*_test.rb; do ruby "$f" |
  tail -1; done`); all passed on `main` at `fd5b6e14`.
- The dev site follows the checkout (`http://workspace.test:8025/impressum`).
