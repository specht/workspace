# Hand-off: Impressum, Datenschutzerklärung and the package for the school

Updated 2026-10-18. Branch `datenschutz`, not pushed. Read AGENTS.md and
[datenschutz.md](datenschutz.md) first; the latter says what exists and how
it fits together.

## The route

The maintainer prefers the route over the school: hand the Schulleitung a
finished package and one small decision (»Der Workspace darf im
Informatikunterricht als digitales Lehr- und Lernmittel eingesetzt werden«).
In Berlin this rests on Art. 6 Abs. 1 Buchst. e DSGVO with § 64 Abs. 11
SchulG and § 2 DigLLV (Verordnung vom 7. August 2023, GVBl. S. 296): the
school checks a digital teaching tool itself (§ 2 Abs. 2 Satz 3), a third
party's servers must be in the EEA (§ 2 Abs. 3 Satz 2), data categories are
listed (§ 2 Abs. 5), deletion at the latest at the end of a school year with
a Löschkonzept (§ 2 Abs. 6). No consent, no per-student unlocking.

External adults (teachers of other schools) use it under the Nutzungsordnung
(Art. 6 Abs. 1 Buchst. b); their students only if their own school decides
the same.

## Done on this branch

- `/impressum` from `include/privacy.rb`: formal, no »du«, every heading with
  its point of Art. 13/14, data tagged with the DigLLV categories.
- `/datenschutz`: the package (`include/privacy_documents.rb`), public.
- Account deletion (admin), list of uninvited accounts, exam deletion now
  removes exam Workspaces and databases, expired sessions deleted, log size
  limits, names optional and updated when they come later.
- Code Bites removed with its three practice sets (the maintainer's
  decision); a note in TODO.md that it may come back revamped.
- The webhosting agreement is now `/datenschutz/webhosting`; the old PDF is
  removed (git history has it).

## Open: to settle with the maintainer

- `HOSTING`: which Hetzner location (the page says the data stay in the EU;
  Hetzner also has US and Singapore locations). Conclude Hetzner's DPA and
  the mail provider's (udag) if not done; the draft AVV says they exist.
- Who pays for the server: the Schulleitung page says »keine Kosten« for the
  school.
- The school's data protection officer (`DATA_PROTECTION_OFFICER`).
- `OPERATOR_ROLE`, and whether »Dr.« belongs in `OPERATOR_NAME` (taken from
  the old webhosting PDF).
- The page promises another way to do the tasks for students who object;
  confirm that he offers one.
- Logrotate keeps nginx logs 7 days if the server was set up with
  `bootstrap/03-setup-storage.rb`; check on production. Traefik in front:
  check that its access log is off (the default), or the page must mention
  IP addresses.
- The webhosting server (port 2222, separate from this repo): what it logs
  about visitors of the students' sites.
- git.nhcham.org: decided; it is voluntary and not part of the Workspace,
  so the page leaves it out.
- Administrators can look into a Workspace without a reason, and then the
  student sees no notice; the page says so in general terms. He trusts
  teachers; leave as is unless he wants a notice for admins too.

## Done since (2026-10-18, later)

- Self-service in the profile (»Meine Daten«): ZIP export (Workspace,
  hand-ins unpacked, every MySQL database, Neo4j dump via `neo4j_bolt` 0.5,
  account JSON) built by `tools/zip_stream.py` as uid 1000 without
  following links; self-deletion in two steps (address, code by mail), with
  a mail to the administrators.
- Admin: people search, optional deletion mail, deletion log and »Löschungen
  wiederholen« after a restore; backups on the page (`BACKUP`,
  `BACKUP_RETENTION_MONTHS`). The maintainer set borgmatic to about 6 months
  (keep_daily 14, keep_weekly 8, keep_monthly 6, no yearly); check with
  `borgmatic rlist` that the oldest archive fits before relying on the page.
- Roster: `include/roster.rb` (tested against a verbatim copy of the old
  loader, `test/support/legacy_invitations.rb`), `include/roster_store.rb`
  (Neo4j, import with preview, verification in one transaction, undo),
  `src/scripts/check-invitation-import.rb`. Not imported on production yet;
  the maintainer wants to test it on a copy first (docs/teacher-access.md,
  »Einladungen importieren«). Tested on the dev site: preview, simulated
  difference (nothing written), import (people overview identical), logins
  and teacher groups, undo.
- `neo4j_bolt` dump/load of dates, times, durations and points: fixed in a
  patch for the gem (tagged maps `{"$neo4j": …}`, older dumps load as
  before; specs pass on Neo4j 4.4, 5.26, 2026.06). The patch was handed to
  the maintainer for `git am` in ~/programming/neo4j_bolt (not committed
  there); after a release, raise the Workspace's Gemfile.
- Teacher onboarding (lehrmaterial-einstieg) says the Workspace's
  administration keeps the invitations; update it again once teachers manage
  their groups on /gruppen.

## State at the end of the session (2026-10-18, last)

Built and committed, all Ruby tests passing:

- `/gruppen` (`include/groups_page.rb`, `static/gruppen.html`): teachers
  create groups (they belong to the teacher's school), add addresses
  (`Roster.parse_member_lines`), edit names, add co-teachers of the same
  school, send welcome mails (`MailTexts.welcome_student`, reply-to the
  teacher, `welcomed_at` on the Invitation), delete groups. Works only after
  the import; before, the page says so. Links to the package of the
  teacher's school.
- `/personen` (`include/people_page.rb`, `static/personen.html`), admin only:
  schools (name, address, DSB, law, authority; `decision_at` and
  `agreement_at`, approved when both are set; the home school is approved by
  the import; »Gäste« school `guests`, always approved, its groups are made
  on /gruppen), teachers with a required school (only the admin assigns
  it), welcome mail to teachers (`MailTexts.welcome_teacher`, link
  `/datenschutz?schule=KEY`, `COST_NOTE`), overview of everyone with schools
  and groups.
- The package per school (`Privacy.for_school`, `Main#privacy_settings_for`):
  `/datenschutz?schule=KEY` and every document under it with that school as
  controller (its School node's fields; a missing field stays missing), the
  operator as processor, its teachers as contact, the Elternbrief signed by
  the logged-in teacher of that school (else all its teachers). Home school
  and guests get the plain package; an unknown key is 404. `/impressum`
  lists approved partner schools (`Main#partner_schools`,
  `Privacy.page_html(s, partners:)`). docs/datenschutz.md »Partnerschulen«.
- `COST_NOTE` (credentials.template.rb, `Privacy.settings[:cost_note]`): on
  the Schulleitung page, in § 6 of the agreement and in the welcome mail,
  each time with the sentence that the use never depends on it. Not set in
  the maintainer's credentials.rb yet; tax and Nebentätigkeit noted in the
  template.
- Guests named in the Datenschutzerklärung (`others_html`); /klasse nests
  the group select under the schools once there is more than one
  (`school_names_of_groups`); groups not in Neo4j (synthesized
  »Lehrkräfte«, test groups, `_` files) come last without a school.
- docs/teacher-access.md describes /gruppen and /personen; the CHANGELOG
  says that after the import the files are no longer read; the onboarding
  says where a teacher of another school finds the package for that school.
- Tested end to end on the dev site (import, partner school with a teacher
  and a group, the package, approval, the partner list on /impressum, the
  nested select), then undone; the dev roster is back to files.
- neo4j_bolt 0.5.1 released and used by the server and the Workspace image
  (`ARG NEO4J_BOLT_VERSION=0.5.1`; the image is not rebuilt yet).

Still to do:

1. Before the live import: the maintainer runs
   `src/scripts/check-invitation-import.rb` on a copy of the production
   `/invitations`, then imports under »Administration › Einladungslisten«
   (docs/teacher-access.md, »Einladungen importieren«). After the import,
   fill in the home school's DSB on /personen if the DSB is not in
   `DATA_PROTECTION_OFFICER` (the home school's package uses the
   credentials, not the node).
2. The maintainer decides whether to set `COST_NOTE` and with what text.
3. Open for the maintainer, unchanged: sign the AVVs (Hetzner console,
   united-domains; Cloudflare is DNS only), check `borgmatic rlist` that the
   oldest archive is within the 6 months the page states, rebuild the
   Workspace image for neo4j_bolt 0.5.1, the school's DSB name.
4. Possible later: a notice for students when an admin looks in (he trusts
   teachers; left as is); `PARENTAL_CONSENT_REQUIRED` per group rather than
   per installation, and the blocking it promises (not built).

## Notes on the design (for whoever continues)

- `Privacy.settings` carries `:school_name`, `:school_address` and
  `:contacts` (`[{ name:, email: }]`) next to the operator's values;
  `for_school` replaces them. Texts about »the school« use the school keys,
  texts about the operator (`operator_line`, Impressum, AVV party,
  webhosting) the operator keys. Keep that apart when adding text.
- `PrivacyDocuments.path(s, slug)` keeps `?schule=KEY` on every link
  between the documents; `privacy_url(s)` is the printed address of the
  Datenschutzerklärung (the partner's own version, else /impressum).
- The roster in Neo4j is read through `Main.cached_roster` and invalidated
  by `Main.note_roster_change!`; every write on /gruppen and /personen
  calls it.
