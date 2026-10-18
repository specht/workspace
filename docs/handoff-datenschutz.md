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

## Next: invitations in the browser (decided)

- Groups in Neo4j (decided: everything moves, after the import); students of
  an external school log in only once the admin has approved the school
  (decided).
- Still to build: /gruppen for teachers (create groups, add addresses with
  optional names, names later, co-teachers, welcome mails, print the package
  for their school); /personen for the admin (schools with approval, add a
  teacher with school, welcome mail to teachers, overview of everyone); the
  package per school (controller = that school, operator as processor,
  contact = the teacher, Elternbrief signed by the teacher); a cost note
  setting (free to use, contributions welcome, never a condition; mind tax
  and Nebentätigkeit if money goes to the maintainer personally; a
  Förderverein or the Schulträger paying the server are simpler).

## Next: invitations in the browser (first notes)

The maintainer suggests replacing the text files in `/invitations/*.txt` with
a user interface: teachers add addresses first and names later (names
optional, made-up addresses allowed), and a group can require a consent form
when a school wants one (`PARENTAL_CONSENT_REQUIRED`, per group rather than
per installation). `load_invitations_locked` (main.rb) is read everywhere
(`@@invitations`, teacher roles, groups), so the safe first step is to keep
its output shape and change only where the data comes from (done: Roster).
