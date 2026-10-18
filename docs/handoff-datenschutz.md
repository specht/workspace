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
- git.nhcham.org: his personal Git server, used voluntarily by a few
  (mostly LK) students, no activity needs it. It has its own accounts; it
  probably needs a short notice of its own.
- Administrators can look into a Workspace without a reason, and then the
  student sees no notice; the page says so in general terms. He trusts
  teachers; leave as is unless he wants a notice for admins too.

## Next: invitations in the browser

The maintainer suggests replacing the text files in `/invitations/*.txt` with
a user interface: teachers add addresses first and names later (names
optional, made-up addresses allowed), and a group can require a consent form
when a school wants one (`PARENTAL_CONSENT_REQUIRED`, per group rather than
per installation). `load_invitations_locked` (main.rb) is read everywhere
(`@@invitations`, teacher roles, groups), so the safe first step is to keep
its output shape and change only where the data comes from. Design this with
him before building.
