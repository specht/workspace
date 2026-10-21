# Hand-off: data protection, schools, access rules, operations

Updated 2026-10-21. Replaces every earlier version of this file and
`docs/handoff-datenschutz-2.md` (the maintainer's transition plan of
2026-10-21, now part of this file). Older plans are in the git history.

Read first: `AGENTS.md`, `docs/datenschutz.md` (what is stored, the
documents for the school, deleting), `docs/teacher-access.md` (roles,
/gruppen, /personen, the import), `docs/entwicklung.md`, `e2e/AGENTS.md`
before an E2E test. `CHANGELOG.md` »Unreleased« lists every change in German.

## Git state

- Branch `datenschutz-uebergang` (from `main` of 2026-10-21, not merged):
  the transition tracking on /personen, this hand-off, the docs brought in
  line, the outdated DigLLV reference in the Prüfübersicht.
- Merge or push only when the maintainer asks. Stage files by name, never
  `git add -A` (a swap file of `credentials.rb` was once committed that way).

## The transition period (binding decision of the maintainer)

The Workspace has been used at Berlin schools for about three years; more
schools are joining. Accounts, access rules, the documents and operations
have been brought up to standard; what is missing at most schools is the
paperwork: the school's decision and the agreement on processing (AVV).

The maintainer decided on a structured transition instead of interrupting
lessons: every participating school is to be in a documented state by
**12 April 2027** (after the Berlin Easter holidays; `TRANSITION_DEADLINE`
in `credentials.rb`).

<table>
<tr><th>Period</th><th>Aim</th><th>Policy</th></tr>
<tr><td>until 1 Nov 2026</td><td>autumn holidays, prepare the information</td><td>no new work for the schools</td></tr>
<tr><td>2 Nov – 18 Dec 2026</td><td>talk to the maintainer's Schulleitung and the other schools</td><td>access continues</td></tr>
<tr><td>Jan – Feb 2027</td><td>collect decisions and documents, follow up, answer questions</td><td>access continues; progress on /personen</td></tr>
<tr><td>end of Jan 2027</td><td>review of the progress</td><td>name what is in the way, adjust the follow-up</td></tr>
<tr><td>Mar 2027</td><td>last review, advance notice to schools still open</td><td>prepare restrictions if needed, no automatic lockout</td></tr>
<tr><td>12 Apr 2027</td><td>target</td><td>ordinary school use needs a documented basis</td></tr>
</table>

What this is and is not:

- It is a **project deadline**. It does not suspend legal obligations: the
  GDPR applies from the start of processing, DigLLV § 3 Abs. 3 wants the
  checks »vor Aufnahme der Verarbeitung«, and the BlnBDI wrote in January
  2021 that tolerance »bedeutet jedoch keinesfalls, dass der Einsatz nicht
  datenschutzgerechter digitaler Dienste dadurch rechtmäßig wird« and that
  such use »darf sich in Berliner Schulen keinesfalls verstetigen« (checked
  in the press release of 22 January 2021). The honest description is a
  documented remediation plan with dates, which shows good faith and lowers
  the risk; it does not make missing paperwork lawful.
- Missing paperwork is **not** a withdrawal. An explicit objection, a
  withdrawal of a school's approval or an individual request (Auskunft,
  Löschung, Widerspruch) is handled when it arrives, as before
  (`suspend_access`, deletion), whatever the date.
- Until the maintainer approves an enforcement procedure (below), **nobody
  is locked out over a school's legal status**, and nothing happens by
  itself on the deadline.
- Restricting access never deletes accounts or students' work.

Pending paperwork is not a new security issue; do not reopen the decision.
Real technical vulnerabilities are handled at once, as always.

### What the software does now (branch `datenschutz-uebergang`)

`src/ruby/include/school_transition.rb`, used by `people_page.rb` and
`/personen` › Schulen:

- per school, chosen by the administration: the state of the talks (noch
  nicht angesprochen, Gespräch geplant, im Gespräch, Unterlagen ausstehend,
  Nachfassen nötig), last contact, next follow-up, open questions, a note
  (without names of people);
- the checklist, each step a date: Schulleitung angesprochen, Rechtsgrundlage
  mit der Schule geklärt, Datenschutzbeauftragte Stelle beteiligt, plus the
  existing Entscheidung (`decision_at`) and AVV (`agreement_at`), which are
  still set only on their own route (`school_status`);
- above the table: the deadline, the days left, what the current phase asks
  for, and four counts (complete, in progress, not contacted, follow-up due);
- »Nachricht vorbereiten«: a text for the phase, filled in for the school,
  to copy into one's own mail. Nothing is sent from the Workspace.

Kept apart on purpose: the progress never sets `decision_at`,
`agreement_at`, `approved_at` or anyone's `consent_at`, never changes
access, never sends mail (`test/school_transition_test.rb` checks the route
source; `e2e/tests/school-transition.browser.spec.ts` checks it end to end,
including that the note shows up on no page but /personen). A school is
approved, as before, only with both dates (`school_approval!`). The guests
are outside the whole process.

## The legal situation in Berlin (researched 2026-10-21)

Research, not legal advice; marked where a primary text was read. The
regional Datenschutzbeauftragte (DSB) of the school decides the open points.

- **Basis.** § 64 Abs. 11 SchulG Berlin (since the 2021 amendment) allows
  schools to process personal data »zum Zweck des Einsatzes digitaler Lehr-
  und Lernmittel«, as far as needed for their tasks; the explanatory
  memorandum calls it »eine eigenständige datenschutzrechtliche
  Rechtsgrundlage« for learning platforms (Drs. 18/3879). The DigLLV
  (GVBl. 2023 S. 296, read in full) fills it in: § 2 Abs. 1 allows the
  processing »insbesondere zur Durchführung von Unterricht« and forbids
  other purposes (advertising, research of third parties); § 2 Abs. 2: for
  tools the Schulaufsicht does not provide, »prüft die Schule die Einhaltung
  der datenschutzrechtlichen Vorschriften«, and compliance »obliegt der
  Schule«; § 2 Abs. 3: third parties only as processors (Art. 28 GDPR);
  § 2 Abs. 5: the allowed categories (Verkehrs- und Verbindungsdaten,
  Stammdaten, Account-Daten, Zuordnungsdaten, nutzergenerierte Inhalte);
  § 2 Abs. 6: delete »sobald kein Verarbeitungszweck mehr besteht, in der
  Regel spätestens zum Ende eines Schuljahres«, in particular when a student
  has left; the school sets this out in a Löschkonzept. GDPR letter: the
  current Berlin template for Art. 13 notices names Art. 6 Abs. 1 lit. c,
  other states use lit. e; the DSB decides (lit. e brings the right to object,
  Art. 21).
- **Consent is not needed for each student, under conditions.** Checked:
  the BlnBDI wrote on 17 September 2021 that with the new law »die
  Einholung von Einwilligungen zur Nutzung von Videokonferenzen und des
  Lernraums Berlin nicht mehr nötig« is. That holds when the use is part of
  the school's task, the school has decided on it and checked it with its
  DSB, an AVV with the operator exists, only the § 2 Abs. 5 categories are
  processed and deleted per § 2 Abs. 6, and the people are informed
  (Art. 13). It does not cover what the school does not need (statistics,
  the operator's own purposes), publishing work under names, accounts after
  leaving school, or adult guests.
- **Consent does not fit compulsory lessons.** Art. 7 Abs. 4 and recital 43
  GDPR, EDPB Guidelines 05/2020 (public authorities, example 4: only a
  choice if refusing costs nothing): in a lesson built on the Workspace there
  is no real alternative. The written consent on /gruppen (`consent_at`) is
  therefore at most a stopgap for voluntary use, not a basis for lessons.
- **Who decides at a school.** The Gesamtkonferenz decides the principles
  for teaching media (§ 79 Abs. 3 Nr. 8 SchulG; the 2021 memorandum puts
  digital tools under it), the Fachkonferenz introduces »eine Lernsoftware,
  webbasierte oder andere Unterrichtsmedien« within them (§ 16 Abs. 2), the
  Schulleitung ensures compliance (§ 69 Abs. 1). Mentioning the tool in the
  Schulprogramm is useful, apparently not required. Who signs an AVV for a
  Berlin school (Schulleitung or the Land): not verified, ask the DSB.
- **Documents a school needs.** Its own check with the DSB (DigLLV § 2
  Abs. 2, Art. 38); the AVV (Art. 28); its entry in the record of processing
  (Art. 30 Abs. 1; the operator keeps his own, Art. 30 Abs. 2); the Art. 13
  information; a Löschkonzept. If the Workspace counts as a
  Lernmanagementsystem (§ 3 DigLLV): the agreement (»Einvernehmen«) of the
  Schulaufsicht, a risk analysis and a role, permission and deletion
  concept; a DPIA (Art. 35) if required (children and progress views speak
  for one). The SenBJF checklist »Datenschutzrechtliche Prüfkriterien für
  digitale Lösungen« (in Drs. 19/21945) is the official yardstick.
  `/datenschutz` already provides most of this (Entscheidung, Prüfübersicht,
  VVT entry, TOM, Löschkonzept, AVV draft, notices).
- **The operator.** For other schools he is a processor (Art. 28) and needs
  an AVV with each; using their data for his own purposes would make him a
  controller and break DigLLV § 2 Abs. 1. At his own school it is unclear
  whether he acts within his duties (then no AVV, but the SchuldatenV's rules
  on private equipment, § 17, may get in the way) or privately as a processor;
  the DSB decides. Nebentätigkeit: ask the personnel office.

### Ways the Workspace may be used at a school

<table>
<tr><th>Way</th><th>Fits compulsory lessons</th><th>What it takes</th></tr>
<tr><td>A. As a digital teaching tool under the school's task (§ 64 Abs. 11 SchulG, DigLLV § 2)</td><td>yes, the normal case</td><td>the school's check with its DSB; decision (Fachkonferenz within the Gesamtkonferenz's principles, Schulleitung); AVV with the operator; VVT entry; Art. 13 information; Löschkonzept (end of the school year, leaving school). If it is an LMS (§ 3): also the Schulaufsicht's agreement and a risk analysis</td></tr>
<tr><td>B. As an officially listed tool (§ 7 Abs. 2a SchulG)</td><td>yes, the most certainty</td><td>a school reports the need, SenBJF checks and lists it; depends on SenBJF, no deadlines known</td></tr>
<tr><td>C. Voluntary extras with consent (Art. 6 Abs. 1 lit. a)</td><td>no, not as the basis of a lesson</td><td>written, informed, revocable; refusing costs nothing. For what A does not cover (keeping an account after school, publishing under one's name). AGs of the school probably fall under A (DigLLV § 2 Abs. 1, »außerunterrichtliche … Förderung«)</td></tr>
<tr><td>D. Adults on their own account (guests, workshops)</td><td>not applicable</td><td>the operator is the controller; the Nutzungsordnung (contract, Art. 6 Abs. 1 lit. b); his own notice, VVT, TOM. Minors never this way</td></tr>
<tr><td>E. Without personal data (truly anonymous)</td><td>in theory</td><td>outside the GDPR; with accounts and work stored this is not the Workspace. Pseudonyms lower the risk but remain personal data for the operator</td></tr>
</table>

Use at the maintainer's own school is way A, once the DSB has said whether
he acts as part of the school or as its processor.

## After the transition: proposal for enforcement (not built, not active)

To be reviewed and approved by the maintainer after the talks; nothing of
this runs on 12 April 2027 by itself.

- **What counts.** A school with both dates (`approved_at`, way A or B).
  Adults in the guests' school (way D) stay outside, as now. Individual
  consent (way C) counts only for voluntary use, which the school or the
  teacher has to mark as such; it does not make a lesson lawful.
- **Who is affected.** Members of groups of schools without `approved_at`,
  minus teachers, administrators and guests. /personen shows the list before
  anything changes.
- **Who checks the evidence.** The administration enters the dates only from
  the signed documents the school sends; nothing is set from the progress.
- **Notice.** March 2027: a mail to the Schulleitung (from the template, sent
  by the maintainer himself) and to the school's teachers with the date and
  what is missing; the teachers tell their students. Then a notice on the
  affected students' start page some weeks before.
- **The restricted state.** Reversible, like `suspend_access` but per
  school: login still works for »Meine Daten« (download, deletion), the
  Workspace does not start, shared links and live apps stop. Running
  Workspaces are stopped at the next start, not killed in a lesson. Nothing
  is deleted; the ordinary deletion rules (end of school year, two weeks'
  notice) apply as before.
- **Restoring.** As soon as both dates are entered, everything works again,
  at once (the same check as now, `may_use_workspace?`).
- **A school withdraws.** Treated at once, not at the deadline: the
  administration restricts the school, informs the teachers, and the school
  says whether the data is to be deleted.
- **Exceptions.** Per school, by an administrator, with a reason and an end
  date, written to the audit log (as for opening a Workspace).
- **Audit.** Every change of the restricted state with who, when and why in
  `/internal/admin-access.jsonl` or a log of its own; /personen shows the
  history.

### What needs the maintainer's approval before any enforcement

1. The procedure above, and that `may_use_workspace?` may depend on a
   school's approval.
2. The date it starts (the deadline, or later per school).
3. The texts of the notices and who sends them.
4. What a restricted account may still do (proposal: Meine Daten only).
5. How exceptions are granted and for how long.
6. What happens to a school that never answers (the ordinary deletion after
   the school year, after notice).
7. The DSB's answers to the open points below.

## Open, for the maintainer and the DSB

1. Is the Workspace a digital teaching tool (§ 2 DigLLV) or a
   Lernmanagementsystem (§ 3)? The documents argue § 2 (no communication,
   organisation or grades); tasks, groups and progress views look LMS-like.
   § 3 adds the Schulaufsicht's agreement and a risk analysis.
2. Lit. c or lit. e of Art. 6 Abs. 1 in the notices.
3. The maintainer at his own school: part of the school, or its processor?
   Who signs AVVs for Berlin schools?
4. DigLLV § 2 Abs. 6 (»in der Regel spätestens zum Ende eines
   Schuljahres«): the Löschkonzept keeps accounts of students who go on
   with computer science next year (the purpose continues). The DSB should
   confirm this reading.
5. A DPIA: likely advisable (children, progress views); the DSB decides.
6. `LEGAL_BASIS = :consent` (an offer of one's own based on consent): the
   privacy notice then says »Ein Konto wird erst freigeschaltet, wenn die
   Einwilligung vorliegt«, but the access rules do not check consent.
   Either enforce it or change the sentence before anyone uses that mode.
7. The full current wording of § 64 Abs. 11 SchulG (the DigLLV cites a
   »Satz 2«; the source used showed one sentence): check on gesetze.berlin.de.
8. Not verified: Germany's age in Art. 8 GDPR (widely reported as 16), the
   BlnDSG on fines for public bodies, Nebentätigkeit rules, a central
   Dienstvereinbarung for school software.

Sources (all opened 2026-10-21): DigLLV, GVBl. 2023 S. 296
(pardok.parlament-berlin.de, g23220296.pdf); SchuldatenV, GVBl. 2023 S. 283;
SchulG §§ 7, 16, 64, 69, 76, 79 (schulgesetz-berlin.de, check against
gesetze.berlin.de); Drs. 18/3879 (2021 amendment); Senate answer Drs.
19/21945 of 27 March 2025 (with the SenBJF checklist; the deletion of § 2
Abs. 3 Satz 2 DigLLV); BlnBDI press releases of 17 September 2021 and
22 January 2021; EDPB Guidelines 05/2020 (consent) and 07/2020 (controller
and processor); LfD Niedersachsen, Eckpunkte Lernplattformen (2022).

## Binding decisions of the maintainer (other topics)

- Only `/impressum` is public. School documents are PDFs or one ZIP behind
  a login; a partner school's Datenschutzerklärung goes with each welcome
  mail as a PDF, after a preview.
- Guests are adults (workshops, companies, former students); they agree to
  the Nutzungsordnung once. Minors only come through their school.
- Long inactivity: an offer to announce a deletion (two weeks' notice, mail,
  data takeout, keep as guest), no automatic deletion rules.
- No school merge (move groups, delete the empty school). Backups as they
  are (a restore would be from the day before; `repeat_deletions` after it).
- Administrators give a reason when they open a Workspace or imitate
  someone; the person sees it; /internal/admin-access.jsonl for a year.
- Administrator accounts cannot be deleted, by no path.
- The roster import is final (no way back to the files).
- The account is created with the first login request (the texts say so).
- Sessions stay 365 days (class rooms); »überall abmelden« in the profile.
  Login codes stay readable: teachers project them.
- The home directory stays `/workspace` (docs/entwicklung.md, »Namen und
  Home-Verzeichnis im Workspace«).
- Never touch the backups (no borgmatic commands, no prune). Production
  dumps never in the repository. Never alter real users, also not in the
  development copy of the production data.

## What exists (pointers, details in the docs)

- Access rules in one place: `src/ruby/include/access.rb`
  (`may_use_workspace?`: suspended → no; admin → yes; invitations not loaded
  → no; invited or test account → yes; deletion announced → yes until the
  day). Sessions, nginx map, share links, live apps and watch links all ask
  it. E2E: `e2e/tests/access-rules.browser.spec.ts`.
- Roster in Neo4j: School, Group, Invitation, Setting `roster`;
  `include/roster.rb`, `roster_store.rb`, `groups_page.rb` (/gruppen),
  `people_page.rb` (/personen), `school_transition.rb`. Every roster write
  calls `Main.note_roster_change!` (the transition fields are not the
  roster and do not).
- Privacy texts: `include/privacy.rb` (facts, checked by tests),
  `privacy_documents.rb`, PDFs with weasyprint.
- Docker guard: `docker/docker-guard/guard.rb`, rules in
  `include/docker_policy.rb`. Container hardening:
  `WorkspaceRuntime::DirectDocker::HARDENING`.
- Monitoring: `include/health.rb`, `include/server_health.rb`, »Betrieb«
  on /admin.

## Open from earlier (2026-10-19), not confirmed since

1. Access per school: accounts are global per address; real separation
   would be a larger rebuild.
2. Workspaces reach nginx, ruby, mysql and neo4j on every port: worth a test
   that the MySQL and Neo4j root logins are refused from a Workspace.
3. More container hardening (read-only root, user namespaces, seccomp),
   each against the full E2E suite.
4. `main.rb` (~7,800 lines): move routes by topic into `include/`.
5. A restore test of the backup, by the maintainer on the server.
6. An outside uptime check (a server that is down is invisible to »Betrieb«).
7. AVVs with Hetzner and united-domains (the AVV draft says they exist);
   `COST_NOTE` on the live server; the school's DSB name; the oldest backup
   archive must fit the months on the page.
8. Deploy steps of 2026-10-19 (image before Ruby, `./config.rb up -d`, the
   borgmatic hook touching `internal/backup-done`, the docker_guard log and
   `measure-capacity.rb`): ask whether they are done.

## Working on the dev site

- `http://workspace.test:8025`, admin login with an `ADMIN_USERS` address
  and code `123456`; mails are only logged in development.
- The dev database holds the live dump: made-up addresses (`…@example.com`)
  and schools in tests, cleaned up afterwards; never real identities.
- Saving a file under `src/ruby` restarts the dev server; after a change in
  `src/static` restart it (`docker restart workspace_ruby_1`), the cache
  buster changes only then.
- Full E2E: `cd e2e && npm test`; one file:
  `npx playwright test --project=workspace-smoke --no-deps tests/FILE`.
- Ruby tests: `cd src/ruby; for f in test/*_test.rb; do ruby "$f" | tail -1; done`.
