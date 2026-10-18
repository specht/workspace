# Hand-off: Impressum, Datenschutzerklärung and the package for the school

Updated 2026-10-18. Branch `datenschutz`. Read AGENTS.md and
[datenschutz.md](datenschutz.md) first; the latter says what exists and how
it fits together. **The new action plan below supersedes outdated status
notes later in this handoff.**

## Next Claude session — Datenschutz review and school/group corrections (2026-10-18)

**Read first:** `AGENTS.md`, `docs/datenschutz.md`, `docs/teacher-access.md`, and the rest of this handoff. Work in `specht/workspace`, branch `datenschutz`; do not merge/deploy or modify production data without the maintainer's explicit approval. Keep changes in coherent commits, write a German CHANGELOG entry, and run the repository's Ruby and relevant browser/E2E tests.

### Status of the plan below (2026-10-18, night) — read this first

Done on the branch (each with tests or a run on the dev copy):
- School approval truthful: the import approves no school; both dates on
  /personen for every school; per-student consent on /gruppen; nobody is
  locked out (the maintainer's rule, overrides the plan's lockout).
- Group transfer (»Schule wechseln«), rename, suggestions, the reworked
  /gruppen and /personen, cleanup under »Aufräumen« (invitations without
  account, teachers without account, accounts without invitation).
- Finding 1, session revocation: `Main.may_use_workspace?`; sessions end
  and the nginx map drops running Workspaces of people no longer invited.
- Finding 2, cross-school: adding a student of another school to a group
  and moving a group are refused; guest groups are exempt by design.
  Existing double memberships from the files are not audited yet.
- Announced deletion (two weeks, mail, data takeout, keep as guest, the
  hourly job); admin-triggered, not yet by automatic rules.
- Finding 6, export: dumps checked, `nicht-enthalten.txt`, chown.
- Finding 8 partly: welcome mail links the school's notice;
  PARENTAL_CONSENT_REQUIRED no longer claims a gate.
- Guests: adults only, agree to the Nutzungsordnung at first login.
- The import is final (no »Zurück zu den Dateien«).

Decided and built since:
- Only /impressum is public; all school documents behind a login, as PDFs
  (weasyprint in the Ruby image) and as one ZIP for the Schulleitung; the
  partner school's Datenschutzerklärung goes with each welcome mail as a
  PDF, after a preview. That settles finding 3 (teachers' addresses are no
  longer public).
- Long inactivity: an offer (»über ein Jahr nicht da« selects them for an
  announced deletion); fully automatic rules not wanted yet.
- Finding 5: the backup includes /internal and a restore would be from the
  day before; the maintainer accepts that. No merge of schools (moving
  groups and deleting schools suffices).
- Finding 7: the Prüfübersicht argues § 2 DigLLV (no communication, no
  organisation, no grades; hand-ins and exams named); the school decides.

Open:
- Finding 8, the AVV draft says contracts with Hetzner and the mail provider
  exist: true once the maintainer has concluded them.
- Not wanted: teachers requesting their school themselves (only the admin
  assigns), a notice when an admin looks in.

### Decided by the maintainer later the same day — overrides the plan below where they differ

- **Nobody is locked out over the legal status.** (1) A school that agreed
  (decision of the Schulleitung and AVV, sent by the teacher, entered by the
  admin on /personen) covers all its students. (2) Until then, a student's
  written consent covers that student; the teacher keeps it, records the
  date on /gruppen and is responsible for it, not the admin. (3) Neither:
  tolerated while it is being clarified, shown as »ungeklärt«. So the P0
  acceptance criteria »student logins blocked« / »pending school's inability
  to enroll pupils« do not apply; login never depends on approval.
- Built: the import approves no school, the home school included
  (`approved_at` only from both dates, `school_approval!` covers home);
  `Roster.from_records` has no approval gate; per-student
  `consent_at`/`consent_by` on the Invitation (`/api/groups/set_consent`,
  date column on /gruppen while the school has not agreed); counts per
  school on /personen; the Elternbrief with consent form at
  `/datenschutz/elternbrief?einverstaendnis=1`; »Schule wechseln« for admins
  (preview, teachers moved along or taken off, blocked when a student is in
  another school's group); »Einladungen ohne Konto« on /personen (remove
  never-used invitations by group, confirmed by their number; teachers never
  count).
- Dev state: the live dump loaded, the invitations imported with this code
  (no school approved, people overview identical to before the import).
  Ernst-Haeckel OS is still under `home`; the maintainer moves it with
  »Schule wechseln« after creating the school. Group rename is not built.
- Still open from the plan, not yet discussed with the maintainer: session
  revocation, cross-school isolation, workshop accounts, school-requested
  affiliation, restricting the document package, the deletion warning,
  school merge.

### New information from the maintainer — overrides older notes below

1. **The invitation import was tested in DEVELOPMENT only. Production has NOT been imported.** Some entries under `home` and the false home-school approval were observed in that development test. Do not treat the development database as production or perform a production import until the fixes are finished and tested. Dev undo/reimport may be used only as part of an intentional, non-destructive test plan; never reset real learner data.
2. **Some groups in the DEVELOPMENT test import were placed under the operator's/home school (`home`) although they belong to other schools.** The maintainer, acting as an administrator, needs a safe UI for transferring individual existing groups to a selected partner school. The same operation must work after the eventual production import; do not guess group assignments.
3. **The home school was shown as approved although it has not approved use of Workspace.** This is a real defect and a release blocker. The code currently pre-approves `home` on import. Never treat import, pre-existing use, or an administrator's assumption as evidence of a Schulleitung decision or an AVV.
4. The maintainer also requires working **withdrawal paths**: a school can withdraw its authorization; a parent/guardian (or eligible learner) can withdraw actual consent where consent is the legal basis, or object/request restriction or erasure where the school/public-task basis applies. Those situations have different legal rules but both need a practical way to stop a particular learner's access when appropriate. Confirm the exact legal handling with the responsible school/DSB; do not falsely describe an Art. 21 objection as Art. 7 consent withdrawal.
5. At **teacher workshops**, visitors need temporary, low-privilege accounts and projector-visible login codes. They may later request regular teacher access; the administrator approves it and school approval remains a separate requirement.
6. Teachers should **select/request their own school**, without this automatically establishing employment, full teacher privileges or the school's consent. The **school paperwork package should not all be public**, though parents need a public privacy notice.
7. The maintainer wants an **email and grace period before routine automatic account deletion**; inaction should mean deletion proceeds, and a keep-account action must not override authorization or retention deadlines.
8. A mistaken school name should be **editable by an admin** (already supported); the admin also needs a safe way to **merge duplicate School entries** representing the same actual school, without merging legal authorizations or content from genuinely different schools.

### P0: Make school approval truthful, explicit and reversible

Current code to inspect: `RosterStore.write_roster_records` in `src/ruby/include/roster_store.rb` sets `home.approved_at` while importing. `Roster.records_from_files` builds imported records with `approved: true`. `Main#school_approval!` in `include/people_page.rb` derives partner approval from `decision_at` and `agreement_at`, but the home school is treated specially. `Roster.from_records` filters students of unapproved schools; changing the importer without changing verification can break the import comparison. `privacy.rb` and `privacy_documents.rb` can then wrongly claim that school authorization already exists.

- Introduce a consistent **pending / approved / withdrawn** school state, with date, recorder, documented basis, and an audit history. An actual verified school decision **and** the required processing agreement/other prerequisites must precede approval. Do not fabricate historical dates, signatures or agreements. The home school must follow the same auditable rules as partners; fix the UI so the administrator can record and withdraw its status.
- For the DEVELOPMENT test, fix any `home.approved_at` assigned solely by import without losing group assignments. For future deployments, provide a non-destructive correction for any legacy auto-approval; do not infer production was imported. The maintainer explicitly confirms **no actual home-school approval yet**.
- Separate **roster import correctness** (names, addresses, groups, teaching relationships) from authorization. The import comparison must still succeed when all schools start as pending, without temporarily granting students access. Teachers may have limited setup access, as intended, but not unrestricted learner access.
- A withdrawal or suspension must stop **new and existing** access to that school's student resources, including long-lived browser sessions, running code-server Workspaces, tests, live-app/share links and direct APIs where relevant. It must not silently delete data needed for orderly export, an objection decision, or required retention. Re-approval must be explicit, audited and must not override a separate learner-level block.
- Public **privacy notices** (`/impressum` and the correct school-specific Datenschutzerklärung), the restricted school document package, school status and welcome mails must describe verified current state, not presumed consent. Do not advertise the home school as approved until it has. See the section below separating public information from teacher-only documents.

**Acceptance:** An import into an unapproved home school preserves all group/member/teacher records and leaves student logins blocked. Pending → approved → withdrawn → approved works, including already-open sessions. No unverifiable legacy `approved_at` is silently trusted.

### P0/P1: Admin UI to transfer already-imported groups to another school

Relevant code: `include/groups_page.rb`, `static/gruppen.html`, `include/people_page.rb`, `include/roster_store.rb`, `include/roster.rb`, and school-group selector on `/klasse`. The importer currently creates `(:Group)-[:AT]->(:School {key:'home'})`; `Invitation` nodes, membership and teaching edges must survive a transfer.

- Add an **administrator-only** action on each existing group, preferably **“Schule wechseln”** in `/gruppen`, with a destination school selector. Display current school, target school, approval state of both, member count, teachers and a clear confirmation. This is **not** a delete/create or redo-import operation. The admin should be able to move one imported home-school group to an existing partner school even if that school is still pending approval. Do not invent school assignment automatically.
- Server-side enforce `admin_logged_in?`, verify group and destination, reject invalid/no-op requests appropriately, use parameterized Cypher and a transaction. Change only the group's `AT` school relationship and intentional audit metadata; preserve `MEMBER_OF`, `TEACHES`, invitation names/email, `welcomed_at`, account IDs, user files, DBs, task progress, exams and submissions. Record who transferred it, when and from/to which school. Refresh the roster cache with `Main.note_roster_change!` and effective permissions.
- **Teacher conflicts:** A teacher's school is stored on `(:Invitation)-[:AT]->(:School)`. Moving a group must not invisibly move the teacher or give a teacher of the old school access to another school's pupils. Show a preview of mismatches. Require an explicit safe reassignment/removal or block the transfer pending resolution; privileged administrators can remain administrators without silently becoming a target school's ordinary teacher. A teacher may currently own groups at multiple schools, so do not blindly move their whole school affiliation. Keep a clear recovery plan for orphaned groups. Respect pending self-selected affiliations and workshop-only users.
- **Student conflicts:** `Invitation`/`User` accounts are globally keyed by email. If one account ends up at two schools, moving a group may allow a new school's teacher to view files originating at the first school (see isolation finding below). Detect and block ambiguous memberships until school-scoped authorization is solved; never transfer student files or delete content as a hidden side effect.
- If the target school is pending/withdrawn, its student members **must remain unable to log in**, even if the source school was approved. Conversely, moving to an approved destination must not implicitly restore someone who has an individual withdrawal/block.
- Update `/gruppen`, `/personen` school counts, `/klasse` group display and per-school document links; include integration/E2E tests for transfer, invalid target, non-admin 403, teacher mismatch, duplicate student, pending destination, concurrency/rollback, school withdrawal and no data loss.

**Acceptance:** The administrator selects a legacy group now under `home`, selects an existing partner school, confirms it, and the group and its membership survive intact; the correct school appears everywhere, and access follows the *actual* target approval and individual blocks.

### P0: Parent/learner withdrawal and individual access block

Do not conflate two legal bases. On an actual **consent** basis (Art. 6(1)(a)), withdrawal (Art. 7(3)) must be honoured. On the **school/public-task** basis (Art. 6(1)(e)), a parent/student can object (Art. 21), seek restriction/erasure, and raise concerns with the responsible school; this is not necessarily an automatic withdrawal of consent that was never legally required. Provide a practical verified-request workflow with the school controller/DSB deciding the outcome, and a prompt temporary suspension if warranted. The school may also withdraw authorization for the entire school.

- Add a minimal per-learner **access suspended/withdrawn** state (with actor, reason class, date, legal basis and audit trail), checked independently of invitations and school approval. A stale invitation or newly re-sent welcome email must **not** recreate or reactivate a blocked account. Explicit authorized reinstatement only.
- On confirmed stop, revoke existing sessions and access to the live Workspace and other entry points; provide safe export/deletion where warranted. Never delete unrelated group or teacher data. Define handling when an account legitimately participates at more than one school rather than deleting the account globally by default.
- Fix `PARENTAL_CONSENT_REQUIRED`: presently it changes the generated statement but does **not** gate account activation. Either implement a genuine verified consent gate for deployments that rely on it, or stop publishing the claim. Do not add an unnecessary blanket parent-consent requirement for the school/public-task route.
- Update the school's information documents and parent-facing contact/withdrawal instructions; ensure the promised alternative to participation is actually available.

**Acceptance:** Neither re-inviting a blocked email nor retaining an old session bypasses an individual stop. Other learners at the same school stay unaffected.

### P1: Workshop attendees, projector login codes and later teacher onboarding

**User workflow:** The administrator hosts an occasional teacher workshop. Participants need real but initially **limited** Workspace accounts; the presenter must be able to show short-lived individual login codes on a Beamer as with a class. Afterward, a participant may decide to use Workspace in school lessons, select or request a school and ask the maintainer to approve their **regular teacher/group-management** access. They must not become a privileged teacher merely by attending a workshop or checking “I am a teacher.” Where feasible, preserve the participant's own files when upgrading instead of creating a duplicate account.

- Model explicit roles/state, e.g. `workshop_attendee` -> `teacher_requested` -> `teacher_approved`, separate from the **school** state `pending` / `approved` / `withdrawn`. `User`/`Invitation.teacher = true`, `@@teacher_emails`, `teacher_account_logged_in?`, `teacher_logged_in?`, role-dependent pages, `save_teacher` and `/gruppen` require review so no workshop user acquires group creation, pupil access, admin view, impersonation, exam-management, or visibility of other participants. A workshop attendee may work in **their own** environment and see appropriately permitted teaching materials, without access to student data or school contracts.
- Add a scoped **Workshop/Event** (or equivalent event cohort) with title, organizer/presenter, attendee invitation list and expiry, separate from a real school's verified student roster. Allow admin (optionally an explicitly delegated presenter) to bulk-invite or check in participants and use an authenticated **projection mode** that displays each attendee's requested one-time six-digit code with a short label/seat number and expiry/countdown. The existing `Main#broadcast_login_codes` skips `@@teachers` and `GroupVisibility.login_codes_for` assumes teacher-owned groups; adapt without exposing codes from unrelated groups or letting all teachers see workshop accounts. No public unauthenticated listing; no logging codes, no caching, no leakage via projector tab/history/screenshots.
- **Threat model:** A projected login code is visible to the whole room. Never project codes for existing full teacher/admin accounts, reused accounts with sensitive records, or student accounts outside the workshop. Workshop credentials should have minimal rights, short code TTL, one-time use, an event/session expiry and an explicit revoke/finish action. If projecting a code for a continuing personal account would give access to private files, use separate restricted event credentials instead. Attendees must privately prove control of their email address before claiming/upgrading to a permanent account, and get a fresh private sign-in after the workshop; do not convert public screen possession into indefinite account control.
- Provide a clear self-service **“Mit meinen Klassen nutzen / Lehrkraftzugang beantragen”** request from the participant's profile. Require verified email and a selected/requested school, with a brief message and request status. Notify the administrator of a new request; provide admin approve/decline with an audit trail, no automatic approval. On teacher approval, regular group administration is enabled **for that school only**. School-level authorization is independent: until the school's use is approved, the teacher may prepare non-personal course settings/group names and print the needed approval documents, but **cannot invite pupils, collect pupil data or begin pupil processing**. Send the appropriate welcome/instructions when approved. Decline leaves only permitted workshop access until its expiry.
- Add expiration/revocation of workshop authorization independent of account retention. Only authorized current presenters can see codes. A participant may opt to keep an otherwise lawful account at the deletion-notice stage, but not extend expired workshop privileges or bypass teacher/school approval. Avoid creating `guests` as a globally approved school that silently grants student or teacher rights.
- Test attendee vs teacher vs presenter/admin permissions; projection filtering and one-time code lifetime; existing full-teacher email; workshop expiration; email verification; pending/approved/declined conversion; pending school's inability to enroll pupils; account continuity and safe post-event session termination.

**Acceptance:** A workshop participant logs in with a projected temporary code to their own limited Workspace; later they request a school/teacher upgrade; only the maintainer can approve class management, and pupils still cannot use the Workspace until the school itself has approved.

### P1: Teacher chooses their school; admin verifies the requested affiliation

- Let a signed-in workshop attendee or prospective teacher find an existing school and **request assignment to it**; if missing, propose a new school name/address. Treat self-selection as an unverified **request**, never as authoritative `(:Invitation)-[:AT]->(:School)` affiliation or as a school's legal authorization. A teacher should not be able to impersonate staff at any listed school by clicking its name. Notify the admin; require admin confirmation of teacher-school affiliation and full teacher role. Existing admin-created teachers remain supported.
- A self-proposed school starts **pending**, with no implicit approval, no signed agreement, and no pupil invitations. Prevent duplicates with normalized name/address suggestions, but do not force a merge merely on a similar name. A teacher can correct a mistaken *pending* request; moving an already approved teacher between schools requires administrator review because their groups and access may change.
- Separate three independent decisions in the UI: (1) **workshop participation** / limited personal use, (2) **admin-approved teacher affiliation and class-management rights**, and (3) **documented school authorization to process pupil data**. Distinguish them visually in `/personen`, profile and `/gruppen`; never make one checkbox grant the others. Requests/decisions must be idempotent, timestamped and audited; define what happens when a teacher withdraws their request or leaves a school.
- A pending applicant should be able to view/print the **school approval forms they need to obtain a decision**, without access to confidential signed agreements or another school's staff/student data. A verified teacher can download the package for their school. Workshop-only participants should not browse arbitrary partner-school documents.

**Acceptance:** Someone can select/request a school themselves, but cannot see other schools' students or create usable learner invitations until an admin approves them **and** the school is authorized.

### P1: School documents — public notices, private administration package

The maintainer **does not want the entire school document package publicly readable**. Separate document access by purpose, instead of making every `/datenschutz` subpage anonymous:

- Keep legally required **Impressum** and the **correct school-specific Datenschutzerklärung/Art. 13 information** publicly available so learners and parents can read them before creating an account. Make their links predictable without leaking invitations or personally identifying teacher rosters. Ask the school DSB to confirm publication/content and required Art. 13 delivery.
- Require authenticated, school-authorized teacher/applicant or administrator access for the **school-leadership decision template, draft AVV, TOMs, record of processing, deletion-policy documents, printable parent letter and similar administrative package**. For pending applicants, expose only the non-confidential *template/proposal documents* needed to seek authorization; restrict executed contracts and internal compliance information further to the school controller and admin. Prevent access by changing `?schule=KEY` or guessing a document slug; enforce on every route/download/print endpoint, not just hide UI links.
- Provide a coherent **“Unterlagen meiner Schule / Drucken”** page in the teacher profile and `/gruppen`, using current selected/verified affiliation. Print views/PDF generation must carry the correct school name, legal basis and signatory and must not pretend `approved_at` exists when it does not. A logged-in teacher should not appear as public contact for all school participants.
- Review `Main#privacy_settings_for`, `Privacy.for_school`, `PrivacyDocuments.path` and the `/datenschutz` routes; verify nested document links, unauthorized 403, parents' access to the public notice, school change, pending applicants and guests. Document any public URL kept for a legitimate reason rather than exposing every draft by default.

**Acceptance:** A parent can read their school's privacy notice without signing in; a verified teacher can print their school's package; strangers and workshop guests cannot browse school contracts or other schools' documents.

### P1: Warn before scheduled account deletion; no action to let it go

**User expectation:** Before a *routine automatic* profile cleanup, email the affected user **once** with a meaningful grace period and simple instructions to keep an **eligible** account and/or export data. If they are happy for the profile to go, they do **nothing**: after the deadline the cleanup proceeds automatically. Do not confuse account retention with authorization to use school/student resources.

- Define eligibility and triggers (end of workshop, ended groups/school year, prolonged inactivity, uninvited accounts); use a configurable, documented grace interval (e.g. 14 days if compatible with the school's legal deletion deadline) and explicit deletion-at timestamp. The notice must not prolong data beyond the lawful maximum period; if a deletion deadline is immediate or already due, do not send a misleading grace-period promise. Consider mandatory immediate action on a verified erasure request as a distinct path.
- Email to the verified address with reason/category, scheduled date, export instructions, a **“Konto behalten”** or extension-request path *only if lawful and still eligible*, and clear text: “Wenn du dein Konto nicht behalten möchtest, musst du nichts tun.” Avoid disclosing more student/school data than necessary in email. Keep a durable `notified_at`, `delete_after`, status and consent/retention purpose, with an idempotent scheduled worker so no duplicate notices, forgotten deletions or perpetual renewals. Retry transient email failures safely and surface admin alerts.
- The keep action should require a safe one-time confirmation or fresh personal sign-in; check eligibility **at the moment of confirmation**, not only when sending the email. If a group/school is withdrawn, a parent stop is accepted, or an invitation is gone, the user cannot restore access by clicking a link: instead offer export (where appropriate), request affiliation/reinstatement via the admin/school, or an extension that does **not** grant school access. Personal work may only remain where a documented lawful purpose supports continued retention.
- At expiry, revoke sessions and running code-server services, safely export only if requested, execute existing complete deletion paths and post-restore deletion ledger; retain minimal audit metadata under a distinct retention policy. Distinguish **workshop-only** accounts, approved teachers and students. Do not destroy active school work because someone missed an email without validating the status; set policy with controller/DSB and honor required legal retention.
- Test: one warning, mail failure/retry, keep while eligible, ignore-and-delete, expired link, revoked school or parent stop, no invitation, account export, backup restore, race with admin status changes, and applicable retention deadline.

**Acceptance:** Routine deletion is automatic after warning and grace; eligible users can keep the account, while no click can revive revoked school/student access or indefinitely bypass the school's deletion rules.

### P1: School rename and careful duplicate-school merge

**Rename already exists:** `POST /api/people/save_school` in `include/people_page.rb` can update an existing school's `name`, and `/personen` already has a pencil/edit action. Preserve and make this functionality obvious, verify `School.key` remains stable, and add tests that teacher affiliations, groups, school-specific document links, status and history still point to the same school after a rename. Avoid creating a new school just to correct spelling. Audit old/new names and actor.

**New merge requirement:** The administrator may later have **two School records created by mistake for the same real school**. Add admin-only **“Schulen zusammenführen”** as an explicitly previewed, confirmed de-duplication operation, *not* a blind merger of two separate legal controllers.

- Choose **source (duplicate)** and **destination (canonical)** school by stable keys; show names, addresses, controller/DSB and regulatory fields, groups and teachers, outstanding teacher requests, approval/document states, and per-school counts. Clearly warn if name/address/legal controller does not match; different actual schools must **not** be merged merely to share access. The admin must attest they are the same real school; otherwise direct them to transfer individual groups instead.
- Dry-run conflict detection: teacher role/affiliation conflicts, users in multiple school groups, global group-name constraints, pending affiliations and workshop upgrades, missing/contradictory metadata, existing signatures and decision/AVV records, `home`/`guests` special cases, school-specific public document URLs. Do **not** equate `source.approved OR destination.approved` with approval for the merged records. Preserve independent historical decisions and require re-verification if the controller or legal basis would change.
- Transactionally repoint all intentionally school-linked `(:Group)-[:AT]->(:School)` and teacher `(:Invitation)-[:AT]->(:School)` relationships, pending requests and other relations once verified safe, without deleting any User, Invitation, Workspace, files, databases, group members, exams, logs or deletion state. Keep an immutable merge audit (actor, time, keys, exact changes) and archive/tombstone the source (ideally redirect old public notice links to canonical school) rather than immediately dropping it and its evidence. Idempotence and recovery for partial application matter. Explicitly guard merging `home` or `guests` unless a separately designed migration is approved.
- Recompute privileges **after** merge (school approval and individual blocks), invalidate caches and old sessions as needed. Preview and post-merge counts must match; test sources with conflicting approvals, no-op, unauthorized non-admin, rollback, concurrent edits and a merge following an earlier group transfer. Provide admin-facing explanation of irreversible consequences and backup/restore plan.

**Acceptance:** Admin can correct a school's name instantly without losing associations, or safely combine two *duplicate records of the same school* after reviewing conflicts; no approvals or permissions are silently inherited.

### Remaining security/privacy review findings to address

1. **P0 — Existing session revocation:** `main.rb` session restoration checks an existing unexpired `Session` and `User`, but not a current invitation/school approval; sessions last `Authentication::SESSION_LIFETIME_DAYS = 365`. Removing a member/teacher or withdrawing school approval must deny access immediately, including non-page entry points. Test with a real preexisting session.
2. **P0 — Cross-school boundary:** `Invitation`/`User` are email-global; `LookIn.allowed?` derives access from group membership. A teacher at School B could see a student's existing School A Workspace if both enrol the same email. Choose proper school-scoped ownership of user content and authorization; until then reject ambiguous cross-school student memberships or provide a strictly controlled migration. Cover impersonation, Workspace, submissions, tests, database access and share links, not just the groups UI.
3. **P1 — Public teacher contacts:** `Main#privacy_settings_for` collects all teacher records of a partner school and `Privacy.for_school` publishes their names/email addresses on public `/datenschutz?schule=...` pages. Provide one intentionally selected school contact (prefer functional email) instead, while allowing the logged-in teacher to sign an individual Elternbrief.
4. **P1 — Retention:** End-of-school-year cleanup still depends on teachers remembering to end courses and administrators manually deleting uninvited accounts. Add group end dates, reminders, review windows and enforceable retention/deletion safeguards, including for abandoned groups; permit documented extensions for continuing projects.
5. **P1 — Restore/deletion ledger:** `main.rb` writes `/internal/deleted-accounts.jsonl`, and `repeat_deletions` is manually invoked. A restore may overwrite the ledger; hashes depend on `LOGIN_CODE_SALT`. Keep the ledger protected outside restored snapshots, use a stable secret/key, verify the log survives restoration, replay before opening service, and test partial failures/pruning.
6. **P1 — Export:** In `main.rb`, `FileUtils.chmod_R('a+rX', dir)` broadens permissions on a sensitive temporary export. `mysqldump` and `neo4j_bolt dump` exit statuses aren't checked, so an incomplete archive could be delivered. Use private minimal privileges, verified subprocess exit status, limits/timeouts, and failure tests.
7. **P1 — Legal route:** Have the school/DSB confirm whether Workspace (groups, submissions, progress, tests) is a digital teaching tool under **§ 2 DigLLV** or qualifies as a third-party **Lernmanagementsystem under § 3 DigLLV** with additional approval/risk assessment obligations. Treat this as an unresolved legal classification, not a proven defect.
8. **P1 — Documentation must match reality:** Verify actual German/EU hosting and backup locations, Hetzner and mail-provider AVVs/DPAs, Traefik/nginx/webhosting logs and backup age; do not say processor agreements exist unless signed. Link student welcome mails to the correct school's public privacy notice. Do not claim parental consent gate or immediate post-restore deletion unless actually enforced.
9. **P2 — Admin look-in:** Admins can inspect without a stated reason and without a student-visible event; earlier maintainer preference was to leave this as is. Consider a minimal administrative audit, but discuss changes in policy rather than silently altering it.

### Test and delivery checklist for the next session

- Inspect present branch and actual imported roster before coding. Do not treat old handoff snapshots as fresh environment state. Work on fake fixture schools or a copy; do not put real student identities in tests, commits or logs.
- Prioritize **truthful approval state + existing-access revocation + cross-school isolation + safe admin group transfer**. Then implement workshop onboarding/presenter codes and teacher affiliation requests, followed by school-doc access controls, withdrawal handling, school de-duplication and retention emails. Avoid shipping access-granting features before effective permissions are safe; parallelize tests/docs appropriately.
- Add tests for: dev-only imported home school not approved; real approval evidence; reversal/reapproval; 365-day old session after withdrawal; move group home → pending partner; teacher mismatch; cross-school duplicate email; rejected non-admin; projector-only limited workshop account and conversion request; school rename/duplicate merge conflicts; document authorization; warning/grace/keep/ignore account deletion; no data loss, backup restore and export failures.
- Update the code, generated Datenschutzerklärung/AVV/Elternbrief, `docs/datenschutz.md`, `docs/teacher-access.md`, E2E tests and CHANGELOG together. Run all Ruby tests (`cd src/ruby; for f in test/*_test.rb; do ruby "$f" | tail -1; done`) and targeted E2E tests; document anything not executable.
- Summarize changed files, commits, tests, remaining legal/provider checks, and exact safe admin steps for transferring imported groups, verifying teacher upgrades, merging duplicates and auditing status. **Never deploy, merge the Git branch, import production data, undo a live import, delete data or alter live school approvals without the maintainer's authorization.**

---

---

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
