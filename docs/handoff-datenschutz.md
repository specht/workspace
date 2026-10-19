# Hand-off: data protection, roster, access rules, operations

Updated 2026-10-19. Replaces every earlier version of this file (the old
action plan of 2026-10-18 and its status notes are in the git history;
everything in it is either done or decided against below).

Read first: `AGENTS.md`, `docs/datenschutz.md` (what is stored, the
documents, deletion), `docs/teacher-access.md` (roles, /gruppen, /personen,
the import), `docs/entwicklung.md` (»Der Docker-Wächter«, »Kapazität
messen«, »Betrieb überwachen«, »Workspace-Image«), `e2e/AGENTS.md` before an
E2E test. `CHANGELOG.md` »Unreleased« lists every change in German.

## Git state

- Branch `datenschutz`. `origin/main` is at 6429b4ee (Docker guard,
  capacity script), merged and pushed on the maintainer's request.
- On `datenschutz`, **not merged, not pushed** (wait for the maintainer):
  - bc02ce37 E2E tests for the access rules
  - 3d597d87 /admin »Betrieb« (monitoring)
  - c0334ade container hardening (needs an image rebuild, see below)
  - bf1f7c16 »Betrieb« shows the running commit and the image's build date
- Stage files by name, never `git add -A` (a swap file of `credentials.rb`
  was once committed that way and had to be filtered out before pushing).

## When the maintainer deploys the four commits

In this order, on the server:

1. `git pull`, then `./build-image.sh` **before** the Ruby service picks up
   c0334ade: the new Workspaces drop NET_RAW, and with the old image `ping`
   does not start (it carried a file capability; the new image strips them).
   /admin »Betrieb« warns about an image older than the hardening
   (label `de.hackschule.file-capabilities`).
2. `./config.rb up -d` (the Ruby service now mounts `./.git` read-only for
   the commit on »Betrieb«).
3. A borgmatic hook that touches `<DATA_PATH>/internal/backup-done` after
   each successful backup (exact YAML for borgmatic 1.x and 2 in
   docs/entwicklung.md, »Betrieb überwachen«); until then »Sicherung« shows
   »nicht eingerichtet«. Optional `BACKUP_MAX_AGE_HOURS` (default 26).
4. Already asked earlier, still to confirm: after lessons,
   `./config.rb logs docker_guard | grep refused` (a refusal means the
   policy misses something the Workspace needs); run
   `src/scripts/measure-capacity.rb` outside lessons and decide on a cap
   for parallel starts from its report in `/internal/capacity-*.txt`.

## Binding decisions of the maintainer

- **Nobody is locked out over the legal status.** A school that agreed
  (Schulleitung decision and AVV, entered by the admin on /personen) covers
  its students; until then a student's written consent (kept by the teacher,
  dated on /gruppen) covers that student; neither: tolerated, shown as
  »ungeklärt«. Login never depends on approval. Stopping one person is
  `suspend_access` on /personen.
- Only `/impressum` is public. School documents are PDFs or one ZIP behind
  a login; a partner school's Datenschutzerklärung goes with each welcome
  mail as a PDF, after a preview.
- Guests are adults (workshops, companies, former students); they agree to
  the Nutzungsordnung once. Minors only come through their school. Several
  guest groups are fine; the same person may be in several.
- Long inactivity: an offer to announce a deletion (two weeks' notice, mail,
  data takeout, keep as guest), no automatic deletion rules.
- No school merge (move groups, delete the empty school). Backups as they
  are (a restore would be from the day before; `repeat_deletions` after it).
- Administrators give a reason too when they open a Workspace or imitate
  someone; the person sees it; /internal/admin-access.jsonl for a year.
- Administrator accounts cannot be deleted, by no path.
- The roster import is final (no way back to the files).
- The account is created with the first login request (the texts say so).
- Sessions stay 365 days (class rooms); »überall abmelden« in the profile.
  Login codes stay readable: teachers project them.
- DigLLV: argued as a teaching tool under § 2 (no communication,
  organisation or grades); the school decides.
- Never touch the backups (no borgmatic commands, no prune: »you have no idea
  where my backup is«). Production dumps never in the repository.

## What exists (pointers, details in the docs)

- Access rules in one place: `src/ruby/include/access.rb`
  (`may_use_workspace?`: suspended → no; admin → yes; invitations not loaded
  → no; invited or test account → yes; deletion announced → yes until the
  day). Sessions, nginx map, share links, live apps and watch links all ask
  it. E2E: `e2e/tests/access-rules.browser.spec.ts` (logs in as the first
  `ADMIN_USERS` address or `E2E_ADMIN_EMAIL`, made-up schools/groups/accounts,
  deleted afterwards; needs the imported roster).
- Roster in Neo4j: School, Group, Invitation, Setting `roster`;
  `include/roster.rb`, `roster_store.rb`, `groups_page.rb` (/gruppen),
  `people_page.rb` (/personen). Every write calls `Main.note_roster_change!`
  (reloads invitations, refreshes nginx when someone lost access). `_*.txt`
  files in `/invitations` (the E2E pool) are still read after the import.
- Privacy texts: `include/privacy.rb` (facts, checked by tests),
  `privacy_documents.rb`, PDFs with weasyprint (`Main.pdf_font_css` loads
  only IBM Plex Sans; otherwise 10 s per PDF).
- Docker guard: `docker/docker-guard/guard.rb`, rules in
  `include/docker_policy.rb` (tested). The backend has no Docker socket.
- Container hardening: `WorkspaceRuntime::DirectDocker::HARDENING`
  (keeps CHOWN, DAC_OVERRIDE, FOWNER, KILL, SETGID, SETUID for the s6 init;
  `no-new-privileges`). The image strips file capabilities;
  `build-image.sh` checks `ping` as a student. The capacity script uses the
  same flags.
- Monitoring: `include/health.rb` (judges, tested) and
  `include/server_health.rb` (gathers: git, image label, backup stamp, start
  history in memory, queue, probes with 3 s timeouts, disk, hourly jobs);
  mails once a day per broken item via `/internal/health-alerts.json`; the
  disk has its own mail (`check_disk_space`). The hourly thread in
  `main.rb` runs deletions, share expiry, disk, group reminders, then
  `check_health` (first run 60 s after start in development).
- `client_ip`: REMOTE_ADDR; X-Forwarded-For only from nginx; a Workspace
  counts as itself (X-Hs-Peer).
- CI: `.github/workflows/ruby-tests.yml` (Ruby tests only).

## Open, for the maintainer to decide (assessments)

1. **Access per school** (review point 5): accounts are global per address;
   adding a student of another school and moving a group across schools are
   refused, the cross-school report lists old cases. Real separation would
   be a larger rebuild.
2. **Workspaces reach nginx, ruby, mysql and neo4j on every port**
   (`peer-firewall.sh`, »infrastructure«). Worth a test from inside a
   Workspace that the MySQL and Neo4j root logins are refused there.
3. **More hardening**, not done: read-only root file system, user
   namespaces, an own seccomp profile. Each needs trying against the full
   E2E suite (the s6 init runs as root and writes).
4. **main.rb** (~7,800 lines): move routes by topic into `include/` as was
   done for groups, people, access and health.
5. **Learning paths** on the start page (a route for beginners, a first
   lesson for teachers): a content question.
6. **A restore test of the backup**: on the server, by the maintainer.
7. A server that is down entirely is invisible to »Betrieb«: an outside
   uptime check.
8. Legal and providers, by the maintainer: AVVs with Hetzner (he was filling
   in Hetzner's form) and united-domains; the AVV draft says they exist.
   `COST_NOTE` (a suggestion was written; check whether it is set in the
   live `credentials.rb`). The school's DSB name. `borgmatic rlist`: the
   oldest archive must fit the months on the page.
9. Production roster: the import was rehearsed on dev with the live dump;
   ask the maintainer whether production has been imported before assuming
   either way. Ernst-Haeckel OS groups were to be moved with »Schule
   wechseln« after creating that school.

## Working on the dev site

- `http://workspace.test:8025`, admin login with an `ADMIN_USERS` address
  and code `123456`; mails are only logged in development
  (`./config.rb logs ruby`).
- The dev database holds the live dump: use made-up addresses
  (`…@example.com`) and clean up; never real identities in tests or logs.
- Saving a file under `src/ruby` restarts the dev server; do not edit there
  while an E2E run is going.
- The dev disk is nearly full (/home a few GB; Docker on /mnt/docker); ask
  before pruning anything, and never anything to do with backups.
- Full E2E: `cd e2e && npm test` (about 16 minutes, 65 tests, all passing on
  2026-10-19 with the hardened image). One file:
  `npx playwright test --project=workspace-smoke --no-deps tests/FILE`.
- Ruby tests: `cd src/ruby; for f in test/*_test.rb; do ruby "$f" | tail -1; done`
  (all passing).

## Notes on the design

- `Privacy.settings` carries `:school_name`, `:school_address` and
  `:contacts` next to the operator's values; `for_school` replaces them.
  Texts about »the school« use the school keys, texts about the operator
  the operator keys.
- `PrivacyDocuments.path(s, slug)` keeps `?schule=KEY` on every link
  between the documents.
- `respond` does not halt; `api_call` in code.js removes only
  `.please-wait` (pages load with `no_please_wait`).
- The admin page's WebSocket reconnects by itself (1, 2, 4 … s, at most 30;
  at once when the tab is shown again).
