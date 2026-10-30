# Git repositories for groups, in the Workspace itself

A plan, for the maintainer to read before anything is built (27 October
2026). Pages with `class_repo: group` (the sandboxes, the class quiz) need a
repository the whole group can push to. Today that is an outside Git server:
the teacher creates the repository there, invites every student, and the
students register an SSH key or a token. A teacher without such a server
cannot use these pages with Git at all. The idea: the Workspace keeps the
group's repositories itself.

Decided so far: no GitLab or other forge (too heavy to run and maintain);
plain Git, which speaks to a remote over several transports (a path, SSH,
HTTP). Plan first, then build. The maintainer chose SSH as the transport,
then suggested a directory mounted into the students' Workspaces as the
remote; both are below, with what each costs.

## What both have in common

- **Where:** `data/git/<group id>/<name>.git` on the host, bare repositories,
  branch `main`. The group's id, not its name, so a renamed group keeps its
  repositories.
- **Who creates them:** a teacher of the group, on the class page, in the
  dialog behind »Git« that today takes outside addresses: »Im Workspace
  anlegen« next to each page with `class_repo: group`. An activity with a
  folder `starter/` (the class quiz) fills the new repository with a first
  commit from it; the sandboxes start empty. The address then lands in
  `GroupSetting.class_repos` like an outside one, so `${class_repo}` and the
  `git` checks work unchanged.
- **Protection in Git:** every repository gets `receive.denyNonFastForwards`
  and `receive.denyDeletes`: `git push --force` and deleting `main` are
  refused, which the class quiz turns into a lesson.
- **Snapshots:** the server writes `git bundle create --all` for each
  repository whose refs changed, at most once an hour, and keeps the last
  seven per repository in `data/git-snapshots/`. A teacher restores one with
  a button. This is what saves a repository someone has damaged.
- **For teachers:** the list of the group's repositories with size and last
  push, download as zip (`git archive`), restore a snapshot, delete.
- **Browsing on the site:** members and teachers of the group read a
  repository in the browser, read-only, at `/git/<group>/<name>`: the files
  of any commit or branch (code highlighted as on the site's pages, Markdown
  rendered), the history (`git log` with author, date and message, also for
  one file), a commit with its diff, and the branches. Wanted by the
  maintainer (27 October 2026): students should see the history of their
  shared work without a terminal, and the review in the class quiz can link
  a commit. Built into the site rather than with cgit: one login, one look,
  access by group membership as everywhere else. The backend reads the
  repositories read-only with fixed `git` commands (`ls-tree`, `cat-file`,
  `log`, `show`, `for-each-ref`), never with input passed to a shell; a
  path or a commit from the address is checked first (`rev-parse --verify`,
  no `..`, no option). Large files and diffs are cut with a note.
- **Limits:** a soft limit per repository (100 MB), shown to the teacher; a
  push is not stopped by it.
- **Exam mode:** no access, as for everything with a network.
- **Pages:** a new condition `class_repo_local` beside `class_repo_ssh` and
  `class_repo_https`; with it, the steps for an outside server (SSH key or
  token, invitation, the account's e-mail address) are left out. Name and
  e-mail in commits: any; `git config` stays as it is.
- **Switched off at first:** a value in `credentials.rb`
  (`GIT_GROUP_REPOS = false`); a CHANGELOG line with the new data folders.
- **What is stored:** students' work and the names they put in their commits
  are stored on the server as long as the group exists. `privacy.rb`,
  docs/datenschutz.md and the deletion of groups change with it. Deleting a
  group deletes its repositories and snapshots. Deleting one account leaves
  the group's repositories as they are, as group work; the DSB should agree
  (open question).
- **Backups:** whether `data/git/` and `data/git-snapshots/` go into the
  backups is the maintainer's decision; nothing here touches borgmatic.

## Way 1: a directory mounted into the Workspaces

Every Workspace runs as the same user (uid 1000), so a shared directory is
writable for every member without any permission work. At start, each
Workspace gets the directories of its person's groups:
`-v data/git/<id>:/git/<group>`; the address is a path,
`git clone /git/7b/quiz.git`. Git pushes to a path as it pushes to a server.

- **Simple:** no service, no keys, no tokens, no network.
- **A new group or a new repository needs a restart** of the Workspace,
  because mounts are set when it starts (for the overlays below, also a new
  repository).
- **The catch: code of one student running in another's Workspace.** With a
  path as remote, `git push` runs `git-receive-pack` and `git fetch` runs
  `git-upload-pack` in the Workspace of the person who pushes or fetches,
  and they read the shared repository's `config` and `hooks/`. Anyone in the
  group could put a hook there, or `core.fsmonitor` (a command) into the
  config, and it would run in a classmate's Workspace, as that classmate,
  with their files. This must be closed: `config` and `hooks/` of every
  repository are mounted read-only on top of the writable directory (a
  mount point cannot be replaced by renaming another file onto it). Then
  the server alone writes them.
- **Anyone can damage the data:** `rm -rf /git/7b/quiz.git`, or writing into
  `refs/`, bypasses everything Git checks. The snapshots bring it back; the
  class loses at most an hour.

## Way 2: an SSH service container

A small container with `sshd` and Git, like the server of the crime story
(`museum.bitstein`): in the network of the Workspaces, reachable from them
only on port 22, reaching none of them (the peer firewall's label for such
servers). One Unix user `git`; the address is
`git@git.workspace:7b/quiz.git`.

- **Keys:** a student registers the public key of their Workspace
  (`~/.ssh/id_ed25519.pub`, which the sandboxes already ask for) with a
  button on the page; the server reads it from the Workspace and stores it
  with the account. `sshd` asks the backend for a key
  (`AuthorizedKeysCommand`) and gets the account with a forced command; a
  small gate checks that the account is a member or teacher of the group
  and runs `git-shell` for that one repository.
- **Everything runs on the server's side,** as `git`: hooks and config are
  out of reach of students, deleting or rewriting a repository is not
  possible, `denyNonFastForwards` holds for everyone. No restarts: a new
  repository or group works at once.
- **More to build and run:** a container, the gate, the key endpoint in the
  backend, a firewall label, and keys as one more kind of data per account.
- **Closer to the real thing:** students meet SSH keys and a server address
  as they would with any Git host.

## Comparison and recommendation

| | Directory | SSH container |
| --- | --- | --- |
| Parts | mounts at start, read-only overlays | container, gate, key endpoint |
| Student setup | none | an SSH key, once |
| New repository usable | after a restart of the Workspace | at once |
| Code of one student in another's Workspace | possible unless config and hooks are read-only | not possible |
| Repository damaged by a student | possible; snapshots repair | not possible |

Recommendation: **the SSH container.** It is more work once, but a
repository shared by twenty students should not depend on each of them
leaving its files alone, and the read-only overlays that make the directory
safe are exactly the kind of detail that breaks unnoticed. The directory is
the better choice only if setup for students must be zero.

## Decided and built (27 October 2026)

- **The SSH container**, closer to how Git servers really work. Built as
  docs/teacher-access.md »Repositorys im Workspace« and docs/entwicklung.md
  »Der Git-Server der Gruppen« describe it; the repositories live in
  `data/git-repos`, not `data/git` (the backend's container mounts the
  site's own `.git` at `/repos`).
- **Keys:** students create a key pair in their Workspace and hand in the
  public key on the page, as on any Git server, and can take it back. The
  Workspace never makes a key (the maintainer's decision, 30 Oct 2026; it
  could once set one up for a whole group, `~/.ssh/workspace_git`, and keys
  made then stay valid until taken back). Pages have the variant
  `class_repo_local_keys`.
- **Browsing:** built into the site as planned, with the maintainer's wish
  for a light GitHub: files, history (also of a file), commits with their
  diff, branches, a zip.
- **Backups:** borgmatic takes all of `data/`; nothing to do.
- **No snapshots for now:** through SSH no student can damage a
  repository (no force push, no deleting a branch, no access to the files),
  so the hourly bundles are left out. A teacher can still delete a
  repository; the backups hold it.
- **No size limit per repository yet;** a single push is limited to 50 MB
  (`receive.maxInputSize`).
- **Moving repositories** from a Git server elsewhere is the maintainer's
  own work (`git push --mirror` into a new empty repository).

- **Free repositories of a group (28 October 2026):** a teacher makes one
  on `/repos` with »Neues Repository«, in the group's folder, not tied to an
  activity (its name may not be an activity's with a class repository);
  members and teachers reach it like a class repository, with the group's
  keys. Deleted on `/repos` after typing its name.
- **Own repositories for practice (28 October 2026), private to their
  owner, decided by the maintainer:** every person may keep up to
  `GitServer::PERSONAL_MAX` (10) repositories in a folder of their own
  (`User.git_id`). Only the owner may clone, push and read them on the site;
  not a teacher of the owner's group, not another student, and the
  administration only sees that they exist (owner, name, size, date) and may
  delete one, without reading it. **Why:** practice is private; shared work
  belongs in the class repositories, which teachers see. A later change must
  not open them to teachers by accident (`GitServer.access_map`,
  `GitServer.may_read?`, tested in `git_server_test.rb`). They go with the
  account (`Main#delete_account`).

## Open questions

- Account deletion: a deleted student's commits stay in the group's
  repositories as group work (so the Datenschutzerklärung says). Whether
  the name must be removed is for the DSB (docs/handoff-datenschutz.md).
