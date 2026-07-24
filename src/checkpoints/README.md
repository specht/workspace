# Hackschule Checkpoints

A child-friendly VS Code extension for creating, comparing, and restoring local
project checkpoints. It uses Git internally without exposing branches, staging,
commits, remotes, or merges to students.

## Student interface

Open **Checkpoints** from the left Activity Bar.

- `+` creates a checkpoint.
- An empty name is allowed and becomes `Checkpoint 1`, `Checkpoint 2`, etc.
- The diff icon compares an older checkpoint with the current saved project.
- The restore icon explicitly restores a checkpoint.
- Clicking a row only selects it; it does not restore anything.

Before creating or comparing a checkpoint, the extension requires all editor
changes to be saved. It offers **Alle speichern und fortfahren**.

If a folder is not yet a Git repository, the extension asks once before running
local `git init`.

## Linear checkpoint history

Restoration never moves history backwards and never removes later checkpoints:

```text
A → B → C → Zurück zu A → weitere Arbeit
```

When the current saved files differ from the latest checkpoint, the extension
first creates an automatic safety checkpoint named, for example:

```text
Vor dem Wiederherstellen von „Anfang“
```

It then continues the requested restoration automatically.

## Git isolation

Checkpoints are stored under:

```text
refs/hackschule-checkpoints/current
```

A temporary Git index is used through `GIT_INDEX_FILE`. The extension does not
change the current branch, `HEAD`, the ordinary staging area, remotes, normal
commits, or push/pull configuration. Ignored files remain untouched.

These checkpoints are local to the repository's `.git` directory. They protect
against editing mistakes but are not a remote backup.

## Build the extension

Requirements:

- Git
- Node.js 20 or newer
- npm
- VS Code or a compatible VS Code server

From this directory:

```bash
npm install
npm run check-types
npm test
npm run compile
```

To test it interactively, open this directory in VS Code and press `F5`. This
starts an Extension Development Host using `.vscode/launch.json`.

Create the installable VSIX with:

```bash
npm run package
```

The result is:

```text
hackschule-checkpoints-0.5.1.vsix
```

Install or replace it with:

```bash
code --install-extension hackschule-checkpoints-0.5.1.vsix --force
```

Then run **Developer: Reload Window**.

Because the previous prototype used the package name
`hackschule-project-checkpoints`, uninstall that prototype before installing
this renamed extension:

```bash
code --uninstall-extension hackschule.hackschule-project-checkpoints
```

The hidden Git checkpoint reference remains unchanged, so existing checkpoints
inside projects remain available.

## Useful scripts

```bash
npm run check-types  # TypeScript validation
npm test             # source/package assertions
npm run compile      # build dist/extension.js
npm run watch        # rebuild while editing
npm run package      # validate, test, compile and create VSIX
```

## Restore safeguards

- Restoring a checkpoint whose file tree already matches the current project is
  a no-op and creates no additional history entry.
- Restoring an earlier restore entry resolves its original named checkpoint, so
  labels never accumulate as `Zurück zu „Zurück zu …“`.
- Preparing a new folder with `git init` continues the original command and
  creates the first checkpoint immediately afterward.

## Day grouping

The sidebar groups the linear checkpoint history by local calendar day. Each
header includes the weekday and date, while checkpoint rows show only their
time. Day groups are expanded by default.

## Byte statistics

Each checkpoint row shows how much project content was added and removed compared
with the preceding checkpoint:

```text
Checkpoint 3   10:42:17 · +2,4 MB −18 KB
```

For a modified file, the full old blob size counts as removed and the full new
blob size counts as added. This also works for images and other binary files.
A pure rename with unchanged content contributes zero bytes.

These figures describe changed **project content**. They are not the exact
physical size occupied inside `.git`, because Git reuses, compresses and packs
objects.

Before creating an unusually large checkpoint, the extension warns by default
when one of these limits is reached:

- 250 MB added or changed data
- 2,000 affected files
- one new or changed file of at least 100 MB

The thresholds are configurable under `hackschuleCheckpoints.*` settings. A
value of `0` disables that threshold.

## Delete all checkpoints

The Checkpoints view's `…` menu contains **Alle Checkpoints löschen…**. This
removes only the private ref:

```text
refs/hackschule-checkpoints/current
```

The current project files, normal branch, staging area, commits and remotes are
not changed. The operation requires a modal warning and typing `LÖSCHEN`
exactly.

Git may reclaim the now-unreachable object data later during automatic garbage
collection. The extension deliberately does not run an aggressive
`git gc --prune=now`, because that could also remove unrelated recovery objects
from normal Git work.
