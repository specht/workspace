# Changelog

## 0.6.0

- Shows added and removed project bytes for every checkpoint.
- Counts complete old and new blob sizes for modified files, including binary assets.
- Adds byte totals to the friendly comparison tab.
- Adds configurable warnings for unusually large checkpoints.
- Adds a protected “Delete all checkpoints” command in the Checkpoints view menu.
- Deletion requires a modal warning followed by typing `LÖSCHEN` exactly.
- Deleting checkpoints leaves the current files and ordinary Git history untouched.

## 0.5.1

- Prevents restoring a checkpoint when the current project already has the same tree.
- Prevents nested “Zurück zu …” labels by resolving restore entries to their original checkpoint.
- Continues first-checkpoint creation after initializing a new Git repository.

## 0.5.0

- Groups checkpoints by local calendar day, including the weekday.
- Shows only the local time on checkpoint rows.
