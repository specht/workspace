# Changelog

## 0.5.1

- Do not create another restore entry when the current project already matches
  the selected checkpoint.
- Resolve restore entries to their original checkpoint so restore labels do not
  accumulate repeated “Zurück zu” prefixes.
- Continue the original create action immediately after initializing a new Git
  repository.

## 0.4.0

- Made the comparison tab student-friendly.
- Added a summary of changed files and added/removed lines.
- Replaced Git-internal headers with clear file sections.
- Added explanations for added, removed, new, deleted, renamed, and binary files.
- Renamed comparison tabs to “Vergleich mit …”.
- Creating a checkpoint is now a no-op when nothing changed since the latest checkpoint.

## 0.3.0

- Renamed the extension to Hackschule Checkpoints.
- Added automatic names for checkpoints with an empty message.
- Required saved editor contents before creating or restoring checkpoints.
- Fixed dirty restore so the safety checkpoint is followed by the requested restore.
- Removed duplicate cancel buttons from modal dialogs.
- Added a colored comparison tab for every checkpoint.

## 0.2.0

- Added the Activity Bar container and checkpoint sidebar.
- Added explicit restore actions and optional Git initialization.

## 0.5.0

- Grouped the linear checkpoint history under one expandable header per local calendar day.
- Day headers include the weekday and full date.
- Checkpoint rows now show only the local time because the date is supplied by the parent header.