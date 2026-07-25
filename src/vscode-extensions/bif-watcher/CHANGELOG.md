# Changelog

## 0.2.0

- Removed the bundled BIF analyzer, diagnostics adapter, watcher, and publication logic.
- Added root-level `.bif-project` detection.
- Added conditional repository dependency installation.
- Runs the repository's own `npm run dev` as a managed background task.
- Prevents duplicate tasks and stops tasks with the workspace lifecycle.
- Disabled activation in untrusted workspaces.
