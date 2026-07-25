# BIF Authoring Tools

This extension is deliberately only a launcher. It contains no BIF parser, analyzer, diagnostics implementation, story watcher, or browser-publication logic.

When a **trusted** workspace folder contains this marker at its root:

```text
.bif-project
```

the extension:

1. checks whether the repository dependencies need installation;
2. runs `npm install --no-audit --no-fund` when necessary;
3. starts `npm run dev` in that workspace-folder root;
4. keeps the long-running command in a managed VS Code task;
5. avoids duplicate tasks and stops its task when the folder or extension closes.

All actual BIF behavior stays in the student's repository clone. Updating the repository updates the analyzer and watcher without changing this extension.

## BIF repository requirements

The repository root must contain:

```text
.bif-project
package.json
```

The marker can be empty and should be committed to Git. `package.json` must define the development command, for example:

```json
{
  "scripts": {
    "dev": "node tools/publish-analysis.js --watch"
  }
}
```

## Dependency installation

For repositories that declare dependencies, the extension checks the installed top-level packages and npm's own `node_modules/.package-lock.json`. It runs installation when a package is missing or when `package.json`, `package-lock.json`, or `npm-shrinkwrap.json` is newer.

When installation is required, one managed task runs:

```bash
npm install --prefer-offline --no-audit --no-fund && npm run dev
```

Otherwise the same task starts directly with `npm run dev`.

## Workspace Trust

The extension is disabled in untrusted workspaces. This is necessary because both `npm install` and `npm run dev` execute code from the opened repository.

## Local build

No Marketplace tooling is required:

```bash
npm install
npm test
```

For automatic deployment, install or copy the compiled extension directory containing at least:

```text
package.json
dist/extension.js
dist/core.js
resources/icon.png
README.md
CHANGELOG.md
LICENSE
```

The extension identifier remains `gymnasiumsteglitz.bif-authoring-tools`.
