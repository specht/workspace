# End-to-end tests

These tests exercise the local Hackschule Workspace through a real browser:
login, reset a disposable workspace, launch code-server, and interact with it.

The test suite creates its disposable invitation pool automatically in
`data/invitations/_e2e.txt`. That directory is used only by local development
and is already ignored by Git.

## First-time setup

Start Workspace from the repository root as usual:

```bash
./config.rb up -d
```

Then install Playwright:

```bash
cd e2e
npm install --save-dev @playwright/test
npx playwright install chromium
```

Commit the resulting `package-lock.json` as well.

## Run

```bash
npm test
```

Open the HTML report:

```bash
npm run report
```

Every test is broken into named `test.step(...)` phases. Important phases have
screenshots attached, and every run records a complete Playwright trace. The
HTML report therefore shows both the high-level tutorial steps and the detailed
browser actions inside them.

## Writing tutorial tests

Tutorial tests should normally use the `freshWorkspace` fixture:

```ts
import { test } from './fixtures';

test('my tutorial', async ({ freshWorkspace: workspace }, testInfo) => {
  // workspace is a freshly reset, logged-in and fully loaded VS Code page.
});
```

The fixture performs the common lifecycle for every test:

1. log in with the worker's disposable E2E account;
2. reset that user's server, files and VS Code profile;
3. launch the workspace;
4. wait until the VS Code workbench is ready.

Reusable browser-level VS Code helpers live in `tests/vscode.ts`. They cover
creating/saving text files, replacing editor contents, installing extensions,
opening the integrated terminal, normal commands and interactive commands.
`tests/tutorial.ts` provides `readTutorialFile(...)` so tests can reuse the
actual example files from `src/content`. Keep tutorial-specific assertions in
the corresponding `*.spec.ts` file.

The FORTRAN test reads `hello.f90`, `factor.f90` and `bubblesort.f90` directly
from `src/content/fortran`, so the E2E test always types the exact source code
shown by the tutorial rather than maintaining duplicate copies.

## Parallel runs

The suite defaults to one worker. Each concurrent worker gets an independent
disposable account based on Playwright's `parallelIndex`:

```text
e2e-0@example.com
e2e-1@example.com
...
```

For example:

```bash
E2E_WORKERS=4 npm test
```

Eight E2E users are created by default. To increase both the pool and the
number of workers:

```bash
E2E_USER_COUNT=16 E2E_WORKERS=16 npm test
```
