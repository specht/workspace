# AGENTS.md — Hackschule Workspace E2E

The E2E suite deliberately has two layers.

## 1. Browser smoke test

Use Playwright/browser automation only for the boundary that actually requires
a browser:

- log in as a disposable E2E student;
- reset the student's Workspace;
- launch Workspace through the real application;
- verify that code-server reaches a visible `.monaco-workbench`.

Do not replay complete programming tutorials through Monaco. Earlier attempts
became tests of Monaco selection, auto-indent, clipboard behavior, Quick Pick
timing, xterm rendering, and keyboard focus instead of tests of Workspace.

Each browser worker intentionally leaves its `hs_code_*` container running.
The toolchain project depends on the smoke project, remains serial, and reuses
e2e-0's container after every browser worker has finished.

## 2. Toolchain/tutorial smoke tests via docker exec

Toolchain tests do **not** request Playwright's `page` or `browser` fixture.
They execute commands inside the real container that the browser smoke test
already caused Workspace to create.

This is important: do not create a second ad-hoc Docker container for these
tests. We want Workspace's real launch path to have configured:

- the actual image;
- mounts;
- uid/gid;
- environment variables;
- networks/DNS;
- HOME;
- database-related environment;
- any startup-time initialization.

`WorkspaceContainer` derives the running container name from the E2E email
using exactly the same SHA-256 -> base36 algorithm as `main.rb`.

## Test isolation

Do not delete or replace `/workspace` while code-server is running.

A previous manual deletion of a user's mounted Workspace directory left a
running container with a stale bind mount and produced:

```text
current working directory is outside of container mount namespace root
-- possible container breakout detected
```

The live Workspace also keeps code-server state under `/workspace`, including
`.local` and potentially `.extensions`.

Therefore command-line tests use:

```text
/workspace/.e2e
```

as a disposable sandbox.

The `workspaceContainer` fixture deletes and recreates only that directory
before every toolchain test. All test programs, temporary extension dirs,
build artifacts, clones, etc. belong underneath it.

Run toolchain commands as uid/gid `1000:1000`, matching the Workspace user.
Root is used only to reset/chown the test sandbox.

## Actual tutorial files

Read examples directly from `src/content` with `readTutorialFile()` and write
them into `/workspace/.e2e`.

Do not maintain duplicate source programs in E2E tests.

For example, the FORTRAN test uses:

```text
src/content/fortran/hello.f90
src/content/fortran/factor.f90
src/content/fortran/bubblesort.f90
```

## What toolchain tests should check

Prefer major, stable boundaries that can break after image/software updates:

- required command exists;
- compiler/interpreter can run;
- required VS Code extension can still be obtained;
- required Git repository still resolves;
- actual tutorial source compiles/runs;
- major input/output behavior works.

Avoid checking editor keystrokes, exact compiler wording, terminal prompts, or
pixel layout.

### Extensions

Check extension availability with code-server's CLI inside the launched
Workspace container, using fresh directories under `/workspace/.e2e`.

A test must not pass just because the student's persistent extension directory
already contains the extension.

Use explicit timeouts for extension installation because it depends on an
external extension service.

### Git repositories

For a tutorial that requires a repository, a cheap reusable availability check
is:

```bash
timeout 60s git ls-remote --exit-code URL HEAD
```

Clone/build only when clone/build behavior itself is an important tutorial
dependency.

### Compiled languages

Check observable facts:

```text
compiler exits 0
executable exists
executable runs
expected behavior/output occurs
```

When useful:

```bash
test -x executable && ! test executable -ot source
```

Do not detect completion by parsing shell prompts.

### Interactive programs

Use stdin directly when browser interaction is not the thing under test:

```text
input: "123\n"
command: ./factor
```

Assert semantic output.

### Random programs

Assert invariants rather than a particular random sequence. The FORTRAN
Bubblesort test checks that the ten output numbers are the numeric sort of the
ten input numbers.

## Playwright project ordering

`playwright.config.ts` defines:

```text
workspace-smoke
    ↓ dependency
toolchains
```

The smoke project creates the container first. The toolchain project uses one
worker and reuses `e2e-0@example.com`'s container. Browser workers use distinct
`e2e-N@example.com` accounts and containers; the cleanup project stops every
container in the configured E2E user pool after dependent projects finish.

Running:

```bash
npm test
```

runs both in the correct order and produces one Playwright report.

Running:

```bash
npm run test:toolchains
```

also runs the browser prerequisite first.

If the correct E2E Workspace container is already running and you explicitly
want to skip the browser prerequisite:

```bash
npm run test:toolchains:reuse
```

## Important browser findings worth retaining

The login form requires the email/code field to lose focus before its submit
button reliably becomes enabled. The helpers press `Tab` after filling.

Reset the user before the browser smoke test. Persisted Workspace state is not
cleared by a fresh BrowserContext.

Never manually remove `/user/<tag>` while its container is running. The normal
`/api/reset_server` path stops the container before deleting persisted data.

## No terminal-renderer test setting needed

The toolchain tests no longer scrape VS Code's integrated terminal DOM.

Therefore E2E-only changes such as:

```text
terminal.integrated.gpuAcceleration = off
editor.autoIndent = none
formatOnPaste / formatOnType changes
```

are not required by this architecture.

Do not change student VS Code behavior merely for the command-line test suite.

## Adding another tutorial

A future test should normally be named:

```text
something.toolchain.spec.ts
```

and request only:

```ts
workspaceContainer
```

not `page`.

The fixture gives it a freshly reset `/workspace/.e2e` sandbox inside the
already-launched Workspace container.

Before adding a browser action, ask:

> Is the browser behavior itself the thing that could break?

If not, test it through `WorkspaceContainer.exec()`.
