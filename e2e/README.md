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

To run only the Shared Live Apps lifecycle coverage (including access control,
HTTP, WebSocket, revocation, and listener replacement), use:

```bash
npx playwright test tests/shared-live-apps.browser.spec.ts --project=workspace-smoke
```

To run the browser/toolchain coverage for the BIF and PixelRAM tutorials, use:

```bash
npx playwright test tests/bif.browser.spec.ts --project=workspace-smoke
npx playwright test tests/pixelram.browser.spec.ts --project=workspace-smoke
npx playwright test tests/bif.browser.spec.ts tests/pixelram.browser.spec.ts --project=workspace-smoke
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
opening the integrated terminal, normal commands and interactive commands. Terminal
commands are completed using their real shell exit status rather than rendered prompt
text, and `expectExecutableUpToDate(...)` verifies compiled artifacts against their
source files.
`tests/tutorial.ts` provides `readTutorialFile(...)` so tests can reuse the
actual example files from `src/content`. Keep tutorial-specific assertions in
the corresponding `*.spec.ts` file.

The FORTRAN test reads `hello.f90`, `factor.f90` and `bubblesort.f90` directly
from `src/content/fortran`, so the E2E test always types the exact source code
shown by the tutorial rather than maintaining duplicate copies.

## Parallel runs

The suite defaults to one worker. `workspace-smoke` honors `E2E_WORKERS`, and
each concurrent browser worker gets an independent disposable account and
Workspace container based on Playwright's `parallelIndex`:

```text
e2e-0@example.com
e2e-1@example.com
...
```

For example:

```bash
E2E_WORKERS=4 npm test
```

The `toolchains` project intentionally remains serial. After all browser tests
finish, it reuses e2e-0's running Workspace container. Final teardown stops all
containers belonging to the configured E2E user pool, including unused or
leftover `e2e-N` containers, without matching ordinary Workspace users.

Eight E2E users are created by default. To increase both the pool and the
number of workers:

```bash
E2E_USER_COUNT=16 E2E_WORKERS=16 npm test
```

## Profile student-container resources

Resource-limit values should be based on measurements from the real student
container, not on guesses. The opt-in resource profiler launches an E2E student
through the normal Workspace UI, keeps the VS Code session connected, and then
runs representative commands inside that same `hs_code_*` container.

It deliberately does **not** run as part of `npm test`. Run it explicitly on a
Linux Docker host with cgroup v2:

```bash
npm run profile:resources
```

For measurements that will be used to choose production limits, repeat each
workload a few times:

```bash
E2E_RESOURCE_RUNS=3 npm run profile:resources
```

`E2E_RESOURCE_SAMPLE_MS` controls the cgroup polling interval and defaults to
10 ms. The profiler reads the container's cgroup from the host instead of
starting a sampler process inside the container, so the measurement itself does
not inflate the container's PID count.

The current workloads are:

- the real C `bubblesort.c` tutorial compiled with GCC;
- the real C++ `bubblesort.cpp` tutorial compiled with G++;
- a clone and `make` of the PixelRAM starter, exercising Emscripten;
- a fresh Flutter project built for Web and Android APKs, each in debug and
  release modes;
- a fresh minimal SvelteKit project built with `npm run build`;
- the LaTeX tutorial's larger `wpgtr.tex` document through
  `latexmk -lualatex`;
- the real Python and Ruby Bubblesort tutorial programs, repeated sequentially
  so the short interpreter runs are observable by the sampler.

Preparation such as cloning and project scaffolding is kept outside the timed
build where practical. Long-lived Gradle daemons are stopped before and after
Android runs so their heap and threads do not become the baseline for later
measurements. Reclaimable filesystem page cache remains visible to
`memory.current` and may still accumulate across workloads.

Every measured run records both the idle baseline and the whole-container peak,
which makes accumulated caches and the code-server baseline visible instead of
hiding them.

The profiler reads cgroup-v2 `memory.current` and `pids.current`. The memory
counter is the whole cgroup usage, including descendants and reclaimable page
cache. The PID counter is the kernel task/thread count relevant to Docker's
`--pids-limit`, so it may be higher than a simple `ps` process count.

Results are printed to the terminal and written to:

```text
e2e/test-results/resource-profile.json
e2e/test-results/resource-profile.csv
```

Both files are also attached to the Playwright report. The JSON includes the
container image, current CPU/memory/PID settings, tool versions, per-run
baselines and peaks, and the highest values observed. It intentionally does not
turn those measurements into proposed limits.

The profiler can run both before and after resource limits are enabled.

Without limits, the result is marked `unconstrained` and can be used when
choosing initial limits. With limits enabled, the result is marked
`constrained` and is useful for verifying that representative workloads
still have sufficient headroom.

To explicitly require an unconstrained container, use:

```bash
E2E_RESOURCE_REQUIRE_UNLIMITED=1 E2E_RESOURCE_RUNS=3 npm run profile:resources
```

This deliberately fails if a memory or PID limit is already active.
A constrained peak near a configured limit should not be interpreted as
the workload's unconstrained resource requirement.

