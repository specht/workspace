# End-to-end tests

Playwright tests against the local Hackschule Workspace
(`http://workspace.test:8025`). There are two kinds:

- **Browser tests** (`*.browser.spec.ts`, project `workspace-smoke`) for what
  only a browser can show: login and sessions, starting a Workspace, the
  website itself (catalog, class page, Handbuch tables, task cards, media
  variants), Shared Live Apps and webviews such as Pixelflow Canvas or
  TIC-80.
- **Toolchain tests** (`*.toolchain.spec.ts`, project `toolchains`) for
  compilers and tools. They run commands with `docker exec` inside the real
  Workspace container that the browser tests started, on the real example
  files from `src/content`.

The rules for writing either kind, and why the suite is split this way, are
in [AGENTS.md](AGENTS.md).

The suite creates its disposable invitation pool itself in
`data/invitations/_e2e.txt` (ignored by Git).

`tests/access-rules.browser.spec.ts` tests who may use the Workspace
(removed from a group, suspended, guests without the Nutzungsordnung,
announced and repeated deletions, students of two schools). It needs an
imported roster and logs in as the first address in `ADMIN_USERS`
(`src/ruby/credentials.rb`), or `E2E_ADMIN_EMAIL`; its schools, groups and
accounts are made up and deleted afterwards.

## First-time setup

Start Workspace from the repository root as usual:

```bash
./config.rb up -d
```

Then install the test dependencies and the browser:

```bash
cd e2e
npm install
npx playwright install chromium
```

## Run

```bash
npm test                       # browser tests, then toolchain tests, one report
npm run test:browser           # only the browser tests
npm run test:toolchains        # toolchain tests (starts the browser prerequisite first)
npm run test:toolchains:reuse  # toolchain tests in the already running E2E container
npm run report                 # open the HTML report
```

One file at a time:

```bash
npx playwright test tests/shared-live-apps.browser.spec.ts --project=workspace-smoke
npx playwright test tests/latex.toolchain.spec.ts --project=toolchains
```

Every test is broken into named `test.step(...)` phases. Important phases have
screenshots attached, and every run records a complete Playwright trace, so
the HTML report shows both the steps and the browser actions inside them.

## Writing tests

Browser tests use the `freshWorkspace` fixture when they need a running
Workspace:

```ts
import { test } from './fixtures';

test('my page', async ({ freshWorkspace: workspace }, testInfo) => {
  // a freshly reset, logged-in and fully loaded VS Code page
});
```

The fixture logs in with the worker's disposable E2E account, resets that
user's server, files and VS Code profile, launches the Workspace and waits
until the workbench is ready. Tests of the website alone use Playwright's
`page` directly.

Toolchain tests request only `workspaceContainer`, never `page`. It gives them
a freshly reset `/workspace/.e2e` inside the running Workspace container.
`readTutorialFile(...)` (`tests/tutorial.ts`) reads the example files from
`src/content`, so a test checks exactly the code a tutorial shows; the FORTRAN
test compiles and runs `hello.f90`, `factor.f90` and `bubblesort.f90` from
`src/content/fortran`.

## Pictures for pages

Some pages show pictures of the Workspace itself (the teacher guide). They
are taken from the running development site by a separate project that never
runs in a normal test run:

```bash
HS_PICTURES=1 npx playwright test --project=pictures
```

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

