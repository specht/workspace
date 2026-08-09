# AGENTS.md — Hackschule Workspace E2E Tests

This directory contains Playwright end-to-end tests for Hackschule Workspace.

The goal of these tests is **not** to test code-server in isolation. They should reproduce the path a student actually follows in the browser: log in, launch a fresh Workspace, use VS Code through the UI, type tutorial code, use the integrated terminal, compile/run programs, and verify observable results.

Keep this file up to date when an E2E debugging session reveals a new invariant or pitfall.

---

## Core principles

### Test the student path

Prefer real browser interaction over shortcuts.

Good:

- clicking the same Workspace buttons a student clicks;
- creating files through VS Code;
- typing/pasting source into Monaco;
- saving through VS Code;
- installing extensions through Quick Open when the tutorial tells students to do so;
- compiling and running programs in the integrated terminal.

Avoid using `docker exec`, writing files directly on the host, or otherwise bypassing the UI for tutorial behavior. Those techniques may be useful for debugging infrastructure, but they do not prove that the tutorial works for a student.

### Assert behavior, not pixels

Use text, state, files, compiler output, program output, and other semantic behavior for assertions.

Screenshots, video, and Playwright traces are diagnostic artifacts. Do not make the tests pixel-perfect.

### Start each test from a genuinely fresh Workspace

A fresh browser context is not sufficient because Workspace state is persisted on disk.

The E2E fixture must reset the E2E user before launching the server.

The intended initial VS Code state for a normal fresh user is:

```text
NO FOLDER OPENED
```

Do **not** change Workspace to automatically open `/workspace` merely to simplify the tests.

---

## Local E2E accounts

Local development uses disposable users such as:

```text
e2e-0@example.com
e2e-1@example.com
...
```

The user is selected by Playwright worker index / `parallelIndex`.

Important:

- use `parallelIndex`, not the transient worker process number;
- `parallelIndex` remains stable if Playwright restarts a failed worker;
- this lets different workers own independent Workspace users;
- tests may run with one worker today but the harness should remain parallel-safe.

The local development login code is:

```text
123456
```

The E2E invitation pool should be generated in local development data, e.g. `data/invitations/_e2e.txt`. Do not require a committed production configuration change merely to create test users.

---

## Reusable fixture design

Future tutorial tests should use the shared fresh-workspace fixture rather than repeating login/reset/launch logic.

The intended shape is approximately:

```ts
test('tutorial', async ({ freshWorkspace: workspace }, testInfo) => {
    // workspace is already logged in, reset, launched, and ready
});
```

Keep responsibilities separated:

```text
reporting.ts   screenshots / report attachments
workspace.ts   login, reset, launch, wait for VS Code
fixtures.ts    worker-specific E2E identity + freshWorkspace fixture
tutorial.ts    read real tutorial source files
vscode.ts      reusable VS Code/editor/terminal actions
*.spec.ts      tutorial-specific journey and assertions
```

Do not move tutorial-specific FORTRAN behavior into generic helpers unless it is genuinely reusable for other languages.

---

## Workspace launch and reset

The local development URL is currently:

```text
http://workspace.test:8025
```

Login selectors that have worked reliably:

```text
#ti_email
#bu_submit_email
#ti_code
#bu_submit_code
#bu_launch
```

After filling the email or login code, explicitly blur the field (for example with `Tab`) before expecting the corresponding button to become enabled. Workspace UI validation may not update while the input remains focused.

Example:

```ts
await emailInput.fill(email);
await emailInput.press('Tab');
await expect(page.locator('#bu_submit_email')).toBeEnabled();
await page.locator('#bu_submit_email').click();
```

### Reset before the test

Reset before each test, not merely as cleanup after it.

This protects against:

- failed tests leaving files behind;
- installed extensions persisting;
- stale VS Code settings;
- previous terminal/editor state;
- Playwright retries.

### Never delete a mounted Workspace directory while its container is running

A manual `rm -rf /user/<user>` while the user's code-server container is still running can leave `/workspace` attached to a deleted/stale bind mount.

A symptom seen during development was:

```text
OCI runtime exec failed:
current working directory is outside of container mount namespace root
-- possible container breakout detected
```

The correct order is:

1. stop/kill the user's container;
2. remove the user's persisted directory;
3. launch a fresh container.

The normal `/api/reset_server` path is intended to do this in the safe order.

If a local development user gets into this stale-mount state, kill the old `hs_code_*` container and relaunch instead of debugging permissions first.

---

## File creation in VS Code

### Fresh Workspace deliberately has no folder open

Even with `NO FOLDER OPENED`, saving a new text file should suggest a path under:

```text
/workspace/
```

That behavior is part of the tutorial contract and should be tested.

### "New File" behavior is not completely uniform

Depending on code-server / VS Code state, clicking `New File` may:

1. immediately create `Untitled-1`, or
2. show a file-type Quick Pick first, from which `Text File` must be selected.

A generic helper should tolerate both legitimate states instead of assuming exactly one transition.

### Saving: do not use `fill(filename)`

This was an important bug.

When VS Code opens Save As, the field may contain something like:

```text
/workspace/Untitled-1
```

with only `Untitled-1` selected.

A real user types `hello.f90`, replacing only the selected filename and preserving `/workspace/`.

This is wrong:

```ts
await saveInput.fill('hello.f90');
```

`fill()` replaces the entire value and can turn the destination into:

```text
/hello.f90
```

Instead:

1. assert the suggested value starts with `/workspace/`;
2. use keyboard insertion so it replaces the current selection;
3. assert the resulting value is `/workspace/<filename>`;
4. press Enter.

---

## Monaco editor interaction

### Do not depend on Monaco's hidden textarea for ordinary typing unless necessary

Click the visible editor first and then send keyboard input.

However, multiline source code has an additional trap once a language extension is active.

### Use a paste event for multiline source

Typing a multiline source string with:

```ts
page.keyboard.insertText(contents)
```

can interact badly with editor auto-indentation.

For example, after Photran activates, Monaco may auto-indent after every newline while the source being inserted already contains indentation. The result is progressively increasing indentation ("staircase" indentation).

The program may still compile in free-form Fortran, which makes this bug easy to miss visually.

For complete source files, prefer dispatching a paste event into the active Monaco input so the source text is inserted exactly as stored.

The test should ideally read the real source from `src/content/...` and paste that exact content.

### Reuse the actual tutorial source files

Do not duplicate large tutorial programs in the E2E spec.

Use the shared tutorial-file helper to load files such as:

```text
src/content/fortran/hello.f90
src/content/fortran/factor.f90
src/content/fortran/bubblesort.f90
```

This prevents the tutorial and the E2E test from silently drifting apart.

---

## Keyboard focus matters

VS Code keyboard shortcuts are context-sensitive.

A recurring failure mode is that the integrated terminal still owns keyboard focus when the test sends an editor/workbench shortcut.

For example:

```text
Ctrl+Alt+N
```

will not reliably create the next text file if xterm still has focus.

Rule for reusable helpers:

```text
editor/workbench shortcut -> establish editor/workbench focus first
terminal typing           -> focus xterm input first
Quick Open input          -> wait for and focus Quick Open first
```

Do not assume that the previous helper left focus in the state the next helper needs.

---

## Photran / extension installation

The FORTRAN tutorial installs:

```text
fiuba.photran-lsp-client-vscode
```

through Quick Open:

```text
Ctrl+P
ext install fiuba.photran-lsp-client-vscode
```

### Quick Open is asynchronous

Do not type the `ext install ...` command and immediately press Enter.

VS Code first resolves it into an action row similar to:

```text
Press Enter to install extension 'fiuba.photran-lsp-client-vscode'.
```

Wait until that action row is visible, then press Enter.

Otherwise the Enter key may arrive before the action exists and nothing gets installed.

### Quick Open may remain visible

Do not assume Quick Open disappears immediately after submitting an extension installation.

If needed, close it with Escape after the action has been submitted.

The meaningful assertion is whether the extension actually becomes usable, not whether a transient Quick Open widget disappeared at a particular instant.

### Publisher trust dialogs may appear

Modern VS Code may show a publisher-trust confirmation for a first-time extension install. Extension-install helpers should be prepared to handle that UI.

### Avoid long shell polling loops for extension installation

An earlier approach repeatedly ran:

```text
code-server --list-extensions
```

inside a 90-second shell loop.

This was slow, opaque, and could leave the test visibly stuck in `sleep`.

Prefer a UI/behavior-level signal that the extension has installed and activated. For Photran, a useful signal is that the `.f90` editor is recognized/highlighted as Fortran.

---

## Integrated terminal

### E2E users use the DOM terminal renderer

Playwright needs actual DOM text to inspect terminal output.

In local development, E2E users are configured with:

```json
"terminal.integrated.gpuAcceleration": "off"
```

This should be limited to disposable local E2E users.

Do not disable the Chromium GPU globally and do not change the production/student terminal renderer solely for testing.

With the DOM renderer active, terminal text can be read from xterm DOM rows such as:

```text
.xterm-rows
```

### Do not use the Accessible Terminal Buffer shortcut

An attempted `Shift+Alt+F2` solution failed because the keystroke was delivered to bash as an escape sequence rather than being handled as the intended VS Code command.

It also caused the test to accidentally read the source editor instead of the terminal.

Do not bring this approach back without a very good reason.

### Do not detect command completion by counting shell prompts

Prompt text is a rendering detail.

Attempts to count `$` / `#` prompts failed because xterm's DOM renderer can omit trailing blank cells and format the active prompt differently.

Avoid assertions like:

```text
number of "$ " prompts increased
```

They are brittle.

### Prefer observable postconditions

For a compiler command such as:

```bash
gfortran hello.f90 -o hello
```

the useful fact is not that bash displayed another prompt.

The useful fact is that the compiled artifact exists and is current.

A reusable compiled-artifact check can verify:

```bash
test -f hello &&
test -x hello &&
! test hello -ot hello.f90
```

In words:

- executable exists;
- executable bit is set;
- executable is not older than its source.

A marker printed after that probe can give Playwright a deterministic piece of terminal text to wait for.

This pattern is reusable for C, C++, Fortran, Pascal, Rust, or other tutorials that produce a native executable.

### Do not over-test shell mechanics

The browser test should send the same tutorial command the student sends:

```bash
gfortran hello.f90 -o hello
```

Then verify its effects.

Avoid replacing tutorial commands with complicated synthetic shell constructs merely to make the test framework easier to write.

---

## Interactive terminal programs

Interactive programs need a different flow from simple commands.

Typical pattern:

1. focus terminal;
2. type `./program`;
3. wait for the program's input prompt;
4. type the requested input;
5. wait for the expected program output.

For the FORTRAN factorization tutorial:

```text
./factor
wait for: Enter a number:
type: 123
assert factors include: 3 and 41
```

Do not wait for a shell prompt while the program is intentionally waiting for user input.

---

## FORTRAN tutorial coverage

The completed FORTRAN journey should cover the meaningful tutorial contract rather than merely "gfortran is installed".

Expected progression:

```text
fresh Workspace
  -> NO FOLDER OPENED
  -> create hello.f90
  -> save under /workspace
  -> install Photran
  -> compile hello.f90
  -> verify executable exists and is current
  -> run hello
  -> assert Hello, World!
  -> deliberately change print -> prin
  -> compile
  -> assert compiler error
  -> restore source
  -> compile successfully again
  -> create factor.f90 with Ctrl+Alt+N
  -> compile
  -> run with input 123
  -> assert factors
  -> create bubblesort.f90
  -> compile
  -> run
  -> assert output is actually sorted
```

For Bubblesort, prefer semantic assertions:

- expected number of values is present;
- the sorted output contains the same values as the original output;
- the resulting values are monotonically sorted.

Do not assert a particular random sequence unless the tutorial itself promises one.

---

## Reporting

Use named `test.step(...)` blocks for human-readable HTML reports.

Attach screenshots at meaningful milestones, not after every trivial click.

Prefer viewport screenshots:

```ts
page.screenshot()
```

Do not default to `fullPage: true`; full-page VS Code screenshots produce unwieldy report pages.

Keep Playwright traces enabled for deep debugging. Video can be retained on failure.

The HTML report should help answer:

```text
What exact tutorial step failed?
What was visible when it failed?
What had already succeeded?
```

---

## Timeouts

A timeout should reflect one specific operation.

Do not hide long internal loops behind a helper whose caller appears to have a 30-second timeout. This previously produced a test that looked "hung" for much longer than expected because the extension helper itself contained a 90-second polling loop and a 120-second command timeout.

Prefer:

- a single explicit wait for a UI state;
- a single explicit wait for a marker/output;
- failure messages that describe the state being awaited.

Long retries should be rare and obvious in the source.

---

## Selector strategy

Prefer stable semantic selectors where practical:

```ts
page.getByText(...)
```

For Monaco/code-server internals where there is no useful accessible selector, targeted classes are acceptable, for example:

```text
.monaco-workbench
.monaco-editor:visible
.quick-input-widget:visible
.terminal-wrapper:visible
textarea.xterm-helper-textarea
.xterm-rows
```

Treat these as implementation-sensitive. If code-server is upgraded and tests fail, inspect the live DOM before blindly increasing timeouts.

---

## Debugging methodology

When a test fails:

1. inspect the screenshot/report first;
2. determine whether the student action actually happened;
3. distinguish application failure from assertion/helper failure;
4. fix the helper if the screenshot proves Workspace already did the right thing;
5. avoid adding sleeps unless no state-based wait is possible.

Examples from previous debugging:

- compiler command visibly succeeded but the test timed out -> terminal assertion was wrong;
- file was saved as `/hello.f90` -> test used `fill()` and destroyed `/workspace/`;
- `Ctrl+Alt+N` did nothing -> terminal still had focus;
- Fortran indentation drifted -> multiline source was typed instead of pasted;
- extension command remained in Quick Open -> Enter was sent before VS Code resolved the install action;
- fresh E2E user worked while old admin user did not -> stale bind-mounted container, not a general permissions bug.

The screenshot is often evidence that the **test harness**, not Workspace, is wrong.

---

## Things not to "fix" without understanding the intended behavior

Do not change these merely to make an E2E test easier:

- fresh VS Code starts with `NO FOLDER OPENED`;
- Save As should nevertheless default to `/workspace`;
- tutorial keyboard shortcuts should still be tested when the tutorial tells students to use them;
- normal students should retain the normal terminal renderer;
- tutorial commands should remain the commands students actually type.

If the harness cannot handle one of these states, improve the harness.

---

## Adding another tutorial test

For a new language/tutorial:

1. use `freshWorkspace`;
2. read source examples from the real tutorial files where possible;
3. reuse generic file/editor/terminal helpers;
4. add only language-specific assertions to the spec;
5. for compiled languages, use executable freshness checks;
6. for interpreted languages, assert actual output;
7. for interactive programs, wait for input prompts before typing;
8. keep the test faithful to the tutorial's visible sequence.

Before creating a new helper, ask:

```text
Will another tutorial genuinely reuse this?
```

If not, keep it in the tutorial spec.

---

## Local run

Typical local run:

```bash
cd e2e
npm test
```

When debugging one test, use normal Playwright test filtering rather than commenting out shared setup.

Generated directories such as Playwright reports/results and `node_modules` should stay uncommitted. The E2E source, configuration, lockfile, and this `AGENTS.md` should be committed.

---

## Final rule

The E2E suite should answer one question:

> If a student follows this tutorial in a completely fresh Hackschule Workspace, does it actually work?

Optimize the helpers for making that statement trustworthy.