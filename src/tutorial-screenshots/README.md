# Tutorial screenshots

In development, tutorial screenshots can be generated from small recipes embedded
next to the normal `<img>` tag. The image filename stays exactly the same as for a
hand-made screenshot and the generated file is written into the tutorial directory
as a lossless WebP. After that, Workspace's existing `/webcache` image pipeline
handles it like any other tutorial image. There is no separate screenshot command in
the normal authoring loop: reloading a tutorial is the trigger.

A manual image needs no recipe:

```html
<img class='full' src='welcome.webp' alt=''>
```

An automated image has a comment immediately before it:

```html
<!-- tutorial-screenshot
clone-start: https://github.com/example/tutorial-project.git
crop-top: 33.5%
crop-bottom: 33.6%
-->

<img class='full' src='git-clone.webp' alt=''>
```

On a development reload, `git-clone.webp` is generated when it is missing or
stale. A `.tutorial-screenshots.json` file in the same tutorial directory records
the recipe-state hash and the Workspace image fingerprint. Both the generated
WebP files and this manifest are normal source files and should be committed.

In development the reload itself does not wait for Playwright. The generator first
returns a fast freshness plan (including the final pixel dimensions), then queues
stale screenshots in the background. Existing stale images stay visible with a
warning outline; missing images use size-correct placeholders. While rendering is
active, the tutorial footer shows replay progress and a filmstrip that receives
each newly generated image as soon as it is written. Clicking a filmstrip image
opens a large preview. When every generated screenshot is already current, none of
that monitoring UI is shown. Production continues to use the committed tutorial
images directly and never enables this live-preview path.

The state hash is chained: each screenshot depends on its own recipe and on the
state hash of every automated screenshot before it. `write-file` also includes the
selected tutorial code block in the hash. Editing an early step therefore
invalidates the screenshots that follow it. Rebuilding `hs_code_server` changes
the environment fingerprint and transparently regenerates the same named images
on the next development reload.

To deliberately recreate every generated screenshot for one tutorial, run the
helper from the repository root while the development services are running:

```bash
src/scripts/recreate-screenshots.rb my-tutorial
```

The argument may also be a Markdown path such as `my-tutorial/my-tutorial.md` or
`src/content/my-tutorial/my-tutorial.md`. Forced recreation ignores manifest freshness, replays
the complete recipe chain, overwrites the generated images in place, and writes a
fresh manifest. Manual tutorial images are untouched. The helper follows the
screenshot-generator log while it runs, so target filenames and individual recipe
steps are visible directly in the terminal.

## Recipe language

Recipes are intentionally line-oriented. There is one set of general browser
interaction primitives; actions that need stronger Workspace-specific readiness
checks stay semantic. The complete instruction set is:

```text
# Workspace layout
close-folder
show-left-sidebar
left-sidebar-width: CSS_PIXELS
hide-left-sidebar
show-right-sidebar
hide-right-sidebar
show-bottom-panel
hide-bottom-panel

# Git and editor
clone-start: URL
clone-start: URL @ SHA1
open-file: PATH
close-tab: LABEL

# Terminal
terminal-open
terminal-wait-for-prompt
terminal-maximize
terminal-run: TEXT

# Files and preview
write-file: PATH <- previous-code
write-file: PATH <- previous-code (LABEL)
write-file: PATH <- code:UNIQUE TEXT||ANOTHER UNIQUE TEXT
write-file: PATH <- file:RELATIVE_PATH
wait-for-file: PATH
wait-for-file-newer: TARGET <- SOURCE
go-live
preview-reload
preview-reset

# General interaction
click: TEXT
click: selector:CSS_SELECTOR
hold: SECONDSs TEXT
hold: SECONDSs selector:CSS_SELECTOR
move-mouse: TEXT
move-mouse: selector:CSS_SELECTOR
type: TEXT
press: PlaywrightKey
sleep: SECONDS
wait-for-text: TEXT
wait-for-input-value: TEXT

# Screenshot target and capture
tab: workspace|preview
viewport: WIDTHxHEIGHT
zoom: NUMBER
crop-top: PERCENT%
crop-bottom: PERCENT%
crop-terminal-lines: NUMBER|auto
crop-terminal-skip-top: NUMBER
crop-terminal-skip-bottom: NUMBER
```

`tab` selects both the page to capture and the page used by the general
interaction instructions (`click`, `hold`, `move-mouse`, `type`, `press`,
`wait-for-text`, and `wait-for-input-value`). It defaults to `workspace`. Actions
such as `terminal-open`, `open-file`, and `clone-start` always operate on the
Workspace because that is part of their meaning.

### General interaction

`press` is the single keypress instruction. It accepts Playwright key names and
chords such as `Enter`, `Escape`, `Control+C`, or `Control+K`. For a VS Code chord,
write the two keypresses in the same order a user would press them:

```text
press: Control+K
press: KeyO
```

After `terminal-open` or `terminal-maximize`, the terminal has focus, so the same
instruction handles terminal keys too:

```text
terminal-open
press: q
```

`type` inserts text literally into the currently focused text control without
pressing Enter. It is useful for Quick Input and file dialogs. It does not clear
existing text first; use `press: Control+A` when you want to replace the current
value.

`click` and `hold` target the page selected by `tab`. They first match an exact
accessible button or link name; when a Workspace control is exposed only as text,
`click`/`hold` fall back to exact visible text. Prefix the target with `selector:`
to address one element by CSS selector instead. `hold` keeps the real Chromium
pointer pressed for the requested duration before releasing it; durations must be
between 0.05 and 30 seconds.

`move-mouse` also uses the selected page. Without a prefix, the target is matched
by exact visible text; prefix it with `selector:` to use a CSS selector instead.
This is useful for parking the pointer on a neutral element before capture so a
button does not remain in its hover state.

`wait-for-text` waits up to 60 seconds until the selected page contains the given
visible text. Whitespace is normalized before matching. `wait-for-input-value`
performs the same kind of readiness check against visible `<input>` and
`<textarea>` values. The latter is useful for VS Code Quick Input because input
values are not part of `document.body.innerText`.

### Opening and creating folders

Opening folders needs no special screenshot-only shortcut. The recipe can drive
the same VS Code UI a student sees. To open an existing folder with
<kbd>Ctrl</kbd>+<kbd>K</kbd>, <kbd>O</kbd>, type its name, enter it, and then accept
it:

```text
press: Control+K
press: KeyO
wait-for-input-value: /workspace/
type: bif
press: Enter
wait-for-input-value: /workspace/bif/
press: Enter
wait-for-text: pages-starter
```

The final readiness marker should be something the opened folder is expected to
show. That makes the screenshot deterministic instead of relying on a fixed
sleep.

The no-folder welcome screen can be driven in the same way through its button. A
missing folder causes VS Code's normal creation confirmation to appear, which can
also be captured before it is accepted:

```text
click: Open Folder
wait-for-input-value: /workspace/
type: mein-projekt
press: Enter
wait-for-text: does not exist. Would you like to create it?
press: Enter
wait-for-text: mein-projekt
```

### Cloning repositories

`clone-start` opens VS Code's Clone Repository flow and types the URL, leaving the
URL field visible for the screenshot. A public repository can be pinned to a full
40-character SHA-1:

```text
clone-start: https://github.com/example/tutorial-project.git @ 0123456789abcdef0123456789abcdef01234567
```

The remaining clone steps use the same general primitives as every other dialog:

```text
press: Enter
wait-for-input-value: /workspace/
press: Enter
wait-for-text: Would you like to open the cloned repository?
click: Open
wait-for-text: SOME PROJECT-SPECIFIC MARKER
```

Keeping the readiness marker in the tutorial avoids putting knowledge about one
project into the screenshot engine.

Pinned repositories are cached automatically below
`data/tutorial-screenshot-git-cache`. The first pinned use of a URL creates a
mirror in that cache. On later runs the generator checks whether the requested
commit already exists locally; if it does, the remote Git server is not contacted.
When a new pinned commit is requested, the existing mirror is refreshed once and
the commit is checked again.

For the actual VS Code interaction the generator makes a private bare copy from
the cache inside the disposable screenshot Workspace, points its default branch at
the pinned commit, and configures Git's `insteadOf` rewriting. VS Code therefore
still shows and clones the public URL entered in Quick Input even though the Git
transport is local. The persistent cache survives screenshot Workspace resets and
screenshot-generator rebuilds. It can be deleted at any time to force a clean
remote mirror on the next pinned clone.

Without `@ SHA1`, `clone-start` clones from the remote repository normally.

### Terminal

`terminal-open` opens VS Code's integrated terminal if necessary, waits for the
initial shell prompt (for example `student@workspace:~$`) and focuses the active
terminal. Repeating it does not create another terminal or wait for Bash again,
so later `terminal-run` instructions can also drive an interactive program.

`terminal-wait-for-prompt` waits until the last non-empty line in the integrated
terminal is a Bash prompt such as `student@workspace:~$`. This is useful after a
foreground shell command when the next screenshot or action must not happen until
the command has finished:

```text
terminal-run: wget https://github.com/specht/workspace-files/raw/main/terra1.sql
terminal-wait-for-prompt
```

Do not use it after starting an interactive program such as `mycli`, because Bash
will not show another prompt until that program exits.

`terminal-maximize` makes the panel containing the terminal fill the available
workbench area and returns focus to the terminal. It is safe to repeat when the
panel is already maximized.

`terminal-run` sends its text literally to the focused integrated terminal and
then presses Enter. It deliberately does not distinguish between a shell and a
program already running in the terminal, so the same instruction works for Bash,
`mycli`, or another interactive command:

```text
terminal-run: wget -q -O terra1.sql https://github.com/specht/workspace-files/raw/main/terra1.sql
terminal-run: mysql < terra1.sql
terminal-run: mycli
terminal-run: SHOW TABLES;
```

`crop-terminal-lines` is a capture directive for Workspace terminal
screenshots. At capture time it finds the visible integrated terminal, keeps the
top of its containing panel (including the panel header/tabs), and crops using
the terminal's measured xterm row geometry. The measured CSS geometry is
converted through the active viewport/zoom/device-scale profile, so the crop
does not depend on fixed percentages or a configured font size.

With `crop-terminal-lines: NUMBER`, the generator keeps the first `NUMBER`
rendered terminal rows exactly as they appear, including blank rows, plus half
of the actual rendered row height as padding. If fewer than `NUMBER` rendered
rows are available, all available rows are kept and the padding is clamped to
the terminal screen.

With `crop-terminal-lines: auto`, the generator keeps rows through the last
visible non-empty terminal row, again with the same half-row padding. This is
useful when the visible output height depends on the command you just ran and
you simply want the screenshot to include all visible terminal content.

`crop-terminal-skip-top: NUMBER` and `crop-terminal-skip-bottom: NUMBER` remove
rendered xterm rows from consideration before the terminal crop is calculated.
This is especially useful with `auto` when an interactive program reserves a
status/help row at the bottom of the terminal:

```text
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
```

A bottom skip can therefore hide an otherwise unrelated status row without
hard-coding knowledge of that program into the screenshot engine. A top skip
moves the top crop edge to the first remaining terminal row, so the panel header
and the skipped terminal rows are omitted. Skip values must be non-negative
integers and require `crop-terminal-lines`.

`crop-terminal-lines` requires `tab: workspace` and cannot be combined with
`crop-top` or `crop-bottom`; those combinations are validation errors.

### Files and editor state

`open-file` opens a Workspace file through Quick Open and waits for the Monaco
editor to settle. `close-tab` closes an editor tab by its exact label and is a
no-op when that tab is already closed.

`wait-for-file` waits up to 60 seconds for a file below the screenshot user's
Workspace root. `wait-for-file-newer: TARGET <- SOURCE` waits until TARGET exists,
is newer than SOURCE, and has stopped changing briefly. These are useful for
builds and background tools whose completion is observable on disk.

`previous-code` means the last fenced code block before the recipe. When several
blocks belong to the same step, put a `screenshot-code` marker immediately before
a fence and reference it by label:

````markdown
<!-- screenshot-code: 3-new -->
```markdown_wrap
Contents for page 3.
```
````

```text
write-file: pages-starter/3.md <- previous-code (3-new)
```

Labels must be unique within the tutorial. `code:...` selects the most recent
earlier fenced code block containing all snippets separated by `||`. This keeps
the visible tutorial code as the source of truth instead of duplicating it in the
hidden recipe.

`file:RELATIVE_PATH` reads a file next to the tutorial Markdown (or from a
subdirectory beneath it) as raw bytes and copies it without text decoding. The
source SHA-256 is included in the screenshot state, so binary assets such as JPEG,
PNG, WebP, or audio files are safe and still invalidate dependent screenshots when
changed.

`left-sidebar-width` resizes the primary sidebar through VS Code's own sash, so
the workbench layout reacts exactly as it does when the divider is dragged by
hand. The width is specified in CSS pixels and is intentionally per recipe rather
than a global screenshot setting.

`sleep` pauses recipe execution for the given number of seconds (fractions are
allowed, up to 300 seconds). Prefer an observable readiness instruction such as
`wait-for-text`, `wait-for-input-value`, or `wait-for-file` whenever one exists.

## Tutorial-specific hooks

The screenshot engine itself has no knowledge of individual tutorial applications.
A tutorial that needs application-specific behavior can place
`tutorial-screenshot-hooks.mjs` next to its Markdown. The optional module may
export a default object such as:

```js
export default {
    writeFileSubdirectory: 'project-directory',

    async waitForPreview({ page, timeout }) {
        await page.waitForSelector('#app-ready', { timeout });
    },
};
```

`writeFileSubdirectory` makes `write-file` paths relative to that directory below
the screenshot user's Workspace instead of the Workspace root. This only affects
`write-file`; `wait-for-file` and `wait-for-file-newer` remain explicitly relative
to the Workspace root so their recipe paths stay unambiguous.

`waitForPreview` runs after `DOMContentLoaded` whenever `go-live`,
`preview-reload`, or `preview-reset` opens or reloads the preview. Tutorials that
do not provide a hook simply continue after `DOMContentLoaded`. The hook file's
SHA-256 is included in the environment fingerprint, so changing hook behavior
invalidates that tutorial's generated screenshots automatically.

The global screenshot profile lives in `config.rb`. It defaults to a 1853x929
viewport, 100% browser-style zoom, and a 2x capture scale. The renderer deliberately
does not choose a light or dark color scheme: code-server loads the Workspace's
actual VS Code theme, and the screenshot generator waits until that theme has
remained stable before running the recipe. The page keeps its normal layout while
Chromium renders the screenshot capture at the requested scale, so the default
1853x929 viewport produces a 3706x1858 bitmap without resizing a smaller capture.

`viewport` and `zoom` describe the layout in which the selected tab is operated
and captured. The global capture scale changes only the output resolution. The
renderer applies the layout profile before recipe actions run and again as soon as
a newly-created target tab becomes available. This means actions that intentionally
depend on a custom layout zoom can continue to use it, for example:

```text
zoom: 1.2
click: Fit graph
tab: preview
```

Workspace screenshots also hide VS Code notification toasts so startup warnings
do not leak into tutorial images. Chromium is launched with subpixel positioning
and font hinting disabled explicitly, keeping text rasterization stable at the
scaled screenshot profile without relying on a container-wide Fontconfig override.

After a render that generated screenshots has returned its images, the generator
immediately resets and starts another pristine screenshot Workspace in the
background. The next render reuses that already-prepared Workspace instead of
paying the reset/start/theme-settling cost again. If another reload arrives while
preparation is still running, only a render that actually needs fresh screenshots
waits for it; a reload whose screenshots are all current does not. Screenshot
Workspace preparation also removes the screenshot account's additional MySQL
databases and resets its provisioned MySQL and Neo4j databases through the
application's normal per-user database list/delete/reset/provisioning paths, so
repeated renders start from fresh database state. The normal student-facing
Workspace reset itself continues to preserve database contents.

The screenshot account logs in as `student@example.com`.

If a recipe action fails while a tutorial is being replayed, the generator first
writes debug screenshots of the currently open Workspace and preview tabs (when
present) to `.tutorial-screenshot-errors/` next to the tutorial Markdown.

If generation fails, the development server logs the error and continues parsing.
An existing image remains usable; a genuinely missing image simply stays missing
until the next reload succeeds. Production never invokes the generator.
