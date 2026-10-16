# Tutorial screenshots

In development, tutorial screenshots can be generated from small recipes embedded
next to the normal `<img>` tag. The image filename stays exactly the same as for a
hand-made screenshot and the generated file is written into the tutorial directory
as a lossless WebP (one that would be over 800 KB that way, such as a picture
full of CRT scanlines, at quality 90 instead). After that, Workspace's existing
`/webcache` image pipeline
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
the recipe-state hash, the environment fingerprint and the Workspace look. Both the generated
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

### Pausing rendering on page views

A page view only renders while automatic rendering is on. When a page view
would start rendering while the generator is busy with another job (an
explicit run of the helper below, for example), automatic rendering pauses
itself instead, so looking around the site never interferes with that run.
The footer then says »Automatisches Rendern pausiert« with a »Fortsetzen«
button; while rendering runs, it offers »Pausieren« (the current job still
finishes). The state is stored in `.auto-render.json` in the Git cache and
survives restarts and rebuilds. The helper below renders either way.

### When is a screenshot re-rendered?

A generated file is rendered again when one of these changed:

- **its own recipe**: actions, `tab`, `viewport`, `zoom`, crops, target name,
  `record`/`fps`, and the code a `write-file` takes from the tutorial;
- **the actions of an earlier recipe** in the same tutorial (and the `tab`,
  `viewport` and `zoom` they ran at), because they change what is on screen
  later. A crop or a file name of an earlier recipe does not count: it only
  shapes that earlier file;
- **the environment fingerprint**: `GENERATOR_VERSION`, the Playwright version,
  the screenshot profile from `config.rb`, the screenshot identity, and the
  tutorial's `tutorial-screenshot-hooks.mjs`;
- **the Workspace look**: the code-server release `hs_code_server` is built on
  (its `org.opencontainers.image.version` label, read by the Ruby service). If
  Docker cannot tell, the look is `unknown`, and that never counts as a change;
- or the file is missing.

Rebuilding `hs_code_server` for a new extension, toolchain or font does **not**
re-render anything. When such a change is visible in screenshots (a new
extension in the sidebar, a different terminal prompt), render the affected
tutorials deliberately with the helper below. Until generator 19 every rebuild
re-rendered every tutorial: on 13 October all 22 BIF screenshots were rendered
again although no recipe had changed, and 17 of them came out byte-identical.

Manifests from generator 19 stay valid: an entry without `workspace_look` is
accepted when it matches the old state hash, and the manifest moves to the new
fields on the next development reload without rendering anything.

### Rendering on purpose

Run the helper from the repository root while the development services are
running:

```bash
src/scripts/recreate-screenshots.rb my-tutorial      # everything in one tutorial
src/scripts/recreate-screenshots.rb --all            # everything in every tutorial
src/scripts/recreate-screenshots.rb --stale          # only what is out of date, all tutorials
src/scripts/recreate-screenshots.rb --stale bif      # only what is out of date in one tutorial
src/scripts/recreate-screenshots.rb --status         # render nothing: state of each generated file,
                                                     # generated and manual images per page, total
```

Every run ends with the same overview for the tutorials it rendered. The state
comes from the generator's `POST /plan`, which builds the plan of a render
without rendering or queuing anything.

`--all` and `--stale` skip `src/content/_drafts`. `--stale` is what a
development reload does in the background, but for every tutorial at once and
waiting until it is done.

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
terminal-wait-for-prompt: SECONDS
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
preview-port: PORT [/PATH]
open-site: PATH
neo4j-browser
hook: NAME ARGS
select-option: CSS_SELECTOR = VALUE
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
git-graph: PATH           (only before an _include_svg(NAME.svg))
pdf-page: PATH PAGE [TEXT] (a page of a PDF in the Workspace as the image)
pdf-page-full: PATH PAGE [TEXT] (the same, the whole sheet with its margins)
record: SECONDSs          (only before a <video>)
fps: NUMBER               (with record; default 30)
viewport: WIDTHxHEIGHT
zoom: NUMBER
crop-top: PERCENT%
crop-bottom: PERCENT%
crop-left: PERCENT%
crop-right: PERCENT%
crop-element: SELECTOR
crop-terminal-lines: NUMBER|auto
crop-terminal-skip-top: NUMBER
crop-terminal-skip-bottom: NUMBER
show-mouse
sharp-canvas
variants: NAME [NAME [NAME]]
```

`tab` selects both the page to capture and the page used by the general
interaction instructions (`click`, `hold`, `move-mouse`, `type`, `press`,
`wait-for-text`, and `wait-for-input-value`). It defaults to `workspace`. Actions
such as `terminal-open`, `open-file`, and `clone-start` always operate on the
Workspace because that is part of their meaning.

`preview-port: PORT` opens a web app started in the screenshot Workspace via
its existing authenticated `/proxy/PORT/` route, then makes it the `preview`
tab. Ports 1024–65535 are accepted. The action retries startup for up to three
minutes, so an app can be launched in the background with `terminal-run` and
captured with `tab: preview`, even if it binds only to 127.0.0.1.
With a path, `preview-port: 1313 /axolotl/` opens that page of the app instead
of its root.

`select-option` selects the given value in a real HTML `<select>` on the active
tab and triggers the normal change event. For example,
`select-option: #structure = avl` picks a completed AVL implementation.

`open-site: PATH` opens a page of the Workspace site itself, logged in as the
screenshot account, as the `preview` tab (`open-site: /tic80/` for TIC-80).
The tutorial's `waitForPreview` hook runs after it, as for `go-live`.

`neo4j-browser` opens the screenshot account's Neo4j Browser as the `preview`
tab, the way the »Neo4j« button in the menu does (`/api/start_neo4j`), and logs
in with the username and password from the profile page. Everything after that
uses the general instructions; `:clear` removes earlier result frames:

```text
neo4j-browser
tab: preview
viewport: 1029x700
click: selector:[data-testid=main-editor] .cm-content
press: Control+A
type: :clear
press: Control+Enter
press: Control+A
type: MATCH (n) RETURN n LIMIT 25;
press: Control+Enter
wait-for-text: Started streaming 25 records
```

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

`type: <- SOURCE` puts a whole text into the focused editor, from the same
sources as `write-file` (`previous-code`, `code:…`, `file:…`), without its
final line break. It arrives as a paste, which the editor takes as it is;
typed, it would get closing brackets and indentation added. The language pages type their programs this way into
an unsaved file; the source's checksum is part of the screenshot state.

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

`click`, `hold`, `move-mouse` and `select-option` give up after 10 seconds
when their element does not show up or cannot be used: they address something
that is already on screen, and a mistyped selector should fail fast.

`variants: NAMES` captures the shot (image or video) once more for every
combination of the named variants switched on. The tutorial's hooks switch
them: `variants: { NAME: async ({ page, on, timeout }) => … }` in
`tutorial-screenshot-hooks.mjs`, where `on` says whether this variant should
be on. The shot itself is captured with all of them off, and they are off
again afterwards, also when the shot is only replayed. The extra files are
named after the variants in recipe order: `variants: dark anaglyph` on
`box.webp` writes `box-dark.webp`, `box-anaglyph.webp` and
`box-dark-anaglyph.webp`. The site knows the names `dark`, `anaglyph` and
`crt` (main.rb `add_media_variants`, `media_variants.js`): the dark file in
the dark theme, the anaglyph file once the reader switches on the glasses
button in the image's corner, the CRT file (PixelRAM's tube-screen filter)
with the screen button beside it. A missing combination falls back to the
closest one.

`sharp-canvas` renders the Workspace at capture resolution for this shot.
Normally the Workspace renders at its layout scale and the capture enlarges it:
DOM text is redrawn sharp at the larger size, but a `<canvas>` keeps its
smaller pixel buffer and comes out blurry. The PDF in LaTeX Workshop's viewer
is such a canvas. Preview tabs always render at capture resolution.

`show-mouse` draws a mouse pointer into the screenshot. Chromium does not
draw one, so the generator puts a pointer where the recipe last moved the
mouse with `move-mouse`, `click` or `hold`. Its shape follows the CSS cursor
under it, as on a real screen: a hand over buttons and links, an I-beam over
text, otherwise the arrow. Actions that may click on their own (`open-file`,
`close-tab`, `clone-start` and other Workspace actions) forget the position;
`show-mouse` after one of them, without a new `move-mouse`, is an error rather
than a pointer in the wrong place. It works for images, not with `record`.

```text
move-mouse: selector:.pane-header a[aria-label="Checkpoint erstellen"]
sleep: 1.5
show-mouse
```

`wait-for-text` waits up to 30 seconds until the selected page contains the given
visible text, including the text of its frames (VS Code webviews such as the
ER-Werkstatt or the Markdown preview are iframes). Whitespace is normalized
before matching. `wait-for-input-value`
performs the same kind of readiness check against visible `<input>` and
`<textarea>` values. The latter is useful for VS Code Quick Input because input
values are not part of `document.body.innerText`.

### Assertions: does the page's step work?

A recipe replays what the page tells students to do, so it can also check
that it works. `wait-for-text` already fails the render when its text does not
appear. Three more instructions only check:

```text
expect-no-text: Traceback
check-task: lock
check-task-fails: lock python
```

- `expect-no-text: TEXT` fails if the selected page shows the text now, in any
  frame: an error message, a Traceback, a file that should be gone.
- `terminal-expect-no-text: TEXT` does the same for the Workspace's terminals,
  whatever tab the recipe captures: `error:` after a compiler, `***` after
  `make`.
- `check-task: ID [ruby|python]` runs the check of task `ID` from the
  tutorial's own `tasks.yaml` exactly as the »Prüfen« button does, as the
  screenshot user against their Workspace, in the given language variant
  (without one: the task as written). The render fails unless the task
  passes. Like »Prüfen«, it records the result for the screenshot user.
- `check-task-fails: ID [ruby|python]` expects the check to reject: the step
  before the task must not already solve it.

Assertions change nothing on screen, so they are not part of any state hash:
adding or removing one re-renders nothing. They run whenever the recipe is
replayed, in a render of that shot or of a later one.

Steps the page describes without a picture go into a recipe of their own,
`<!-- tutorial-step … -->`: the same instructions, no capture and no `<img>`
after it.

```text
<!-- tutorial-step
write-file: bobby_tables/login.py <- file:login.py
terminal-run: printf "admin' #\n\n" | python3 login.py
wait-for-text: Herzlich willkommen, admin
-->
```

A step's actions run in its own `tab` before the next screenshot's actions and
count towards that screenshot's state, since they change what is on screen
later. Steps after the last screenshot run at the end of every replay, so
`src/scripts/recreate-screenshots.rb PAGE` walks through the whole page and
fails at the first assertion that does not hold.

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
will not show another prompt until that program exits. It waits up to 60
seconds; `terminal-wait-for-prompt: 180` waits longer, for a command that
takes its time (LuaLaTeX building its font index in a fresh Workspace).

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

Two terminals side by side, a server on the left and `curl` on the right:
hide the sidebar while the editor still has focus (with the terminal focused,
<kbd>Ctrl</kbd>+<kbd>B</kbd> goes to the shell), then maximize the panel and
split it. `terminal-run` always types into the last, right-hand terminal.

```text
hide-left-sidebar
terminal-open
terminal-maximize
terminal-run: ruby server.rb
press: Control+Shift+5
terminal-run: curl -i localhost:8000
```

To restart the server in a later recipe, do what a student does: go to the
left pane, stop the server, start it again. Do not use »Terminal: Kill All
Terminals« followed by `terminal-run`: the new shell is not ready yet and
loses the typed command, while the Enter still arrives.

```text
terminal-open
terminal-maximize
press: Alt+ArrowLeft
press: Control+C
press: Control+L
type: ruby server.rb
press: Enter
sleep: 2
terminal-run: clear
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

`crop-left` and `crop-right` cut the sides off, in percent of the width like
`crop-top` and `crop-bottom`, for a detail such as one corner of the status bar.
They work for screenshots, not for videos, PDF pages or graphs.

`crop-element: SELECTOR` crops a screenshot to exactly the box of the first
element the CSS selector matches, such as the screen of an emulator. It
replaces the other crops. An element below the fold is scrolled into view
first; it has to fit into the viewport.

### PDF pages

`pdf-page: PATH PAGE` makes the recipe's image from page PAGE of a PDF below
`/workspace`, usually one the recipe has just compiled there: rendered at 300
dpi, cut to its content, 1 mm shaved off the top and bottom. With TEXT, the
page must contain it (`pdf-page: latex-fonts/fonts.pdf 17 Inter`), so a page
number that no longer fits stops the render. The LaTeX
tutorial shows its font samples this way, set with the fonts the Workspace
really has.

`pdf-page-full: PATH PAGE [TEXT]` is the same without the cutting: the whole
sheet with its margins, at 200 dpi, for a page that should look like a page
(the pages of the Facharbeit in the LaTeX activity).

### Files and editor state

`open-file` opens a Workspace file through Quick Open and waits for the Monaco
editor to settle. A file a recipe has just written may take a few seconds to
appear in Quick Open; `open-file` asks again until it does (up to 30 seconds). `close-tab` closes an editor tab by its exact label and is a
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

## Git object graphs

A recipe before `_include_svg(NAME.svg)` produces that SVG instead of a
screenshot. Its only capture instruction is `git-graph: PATH`, a repository
below the screenshot Workspace's `/workspace`; the actions before it bring the
repository into the state the graph shows:

```text
<!-- tutorial-screenshot
terminal-run: git commit -m "initial commit"
terminal-wait-for-prompt
git-graph: git-tutorial/alice
-->

_include_svg(graph0.svg)
```

`git-graph.js` reads the repository with `git cat-file`, `rev-parse`,
`show-ref` and `symbolic-ref` only, and Graphviz draws it: every commit, tree
and blob reachable from HEAD, the branches and the tags, one column per type.
What HEAD stands for (its commit, tree and blobs) is in full colour, older
history faded, author details hidden. Commits get new ids on every render
(their dates change); blobs and trees keep theirs, so the text may name them
(»Hello, world!« is always `af5626b`). The graphs and the screenshots around
them come from the same repository, so their ids agree with each other, as
long as they come from the same run: every replay makes new commits, and
`--stale` captures only the stale files. After changing a Git recipe, render
the whole tutorial (`recreate-screenshots.rb git`) so that screenshots and
graphs show the same commit ids again.

`terminal-wait-for-prompt` also accepts the second line of bash-git-prompt
(`19:09 $`), which the Git tutorial installs halfway through.

## Videos

A recipe may also stand before a `<video>` whose `src` (or the `src` of its
first `<source>`) ends in `.mp4` or `.webm`. Such a recipe needs `record:`;
`record:` before an `<img>` is an error.

```html
<!-- tutorial-screenshot
# Der Torus dreht sich
write-file: scene.ini <- previous-code
tab: preview
preview-reload
record: 12s
fps: 30
crop-top: 20%
crop-bottom: 20%
-->

<video src='animation1.mp4' autoplay loop muted></video>
```

The video shows the same clip a screenshot of that tab would, frame by frame.
The generator does not let the clock run during capture. For every frame it
calls the tutorial's hook `setPreviewTime({ page, seconds })` with the frame's
time (0, 1/fps, 2/fps, …), and the hook puts the application at exactly that
moment and waits until it has drawn it. Slow and fast machines therefore make
the same video, and a 12-second video can take longer than 12 seconds to
render. After the last frame the generator calls
`setPreviewTime({ page, seconds: null })`, so later recipes see the page in
real time again. ffmpeg (installed in the generator image) encodes H.264 in
`yuv420p` with `+faststart` for `.mp4`, VP9 for `.webm`; an odd width or height
loses one pixel. A recipe with `record:` but no `setPreviewTime` hook fails
with a message saying so. Videos do not appear in the filmstrip; they replace
their file like any other generated output.

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

    // Only needed for videos (record:). seconds is null when the recording
    // is over and the page should run in real time again.
    async setPreviewTime({ page, seconds, timeout }) {
        await page.evaluate(t => window.app.showTime(t), seconds);
    },
};
```

`actions` adds recipe instructions only this tutorial's application knows,
called as `hook: NAME ARGS` on the page the recipe's `tab` selects. The
anaglyph tutorial sets the viewer's camera this way
(`hook: camera 20 20 -450`) and switches its anaglyph mode
(`hook: anaglyph off`):

```js
export default {
    actions: {
        async camera({ page, args, timeout }) { /* ... */ },
    },
};
```

An action also gets `writeFile(path, text)`, which writes into the screenshot
user's Workspace exactly like `write-file`: for a value only the page knows, such
as the login of a database the action just created in the profile, written into
a configuration file as a student would copy it.

`writeFileSubdirectory` makes `write-file` paths relative to that directory below
the screenshot user's Workspace instead of the Workspace root. This only affects
`write-file`; `wait-for-file` and `wait-for-file-newer` remain explicitly relative
to the Workspace root so their recipe paths stay unambiguous.

`waitForPreview` runs after `DOMContentLoaded` whenever `go-live`,
`preview-reload`, or `preview-reset` opens or reloads the preview. Tutorials that
do not provide a hook simply continue after `DOMContentLoaded`. The hook file's
SHA-256 is included in the environment fingerprint, so changing hook behavior
invalidates that tutorial's generated screenshots automatically.

The global screenshot profile lives in `config.rb`. It reproduces the look of
the hand-made screenshots, a small browser window at a high zoom: a
1029x516 CSS-pixel viewport (`1853 / 1.8` by `929 / 1.8`) captured at scale
1.8, so the default image is 1853x929 pixels. The renderer deliberately
does not choose a light or dark color scheme: code-server loads the Workspace's
actual VS Code theme, and the screenshot generator waits until that theme has
remained stable before running the recipe. The page keeps its normal layout while
Chromium renders the screenshot capture at the requested scale.

The Workspace renders at device pixel ratio 1 and the capture enlarges it by
1.8; its screenshots are DOM text and SVG, which Chromium draws sharp at any
capture scale. A preview tab renders at device pixel ratio 1.8 and is captured
unscaled, so a `<canvas>` (an anaglyph scene, a sandbox, the Neo4j graph)
draws at full resolution instead of being enlarged. Both give the same image
size for the same layout.

A recipe's `viewport` is in the same CSS pixels. To keep the look of the other
screenshots, change only what needs to change and keep the width at 1029:
`viewport: 1029x560` gives the terminal two more rows. `viewport: 1853x1000`
would lay the Workspace out on almost twice the area, and everything in the
image looks small.

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
