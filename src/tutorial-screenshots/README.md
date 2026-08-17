# Tutorial screenshots

In development, tutorial screenshots can be generated from small recipes embedded
next to the normal `<img>` tag. The image filename stays exactly the same as for a
hand-made screenshot and the generated file is written into the tutorial directory
as a lossless WebP. After that, Workspace's existing `/webcache` image pipeline
handles it like any other tutorial image. There is no separate screenshot command in
the normal authoring loop: reloading a tutorial is the trigger.

A manual image needs no recipe:

```html
<img class='full' src='go-live.webp' alt=''>
```

An automated image has a comment immediately before it:

```html
<!-- tutorial-screenshot
clone-start: https://github.com/specht/bif.git
crop-top: 33.5%
crop-bottom: 33.6%
-->

<img class='full' src='git-clone.webp' alt=''>
```

On a development reload, `git-clone.webp` is generated when it is missing or
stale. A `.tutorial-screenshots.json` file in the same tutorial directory records
the recipe-state hash and the Workspace image fingerprint. Both the generated
WebP files and this manifest are normal source files and should be committed.

The state hash is chained: each screenshot depends on its own recipe and on the
state hash of every automated screenshot before it. `write-file` also includes the
selected tutorial code block in the hash. Editing an early step therefore
invalidates the screenshots that follow it. Rebuilding `hs_code_server` changes
the environment fingerprint and transparently regenerates the same named images
on the next development reload.

To deliberately recreate every generated screenshot for one tutorial, run the
helper from the repository root while the development services are running:

```bash
src/scripts/recreate-screenshots.rb bif
```

The argument may also be a Markdown path such as `bif/bif.md` or
`src/content/bif/bif.md`. Forced recreation ignores manifest freshness, replays
the complete recipe chain, overwrites the generated images in place, and writes a
fresh manifest. Manual tutorial images are untouched. The helper follows the
screenshot-generator log while it runs, so target filenames and individual recipe
steps are visible directly in the terminal.

## Recipe language

Recipes are intentionally line-oriented. The BIF pilot currently uses:

```text
close-folder
show-left-sidebar
left-sidebar-width: CSS_PIXELS
hide-left-sidebar
show-right-sidebar
hide-right-sidebar
show-bottom-panel
hide-bottom-panel
clone-start: URL
clone-start: URL @ SHA1
clone-confirm-url
clone-accept-destination
clone-open
open-file: PATH
go-live
write-file: PATH <- previous-code
write-file: PATH <- previous-code (LABEL)
write-file: PATH <- code:UNIQUE TEXT||ANOTHER UNIQUE TEXT
write-file: PATH <- file:RELATIVE_PATH
preview-reload
preview-reset
click: BUTTON OR LINK TEXT
click: selector:CSS_SELECTOR
hold: SECONDSs BUTTON OR LINK TEXT
hold: SECONDSs selector:CSS_SELECTOR
press: PlaywrightKey
sleep: SECONDS
wait-for-text: TEXT

tab: workspace|preview
viewport: WIDTHxHEIGHT
zoom: NUMBER
crop-top: PERCENT%
crop-bottom: PERCENT%
```

Clone screenshots can pin the public repository to a full 40-character SHA-1:

```text
clone-start: https://github.com/specht/bif.git @ b5215fa72545f05f00d2ba23865c4e2eeff691a2
```

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

Unpinned `clone-start: URL` keeps its existing behavior and clones from the remote
normally.

`click` matches either an accessible button or an accessible link with the exact
given name in the preview tab. Prefix the target with `selector:` to address a
single preview element by CSS selector instead, which is useful for icon-only
controls whose visible text is not a stable target.

`hold` targets controls in the same way as `click`, but keeps the real Chromium
pointer pressed for the requested duration before releasing it. Durations use an
`s` suffix and must be between 0.05 and 30 seconds. For example, BIF's hold-to-
confirm restart control can be driven without depending on its translated label:

```text
hold: 1.5s selector:.story-restart-control
```

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

`file:RELATIVE_PATH` reads the source contents from a file next to the tutorial
Markdown (or from a subdirectory beneath it). The file contents are included in
the screenshot state hash, so editing that file invalidates the screenshot just
like editing an inline code block.

`left-sidebar-width` resizes the primary sidebar through VS Code's own sash, so
the workbench layout reacts exactly as it does when the divider is dragged by
hand. The width is specified in CSS pixels and is intentionally per recipe rather
than a global screenshot setting.
`sleep` pauses recipe execution for the given number of seconds (fractions are
allowed, up to 300 seconds) and is useful when an application has no better
observable readiness signal.
`wait-for-text` waits until the given text is visibly present in the page selected
by `tab` (`workspace` by default). Whitespace is normalized before matching, so
text split across lines can still be used as a readiness signal. The wait times
out after 60 seconds.

The global screenshot profile lives in `config.rb`. It defaults to 1853x929 and
150% browser-style zoom. The renderer deliberately does not choose a light or dark
color scheme: code-server loads the Workspace's actual VS Code theme, and the
screenshot generator waits until that theme has remained stable before running the
recipe. The renderer gives the page the smaller CSS viewport that a browser has at
that zoom level and then asks Chromium to capture that viewport at the requested
scale. Thus VS Code lays itself out at 150% while the generated bitmap still has
the configured 1853x929 pixels.

`viewport` and `zoom` describe the state in which the selected tab is operated as
well as captured. The renderer applies that profile before recipe actions run and
again as soon as a newly-created target tab becomes available. This means actions
that depend on layout can safely follow a custom zoom, for example:

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
waits for it; a reload whose screenshots are all current does not.

The screenshot account still logs in as `screenshots@example.com`, but the Unix
workspace user is configured separately and defaults to `student`. This keeps
terminal prompts suitable for student-facing screenshots.

If generation fails, the development server logs the error and continues parsing.
An existing image remains usable; a genuinely missing image simply stays missing
until the next reload succeeds. Production never invokes the generator.
