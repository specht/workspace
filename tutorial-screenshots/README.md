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
clone-start: URL <- local:REPO @ COMMIT
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
press: PlaywrightKey
sleep: SECONDS
wait-for-text: TEXT

tab: workspace|preview
viewport: WIDTHxHEIGHT
zoom: NUMBER
crop-top: PERCENT%
crop-bottom: PERCENT%
```

For tutorials that demonstrate cloning a public URL, development can use a local
repository for the actual Git transport while keeping the URL shown to students:

```text
clone-start: https://github.com/specht/bif.git <- local:bif @ b5215fa
```

`local:bif` means the `bif` repository below the local Git root. By default that
root is the parent directory of the Workspace checkout, which is convenient when
repositories such as `workspace` and `bif` are siblings. Override it when needed
before generating `docker-compose.yaml`:

```bash
TUTORIAL_SCREENSHOT_LOCAL_GIT_ROOT=/path/to/repos ./config.rb up -d
```

The commit must be either exactly seven hexadecimal characters or a full
40-character SHA-1. The generator resolves it in the local repository, makes a
private bare copy inside the disposable screenshot Workspace, points that copy's
HEAD branch at the pinned commit, and configures Git's `insteadOf` rewriting so
VS Code still clones the public URL entered in Quick Input. No network request is
made for that clone. The local Git root is mounted read-only into the screenshot
generator.

`click` matches either an accessible button or an accessible link with the exact
given name in the preview tab.

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
do not leak into tutorial images. The screenshot image configures Fontconfig with
`hintslight`, matching a desktop configured for slight font hinting while leaving
antialiasing and subpixel order at the container defaults.

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
