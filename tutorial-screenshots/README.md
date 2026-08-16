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
clone-start: URL
clone-confirm-url
clone-accept-destination
clone-open
open-file: PATH
go-live
write-file: PATH <- previous-code
write-file: PATH <- code:UNIQUE TEXT||ANOTHER UNIQUE TEXT
preview-reload
preview-reset
click: BUTTON TEXT
press: PlaywrightKey

tab: workspace|preview
viewport: WIDTHxHEIGHT
zoom: NUMBER
crop-top: PERCENT%
crop-bottom: PERCENT%
```

`previous-code` means the last fenced code block before the recipe. `code:...`
selects the most recent earlier fenced code block containing all snippets separated
by `||`. This keeps the visible tutorial code as the source of truth instead of
duplicating it in the hidden recipe.

The default screenshot profile is 1853x929 at zoom 1. A recipe only needs
`viewport` or `zoom` when a particular tab should be captured differently.

If generation fails, the development server logs the error and continues parsing.
An existing image remains usable; a genuinely missing image simply stays missing
until the next reload succeeds. Production never invokes the generator.
