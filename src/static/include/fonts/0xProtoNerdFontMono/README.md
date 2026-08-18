# 0xProto Nerd Font Mono webfonts

This directory is the single source for the browser-facing Hackschule 0xProto Nerd Font Mono assets.
The Hackschule website serves these files directly, and the Workspace nginx proxy exposes the same
files to the code-server workbench.

Add these files before building:

- `0xProtoNerdFontMono-Regular.woff2`
- `0xProtoNerdFontMono-Bold.woff2`
- `0xProtoNerdFontMono-Italic.woff2`
- `LICENSE`
- `NERD-FONTS-README.md`

Use the corresponding `0xProtoNerdFontMono-*.ttf` files from the official Nerd Fonts 0xProto release
as the source when producing the WOFF2 files. Preserve the font metadata; the CSS/VS Code family name
must remain `0xProto Nerd Font Mono`.

Copy the 0xProto `LICENSE` alongside the webfonts. Also copy Nerd Fonts'
`patched-fonts/0xProto/README.md` to `NERD-FONTS-README.md`; it records the upstream font license plus
the authors, licenses, and repositories for the glyph sets added by Nerd Fonts.
