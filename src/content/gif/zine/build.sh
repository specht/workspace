#!/bin/sh
# Builds the GIF89a zine: gif89a-zine-a5.pdf to read, gif89a-zine-booklet-a4.pdf
# to print double-sided (flip on the short edge), fold and staple. Both land in
# src/static/gif/, where the teacher note of the GIF activity links them.
set -e
cd "$(dirname "$0")"
out=../../../static/gif
tmp=$(mktemp -d)
python3 zine.py spec-gif89a.txt "$tmp/zine.tex"
(cd "$tmp" && lualatex -interaction=nonstopmode zine.tex >/dev/null)
pdfjam --booklet true --landscape --a4paper "$tmp/zine.pdf" --outfile "$tmp/booklet.pdf" >/dev/null 2>&1
cp "$tmp/zine.pdf" "$out/gif89a-zine-a5.pdf"
cp "$tmp/booklet.pdf" "$out/gif89a-zine-booklet-a4.pdf"
rm -rf "$tmp"
echo "$out/gif89a-zine-a5.pdf, $out/gif89a-zine-booklet-a4.pdf"
