#!/usr/bin/env bash
# Python and Ruby for the browser, for the turtle (src/static/include/turtle/):
# Pyodide and ruby.wasm in fixed versions from npm, each package checked
# against its SHA-256. Only what a worker loads is kept, next to a gzipped
# copy for nginx (gzip_static), so that a class does not make the server
# compress 25 MB again for every student. JavaScript needs nothing.
#
# The files are not in git (.gitignore); run this once after a clone and
# whenever the versions below change (turtle_worker.js names the same).
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEST="$ROOT/src/static/include/turtle/runtime"
REGISTRY="https://registry.npmjs.org"

PYODIDE=314.0.7
RUBY_WASM=2.10.1
WASI_SHIM=0.4.2

# package, version, file in the registry, sha256
PACKAGES=(
    "pyodide $PYODIDE pyodide/-/pyodide-$PYODIDE.tgz 66b7949e5a0cba2250b586d7e368bf12fb482800748d182c4618254b685ffaa0"
    "ruby-3.4-wasm-wasi $RUBY_WASM @ruby/3.4-wasm-wasi/-/3.4-wasm-wasi-$RUBY_WASM.tgz 2e709bd9eddefe1c5d63e9fc0391a35cce4754e936d8a2f24b8582f6d5c2ea88"
    "ruby-wasm-wasi $RUBY_WASM @ruby/wasm-wasi/-/wasm-wasi-$RUBY_WASM.tgz 1a4f58a452688b3d53d291539d35ea780cf10a40657694501b7169959512b265"
    "browser_wasi_shim $WASI_SHIM @bjorn3/browser_wasi_shim/-/browser_wasi_shim-$WASI_SHIM.tgz 9c0281520d0e99f027ec7c1c79b4036c0f8168ed9bf98aba19db4737a1333782"
)

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

for entry in "${PACKAGES[@]}"; do
    read -r name version path sum <<< "$entry"
    printf 'Fetching %s %s\n' "$name" "$version"
    curl -fsSL "$REGISTRY/$path" -o "$WORK/$name.tgz"
    echo "$sum  $WORK/$name.tgz" | sha256sum --check --quiet
    mkdir -p "$WORK/$name"
    tar xzf "$WORK/$name.tgz" -C "$WORK/$name"
done

rm -rf "$DEST"
PY="$DEST/pyodide-$PYODIDE"
RB="$DEST/ruby-$RUBY_WASM"
mkdir -p "$PY" "$RB/wasm-wasi" "$RB/browser_wasi_shim"

for file in pyodide.mjs pyodide.asm.mjs pyodide.asm.wasm python_stdlib.zip pyodide-lock.json LICENSE; do
    [[ -f "$WORK/pyodide/package/$file" ]] && cp "$WORK/pyodide/package/$file" "$PY/"
done
# Ruby without its standard library: the turtle needs none, and it is half the size
cp "$WORK/ruby-3.4-wasm-wasi/package/dist/ruby.wasm" "$RB/"
cp -r "$WORK/ruby-wasm-wasi/package/dist/esm/." "$RB/wasm-wasi/"
cp -r "$WORK/browser_wasi_shim/package/dist/." "$RB/browser_wasi_shim/"
for license in ruby-3.4-wasm-wasi ruby-wasm-wasi browser_wasi_shim; do
    for file in LICENSE LICENSE.txt LICENSE-MIT; do
        [[ -f "$WORK/$license/package/$file" ]] && cp "$WORK/$license/package/$file" "$RB/$license-$file"
    done
done
# A worker has no import map: the shim by its path, not by its package name.
sed -i 's#from "@bjorn3/browser_wasi_shim"#from "../browser_wasi_shim/index.js"#' "$RB"/wasm-wasi/*.js

find "$DEST" -type f \( -name '*.wasm' -o -name '*.mjs' -o -name '*.js' -o -name '*.json' \) -size +4k -exec gzip -k -9 {} \;
du -sh "$PY" "$RB"
