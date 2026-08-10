#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TIC80_REF="${TIC80_REF:-4aba09c98f1e5028b82765be1647677b08d35942}"
EMSCRIPTEN_VERSION="${EMSCRIPTEN_VERSION:-6.0.6}"
BUILD_IMAGE="hs_code_tic80"
SOURCE_DIR="$ROOT/temp/TIC-80"
OUTPUT_DIR="$ROOT/src/static/tic80"
BRIDGE_JS="$ROOT/docker/tic80/hackschule-persistence.js"

printf 'Building TIC-80 %s with Emscripten %s\n' "$TIC80_REF" "$EMSCRIPTEN_VERSION"

docker build \
    --build-arg "EMSCRIPTEN_VERSION=$EMSCRIPTEN_VERSION" \
    -t "$BUILD_IMAGE" \
    "$ROOT/docker/tic80"

if [[ -d "$SOURCE_DIR/.git" ]]; then
    printf 'Reusing existing TIC-80 checkout in %s\n' "$SOURCE_DIR"
    git -C "$SOURCE_DIR" fetch origin "$TIC80_REF"
else
    rm -rf "$SOURCE_DIR"
    git clone https://github.com/nesbox/TIC-80.git "$SOURCE_DIR"
fi

git -C "$SOURCE_DIR" checkout --detach "$TIC80_REF"
git -C "$SOURCE_DIR" reset --hard "$TIC80_REF"
git -C "$SOURCE_DIR" submodule update --init --recursive

# Replace TIC-80's browser-only IDBFS persistence with the Hackschule bridge.
# The patch is intentionally tiny and checked against exact source text.  Since
# TIC80_REF is pinned, an unexpected upstream source layout should fail loudly.
python3 - "$SOURCE_DIR/src/system/sdl/main.c" <<'PY'
from pathlib import Path
import sys

path = Path(sys.argv[1])
source = path.read_text()

old_sync = '''            FS.syncfs(false, function()
            {
                Module.syncing = false;
            });'''
new_sync = '''            Module.hsTic80Sync(function()
            {
                Module.syncing = false;
            });'''

old_start = '''            FS.mkdirTree(dir);

            FS.mount(IDBFS, {}, dir);
            FS.syncfs(true, function(e)
            {
                dynCall('iiii', $1, [$2, $3, $0]);
            });'''
new_start = '''            FS.mkdirTree(dir);
            Module.hsTic80Load(dir, function()
            {
                dynCall('iiii', $1, [$2, $3, $0]);
            });'''

for name, old, new in [
    ('runtime sync', old_sync, new_sync),
    ('startup load', old_start, new_start),
]:
    count = source.count(old)
    if count != 1:
        raise SystemExit(f'Could not patch TIC-80 {name}: expected 1 match, found {count}')
    source = source.replace(old, new)

path.write_text(source)
PY

# Older runs may have created root-owned files in the bind-mounted build tree.
# Repair those once inside Docker; no host sudo is required.
HOST_UID="$(id -u)"
HOST_GID="$(id -g)"
docker run --rm \
    -v "$SOURCE_DIR:/src" \
    "$BUILD_IMAGE" \
    bash -lc "chown -R $HOST_UID:$HOST_GID /src/build"

docker run --rm \
    --user "$HOST_UID:$HOST_GID" \
    -e HOME=/tmp \
    -v "$SOURCE_DIR:/src" \
    "$BUILD_IMAGE" \
    bash -lc '
        set -euo pipefail
        cd /src/build
        emcmake cmake \
            -DBUILD_PRO=ON \
            -DBUILD_WITH_ALL=ON \
            -DBUILD_SDLGPU=ON \
            -DBUILD_STATIC=ON \
            -DCMAKE_BUILD_TYPE=MinSizeRel \
            .. --fresh
        cmake --build . --config MinSizeRel --parallel 2
    '

# Prepend the server persistence bridge and version the WASM request.  We no
# longer run js-beautify, so the old "?? =" / "|| =" repair hacks disappear.
python3 - "$SOURCE_DIR/build/bin/tic80.js" "$BRIDGE_JS" "$TIC80_REF" <<'PY'
from pathlib import Path
import sys

js_path = Path(sys.argv[1])
bridge_path = Path(sys.argv[2])
ref = sys.argv[3]

generated = js_path.read_text()
if 'tic80.wasm' not in generated:
    raise SystemExit('Could not find tic80.wasm in generated TIC-80 JavaScript')

generated = generated.replace('tic80.wasm', f'tic80.wasm?v={ref[:12]}')
bridge = bridge_path.read_text().rstrip() + '\n\n'
js_path.write_text(bridge + generated)
PY

mkdir -p "$OUTPUT_DIR"
find "$SOURCE_DIR/build/bin" -maxdepth 1 -type f -name 'tic80*' -exec cp -f {} "$OUTPUT_DIR/" \;

printf 'TIC-80 web build installed in %s\n' "$OUTPUT_DIR"