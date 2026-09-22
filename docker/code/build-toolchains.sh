#!/usr/bin/env bash
set -euo pipefail

# Run from the repository root, or directly to build/refresh the toolchains
# without also building the executors or the final code-server image.
cd "$(dirname "$0")/../.."
export DOCKER_BUILDKIT=1

build_stage() {
    local stage="$1" tag="$2"
    bash docker/code/check-root-space.sh
    echo "=== Building ${stage} -> ${tag} ==="
    docker build --target "$stage" -t "$tag" docker/code
    bash docker/code/check-root-space.sh
}

# Never build these large stages in parallel on the Workspace production host.
# Their stable tags are independently addressable COPY --from sources for the
# final image; a newer CODE_SERVER_IMAGE does not invalidate them.
build_stage texlive hs_code_toolchain_texlive:stable
build_stage flutter hs_code_toolchain_flutter:stable
build_stage android-sdk hs_code_toolchain_android:stable
build_stage flutter-android hs_code_toolchain_gradle:stable
build_stage raylib-web hs_code_toolchain_emscripten:stable

echo '=== Reusable toolchain images are ready ==='
