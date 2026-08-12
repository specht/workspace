#!/bin/bash
set -euo pipefail

# Explicitly enable BuildKit because the Dockerfile uses cache mounts.
export DOCKER_BUILDKIT=1

echo "=== Building executor images ==="

docker build \
    -t hackschule-exec-ruby \
    src/codebites/executor/ruby &
PID_RUBY=$!

docker build \
    -t hackschule-exec-python \
    src/codebites/executor/python &
PID_PYTHON=$!

docker build \
    -t hackschule-exec-javascript \
    src/codebites/executor/javascript &
PID_JAVASCRIPT=$!

# Wait for all three and fail if one failed.
STATUS=0

wait "$PID_RUBY" || STATUS=1
wait "$PID_PYTHON" || STATUS=1
wait "$PID_JAVASCRIPT" || STATUS=1

if [ "$STATUS" -ne 0 ]; then
    echo "One or more executor images failed to build."
    exit 1
fi


echo "=== Preparing VS Code extensions ==="

cp \
    src/vscode-extensions/sidebar-init/hackschule-sidebar-init-0.0.1.vsix \
    docker/code/hackschule-sidebar-init-0.1.0.vsix

cp \
    src/vscode-extensions/checkpoints/hackschule-checkpoints-0.6.0.vsix \
    docker/code/hackschule-checkpoints-0.6.0.vsix

cp \
    src/vscode-extensions/keyboard-tutorial/hackschule-keyboard-tutorial-0.5.3.vsix \
    docker/code/hackschule-keyboard-tutorial-0.5.3.vsix

cp \
    src/vscode-extensions/bif-watcher/bif-authoring-tools-0.2.1.vsix \
    docker/code/bif-authoring-tools-0.2.1.vsix


echo "=== Building code-server image ==="

docker build \
    -t hs_code_server \
    docker/code

echo "=== Done ==="