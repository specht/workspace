#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
export DOCKER_BUILDKIT=1

# Alternative entry point: leave the existing build-image.sh unchanged.
FONT_DIR="src/static/include/fonts/0xProtoNerdFontMono"
for asset in \
    0xProtoNerdFontMono-Regular.woff2 \
    0xProtoNerdFontMono-Bold.woff2 \
    0xProtoNerdFontMono-Italic.woff2 \
    LICENSE NERD-FONTS-README.md; do
    if [ ! -s "$FONT_DIR/$asset" ]; then
        echo "Missing required font asset: $FONT_DIR/$asset" >&2
        exit 1
    fi
done

# The three small executor images are independent of the code-server image.
# Build sequentially on the disk-constrained production server.
echo '=== Building executor images ==='
docker build -t hackschule-exec-ruby src/codebites/executor/ruby
docker build -t hackschule-exec-python src/codebites/executor/python
docker build -t hackschule-exec-javascript src/codebites/executor/javascript

echo '=== Preparing VS Code extensions ==='
cp src/vscode-extensions/sidebar-init/hackschule-sidebar-init-0.0.1.vsix \
    docker/code/hackschule-sidebar-init-0.1.0.vsix
cp src/vscode-extensions/checkpoints/hackschule-checkpoints-0.6.1.vsix \
    docker/code/hackschule-checkpoints-0.6.1.vsix
cp src/vscode-extensions/keyboard-tutorial/hackschule-keyboard-tutorial-0.5.4.vsix \
    docker/code/hackschule-keyboard-tutorial-0.5.4.vsix
cp src/vscode-extensions/bif-watcher/bif-authoring-tools-0.2.1.vsix \
    docker/code/bif-authoring-tools-0.2.1.vsix
cp src/vscode-extensions/live-server/ritwickdey.LiveServer-5.7.10.vsix \
    docker/code/ritwickdey.LiveServer.vsix

echo '=== Building reusable toolchains ==='
bash docker/code/build-toolchains.sh

echo '=== Building code-server candidate ==='
bash docker/code/check-root-space.sh
docker build -t hs_code_server:candidate docker/code

echo '=== Verifying the LaTeX tutorial in the candidate ==='
docker run --rm --user 0 --entrypoint /bin/bash \
    -e HOME=/tmp \
    hs_code_server:candidate \
    -lc 'set -euo pipefail
         git clone --depth 1 https://github.com/specht/latex-tutorial.git /tmp/latex-tutorial
         cd /tmp/latex-tutorial
         for file in hello.tex Bewerbung.tex CV.tex Ausarbeitung.tex Mathe.tex wpgtr.tex; do
             latexmk -lualatex -interaction=nonstopmode -halt-on-error -file-line-error "$file"
             test -s "${file%.tex}.pdf"
         done
         test -s Ausarbeitung.bbl
         grep -Fq mueller2022 Ausarbeitung.bbl'

docker tag hs_code_server:candidate hs_code_server:latest
echo '=== Done ==='
