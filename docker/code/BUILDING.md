# Building the student code-server image

The code-server release (`CODE_SERVER_IMAGE`) and the toolchain build base
(`TOOLCHAIN_BASE_IMAGE`) are deliberately separate Dockerfile ARGs. Upgrading
code-server does not reinstall TeX Live, Flutter, Android or Emscripten. The
toolchain base is pinned to an already known-compatible code-server release;
upgrade it separately after checking operating-system compatibility.

From the repository root, `bash docker/code/build-toolchains.sh` builds the
five stages **sequentially**, saving each result as a locally tagged image:
`hs_code_toolchain_{texlive,flutter,android,gradle,emscripten}:stable`.
Re-running it uses BuildKit's existing cache for unchanged stage instructions.
The final Dockerfile imports directories from those five images via `COPY`.

`./build-image-optimized.sh` refreshes those stages, builds a *candidate*, clones the
real `specht/latex-tutorial` repository, compiles its six example documents
with `latexmk -lualatex`, checks the bibliography and only then updates the
`hs_code_server:latest` tag. A candidate that fails validation leaves the
existing production tag unchanged. The existing e2e tests cover the code-server
LaTeX Workshop UI and should still be run before deployment.

TeX Live uses `scheme-basic` plus explicitly chosen packages for the tutorial,
including LuaLaTeX, Biber, XCharter Math, KOMA-Script, and the older packages
used in `wpgtr.tex`. If a new document needs an additional package, add its
TeX Live package name to the `tlmgr install` block and rebuild the TeX stage;
do not revert to `scheme-full`. The build's tutorial check catches currently
missing dependencies, but cannot guarantee every possible future student
document will compile.

**Space warning for the production server:** Each tagged intermediate image
and the final image may occupy separate Docker layers. The initial rebuild
can need *more* space than the old build; this change is designed primarily
to make subsequent upgrades incremental. The root filesystem previously had
only 17 GB free, and the 297 GB VDO filesystem has much less free *physical*
backing space than `df` reports. Do not assume this patch makes the first
build safe to run without monitoring both pools.

The guard in `check-root-space.sh` defaults to at least 8 GiB free on `/`
between stages. It is not an absolute disk quota; keep `watch -n 5 'df -h /'`
open during every initial build. Do not run `docker builder prune -a` while
trying to preserve BuildKit's expensive stage caches. Existing running student
containers are not stopped by these build scripts.
