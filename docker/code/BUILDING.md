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

`./build-image.sh` refreshes those stages, builds a *candidate*, clones the
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

**Building elsewhere:** A server without room for the build can take the
finished image from a machine that has it. `./upload-image.sh HOST
[REMOTE_DIR]` compresses `hs_code_server:latest` with zstd (about 7 GB for
the 17.8 GB image, in October 2026; loading it needs about twice its size
on the server for a moment), sends it with rsync, which continues
after a dropped connection, checks it on the server, loads it there, starts
it once and removes the old image when no container uses it any more. Put
`REMOTE_DIR` on a disk with room (on the production server
`/mnt/hackschule`); `CHECK_ONLY=1` looks at the server without changing
anything. The toolchain images and BuildKit's cache stay on the building
machine; the server needs only the student image.
