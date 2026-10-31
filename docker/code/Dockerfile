# syntax=docker/dockerfile:1.14

# ====================================================================
# Version pins
#
# Keep defaults here so upgrades are easy to find.
# Each ARG is redeclared only immediately before the stage/layer that
# consumes it, which avoids unnecessary BuildKit cache invalidation.
# ====================================================================

ARG CODE_SERVER_IMAGE=linuxserver/code-server:4.140.0@sha256:c84b35f6238af058e3543e2e9c0532c4d3d3902ef11effea3ed90e4ec293d46d
# Pin the toolchain build environment independently of the code-server release.
# Keep it compatible with the target code-server distribution (notably glibc).
# Updating CODE_SERVER_IMAGE alone must not rebuild TeX/Emscripten.
# Bump this separately when the underlying Linux distribution changes.
ARG TOOLCHAIN_BASE_IMAGE=linuxserver/code-server:4.138.0@sha256:6fb8bbaf3148a56027704830a63f5880cce31dda442b42d6cca66b29a505bad6

ARG TL_YEAR=2026
# Dart on its own, the version Flutter 3.47 brought (Flutter, the Android
# SDK and the Gradle cache left the image on 28 October 2026: no activity
# used them, and they were half its size; see TODO.md, »Flutter«)
ARG DART_VERSION=3.13.0
ARG DART_SHA256=87902573facd8acacac7ee1fe73fa8d0668e06065016068e2ed6c5c99c6b1ee0
ARG EMSCRIPTEN_VERSION=6.0.6
ARG RAYLIB_VERSION=6.0
# raylib publishes no checksum for its tag archive: the hash of the archive
# on 31 October 2026, so that a changed archive stops the build
ARG RAYLIB_SHA256=2b3ee1e2120c7a0796b33062c7e9a694dd8a8caa56a96319ac8c8ecf54a90d0b

ARG NODE_MAJOR=24
ARG GNUCOBOL_MAJOR=4
ARG GUILE_VERSION=3.0
ARG LUA_VERSION=5.4
ARG JAVA_VERSION=25
ARG SMALLTALK_VERSION=3.2.5
# the same from ftp.gnu.org and the fau.de mirror the build uses
ARG SMALLTALK_SHA256=06e574e818ec49f0555d948ae53b1453d8c1df59ef597dad911a4fd1ffba1cce
ARG HUGO_VERSION=0.164.0
# from the release's hugo_${HUGO_VERSION}_checksums.txt
ARG HUGO_SHA256=06f51ad0ec83e399e08a46a960f32453c109d6aad93e51ff8e659cdd5e51be7b
# Go from go.dev instead of Ubuntu's golang-go (1.22); the checksum from
# https://go.dev/dl/?mode=json
ARG GO_VERSION=1.27.3
ARG GO_SHA256=65efbc80fdc1fa2cd2631cad5b481777a68e8725f34e89f50fc6ac714e379b40
ARG YQ_VERSION=4.54.1
ARG YQ_SHA256=8e34fc298390875de416e6a4afcb8cabeceb25d9aa8506c1a2f9353cf702ea5f
ARG DOTNET_VERSION=10.0
ARG POWERSHELL_VERSION=7.6.4
# from the release's hashes.sha256
ARG POWERSHELL_SHA256=e5688e0569568d48051c49d3e93504cde47af709cdaaabd9a8892bc676b3bdf3
ARG RUBY_VERSION=3.4.10
ARG RUBY_API_VERSION=3.4.0
ARG NEO4J_BOLT_VERSION=0.5.1
ARG MYSQL_CONNECTOR_PYTHON_VERSION=26.7.0
ARG RUBY_SHA256=6f32ad662baafc228d12030dbcd284f83b034dd4337b300dc84ac74d11a1eb68

ARG HACKSCHULE_SIDEBAR_INIT_VERSION=0.2.0
ARG HACKSCHULE_CHECKPOINTS_VERSION=0.6.1
ARG HACKSCHULE_KEYBOARD_TUTORIAL_VERSION=0.5.4
ARG BIF_AUTHORING_TOOLS_VERSION=0.2.1
ARG HACKSCHULE_ER_VERSION=0.3.0
ARG HACKSCHULE_BACKROOMS_VERSION=0.1.1
ARG HACKSCHULE_UML_VERSION=0.1.0

# ====================================================================
# TeX Live stage
#
# Kept separate because this is enormous and almost never changes.
# Changing normal packages in the final image will NOT reinstall TeX.
# ====================================================================

FROM ${TOOLCHAIN_BASE_IMAGE} AS texlive

ARG DEBIAN_FRONTEND=noninteractive

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    rm -f /etc/apt/apt.conf.d/docker-clean \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        perl \
        wget \
        xz-utils \
        tar \
        fontconfig \
        ghostscript \
        ca-certificates \
        unzip \
        git \
        make

ARG TL_YEAR

RUN wget -q \
        https://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz \
        -O /tmp/install-tl.tar.gz \
    && mkdir /tmp/install-tl \
    && tar xzf /tmp/install-tl.tar.gz \
        -C /tmp/install-tl \
        --strip-components=1 \
    && printf '%s\n' \
        'selected_scheme scheme-basic' \
        "TEXDIR /usr/local/texlive/${TL_YEAR}" \
        'TEXMFLOCAL /usr/local/texlive/texmf-local' \
        "TEXMFSYSCONFIG /usr/local/texlive/${TL_YEAR}/texmf-config" \
        "TEXMFSYSVAR /usr/local/texlive/${TL_YEAR}/texmf-var" \
        'TEXMFHOME ~/texmf' \
        "TEXMFVAR ~/.texlive${TL_YEAR}/texmf-var" \
        "TEXMFCONFIG ~/.texlive${TL_YEAR}/texmf-config" \
        'binary_x86_64-linux 1' \
        'instopt_adjustpath 0' \
        'instopt_adjustrepo 1' \
        'instopt_letter 0' \
        'instopt_portable 0' \
        'instopt_write18_restricted 1' \
        > /tmp/texlive.profile \
    && /tmp/install-tl/install-tl \
        -profile /tmp/texlive.profile \
        -no-interaction \
    && rm -rf \
        /tmp/install-tl \
        /tmp/install-tl.tar.gz \
        /tmp/texlive.profile

# Keep only the compact official collections used for ordinary LuaLaTeX
# documents. In particular, collection-latexextra and collection-bibtexextra
# pull in hundreds/thousands of packages not needed by the tutorial.
# TeX Live package names are not always the same as .sty names (e.g. tabularx
# is supplied by latex-tools), so the exceptions below are TeX Live packages.
# The scheme-basic installation above is a separately cached Docker layer.
RUN /usr/local/texlive/${TL_YEAR}/bin/x86_64-linux/tlmgr install \
        collection-latexrecommended \
        collection-fontsrecommended \
        collection-langgerman \
        collection-luatex \
        latexmk biber \
        fontspec polyglossia unicode-math biblatex siunitx \
        microtype geometry hyperref parskip titlesec enumitem \
        tabto-ltx wrapfig setspace fancyhdr framed colortbl subfigure \
        mathdesign bera caption abstract nextpage listings \
        xcharter xcharter-math koma-script \
    && /usr/local/texlive/${TL_YEAR}/bin/x86_64-linux/tlmgr \
        option repository ctan \
    && /usr/local/texlive/${TL_YEAR}/bin/x86_64-linux/mktexlsr \
    && /usr/local/texlive/${TL_YEAR}/bin/x86_64-linux/lualatex --version \
    && /usr/local/texlive/${TL_YEAR}/bin/x86_64-linux/latexmk -v \
    && /usr/local/texlive/${TL_YEAR}/bin/x86_64-linux/biber --version

# Verify the real tutorial *before* tagging this stage as a reusable toolchain.
# A missing TeX dependency fails here, before Flutter/Android are built.
# Keep the clone and generated PDF files out of the resulting image layer.
ENV PATH="/usr/local/texlive/${TL_YEAR}/bin/x86_64-linux:${PATH}"

# CV.tex uses \setsansfont{Lato}. Install the system font before verifying
# the tutorial, in a separate layer so TeX Live packages stay cached.
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    apt-get update \
    && apt-get install -y --no-install-recommends fonts-lato \
    && fc-cache -f \
    && luaotfload-tool -u

# The legacy mathdesign fonts need the LY1 (TeX'n'ANSI) encoding.
# Keep this separate so the TeX Live and Lato layers remain cached.
RUN tlmgr install ly1 \
    && kpsewhich texnansi.enc

# pgfplots for diagrams from measured data, csquotes for the quotation
# marks biblatex asks for; a layer of their own, so those above stay cached.
# Cached, they may hold a tlmgr older than the repository's, which then
# refuses to install: tlmgr updates itself first (only itself, not --all).
RUN tlmgr update --self \
    && tlmgr install pgfplots csquotes \
    && kpsewhich pgfplots.sty csquotes.sty

RUN set -eu; \
    git clone --depth 1 https://github.com/specht/latex-tutorial.git /tmp/latex-tutorial; \
    cd /tmp/latex-tutorial; \
    for file in hello.tex Bewerbung.tex CV.tex Ausarbeitung.tex Mathe.tex wpgtr.tex; do \
        latexmk -lualatex -interaction=nonstopmode -halt-on-error -file-line-error "$file"; \
        test -s "${file%.tex}.pdf"; \
    done; \
    test -s Ausarbeitung.bbl; \
    grep -Fq mueller2022 Ausarbeitung.bbl; \
    cd /; \
    rm -rf /tmp/latex-tutorial

# Do NOT run "tlmgr update --self --all" here.
# This is a fresh installation already downloaded from CTAN.

# ====================================================================
# Emscripten + raylib WebAssembly
# ====================================================================

FROM ${TOOLCHAIN_BASE_IMAGE} AS raylib-web

ARG DEBIAN_FRONTEND=noninteractive

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    rm -f /etc/apt/apt.conf.d/docker-clean \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        git \
        make \
        python3 \
        wget \
        xz-utils

ARG EMSCRIPTEN_VERSION

# Emscripten
RUN git clone https://github.com/emscripten-core/emsdk.git /opt/emsdk \
    && cd /opt/emsdk \
    && ./emsdk install ${EMSCRIPTEN_VERSION} \
    && ./emsdk activate ${EMSCRIPTEN_VERSION}

# raylib
ARG RAYLIB_VERSION
ARG RAYLIB_SHA256

RUN wget -q \
        "https://github.com/raysan5/raylib/archive/refs/tags/${RAYLIB_VERSION}.tar.gz" \
        -O /tmp/raylib.tar.gz \
    && printf '%s  %s\n' "${RAYLIB_SHA256}" /tmp/raylib.tar.gz | sha256sum -c - \
    && mkdir /opt/raylib \
    && tar xzf /tmp/raylib.tar.gz \
        -C /opt/raylib \
        --strip-components=1 \
    && rm /tmp/raylib.tar.gz \
    && cd /opt/emsdk \
    && . ./emsdk_env.sh \
    && cd /opt/raylib/src \
    && make PLATFORM=PLATFORM_WEB -j"$(nproc)" \
    && sysroot=/opt/emsdk/upstream/emscripten/cache/sysroot \
    && install -m 644 libraylib.web.a "$sysroot/lib/wasm32-emscripten/libraylib.a" \
    && install -m 644 raylib.h raymath.h rlgl.h rcamera.h "$sysroot/include/"

RUN cd /opt/emsdk \
    && . ./emsdk_env.sh \
    && emcc --check \
    && printf '#include <raylib.h>\nint main(void) { InitWindow(64, 64, "raylib"); CloseWindow(); return 0; }\n' \
        > /tmp/raylib-test.c \
    && emcc /tmp/raylib-test.c -lraylib -sUSE_GLFW=3 -o /tmp/raylib-test.js \
    && test -f /tmp/raylib-test.wasm \
    && rm -f /tmp/raylib-test.c /tmp/raylib-test.js /tmp/raylib-test.wasm \
    && chmod -R a+rwX /opt/emsdk/upstream/emscripten/cache

# ====================================================================
# Actual Hackschule image
# ====================================================================

FROM ${CODE_SERVER_IMAGE}

ARG DEBIAN_FRONTEND=noninteractive
# These are independently built, locally tagged images. They contain the
# completed toolchains, so a code-server-only rebuild need not execute the
# heavy stages above. build-toolchains.sh refreshes each tag before assembling.

# --------------------------------------------------------------------
# Copy the large, stable SDKs/toolchains FIRST.
#
# This order is intentional. If we later change an apt package,
# Docker can still reuse these layers.
# --------------------------------------------------------------------

COPY --link --from=hs_code_toolchain_texlive:stable \
    /usr/local/texlive \
    /usr/local/texlive

COPY --link --from=hs_code_toolchain_emscripten:stable \
    /opt/emsdk \
    /opt/emsdk

ENV EMSDK=/opt/emsdk
ENV EM_CONFIG=/opt/emsdk/.emscripten
ENV EM_CACHE=/opt/emsdk/upstream/emscripten/cache
ENV PATH="/opt/emsdk/upstream/emscripten:${PATH}"

# raylib is installed into the Emscripten sysroot in the raylib-web stage.
# Projects can therefore use normal includes and linking:
#   #include <raylib.h>
#   emcc program.c -lraylib

# The numeric UID of abc can change when linuxserver initializes
# the container, so don't rely on build-time ownership. The Emscripten cache
# is made writable in raylib-web, before it is copied into this image.

# --------------------------------------------------------------------
# Environment
# --------------------------------------------------------------------

ARG TL_YEAR

ENV TEXLIVE_DIR=/usr/local/texlive/${TL_YEAR}
ENV PATH=${TEXLIVE_DIR}/bin/x86_64-linux:$PATH
ENV MANPATH=${TEXLIVE_DIR}/texmf-dist/doc/man
ENV INFOPATH=${TEXLIVE_DIR}/texmf-dist/doc/info

ENV PATH="/opt/dart-sdk/bin:${PATH}"

# No usage reporting: the tools that read it from the environment. Dart keeps
# its setting in the home directory, which is the student's volume; the
# Workspace's start switches it off there (init-code-server-run), and Go's
# (since 1.23 it counts locally, uploading nothing; off stops the counting).
ENV DART_SUPPRESS_ANALYTICS=true
ENV DOTNET_CLI_TELEMETRY_OPTOUT=1
ENV POWERSHELL_TELEMETRY_OPTOUT=1
ENV DO_NOT_TRACK=1


# --------------------------------------------------------------------
# User
# --------------------------------------------------------------------

RUN usermod -d /workspace abc


# --------------------------------------------------------------------
# Main system dependencies
#
# BuildKit keeps downloaded .deb files and package metadata between
# builds instead of downloading everything again after a cache miss.
# --------------------------------------------------------------------

ARG NODE_MAJOR
ARG GNUCOBOL_MAJOR
ARG GUILE_VERSION
ARG LUA_VERSION
ARG JAVA_VERSION

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    rm -f /etc/apt/apt.conf.d/docker-clean \
    && curl -fsSL "https://deb.nodesource.com/setup_${NODE_MAJOR}.x" | bash - \
    && apt-get update \
    && apt-get install --no-install-recommends -y \
        apt-transport-https \
        autoconf \
        bash-completion \
        bsdmainutils \
        build-essential \
        bwbasic \
        byobu \
        ca-certificates \
        clang \
        cmake \
        coreutils \
        curl \
        dnsutils \
        emacs-nox \
        erlang \
        file \
        fontconfig \
        fp-compiler \
        fp-ide \
        gawk \
        gdb \
        gfortran \
        ghostscript \
        git \
        glslang-tools \
        gnucobol${GNUCOBOL_MAJOR} \
        gnupg \
        graphviz \
        guile-${GUILE_VERSION} \
        host \
        htop \
        hyperfine \
        imagemagick \
        iputils-ping \
        iputils-tracepath \
        libboost-date-time-dev \
        libboost-dev \
        libboost-thread-dev \
        libbz2-dev \
        libffi-dev \
        libgdbm-dev \
        libgmp-dev \
        libglu1-mesa \
        libjpeg8-dev \
        liblzma-dev \
        libmagickcore-dev \
        libmysqlclient-dev \
        libmysqlclient21 \
        libncurses5-dev \
        libncursesw5-dev \
        libopenexr-dev \
        libpng-dev \
        libreadline-dev \
        libsqlite3-dev \
        libssl-dev \
        libtiff5-dev \
        libyaml-dev \
        lua${LUA_VERSION} \
        man-db \
        manpages \
        manpages-dev \
        maven \
        mc \
        mono-mcs \
        mono-runtime \
        mtr \
        mysql-client \
        mycli \
        nasm \
        nodejs \
        ntpdate \
        openjdk-${JAVA_VERSION}-jdk-headless \
        perl \
        pgcli \
        pkg-config \
        php-cli \
        python3 \
        python3-dev \
        python3-ipykernel \
        python3-mysql.connector \
        python3-pip \
        python3-venv \
        rsync \
        sbcl \
        software-properties-common \
        swi-prolog-nox \
        tar \
        telnet \
        traceroute \
        tree \
        unzip \
        uuid-dev \
        vim \
        wget \
        whois \
        xxd \
        xz-utils \
        zip \
        zlib1g-dev

# Dart, pinned and checked, from the same download cache as the toolchains
ARG DART_VERSION
ARG DART_SHA256
RUN --mount=type=cache,id=hackschule-downloads,target=/var/cache/hackschule-downloads \
    set -eux; \
    archive=/var/cache/hackschule-downloads/dartsdk-${DART_VERSION}-linux-x64.zip; \
    if [ ! -s "$archive" ]; then \
        wget -q \
            "https://storage.googleapis.com/dart-archive/channels/stable/release/${DART_VERSION}/sdk/dartsdk-linux-x64-release.zip" \
            -O "${archive}.tmp"; \
        mv "${archive}.tmp" "$archive"; \
    fi; \
    printf '%s  %s\n' "${DART_SHA256}" "$archive" | sha256sum -c -; \
    rm -rf /opt/dart-sdk /tmp/dart-sdk; \
    mkdir -p /tmp/dart-sdk; \
    unzip -q "$archive" -d /tmp/dart-sdk; \
    mv /tmp/dart-sdk/dart-sdk /opt/dart-sdk; \
    rm -rf /tmp/dart-sdk; \
    /opt/dart-sdk/bin/dart --version


# --------------------------------------------------------------------
# GNU Smalltalk
#
# Download is persistent between builds and compilation uses all CPUs.
# --------------------------------------------------------------------

ARG SMALLTALK_VERSION
ARG SMALLTALK_SHA256

RUN --mount=type=cache,id=hackschule-downloads,target=/var/cache/hackschule-downloads \
    set -eux; \
    archive=/var/cache/hackschule-downloads/smalltalk-${SMALLTALK_VERSION}.tar.gz; \
    if [ ! -s "$archive" ]; then \
        wget -q \
            "https://ftp.fau.de/gnu/smalltalk/smalltalk-${SMALLTALK_VERSION}.tar.gz" \
            -O "${archive}.tmp"; \
        mv "${archive}.tmp" "$archive"; \
    fi; \
    printf '%s  %s\n' "${SMALLTALK_SHA256}" "$archive" | sha256sum -c -; \
    mkdir -p /tmp/smalltalk; \
    tar xzf "$archive" \
        -C /tmp/smalltalk \
        --strip-components=1; \
    cd /tmp/smalltalk; \
    ./configure; \
    make -j"$(nproc)"; \
    make install; \
    cd /; \
    rm -rf /tmp/smalltalk


# --------------------------------------------------------------------
# Go, pinned and checked
# --------------------------------------------------------------------

ARG GO_VERSION
ARG GO_SHA256

RUN --mount=type=cache,id=hackschule-downloads,target=/var/cache/hackschule-downloads \
    set -eux; \
    archive=/var/cache/hackschule-downloads/go${GO_VERSION}.linux-amd64.tar.gz; \
    if [ ! -s "$archive" ]; then \
        wget -q "https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz" -O "${archive}.tmp"; \
        mv "${archive}.tmp" "$archive"; \
    fi; \
    printf '%s  %s\n' "${GO_SHA256}" "$archive" | sha256sum -c -; \
    rm -rf /usr/local/go; \
    tar -xzf "$archive" -C /usr/local; \
    /usr/local/go/bin/go version

ENV PATH="/usr/local/go/bin:${PATH}"
# what runs is the Go in the image: no toolchain downloaded because a
# project's go.mod asks for a newer one
ENV GOTOOLCHAIN=local


# --------------------------------------------------------------------
# Hugo
# --------------------------------------------------------------------

ARG HUGO_VERSION
ARG HUGO_SHA256

RUN --mount=type=cache,id=hackschule-downloads,target=/var/cache/hackschule-downloads \
    set -eux; \
    deb=/var/cache/hackschule-downloads/hugo_${HUGO_VERSION}_linux-amd64.deb; \
    if [ ! -s "$deb" ]; then \
        wget -q \
            "https://github.com/gohugoio/hugo/releases/download/v${HUGO_VERSION}/hugo_${HUGO_VERSION}_linux-amd64.deb" \
            -O "${deb}.tmp"; \
        mv "${deb}.tmp" "$deb"; \
    fi; \
    printf '%s  %s\n' "${HUGO_SHA256}" "$deb" | sha256sum -c -; \
    dpkg -i "$deb"


# --------------------------------------------------------------------
# yq: YAML (and JSON) in the terminal, like jq (Handbuch »YAML«)
# --------------------------------------------------------------------
#
# mikefarah/yq, a single Go binary. The checksum is the SHA-256 from the
# release's own list (»checksums«, column per »checksums_hashes_order«).

ARG YQ_VERSION
ARG YQ_SHA256

RUN --mount=type=cache,id=hackschule-downloads,target=/var/cache/hackschule-downloads \
    set -eux; \
    bin=/var/cache/hackschule-downloads/yq_${YQ_VERSION}_linux_amd64; \
    if [ ! -s "$bin" ]; then \
        wget -q \
            "https://github.com/mikefarah/yq/releases/download/v${YQ_VERSION}/yq_linux_amd64" \
            -O "${bin}.tmp"; \
        mv "${bin}.tmp" "$bin"; \
    fi; \
    echo "${YQ_SHA256}  ${bin}" | sha256sum -c -; \
    install -m 0755 "$bin" /usr/local/bin/yq; \
    yq --version


# --------------------------------------------------------------------
# .NET
# --------------------------------------------------------------------

ARG DOTNET_VERSION

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    add-apt-repository ppa:dotnet/backports -y \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        dotnet-sdk-${DOTNET_VERSION}


# --------------------------------------------------------------------
# PowerShell
# --------------------------------------------------------------------

ARG POWERSHELL_VERSION
ARG POWERSHELL_SHA256

RUN --mount=type=cache,id=hackschule-downloads,target=/var/cache/hackschule-downloads \
    set -eux; \
    deb=/var/cache/hackschule-downloads/powershell-${POWERSHELL_VERSION}.deb; \
    if [ ! -s "$deb" ]; then \
        wget -q \
            "https://github.com/PowerShell/PowerShell/releases/download/v${POWERSHELL_VERSION}/powershell_${POWERSHELL_VERSION}-1.deb_amd64.deb" \
            -O "${deb}.tmp"; \
        mv "${deb}.tmp" "$deb"; \
    fi; \
    printf '%s  %s\n' "${POWERSHELL_SHA256}" "$deb" | sha256sum -c -; \
    dpkg -i "$deb"


# --------------------------------------------------------------------
# pwndbg
# --------------------------------------------------------------------

RUN curl -qsL 'https://install.pwndbg.re' \
    | sh -s -- -t pwndbg-gdb


# --------------------------------------------------------------------
# Neo4j Cypher Shell
# --------------------------------------------------------------------

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    curl -fsSL \
        https://debian.neo4j.com/neotechnology.gpg.key \
        | gpg --dearmor \
        -o /usr/share/keyrings/neo4j.gpg \
    && echo \
        "deb [signed-by=/usr/share/keyrings/neo4j.gpg] https://debian.neo4j.com stable latest" \
        > /etc/apt/sources.list.d/neo4j.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        cypher-shell


# --------------------------------------------------------------------
# System fonts
# --------------------------------------------------------------------

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    apt-get update \
    && apt-get install -y --no-install-recommends \
        fonts-anonymous-pro \
        fonts-atkinson-hyperlegible \
        fonts-bebas-neue \
        fonts-cantarell \
        fonts-cmu \
        fonts-comfortaa \
        fonts-comic-neue \
        fonts-crosextra-caladea \
        fonts-crosextra-carlito \
        fonts-ebgaramond \
        fonts-firacode \
        fonts-hack \
        fonts-ibm-plex \
        fonts-inter \
        fonts-jetbrains-mono \
        fonts-lato \
        fonts-liberation \
        fonts-lmodern \
        fonts-montserrat \
        fonts-space-grotesk-ttf \
        fonts-stix \
        fonts-ubuntu \
        fonts-vollkorn \
    && fc-cache -f \
    && mktexlsr \
    && luaotfload-tool -u

# raylib-web already compiled a real raylib program. Keep the final-image
# check read-only so verifying the toolchain cannot copy the Emscripten cache
# into another image layer.
RUN command -v emcc \
    && test -f /opt/emsdk/upstream/emscripten/cache/sysroot/lib/wasm32-emscripten/libraylib.a \
    && test -f /opt/emsdk/upstream/emscripten/cache/sysroot/include/raylib.h

# emcc runs »clang --version« on every link. That clang looks for a GCC
# installation, finds the GCC 14 runtime libraries next to the system GCC 13
# and warns about it each time, which beginners read as an error in their
# program. A config file next to the drivers names the system GCC.
RUN dir="/usr/lib/gcc/x86_64-linux-gnu/$(gcc -dumpversion | cut -d. -f1)" \
    && test -d "$dir" \
    && for driver in clang clang++; do \
        echo "--gcc-install-dir=$dir" > "/opt/emsdk/upstream/bin/$driver.cfg"; \
    done \
    && ! /opt/emsdk/upstream/bin/clang --version 2>&1 | grep -q warning

# --------------------------------------------------------------------
# Ruby
# --------------------------------------------------------------------

ARG RUBY_VERSION
ARG RUBY_API_VERSION
ARG RUBY_SHA256

RUN --mount=type=cache,id=hackschule-downloads,target=/var/cache/hackschule-downloads \
    set -eux; \
    archive=/var/cache/hackschule-downloads/ruby-${RUBY_VERSION}.tar.xz; \
    if [ ! -s "$archive" ]; then \
        ruby_series="${RUBY_VERSION%.*}"; \
        wget -q \
            "https://cache.ruby-lang.org/pub/ruby/${ruby_series}/ruby-${RUBY_VERSION}.tar.xz" \
            -O "${archive}.tmp"; \
        mv "${archive}.tmp" "$archive"; \
    fi; \
    printf '%s  %s\n' \
        "${RUBY_SHA256}" \
        "$archive" \
        | sha256sum -c -; \
    rm -rf /tmp/ruby; \
    mkdir -p /tmp/ruby; \
    tar -xf "$archive" \
        -C /tmp/ruby \
        --strip-components=1; \
    cd /tmp/ruby; \
    ./configure \
        --prefix=/opt/ruby \
        --disable-install-doc \
        --disable-yjit; \
    make -j"$(nproc)"; \
    make install; \
    test "$(/opt/ruby/bin/ruby -e 'print RUBY_VERSION')" = "${RUBY_VERSION}"; \
    /opt/ruby/bin/gem --version; \
    /opt/ruby/bin/bundle --version; \
    cd /; \
    rm -rf /tmp/ruby

ENV RUBY_ROOT=/opt/ruby
ENV GEM_HOME=/workspace/.gem/ruby/${RUBY_API_VERSION}
ENV PATH="${GEM_HOME}/bin:${RUBY_ROOT}/bin:${PATH}"

RUN test "$(command -v ruby)" = /opt/ruby/bin/ruby \
    && test "$(gem environment home)" = "${GEM_HOME}"

# neo4j_bolt (MIT, https://github.com/specht/neo4j_bolt): `neo4j_bolt load`
# fills the student's Neo4j database from a dump (Filmdatenbank als Graph).
# It reads NEO4J_URI, NEO4J_USERNAME, NEO4J_PASSWORD and NEO4J_DATABASE.
# Installed next to Ruby, not into GEM_HOME, which lies in /workspace.
ARG NEO4J_BOLT_VERSION
RUN GEM_HOME=/opt/ruby/lib/ruby/gems/${RUBY_API_VERSION} \
        /opt/ruby/bin/gem install --no-document --bindir /opt/ruby/bin \
        neo4j_bolt -v "${NEO4J_BOLT_VERSION}" \
    && neo4j_bolt --version


# --------------------------------------------------------------------
# PHP: MySQL drivers
#
# php-cli above has PDO but no driver for MySQL. php-mysql adds pdo_mysql
# and mysqli, so PHP programs reach the student's
# databases. A layer of its own, so the large layers stay cached.
# --------------------------------------------------------------------

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    apt-get update \
    && apt-get install -y --no-install-recommends \
        php-mysql \
    && php -m | grep -qx pdo_mysql \
    && php -m | grep -qx mysqli


# --------------------------------------------------------------------
# Python: pip like gem, and a MySQL driver that works
#
# Ubuntu marks its Python as externally managed (PEP 668), so pip refuses
# every install outside a venv, even into the student's own folder. The
# container is disposable and students own what they install: without the
# marker, pip run by the student falls back to a user install into
# PYTHONUSERBASE, which lies in /workspace like GEM_HOME, so
# `pip install websockets` works like `gem install` and survives restarts.
# venvs behave as before.
#
# mysql-connector-python comes from pip, not apt: Ubuntu's 8.0.15 cannot
# log in to MySQL 8.4 under Python 3.12 (ssl.wrap_socket is gone, and
# caching_sha2_password needs SSL).
# --------------------------------------------------------------------

ARG MYSQL_CONNECTOR_PYTHON_VERSION
RUN apt-get purge -y python3-mysql.connector \
    && rm -f /usr/lib/python3*/EXTERNALLY-MANAGED \
    && pip install --no-cache-dir "mysql-connector-python==${MYSQL_CONNECTOR_PYTHON_VERSION}" \
    && python3 -c "import mysql.connector as m; assert m.__version__ == '${MYSQL_CONNECTOR_PYTHON_VERSION}', m.__version__"

ENV PYTHONUSERBASE=/workspace/.local
ENV PATH="${PYTHONUSERBASE}/bin:${PATH}"


# --------------------------------------------------------------------
# Hackschule extensions
#
# Keep everything below here as late as possible because these files
# change frequently.
# --------------------------------------------------------------------

ARG HACKSCHULE_SIDEBAR_INIT_VERSION
ARG HACKSCHULE_CHECKPOINTS_VERSION
ARG HACKSCHULE_KEYBOARD_TUTORIAL_VERSION
ARG BIF_AUTHORING_TOOLS_VERSION
ARG HACKSCHULE_ER_VERSION
ARG HACKSCHULE_BACKROOMS_VERSION
ARG HACKSCHULE_UML_VERSION

COPY hackschule-sidebar-init-${HACKSCHULE_SIDEBAR_INIT_VERSION}.vsix \
    /app/extensions/hackschule-sidebar-init-${HACKSCHULE_SIDEBAR_INIT_VERSION}.vsix

COPY hackschule-checkpoints-${HACKSCHULE_CHECKPOINTS_VERSION}.vsix \
    /app/extensions/hackschule-checkpoints-${HACKSCHULE_CHECKPOINTS_VERSION}.vsix

COPY hackschule-keyboard-tutorial-${HACKSCHULE_KEYBOARD_TUTORIAL_VERSION}.vsix \
    /app/extensions/hackschule-keyboard-tutorial-${HACKSCHULE_KEYBOARD_TUTORIAL_VERSION}.vsix

COPY bif-authoring-tools-${BIF_AUTHORING_TOOLS_VERSION}.vsix \
    /app/extensions/bif-authoring-tools-${BIF_AUTHORING_TOOLS_VERSION}.vsix

COPY ritwickdey.LiveServer.vsix \
    /app/extensions/ritwickdey.LiveServer.vsix

COPY jeff-hykin.better-prolog-syntax.vsix \
    /app/extensions/jeff-hykin.better-prolog-syntax.vsix

COPY hackschule-er-${HACKSCHULE_ER_VERSION}.vsix \
    /app/extensions/hackschule-er-${HACKSCHULE_ER_VERSION}.vsix

COPY hackschule-backrooms-${HACKSCHULE_BACKROOMS_VERSION}.vsix \
    /app/extensions/hackschule-backrooms-${HACKSCHULE_BACKROOMS_VERSION}.vsix

COPY hackschule-uml-${HACKSCHULE_UML_VERSION}.vsix \
    /app/extensions/hackschule-uml-${HACKSCHULE_UML_VERSION}.vsix


# --------------------------------------------------------------------
# No file capabilities. Workspaces run without NET_RAW and with
# no-new-privileges (src/ruby/include/workspace_runtime.rb), and the kernel
# refuses to start a binary whose file capability the container cannot
# grant. ping then uses unprivileged ICMP sockets
# (net.ipv4.ping_group_range, which Docker opens for every group).
# --------------------------------------------------------------------

RUN getcap -r /usr /opt /app 2>/dev/null | cut -d' ' -f1 | xargs -r -n 1 setcap -r; \
    test -z "$(getcap -r /usr /opt /app 2>/dev/null)"

# /admin »Betrieb« warns about an image built before this.
LABEL de.hackschule.file-capabilities="none"


# --------------------------------------------------------------------
# Man pages. Ubuntu's minimized base keeps none (/etc/dpkg/dpkg.cfg.d/
# excludes) and puts a note in place of man. From here on dpkg lets the
# English pages in; those of the packages installed above come
# out of their .deb files, only the pages, so that this layer stays a few
# dozen MB instead of every package once more. Late in the file on
# purpose: the layers above stay as they are, and an upload sends only
# this one. The .deb files stay in apt's cache for the next build.
# --------------------------------------------------------------------

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    set -eu; \
    sed -i 's|^path-exclude=/usr/share/man/\*$|&\npath-include=/usr/share/man/man*|' \
        /etc/dpkg/dpkg.cfg.d/excludes; \
    grep -q '^path-include=/usr/share/man/man\*' /etc/dpkg/dpkg.cfg.d/excludes; \
    apt-get update; \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --reinstall --no-install-recommends \
        manpages manpages-dev git-man; \
    installed="$(dpkg-query -W -f '${Status} ${binary:Package}=${Version}\n' | awk '$3 == "installed" { print $4 }')"; \
    # all at once; a package apt cannot fetch (installed from a file)
    # makes that fail, then one by one, skipping those
    apt-get install -y -q --reinstall --download-only $installed >/dev/null 2>&1 || \
        for p in $installed; do apt-get install -y -q --reinstall --download-only "$p" >/dev/null 2>&1 || echo "no .deb: $p"; done; \
    for p in $installed; do \
        name="${p%%=*}"; name="${name%%:*}"; version="$(echo "${p#*=}" | sed 's/:/%3a/')"; \
        for deb in /var/cache/apt/archives/"${name}_${version}"_*.deb; do \
            [ -f "$deb" ] || continue; \
            dpkg-deb --fsys-tarfile "$deb" | tar -x -C / --wildcards './usr/share/man/man*' 2>/dev/null || true; \
        done; \
    done; \
    if [ -f /usr/bin/man.REAL ]; then rm /usr/bin/man && dpkg-divert --quiet --remove --rename /usr/bin/man; fi; \
    export MANPATH="${MANPATH}:"; \
    mandb -q || true; \
    man -w ls >/dev/null; man -w 3 printf >/dev/null; man -w git-commit >/dev/null; \
    du -sh /usr/share/man

# The empty last part of MANPATH stands for the system's own pages
# (manpath.config); without it, man found only TeX Live's (ENV above).
ENV MANPATH=${TEXLIVE_DIR}/texmf-dist/doc/man:


# --------------------------------------------------------------------
# Startup scripts — always last
# --------------------------------------------------------------------

COPY init-code-server-run \
    /etc/s6-overlay/s6-rc.d/init-code-server/run

COPY svc-code-server-run \
    /etc/s6-overlay/s6-rc.d/svc-code-server/run


WORKDIR /workspace
