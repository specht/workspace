# syntax=docker/dockerfile:1.7

# ====================================================================
# Version pins
#
# Keep defaults here so upgrades are easy to find.
# Each ARG is redeclared only immediately before the stage/layer that
# consumes it, which avoids unnecessary BuildKit cache invalidation.
# ====================================================================

ARG CODE_SERVER_IMAGE=linuxserver/code-server:4.131.0

ARG TL_YEAR=2026
ARG FLUTTER_VERSION=3.47.0
ARG ANDROID_CMDLINE_TOOLS_VERSION=15859902
ARG ANDROID_CMDLINE_TOOLS_SHA256=4e4c464f145a7512b57d088ac6c278c03c9eea610886b35a5e0804e74eedf583
ARG ANDROID_PLATFORM=36
ARG ANDROID_BUILD_TOOLS_VERSION=36.1.0
ARG ANDROID_BUILD_TOOLS_COMPAT_VERSION=36.0.0
ARG ANDROID_CMAKE_VERSION=3.22.1
ARG ANDROID_NDK_VERSION=28.2.13676358
ARG EMSCRIPTEN_VERSION=6.0.6
ARG RAYLIB_VERSION=6.0

ARG NODE_MAJOR=24
ARG GNUCOBOL_MAJOR=4
ARG GUILE_VERSION=3.0
ARG LUA_VERSION=5.4
ARG JAVA_VERSION=25
ARG SMALLTALK_VERSION=3.2.5
ARG HUGO_VERSION=0.164.0
ARG DOTNET_VERSION=10.0
ARG POWERSHELL_VERSION=7.6.4
ARG RUBY_VERSION=3.4.10
ARG RUBY_API_VERSION=3.4.0
ARG RUBY_SHA256=6f32ad662baafc228d12030dbcd284f83b034dd4337b300dc84ac74d11a1eb68

ARG HACKSCHULE_SIDEBAR_INIT_VERSION=0.1.0
ARG HACKSCHULE_CHECKPOINTS_VERSION=0.6.0
ARG HACKSCHULE_KEYBOARD_TUTORIAL_VERSION=0.5.3
ARG BIF_AUTHORING_TOOLS_VERSION=0.2.1

# ====================================================================
# TeX Live stage
#
# Kept separate because this is enormous and almost never changes.
# Changing normal packages in the final image will NOT reinstall TeX.
# ====================================================================

FROM ${CODE_SERVER_IMAGE} AS texlive

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
        'selected_scheme scheme-full' \
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

# Do NOT run "tlmgr update --self --all" here.
# This is a fresh installation already downloaded from CTAN.

RUN /usr/local/texlive/${TL_YEAR}/bin/x86_64-linux/tlmgr \
        option repository ctan \
    && /usr/local/texlive/${TL_YEAR}/bin/x86_64-linux/mktexlsr


# ====================================================================
# Flutter stage
#
# Also separate so normal Dockerfile changes do not force Flutter to
# download and precache itself again.
# ====================================================================

FROM ${CODE_SERVER_IMAGE} AS flutter


RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    rm -f /etc/apt/apt.conf.d/docker-clean \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        git \
        unzip \
        wget \
        xz-utils \
        libglu1-mesa

ARG FLUTTER_VERSION

RUN --mount=type=cache,id=hackschule-downloads,target=/var/cache/hackschule-downloads \
    set -eux; \
    archive=/var/cache/hackschule-downloads/flutter-${FLUTTER_VERSION}.tar.xz; \
    if [ ! -s "$archive" ]; then \
        wget -q \
            "https://storage.googleapis.com/flutter_infra_release/releases/stable/linux/flutter_linux_${FLUTTER_VERSION}-stable.tar.xz" \
            -O "${archive}.tmp"; \
        mv "${archive}.tmp" "$archive"; \
    fi; \
    mkdir -p /opt/flutter; \
    tar -xf "$archive" \
        -C /opt/flutter \
        --strip-components=1

ENV FLUTTER_ROOT=/opt/flutter
ENV FLUTTER_SUPPRESS_ANALYTICS=true
ENV PATH="${FLUTTER_ROOT}/bin:${PATH}"

RUN git config --system --add safe.directory /opt/flutter \
    && flutter precache --web --android \
    && flutter --version


# ====================================================================
# Android SDK stage
#
# Keep the Android SDK separate from Flutter itself. This lets Flutter
# and Android tooling be updated independently while keeping both large
# downloads cacheable.
# ====================================================================

FROM ${CODE_SERVER_IMAGE} AS android-sdk

ARG DEBIAN_FRONTEND=noninteractive
ARG JAVA_VERSION

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    rm -f /etc/apt/apt.conf.d/docker-clean \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        openjdk-${JAVA_VERSION}-jdk-headless \
        unzip \
        wget

ARG ANDROID_CMDLINE_TOOLS_VERSION
ARG ANDROID_CMDLINE_TOOLS_SHA256
ARG ANDROID_PLATFORM
ARG ANDROID_BUILD_TOOLS_VERSION
ARG ANDROID_BUILD_TOOLS_COMPAT_VERSION
ARG ANDROID_CMAKE_VERSION
ARG ANDROID_NDK_VERSION

ENV ANDROID_HOME=/opt/android-sdk
ENV ANDROID_SDK_ROOT=/opt/android-sdk
ENV PATH="${ANDROID_HOME}/cmdline-tools/latest/bin:${PATH}"

RUN --mount=type=cache,id=hackschule-downloads,target=/var/cache/hackschule-downloads \
    set -eux; \
    archive=/var/cache/hackschule-downloads/commandlinetools-linux-${ANDROID_CMDLINE_TOOLS_VERSION}_latest.zip; \
    if [ ! -s "$archive" ]; then \
        wget -q \
            "https://dl.google.com/android/repository/commandlinetools-linux-${ANDROID_CMDLINE_TOOLS_VERSION}_latest.zip" \
            -O "${archive}.tmp"; \
        mv "${archive}.tmp" "$archive"; \
    fi; \
    printf '%s  %s\n' \
        "${ANDROID_CMDLINE_TOOLS_SHA256}" \
        "$archive" \
        | sha256sum -c -; \
    rm -rf /tmp/android-cmdline-tools; \
    mkdir -p /tmp/android-cmdline-tools "${ANDROID_HOME}/cmdline-tools"; \
    unzip -q "$archive" -d /tmp/android-cmdline-tools; \
    mv \
        /tmp/android-cmdline-tools/cmdline-tools \
        "${ANDROID_HOME}/cmdline-tools/latest"; \
    rm -rf /tmp/android-cmdline-tools

RUN yes \
        | sdkmanager \
            --sdk_root="${ANDROID_HOME}" \
            --licenses \
            >/dev/null \
    && sdkmanager \
        --sdk_root="${ANDROID_HOME}" \
        "platform-tools" \
        "platforms;android-${ANDROID_PLATFORM}" \
        "build-tools;${ANDROID_BUILD_TOOLS_VERSION}" \
        "build-tools;${ANDROID_BUILD_TOOLS_COMPAT_VERSION}" \
        "cmake;${ANDROID_CMAKE_VERSION}" \
        "ndk;${ANDROID_NDK_VERSION}"


# ====================================================================
# Flutter + Android verification/cache stage
#
# Build one tiny APK during the image build. Besides verifying the
# complete toolchain, this leaves Gradle/AGP artifacts in a cache that
# students can reuse instead of downloading them in every container.
# ====================================================================

FROM flutter AS flutter-android

ARG DEBIAN_FRONTEND=noninteractive
ARG JAVA_VERSION

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    rm -f /etc/apt/apt.conf.d/docker-clean \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        openjdk-${JAVA_VERSION}-jdk-headless

COPY --from=android-sdk \
    /opt/android-sdk \
    /opt/android-sdk

ENV ANDROID_HOME=/opt/android-sdk
ENV ANDROID_SDK_ROOT=/opt/android-sdk
ENV GRADLE_USER_HOME=/opt/gradle-cache
ENV PATH="${PATH}:${ANDROID_HOME}/cmdline-tools/latest/bin"

RUN set -eux; \
    mkdir -p "${GRADLE_USER_HOME}"; \
    printf '%s\n' \
        'org.gradle.jvmargs=-Xmx2G -XX:MaxMetaspaceSize=512m -XX:ReservedCodeCacheSize=256m -XX:+HeapDumpOnOutOfMemoryError' \
        'org.gradle.workers.max=2' \
        'kotlin.compiler.execution.strategy=in-process' \
        > "${GRADLE_USER_HOME}/gradle.properties"; \
    flutter create \
        --empty \
        --platforms=android \
        /tmp/flutter_android_smoke; \
    cd /tmp/flutter_android_smoke; \
    flutter build apk --debug; \
    test -s build/app/outputs/flutter-apk/app-debug.apk; \
    cd /; \
    rm -rf \
        /tmp/flutter_android_smoke \
        "${GRADLE_USER_HOME}/daemon"; \
    find "${GRADLE_USER_HOME}" \
        -type f \
        -name '*.lock' \
        -delete; \
    chmod -R a+rwX "${GRADLE_USER_HOME}"


# ====================================================================
# Emscripten + raylib WebAssembly
# ====================================================================

FROM ${CODE_SERVER_IMAGE} AS raylib-web

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

RUN wget -q \
        "https://github.com/raysan5/raylib/archive/refs/tags/${RAYLIB_VERSION}.tar.gz" \
        -O /tmp/raylib.tar.gz \
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
    && rm -f /tmp/raylib-test.c /tmp/raylib-test.js /tmp/raylib-test.wasm

# ====================================================================
# Actual Hackschule image
# ====================================================================

FROM ${CODE_SERVER_IMAGE}

ARG DEBIAN_FRONTEND=noninteractive

# --------------------------------------------------------------------
# Copy the large, stable SDKs/toolchains FIRST.
#
# This order is intentional. If we later change an apt package,
# Docker can still reuse these layers.
# --------------------------------------------------------------------

COPY --from=texlive \
    /usr/local/texlive \
    /usr/local/texlive

COPY --from=flutter \
    /opt/flutter \
    /opt/flutter

COPY --from=android-sdk \
    /opt/android-sdk \
    /opt/android-sdk

COPY --from=flutter-android \
    /opt/gradle-cache \
    /opt/gradle-cache

COPY --from=raylib-web \
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
# the container, so don't rely on build-time ownership.
#
# Flutter treats parts of its SDK checkout as mutable tool state, including
# packages/flutter_tools/.dart_tool, so the installation must stay writable.
RUN chmod -R a+rwX /opt/flutter \
    && git config --system --add safe.directory /opt/flutter \
    && chmod -R a+rwX /opt/gradle-cache \
    && chmod -R a+rwX /opt/emsdk/upstream/emscripten/cache

# --------------------------------------------------------------------
# Environment
# --------------------------------------------------------------------

ARG TL_YEAR

ENV TEXLIVE_DIR=/usr/local/texlive/${TL_YEAR}
ENV PATH=${TEXLIVE_DIR}/bin/x86_64-linux:$PATH
ENV MANPATH=${TEXLIVE_DIR}/texmf-dist/doc/man
ENV INFOPATH=${TEXLIVE_DIR}/texmf-dist/doc/info

ENV ANDROID_HOME=/opt/android-sdk
ENV ANDROID_SDK_ROOT=/opt/android-sdk
ENV GRADLE_USER_HOME=/opt/gradle-cache
ENV PATH="${PATH}:${ANDROID_HOME}/cmdline-tools/latest/bin"

ENV FLUTTER_ROOT=/opt/flutter
ENV FLUTTER_SUPPRESS_ANALYTICS=true

# Flutter ships the Dart SDK it was tested with. Keep both CLIs together
# instead of mixing Flutter with a separately installed Dart release.
ENV PATH="${FLUTTER_ROOT}/bin:${PATH}"


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
        gnucobol${GNUCOBOL_MAJOR} \
        gnupg \
        golang-go \
        graphviz \
        guile-${GUILE_VERSION} \
        host \
        htop \
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
        tar \
        telnet \
        traceroute \
        tree \
        unzip \
        uuid-dev \
        vim \
        wget \
        whois \
        xz-utils \
        zip \
        zlib1g-dev


# --------------------------------------------------------------------
# GNU Smalltalk
#
# Download is persistent between builds and compilation uses all CPUs.
# --------------------------------------------------------------------

ARG SMALLTALK_VERSION

RUN --mount=type=cache,id=hackschule-downloads,target=/var/cache/hackschule-downloads \
    set -eux; \
    archive=/var/cache/hackschule-downloads/smalltalk-${SMALLTALK_VERSION}.tar.gz; \
    if [ ! -s "$archive" ]; then \
        wget -q \
            "https://ftp.fau.de/gnu/smalltalk/smalltalk-${SMALLTALK_VERSION}.tar.gz" \
            -O "${archive}.tmp"; \
        mv "${archive}.tmp" "$archive"; \
    fi; \
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
# Hugo
# --------------------------------------------------------------------

ARG HUGO_VERSION

RUN --mount=type=cache,id=hackschule-downloads,target=/var/cache/hackschule-downloads \
    set -eux; \
    deb=/var/cache/hackschule-downloads/hugo_${HUGO_VERSION}_linux-amd64.deb; \
    if [ ! -s "$deb" ]; then \
        wget -q \
            "https://github.com/gohugoio/hugo/releases/download/v${HUGO_VERSION}/hugo_${HUGO_VERSION}_linux-amd64.deb" \
            -O "${deb}.tmp"; \
        mv "${deb}.tmp" "$deb"; \
    fi; \
    dpkg -i "$deb"


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

RUN --mount=type=cache,id=hackschule-downloads,target=/var/cache/hackschule-downloads \
    set -eux; \
    deb=/var/cache/hackschule-downloads/powershell-${POWERSHELL_VERSION}.deb; \
    if [ ! -s "$deb" ]; then \
        wget -q \
            "https://github.com/PowerShell/PowerShell/releases/download/v${POWERSHELL_VERSION}/powershell_${POWERSHELL_VERSION}-1.deb_amd64.deb" \
            -O "${deb}.tmp"; \
        mv "${deb}.tmp" "$deb"; \
    fi; \
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

RUN emcc --check \
    && printf 'int main(void) { return 0; }\n' > /tmp/emscripten-test.c \
    && emcc /tmp/emscripten-test.c -o /tmp/emscripten-test.js \
    && test -f /tmp/emscripten-test.wasm \
    && test -f /opt/emsdk/upstream/emscripten/cache/sysroot/lib/wasm32-emscripten/libraylib.a \
    && test -f /opt/emsdk/upstream/emscripten/cache/sysroot/include/raylib.h \
    && printf '#include <raylib.h>\nint main(void) { InitWindow(64, 64, "raylib"); CloseWindow(); return 0; }\n' \
        > /tmp/raylib-test.c \
    && emcc /tmp/raylib-test.c -lraylib -sUSE_GLFW=3 -o /tmp/raylib-test.js \
    && test -f /tmp/raylib-test.wasm \
    && rm -f \
        /tmp/emscripten-test.c \
        /tmp/emscripten-test.js \
        /tmp/emscripten-test.wasm \
        /tmp/raylib-test.c \
        /tmp/raylib-test.js \
        /tmp/raylib-test.wasm \
    && chmod -R a+rwX /opt/emsdk/upstream/emscripten/cache

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


# --------------------------------------------------------------------
# Startup scripts — always last
# --------------------------------------------------------------------

COPY init-code-server-run \
    /etc/s6-overlay/s6-rc.d/init-code-server/run

COPY svc-code-server-run \
    /etc/s6-overlay/s6-rc.d/svc-code-server/run


WORKDIR /workspace