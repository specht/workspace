# syntax=docker/dockerfile:1.7

ARG CODE_SERVER_IMAGE=linuxserver/code-server:4.129.0


# ====================================================================
# TeX Live stage
#
# Kept separate because this is enormous and almost never changes.
# Changing normal packages in the final image will NOT reinstall TeX.
# ====================================================================

FROM ${CODE_SERVER_IMAGE} AS texlive

ARG DEBIAN_FRONTEND=noninteractive
ARG TL_YEAR=2025

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

ARG FLUTTER_VERSION=3.44.7

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
ENV PATH="${PATH}:${FLUTTER_ROOT}/bin"

RUN git config --system --add safe.directory /opt/flutter \
    && flutter config --no-analytics \
    && flutter precache --web \
    && flutter --version


# ====================================================================
# Actual Hackschule image
# ====================================================================

FROM ${CODE_SERVER_IMAGE}

ARG DEBIAN_FRONTEND=noninteractive
ARG TL_YEAR=2025
ARG DART_VERSION=3.8.2
ARG HUGO_VERSION=0.148.1
ARG POWERSHELL_VERSION=7.4.11

# --------------------------------------------------------------------
# Copy the two huge, stable SDKs FIRST.
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

# The numeric UID of abc can change when linuxserver initializes
# the container, so don't rely on build-time ownership.
#
# Each student runs in a separate container, therefore their changes
# to the Flutter SDK remain isolated in that container's writable layer.
RUN chmod -R a+rwX /opt/flutter \
    && git config --system --add safe.directory /opt/flutter

# --------------------------------------------------------------------
# Environment
# --------------------------------------------------------------------

ENV TEXLIVE_DIR=/usr/local/texlive/${TL_YEAR}
ENV PATH=${TEXLIVE_DIR}/bin/x86_64-linux:$PATH
ENV MANPATH=${TEXLIVE_DIR}/texmf-dist/doc/man
ENV INFOPATH=${TEXLIVE_DIR}/texmf-dist/doc/info

ENV FLUTTER_ROOT=/opt/flutter

# Append Flutter rather than prepend it:
# the standalone Dart installation therefore remains the default `dart`.
ENV PATH="${PATH}:${FLUTTER_ROOT}/bin"


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

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    rm -f /etc/apt/apt.conf.d/docker-clean \
    && curl -fsSL https://deb.nodesource.com/setup_22.x | bash - \
    && apt-get update \
    && apt-get install --no-install-recommends -y \
        apt-transport-https \
        autoconf \
        bash-completion \
        bsdmainutils \
        build-essential \
        bwbasic \
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
        gnucobol4 \
        gnupg \
        golang-go \
        graphviz \
        guile-3.0 \
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
        lua5.4 \
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
        openjdk-17-jdk-headless \
        perl \
        pgcli \
        php-cli \
        postgresql-client \
        python3 \
        python3-dev \
        python3-ipykernel \
        python3-mysql.connector \
        python3-pip \
        python3-venv \
        rsync \
        ruby \
        ruby-dev \
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

RUN --mount=type=cache,id=hackschule-downloads,target=/var/cache/hackschule-downloads \
    set -eux; \
    archive=/var/cache/hackschule-downloads/smalltalk-3.2.5.tar.gz; \
    if [ ! -s "$archive" ]; then \
        wget -q \
            https://ftp.fau.de/gnu/smalltalk/smalltalk-3.2.5.tar.gz \
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

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    add-apt-repository ppa:dotnet/backports -y \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        dotnet-sdk-9.0


# --------------------------------------------------------------------
# PowerShell
# --------------------------------------------------------------------

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
# Standalone Dart SDK
# --------------------------------------------------------------------

RUN --mount=type=cache,id=hackschule-downloads,target=/var/cache/hackschule-downloads \
    set -eux; \
    deb=/var/cache/hackschule-downloads/dart_${DART_VERSION}-1_amd64.deb; \
    if [ ! -s "$deb" ]; then \
        wget -q \
            "https://storage.googleapis.com/dart-archive/channels/stable/release/${DART_VERSION}/linux_packages/dart_${DART_VERSION}-1_amd64.deb" \
            -O "${deb}.tmp"; \
        mv "${deb}.tmp" "$deb"; \
    fi; \
    dpkg -i "$deb"


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


# --------------------------------------------------------------------
# Ruby
# --------------------------------------------------------------------

RUN echo 'export GEM_HOME=$HOME/.gem' >> /root/.bashrc


# --------------------------------------------------------------------
# Hackschule extensions
#
# Keep everything below here as late as possible because these files
# change frequently.
# --------------------------------------------------------------------

COPY hackschule-sidebar-init-0.1.0.vsix \
    /app/extensions/hackschule-sidebar-init-0.1.0.vsix

COPY hackschule-checkpoints-0.6.0.vsix \
    /app/extensions/hackschule-checkpoints-0.6.0.vsix

COPY hackschule-keyboard-tutorial-0.5.2.vsix \
    /app/extensions/hackschule-keyboard-tutorial-0.5.2.vsix

COPY bif-authoring-tools-0.2.0.vsix \
    /app/extensions/bif-authoring-tools-0.2.0.vsix


# --------------------------------------------------------------------
# Startup scripts — always last
# --------------------------------------------------------------------

COPY init-code-server-run \
    /etc/s6-overlay/s6-rc.d/init-code-server/run

COPY svc-code-server-run \
    /etc/s6-overlay/s6-rc.d/svc-code-server/run


WORKDIR /workspace