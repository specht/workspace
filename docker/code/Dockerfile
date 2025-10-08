FROM linuxserver/code-server:4.103.2

# Set user home directory early (rarely changed)
RUN usermod -d /workspace abc

# Install system dependencies in one RUN to reduce layers and improve cache reuse
RUN curl -sL https://deb.nodesource.com/setup_22.x | bash - \
    && apt-get update && apt-get install --no-install-recommends -y \
        wget ruby ruby-dev libmagickcore-dev zip unzip \
        python3 python3-pip python3-dev python3-venv python3-ipykernel libmysqlclient-dev libmysqlclient21 python3-mysql.connector \
        mysql-client golang-go openjdk-17-jdk-headless nodejs lua5.4 htop \
        texlive-latex-recommended texlive-latex-extra texlive-fonts-recommended \
        build-essential file libncurses5-dev iputils-ping cmake autoconf mc gfortran bwbasic sbcl mono-mcs mono-runtime \
        nasm gdb emacs-nox vim bsdmainutils iputils-tracepath traceroute gnucobol4 fp-compiler fp-ide \
        gawk erlang php-cli tree telnet postgresql-client pgcli bash-completion imagemagick \
        libboost-dev libboost-date-time-dev libboost-thread-dev zlib1g-dev libpng-dev libjpeg8-dev libtiff5-dev libopenexr-dev \
        apt-transport-https clang guile-3.0 maven rsync graphviz mtr whois dnsutils host ntpdate software-properties-common \
        libssl-dev libffi-dev libbz2-dev libreadline-dev libsqlite3-dev zlib1g-dev libncurses5-dev libncursesw5-dev liblzma-dev \
        mycli graphviz man-db manpages manpages-dev coreutils libgdbm-dev uuid-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install rustup to /workspace
# RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | HOME=/workspace sh -s -- -y

# Smalltalk build: download + build in one RUN to cache whole step, remove tarball afterward
RUN wget -O /smalltalk.tar.gz https://ftp.fau.de/gnu/smalltalk/smalltalk-3.2.5.tar.gz \
    && tar xzf /smalltalk.tar.gz \
    && rm /smalltalk.tar.gz \
    && cd /smalltalk-3.2.5 && ./configure && make && make install \
    && cd / && rm -rf /smalltalk-3.2.5

# Install hugo
RUN wget -q https://github.com/gohugoio/hugo/releases/download/v0.148.1/hugo_0.148.1_linux-amd64.deb \
    && dpkg -i hugo_0.148.1_linux-amd64.deb \
    && rm hugo_0.148.1_linux-amd64.deb

# Install dotnet
RUN sudo add-apt-repository ppa:dotnet/backports -y \
    && apt-get update && apt-get install -y dotnet-sdk-9.0 \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install PowerShell
RUN wget https://github.com/PowerShell/PowerShell/releases/download/v7.4.11/powershell_7.4.11-1.deb_amd64.deb \
    && dpkg -i powershell_7.4.11-1.deb_amd64.deb \
    && rm powershell_7.4.11-1.deb_amd64.deb

# Install Python 3.10 as well
# RUN apt install -y software-properties-common \
#     && add-apt-repository ppa:deadsnakes/ppa -y \
#     && apt update \
#     && apt install -y python3.10 python3.10-venv python3.10-dev python3.10-distutils python3.10-full \
#     && apt-get clean && rm -rf /var/lib/apt/lists/*

# # Python 3.10.12 build + install
# RUN mkdir /tmp/python-build && cd /tmp/python-build \
#     && wget https://www.python.org/ftp/python/3.10.12/Python-3.10.12.tgz \
#     && tar xzf Python-3.10.12.tgz \
#     && cd Python-3.10.12 \
#     && ./configure --enable-optimizations --with-lto \
#     && make -j$(nproc) \
#     && make altinstall \
#     && cd / && rm -rf /tmp/python-build && rm -rf /usr/local/lib/python3.10/test

# # Install Tensorflow etc.
# RUN python3.10 -m venv /opt/python-env \
#     && /opt/python-env/bin/pip install --no-cache-dir --upgrade pip setuptools wheel \
#     && /opt/python-env/bin/pip install --no-cache-dir matplotlib jax==0.4.21 jaxlib==0.4.21 numpy==1.26.4 keras==2.11.0 tensorflow-cpu==2.11.0 tensorflowjs==4.0.0 ipykernel \
#     && chmod -R a+rx /opt/python-env

# RUN apt-get purge -y \
#   libmysqlclient-dev libboost-dev libboost-thread-dev libssl-dev libbz2-dev \
#   libpng-dev zlib1g-dev \
#   && apt-get autoremove -y && apt-get clean

COPY dart.deb /
# Dart SDK install in one step
RUN dpkg -i /dart.deb && rm /dart.deb

# Add environment variable for gems only once
RUN echo "export GEM_HOME=\$HOME/.gem" >> /root/.bashrc

# Copy your custom init scripts (should be last, change this less frequently)
COPY init-code-server-run /etc/s6-overlay/s6-rc.d/init-code-server/run
COPY svc-code-server-run /etc/s6-overlay/s6-rc.d/svc-code-server/run

WORKDIR /workspace
