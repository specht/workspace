FROM linuxserver/code-server:4.102.2
RUN usermod -d /workspace abc
RUN curl -sL https://deb.nodesource.com/setup_22.x | sudo -E bash -
RUN sudo apt-get update && sudo apt-get install -y \
    wget \
    ruby ruby-dev libmagickcore-dev \
    python3 python3-pip \
    python3-dev libmysqlclient-dev libmysqlclient21 \
    python3-mysql.connector \
    mysql-client \
    golang-go \
    openjdk-17-jdk-headless \
    nodejs \
    lua5.4 \
    texlive-full \
    htop \
    build-essential file \
    libncurses5-dev iputils-ping \
    cmake autoconf \
    mc \
    gfortran bwbasic clisp mono-devel \
    golang-go \
    rustc cargo \
    nasm \
    gdb \
    emacs vim \
    bsdmainutils \
    iputils-tracepath traceroute \
    gnucobol4 \
    fp-compiler fp-ide \
    gawk \
    erlang \
    php-cli \
    tree \
    man-db \
    telnet \
    postgresql-client pgcli \
    bash-completion \
    libboost-dev libboost-date-time-dev libboost-thread-dev zlib1g-dev libpng-dev libjpeg8-dev libtiff5-dev libopenexr-dev \
    apt-transport-https \
    clang \
    guile-3.0 \
    maven \
    rsync \
    && sudo apt-get clean \
    && sudo rm -rf /var/lib/apt/lists/*
RUN wget -O /smalltalk.tar.gz https://ftp.gnu.org/gnu/smalltalk/smalltalk-3.2.5.tar.gz
RUN tar xzf /smalltalk.tar.gz
RUN rm /smalltalk.tar.gz
RUN cd /smalltalk-3.2.5 && ./configure && make && make install
RUN rm -rf /smalltalk-3.2.5
RUN curl -o /dart.deb https://storage.googleapis.com/dart-archive/channels/stable/release/latest/linux_packages/dart_3.8.2-1_amd64.deb
RUN dpkg -i /dart.deb  && rm /dart.deb 
RUN apt-get install -y mycli

RUN wget -q https://github.com/gohugoio/hugo/releases/download/v0.148.1/hugo_0.148.1_linux-amd64.deb
RUN dpkg -i hugo_0.148.1_linux-amd64.deb
RUN rm hugo_0.148.1_linux-amd64.deb

RUN apt-get update && apt-get install -y graphviz mtr whois dnsutils host ntpdate
RUN yes | unminimize

RUN apt-get update && apt-get install -y \
    software-properties-common wget build-essential libssl-dev libffi-dev libbz2-dev libreadline-dev libsqlite3-dev \
    zlib1g-dev libncurses5-dev libncursesw5-dev liblzma-dev libgdbm-dev uuid-dev \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir /tmp/python-build && cd /tmp/python-build \
    && wget https://www.python.org/ftp/python/3.10.12/Python-3.10.12.tgz \
    && tar xzf Python-3.10.12.tgz \
    && cd Python-3.10.12 \
    && ./configure --enable-optimizations \
    && make -j$(nproc) \
    && make altinstall

RUN python3.10 -m venv /opt/python-env \
    && /opt/python-env/bin/pip install --upgrade pip setuptools wheel \
    && /opt/python-env/bin/pip install matplotlib jax==0.4.21 jaxlib==0.4.21 numpy==1.26.4 keras==2.11.0 tensorflow==2.11.0 tensorflowjs==4.0.0 ipykernel \
    && chmod -R a+rx /opt/python-env

COPY init-code-server-run /etc/s6-overlay/s6-rc.d/init-code-server/run
COPY svc-code-server-run /etc/s6-overlay/s6-rc.d/svc-code-server/run
RUN echo "export GEM_HOME=\$HOME/.gem" >> /root/.bashrc
WORKDIR /workspace