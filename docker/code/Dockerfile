FROM linuxserver/code-server:4.91.1
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
    gnu-smalltalk \
    erlang \
    php-cli \
    tree \
    man-db \
    telnet \
    postgresql-client pgcli \
    bash-completion \
    libboost-dev libboost-date-time-dev libboost-thread-dev zlib1g-dev libpng-dev libjpeg8-dev libtiff5-dev libopenexr-dev \
    clang \
    guile-3.0 \
    maven \
    rsync \
    && sudo apt-get clean \
    && sudo rm -rf /var/lib/apt/lists/*
RUN apt-get update && sudo apt-get install apt-transport-https
RUN wget -q https://github.com/gohugoio/hugo/releases/download/v0.147.0/hugo_0.147.0_linux-amd64.deb
RUN dpkg -i hugo_0.147.0_linux-amd64.deb
RUN rm hugo_0.147.0_linux-amd64.deb
RUN wget -qO- https://dl-ssl.google.com/linux/linux_signing_key.pub | gpg  --dearmor -o /usr/share/keyrings/dart.gpg
RUN echo 'deb [signed-by=/usr/share/keyrings/dart.gpg arch=amd64] https://storage.googleapis.com/download.dartlang.org/linux/debian stable main' | tee /etc/apt/sources.list.d/dart_stable.list
RUN apt-get update && apt-get install -y dart graphviz mtr whois dnsutils host ntpdate
RUN yes | unminimize
RUN pip install matplotlib numpy==1.26.4 keras==2.11.0 tensorflow==2.11.0 tensorflowjs==4.0.0
RUN pip install -U mycli
COPY init-code-server-run /etc/s6-overlay/s6-rc.d/init-code-server/run
COPY svc-code-server-run /etc/s6-overlay/s6-rc.d/svc-code-server/run
RUN echo "export GEM_HOME=\$HOME/.gem" >> /root/.bashrc
WORKDIR /workspace