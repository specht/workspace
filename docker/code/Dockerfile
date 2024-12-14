FROM linuxserver/code-server:4.92.2
RUN usermod -d /workspace abc
RUN sudo apt-get update && sudo apt-get install -y \
    wget \
    ruby ruby-dev libmagickcore-dev \
    python3 python3-pip \
    python3-dev libmysqlclient-dev libmysqlclient21 \
    python3-mysql.connector \
    mysql-client \
    golang-go \
    openjdk-17-jdk-headless \
    lua5.4 \
    nodejs npm \
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
    && sudo apt-get clean \
    && sudo rm -rf /var/lib/apt/lists/*
RUN wget -O /smalltalk.tar.gz https://ftp.gnu.org/gnu/smalltalk/smalltalk-3.2.5.tar.gz
RUN tar xzf /smalltalk.tar.gz
RUN rm /smalltalk.tar.gz
RUN cd /smalltalk-3.2.5 && ./configure && make && make install
RUN rm -rf /smalltalk-3.2.5
RUN wget -qO- https://dl-ssl.google.com/linux/linux_signing_key.pub | gpg  --dearmor -o /usr/share/keyrings/dart.gpg
RUN echo 'deb [signed-by=/usr/share/keyrings/dart.gpg arch=amd64] https://storage.googleapis.com/download.dartlang.org/linux/debian stable main' | tee /etc/apt/sources.list.d/dart_stable.list
RUN apt-get update && apt-get install dart
RUN apt-get install -y mycli
# RUN pip install --break-system-packages matplotlib numpy==1.26.4 keras==2.11.0 tensorflow==2.11.0 tensorflowjs==4.0.0
RUN pip install --break-system-packages matplotlib numpy keras tensorflow tensorflowjs
RUN apt-get install -y python3-ipykernel
COPY init-code-server-run /etc/s6-overlay/s6-rc.d/init-code-server/run
COPY svc-code-server-run /etc/s6-overlay/s6-rc.d/svc-code-server/run
RUN echo "export GEM_HOME=\$HOME/.gem" >> /root/.bashrc
WORKDIR /workspace