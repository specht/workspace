FROM linuxserver/code-server:4.89.1
RUN sudo apt-get update && sudo apt-get install -y \
    wget \
    ruby \
    python3 python3-pip \
    golang-go \
    openjdk-17-jdk-headless \
    lua5.4 \
    nodejs npm \
    texlive-full \
    htop \
    build-essential file \
    && sudo apt-get clean \
    && sudo rm -rf /var/lib/apt/lists/*
RUN wget https://storage.googleapis.com/dart-archive/channels/stable/release/latest/linux_packages/dart_3.4.3-1_amd64.deb
RUN sudo dpkg -i dart_3.4.3-1_amd64.deb
RUN rm dart_3.4.3-1_amd64.deb
COPY init-code-server-run /etc/s6-overlay/s6-rc.d/init-code-server/run
COPY svc-code-server-run /etc/s6-overlay/s6-rc.d/svc-code-server/run
WORKDIR /workspace