# The Git server of the groups (docs/plan-git-gruppen.md, GitServer in
# src/ruby/include/git_server.rb): sshd for one user, git, whose every key
# runs git-gate. The repositories are in /srv/git (data/git-repos), the
# keys and the access list the backend writes in /etc/git-server
# (data/git-server/config, read-only here), the host keys in /keys.
FROM debian:bookworm-slim@sha256:99c0b25c46d0214d5a2091092e7908907dcaecd6a962cde0ac700acee222b6f3

RUN apt-get update \
    && apt-get install -y --no-install-recommends openssh-server git \
    && rm -rf /var/lib/apt/lists/* \
    && rm -f /etc/ssh/ssh_host_* /etc/update-motd.d/* /etc/motd \
    && mkdir -p /run/sshd \
    && useradd --uid 1000 --home-dir /srv/git --no-create-home --shell /bin/sh git \
    && usermod -p '*' git

ENV LANG=C.UTF-8 TZ=Europe/Berlin

COPY sshd_config /etc/ssh/sshd_config
COPY git-gate /usr/local/bin/git-gate
COPY entrypoint.sh /usr/local/sbin/git-server
RUN chmod 0755 /usr/local/bin/git-gate /usr/local/sbin/git-server

EXPOSE 22
ENTRYPOINT ["/usr/local/sbin/git-server"]
