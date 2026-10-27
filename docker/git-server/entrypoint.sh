#!/bin/sh
# Starts the Git server: a host key that survives a rebuild (in /keys, so its
# fingerprint stays the same), then sshd in the foreground.
set -eu
[ -f /keys/ssh_host_ed25519_key ] || ssh-keygen -q -t ed25519 -N '' -C git.workspace -f /keys/ssh_host_ed25519_key
mkdir -p /srv/git
chown git:git /srv/git
exec /usr/sbin/sshd -D -e
