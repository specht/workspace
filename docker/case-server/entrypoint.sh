#!/bin/sh
# Starts one host of the case server: ACCOUNTS accounts e000 … e999 with the
# passwords CaseServer.password computes (src/ruby/include/case_server.rb),
# then sshd. Host keys stay in /keys, so their fingerprints survive a rebuild.
set -eu
: "${ROLE:?ROLE must be museum or alarmanlage}"
: "${CASE_SERVER_SECRET:?CASE_SERVER_SECRET is missing}"
ACCOUNTS=1000

hmac() {
    printf '%s' "$1" | openssl dgst -sha256 -hmac "$CASE_SERVER_SECRET" | sed 's/^.*= //'
}

for type in ed25519 rsa; do
    [ -f "/keys/ssh_host_${type}_key" ] || ssh-keygen -q -t "$type" -N '' -f "/keys/ssh_host_${type}_key"
done

case "$ROLE" in
    museum) home=/srv/museum ;;
    alarmanlage) home=/srv/alarmanlage ;;
    *) echo "unknown ROLE $ROLE" >&2; exit 1 ;;
esac

# The accounts, written afresh from the image's files on every start
cp /etc/passwd.image /etc/passwd
cp /etc/shadow.image /etc/shadow
cp /etc/group.image /etc/group
echo 'ermittlung:x:2000:' >> /etc/group
i=0
while [ "$i" -lt "$ACCOUNTS" ]; do
    login=$(printf 'e%03d' "$i")
    password=$(hmac "password:$login" | cut -c1-12)
    salt=$(hmac "salt:$login" | cut -c1-16)
    echo "$login:x:$((3000 + i)):2000:Ermittlung:$home:/bin/bash" >> /etc/passwd
    echo "$login:$(openssl passwd -6 -salt "$salt" "$password"):20000:0:99999:7:::" >> /etc/shadow
    i=$((i + 1))
done

# Only /tmp is writable, and nobody sees what others put there
chmod 1733 /tmp

if [ "$ROLE" = museum ]; then
    # The one way on: ssh -J through here to the alarm system, nowhere else
    forwarding='AllowTcpForwarding local
PermitOpen alarmanlage.bitstein:22'
else
    forwarding='DisableForwarding yes'
fi

cat > /etc/ssh/sshd_config <<CONFIG
Port 22
HostKey /keys/ssh_host_ed25519_key
HostKey /keys/ssh_host_rsa_key
PermitRootLogin no
AllowGroups ermittlung
PasswordAuthentication yes
KbdInteractiveAuthentication no
UsePAM yes
PrintMotd no
X11Forwarding no
AllowAgentForwarding no
PermitTunnel no
GatewayPorts no
PermitUserEnvironment no
$forwarding
MaxAuthTries 4
MaxSessions 4
LoginGraceTime 30
MaxStartups 30:50:100
ClientAliveInterval 60
ClientAliveCountMax 3
AcceptEnv LANG
Subsystem sftp internal-sftp
CONFIG

# Inside the museum's network the alarm system is known; no question about
# its fingerprint on the second hop.
cat > /etc/ssh/ssh_config.d/case.conf <<CONFIG
Host alarmanlage.bitstein
    StrictHostKeyChecking no
    UserKnownHostsFile /dev/null
    LogLevel ERROR
CONFIG

echo "case server $ROLE: $ACCOUNTS accounts ready"
exec /usr/sbin/sshd -D -e
