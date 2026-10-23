#!/bin/sh
# Starts one host of the case server: ACCOUNTS accounts e000 … e999, then
# sshd. On the museum server each has the password CaseServer.password
# computes (src/ruby/include/case_server.rb), which a person finds in its
# case files; on the alarm system all have the maintenance password from
# /etc/case-alarm-password, which lies in the maintenance papers on the
# museum server (make_case.rb). Host keys stay in /keys, so their fingerprints survive a rebuild.
set -eu
: "${ROLE:?ROLE must be museum or alarmanlage}"
: "${CASE_SERVER_SECRET:?CASE_SERVER_SECRET is missing}"
ACCOUNTS=1000
alarm_password=
if [ "$ROLE" = alarmanlage ]; then
    alarm_password=$(cat /etc/case-alarm-password)
fi

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
    if [ -n "$alarm_password" ]; then
        password=$alarm_password
    else
        password=$(hmac "password:$login" | cut -c1-12)
    fi
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

# On the alarm system someone else is logged in: wartung, from the old
# laptop of Elektro Reuter (10.8.0.23, geraete.txt on the museum server),
# trying to delete the shell history that convicts him. who shows the
# session, ps the two processes. His rm waits for its »yes« forever, and
# could not delete the file anyway: it belongs to root, and so does the
# directory. Nothing changes for anyone.
if [ "$ROLE" = alarmanlage ]; then
    echo 'wartung:x:2500:2500:Wartung:/home/wartung:/bin/bash' >> /etc/passwd
    echo 'wartung:!:20000:0:99999:7:::' >> /etc/shadow
    echo 'wartung:x:2500:' >> /etc/group
    as_wartung='setpriv --reuid=2500 --regid=2500 --clear-groups --reset-env'
    # His login shell, idle: a bash that reads from a FIFO nobody writes to,
    # opened read and write so it never ends. Its output goes to rm, which
    # waits for an answer that never comes.
    cd /home/wartung
    # a restart keeps the container's /run: the FIFO of the last start is still there
    rm -f /run/wartung-tty
    mkfifo -m 0600 /run/wartung-tty
    chown 2500:2500 /run/wartung-tty
    # Unquoted on purpose: the words of the command, and no shell in between
    # that would stay in ps
    $as_wartung bash -c 'exec -a -bash bash <> /run/wartung-tty' | $as_wartung rm -i /home/wartung/.bash_history 2>/dev/null &
    sleep 1
    shell=$(pgrep -u 2500 -x bash | head -1)
    started=$(date -u -d '-7 minutes' '+%Y-%m-%dT%H:%M:%S,000000+00:00')
    mkdir -p /var/run
    printf '[7] [%05d] [ts/1] [wartung ] [pts/1       ] [10.8.0.23           ] [10.8.0.23      ] [%s]\n' "$shell" "$started" \
        | utmpdump -r > /var/run/utmp 2>/dev/null
    chmod 0644 /var/run/utmp
    cd /
fi

echo "case server $ROLE: $ACCOUNTS accounts ready"
exec /usr/sbin/sshd -D -e
