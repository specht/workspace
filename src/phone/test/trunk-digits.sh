#!/bin/sh
# Does the provider pass on digits dialled after the number (»Ruf 030 …
# 123 an«, docs/plan-telefonspiel.md, »Durchwahl«)? Asterisk registers at the
# trunk of credentials.rb (PHONE_SIP_SERVER, PHONE_SIP_USER,
# PHONE_SIP_PASSWORD), with nothing else on it: no test telephone, no ARI.
# Every call is answered with a short beep every three seconds and held for
# a minute, so that several phones can call at once; the log line »DIGITS«
# says what the provider delivered (the dialled number in the To header and
# in P-Called-Party-ID) and how many calls are open at that moment, which
# shows how many the trunk lets through (a call beyond its limit is busy and
# never arrives). The caller's number is not logged.
#   src/phone/test/trunk-digits.sh [minutes] [seconds a call is held]   (default 10 and 60; image phone-asterisk built)
# Runs with the host's network (SIP behind the router's NAT, kept open by the
# registration), on port 5060/udp, and stops by itself.
set -eu
REPO=$(cd "$(dirname "$0")/../../.." && pwd)
MINUTES=${1:-10}
HOLD=${2:-60}
NAME=phone-trunk-digits
values=$(cd "$REPO" && ruby -e 'require "./src/ruby/credentials.rb"; puts [PHONE_SIP_SERVER, PHONE_SIP_USER, PHONE_SIP_PASSWORD].join("\n")') || {
    echo 'PHONE_SIP_SERVER, PHONE_SIP_USER and PHONE_SIP_PASSWORD are needed in src/ruby/credentials.rb'; exit 1; }
SERVER=$(echo "$values" | sed -n 1p)
USER=$(echo "$values" | sed -n 2p)
PASSWORD=$(echo "$values" | sed -n 3p)
CONF=$(mktemp -d)
cleanup() { docker rm -f $NAME >/dev/null 2>&1 || true; rm -rf "$CONF"; }
trap cleanup EXIT INT TERM
cleanup; CONF=$(mktemp -d)
cp "$REPO/src/phone/asterisk/asterisk.conf" "$REPO/src/phone/asterisk/logger.conf" "$REPO/src/phone/asterisk/modules.conf" "$REPO/src/phone/asterisk/rtp.conf" "$CONF/"
printf '[general]\nenabled = no\n' > "$CONF/http.conf"
printf '[general]\nenabled = no\n' > "$CONF/ari.conf"
cat > "$CONF/pjsip.conf" <<CONF
[transport-udp]
type = transport
protocol = udp
bind = 0.0.0.0:5060

[trunk]
type = registration
transport = transport-udp
outbound_auth = trunk
server_uri = sip:$SERVER
client_uri = sip:$USER@$SERVER
contact_user = $USER
retry_interval = 30
expiration = 120
line = yes
endpoint = trunk

[trunk]
type = auth
auth_type = userpass
username = $USER
password = $PASSWORD

[trunk]
type = aor
contact = sip:$SERVER
qualify_frequency = 30

[trunk]
type = endpoint
context = from-trunk
disallow = all
allow = g722,alaw,ulaw
outbound_auth = trunk
aors = trunk
from_user = $USER
from_domain = $SERVER
direct_media = no
rtp_symmetric = yes
force_rport = yes
rewrite_contact = yes
dtmf_mode = rfc4733

[trunk]
type = identify
endpoint = trunk
match = $SERVER
CONF
cat > "$CONF/extensions.conf" <<CONF
[globals]
HOLD = $HOLD

[from-trunk]
exten => _.,1,Set(GROUP()=calls)
 same => n,Verbose(0,DIGITS calls=\${GROUP_COUNT(calls)} exten=\${EXTEN} to=\${PJSIP_HEADER(read,To)} called=\${PJSIP_HEADER(read,P-Called-Party-ID)} ruri=\${CHANNEL(pjsip,request_uri)})
 same => n,Answer()
 same => n,Playtones(440/300,0/2700)
 same => n,Wait(\${HOLD})
 same => n,Hangup()
exten => s,1,Goto(from-trunk,0,1)
CONF
chmod 755 "$CONF"; chmod 644 "$CONF"/*
docker run -d --name $NAME --network host -v "$CONF:/etc/asterisk:ro" phone-asterisk:latest >/dev/null
for i in $(seq 1 20); do
    sleep 3
    state=$(docker exec $NAME asterisk -rx 'pjsip show registrations' 2>/dev/null | grep -o 'Registered\|Rejected\|Unregistered\|Failed' | head -1 || true)
    [ "$state" = Registered ] && break
done
echo "registration: ${state:-unknown}"
[ "$state" = Registered ] || { docker logs $NAME 2>&1 | grep -i -E 'regist|auth|error' | tail -5; exit 1; }
echo "listening for $MINUTES minutes: call the number with digits appended"
end=$(( $(date +%s) + MINUTES * 60 ))
seen=0
while [ "$(date +%s)" -lt "$end" ]; do
    sleep 5
    lines=$(docker logs $NAME 2>&1 | grep 'DIGITS' || true)
    count=$(printf '%s' "$lines" | grep -c DIGITS || true)
    if [ "$count" -gt "$seen" ]; then printf '%s\n' "$lines" | tail -n $((count - seen)); seen=$count; fi
done
echo "done: $seen calls"
