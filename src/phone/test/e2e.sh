#!/bin/sh
# The Telefonspiel end to end, without a trunk: Asterisk, the runner and a
# test telephone (baresip) in a Docker network of their own. The telephone
# calls 100, chooses game 1 (Zahlenraten) and guesses; the test checks the
# runner's log and keeps the recording in $OUT.
#   src/phone/test/e2e.sh            (from the repository; images phone-asterisk,
#                                     phone-runner, phone-testclient built)
set -eu
HERE=$(cd "$(dirname "$0")/.." && pwd)
OUT=${OUT:-/tmp/phone-e2e}
NET=phone-e2e
rm -rf "$OUT"; mkdir -p "$OUT/conf" "$OUT/in" "$OUT/out" "$OUT/cache"
cleanup() { docker rm -f phone-e2e-asterisk phone-e2e-runner >/dev/null 2>&1 || true; docker network rm $NET >/dev/null 2>&1 || true; }
trap cleanup EXIT
cleanup
docker network create $NET >/dev/null
docker run -d --name phone-e2e-asterisk --network $NET --network-alias asterisk \
    -v "$HERE/asterisk:/etc/asterisk:ro" phone-asterisk >/dev/null
docker run -d --name phone-e2e-runner --network $NET --network-alias runner \
    -v "$HERE/runner:/app:ro" -v "$HERE/games:/games:ro" -v "$OUT/cache:/cache" phone-runner >/dev/null
cp "$HERE/test/baresip-config" "$OUT/conf/config"
echo "<sip:test@asterisk>;auth_pass=test;regint=0" > "$OUT/conf/accounts"
cp "$HERE/test/call.py" "$OUT/call.py"
docker run --rm -v "$OUT:/t" phone-testclient sox -n -r 16000 -c 1 -b 16 /t/in/source.wav synth 60 sine 1 vol 0.0001
for i in $(seq 1 30); do docker logs phone-e2e-runner 2>&1 | grep -q 'connected to Asterisk' && break; sleep 1; done
# the menu speaks for about 5 s; then 1; then three guesses, 6 s apart
docker run --rm --network $NET -v "$OUT:/t" -v "$OUT/conf:/root/.baresip" -v "$OUT/out:/out" -v "$OUT/in:/in" \
    phone-testclient python3 /t/call.py 100 40 1@9 5@18 3@24 7@30 >/dev/null 2>&1
docker logs phone-e2e-runner > "$OUT/runner.log" 2>&1
fail=0
for expected in "say 'Hallo und willkommen" "keys '1'" "say 'Ich denke mir eine Zahl" "keys '5'" "keys '3'" "keys '7'" "game ended with 0"; do
    if grep -q "$expected" "$OUT/runner.log"; then echo "ok    $expected"; else echo "MISSING $expected"; fail=1; fi
done
heard=$(ls "$OUT"/out/*-dec.wav | head -1)
seconds=$(docker run --rm -v "$OUT:/t" phone-testclient sh -c "sox /t/out/$(basename "$heard") -n stat 2>&1 | sed -n 's/^Length (seconds): *//p'")
echo "heard ${seconds} s: $heard"
exit $fail
