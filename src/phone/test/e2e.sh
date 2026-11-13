#!/bin/sh
# The Telefonspiel end to end, without a trunk: Asterisk, the speech
# service, the runner and a test telephone (baresip) in a Docker network of
# their own, with one published game (123, the demo Zahlenraten). The
# telephone calls 100, the line's number here: the Telefonbuch's menu
# answers, it types 123 and guesses. Then it calls 100123, the number with
# the game's dialled after it: the game answers at once, without the menu;
# 100999, a number without a game (the tones, »Kein Anschluss«, the menu);
# and 100 without pressing a key (after two rounds of titles, goodbye).
# The test checks the runner's log and each call's line for /statistik
# ($OUT/calls, record_call: the games reached, nothing about the caller),
# and keeps the recordings in $OUT.
#   src/phone/test/e2e.sh            (from the repository; images phone-asterisk,
#                                     phone-speech, phone-runner, phone-testclient built)
set -eu
HERE=$(cd "$(dirname "$0")/.." && pwd)
OUT=${OUT:-/tmp/phone-e2e}
CACHE=${CACHE:-/tmp/phone-e2e-cache}       # speech, kept between runs
NET=phone-e2e
rm -rf "$OUT"; mkdir -p "$OUT/conf" "$OUT/in" "$OUT/out" "$OUT/games/1" "$OUT/calls" "$CACHE"
# the published list as the Workspace writes it (phone_games.rb)
cp "$HERE/games/1/main.py" "$OUT/games/1/"
# a game with a syntax error (124): the caller hears so and gets the menu
mkdir -p "$OUT/games/2"
printf "require 'telefon'\nkey = keys(1)\ncase key\nwhen '1' or '2':\n  say 'eins'\nend\n" > "$OUT/games/2/start.rb"
echo '{"games": [{"number": "123", "title": "Zahlenraten", "folder": "games/1", "file": "main.py"}, {"number": "124", "title": "Kaputt", "folder": "games/2", "file": "start.rb"}]}' > "$OUT/games/published.json"
chmod -R a+rX "$OUT/games"
cleanup() { docker rm -f phone-e2e-asterisk phone-e2e-speech phone-e2e-runner >/dev/null 2>&1 || true; docker network rm $NET >/dev/null 2>&1 || true; }
trap cleanup EXIT
cleanup
docker network create $NET >/dev/null
docker run -d --name phone-e2e-asterisk --network $NET --network-alias asterisk \
    -v "$HERE/asterisk:/etc/asterisk:ro" phone-asterisk >/dev/null
# a new folder for the container; on later runs it belongs to the speech user
chmod 777 "$CACHE" 2>/dev/null || true
docker run -d --name phone-e2e-speech --network $NET --network-alias phone-speech \
    -v "$HERE/speech:/app:ro" -v "$CACHE:/cache" phone-speech >/dev/null
docker run -d --name phone-e2e-runner --network $NET --network-alias runner \
    -e PHONE_NUMBER=100 -e PHONE_DIRECT_DIAL=1 -e PHONE_CALLS=/calls \
    -v "$HERE:/phone:ro" -v "$OUT/games:/games:ro" -v "$OUT/calls:/calls" phone-runner >/dev/null
cp "$HERE/test/baresip-config" "$OUT/conf/config"
echo "<sip:test@asterisk>;auth_pass=test;regint=0" > "$OUT/conf/accounts"
cp "$HERE/test/call.py" "$OUT/call.py"
docker run --rm -v "$OUT:/t" phone-testclient sox -n -r 16000 -c 1 -b 16 /t/in/source.wav synth 60 sine 1 vol 0.0001
for i in $(seq 1 30); do docker logs phone-e2e-runner 2>&1 | grep -q 'connected to Asterisk' && break; sleep 1; done
call() {
    docker run --rm --network $NET -v "$OUT:/t" -v "$OUT/conf:/root/.baresip" -v "$OUT/out:/out" -v "$OUT/in:/in" \
        phone-testclient python3 /t/call.py "$@" >/dev/null 2>&1
}
fail=0
check() {      # check LOG TEXT…: each text in the log
    log=$1; shift
    for expected in "$@"; do
        if grep -q "$expected" "$log"; then echo "ok    $expected"; else echo "MISSING $expected"; fail=1; fi
    done
}
# 1. the bare number: the menu speaks; 1 2 3 cut it short; then the game's
# music alone for 3 s, its question, and three guesses (the game ends when
# one is right, so not every guess is read)
call 100 45 1@5 2@5.6 3@6.2 5@20 3@26 7@32
docker logs phone-e2e-runner > "$OUT/runner.log" 2>&1
check "$OUT/runner.log" "say 'Hier ist das Telefonbuch" "game 1" "say 'Ich denke mir eine Zahl" "keys '5'" "game ended: ended 0"
# 2. the number with the game's after it: the game, no menu
lines=$(wc -l < "$OUT/runner.log")
call 100123 25 4@14
docker logs phone-e2e-runner 2>&1 | tail -n +$((lines + 1)) > "$OUT/runner-direct.log"
check "$OUT/runner-direct.log" "game 1" "say 'Ich denke mir eine Zahl"
if grep -q "say 'Hier ist das Telefonbuch" "$OUT/runner-direct.log"; then echo "WRONG the menu answered a call dialled with the game's number"; fail=1; else echo "ok    no menu when dialled directly"; fi
# 3. a number without a game after the line's: the three tones, »Kein
# Anschluss«, then the menu
lines=$(docker logs phone-e2e-runner 2>&1 | wc -l)
call 100999 16
docker logs phone-e2e-runner 2>&1 | tail -n +$((lines + 1)) > "$OUT/runner-none.log"
check "$OUT/runner-none.log" "play kein-anschluss.wav" "say 'Kein Anschluss unter dieser Nummer" "say 'Hier ist das Telefonbuch"
# 4. the bare number and no key: after two rounds of titles, goodbye
lines=$(docker logs phone-e2e-runner 2>&1 | wc -l)
call 100 75
docker logs phone-e2e-runner 2>&1 | tail -n +$((lines + 1)) > "$OUT/runner-quiet.log"
check "$OUT/runner-quiet.log" "say 'Ruf gern wieder an" "game ended: hangup"
# 5. a game with a syntax error, dialled directly: the caller hears that it
# has an error and gets the Telefonbuch's menu instead of silence
lines=$(docker logs phone-e2e-runner 2>&1 | wc -l)
call 100124 25
docker logs phone-e2e-runner 2>&1 | tail -n +$((lines + 1)) > "$OUT/runner-crash.log"
check "$OUT/runner-crash.log" "game 2" "syntax error" "say 'Dieses Spiel hat gerade einen Fehler" "say 'Hier ist das Telefonbuch"
# each call one line for /statistik: the first two reached game 123, the
# next two none, the last 124; only hour, line, seconds and games (and the
# error of the game that crashed), never the caller
sleep 2
if [ "$(cat "$OUT/calls/telefon.jsonl" 2>/dev/null | wc -l)" = 5 ]; then echo "ok    five calls recorded"; else echo "WRONG calls recorded: $(cat "$OUT/calls/telefon.jsonl" 2>/dev/null | wc -l)"; fail=1; fi
games=$(sed -n 's/.*"games": \(\[[^]]*\]\).*/\1/p' "$OUT/calls/telefon.jsonl" 2>/dev/null | tr '\n' ' ')
if [ "$games" = '["123"] ["123"] [] [] ["124"] ' ]; then echo "ok    games of the calls: $games"; else echo "WRONG games of the calls: $games"; fail=1; fi
if head -4 "$OUT/calls/telefon.jsonl" | grep -qv '^{"hour": [0-9]*, "line": "telefon", "seconds": [0-9]*, "games": \[[^]]*\]}$'; then echo "WRONG a call's line holds more"; fail=1; else echo "ok    nothing but hour, line, seconds, games"; fi
if tail -1 "$OUT/calls/telefon.jsonl" | grep -q '"errors": \[{"game": "124", "text": ".*syntax error'; then echo "ok    the crash's error recorded for 124"; else echo "WRONG no error recorded for 124"; fail=1; fi
heard=$(ls "$OUT"/out/*-dec.wav | head -1)
seconds=$(docker run --rm -v "$OUT:/t" phone-testclient sh -c "sox /t/out/$(basename "$heard") -n stat 2>&1 | sed -n 's/^Length (seconds): *//p'")
echo "heard ${seconds} s: $heard"
exit $fail
