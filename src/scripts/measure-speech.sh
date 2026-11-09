#!/usr/bin/env bash
# How fast the voices of the Telefonspiel speak on this machine
# (docs/plan-telefonspiel.md, »Two voices«): Piper in the running
# phone_speech, with 1, 2 and 4 threads per worker under its limit of two
# processors, and Thorsten's CosyVoice in a container of its own with 2, 4
# and all processors at the lowest priority (--cpu-shares 2): it gets only
# the time nothing else wants, so Workspaces and calls do not notice it.
# Prints, per setting, the seconds per second of speech (below 1: faster
# than speaking) and the memory CosyVoice needed.
#
# Run from the repository, on the server as on a development machine:
#
#   src/scripts/measure-speech.sh            # both voices
#   src/scripts/measure-speech.sh piper      # only Piper
#   src/scripts/measure-speech.sh cosyvoice  # only CosyVoice
#
# The first run of CosyVoice downloads its models (about 8.5 GB) into the
# Docker volume hs_cosyvoice_models, which the service uses later; that
# takes a while once. Nothing else is kept: the measuring container goes
# when the script ends, also on Ctrl+C.
set -euo pipefail

WHAT="${1:-all}"
SPEECH="${PHONE_SPEECH_CONTAINER:-workspace_phone_speech_1}"
IMAGE='thorstenvoice/cosyvoice-tts:cosyvoice3'
MEASURE='hs-cosyvoice-measure'
VOLUME='hs_cosyvoice_models'
MEMORY='10g'
SENTENCES=(
    'Hallo!'
    'Du stehst vor einer dunklen Höhle.'
    'Drinnen teilt sich der Gang. Links flackert Licht, rechts rauscht Wasser.'
    'Im Licht einer Fackel glänzt eine Kiste voller Gold. Du hast den Schatz gefunden!'
    'Haben Sie das Formular 27 B Strich 6 ausgefüllt, in dreifacher Ausfertigung? Drücken Sie die 1 für ja, die 2 für welches Formular.'
)

cores="$(nproc)"
echo "Rechner: ${cores} Kerne, $(free -g | awk '/^Mem/ {print $2 " GB Arbeitsspeicher, " $7 " GB frei"}'), Last $(cut -d' ' -f1-3 /proc/loadavg)"
echo

if [[ "$WHAT" == all || "$WHAT" == piper ]]; then
    echo "== Piper (thorsten-high) in ${SPEECH}, begrenzt auf $(docker inspect -f '{{.HostConfig.NanoCpus}}' "$SPEECH" | awk '{printf "%g", $1 / 1e9}') Prozessoren =="
    printf '%s\n' "${SENTENCES[@]}" | docker exec -i "$SPEECH" timeout 900 python3 -c '
import sys, time, onnxruntime
sys.path.insert(0, "/app")
import server
from piper import PiperVoice
texts = [l.strip() for l in sys.stdin if l.strip()]
voice = PiperVoice.load(server.VOICE)
for threads in (1, 2, 4):
    options = onnxruntime.SessionOptions()
    options.intra_op_num_threads = threads
    options.inter_op_num_threads = 1
    voice.session = onnxruntime.InferenceSession(server.VOICE, sess_options=options, providers=["CPUExecutionProvider"])
    voice.phoneme_ids_to_audio(voice.phonemes_to_ids(server.phonemes_of(voice, "Warm.")))
    started, audio = time.monotonic(), 0.0
    for text in texts:
        for sentence in server.sentences(text):
            audio += len(voice.phoneme_ids_to_audio(voice.phonemes_to_ids(server.phonemes_of(voice, sentence)))) / voice.config.sample_rate
    took = time.monotonic() - started
    print(f"  {threads} Thread(s): {took:6.1f} s für {audio:5.1f} s Sprache, {took / audio:5.2f} s je Sekunde Sprache")
'
    echo
fi

if [[ "$WHAT" == all || "$WHAT" == cosyvoice ]]; then
    echo "== CosyVoice (Thorsten, ${IMAGE}), niedrigste Priorität, höchstens ${MEMORY} Speicher =="
    docker pull -q "$IMAGE" >/dev/null
    cleanup() { docker rm -f "$MEASURE" >/dev/null 2>&1 || true; }
    trap cleanup EXIT
    settings=(2 4 "$cores")
    [[ "$cores" -le 4 ]] && settings=(2 "$cores")
    for cpus in $(printf '%s\n' "${settings[@]}" | sort -nu); do
        cleanup
        docker run -d --name "$MEASURE" --cpus "$cpus" --cpu-shares 2 --memory "$MEMORY" --memory-swap "$MEMORY" \
            -v "$VOLUME":/app/CosyVoice/pretrained_models "$IMAGE" >/dev/null
        # ready when /health answers; the first start downloads the models
        for _ in $(seq 1 720); do
            docker exec "$MEASURE" python3 -c 'import urllib.request; urllib.request.urlopen("http://127.0.0.1:8000/health", timeout=5)' >/dev/null 2>&1 && break
            if ! docker ps -q -f name="^${MEASURE}$" | grep -q .; then
                echo "  Der Container ist beendet:"; docker logs --tail 20 "$MEASURE" 2>&1 | sed 's/^/    /'; exit 1
            fi
            sleep 5
        done
        peak=0
        ( while docker ps -q -f name="^${MEASURE}$" | grep -q .; do
              docker stats --no-stream --format '{{.MemUsage}}' "$MEASURE" 2>/dev/null | awk '{print $1}'
              sleep 2
          done ) > "/tmp/${MEASURE}.mem" &
        watcher=$!
        printf '%s\n' "${SENTENCES[@]}" | docker exec -i "$MEASURE" timeout 3000 python3 -c '
import sys, time, io, wave, urllib.request, urllib.parse
texts = [l.strip() for l in sys.stdin if l.strip()]
def tts(text):
    body = urllib.parse.urlencode({"text": text}).encode()
    with urllib.request.urlopen(urllib.request.Request("http://127.0.0.1:8000/tts", data=body), timeout=1200) as r:
        data = r.read()
    with wave.open(io.BytesIO(data)) as w:
        return w.getnframes() / w.getframerate()
tts("Warm.")
started, audio = time.monotonic(), 0.0
for text in texts:
    t = time.monotonic(); seconds = tts(text); audio += seconds
    print(f"    {time.monotonic() - t:6.1f} s für {seconds:4.1f} s: {text[:50]}", flush=True)
took = time.monotonic() - started
print(f"  zusammen {took:.1f} s für {audio:.1f} s Sprache, {took / audio:.2f} s je Sekunde Sprache", flush=True)
' | sed "s/^  zusammen/  ${cpus} Prozessoren: zusammen/"
        kill "$watcher" 2>/dev/null || true
        echo "  höchster Speicher: $(sort -h "/tmp/${MEASURE}.mem" | tail -1)"
        rm -f "/tmp/${MEASURE}.mem"
        echo
    done
fi
