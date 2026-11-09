#!/usr/bin/env bash
# How fast the voice of the Telefonspiel (Piper, thorsten-high) speaks on
# this machine: in containers of phone_speech's image with 2 and 4
# processors and 1, 2 and 4 threads per worker, beside the running service.
# Prints, per setting, the seconds per second of speech (below 1: faster
# than speaking); phone_speech's :cpus in config.rb and PHONE_SPEECH_THREADS
# follow from it. About a minute, up to 4 processors at normal priority.
#
# Run from the repository, on the server as on a development machine:
#
#   src/scripts/measure-speech.sh
#
# (Thorsten's CosyVoice was measured too and left out: its image takes
# 19.4 GB and its models 9.7 GB more, docs/plan-telefonspiel.md.)
set -euo pipefail

SPEECH="${PHONE_SPEECH_CONTAINER:-workspace_phone_speech_1}"
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

    # in containers of phone_speech's image, beside the running service: how
    # many processors and threads per worker make the live voice fastest
    piper_image="$(docker inspect -f '{{.Config.Image}}' "$SPEECH")"
    echo "== Piper (thorsten-high), Image ${piper_image}; der Dienst hat jetzt $(docker inspect -f '{{.HostConfig.NanoCpus}}' "$SPEECH" | awk '{printf "%g", $1 / 1e9}') Prozessoren =="
    for cpus in 2 4; do
        [[ "$cpus" -gt "$cores" ]] && continue
        printf '%s\n' "${SENTENCES[@]}" | docker run --rm -i --cpus "$cpus" -v "$PWD/src/phone/speech:/app:ro" \
            --entrypoint timeout "$piper_image" 900 python3 -c '
import sys, time, onnxruntime
sys.path.insert(0, "/app")
import server
from piper import PiperVoice
cpus = sys.argv[1]
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
    print(f"  {cpus} Prozessoren, {threads} Thread(s): {took:6.1f} s für {audio:5.1f} s Sprache, {took / audio:5.2f} s je Sekunde Sprache", flush=True)
' "$cpus"
    done
    echo
