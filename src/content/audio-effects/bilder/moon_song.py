"""A short song from Kennedy's speech at Rice University, 12 September 1962
(the recording on Wikimedia Commons, public domain:
File:Jfk_rice_university_we_choose_to_go_to_the_moon.ogg), for the
autotune widget, in three files of the same length, 8 bars at 120 beats a
minute, Am F C G twice:

  kennedy.wav           his phrases, cut where Whisper put the words and
                        where the voice stops: three times »We choose to go
                        to the Moon«, »in this decade«, »the Moon« twice as
                        an echo, then twice »not because they are easy, but
                        because they are hard«
  kennedy-melodie.wav   a melody on his words, one note per syllable from
                        Whisper's word times, in his own range, so that
                        autotune moves him only a few semitones
  kennedy-band.wav      kick on every beat, clap on 2 and 4, an open hi-hat
                        between, bass in eighths (root and octave), short
                        chords on the offbeats

    mkdir -p /tmp/jfk && curl -sL -o /tmp/jfk/jfk.ogg \\
        https://commons.wikimedia.org/wiki/Special:FilePath/Jfk_rice_university_we_choose_to_go_to_the_moon.ogg
    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -v /tmp/jfk:/jfk -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/audio-effects/bilder/moon_song.py /jfk/jfk.ogg
"""

import os
import subprocess
import sys
import wave

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
AUDIO = os.path.join(HERE, '..', '..', '..', 'static', 'audio')
RATE = 44100
BPM = 120
BEAT = 60 / BPM
BAR = 4 * BEAT

# the phrases: the start of each syllable in seconds of the recording
# (Whisper's word times; a word of two syllables split by ear), and where
# the voice stops (its loudness and how well a block fits itself one
# period later, every 40 ms), so that »Moon« keeps its »n«
PHRASES = {
    'moon1': [517.88, 518.04, 518.40, 518.62, 518.78, 518.92, 519.02, 519.46],
    'moon2': [520.45, 520.70, 521.02, 521.26, 521.42, 521.54, 521.64, 522.10],
    'moon3': [528.00, 528.60, 529.00, 529.34, 529.54, 529.70, 529.82, 530.12],
    'decade': [530.12, 530.38, 530.62, 530.80, 531.00],
    'the-moon': [518.92, 519.02, 519.46],
    'easy': [532.62, 533.00, 533.23, 533.46, 533.80, 533.96, 534.15, 534.27],
    'hard': [534.27, 534.74, 534.95, 535.16, 535.46, 535.60, 536.05],
}
CHORDS = [('Am', [9, 0, 4]), ('F', [5, 9, 0]), ('C', [0, 4, 7]), ('G', [7, 11, 2])] * 2
# (bar, beat, phrase, a note per syllable); notes above C3, + one octave up
SONG = [
    (0, 0, 'moon1', 'E G G A G E A'),
    (1, 0, 'moon2', 'F A A C+ A F A'),
    (2, 0, 'moon3', 'G E G C+ B G E'),
    (3, 0, 'decade', 'D G B G'),
    (3, 2, 'the-moon', 'B D+'),
    (3, 3, 'the-moon', 'B G'),
    (4, 0, 'easy', 'E A A C+ B A E'),
    (5, 0, 'hard', 'C+ C+ A A G F'),
    (6, 0, 'easy', 'G G E G C+ B G'),
    (7, 0, 'hard', 'D D B G A G'),
]
STEP = {'C': 0, 'D': 2, 'E': 4, 'F': 5, 'G': 7, 'A': 9, 'B': 11}
C2, C3 = 65.41, 130.81
LENGTH = int(round(len(CHORDS) * BAR * RATE))


def note_hz(name):
    return C3 * 2 ** ((STEP[name[0]] + (12 if name.endswith('+') else 0)) / 12)


def write(name, z):
    with wave.open(os.path.join(AUDIO, name), 'wb') as f:
        f.setnchannels(1)
        f.setsampwidth(2)
        f.setframerate(RATE)
        f.writeframes((np.clip(z, -1, 1) * 32767).astype('<i2').tobytes())
    print(name, f'{len(z) / RATE:.2f} s, peak {np.abs(z).max():.2f}')


def fade(z, ms, out_ms=None):
    r = min(len(z) // 2, int(ms / 1000 * RATE))
    q = min(len(z) // 2, int((out_ms or ms) / 1000 * RATE))
    z = z.copy()
    z[:r] *= np.linspace(0, 1, r)
    z[-q:] *= np.linspace(1, 0, q)
    return z


def tone(f, n, overtones=(1, 0.35, 0.15)):
    t = np.arange(n) / RATE
    return sum(a * np.sin(2 * np.pi * f * (k + 1) * t) for k, a in enumerate(overtones))


raw = subprocess.run(['ffmpeg', '-loglevel', 'error', '-i', sys.argv[1], '-ss', '515', '-t', '23',
                      '-f', 'f32le', '-ac', '1', '-ar', str(RATE), '-'], check=True, capture_output=True).stdout
speech = np.frombuffer(raw, dtype=np.float32).astype(float)
speech /= np.abs(speech).max()

voice = np.zeros(LENGTH + RATE)
melody = np.zeros(LENGTH + RATE)
for bar, beat, phrase, notes in SONG:
    times = PHRASES[phrase]
    notes = notes.split()
    assert len(notes) == len(times) - 1, (phrase, notes)
    start = bar * BAR + beat * BEAT
    a = times[0] - 0.05
    piece = fade(speech[int((a - 515) * RATE):int((times[-1] - 515) * RATE)], 10, 40)
    at = int(start * RATE)
    voice[at:at + len(piece)] += piece
    for t0, t1, name in zip(times[:-1], times[1:], notes):
        n0 = int((start + t0 - a) * RATE)
        length = int((t1 - t0) * RATE)
        melody[n0:n0 + length] += 0.3 * fade(tone(note_hz(name), length), 12)
voice = voice[:LENGTH] * 0.85 / np.abs(voice).max()
melody = melody[:LENGTH]
print('melody:', ' | '.join(n for _, _, _, n in SONG))

band = np.zeros(LENGTH + RATE)
rng = np.random.default_rng(1962)
t = np.arange(int(0.25 * RATE)) / RATE
kick = np.sin(2 * np.pi * np.cumsum(48 + 110 * np.exp(-t * 35)) / RATE) * np.exp(-t * 14)
t = np.arange(int(0.2 * RATE)) / RATE
noise = rng.uniform(-1, 1, len(t))
clap = (noise - np.concatenate([[0], noise[:-1]])) * (np.exp(-t * 28) + 0.6 * np.exp(-np.abs(t - 0.012) * 400))
n = int(0.09 * RATE)
hat = np.diff(rng.uniform(-1, 1, n + 1)) * np.exp(-np.arange(n) / RATE * 35)
for bar, (_, chord) in enumerate(CHORDS):
    for eighth in range(8):
        at = int((bar * BAR + eighth * BEAT / 2) * RATE)
        if eighth % 2 == 0:
            band[at:at + len(kick)] += 0.8 * kick
        else:
            band[at:at + n] += 0.13 * hat
            stab = sum(tone(note_hz('CDEFGAB'[[0, 2, 4, 5, 7, 9, 11].index(c)]) * 2, int(0.12 * RATE), (1, 0.3, 0.1)) for c in chord)
            band[at:at + len(stab)] += 0.05 * fade(stab, 5, 60)
        if eighth in (2, 6):
            band[at:at + len(clap)] += 0.35 * clap
        root = C2 * 2 ** (chord[0] / 12) * (2 if eighth % 2 else 1)
        bass = fade(tone(root, int(BEAT / 2 * 0.85 * RATE), (1, 0.6, 0.3, 0.15)), 5, 30)
        band[at:at + len(bass)] += 0.22 * bass
band = band[:LENGTH]
band *= 0.8 / np.abs(band).max()

write('kennedy.wav', voice)
write('kennedy-melodie.wav', melody)
write('kennedy-band.wav', band)
