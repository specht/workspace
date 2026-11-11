"""A song from Kennedy's speech at Rice University, 12 September 1962 (the
recording on Wikimedia Commons, public domain:
File:Jfk_rice_university_we_choose_to_go_to_the_moon.ogg), for the
autotune widget, made the way Songify songs are made: every syllable cut
out on its own and put on the grid of eighths, held as long as its note
(only its vowel stretched), silence between; the hook »We choose to go to
the Moon« with its »Moon« held; and a melody that takes, for each
syllable, the note of the scale nearest to his own voice, so that autotune
moves him only a little. 16 bars at 120 beats a minute, D A Bm G four
times, in three files of the same length:

  kennedy.flac          his syllables on the grid (SHEET), his own pitch
  kennedy-melodie.flac  the melody, one note per syllable
  kennedy-band.flac     a real drum kit (audio/drums: DRSKit of DrumGizmo,
                        CC BY 4.0, and a clap, CC0), a plucked bass, an
                        electric piano, a pad in the chorus; a rise of noise
                        and a short stop before the chorus

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
DRUMS = os.path.join(AUDIO, 'drums')
RATE = 44100
BPM = 120
BEAT = 60 / BPM
BAR = 4 * BEAT

# the pieces: the start of each syllable in seconds of the recording
# (Whisper's word times, a word of two syllables split by ear) and, last,
# where the voice stops (its loudness and how well a block fits itself one
# period later, measured every 40 ms), so that »Moon« keeps its »n«
PIECES = {
    'moon1': [517.88, 518.04, 518.40, 518.62, 518.78, 518.92, 519.02, 519.46],
    'moon2': [520.45, 520.70, 521.02, 521.26, 521.42, 521.54, 521.64, 522.10],
    'moon3': [528.00, 528.60, 529.00, 529.34, 529.54, 529.70, 529.82, 530.12],
    'we-choose1': [517.88, 518.04, 518.40, 518.62, 518.78, 518.92, 519.02],
    'we-choose2': [520.45, 520.70, 521.02, 521.26, 521.42, 521.54, 521.64],
    'moon-word1': [519.02, 519.46],
    'moon-word2': [521.64, 522.10],
    'decade': [530.12, 530.38, 530.62, 530.80, 531.00],
    'the-moon': [518.92, 519.02, 519.46],
    'easy': [532.62, 533.00, 533.23, 533.46, 533.80, 533.96, 534.15, 534.27],
    'hard': [534.27, 534.74, 534.95, 535.16, 535.46, 535.60, 536.05],
    'because-hard': [534.74, 534.95, 535.16, 535.46, 535.60, 536.05],
}
# per bar its chord, as semitones above C: D A Bm G
CHORDS = [[2, 6, 9], [9, 1, 4], [11, 2, 6], [7, 11, 2]] * 4
# D major: the notes a syllable may sing
SCALE = [2, 4, 6, 7, 9, 11, 1]
# the sheet: (bar, eighth in the bar, piece, eighths per syllable)
SHEET = [
    (0, 0, 'moon1', [1, 1, 1, 1, 1, 1, 2]),
    (1, 0, 'moon2', [1, 1, 1, 1, 1, 1, 2]),
    (2, 0, 'moon3', [1, 1, 1, 1, 1, 1, 2]),
    (3, 0, 'decade', [1, 1, 1, 3]),
    (3, 6, 'the-moon', [1, 1]),
    (4, 0, 'easy', [1, 1, 1, 1, 1, 1, 2]),
    (5, 0, 'hard', [1, 1, 1, 1, 1, 3]),
    (6, 0, 'easy', [1, 1, 1, 1, 1, 1, 2]),
    (7, 0, 'because-hard', [1, 1, 1, 1, 2]),
    (8, 0, 'we-choose1', [1, 1, 1, 1, 1, 1]),
    (8, 6, 'moon-word1', [10]),
    (10, 0, 'we-choose2', [1, 1, 1, 1, 1, 1]),
    (10, 6, 'moon-word2', [6]),
    (11, 4, 'the-moon', [1, 1]),
    (11, 6, 'the-moon', [1, 1]),
    (12, 0, 'easy', [1, 1, 1, 1, 1, 1, 2]),
    (13, 0, 'hard', [1, 1, 1, 1, 1, 3]),
    (14, 0, 'we-choose1', [1, 1, 1, 1, 1, 1]),
    (14, 6, 'moon-word1', [10]),
]
EIGHTH = BEAT / 2
# the line of each phrase: semitones above his own pitch, per syllable; a
# note may lie at most LIMIT semitones from his voice
CONTOUR = {
    'moon1': [0, 2, 2, 4, 2, 0, -1], 'moon2': [0, 2, 2, 4, 2, 0, 2], 'moon3': [0, 2, 2, 4, 5, 4, 2],
    'decade': [0, 2, 4, 2], 'the-moon': [2, 0],
    'easy': [0, -2, 0, 2, 0, -2, -3], 'hard': [3, 3, 2, 0, -1, -3], 'because-hard': [0, 2, 3, 2, 0],
    'we-choose1': [0, 2, 2, 4, 5, 4], 'we-choose2': [0, 2, 2, 4, 2, 0], 'moon-word1': [2], 'moon-word2': [-1],
}
LIMIT = 4
STEP = {'C': 0, 'D': 2, 'E': 4, 'F': 5, 'G': 7, 'A': 9, 'B': 11}
C2, C3 = 65.41, 130.81
BARS = len(CHORDS)
LENGTH = int(round(BARS * BAR * RATE))


def note_hz(name):
    return C3 * 2 ** ((STEP[name[0]] + (12 if name.endswith('+') else 0)) / 12)


def semis_hz(semis, base=C3):
    return base * 2 ** (semis / 12)


def write(name, z):
    """a FLAC file through ffmpeg"""
    data = (np.clip(z, -1, 1) * 32767).astype('<i2').tobytes()
    subprocess.run(['ffmpeg', '-loglevel', 'error', '-y', '-f', 's16le', '-ar', str(RATE), '-ac', '1', '-i', '-',
                    os.path.join(AUDIO, name)], input=data, check=True)
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


def stretch(piece, n):
    """piece made n samples long with grains of 50 ms, each moved by up to
    one period to fit what is already there"""
    G = int(0.05 * RATE)
    D = RATE // 60
    window = np.hanning(G)
    y = np.zeros(n + G)
    weight = np.zeros(n + G)
    padded = np.concatenate([np.zeros(G + D), piece, np.zeros(G + D)])
    for c in range(0, n, G // 4):
        source = G + D + int(c * len(piece) / n)
        part = padded[source - G // 2 - D:source - G // 2 + G + D]
        here = y[c:c + G]
        k = int(np.argmax(np.correlate(part, here, 'valid'))) if here.any() else D
        y[c:c + G] += window * part[k:k + G]
        weight[c:c + G] += window
    return y[G // 2:G // 2 + n] / np.maximum(weight[G // 2:G // 2 + n], 1e-3)


def place(track, sound, seconds, level=1.0):
    at = int(round(seconds * RATE))
    if 0 <= at < len(track):
        track[at:at + len(sound)] += level * sound[:len(track) - at]


raw = subprocess.run(['ffmpeg', '-loglevel', 'error', '-i', sys.argv[1], '-ss', '515', '-t', '23',
                      '-f', 'f32le', '-ac', '1', '-ar', str(RATE), '-'], check=True, capture_output=True).stdout
speech = np.frombuffer(raw, dtype=np.float32).astype(float)
speech /= np.abs(speech).max()



def pitch(block):
    best, best_lag, dipped = 0, 0, False
    for lag in range(1, RATE // 60):
        r = np.dot(block[:-lag], block[lag:])
        if r < 0:
            dipped = True
        elif dipped and r > best:
            best, best_lag = r, lag
    return RATE / best_lag if best_lag else 0


def at(t):
    return int(round((t - 515) * RATE))


def refine(t):
    """a boundary between syllables moved to the quietest 10 ms within 40 ms"""
    step = int(0.005 * RATE)
    best = min(range(-8, 9), key=lambda k: np.sqrt(np.mean(speech[at(t) + k * step:at(t) + k * step + 2 * step] ** 2)))
    return t + best * 0.005


def own_pitch(piece):
    """the median pitch of the voiced blocks of a syllable, 0 if none"""
    found = []
    for s in range(0, len(piece) - 2048, 512):
        block = piece[s:s + 2048]
        if np.sqrt(np.mean(block ** 2)) > 0.03:
            f = pitch(block)
            lag = round(RATE / f) if f else 0
            if 70 < f < 500 and 0 < lag < 2048 and np.dot(block[:-lag], block[lag:]) > 0.5 * np.dot(block, block):
                found.append(f)
    return float(np.median(found)) if found else 0


def hold(piece, n):
    """a syllable n samples long: the start and the end as spoken, only the
    middle (the vowel) stretched or squeezed"""
    if n <= len(piece) * 1.15 and n >= len(piece) * 0.85:
        out = np.zeros(n)
        out[:min(n, len(piece))] = piece[:n]
        return out
    head, tail = int(len(piece) * 0.25), int(len(piece) * 0.2)
    head, tail = min(head, n // 3), min(tail, n // 3)
    middle = stretch(piece[head:len(piece) - tail], n - head - tail)
    return np.concatenate([piece[:head], middle, piece[len(piece) - tail:]])


voice = np.zeros(LENGTH + 2 * RATE)
melody = np.zeros(LENGTH + 2 * RATE)
shifts, sung = [], []
for bar, eighth, name, lengths in SHEET:
    times = PIECES[name]
    assert len(lengths) == len(times) - 1, name
    bounds = [times[0]] + [refine(t) for t in times[1:-1]] + [times[-1]]
    slot = bar * BAR + eighth * EIGHTH
    for (t0, t1), eighths, step in zip(zip(bounds[:-1], bounds[1:]), lengths, CONTOUR[name]):
        piece = speech[at(t0):at(t1)]
        n = int(eighths * EIGHTH * (0.92 if eighths > 1 else 0.98) * RATE)
        place(voice, fade(hold(piece, n), 5, 25), slot)
        # the note: of the scale, nearest to his own pitch, a chord tone on long ones
        f = own_pitch(piece)
        chord = CHORDS[min(BARS - 1, int(slot / BAR))]
        allowed = chord if eighths >= 2 else SCALE
        mine = 12 * np.log2(f / C3) if f else 12 * np.log2(237 / C3)
        near = [o * 12 + c for o in range(-1, 3) for c in allowed if abs(o * 12 + c - mine) <= LIMIT] or \
               [o * 12 + c for o in range(-1, 3) for c in SCALE if abs(o * 12 + c - mine) <= LIMIT]
        note = min(near, key=lambda k: abs(k - (mine + step)))
        if f:
            shifts.append(note - mine)
        sung.append(note)
        place(melody, 0.3 * fade(tone(semis_hz(note), n), 12, 30), slot)
        slot += eighths * EIGHTH
voice = voice[:LENGTH] * 0.85 / np.abs(voice).max()
melody = melody[:LENGTH]
names = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B']
print('melody:', ' '.join(names[n % 12] for n in sung))
print('shift per syllable: median %.1f, largest %.1f semitones' % (np.median(np.abs(shifts)), np.max(np.abs(shifts))))


# the band ----------------------------------------------------------------
def sample(name):
    with wave.open(os.path.join(DRUMS, name + '.wav'), 'rb') as f:
        return np.frombuffer(f.readframes(f.getnframes()), '<i2') / 32768


def electric_piano(f, n):
    """a Rhodes-like note: a sine whose brightness (frequency modulation)
    and loudness fade"""
    t = np.arange(n) / RATE
    index = 1.8 * np.exp(-t * 6) + 0.15
    return np.sin(2 * np.pi * f * t + index * np.sin(2 * np.pi * f * t)) * np.exp(-t * 2.2)


def bass_note(f, n):
    """a plucked bass: the harmonics of a saw wave, the higher ones fading
    faster, as a filter closing after each note"""
    t = np.arange(n) / RATE
    out = sum(np.sin(2 * np.pi * f * k * t) / k * np.exp(-t * (3 + 9 * k)) for k in range(1, 12) if f * k < 5000)
    return fade(out * np.minimum(1, t / 0.004), 1, 15)


def pad(chord, n):
    """the chord held softly: detuned saws with few harmonics, slowly in"""
    t = np.arange(n) / RATE
    out = np.zeros(n)
    for c in chord:
        for detune in (0.997, 1.003):
            out += sum(np.sin(2 * np.pi * semis_hz(c) * 2 * detune * k * t) / k ** 1.5 for k in range(1, 6))
    return fade(out * np.minimum(1, t / 0.3), 1, 80)


kick, snare, clap = sample('kick'), sample('snare'), sample('clap')
hat, open_hat, crash = sample('hat-closed'), sample('hat-open'), sample('crash')
open_short = open_hat[:int(0.2 * RATE)] * np.linspace(1, 0, int(0.2 * RATE))
drums = np.zeros(LENGTH + 5 * RATE)
keys = np.zeros(LENGTH + 2 * RATE)
low = np.zeros(LENGTH + 2 * RATE)
rng = np.random.default_rng(1962)
for bar, chord in enumerate(CHORDS):
    t0 = bar * BAR
    part = 'verse' if bar < 4 else 'build' if bar < 8 else 'chorus'
    root = semis_hz(chord[0], C2)
    if bar in (0, 8, 12):
        place(drums, crash, t0, 0.35)
    for step in range(16):                     # sixteenths
        t = t0 + step * BEAT / 4
        beat, sub = divmod(step, 4)
        stop = bar == 7 and beat == 3            # a short stop before the chorus
        if stop:
            continue
        if sub == 0 and (part == 'chorus' or beat in (0, 2)) or (part != 'chorus' and step == 10):
            place(drums, kick, t, 0.9)
        if sub == 0 and beat in (1, 3):
            place(drums, snare, t, 0.7)
            if part == 'chorus':
                place(drums, clap, t, 0.35)
        if part == 'chorus':
            if sub == 2:
                place(drums, open_short, t, 0.3)
            elif sub == 0:
                place(drums, hat, t, 0.25)
        elif part == 'build' or sub % 2 == 0:
            place(drums, hat, t, 0.28 if sub == 0 else 0.15)
        if bar == 7 and beat == 2:              # the fill into the stop
            place(drums, snare, t, 0.3 + 0.1 * sub)
    if bar == 7:
        continue                                # bass and keys rest in the last bar of the build
    pattern = ((0, 1, 0.75), (1.5, 1, 0.4), (2, 2, 0.4), (2.5, 1, 0.4), (3.5, 2, 0.4)) if part != 'verse' else ((0, 1, 1.5), (2, 1, 0.75), (3, 2, 0.4))
    for beats, octave, length in pattern:
        place(low, bass_note(root * octave, int(length * BEAT * RATE)), t0 + beats * BEAT, 0.5)
    for beats in (0.5, 1.5, 2.5, 3.5):
        n = int(0.9 * BEAT * RATE)
        notes = sum(electric_piano(semis_hz(c) * 2, n) for c in chord)
        place(keys, notes, t0 + beats * BEAT, 0.07 if part == 'verse' else 0.09)
    if part == 'chorus':
        place(keys, pad(chord, int(BAR * RATE)), t0, 0.022)
# the bass of the last build bar on its first beat only, then the stop
place(low, bass_note(semis_hz(CHORDS[7][0], C2), int(1.5 * BEAT * RATE)), 7 * BAR, 0.5)
# a rise of noise through the build's last bar
n = int(3 * BEAT * RATE)
noise = rng.uniform(-1, 1, n)
noise = noise - np.concatenate([[0], noise[:-1]])
place(drums, noise * np.linspace(0, 1, n) ** 2, 7 * BAR, 0.12)

band = drums[:LENGTH] + keys[:LENGTH] + low[:LENGTH]
band *= 0.8 / np.abs(band).max()

write('kennedy.flac', voice)
write('kennedy-melodie.flac', melody)
write('kennedy-band.flac', band)
