"""A song from Kennedy's speech at Rice University, 12 September 1962 (the
recording on Wikimedia Commons, public domain:
File:Jfk_rice_university_we_choose_to_go_to_the_moon.ogg), for the
autotune widget, made the way Songify songs are made: every syllable cut
out on its own and put on the grid of sixteenths like a rapped line, held
where its note is long (only its vowel stretched), silence between; the
hook »We choose to go to the Moon« with its »Moon« held and »the Moon« as
a stutter; a melody that takes, for each syllable, the note of the scale
nearest to his own voice plus a line per phrase, so that autotune moves
him only a little. A modern beat in half time: 95 beats a minute, B minor
(Bm G D A), 16 bars, in three files of the same length:

  kennedy.flac          his syllables on the grid (SHEET), his own pitch
  kennedy-melodie.flac  the melody, one note per syllable
  kennedy-band.flac     a real drum kit (audio/drums: DRSKit of DrumGizmo,
                        CC BY 4.0, and a clap, CC0) as a half-time beat with
                        hi-hat rolls, an 808 bass that slides into each
                        note, dark piano chords, a pad in the chorus that
                        ducks under the kick

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
BPM = 95
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
    'hard-word': [535.60, 536.05],
}
# per bar its chord, as semitones above C: Bm G D A
CHORDS = [[11, 2, 6], [7, 11, 2], [2, 6, 9], [9, 1, 4]] * 4
# B minor: the notes a syllable may sing
SCALE = [11, 1, 2, 4, 6, 7, 9]
# the sheet: (bar, sixteenth in the bar, piece, sixteenths per syllable)
SHEET = [
    (0, 0, 'moon1', [1, 2, 1, 2, 1, 1, 4]),
    (1, 0, 'moon2', [1, 2, 1, 2, 1, 1, 4]),
    (2, 0, 'moon3', [1, 2, 1, 2, 1, 1, 4]),
    (3, 0, 'decade', [1, 1, 2, 4]),
    (3, 9, 'the-moon', [1, 3]),
    (4, 0, 'easy', [2, 1, 1, 1, 1, 2, 4]),
    (5, 0, 'hard', [2, 1, 1, 1, 1, 6]),
    (6, 0, 'easy', [2, 1, 1, 1, 1, 2, 4]),
    (7, 0, 'because-hard', [1, 1, 1, 1, 4]),
    (7, 8, 'hard-word', [2]),
    (7, 10, 'hard-word', [2]),
    (8, 0, 'we-choose1', [1, 2, 1, 2, 1, 1]),
    (8, 8, 'moon-word1', [8]),
    (9, 0, 'the-moon', [1, 3]),
    (9, 4, 'the-moon', [1, 3]),
    (9, 8, 'the-moon', [1, 7]),
    (10, 0, 'we-choose2', [1, 2, 1, 2, 1, 1]),
    (10, 8, 'moon-word2', [8]),
    (11, 0, 'easy', [2, 1, 1, 1, 1, 2, 4]),
    (12, 0, 'hard', [2, 1, 1, 1, 1, 9]),
    (13, 0, 'we-choose1', [1, 2, 1, 2, 1, 1]),
    (13, 8, 'moon-word1', [8]),
    (14, 0, 'the-moon', [1, 3]),
    (14, 4, 'the-moon', [1, 3]),
    (14, 8, 'hard-word', [4]),
]
SIXTEENTH = BEAT / 4
# the line of each phrase: semitones above his own pitch, per syllable; a
# note may lie at most LIMIT semitones from his voice
CONTOUR = {
    'moon1': [0, 3, 0, 5, 3, 0, -2], 'moon2': [0, 3, 0, 5, 3, 0, 5], 'moon3': [0, 3, 0, 5, 7, 5, 0],
    'decade': [0, 3, 5, -2], 'the-moon': [3, 0],
    'easy': [0, -3, 0, 3, 0, -3, -5], 'hard': [5, 5, 3, 0, -2, -5], 'because-hard': [0, 3, 5, 3, 0],
    'hard-word': [-3],
    'we-choose1': [0, 3, 0, 5, 7, 5], 'we-choose2': [0, 3, 0, 5, 3, 0], 'moon-word1': [3], 'moon-word2': [-2],
}
LIMIT = 6
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


def clean(x):
    """the recording of 1962 made easier to understand, in blocks of 2048
    with half overlap: the noise (its spectrum from the quietest tenth of the
    blocks) taken away gently, never below a tenth of what was there; below
    90 Hz out, around 300 Hz 3 dB less, around 3 kHz 4 dB more"""
    N, H = 2048, 1024
    window = np.hanning(N)
    frames = [x[i:i + N] * window for i in range(0, len(x) - N, H)]
    spectra = [np.fft.rfft(f) for f in frames]
    power = np.array([np.sum(np.abs(s) ** 2) for s in spectra])
    quiet = np.argsort(power)[:max(1, len(power) // 10)]
    noise = np.mean([np.abs(spectra[i]) for i in quiet], axis=0)
    freq = np.fft.rfftfreq(N, 1 / RATE)
    octaves = lambda f0: np.log2(np.maximum(freq, 1) / f0)
    eq_db = -3 * np.exp(-(octaves(300) / 0.7) ** 2) + 4 * np.exp(-(octaves(3000) / 0.8) ** 2)
    eq = 10 ** (eq_db / 20) * (1 / (1 + (90 / np.maximum(freq, 1)) ** 4))
    out = np.zeros(len(x) + N)
    for k, spec in enumerate(spectra):
        size = np.abs(spec)
        keep = np.maximum(1 - 1.5 * noise / np.maximum(size, 1e-9), 0.1)
        out[k * H:k * H + N] += np.fft.irfft(spec * keep * eq) * window
    out = out[:len(x)] / 1.5
    return out / np.abs(out).max()


def snr(x):
    """how far the quietest tenth of 50 ms blocks lies below the loudest tenth, in dB"""
    n = int(0.05 * RATE)
    levels = np.sort([np.sqrt(np.mean(x[i:i + n] ** 2)) + 1e-9 for i in range(0, len(x) - n, n)])
    k = max(1, len(levels) // 10)
    return 20 * np.log10(np.mean(levels[-k:]) / np.mean(levels[:k]))


before = snr(speech)
speech = clean(speech)
print(f'noise below speech: {before:.1f} dB before, {snr(speech):.1f} dB after cleaning')



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
shifts, sung, syllables = [], [], []
for bar, sixteenth, name, lengths in SHEET:
    times = PIECES[name]
    assert len(lengths) == len(times) - 1, name
    bounds = [times[0]] + [refine(t) for t in times[1:-1]] + [times[-1]]
    slot = bar * BAR + sixteenth * SIXTEENTH
    for (t0, t1), units, step in zip(zip(bounds[:-1], bounds[1:]), lengths, CONTOUR[name]):
        piece = speech[at(t0):at(t1)]
        n = int(units * SIXTEENTH * (0.92 if units > 2 else 0.98) * RATE)
        held = hold(piece, n)
        rms = np.sqrt(np.mean(held ** 2)) + 1e-9
        held *= (0.12 / rms) ** 0.5                    # quiet syllables up, loud ones down, halfway
        place(voice, fade(held, 5, 25), slot)
        # the note: of the scale, nearest to his own pitch, a chord tone on long ones
        f = own_pitch(piece)
        chord = CHORDS[min(BARS - 1, int(slot / BAR))]
        allowed = chord if units >= 4 else SCALE
        mine = 12 * np.log2(f / C3) if f else 12 * np.log2(240 / C3)
        near = [o * 12 + c for o in range(-1, 3) for c in allowed if abs(o * 12 + c - mine) <= LIMIT] or \
               [o * 12 + c for o in range(-1, 3) for c in SCALE if abs(o * 12 + c - mine) <= LIMIT]
        note = min(near, key=lambda k: abs(k - (mine + step)))
        if f:
            shifts.append(note - mine)
        sung.append(note)
        syllables.append((slot, held, f, note))
        place(melody, 0.3 * fade(tone(semis_hz(note), n), 12, 30), slot)
        slot += units * SIXTEENTH
voice = voice[:LENGTH] * 0.85 / np.abs(voice).max()
melody = melody[:LENGTH]
names = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B']
print('melody:', ' '.join(names[n % 12] for n in sung))
print('shift per syllable: median %.1f, largest %.1f semitones' % (np.median(np.abs(shifts)), np.max(np.abs(shifts))))


# the band ----------------------------------------------------------------
def sample(name):
    with wave.open(os.path.join(DRUMS, name + '.wav'), 'rb') as f:
        return np.frombuffer(f.readframes(f.getnframes()), '<i2') / 32768


def piano(f, n):
    """a dark piano note: partials a little sharper than whole multiples,
    the higher ones fading faster, a soft attack"""
    t = np.arange(n) / RATE
    out = sum(np.sin(2 * np.pi * f * k * np.sqrt(1 + 0.0004 * k * k) * t) / k ** 1.3 * np.exp(-t * (0.8 + 0.9 * k))
              for k in range(1, 9) if f * k < 6000)
    return fade(out * np.minimum(1, t / 0.006), 1, 40)


def bass_808(f, n):
    """an 808: a sine that slides down into its note in 60 ms, held long,
    a little saturated so that it is heard on small speakers too"""
    t = np.arange(n) / RATE
    freq = f * (1 + 0.9 * np.exp(-t / 0.02))
    out = np.sin(2 * np.pi * np.cumsum(freq) / RATE) * np.exp(-t * 1.1)
    return fade(np.tanh(2.2 * out) / np.tanh(2.2), 2, 60)


def pad(chord, n):
    """the chord held softly: detuned saws with few harmonics, slowly in"""
    t = np.arange(n) / RATE
    out = np.zeros(n)
    for c in chord:
        for detune in (0.996, 1.004):
            out += sum(np.sin(2 * np.pi * semis_hz(c) * 2 * detune * k * t) / k ** 1.4 for k in range(1, 7))
    return fade(out * np.minimum(1, t / 0.5), 1, 120)


kick, snare, clap = sample('kick'), sample('snare'), sample('clap')
hat, open_hat, rim = sample('hat-closed'), sample('hat-open'), sample('rim')
crash = sample('crash')
open_short = open_hat[:int(0.25 * RATE)] * np.linspace(1, 0, int(0.25 * RATE))
drums = np.zeros(LENGTH + 5 * RATE)
keys = np.zeros(LENGTH + 3 * RATE)
low = np.zeros(LENGTH + 3 * RATE)
pads = np.zeros(LENGTH + 3 * RATE)
duck = np.ones(LENGTH + 3 * RATE)                # the pad ducks under every kick
rng = np.random.default_rng(1962)
KICKS = {'verse': (0, 10), 'build': (0, 7, 10), 'chorus': (0, 3, 7, 10, 14)}
for bar, chord in enumerate(CHORDS):
    t0 = bar * BAR
    part = 'verse' if bar < 4 else 'build' if bar < 8 else 'chorus' if bar < 15 else 'outro'
    stop = bar == 7
    root = semis_hz(chord[0], C2 / 2)            # B0 … A1: the 808's octave
    for k in KICKS.get(part, (0,)):
        if stop and k > 8:
            continue
        place(drums, kick, t0 + k * SIXTEENTH, 0.95)
        d = int(0.25 * RATE)
        at = int((t0 + k * SIXTEENTH) * RATE)
        duck[at:at + d] = np.minimum(duck[at:at + d], 0.35 + 0.65 * np.linspace(0, 1, d) ** 2)
    if not (stop and False):
        place(drums, snare, t0 + 8 * SIXTEENTH, 0.75)
        if part != 'verse':
            place(drums, clap, t0 + 8 * SIXTEENTH, 0.45)
    # hi-hats: eighths in the verse, sixteenths later, a roll of 32nds at the end of every fourth bar
    for k in range(16):
        if stop and k >= 12:
            break
        if part == 'verse' and k % 2:
            continue
        accent = 0.26 if k % 4 == 0 else 0.17 if k % 2 == 0 else 0.11
        place(drums, hat, t0 + k * SIXTEENTH, accent)
        if bar % 4 == 3 and k in (13, 14, 15) and part != 'verse':
            place(drums, hat, t0 + (k + 0.5) * SIXTEENTH, 0.13)
    if part in ('chorus', 'build'):
        place(drums, open_short, t0 + 14 * SIXTEENTH, 0.25)
    if part == 'chorus':
        place(drums, rim, t0 + 13 * SIXTEENTH, 0.5)
    # 808: on the kicks, held to the next
    hits = [k for k in KICKS.get(part, (0,)) if not (stop and k > 8)]
    for a, b in zip(hits, hits[1:] + [16]):
        place(low, bass_808(root, int((b - a) * SIXTEENTH * RATE)), t0 + a * SIXTEENTH, 0.2)
    # piano: the chord on 1, struck again on the »and« of 2
    for k, level in ((0, 0.16), (6, 0.11)):
        if stop and k > 8:
            continue
        n = int((BAR - k * SIXTEENTH) * RATE)
        place(keys, sum(piano(semis_hz(c) * (2 if c < 6 else 1), n) for c in chord), t0 + k * SIXTEENTH, level)
    if part == 'chorus':
        place(pads, pad(chord, int(BAR * RATE)), t0, 0.03)
# a rise of noise through the build's last bar
n = int(3 * BEAT * RATE)
noise = rng.uniform(-1, 1, n)
noise = noise - np.concatenate([[0], noise[:-1]])
place(drums, noise * np.linspace(0, 1, n) ** 2, 7 * BAR, 0.1)



def shift(x, factor):
    """x factor-times as high, as long as it is: grains of 40 ms, each read
    factor-times as fast and moved by up to one period to fit (shift() of
    the page, with a constant factor)"""
    G = int(0.04 * RATE)
    D = RATE // 60
    window = np.hanning(G)
    offsets = np.arange(-G // 2 - D, G - G // 2 + D)
    padded = np.concatenate([np.zeros(G * 3), x, np.zeros(G * 3)])
    y = np.zeros(len(padded))
    weight = np.zeros(len(padded))
    for c in range(3 * G, 3 * G + len(x), G // 4):
        pos = np.clip(c + offsets * factor, 0, len(padded) - 1)
        piece = np.interp(pos, np.arange(len(padded)), padded)
        here = y[c - G // 2:c - G // 2 + G]
        k = int(np.argmax(np.correlate(piece, here, 'valid'))) if here.any() else D
        y[c - G // 2:c - G // 2 + G] += window * piece[k:k + G]
        weight[c - G // 2:c - G // 2 + G] += window
    return (y / np.maximum(weight, 1e-3))[3 * G:3 * G + len(x)]


def scale_step(note, steps):
    """the note steps notes of the scale higher"""
    ladder = sorted(o * 12 + c for o in range(-1, 4) for c in SCALE)
    k = min(range(len(ladder)), key=lambda i: abs(ladder[i] - note))
    return ladder[min(len(ladder) - 1, k + steps)]


extras = np.zeros(LENGTH + 3 * RATE)
# harmonies in the chorus: Kennedy a third and a fifth above his melody, exactly on the notes
for slot, held, f, note in syllables:
    if 8 * BAR <= slot < 15 * BAR and f:
        for steps, level, delay in ((2, 0.32, 0.012), (4, 0.22, 0.021)):
            target = semis_hz(scale_step(note, steps))
            place(extras, fade(shift(held, target / f), 5, 30), slot + delay, level)
# vocal chops: »Moon« as an instrument on eighths, through the build and the last bar
moon = PIECES['moon-word1']
m0, m1 = int(round((moon[0] - 515) * RATE)), int(round((moon[1] - 515) * RATE))
chop = speech[m0:m0 + int(0.22 * RATE)]
chop_f = own_pitch(speech[m0:m1]) or 240
RIFF = [0, 2, 4, 2, 5, 4, 2, 1]                       # steps of the scale above the chord's root
for bar in list(range(4, 7)) + [15]:
    root = CHORDS[bar][0] + 12
    for k, steps in enumerate(RIFF):
        target = semis_hz(scale_step(root, steps))
        place(extras, fade(shift(chop, target / chop_f), 3, 40), bar * BAR + k * 2 * SIXTEENTH, 0.3)
# a lead of detuned saws doubling the melody an octave up in the chorus
for slot, held, f, note in syllables:
    if 8 * BAR <= slot < 15 * BAR:
        n = len(held)
        t = np.arange(n) / RATE
        f0 = semis_hz(note) * 2
        saw = sum(np.sin(2 * np.pi * f0 * d * k * t) / k for d in (0.993, 1.0, 1.007) for k in range(1, 8) if f0 * k < 9000)
        place(extras, fade(saw * np.exp(-t * 1.5), 5, 40), slot, 0.025)
# ear candy: a crash played backwards into the chorus, an impact on its first beat, a shaker in the chorus
place(drums, crash[::-1][-int(1.5 * RATE):], 8 * BAR - 1.5, 0.3)
t = np.arange(int(0.6 * RATE)) / RATE
place(drums, np.sin(2 * np.pi * np.cumsum(40 + 80 * np.exp(-t * 20)) / RATE) * np.exp(-t * 5), 8 * BAR, 0.6)
n = int(0.03 * RATE)
for bar in range(8, 15):
    for k in range(16):
        shaker = np.diff(rng.uniform(-1, 1, n + 1)) * np.exp(-np.arange(n) / RATE * 120)
        place(drums, shaker, bar * BAR + k * SIXTEENTH + (0.012 if k % 2 else 0), 0.06 if k % 2 else 0.1)

band = drums[:LENGTH] + keys[:LENGTH] + low[:LENGTH] + (pads * duck)[:LENGTH] + extras[:LENGTH]
# room for the voice: 1 to 4 kHz 4 dB quieter in the band
spec = np.fft.rfft(band)
freq = np.fft.rfftfreq(len(band), 1 / RATE)
spec *= 10 ** (-4 * np.exp(-(np.log2(np.maximum(freq, 1) / 2000) / 0.9) ** 2) / 20)
band = np.fft.irfft(spec, len(band))
band *= 0.8 / np.abs(band).max()
print('band: drums %.3f keys %.3f 808 %.3f pad %.3f extras %.3f' % tuple(float(np.sqrt(np.mean(z[:LENGTH] ** 2))) for z in (drums, keys, low, pads, extras)))

write('kennedy.flac', voice)
write('kennedy-melodie.flac', melody)
write('kennedy-band.flac', band)
