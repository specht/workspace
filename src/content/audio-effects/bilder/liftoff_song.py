"""A song from NASA's commentary of the launch of Artemis II, 1 April 2026
(public domain: images.nasa.gov, KSC-20260401-AU-LMM01-0001-Artemis_II_Live_
Launch_Coverage_Program_LOS, the commentary channel without the rocket),
for the autotune widget, made the way Songify songs are made: every
syllable cut out on its own and put on the grid of sixteenths, legato
within a phrase; the countdown on the beats as the build, »and liftoff« as
the drop and the hook; a melody that takes, for each syllable, the note of
the scale nearest to the voice plus a line per phrase, so that autotune
moves it only a little; harmonies, »lift« chopped into a riff. A half-time
beat at 95 beats a minute in B minor (Bm G D A), 16 bars, three files of the
same length:

  liftoff.flac          the syllables on the grid (SHEET), the voice's own pitch
  liftoff-melodie.flac  the melody, one note per syllable
  liftoff-band.flac     a real drum kit (audio/drums: DRSKit of DrumGizmo,
                        CC BY 4.0, and a clap, CC0), an 808 bass, a clavinet,
                        brass stabs, a pad, harmonies and chops of the voice

The two excerpts (artemis/four-explorers.flac, 5:35:01.6 to 5:35:15.6 of the
commentary, and artemis/liftoff.flac, 5:35:23.4 to 5:35:44.0) are made from
the original with ffmpeg, channel 1, mono, 44.1 kHz.

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/audio-effects/bilder/liftoff_song.py
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

# the two excerpts, one after the other on one time line: the explorers at
# 0 s, the liftoff at OFFSET s
OFFSET = 20.0
# the words of the commentary: (start, end, syllables), in seconds of that
# time line (Whisper's word times)
E, L = 0.0, OFFSET
WORDS = {
    'rocket-own': [(3.80, 3.92, 1), (3.92, 4.22, 2), (4.22, 4.44, 1), (4.44, 4.56, 1), (4.56, 4.74, 1), (4.74, 5.02, 1)],
    'four-brave': [(5.32, 5.66, 1), (5.66, 6.00, 1), (6.00, 6.74, 3)],
    'ready-ride': [(6.74, 7.14, 2), (7.14, 7.30, 1), (7.30, 7.52, 1), (7.52, 7.70, 1), (7.70, 7.86, 1), (7.86, 8.32, 3), (8.32, 8.80, 2)],
    'nasa-launched': [(11.30, 11.90, 2), (11.90, 12.22, 1), (12.22, 12.70, 2), (12.70, 13.44, 1)],
    'ten': [(L + 0.00, L + 1.08, 1)], 'nine': [(L + 1.54, L + 2.12, 1)], 'eight': [(L + 2.50, L + 3.16, 1)],
    'seven': [(L + 3.56, L + 4.22, 2)],
    'rs25-lit': [(L + 4.48, L + 5.12, 2), (L + 5.12, L + 5.70, 3), (L + 5.70, L + 6.22, 2), (L + 6.22, L + 6.68, 1)],
    'four': [(L + 7.24, L + 8.12, 1)], 'three': [(L + 8.40, L + 8.88, 1)], 'two': [(L + 9.04, L + 9.64, 1)],
    'one': [(L + 10.04, L + 10.64, 1)],
    'booster-ignition': [(L + 10.96, L + 11.40, 2), (L + 11.40, L + 11.84, 3)],
    'and-liftoff': [(L + 12.40, L + 12.94, 1), (L + 12.94, L + 13.26, 1), (L + 13.26, L + 13.66, 1)],
    'liftoff': [(L + 12.94, L + 13.26, 1), (L + 13.26, L + 13.66, 1)],
    'lift': [(L + 12.94, L + 13.26, 1)],
    'crew-moon': [(L + 15.02, L + 15.12, 1), (L + 15.12, L + 15.36, 1), (L + 15.36, L + 15.56, 1), (L + 15.56, L + 15.92, 3),
                  (L + 15.92, L + 16.18, 1), (L + 16.18, L + 16.56, 1), (L + 16.56, L + 16.78, 1), (L + 16.78, L + 17.14, 1),
                  (L + 17.14, L + 17.26, 1), (L + 17.26, L + 17.50, 1)],
    'humanity-begins': [(L + 18.04, L + 18.64, 4), (L + 18.64, L + 18.82, 1), (L + 18.82, L + 19.22, 1), (L + 19.22, L + 19.68, 2),
                        (L + 19.68, L + 20.14, 2)],
}


# per bar its chord, as semitones above C: Bm G D A
CHORDS = [[11, 2, 6], [7, 11, 2], [2, 6, 9], [9, 1, 4]] * 4
# B minor: the notes a syllable may sing
SCALE = [11, 1, 2, 4, 6, 7, 9]
# the sheet: (bar, sixteenth in the bar, piece, sixteenths per syllable)
SHEET = [
    # verse
    (0, 0, 'rocket-own', [1, 2, 1, 1, 1, 1, 5]),
    (1, 0, 'four-brave', [2, 2, 2, 2, 4]),
    (2, 0, 'ready-ride', [1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 3]),
    (3, 0, 'nasa-launched', [2, 2, 2, 2, 2, 6]),
    # build: the countdown on the beats
    (4, 0, 'ten', [4]), (4, 4, 'nine', [4]), (4, 8, 'eight', [4]), (4, 12, 'seven', [2, 2]),
    (5, 0, 'rs25-lit', [1, 1, 2, 1, 1, 2, 2, 6]),
    (6, 0, 'four', [4]), (6, 4, 'three', [4]), (6, 8, 'two', [4]), (6, 12, 'one', [4]),
    (7, 0, 'booster-ignition', [2, 2, 2, 2, 4]),
    # drop and chorus
    (8, 0, 'and-liftoff', [2, 4, 10]),
    (9, 0, 'crew-moon', [1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 4]),
    (10, 0, 'humanity-begins', [1, 1, 1, 1, 2, 2, 2, 1, 1, 4]),
    (11, 0, 'lift', [2]), (11, 2, 'lift', [2]), (11, 4, 'liftoff', [2, 4]), (11, 10, 'liftoff', [2, 4]),
    (12, 0, 'and-liftoff', [2, 4, 10]),
    (13, 0, 'crew-moon', [1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 4]),
    (14, 0, 'humanity-begins', [1, 1, 1, 1, 2, 2, 2, 1, 1, 4]),
    (15, 0, 'liftoff', [4, 12]),
]
SIXTEENTH = BEAT / 4
# the line of each phrase: semitones above his own pitch, per syllable; a
# note may lie at most LIMIT semitones from his voice
CONTOUR = {
    'rocket-own': [0, 3, 0, 2, 0, -2, -3], 'four-brave': [0, 2, 3, 5, 3], 'ready-ride': [0, 2, 0, 3, 2, 3, 5, 3, 2, 0, -2],
    'nasa-launched': [3, 2, 0, 2, 0, -3],
    'ten': [5], 'nine': [3], 'eight': [2], 'seven': [0, -2], 'rs25-lit': [0, 2, 3, 2, 0, 2, 3, 5],
    'four': [0], 'three': [2], 'two': [3], 'one': [5], 'booster-ignition': [3, 5, 5, 7, 7],
    'and-liftoff': [3, 5, 7], 'liftoff': [5, 3], 'lift': [5],
    'crew-moon': [0, 2, 0, 3, 2, 0, 2, 3, 2, 0, -2, -3], 'humanity-begins': [0, 2, 3, 5, 7, 5, 3, 2, 0, -2],
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




def load(path):
    raw = subprocess.run(['ffmpeg', '-loglevel', 'error', '-i', path, '-f', 'f32le', '-ac', '1', '-ar', str(RATE), '-'],
                         check=True, capture_output=True).stdout
    return np.frombuffer(raw, dtype=np.float32).astype(float)


explorers = load(os.path.join(HERE, 'artemis', 'four-explorers.flac'))
liftoff = load(os.path.join(HERE, 'artemis', 'liftoff.flac'))
speech = np.zeros(int((OFFSET + 1) * RATE) + len(liftoff))
speech[:len(explorers)] = explorers
speech[int(OFFSET * RATE):int(OFFSET * RATE) + len(liftoff)] = liftoff
speech /= np.abs(speech).max()
raw = (speech.astype(np.float32)).tobytes()


def clean(x):
    """the recording of 1962 clearer and quieter between the words, gently:
    blocks of 2048 overlapping by three quarters, root-Hann windows (they add
    up without seams); per frequency a Wiener gain from the noise (its
    spectrum from the quietest fifth of the blocks), never below 0,2
    (−14 dB), smoothed over five neighbouring frequencies and over time, so
    that nothing bubbles; then below 90 Hz out, around 300 Hz 3 dB less,
    around 3 kHz 3 dB more"""
    N, H = 2048, 512
    window = np.sqrt(np.hanning(N + 1)[:N])
    starts = range(0, len(x) - N, H)
    spectra = np.array([np.fft.rfft(x[i:i + N] * window) for i in starts])
    power = np.abs(spectra) ** 2
    quiet = np.argsort(power.sum(axis=1))[:max(1, len(spectra) // 5)]
    noise = np.median(power[quiet], axis=0)
    freq = np.fft.rfftfreq(N, 1 / RATE)
    octaves = lambda f0: np.log2(np.maximum(freq, 1) / f0)
    eq = 10 ** ((-3 * np.exp(-(octaves(300) / 0.7) ** 2) + 3 * np.exp(-(octaves(3000) / 0.8) ** 2)) / 20)
    eq /= 1 + (90 / np.maximum(freq, 1)) ** 4
    out = np.zeros(len(x) + N)
    gain = np.ones(len(freq))
    for k, i in enumerate(starts):
        want = np.maximum(1 - 1.2 * noise / np.maximum(power[k], 1e-12), 0.2)
        want = np.convolve(want, np.ones(5) / 5, 'same')
        gain = np.where(want > gain, 0.6 * want + 0.4 * gain, 0.85 * gain + 0.15 * want)
        out[i:i + N] += np.fft.irfft(spectra[k] * gain * eq, N) * window
    out = out[:len(x)] / 1.5
    return out / np.abs(out).max()


def snr(x):
    """how far the quietest tenth of 50 ms blocks lies below the loudest tenth, in dB"""
    n = int(0.05 * RATE)
    levels = np.sort([np.sqrt(np.mean(x[i:i + n] ** 2)) + 1e-9 for i in range(0, len(x) - n, n)])
    k = max(1, len(levels) // 10)
    return 20 * np.log10(np.mean(levels[-k:]) / np.mean(levels[:k]))


before = snr(speech[:len(explorers)])
speech = clean(speech)
print(f'noise below speech: {before:.1f} dB before, {snr(speech[:len(explorers)]):.1f} dB after cleaning')



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
    return int(round(t * RATE))


def voice_end(t0, t1):
    """where the voice of a word stops: the end of the last 10 ms after t0
    and before t1 + 0.15 s that is no more than 24 dB below the word's loudest"""
    step = int(0.01 * RATE)
    a, b = at(t0), at(t1 + 0.15)
    levels = [np.sqrt(np.mean(speech[i:i + step] ** 2)) for i in range(a, b, step)]
    top = max(levels) if levels else 0
    last = max((k for k, v in enumerate(levels) if v > top * 10 ** (-24 / 20)), default=len(levels) - 1)
    return t0 + (last + 1) * 0.01


def pieces():
    """per phrase the start of each syllable and, last, where the voice
    stops: a word of several syllables split evenly (refine() moves each cut
    to a quiet place)"""
    out = {}
    for name, words in WORDS.items():
        times = []
        for t0, t1, n in words:
            times += [t0 + (t1 - t0) * k / n for k in range(n)]
        times.append(voice_end(words[-1][0], words[-1][1]))
        out[name] = times
    return out


PIECES = pieces()


def zero_crossing(i):
    """the sample nearest to i (within 3 ms) where the wave crosses zero"""
    w = int(0.003 * RATE)
    seg = speech[max(0, i - w):i + w]
    z = np.where(np.sign(seg[:-1]) != np.sign(seg[1:]))[0]
    return max(0, i - w) + int(z[np.argmin(np.abs(z - (i - max(0, i - w))))]) if len(z) else i


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
    middle (the vowel) stretched or squeezed, joined with crossfades of
    10 ms so that nothing clicks"""
    if len(piece) * 0.85 <= n <= len(piece) * 1.15:
        out = np.zeros(n)
        out[:min(n, len(piece))] = piece[:n]
        return fade(out, 3, 30) if n < len(piece) else out
    x = int(0.01 * RATE)
    head, tail = min(int(len(piece) * 0.25), n // 3), min(int(len(piece) * 0.2), n // 3)
    middle = stretch(piece[max(0, head - x):len(piece) - tail + x], n - head - tail + 2 * x)
    out = np.zeros(n + 2 * x)
    ramp = np.linspace(0, 1, x)
    out[:head] = piece[:head]
    out[head - x:head] = piece[head - x:head] * (1 - ramp) + middle[:x] * ramp
    out[head:head + len(middle) - 2 * x] = middle[x:len(middle) - x]
    end = head + len(middle) - 2 * x
    tail_piece = piece[len(piece) - tail - x:]
    out[end - x:end] = middle[-x:] * (1 - ramp) + tail_piece[:x] * ramp
    out[end:end + len(tail_piece) - x] = tail_piece[x:]
    return out[:n]


voice = np.zeros(LENGTH + 2 * RATE)
melody = np.zeros(LENGTH + 2 * RATE)
shifts, sung, syllables = [], [], []
for bar, sixteenth, name, lengths in SHEET:
    times = PIECES[name]
    assert len(lengths) == len(times) - 1, name
    bounds = [times[0]] + [refine(t) for t in times[1:-1]] + [times[-1]]
    slot = bar * BAR + sixteenth * SIXTEENTH
    for (t0, t1), units, step in zip(zip(bounds[:-1], bounds[1:]), lengths, CONTOUR[name]):
        piece = speech[zero_crossing(at(t0)):zero_crossing(at(t1))]
        # legato: up to the next syllable and 20 ms into it, longer by up to a
        # quarter rather than squeezed hard into a short slot
        n = int(units * SIXTEENTH * RATE) + int(0.02 * RATE)
        n = max(n, min(len(piece), int(n * 1.25)))
        held = hold(piece, n)
        rms = np.sqrt(np.mean(held ** 2)) + 1e-9
        held *= (0.12 / rms) ** 0.5                    # quiet syllables up, loud ones down, halfway
        place(voice, fade(held, 10, 20), slot)
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


def clav(f, n):
    """a clavinet: a plucked string with many bright harmonics that fade
    fast, the odd ones a little stronger, a hard start"""
    t = np.arange(n) / RATE
    out = sum(np.sin(2 * np.pi * f * k * t) / k ** 0.8 * (1.3 if k % 2 else 0.8) * np.exp(-t * (7 + 4 * k))
              for k in range(1, 16) if f * k < 9000)
    return fade(out, 1, 15)


def brass(f, n):
    """a short brass stab: detuned saws that swell in 15 ms and fall"""
    t = np.arange(n) / RATE
    out = sum(np.sin(2 * np.pi * f * d * k * t) / k for d in (0.995, 1.005) for k in range(1, 10) if f * k < 7000)
    return fade(out * np.minimum(1, t / 0.015) * np.exp(-t * 6), 1, 30)


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
    # clavinet: the chord in syncopated sixteenths, a ghost note between
    CLAV = {0: 0.09, 3: 0.07, 6: 0.09, 7: 0.035, 10: 0.08, 12: 0.04, 14: 0.08}
    for k, level in CLAV.items():
        if stop and k > 8:
            continue
        n = int(SIXTEENTH * (1.6 if level > 0.05 else 0.6) * RATE)
        place(keys, sum(clav(semis_hz(c + 12 if c < 6 else c), n) for c in chord), t0 + k * SIXTEENTH + (0.008 if k % 2 else 0), level)
    # the piano only as a soft bed under it
    place(keys, sum(piano(semis_hz(c) * (2 if c < 6 else 1), int(BAR * RATE)) for c in chord), t0, 0.04)
    # brass stabs in the chorus, on the »and« of 2 and of 4
    if part == 'chorus':
        for k in (6, 14):
            place(keys, sum(brass(semis_hz(c + 12), int(0.18 * RATE)) for c in chord), t0 + k * SIXTEENTH, 0.035)
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
# harmonies in the chorus: the voice a third and a fifth above its melody, exactly on the notes
for slot, held, f, note in syllables:
    if 8 * BAR <= slot < 15 * BAR and f:
        for steps, level, delay in ((2, 0.32, 0.012), (4, 0.22, 0.021)):
            target = semis_hz(scale_step(note, steps))
            place(extras, fade(shift(held, target / f), 5, 30), slot + delay, level)
# vocal chops: »lift« as an instrument on eighths, through the build and the last bar
moon = PIECES['lift']
m0, m1 = int(round(moon[0] * RATE)), int(round(moon[1] * RATE))
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

write('liftoff.flac', voice)
write('liftoff-melodie.flac', melody)
write('liftoff-band.flac', band)
