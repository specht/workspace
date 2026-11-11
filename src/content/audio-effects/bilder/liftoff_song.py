"""A song from NASA's commentary of the launch of Artemis II, 1 April 2026
(public domain: images.nasa.gov, KSC-20260401-AU-LMM01-0001-Artemis_II_Live_
Launch_Coverage_Program_LOS, the commentary channel without the rocket),
for the autotune widget: the commentary as it was said over the verse, the
build and »and liftoff« on the drop, sung only in the chorus (»The crew of
Artemis twoooo / now bound for the Moooon«), answered spoken (»humanity's
next great voyage begins«); in the arc of a dance
track at 128 beats a minute
in B minor (Bm G D A): a funky verse, the countdown as the build (a snare
roll getting faster, a rise of noise, no bass), a stop, and on »liftoff« the
drop (a kick on every beat, chords pumping under it, a bass jumping
octaves, a riff of one bar as the hook, »lift-off« chopped). Every syllable
starts where its voice starts, on the grid of sixteenths, legato within a
phrase; the chorus has fixed notes, the same every time it comes. Three
files of the same length:

  liftoff-gesprochen.flac  the commentary as spoken
  liftoff.flac          the syllables on the grid (SHEET), the voice's own pitch
  liftoff-melodie.flac  the melody, one note per syllable
  liftoff-band.flac     a real drum kit (audio/drums: DRSKit of DrumGizmo,
                        CC BY 4.0, and a clap, CC0) and the instruments,
                        harmonies and chops of the voice

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
BPM = 128
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
    'crew-two': [(L + 15.02, L + 15.12, 1), (L + 15.12, L + 15.36, 1), (L + 15.36, L + 15.56, 1), (L + 15.56, L + 15.92, 3),
                 (L + 15.92, L + 16.18, 1)],
    'bound-moon': [(L + 16.18, L + 16.56, 1), (L + 16.56, L + 16.78, 1), (L + 16.78, L + 17.14, 1), (L + 17.14, L + 17.26, 1),
                   (L + 17.26, L + 17.50, 1)],
    'humanity-begins': [(L + 18.04, L + 18.64, 4), (L + 18.64, L + 18.82, 1), (L + 18.82, L + 19.22, 1), (L + 19.22, L + 19.68, 2),
                        (L + 19.68, L + 20.14, 2)],
}


STEP = {'C': 0, 'D': 2, 'E': 4, 'F': 5, 'G': 7, 'A': 9, 'B': 11}
C2, C3 = 65.41, 130.81


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



# the song: 24 bars of seventh chords, as semitones above C (the root first).
# The verse in minor (Bm7 Gmaj7 Em7 F#7), the build and the chorus on the
# »royal road« of Japanese pop and anime, IV V iii vi (Gmaj7 A7 F#7 Bm7),
# with F#7 instead of F#m7, which pulls to Bm
BM7, GMAJ7, EM7, FS7, A7 = [11, 2, 6, 9], [7, 11, 2, 6], [4, 7, 11, 2], [6, 10, 1, 4], [9, 1, 4, 7]
CHORDS = ([BM7, GMAJ7, EM7, FS7] * 2 + [GMAJ7, A7, FS7, BM7] * 2
          + [GMAJ7, A7, FS7, BM7, A7, FS7, BM7, GMAJ7])
SCALE = [11, 1, 2, 4, 6, 7, 9]
BARS = len(CHORDS)
LENGTH = int(round(BARS * BAR * RATE))
SIXTEENTH = BEAT / 4
VERSE, BUILD, DROP = range(0, 8), range(8, 16), range(16, 24)
# the spoken part, as it was said, over the verse and the build: (where in
# the song, from, to), in seconds; the countdown so placed that »lift«
# (OFFSET + 12.94 s) falls on the first beat of the drop
SPOKEN = [
    (2.0, 2.40, 8.90),                  # … ready to ride the most powerful rocket
    (8.8, 11.25, 13.60),                # NASA has ever launched: without the pause of 2,4 s
    (16 * BAR - 12.94, OFFSET + 0.0, OFFSET + 13.80),   # … and liftoff, spoken, »lift« on the drop
    (19 * BAR, OFFSET + 17.98, OFFSET + 20.30),         # humanity's next great voyage begins, spoken, as an answer
    (22 * BAR, OFFSET + 17.98, OFFSET + 20.30),
]
# the chorus, sung, after the drop: (bar, sixteenth in the bar, piece, sixteenths per syllable,
# None for the length it was spoken, rounded to whole sixteenths, or 'hold'
# for that and the last syllable held to the end of the bar; a note per syllable)
SHEET = [
    # the hook, written first: its rhythm and notes; the syllables are laid on them
    (17, 0, 'crew-two', [1, 2, 1, 2, 1, 1, 8], 'F# A B B A B C#+'),    # over A7, held on its third
    (18, 0, 'bound-moon', [2, 2, 1, 1, 10], 'C#+ B A# F# B'),         # over F#7, A# leading, held B into Bm7
    (20, 0, 'crew-two', [1, 2, 1, 2, 1, 1, 8], 'F# A B B A B C#+'),
    (21, 0, 'bound-moon', [2, 2, 1, 1, 10], 'C#+ B A# F# B'),
]
NOTES = {'C': 0, 'C#': 1, 'D': 2, 'D#': 3, 'E': 4, 'F': 5, 'F#': 6, 'G': 7, 'G#': 8, 'A': 9, 'A#': 10, 'B': 11}


def note_semis(name):
    """semitones above C3: »F#« is F#3, »D+« is D4"""
    return NOTES[name.rstrip('+')] + (12 if name.endswith('+') else 0)


def voice_start(t0, t1):
    """where the voice of a syllable starts: the first 10 ms after t0 - 0.05 s
    (and before t1) that is no more than 24 dB below the syllable's loudest"""
    step = int(0.01 * RATE)
    a, b = at(max(0, t0 - 0.05)), at(t1)
    levels = [np.sqrt(np.mean(speech[i:i + step] ** 2)) for i in range(a, b, step)]
    top = max(levels) if levels else 0
    first = next((k for k, v in enumerate(levels) if v > top * 10 ** (-24 / 20)), 0)
    return max(0, t0 - 0.05) + first * 0.01


voice = np.zeros(LENGTH + 2 * RATE)
melody = np.zeros(LENGTH + 2 * RATE)
shifts, syllables, voice_pitch = [], [], []
for start, t0, t1 in SPOKEN:
    place(voice, fade(speech[at(t0):at(t1)], 30, 80), start)
for bar, sixteenth, name, lengths, notes in SHEET:
    times = PIECES[name]
    notes = notes.split()
    if lengths in (None, 'hold'):
        held_last = lengths == 'hold'
        lengths = [max(1, round((b - a) / SIXTEENTH)) for a, b in zip(times[:-1], times[1:])]
        if held_last:                            # the last syllable held to the end of the bar (»twoooo«, »mooon«)
            lengths[-1] = max(lengths[-1], 16 - sixteenth - sum(lengths[:-1]))
    assert len(lengths) == len(times) - 1 == len(notes), name
    bounds = [voice_start(times[0], times[1])] + [refine(t) for t in times[1:-1]] + [times[-1]]
    slot = bar * BAR + sixteenth * SIXTEENTH
    for (t0, t1), units, name_note in zip(zip(bounds[:-1], bounds[1:]), lengths, notes):
        piece = speech[zero_crossing(at(t0)):zero_crossing(at(t1))]
        n = int(units * SIXTEENTH * RATE) + int(0.02 * RATE)
        n = max(n, min(len(piece), int(n * 1.25)))
        held = hold(piece, n)
        rms = np.sqrt(np.mean(held ** 2)) + 1e-9
        held *= (0.12 / rms) ** 0.5
        place(voice, fade(held, 6, 20), slot)
        f = own_pitch(piece)
        note = note_semis(name_note)
        if f:
            mine = 12 * np.log2(f / C3)
            voice_pitch.append(f)
            # the same note an octave up or down, whichever is nearer to the voice
            note = min((note - 12, note, note + 12), key=lambda k: abs(k - mine))
            shifts.append(note - mine)
        syllables.append((slot, held, f, note))
        place(melody, 0.3 * fade(tone(semis_hz(note), int(units * SIXTEENTH * RATE)), 12, 30), slot)
        slot += units * SIXTEENTH
voice = voice[:LENGTH] * 0.85 / np.abs(voice).max()
melody = melody[:LENGTH]
print('voice median %.0f Hz; shift per syllable: median %.1f, largest %.1f semitones'
      % (np.median(voice_pitch), np.median(np.abs(shifts)), np.max(np.abs(shifts))))


# the band ----------------------------------------------------------------
def sample(name):
    with wave.open(os.path.join(DRUMS, name + '.wav'), 'rb') as f:
        return np.frombuffer(f.readframes(f.getnframes()), '<i2') / 32768


def clav(f, n):
    """a clavinet: a plucked string with many bright harmonics that fade
    fast, the odd ones a little stronger, a hard start"""
    t = np.arange(n) / RATE
    out = sum(np.sin(2 * np.pi * f * k * t) / k ** 0.8 * (1.3 if k % 2 else 0.8) * np.exp(-t * (7 + 4 * k))
              for k in range(1, 16) if f * k < 9000)
    return fade(out, 1, 15)


def saws(f, n, voices=(0.99, 0.995, 1.0, 1.005, 1.01), top=9000):
    """detuned saw waves, as a dance track's chords and leads"""
    t = np.arange(n) / RATE
    return sum(np.sin(2 * np.pi * f * d * k * t + 1.7 * k * d) / k for d in voices for k in range(1, 30) if f * k < top) / len(voices)


def pluck_bass(f, n):
    """a bass whose brightness falls after each note"""
    t = np.arange(n) / RATE
    out = sum(np.sin(2 * np.pi * f * k * t) / k * np.exp(-t * (4 + 6 * k)) for k in range(1, 14) if f * k < 5000)
    return fade(out + 0.8 * np.sin(2 * np.pi * f * t) * np.exp(-t * 3), 2, 20)


def scale_step(note, steps, chord=None):
    """the note steps notes higher: on the tones of the chord when it is given
    (so that nothing clashes with its A#), else on the scale"""
    ladder = sorted(o * 12 + c for o in range(-1, 5) for c in (chord or SCALE))
    k = min(range(len(ladder)), key=lambda i: abs(ladder[i] - note))
    return ladder[min(len(ladder) - 1, k + steps)]


def chord_at(seconds):
    return CHORDS[min(BARS - 1, int(seconds / BAR))]


kick, snare, clap = sample('kick'), sample('snare'), sample('clap')
hat, open_hat, crash = sample('hat-closed'), sample('hat-open'), sample('crash')
open_short = open_hat[:int(0.2 * RATE)] * np.linspace(1, 0, int(0.2 * RATE))
drums = np.zeros(LENGTH + 5 * RATE)
keys = np.zeros(LENGTH + 3 * RATE)
low = np.zeros(LENGTH + 3 * RATE)
lead = np.zeros(LENGTH + 3 * RATE)
pump = np.ones(LENGTH + 3 * RATE)                  # what the kick ducks in the drop
rng = np.random.default_rng(1962)
# the hook of the drop: a riff of eighths, steps of the scale above the chord's root
RIFF = [0, 4, 7, 4, 9, 7, 4, 2]
for bar, chord in enumerate(CHORDS):
    t0 = bar * BAR
    root = semis_hz(chord[0], C2 / 2)
    if bar in VERSE:
        # the first four bars only hi-hats, then a plain, solid groove
        for k in range(0, 16, 2):
            place(drums, hat, t0 + k * SIXTEENTH, 0.14)
        if bar >= 4:
            for k in (0, 8, 10):
                place(drums, kick, t0 + k * SIXTEENTH, 0.8)
            for k in (4, 12):
                place(drums, snare, t0 + k * SIXTEENTH, 0.55)
        for k, level in {0: 0.08, 3: 0.06, 6: 0.08, 10: 0.07, 14: 0.07}.items():
            place(keys, sum(clav(semis_hz(c + 12 if c < 6 else c), int(SIXTEENTH * 1.5 * RATE)) for c in chord), t0 + k * SIXTEENTH, level)
        for k, length in ((0, 6), (6, 4), (10, 6)):
            place(low, pluck_bass(root * 2, int(length * SIXTEENTH * RATE)), t0 + k * SIXTEENTH, 0.17)
    elif bar in BUILD:
        # the snare roll: quarters, eighths, then sixteenths, louder evenly; the last beat silent
        step = {8: 4, 9: 4, 10: 2, 11: 2, 12: 1, 13: 1, 14: 1, 15: 1}[bar]
        k = 0.0
        while k < (12 if bar == 15 else 16):
            progress = ((bar - 8) * 16 + k) / (8 * 16)
            place(drums, snare, t0 + k * SIXTEENTH, 0.12 + 0.33 * progress)
            k += step
        for c in chord:                          # the chord held high and thin
            place(keys, saws(semis_hz(c) * 4, int(BAR * RATE), top=12000) * np.linspace(0.3, 1, int(BAR * RATE)), t0, 0.006 + 0.001 * (bar - 8) if bar < 15 else 0.0)
    else:
        if bar in (16, 20):
            place(drums, crash, t0, 0.4)
        for k in range(0, 16, 4):
            place(drums, kick, t0 + k * SIXTEENTH, 0.95)
            a = int((t0 + k * SIXTEENTH) * RATE)
            d = int(BEAT * 0.9 * RATE)
            pump[a:a + d] = np.minimum(pump[a:a + d], 0.25 + 0.75 * np.linspace(0, 1, d) ** 1.5)
        for k in (4, 12):
            place(drums, clap, t0 + k * SIXTEENTH, 0.5)
            place(drums, snare, t0 + k * SIXTEENTH, 0.4)
        for k in range(16):
            place(drums, open_short if k % 4 == 2 else hat, t0 + k * SIXTEENTH, 0.22 if k % 4 == 2 else 0.1)
        # chords held (they pump under the kick), the bass jumping octaves on eighths
        for c in chord:
            place(keys, saws(semis_hz(c) * 2, int(BAR * RATE)), t0, 0.06)
        for k in range(8):
            place(low, pluck_bass(root * (4 if k % 2 else 2), int(2 * SIXTEENTH * 0.9 * RATE)), t0 + k * 2 * SIXTEENTH, 0.17)
        # the riff, an octave above the voice
        base = chord[0] + 24
        for k, steps in enumerate(RIFF):
            note = scale_step(base, [0, 2, 4, 2, 5, 4, 2, 1][k], chord)
            place(lead, fade(saws(semis_hz(note), int(2 * SIXTEENTH * 0.8 * RATE), voices=(0.995, 1.0, 1.005)), 3, 30), t0 + k * 2 * SIXTEENTH, 0.13)
# a rise of noise through the build, brighter as it goes
n = int((8 * BAR - BEAT) * RATE)
noise = rng.uniform(-1, 1, n)
noise = noise - np.concatenate([[0], noise[:-1]])
place(drums, noise * np.linspace(0, 1, n) ** 3, 8 * BAR, 0.05)
place(drums, crash[::-1][-int(1.5 * RATE):], 16 * BAR - 1.5, 0.3)


def shift(x, factor):
    """x factor-times as high, as long as it is: grains of 40 ms, each read
    factor-times as fast and moved by up to one period to fit"""
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


extras = np.zeros(LENGTH + 3 * RATE)
# harmonies in the drop: the voice a third above its melody, exactly on the notes
for slot, held, f, note in syllables:
    if 16 * BAR <= slot and f:
        place(extras, fade(shift(held, semis_hz(scale_step(note, 1, chord_at(slot))) / f), 5, 30), slot + 0.012, 0.35)
# »lift-off« chopped: on the offbeats of the drop's second and fourth bar, as an answer
lift = PIECES['liftoff']
c0, c1 = int(round(lift[0] * RATE)), int(round(lift[-1] * RATE))
chop = speech[c0:c1]
chop_f = own_pitch(speech[c0:int(round(lift[1] * RATE))]) or 120
for bar in (17, 19, 21, 23):
    for k, steps in ((10, 4), (14, 2)):
        target = semis_hz(scale_step(CHORDS[bar][0] + 12, steps // 2, CHORDS[bar]))
        place(extras, fade(shift(chop, target / chop_f), 3, 40), bar * BAR + k * SIXTEENTH, 0.3)

band = drums[:LENGTH] + (keys * pump)[:LENGTH] + low[:LENGTH] + lead[:LENGTH] + extras[:LENGTH]
spec = np.fft.rfft(band)
freq = np.fft.rfftfreq(len(band), 1 / RATE)
spec *= 10 ** (-4 * np.exp(-(np.log2(np.maximum(freq, 1) / 2000) / 0.9) ** 2) / 20)
band = np.fft.irfft(spec, len(band))
band *= 0.8 / np.abs(band).max()
print('band: drums %.3f keys %.3f bass %.3f lead %.3f extras %.3f' % tuple(float(np.sqrt(np.mean(z[:LENGTH] ** 2))) for z in (drums, keys * pump, low, lead, extras)))

# the commentary as spoken, the two excerpts one after the other with half a second between
spoken = np.concatenate([speech[:len(explorers)], np.zeros(int(0.5 * RATE)), speech[int(OFFSET * RATE):int(OFFSET * RATE) + len(liftoff)]])
write('liftoff-gesprochen.flac', spoken * 0.85 / np.abs(spoken).max())
write('liftoff.flac', voice)
write('liftoff-melodie.flac', melody)
write('liftoff-band.flac', band)
