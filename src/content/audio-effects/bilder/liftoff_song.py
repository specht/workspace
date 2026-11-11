"""A song from NASA's commentary of the launch of Artemis II, 1 April 2026
(public domain: images.nasa.gov, KSC-20260401-AU-LMM01-0001-Artemis_II_Live_
Launch_Coverage_Program_LOS, the commentary channel without the rocket),
for the vocoder widget: the commentary as it was said over the verse, the
build and »and liftoff« on the chorus, sung only in the chorus (»The crew of
Artemis twoooo / now bound for the Moooon«), answered spoken (»humanity's
next great voyage begins«); a funk track at 128 beats a minute in B minor
with the anime »royal road« in the chorus (Gmaj7 A7 F#7 Bm7): a clean
guitar in sixteenths alone, then drums and bass, the countdown over the
groove with an electric piano growing, a stop of the whole band, and on
»liftoff« the chorus (horn stabs where the voice rests, harmonies,
»lift-off« chopped). Every syllable
starts where its voice starts, on the grid of sixteenths, legato within a
phrase; the chorus has fixed notes, the same every time it comes. Three
files of the same length:

  liftoff-gesprochen.flac  the commentary as spoken
  liftoff.flac          the syllables on the grid (SHEET), the voice's own pitch
  liftoff-melodie.flac  the melody, one note per syllable
  liftoff-traeger.flac  the melody as a punchy lead, the vocoder's carrier
  liftoff-band.flac     electronic drum samples (audio/drums-electronic, CC0)
                        and the instruments, harmonies and chops of the voice

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
    eq = 10 ** ((-2 * np.exp(-(octaves(300) / 0.7) ** 2) + 1 * np.exp(-(octaves(3000) / 0.8) ** 2)) / 20)
    eq /= 1 + (90 / np.maximum(freq, 1)) ** 4
    # a clean recording (the noise more than 40 dB below) only gets the EQ
    clean_enough = 10 * np.log10(power.sum(axis=1).max() / max(1e-12, np.median(noise) * len(freq))) > 40
    out = np.zeros(len(x) + N)
    gain = np.ones(len(freq))
    for k, i in enumerate(starts):
        want = np.ones(len(freq)) if clean_enough else np.maximum(1 - 1.2 * noise / np.maximum(power[k], 1e-12), 0.2)
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
print(f'noise below speech: {before:.1f} dB before, {snr(speech[:len(explorers)]):.1f} dB after the EQ')



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



# the song: 28 bars of seventh chords, as semitones above C (the root first).
# The verse in minor (Bm7 Gmaj7 Em7 F#7), the build and the chorus on the
# »royal road« of Japanese pop and anime, IV V iii vi (Gmaj7 A7 F#7 Bm7),
# with F#7 instead of F#m7, which pulls to Bm
BM7, GMAJ7, EM7, FS7, A7 = [11, 2, 6, 9], [7, 11, 2, 6], [4, 7, 11, 2], [6, 10, 1, 4], [9, 1, 4, 7]
# the chorus two bars per chord, twice
CHORUS = [GMAJ7, GMAJ7, A7, A7, FS7, FS7, BM7, BM7]
# 6 bars of verse (ending on F#7 into the build), 6 of build, the chorus twice
CHORDS = [BM7, GMAJ7, EM7, FS7, EM7, FS7] + [GMAJ7, A7, FS7, BM7, GMAJ7, A7] + CHORUS * 2
D = 12                                   # the bar of the drop
SCALE = [11, 1, 2, 4, 6, 7, 9]
BARS = len(CHORDS)
LENGTH = int(round(BARS * BAR * RATE))
SIXTEENTH = BEAT / 4
VERSE, BUILD, DROP = range(0, 6), range(6, D), range(D, D + 16)
# the spoken part, as it was said, over the verse and the build: (where in
# the song, from, to), in seconds; the countdown so placed that »lift«
# (OFFSET + 12.94 s) falls on the first beat of the drop
SPOKEN = [
    (0.3, 2.40, 8.90),                  # from the start: … ready to ride the most powerful rocket
    (0.3 + 6.75, 11.25, 13.60),         # NASA has ever launched: without the pause of 2,4 s
    # the whole countdown, ten … RS-25 engines lit … and liftoff, »lift« on the first bar of the chorus
    (D * BAR - 12.94, OFFSET + 0.0, OFFSET + 13.80),
    ((D + 6) * BAR, OFFSET + 17.98, OFFSET + 20.30),    # humanity's next great voyage begins, spoken, as an answer
    ((D + 14) * BAR, OFFSET + 17.98, OFFSET + 20.30),
]
# the chorus, sung, after the drop: (bar, sixteenth in the bar, piece, sixteenths per syllable,
# None for the length it was spoken, rounded to whole sixteenths, or 'hold'
# for that and the last syllable held to the end of the bar; a note per syllable)
SHEET = [
    # the hook as the maintainer set it: »The *crew of AR-te-mis *twooooo *ooo / * * * * /
    # Now *bound for *the *moooooon« (lengths in sixteenths; the line starts on
    # the »and« of 4 before its first bar)
    (D + 1, 14, 'crew-two', [2, 2, 2, 2, 1, 1, 8], 'F# A B C#+ B A C#+'),
    (D + 3, 14, 'bound-moon', [2, 4, 2, 2, 22], 'C#+ D+ C#+ A# F#+'),
    (D + 9, 14, 'crew-two', [2, 2, 2, 2, 1, 1, 8], 'F# A B C#+ B A C#+'),
    (D + 11, 14, 'bound-moon', [2, 4, 2, 2, 22], 'C#+ D+ C#+ A# F#+'),
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
shifts, syllables, voice_pitch, lead_notes = [], [], [], []
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
            # the note as written, so that the melody keeps its direction
            shifts.append(note - mine)
        syllables.append((slot, held, f, note))
        lead_notes.append((slot, units * SIXTEENTH, note))
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


PLUCKS = {}


def pluck(f, seconds, ring):
    """a guitar string (Karplus-Strong): a burst of noise in a loop one period
    long, each pass the mean of neighbouring samples; ring near 1 lets it
    ring, lower damps it like a palm on the strings"""
    key = (round(f, 2), round(seconds, 3), ring)
    if key not in PLUCKS:
        period = max(2, int(round(RATE / f)))
        n = int(seconds * RATE)
        out = np.zeros(n + period)
        out[:period] = rng.uniform(-1, 1, period)
        out[:period] -= out[:period].mean()
        for start in range(period, n + period, period):
            prev = out[start - period:start]
            block = 0.5 * (prev + np.roll(prev, 1)) * ring
            out[start:start + period] = block[:len(out[start:start + period])]
        out = out[:n]
        out[:int(0.002 * RATE)] *= np.linspace(0.3, 1, int(0.002 * RATE))
        PLUCKS[key] = fade(out, 1, 8)
    return PLUCKS[key]


def guitar_voicing(chord):
    """the chord on six strings, as a guitar holds it: the root low (E2 to
    D#3), its fifth and octave, then the third and the seventh above"""
    root = chord[0] - 12
    third, seventh = (chord[1] - chord[0]) % 12, (chord[3] - chord[0]) % 12
    return [root, root + 7, root + 12, root + 12 + third, root + 12 + seventh, root + 24]


# which strings a stroke hits: all six, the top four, the top three, three in the middle
STRINGS = {'full': slice(0, 6), 'top': slice(2, 6), 'high': slice(3, 6), 'mid': slice(1, 4)}


def bright_noise(n):
    """noise without its low part, the click of a pick"""
    return np.diff(rng.uniform(-1, 1, n + 1))


def strum(track, chord, seconds, length, level, up, ring, strings='full'):
    """a stroke of the pick over some strings: down from the lowest to the
    highest, 4 ms apart; up the other way, 3 ms apart and softer; each string
    with the click of the pick, the whole a few ms early or late and a little
    louder or softer, as a hand plays it"""
    notes = guitar_voicing(chord)[STRINGS[strings]]
    notes = list(reversed(notes)) if up else notes
    gap = 0.003 if up else 0.004
    start = seconds + rng.uniform(-0.002, 0.002)
    level *= rng.uniform(0.9, 1.1) * (0.75 if up else 1.0)
    click = int(0.003 * RATE)
    for i, note in enumerate(notes):
        place(track, pluck(semis_hz(note), length, ring), start + i * gap, level / len(notes) * 3)
        place(track, bright_noise(click) * np.linspace(1, 0, click), start + i * gap, level * 0.2)


def scratch(track, chord, seconds, level, up):
    """a muted stroke, »chk«: the left hand lies on the strings, so mostly the
    noise of the pick and a short pitch from two or three strings"""
    n = int(0.03 * RATE)
    noise = np.convolve(bright_noise(n), np.ones(4) / 4, 'same') * np.exp(-np.arange(n) / RATE / 0.008)
    start = seconds + rng.uniform(-0.002, 0.002)
    level *= rng.uniform(0.85, 1.15) * (0.8 if up else 1.0)
    place(track, noise, start, level)
    strum(track, chord, start, 0.04, level * 0.5, up, 0.8, 'mid' if rng.random() < 0.6 else 'high')


def rhodes(f, n):
    """an electric piano: a sine bent by a sine of its own frequency, the
    bending (and so the brightness) fading, a short ping of the tine on top"""
    t = np.arange(n) / RATE
    bend = 1.2 * np.exp(-t / 0.3)
    out = np.sin(2 * np.pi * f * t + bend * np.sin(2 * np.pi * f * t)) * np.exp(-t / 1.5)
    out += 0.12 * np.sin(2 * np.pi * f * 7 * t) * np.exp(-t / 0.04)
    return fade(out, 2, 40)


def horn(f, n):
    """a brass stab: harmonics that open up in the first 40 ms, as a player's
    lips start the note, then a little falling"""
    t = np.arange(n) / RATE
    open_ = 1 + 7 * np.minimum(1, t / 0.04)
    vib = 1 + 0.003 * np.sin(2 * np.pi * 5.5 * t)
    out = sum(np.sin(2 * np.pi * f * k * t * vib) / k * np.exp(-(k - 1) / open_) for k in range(1, 30) if f * k < 9000)
    return fade(out * (0.8 + 0.2 * np.exp(-t / 0.1)), 4, 30)


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


rng = np.random.default_rng(1962)
DRUMS = os.path.join(AUDIO, 'drums-electronic')
guitar = np.zeros(LENGTH + 3 * RATE)
# the guitar, as Cory Wong plays: sixteenths all the time, down on the
# beats and the »and«, up between; most of them muted scratches, the
# backbeat (2 and 4) scratched harder; on syncopated accents chords: the
# whole chord low only on the one, else short stabs on the top strings;
# the pattern changes every other bar
ACCENTS = [{0: (0.9, 'full'), 3: (0.75, 'top'), 6: (0.8, 'high'), 10: (0.8, 'top'), 13: (0.7, 'high')},
           {0: (0.85, 'top'), 2: (0.65, 'high'), 7: (0.85, 'top'), 10: (0.75, 'high'), 14: (0.8, 'top')}]


def guitar_bar(t0, chord, bar, upto=16):
    accents = ACCENTS[bar % 2]
    for k in range(upto):
        up = k % 2 == 1
        if k in accents:
            level, strings = accents[k]
            strum(guitar, chord, t0 + k * SIXTEENTH, 0.3 if strings == 'full' else 0.18, 0.2 * level, up, 0.996, strings)
        else:
            scratch(guitar, chord, t0 + k * SIXTEENTH, 0.16 if k in (4, 12) else 0.07, up)


kick, snare, clap = sample('kick'), sample('snare'), sample('clap')
hat, open_hat, crash, shaker = sample('hat-closed'), sample('hat-open'), sample('crash'), sample('perc')
open_short = open_hat[:int(0.2 * RATE)] * np.linspace(1, 0, int(0.2 * RATE))
drums = np.zeros(LENGTH + 5 * RATE)
keys = np.zeros(LENGTH + 3 * RATE)
low = np.zeros(LENGTH + 3 * RATE)
horns = np.zeros(LENGTH + 3 * RATE)
# the funky bass: (sixteenth, semitones above the root or 'next' for the step
# up to the next chord's root, sixteenths long, level); short ones are ghost notes
FUNK = [(0, 0, 2, 1.0), (1, 0, 0.5, 0.35), (3, 12, 1, 0.8), (6, 7, 2, 0.85), (8, 0, 2, 1.0),
        (10, 10, 1, 0.7), (11, 12, 1, 0.6), (13, 7, 1, 0.7), (14, 12, 1, 0.55), (15, 'next', 1, 0.75)]
# the kick, syncopated, two bars that alternate; the snare on 2 and 4 with ghost notes around
KICKS = [[(0, 0.95), (6, 0.6), (10, 0.85)], [(0, 0.95), (3, 0.55), (8, 0.7), (10, 0.85)]]
GHOSTS = [7, 9, 15]
# sixteenths where the voice sings: the horns stay out of them
sung = {int(round((slot + i * SIXTEENTH) / SIXTEENTH)) for slot, seconds, note in lead_notes
        for i in range(int(round(seconds / SIXTEENTH)))}


def funk_bass(t0, bar, level, upto=16):
    root = CHORDS[bar][0] - 24
    following = CHORDS[min(BARS - 1, bar + 1)][0] - 24
    for k, interval, length, gain in FUNK:
        if k < upto:
            note = following + (12 if following < root - 6 else 0) - 1 if interval == 'next' else root + interval
            place(low, pluck_bass(semis_hz(note, C2), int(length * SIXTEENTH * 0.9 * RATE)), t0 + k * SIXTEENTH, level * gain)


def groove(t0, bar, hats, level=1.0, upto=16):
    for k, gain in KICKS[bar % 2]:
        if k < upto:
            place(drums, kick, t0 + k * SIXTEENTH, 0.85 * gain * level)
    for k in (4, 12):
        if k < upto:
            place(drums, snare, t0 + k * SIXTEENTH, 0.55 * level)
    for k in GHOSTS:
        if k < upto:
            place(drums, snare, t0 + k * SIXTEENTH, 0.07 * level)
    for k in range(0, upto, 1 if hats == 16 else 2):
        open_ = hats == 16 and bar % 2 == 1 and k == 14
        place(drums, open_short if open_ else hat, t0 + k * SIXTEENTH, 0.2 if open_ else (0.13 if k % 2 == 0 else 0.07))


def keys_bar(t0, chord, level):
    """the electric piano: the chord on the one, held, and again on the »a« of 2"""
    for c in chord:
        place(keys, rhodes(semis_hz(c) * 2, int(1.5 * BEAT * RATE)), t0, level)
        place(keys, rhodes(semis_hz(c) * 2, int(0.4 * BEAT * RATE)), t0 + 7 * SIXTEENTH, level * 0.7)


for bar, chord in enumerate(CHORDS):
    t0 = bar * BAR
    if bar in VERSE:
        # the guitar alone with hi-hats, then drums and bass together
        guitar_bar(t0, chord, bar)
        if bar < 2:
            for k in range(0, 16, 2):
                place(drums, hat, t0 + k * SIXTEENTH, 0.12)
        else:
            groove(t0, bar, 8, 0.9)
            funk_bass(t0, bar, 0.15)
    elif bar in BUILD and bar < D - 1:
        # the countdown: the groove goes on, sixteenths on the hi-hat, the
        # electric piano coming in and growing
        guitar_bar(t0, chord, bar)
        groove(t0, bar, 16)
        funk_bass(t0, bar, 0.16)
        keys_bar(t0, chord, 0.01 + 0.004 * (bar - BUILD[0]))
    elif bar == D - 1:
        # the stop: all together on the one, then silence until »liftoff«
        strum(guitar, chord, t0, 0.8, 0.22, False, 0.998, 'full')
        place(drums, kick, t0, 0.9)
        place(drums, crash, t0, 0.25)
        funk_bass(t0, bar, 0.18, upto=1)
        for c in chord:
            place(horns, horn(semis_hz(c) * 2, int(0.25 * RATE)), t0, 0.035)
    else:
        # the chorus: the whole band
        if bar in (D, D + 8):
            place(drums, crash, t0, 0.4)
        for k in range(1, 16, 2):                    # the shaker on the off-sixteenths
            place(drums, shaker, t0 + k * SIXTEENTH, 0.09)
        groove(t0, bar, 16)
        for k in (4, 12):
            place(drums, clap, t0 + k * SIXTEENTH, 0.3)
        guitar_bar(t0, chord, bar)
        funk_bass(t0, bar, 0.19)
        keys_bar(t0, chord, 0.03)
        # the horns: short stabs on the »a« of 1 and the »and« of 3, where the voice rests
        here = round(t0 / SIXTEENTH)
        for k, length in ((3, 0.12), (10, 0.2), (14, 0.12)):
            if here + k not in sung and here + k + 1 not in sung:
                for c in chord[1:]:
                    place(horns, horn(semis_hz(c) * 2, int(length * RATE)), t0 + k * SIXTEENTH, 0.03)

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
    if D * BAR <= slot and f:
        place(extras, fade(shift(held, semis_hz(scale_step(note, 1, chord_at(slot))) / f), 5, 30), slot + 0.012, 0.35)
# »lift-off« chopped: on the offbeats of the drop's second and fourth bar, as an answer
lift = PIECES['liftoff']
c0, c1 = int(round(lift[0] * RATE)), int(round(lift[-1] * RATE))
chop = speech[c0:c1]
chop_f = own_pitch(speech[c0:int(round(lift[1] * RATE))]) or 120
for bar in (D + 1, D + 9, D + 15):
    for k, steps in ((10, 4), (14, 2)):
        target = semis_hz(scale_step(CHORDS[bar][0] + 12, steps // 2, CHORDS[bar]))
        place(extras, fade(shift(chop, target / chop_f), 3, 40), bar * BAR + k * SIXTEENTH, 0.3)

band = drums[:LENGTH] + 1.7 * guitar[:LENGTH] + keys[:LENGTH] + low[:LENGTH] + 1.6 * horns[:LENGTH] + extras[:LENGTH]
spec = np.fft.rfft(band)
freq = np.fft.rfftfreq(len(band), 1 / RATE)
spec *= 10 ** (-4 * np.exp(-(np.log2(np.maximum(freq, 1) / 2000) / 0.9) ** 2) / 20)
band = np.fft.irfft(spec, len(band))
band *= 0.8 / np.abs(band).max()
print('band: drums %.3f guitar %.3f keys %.3f bass %.3f horns %.3f extras %.3f' % tuple(float(np.sqrt(np.mean(z[:LENGTH] ** 2))) for z in (drums, guitar, keys, low, horns, extras)))
if os.environ.get('GUITAR_PREVIEW'):
    # the guitar alone over the bass, for listening
    demo = guitar[:LENGTH] / np.abs(guitar).max() * 0.7 + low[:LENGTH] / max(1e-9, np.abs(low).max()) * 0.35
    write(os.environ['GUITAR_PREVIEW'], demo / np.abs(demo).max() * 0.9)

# the carrier of the vocoder: the melody as a punchy lead of saw and square
# waves, a sharp start, held as long as the note, silent between the notes
carrier = np.zeros(LENGTH + RATE)
for slot, seconds, note in lead_notes:
    n = int(seconds * RATE)
    t = np.arange(n) / RATE
    f = semis_hz(note)
    wave_ = sum(np.sin(2 * np.pi * f * k * t) / k for k in range(1, 40) if f * k < 12000)
    wave_ += 0.5 * sum(np.sin(2 * np.pi * f * 1.003 * k * t) / k for k in range(1, 40, 2) if f * k < 12000)
    env = np.minimum(1, t / 0.004) * (0.75 + 0.25 * np.exp(-t / 0.08))
    place(carrier, fade(wave_ * env, 1, 25), slot, 0.5)
write('liftoff-traeger.flac', carrier[:LENGTH] * 0.9 / np.abs(carrier).max())

# the commentary as spoken, the two excerpts one after the other with half a second between
spoken = np.concatenate([speech[:len(explorers)], np.zeros(int(0.5 * RATE)), speech[int(OFFSET * RATE):int(OFFSET * RATE) + len(liftoff)]])
write('liftoff-gesprochen.flac', spoken * 0.85 / np.abs(spoken).max())
write('liftoff.flac', voice)
write('liftoff-melodie.flac', melody)
write('liftoff-band.flac', band)
