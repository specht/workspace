"""The two sounds of the autotune widget: »Ode an die Freude« (Beethoven,
1824) in its own rhythm, E E F G | G F E D | C C D E | E. D D, as a soft
tone with a few overtones in the range of the voice; and a voice timed
to it: the words of the tune, »Freude, schöner Götterfunken, Tochter aus
Elysium«, spoken by Thorsten (lyrics_words.py), one syllable per note,
made as long as it with the grains of shift() of the page.
Both files have the same length, so the widget plays them in the same
loop and takes the melody's pitch as the target of autotune.

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/audio-effects/bilder/voice_melody.py
"""

import os
import wave

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
AUDIO = os.path.join(HERE, '..', '..', '..', 'static', 'audio')
SIZE, HOP = 2048, 512

# the first line of the tune, C3 = 130.8 Hz, with its length in beats
C3 = 130.81
STEPS = {'C': 0, 'D': 2, 'E': 4, 'F': 5, 'G': 7}
TUNE = [('E', 1), ('E', 1), ('F', 1), ('G', 1), ('G', 1), ('F', 1), ('E', 1), ('D', 1),
        ('C', 1), ('C', 1), ('D', 1), ('E', 1), ('E', 1.5), ('D', 0.5), ('D', 2)]


def read(path):
    with wave.open(path, 'rb') as f:
        rate = f.getframerate()
        return np.frombuffer(f.readframes(f.getnframes()), '<i2') / 32768, rate


def pitch(block, rate):
    best, best_lag, dipped = 0, 0, False
    for lag in range(1, rate // 60):
        r = np.dot(block[:-lag], block[lag:])
        if r < 0:
            dipped = True
        elif dipped and r > best:
            best, best_lag = r, lag
    return rate / best_lag if best_lag else 0


rate = 44100
LYRICS = os.path.join(HERE, 'lyrics')
# the words (lyrics_words.py) and their syllables, one per note
WORDS = [('0-freude', 2), ('1-schoener', 2), ('2-goetterfunken', 4), ('3-tochter', 2), ('4-aus', 1), ('5-elysium', 4)]


def syllables(word, n):
    """the word without the silence around it, cut into n pieces at the n - 1
    deepest dips of its loudness (10 ms steps), each at least 60 ms long"""
    loud = np.abs(word) > 0.02 * np.abs(word).max()
    word = word[np.argmax(loud):len(word) - np.argmax(loud[::-1])]
    step = int(0.01 * rate)
    env = np.array([np.sqrt(np.mean(word[i:i + step] ** 2)) for i in range(0, len(word) - step, step)])
    env = np.convolve(env, np.ones(3) / 3, 'same')
    dips = [i for i in range(1, len(env) - 1) if env[i] <= env[i - 1] and env[i] <= env[i + 1]]
    cuts = []
    for i in sorted(dips, key=lambda i: env[i]):
        if len(cuts) == n - 1:
            break
        if 6 <= i <= len(env) - 6 and all(abs(i - c) >= 6 for c in cuts):
            cuts.append(i)
    edges = [0] + [c * step for c in sorted(cuts)] + [len(word)]
    return [word[a:b] for a, b in zip(edges[:-1], edges[1:])]


pieces = []
for name, n in WORDS:
    w, r = read(os.path.join(LYRICS, name + '.wav'))
    w = np.interp(np.arange(len(w) * rate // r) * r / rate, np.arange(len(w)), w)
    parts = syllables(w, n)
    print(name, [round(len(p) / rate, 2) for p in parts])
    pieces += parts
assert len(pieces) == len(TUNE), (len(pieces), len(TUNE))


def stretch(piece, n):
    """piece made n samples long with grains of 50 ms, each moved to fit
    what is already there (as shift() of the page, with factor 1)"""
    G = int(0.05 * rate)
    D = rate // 60
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
    out = y[G // 2:G // 2 + n] / np.maximum(weight[G // 2:G // 2 + n], 1e-3)
    ramp = int(0.015 * rate)
    out[:ramp] *= np.linspace(0, 1, ramp)
    out[-ramp:] *= np.linspace(1, 0, ramp)
    return out


# each syllable sings its note, made as long as the note; within a word
# the syllables follow without a gap, between words a short one
BEAT = 0.4
total_beats = sum(d for _, d in TUNE)
onsets = np.cumsum([0] + [d for _, d in TUNE])[:-1] * BEAT
length = int(round((total_beats * BEAT + 0.3) * rate))
print(f'beat {BEAT} s, {60 / BEAT:.0f} beats a minute, {length / rate:.2f} s')
word_ends = set(np.cumsum([n for _, n in WORDS]) - 1)

voice = np.zeros(length + rate)
for k, piece in enumerate(pieces):
    beats = TUNE[k][1]
    n = int(beats * BEAT * (0.9 if k in word_ends else 1.0) * rate)
    at = int(round(onsets[k] * rate))
    voice[at:at + n] += stretch(piece, n)
voice = voice[:length] * 0.8 / np.abs(voice).max()

y = np.zeros(length)
for (note, beats), t in zip(TUNE, onsets):
    f = C3 * 2 ** (STEPS[note] / 12)
    n = int(beats * BEAT * 0.92 * rate)
    tt = np.arange(n) / rate
    tone = np.sin(2 * np.pi * f * tt) + 0.35 * np.sin(4 * np.pi * f * tt) + 0.15 * np.sin(6 * np.pi * f * tt)
    ramp = int(0.02 * rate)
    envelope = np.ones(n)
    envelope[:ramp] = np.linspace(0, 1, ramp)
    envelope[-ramp:] = np.linspace(1, 0, ramp)
    at = int(round(t * rate))
    y[at:at + n] += 0.35 * tone * envelope


def write(name, z):
    out = os.path.join(AUDIO, name)
    with wave.open(out, 'wb') as f:
        f.setnchannels(1)
        f.setsampwidth(2)
        f.setframerate(rate)
        f.writeframes((np.clip(z, -1, 1) * 32767).astype('<i2').tobytes())
    print(out)


write('stimme-melodie.wav', y)
write('stimme-im-takt.wav', voice)
