"""The two sounds of the autotune widget: »Ode an die Freude« (Beethoven,
1824) in its own rhythm, E E F G | G F E D | C C D E | E. D D, as a soft
tone with a few overtones in the range of the voice; and stimme.wav timed
to it: every voiced stretch of the voice (its pitch found as pitch() of
the page finds it) sings one or more notes, made as long as them with
the grains of shift() of the page.
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


x, rate = read(os.path.join(AUDIO, 'stimme.wav'))
voiced = []
for start in range(0, len(x) - SIZE, HOP):
    block = x[start:start + SIZE]
    f = pitch(block, rate) if np.sqrt(np.mean(block ** 2)) > 0.02 else 0
    voiced.append(60 < f < 400)

# stretches of voiced blocks; gaps of one block are closed, stretches shorter
# than three blocks dropped
stretches, k = [], 0
while k < len(voiced):
    if not voiced[k]:
        k += 1
        continue
    first = k
    while k < len(voiced) and (voiced[k] or (k + 1 < len(voiced) and voiced[k + 1])):
        k += 1
    if k - first >= 3:
        stretches.append((first * HOP + SIZE // 4, (k - 1) * HOP + 3 * SIZE // 4))
print(len(stretches), 'stretches:', [(round(a / rate, 2), round(b / rate, 2)) for a, b in stretches])



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


# each stretch gets consecutive notes, the longer ones more: the notes are
# shared out so that the voice's own time in front of each stretch decides
# on which note it starts
BEAT = 0.42
total_beats = sum(d for _, d in TUNE)
onsets = np.cumsum([0] + [d for _, d in TUNE])[:-1] * BEAT
lengths = np.array([b - a for a, b in stretches], float)
share = np.concatenate([[0], np.cumsum(lengths) / lengths.sum()])
first_note = [int(round(f * len(TUNE))) for f in share[:-1]]
for k in range(1, len(first_note)):
    first_note[k] = max(first_note[k], first_note[k - 1] + 1)
length = int(round((total_beats * BEAT + 0.3) * rate))
print(f'beat {BEAT} s, {60 / BEAT:.0f} beats a minute, {length / rate:.2f} s')

voice = np.zeros(length)
for k, (a, b) in enumerate(stretches):
    start = onsets[first_note[k]]
    end = onsets[first_note[k + 1]] if k + 1 < len(stretches) else total_beats * BEAT
    n = int((end - start) * 0.92 * rate)
    at = int(round(start * rate))
    voice[at:at + n] += stretch(x[a:b], n)
    notes = ' '.join(t[0] for t in TUNE[first_note[k]:first_note[k + 1] if k + 1 < len(stretches) else None])
    print(f'stretch {a / rate:.2f}–{b / rate:.2f} s ({(b - a) / rate:.2f} s) on {notes}: {n / rate:.2f} s')

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
