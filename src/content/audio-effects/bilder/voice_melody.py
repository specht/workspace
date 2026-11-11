"""The melody for the autotune widget: stimme.wav sings »Ode an die
Freude« (Beethoven, 1824). Every voiced stretch of the voice (its pitch found
as pitch() of the page finds it) gets the next note of the tune, as a soft
tone with a few overtones, in the range of the voice; between them silence.
The file is exactly as long as stimme.wav, so the widget can play both in
the same loop and use the melody's pitch as the target of autotune.

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/audio-effects/bilder/voice_melody.py
"""

import os
import wave

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
AUDIO = os.path.join(HERE, '..', '..', '..', 'static', 'audio')
SIZE, HOP = 2048, 512

# the first two lines of the tune, C3 = 130.8 Hz: E E F G G F E D C C D E E D D
C3 = 130.81
STEPS = {'C': 0, 'D': 2, 'E': 4, 'F': 5, 'G': 7}
TUNE = 'E E F G G F E D C C D E E D D'.split()


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

y = np.zeros(len(x))
for (a, b), note in zip(stretches, TUNE * 3):
    f = C3 * 2 ** (STEPS[note] / 12)
    t = np.arange(b - a) / rate
    tone = np.sin(2 * np.pi * f * t) + 0.35 * np.sin(4 * np.pi * f * t) + 0.15 * np.sin(6 * np.pi * f * t)
    ramp = min(len(t) // 2, int(0.015 * rate))
    envelope = np.ones(len(t))
    envelope[:ramp] = np.linspace(0, 1, ramp)
    envelope[-ramp:] = np.linspace(1, 0, ramp)
    y[a:b] += 0.35 * tone * envelope
    print(f'{a / rate:.2f}–{b / rate:.2f} s: {note} {f:.1f} Hz')

out = os.path.join(AUDIO, 'stimme-melodie.wav')
with wave.open(out, 'wb') as f:
    f.setnchannels(1)
    f.setsampwidth(2)
    f.setframerate(rate)
    f.writeframes((np.clip(y, -1, 1) * 32767).astype('<i2').tobytes())
print(out, len(y) == len(x))
