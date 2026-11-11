"""The card picture of »Audioeffekte«: five seconds of the Gesang clip as a
spectrogram (time to the right, frequency from 90 Hz to 2,6 kHz on a linear scale,
the louder the stronger the colour), as sung on the left, and from a line
on, the same voice after the autotune widget's »ein Ton« (its own code,
autotune.js): the harmonics of the voice, curved by vibrato and glides,
become straight lines. 1600 × 900, light and dark, in the colours of the
card of »Audio komprimieren«; drawn twice as large and scaled down.

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/audio-effects/bilder/teaser.py
"""

import os
import subprocess
import tempfile

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.join(HERE, '..', '..', '..', '..')
SOURCE = os.path.join(REPO, 'src', 'static', 'audio', 'effects', 'singing-voice-2.flac')
WIDGET = os.path.join(REPO, 'src', 'static', 'include', 'effect_widget.js')
OUT = os.path.join(HERE, '..')
W, H = 1600, 900
RATE = 44100
START, LENGTH, SPLIT = 1.9, 5.0, 0.48       # seconds of the clip; where the line stands, of the width
BOTTOM, TOP = 90, 2600                      # Hz at the lower and upper edge
N, PAD, HOP = 4096, 16384, 128             # a window of N samples, zero-padded to PAD: smoother in frequency, as sharp in time
RANGE = 50                                  # dB from the loudest to the ground colour
THEMES = {
    '': {'ground': '#f1f2f5', 'hot': '#2d1680', 'mark': '#6741d9'},
    '-dark': {'ground': '#111213', 'hot': '#e5dbff', 'mark': '#b197fc'},
}


def rgb(colour):
    return np.array([int(colour[i:i + 2], 16) for i in (1, 3, 5)], dtype=float)


def spectrogram(z):
    """level in dB per frequency (rows, low at the bottom) and time (columns)"""
    window = np.hanning(N)
    frames = [np.abs(np.fft.rfft(z[i:i + N] * window, PAD)) for i in range(0, len(z) - N, HOP)]
    rows = slice(int(BOTTOM / RATE * PAD), int(TOP / RATE * PAD))
    return 20 * np.log10(np.array(frames).T[rows][::-1] + 1e-9)


with tempfile.TemporaryDirectory() as tmp:
    raw, tuned = os.path.join(tmp, 'gesang.f32'), os.path.join(tmp, 'tuned.f32')
    subprocess.run(['ffmpeg', '-loglevel', 'error', '-i', SOURCE, '-ac', '1', '-ar', str(RATE), '-f', 'f32le', raw], check=True)
    subprocess.run(['node', os.path.join(HERE, 'autotune.js'), WIDGET, raw, tuned, 'flat'], check=True)
    x, y = np.fromfile(raw, '<f4'), np.fromfile(tuned, '<f4')

a, b = int(START * RATE) - N // 2, int((START + LENGTH) * RATE) + N // 2
before, after = spectrogram(x[a:b]), spectrogram(y[a:b])
# the same scale for both: RANGE dB below the loudest of the original
top = before.max()
level = lambda s: np.clip((s - (top - RANGE)) / RANGE, 0, 1) ** 1.5
cut = int(SPLIT * before.shape[1])
# a short crossfade at the line, so the two halves meet without a seam
blend = np.clip((np.arange(before.shape[1]) - cut) / 6 + 0.5, 0, 1)
v = level(before) * (1 - blend) + level(after) * blend
# a little blur at twice the size: without it the zero-padding leaves a dotted grain between the lines
big = Image.fromarray(v.astype(np.float32), 'F').resize((2 * W, 2 * H), Image.BICUBIC)
kernel = np.exp(-np.arange(-9, 10) ** 2 / (2 * 3.0 ** 2))
kernel /= kernel.sum()
big = np.array(big)
big = np.apply_along_axis(lambda r: np.convolve(r, kernel, 'same'), 0, big)
big = np.clip(np.apply_along_axis(lambda r: np.convolve(r, kernel, 'same'), 1, big), 0, 1)
line = int(SPLIT * 2 * W)
for suffix, theme in THEMES.items():
    ground, hot, mark = rgb(theme['ground']), rgb(theme['hot']), rgb(theme['mark'])
    picture = ground + big[..., None] * (hot - ground)
    picture[:, line - 3:line + 3] = mark
    Image.fromarray(picture.clip(0, 255).astype(np.uint8)).resize((W, H), Image.LANCZOS).save(
        os.path.join(OUT, f'teaser{suffix}.webp'), quality=90)
    print(f'teaser{suffix}.webp')
