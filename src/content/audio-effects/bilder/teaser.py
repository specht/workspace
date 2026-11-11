"""The card picture of »Audioeffekte«: an echo. A phrase of the Gesang clip
(»Running with Renegades«, as in the widgets) as a wave, and after it its
echoes, each a little later, smaller and fainter than the one before, as if
the sound went off into the distance; the first strong in the widgets'
purple, the last nearly the colour of the ground. No labels, the widgets'
colours, light and dark; 1600 × 900, drawn twice as large.

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/audio-effects/bilder/teaser.py
"""

import os
import subprocess

import numpy as np
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.join(HERE, '..', '..', '..', '..')
SOURCE = os.path.join(REPO, 'src', 'static', 'audio', 'effects', 'singing-voice-2.flac')
OUT = os.path.join(HERE, '..')
W, H, S = 1600, 900, 2                      # the picture, drawn S times as large
RATE = 22050
START, END = 1.95, 3.05                     # seconds of the clip: one phrase
ECHOES = 8
STEP, SHRINK, LOWER = 0.78, 0.72, 0.62      # each echo: this far on (of the width before), this wide, this high
# the widgets' colours (default.css, --fx-…)
THEMES = {
    '': {'ground': '#f1f2f5', 'mark': '#6741d9'},
    '-dark': {'ground': '#111213', 'mark': '#b197fc'},
}


def rgb(colour):
    return np.array([int(colour[i:i + 2], 16) for i in (1, 3, 5)], dtype=float)


def towards(a, b, t):
    return tuple(int(v) for v in rgb(a) * (1 - t) + rgb(b) * t)


def envelope(z, columns):
    """per column the highest and the lowest sample"""
    edges = np.linspace(0, len(z), columns + 1).astype(int)
    return [(z[edges[i]:edges[i + 1]].max(), z[edges[i]:edges[i + 1]].min()) for i in range(columns)]


raw = subprocess.run(['ffmpeg', '-loglevel', 'error', '-i', SOURCE, '-ac', '1', '-ar', str(RATE), '-f', 'f32le', '-'],
                     capture_output=True, check=True).stdout
phrase = np.frombuffer(raw, '<f4').astype(float)[int(START * RATE):int(END * RATE)]
phrase /= np.abs(phrase).max()

for suffix, theme in THEMES.items():
    image = Image.new('RGB', (W * S, H * S), theme['ground'])
    draw = ImageDraw.Draw(image)
    middle = H * S / 2
    left, width, height = 150 * S, 520 * S, 350 * S
    # the farthest first, so that each nearer one lies over it
    copies = []
    for k in range(ECHOES):
        copies.append((k, left, width, height))
        left, width, height = left + width * STEP, width * SHRINK, height * LOWER
    for k, left, width, height in reversed(copies):
        colour = towards(theme['mark'], theme['ground'], min(0.85, 0.16 * k))
        for i, (hi, lo) in enumerate(envelope(phrase, int(width))):
            draw.line([(left + i, middle - hi * height), (left + i, middle - lo * height)], fill=colour)
    image.resize((W, H), Image.LANCZOS).save(os.path.join(OUT, f'teaser{suffix}.webp'), quality=90)
    print(f'teaser{suffix}.webp')
