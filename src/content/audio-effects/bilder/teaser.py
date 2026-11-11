"""The card picture of »Audioeffekte«: the compressor widget of the page,
fitted to the card. On the left its curve (how loud a sound comes out for
each level: dashed without the compressor, purple with it, a dot at the
level that plays), on the right the sound itself, an orchestra from very
quiet to loud (»Orchester leise und laut« of the widget): above as it was,
in grey, below in purple as compress() of the page makes it, the quiet part
raised, the loud one held down. No labels and no boxes, the widget's
colours on its ground, light and dark; 1600 × 900, drawn twice as large.

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/audio-effects/bilder/teaser.py
"""

import math
import os
import subprocess

import numpy as np
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.join(HERE, '..', '..', '..', '..')
SOURCE = os.path.join(REPO, 'src', 'static', 'audio', 'effects', 'dynamic-orchestra.flac')
OUT = os.path.join(HERE, '..')
W, H, S = 1600, 900, 2                      # the picture, drawn S times as large
RATE = 22050
START, LENGTH = 0, 19.5                     # seconds of the clip: all of it
THRESHOLD, RATIO, ATTACK, RELEASE, MAKEUP = -30, 6, 0.005, 0.2, 14
# the widget's colours (default.css, --fx-…)
THEMES = {
    '': {'ground': '#f1f2f5', 'quiet': '#dbdee3', 'sound': '#8a90a0', 'mark': '#6741d9'},
    '-dark': {'ground': '#111213', 'quiet': '#2a2e34', 'sound': '#6b7280', 'mark': '#b197fc'},
}


def gain_db(level, threshold, ratio):
    return 0 if level <= threshold else (threshold - level) * (1 - 1 / ratio)


def compress(x, rate):
    """compress() of the page, its level in dB per sample"""
    a, r = math.exp(-1 / (ATTACK * rate)), math.exp(-1 / (RELEASE * rate))
    level, y, levels = 0.0, np.zeros_like(x), np.zeros_like(x)
    for n, v in enumerate(np.abs(x).tolist()):
        c = a if v > level else r
        level = c * level + (1 - c) * v
        db = 20 * math.log10(max(level, 1e-9))
        levels[n] = db
        y[n] = x[n] * 10 ** ((gain_db(db, THRESHOLD, RATIO) + MAKEUP) / 20)
    return y, levels


raw = subprocess.run(['ffmpeg', '-loglevel', 'error', '-i', SOURCE, '-ac', '1', '-ar', str(RATE), '-f', 'f32le', '-'],
                     capture_output=True, check=True).stdout
x = np.frombuffer(raw, '<f4').astype(float)
x = x[int(START * RATE):int((START + LENGTH) * RATE)]
x /= np.abs(x).max()
y, levels = compress(x, RATE)
y /= max(1, np.abs(y).max())

# where things go: the curve left, the two sounds right, in pixels of the large picture
M = 70 * S
curve = (M, M, 640 * S, H * S - M)                   # left, top, right, bottom
above = (720 * S, M, W * S - M, H * S // 2 - 25 * S)
below = (720 * S, H * S // 2 + 25 * S, W * S - M, H * S - M)


def envelope(z, box):
    """per column the lowest and highest sample, as y in the box"""
    left, top, right, bottom = box
    cols = right - left
    edges = np.linspace(0, len(z), cols + 1).astype(int)
    mid, half = (top + bottom) / 2, (bottom - top) / 2
    return [(left + i, mid - z[edges[i]:edges[i + 1]].max() * half, mid - z[edges[i]:edges[i + 1]].min() * half)
            for i in range(cols)]


for suffix, theme in THEMES.items():
    image = Image.new('RGB', (W * S, H * S), theme['ground'])
    draw = ImageDraw.Draw(image)
    # the curve: in −60 … 0 dB to the right, out −60 … +6 dB up (the widget
    # goes to +24 dB, which this curve does not reach)
    left, top, right, bottom = curve
    LO, HI = -60, 6
    xof = lambda db: left + (db - LO) / -LO * (right - left)
    yof = lambda db: top + (HI - db) / (HI - LO) * (bottom - top)
    for db in (0, -24, -48):
        draw.line([(left, yof(db)), (right, yof(db))], fill=theme['quiet'], width=2 * S)
    # dashed: without the compressor, out = in
    for k in range(0, 60, 3):
        draw.line([(xof(LO + k), yof(LO + k)), (xof(LO + k + 1.8), yof(LO + k + 1.8))], fill=theme['sound'], width=3 * S)
    out = lambda db: db + gain_db(db, THRESHOLD, RATIO) + MAKEUP
    points = [(xof(db), yof(out(db))) for db in np.linspace(LO, 0, 400)]
    draw.line(points, fill=theme['mark'], width=6 * S, joint='curve')
    now = float(np.median(levels[int(0.7 * len(levels)):]))
    cx, cy, rad = xof(now), yof(out(now)), 14 * S
    draw.ellipse([cx - rad, cy - rad, cx + rad, cy + rad], fill=theme['mark'], outline=theme['ground'], width=4 * S)
    # the sound, before and after
    for box, z, colour in ((above, x, theme['sound']), (below, y, theme['mark'])):
        for px, hi, lo in envelope(z, box):
            draw.line([(px, hi), (px, max(lo, hi + S))], fill=colour, width=1)
    image.resize((W, H), Image.LANCZOS).save(os.path.join(OUT, f'teaser{suffix}.webp'), quality=90)
    print(f'teaser{suffix}.webp')
