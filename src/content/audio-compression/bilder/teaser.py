"""The picture for the card of »Audio komprimieren«: the spectrogram of
musik.wav as the widget on the page shows it (an FFT of 4.096 samples under
a Hann window, frequencies on a logarithmic scale from 40 Hz to 20 kHz, the
louder the stronger the colour), a line at one moment, and under it the
spectrum at that moment as a filled curve. 1600 × 900, light and dark, with
the widget's colours; computed, not drawn, so it is a picture (WebP), drawn
twice as large and scaled down for smooth lines.

    src/scripts/drawings python3 src/content/audio-compression/bilder/teaser.py
"""

import os
import wave

import numpy as np
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, '..')
SOURCE = os.path.join(HERE, '..', '..', '..', 'static', 'audio', 'musik.wav')
W, H, S = 1600, 900, 2                    # the picture, drawn S times as large
N, HOP = 4096, 256
BIN = 44100 / N
F0, F1 = 40, 20000
FLOOR, TOP = -90, -6
AT = 5.29                                 # the moment marked, a beat of the bass drum
GRAM = (0, 0, W, 560)                     # the spectrogram
CHART = (0, 600, W, 880)                  # the spectrum under it
THEMES = {
    '': {'ground': '#f1f2f5', 'hot': '#2d1680', 'mark': '#6741d9', 'back': '#ffffff', 'line': '#dbdee3'},
    '-dark': {'ground': '#111213', 'hot': '#e5dbff', 'mark': '#b197fc', 'back': '#151514', 'line': '#2a2e34'},
}


def rgb(colour):
    return np.array([int(colour[i:i + 2], 16) for i in (1, 3, 5)], dtype=float)


def spectra(x, rate):
    window = np.hanning(N)
    starts = range(0, len(x) - N, HOP)
    frames = np.array([np.abs(np.fft.rfft(x[s:s + N] * window)) for s in starts])
    db = 20 * np.log10(2 * frames / window.sum() + 1e-9)
    return db, np.fft.rfftfreq(N, 1 / rate), np.array([(s + N / 2) / rate for s in starts])


def level(spectrum, b0, b1):
    """the level between bin b0 and bin b1 (not whole numbers), as the widget:
    low down, where a pixel is less than a bin, between the two nearest
    bins; high up the loudest of the bins it covers"""
    if b1 - b0 < 1:
        b = (b0 + b1) / 2
        i = int(b)
        t = b - i
        return spectrum[..., i] * (1 - t) + spectrum[..., min(i + 1, spectrum.shape[-1] - 1)] * t
    return spectrum[..., int(b0):int(np.ceil(b1)) + 1].max(axis=-1)


def band(i, count, top_down=False):
    """the bins (as fractions) that pixel i of count covers on the log scale"""
    u0, u1 = (count - i - 1) / count if top_down else i / count, (count - i) / count if top_down else (i + 1) / count
    return F0 * (F1 / F0) ** u0 / BIN, F0 * (F1 / F0) ** u1 / BIN


def main():
    with wave.open(SOURCE, 'rb') as f:
        rate = f.getframerate()
        x = np.frombuffer(f.readframes(f.getnframes()), dtype='<i2') / 32768
    db, freqs, times = spectra(x, rate)
    gw, gh = (GRAM[2] - GRAM[0]) * S, (GRAM[3] - GRAM[1]) * S
    columns = np.linspace(0, len(db) - 1, gw).round().astype(int)
    picked = db[columns]
    levels = np.array([level(picked, *band(y, gh, top_down=True)) for y in range(gh)])
    v = np.clip((levels - FLOOR) / (TOP - FLOOR), 0, 1) ** 2
    frame = int(np.argmin(np.abs(times - AT)))
    for suffix, theme in THEMES.items():
        ground, hot = rgb(theme['ground']), rgb(theme['hot'])
        gram = (ground + (hot - ground) * v[..., None]).astype(np.uint8)
        picture = Image.new('RGB', (W * S, H * S), theme['back'])
        picture.paste(Image.fromarray(gram, 'RGB'), (GRAM[0] * S, GRAM[1] * S))
        draw = ImageDraw.Draw(picture, 'RGBA')
        x_at = GRAM[0] * S + AT / (len(x) / rate) * gw
        draw.line([(x_at, GRAM[1] * S), (x_at, GRAM[3] * S)], fill=theme['mark'], width=3 * S)
        # the spectrum at the moment: per column of the chart the loudest bin it covers
        left, top, right, bottom = (c * S for c in CHART)
        draw.rectangle([left, top, right, bottom], fill=theme['ground'])
        for d in (-20, -40, -60, -80):
            y = top + (d / FLOOR) * (bottom - top)
            draw.line([(left, y), (right, y)], fill=theme['line'], width=S)
        points = [(left, bottom)]
        for px in range(right - left + 1):
            d = float(level(db[frame], *band(px, right - left)))
            points.append((left + px, top + (min(0, max(FLOOR, d)) / FLOOR) * (bottom - top)))
        points.append((right, bottom))
        mark = tuple(int(c) for c in rgb(theme['mark']))
        draw.polygon(points, fill=mark + (70,))
        draw.line(points[1:-1], fill=mark + (255,), width=3 * S, joint='curve')
        picture = picture.resize((W, H), Image.LANCZOS)
        picture.save(os.path.join(OUT, f'teaser{suffix}.webp'), quality=88)
        print(f'teaser{suffix}.webp')


main()
