"""The bands of the codec (»Bänder«): the 1.024 numbers of an MDCT block as
one bar, frequency to the right (0 to 22.050 Hz), split into the bands of
make_bands in codec.py, one for each Bark, at least four numbers wide, in
alternating blue and purple, with the Hz under it. The bands at the bottom
are so narrow that a second bar magnifies the first 91 numbers, the first
13 bands (up to about 1.960 Hz), each band with how many numbers it has. Computed with the page's
formulas (bark, FREQS, make_bands). Wide for a desktop, narrow for a phone.

    src/scripts/drawings python3 src/content/audio-compression/bilder/bands.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/bands.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/bands-wide.svg
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, BLUE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#2b5fb4', '#7c3aed'
N, RATE = 1024, 44100
ZOOM = 91                      # the first 13 bands, up to a band edge


def bark(f):
    return 13 * math.atan(0.00076 * f) + 3.5 * math.atan((f / 7500) ** 2)


FREQS = [(k + 0.5) * RATE / (2 * N) for k in range(N)]


def make_bands():
    edges = [0]
    barks = [bark(f) for f in FREQS]
    for z in range(1, 26):
        k = next((i for i, b in enumerate(barks) if b >= z), N)    # np.searchsorted
        if k - edges[-1] >= 4 and k < N:
            edges.append(k)
    edges.append(N)
    return list(zip(edges[:-1], edges[1:]))


BANDS = make_bands()


def de(n):
    return f'{n:,}'.replace(',', '.')


def bar(parts, left, width, y, h, lo, hi, size, numbers):
    """the numbers lo to hi as a bar of width, split into the bands"""
    x = lambda k: left + width * (k - lo) / (hi - lo)
    for i, (a, b) in enumerate(BANDS):
        if b <= lo or a >= hi:
            continue
        a, b = max(a, lo), min(b, hi)
        colour = BLUE if i % 2 == 0 else PURPLE
        parts.append(f"<rect x='{x(a):.1f}' y='{y}' width='{x(b) - x(a):.1f}' height='{h}' fill='{colour}' fill-opacity='0.18' stroke='{colour}' stroke-width='1.2'/>")
        if numbers and x(b) - x(a) > size * 1.4:
            parts.append(ap.text((x(a) + x(b)) / 2, y + h / 2 + size * 0.35, str(b - a), size, colour))
    return x


def picture(name, width, size, zoom_width):
    left, right = 20, width - 20
    w = right - left
    parts = [ap.text(left, 18, f'die {de(N)} Zahlen eines Blocks, in {len(BANDS)} Bänder geteilt', size, GREY, 'start')]
    y1, h = 30, 40
    x = bar(parts, left, w, y1, h, 0, N, size, True)
    for f in (0, 5000, 10000, 15000, 20000):
        k = f / (RATE / 2) * N
        parts.append(f"<line x1='{x(k):.1f}' y1='{y1 + h}' x2='{x(k):.1f}' y2='{y1 + h + 6}' stroke='{GREY}' stroke-width='1.5'/>")
        anchor = 'start' if f == 0 else 'middle'
        parts.append(ap.text(x(k), y1 + h + 8 + size, f'{de(f)} Hz', size, GREY, anchor))
    # the magnified start, with lines from its piece of the bar, below the labels
    below = y1 + h + 8 + size + 6
    y2 = below + 34
    zx = bar(parts, left, zoom_width, y2, h, 0, ZOOM, size, True)
    parts.append(f"<path d='M {x(0):.1f} {below} L {left:.1f} {y2 - 3} M {x(ZOOM):.1f} {below} L {left + zoom_width:.1f} {y2 - 3}' fill='none' stroke='{LINE}' stroke-width='1.5' stroke-dasharray='4 4'/>")
    end_hz = int(round(ZOOM * RATE / (2 * N), -1))
    base = y2 + h + 8 + size
    parts.append(ap.text(left, base, '0 Hz', size, GREY, 'start'))
    parts.append(ap.text(left + zoom_width / 2, base, f'die ersten {ZOOM} Zahlen vergrößert', size, GREY))
    parts.append(ap.text(left + zoom_width, base, f'{de(end_hz)} Hz', size, GREY, 'end'))
    height = y2 + h + 8 + size + 12
    head = ap.svg_head(width, round(height), 'Die 1.024 Zahlen eines Blocks in den Bändern des Codecs; gemacht von bilder/bands.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/audio-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


print(len(BANDS), [b - a for a, b in BANDS])
picture('bands-wide', 960, 15, 920)
picture('bands', 440, 13, 400)
