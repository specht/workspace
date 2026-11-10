"""The picture for the card of »Audio komprimieren«: a wave measured at
equal steps, each measurement rounded to one of seven levels (3 bits, the
code of each level at the left), and under it the bits that are stored.

    src/scripts/drawings python3 src/content/audio-compression/bilder/teaser.py
    src/scripts/drawings ruby src/scripts/render-svg.rb --webp src/content/audio-compression/teaser.svg
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
MONO = 'IBM Plex Mono, DejaVu Sans Mono, monospace'
INK, GREY, LINE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#7c3aed'
W, H = 1600, 900
LEFT, RIGHT = 190, 1540
MID, STEP = 390, 92                  # the level 0 and the distance between levels
COUNT = 14                           # measurements


def wave(u):
    return 0.78 * math.sin(2 * math.pi * 1.25 * u + 0.3) + 0.22 * math.sin(2 * math.pi * 4.5 * u + 1.1)


def code(k):
    return format(k & 0b111, '03b')       # two's complement in 3 bits


def text(x, y, s, size, color, anchor='middle', weight=None):
    w = f" font-weight='{weight}'" if weight else ''
    return (f"<text x='{x:.1f}' y='{y:.1f}' font-family='{MONO}' font-size='{size}' fill='{color}' "
            f"text-anchor='{anchor}'{w}>{s}</text>")


def main():
    x = lambda u: LEFT + (RIGHT - LEFT) * u
    y = lambda v: MID - 3 * STEP * 0.97 * v
    parts = []
    for k in range(-3, 4):
        ly = MID - k * STEP
        parts.append(f"<line x1='{LEFT - 20}' y1='{ly}' x2='{RIGHT + 20}' y2='{ly}' stroke='{LINE}' stroke-width='2'/>")
        parts.append(text(LEFT - 40, ly + 11, code(k), 32, GREY, 'end'))
    points = ' '.join(f'{x(i / 800):.1f},{y(wave(i / 800)):.1f}' for i in range(801))
    parts.append(f"<polyline points='{points}' fill='none' stroke='{GREY}' stroke-width='4'/>")
    bits = []
    for i in range(COUNT):
        u = (i + 0.5) / COUNT
        k = max(-3, min(3, round(wave(u) * 3 * 0.97)))
        dot = MID - k * STEP
        parts.append(f"<line x1='{x(u):.1f}' y1='{MID}' x2='{x(u):.1f}' y2='{dot}' stroke='{PURPLE}' stroke-width='5'/>")
        parts.append(f"<circle cx='{x(u):.1f}' cy='{dot}' r='15' fill='{PURPLE}'/>")
        bits.append(text(x(u), 805, code(k), 38, PURPLE, weight=600))
    parts += bits
    head = ap.svg_head(W, H, 'Die Karte: eine Welle, gemessen und auf sieben Stufen gerundet; gemacht von bilder/teaser.py, als Bild:',
                       'src/scripts/render-svg.rb (mit der Option webp) src/content/audio-compression/teaser.svg')
    with open(os.path.join(OUT, 'teaser.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


main()
