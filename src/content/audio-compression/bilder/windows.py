"""Overlapping windows (»Blöcke und Fenster«), two drawings from one script.

windows: four blocks of 2N samples, each half over the next, each with the
sine window w_n = sin(π/(2N) · (n + 1/2)) as a hump that rises from 0 and
falls back to 0, in alternating blue and purple; under them the halves of
N samples, so one sees that every sample lies in two blocks, one fading
out while the next fades in.

window-squares: the same windows squared, w², dashed, and their sum, a
solid line: wherever two overlap it is exactly 1 (Princen and Bradley's
condition), so a block faded out and the next faded in give the sound
back unchanged.

Wide for a desktop, narrow for a phone (the same, narrower).

    src/scripts/drawings python3 src/content/audio-compression/bilder/windows.py
    for f in windows windows-wide window-squares window-squares-wide; do
        src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/$f.svg; done
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, BLUE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#2b5fb4', '#7c3aed'
BLOCKS = 4                     # windows; they cover BLOCKS + 1 halves
STEPS = 200


def window(t):
    """the sine window over one block, t from 0 to 1"""
    return math.sin(math.pi * t)


def frame(width, size, height):
    left, right = 20, width - 20
    half = (right - left) / (BLOCKS + 1)
    x = lambda block, t: left + half * (block + 2 * t)      # t from 0 to 1 over a block of two halves
    return left, right, half, x


def windows(name, width, size, title):
    top, h = 30 + 2 * size, 120
    left, right, half, x = frame(width, size, h)
    base = top + h
    y = lambda v: base - h * v
    parts = [ap.text(left, 18, title, size, GREY, 'start'),
             f"<line x1='{left}' y1='{base}' x2='{right}' y2='{base}' stroke='{LINE}' stroke-width='1.5'/>"]
    for b in range(BLOCKS):
        colour = BLUE if b % 2 == 0 else PURPLE
        points = ' '.join(f'{x(b, i / STEPS):.1f},{y(window(i / STEPS)):.1f}' for i in range(STEPS + 1))
        parts.append(f"<polyline points='{points}' fill='{colour}' fill-opacity='0.08' stroke='{colour}' stroke-width='2.5'/>")
        parts.append(ap.text(x(b, 0.5), y(1) - 8, f'Block {b + 1}', size, colour))
    # the halves of N samples under the axis
    for k in range(BLOCKS + 2):
        xx = left + half * k
        parts.append(f"<line x1='{xx:.1f}' y1='{base - 6}' x2='{xx:.1f}' y2='{base + 6}' stroke='{GREY}' stroke-width='1.5'/>")
    for k in range(BLOCKS + 1):
        parts.append(ap.text(left + half * (k + 0.5), base + size + 8, 'N', size, GREY))
    save(name, width, base + size + 18, parts, 'Vier Fenster über Blöcken, die sich zur Hälfte überlappen')


def squares(name, width, size, label):
    top, h = 40 + 2 * size, 110
    left, right, half, x = frame(width, size, h)
    base = top + h
    y = lambda v: base - h * v
    parts = [ap.text(left, 18, 'die Quadrate der Fenster und ihre Summe', size, GREY, 'start'),
             f"<line x1='{left}' y1='{base}' x2='{right}' y2='{base}' stroke='{LINE}' stroke-width='1.5'/>"]
    for b in range(BLOCKS):
        colour = BLUE if b % 2 == 0 else PURPLE
        points = ' '.join(f'{x(b, i / STEPS):.1f},{y(window(i / STEPS) ** 2):.1f}' for i in range(STEPS + 1))
        parts.append(f"<polyline points='{points}' fill='none' stroke='{colour}' stroke-width='2' stroke-dasharray='6 4'/>")
    # the sum of the squares, from the first half to the last
    total = []
    for i in range(STEPS * (BLOCKS + 1) // 2 + 1):
        pos = i / (STEPS / 2)                       # in halves
        v = 0
        for b in range(BLOCKS):
            t = (pos - b) / 2
            if 0 <= t <= 1:
                v += window(t) ** 2
        total.append(f'{left + half * pos:.1f},{y(v):.1f}')
    parts.append(f"<polyline points='{' '.join(total)}' fill='none' stroke='{INK}' stroke-width='2.5'/>")
    parts.append(ap.text(left + half * (BLOCKS + 1) / 2, y(1) - 10, label, size, INK))
    save(name, width, base + 14, parts, 'Die Quadrate der Fenster summieren sich zu 1')


def save(name, w, h, parts, what):
    head = ap.svg_head(w, round(h), f'{what}; gemacht von bilder/windows.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/audio-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


windows('windows-wide', 960, 16, 'Fenster über Blöcken aus 2N Samples, jeder halb über dem nächsten')
windows('windows', 440, 13, 'Fenster über Blöcken aus 2N Samples')
squares('window-squares-wide', 960, 16, 'Summe: überall, wo zwei sich überlappen, genau 1')
squares('window-squares', 440, 13, 'Summe: wo zwei sich überlappen, genau 1')
