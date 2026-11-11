"""How a test tone finds a frequency (»Mit einem Testton vergleichen«): 16
samples of a sound with two oscillations, x_n = sin(2π · 2n / 16), as
stems. Top row: the test tone with the same two oscillations and, sample by
sample, the product of both, never below 0, so the sum is 8. Bottom row: a
test tone with three oscillations; the products are positive and negative
and cancel, the sum is 0. Positive products purple, negative grey. Wide for
a desktop (sound, test tone and product side by side, the sum beside them),
narrow for a phone (the three under each other).

    src/scripts/drawings python3 src/content/audio-compression/bilder/testton.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/testton.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/testton-wide.svg
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#7c3aed'
N = 16
SOUND = [math.sin(2 * math.pi * 2 * n / N) for n in range(N)]


def de(v):
    return f'{v:g}'.replace('.', ',').replace('-', '−')


def panel(x0, y0, w, h, values, label, size, product=False):
    """a row of stems from a middle line, values from −1 to 1, its label above"""
    mid = y0 + size + 10 + h / 2
    parts = [ap.text(x0, y0 + size, label, size, GREY, 'start'),
             f"<line x1='{x0}' y1='{mid:.1f}' x2='{x0 + w}' y2='{mid:.1f}' stroke='{LINE}' stroke-width='1.5'/>"]
    step = w / N
    for n, v in enumerate(values):
        x = x0 + step * (n + 0.5)
        y = mid - v * h / 2
        colour = (PURPLE if v >= 0 else GREY) if product else INK
        if abs(v) > 1e-9:
            parts.append(f"<line x1='{x:.1f}' y1='{mid:.1f}' x2='{x:.1f}' y2='{y:.1f}' stroke='{colour}' stroke-width='2.5'/>")
        parts.append(f"<circle cx='{x:.1f}' cy='{y:.1f}' r='4' fill='{colour}'/>")
    return parts, y0 + size + 10 + h


def rows(k):
    test = [math.sin(2 * math.pi * k * n / N) for n in range(N)]
    product = [a * b for a, b in zip(SOUND, test)]
    total = sum(product)
    total = 0 if abs(total) < 1e-9 else total
    return [('Klang: 2 Schwingungen', SOUND, False),
            (f'Testton: {k} Schwingungen', test, False),
            ('Klang mal Testton', product, True)], total


def wide():
    W, size = 960, 15
    pw, ph, gap = 250, 90, 24
    parts, y = [], 10
    for k in (2, 3):
        panels, total = rows(k)
        bottom = y
        for i, (label, values, product) in enumerate(panels):
            drawn, bottom = panel(10 + i * (pw + gap), y, pw, ph, values, label, size, product)
            parts += drawn
        parts.append(ap.text(10 + 3 * (pw + gap), y + size + 10 + ph / 2 + size * 0.35, f'Summe: {de(round(total, 6))}', size + 2, PURPLE if total else GREY, 'start'))
        y = bottom + 30
    save('testton-wide', W, y - 10, parts)


def narrow():
    W, size = 440, 14
    pw, ph = 420, 70
    parts, y = [], 10
    for k in (2, 3):
        panels, total = rows(k)
        for label, values, product in panels:
            drawn, y = panel(10, y, pw, ph, values, label, size, product)
            parts += drawn
            y += 12
        parts.append(ap.text(10, y + size, f'Summe: {de(round(total, 6))}', size + 1, PURPLE if total else GREY, 'start'))
        y += size + 34
    save('testton', W, y - 20, parts)


def save(name, w, h, parts):
    head = ap.svg_head(w, round(h), 'Ein Klang mal ein Testton derselben und einer anderen Frequenz, Sample für Sample, und die Summe; gemacht von bilder/testton.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/audio-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


wide()
narrow()
