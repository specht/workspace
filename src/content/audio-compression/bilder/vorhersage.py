"""Eight samples of the voice (from index 56.019) and how well they are
guessed: as dots rising almost on a straight line, for the third the line
through the two before it, the estimate on it (a ring) and the remainder of
11 between estimate and sample; under them the numbers, right-aligned: the
samples, minus the one before, minus the line. Wide for a desktop (the
numbers in columns under the dots), narrow for a phone (a row per sample).

    src/scripts/drawings python3 src/content/audio-compression/bilder/vorhersage.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/vorhersage.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/vorhersage-wide.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#7c3aed'
SAMPLES = [6445, 6866, 7298, 7730, 8163, 8600, 9040, 9481]
DIFFS = [None] + [b - a for a, b in zip(SAMPLES, SAMPLES[1:])]
RESTS = [None, None] + [c - (2 * b - a) for a, b, c in zip(SAMPLES, SAMPLES[1:], SAMPLES[2:])]
GUESS = 2 * SAMPLES[1] - SAMPLES[0]                 # the estimate for the third, 7.287
ROWS = [('Sample', SAMPLES, INK), ('minus das vorige', DIFFS, INK), ('minus die Gerade', RESTS, PURPLE)]


def de(n):
    return f'{n:,}'.replace(',', '.') if n is not None else ''


def plot(xs, top, bottom):
    """the dots, the line through the first two to the third, the ring of the
    estimate and the remainder; xs: the x of each sample"""
    lo, hi = SAMPLES[0] - 250, SAMPLES[-1] + 150
    y = lambda v: bottom - (bottom - top) * (v - lo) / (hi - lo)
    parts = [f"<polyline points='{' '.join(f'{x:.1f},{y(v):.1f}' for x, v in zip(xs, SAMPLES))}' fill='none' stroke='{LINE}' stroke-width='2'/>"]
    # the line through samples 0 and 1, on to the estimate for sample 2
    x_far = xs[2] + (xs[2] - xs[1]) * 0.35
    v_far = SAMPLES[1] + (SAMPLES[1] - SAMPLES[0]) * (x_far - xs[1]) / (xs[1] - xs[0])
    parts.append(f"<line x1='{xs[0]:.1f}' y1='{y(SAMPLES[0]):.1f}' x2='{x_far:.1f}' y2='{y(v_far):.1f}' stroke='{PURPLE}' stroke-width='2.5' stroke-dasharray='7 6'/>")
    for x, v in zip(xs, SAMPLES):
        parts.append(f"<circle cx='{x:.1f}' cy='{y(v):.1f}' r='6' fill='{INK}'/>")
    parts.append(f"<circle cx='{xs[2]:.1f}' cy='{y(GUESS):.1f}' r='8' fill='none' stroke='{PURPLE}' stroke-width='2.5'/>")
    return parts, y


def wide():
    W, H = 960, 330
    label_w = 210
    col = (W - label_w - 10) / 8
    right = [label_w + col * (i + 1) - 14 for i in range(8)]      # where each column's numbers end
    xs = [r - 27 for r in right]                                    # the dot over the middle of its number
    parts, y = plot(xs, 30, 165)
    parts.append(ap.text(xs[2] + 16, y(GUESS) + 26, 'geschätzt 7.287, Rest 11', 17, PURPLE, 'start'))
    for r, (label, values, color) in enumerate(ROWS):
        base = 215 + r * 40
        parts.append(ap.text(10, base, label, 17, GREY, 'start'))
        parts.append(f"<line x1='10' y1='{base + 13}' x2='{W - 10}' y2='{base + 13}' stroke='{LINE}' stroke-width='1'/>")
        for x, v in zip(right, values):
            parts.append(ap.text(x, base, de(v), 18, color, 'end'))
    save('vorhersage-wide', W, H, parts)


def narrow():
    W = 440
    xs = [40 + (W - 80) * i / 7 for i in range(8)]
    parts, y = plot(xs, 20, 170)
    parts.append(ap.text(xs[2] + 14, y(GUESS) + 26, 'Rest 11', 16, PURPLE, 'start'))
    columns = [(150, 'Sample'), (285, 'minus das|vorige'), (420, 'minus die|Gerade')]
    top = 215
    for x, label in columns:
        for k, part in enumerate(label.split('|')):
            parts.append(ap.text(x, top + k * 20, part, 15, GREY, 'end'))
    parts.append(f"<line x1='10' y1='{top + 32}' x2='{W - 10}' y2='{top + 32}' stroke='{LINE}' stroke-width='1'/>")
    for i in range(8):
        base = top + 60 + i * 30
        for (x, _), (_, values, color) in zip(columns, ROWS):
            parts.append(ap.text(x, base, de(values[i]), 17, color, 'end'))
    save('vorhersage', W, top + 60 + 8 * 30, parts)


def save(name, w, h, parts):
    head = ap.svg_head(w, h, 'Acht Samples der Stimme und wie gut die Gerade sie rät; gemacht von bilder/vorhersage.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/audio-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


wide()
narrow()
