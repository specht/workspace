"""Aliasing: a tone of 5.000 Hz measured 8.000 times a second, one
millisecond long. The tone thin and dark, its eight samples as stems with
dots, and through exactly the same dots a tone of 3.000 Hz, dashed purple,
upside down (sin(2π · 5.000 · n / 8.000) = −sin(2π · 3.000 · n / 8.000)):
from the samples alone the two cannot be told apart. Wide for a desktop,
narrow for a phone (the same, on a narrower axis).

    src/scripts/drawings python3 src/content/audio-compression/bilder/aliasing.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/aliasing.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/aliasing-wide.svg
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#7c3aed'
RATE, HIGH, LOW, SAMPLES = 8000, 5000, 3000, 8          # one millisecond


def curve(f, sign, x, y, steps=400):
    points = []
    for i in range(steps + 1):
        t = i / steps * SAMPLES / RATE
        points.append(f'{x(t):.1f},{y(sign * math.sin(2 * math.pi * f * t)):.1f}')
    return ' '.join(points)


def picture(name, width, size, legend_lines):
    left, right = 20, width - 20
    top = 20 + legend_lines * (size + 8) + 14
    amp = 70
    mid = top + amp + 6
    bottom = mid + amp + 6
    x = lambda t: left + (right - left) * t / (SAMPLES / RATE)
    y = lambda v: mid - amp * v
    parts = []
    # the legend: what is dark, what is purple
    for k, (label, colour, dash) in enumerate([('5.000 Hz, 8.000-mal je Sekunde gemessen', INK, ''),
                                               ('3.000 Hz durch dieselben Punkte', PURPLE, " stroke-dasharray='7 5'")]):
        ly = 20 + k * (size + 8) + size * 0.65
        parts.append(f"<line x1='{left}' y1='{ly - 4}' x2='{left + 26}' y2='{ly - 4}' stroke='{colour}' stroke-width='2.5'{dash}/>")
        parts.append(ap.text(left + 34, ly, label, size, colour, 'start'))
    # the axis and the time
    parts.append(f"<line x1='{left}' y1='{mid}' x2='{right}' y2='{mid}' stroke='{LINE}' stroke-width='1.5'/>")
    for t, label in [(0, '0'), (0.5e-3, '0,5 ms'), (1e-3, '1 ms')]:
        anchor = 'start' if t == 0 else 'end' if t == 1e-3 else 'middle'
        parts.append(ap.text(x(t), bottom + size + 6, label, size, GREY, anchor))
    # the two tones
    parts.append(f"<polyline points='{curve(HIGH, 1, x, y)}' fill='none' stroke='{INK}' stroke-width='1.6'/>")
    parts.append(f"<polyline points='{curve(LOW, -1, x, y)}' fill='none' stroke='{PURPLE}' stroke-width='2.5' stroke-dasharray='7 5'/>")
    # the samples
    for n in range(SAMPLES):
        t = n / RATE
        v = math.sin(2 * math.pi * HIGH * t)
        parts.append(f"<line x1='{x(t):.1f}' y1='{mid}' x2='{x(t):.1f}' y2='{y(v):.1f}' stroke='{INK}' stroke-width='2'/>")
        parts.append(f"<circle cx='{x(t):.1f}' cy='{y(v):.1f}' r='6' fill='{INK}'/>")
    height = bottom + size + 16
    head = ap.svg_head(width, round(height), 'Aliasing: ein Ton mit 5.000 Hz, 8.000-mal in der Sekunde gemessen, und ein Ton mit 3.000 Hz durch dieselben Samples; gemacht von bilder/aliasing.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/audio-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


picture('aliasing-wide', 960, 17, 2)
picture('aliasing', 440, 15, 2)
