"""Masking in time (»Ein lauter Klang verdeckt einen leisen«), schematic: a
loud beat at 0 ms as a bar, the threshold in quiet dashed, and the masking
threshold around the beat: it rises steeply in the 20 ms before it (Vor-
verdeckung) and sinks slowly in the 100 ms after it (Nachverdeckung), as
the page says. No dB on the axis: the shape matters, not the numbers.
Wide for a desktop, narrow for a phone.

    src/scripts/drawings python3 src/content/audio-compression/bilder/time_masking.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/time-masking.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/time-masking-wide.svg
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#7c3aed'
T0, T1 = -50, 200              # ms on the axis
BEAT = 10                      # how long the beat lasts, ms


def threshold(t):
    """0 in quiet, 1 during the beat: steep before, slow after (schematic)"""
    if t < -20:
        return 0
    if t < 0:
        return ((t + 20) / 20) ** 3 * 0.85
    if t <= BEAT:
        return 1
    return 0.85 * math.exp(-(t - BEAT) / 35)


def picture(name, width, size):
    left, right, top, bottom = 20, width - 20, 30 + 2 * size, 210
    x = lambda t: left + (right - left) * (t - T0) / (T1 - T0)
    base = bottom - 16
    y = lambda v: base - (base - top - 20) * v
    parts = [f"<line x1='{left}' y1='{bottom}' x2='{right}' y2='{bottom}' stroke='{LINE}' stroke-width='1.5'/>"]
    # the beat
    parts.append(f"<rect x='{x(0):.1f}' y='{top}' width='{x(BEAT) - x(0):.1f}' height='{bottom - top}' fill='{INK}' fill-opacity='0.85'/>")
    parts.append(ap.text(x(BEAT) + 6, top + size, 'lauter Schlag', size, INK, 'start'))
    # the threshold in quiet and the masking threshold
    parts.append(f"<line x1='{left}' y1='{base}' x2='{right}' y2='{base}' stroke='{GREY}' stroke-width='1.5' stroke-dasharray='6 5'/>")
    parts.append(ap.text(right, base + size - 1, 'Hörschwelle in der Stille', size - 1, GREY, 'end'))
    points = ' '.join(f'{x(t):.1f},{y(threshold(t)):.1f}' for t in range(T0, T1 + 1))
    parts.append(f"<polyline points='{points}' fill='{PURPLE}' fill-opacity='0.1' stroke='{PURPLE}' stroke-width='2.5'/>")
    # the two spans under the axis
    for t0, t1, label in ((-20, 0, 'etwa 20 ms davor'), (BEAT, BEAT + 100, 'etwa 100 ms danach')):
        yb = bottom + 12
        parts.append(f"<path d='M {x(t0):.1f} {yb - 5} L {x(t0):.1f} {yb} L {x(t1):.1f} {yb} L {x(t1):.1f} {yb - 5}' fill='none' stroke='{PURPLE}' stroke-width='1.5'/>")
        if t0 >= 0:
            # on a phone a line below the other label
            parts.append(ap.text((x(t0) + x(t1)) / 2, yb + size + 4 + (size + 6 if width < 600 else 0), label, size, PURPLE))
        elif width < 600:
            parts.append(ap.text(left, yb + size + 4, label, size, PURPLE, 'start'))
        else:
            parts.append(ap.text(x(t1), yb + size + 4, label, size, PURPLE, 'end'))
    parts.append(ap.text(left, 16 + size * 0.3, 'Mithörschwelle um einen lauten Schlag (schematisch)', size, PURPLE, 'start'))
    height = bottom + 12 + size + 14 + (size + 6 if width < 600 else 0)
    head = ap.svg_head(width, round(height), 'Verdeckung in der Zeit: die Mithörschwelle vor und nach einem lauten Schlag; gemacht von bilder/time_masking.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/audio-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


picture('time-masking-wide', 760, 15)
picture('time-masking', 440, 13)
