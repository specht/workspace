"""The levels of 4 bits, the positive half from 0 to 1: even ones above, those
of μ-law below, and the range of a quiet voice (up to 0,06) as a pale band,
in which the even levels have only 0 and μ-law has four.

    src/scripts/drawings python3 src/content/audio-compression/bilder/mulaw-stufen.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/mulaw-stufen.svg
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, PALE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#f3f4f6', '#7c3aed'
W, H = 560, 262
LEFT, RIGHT = 24, 536
MU, TOP = 255, 7                     # 4 bits: the levels 0 to 7 on the positive side
QUIET = 0.06
ROWS = [('gleichmäßig', 92, lambda k: k / TOP),
        ('μ-law', 192, lambda k: ((1 + MU) ** (k / TOP) - 1) / MU)]


def main():
    x = lambda v: LEFT + (RIGHT - LEFT) * v
    parts = [f"<rect x='{x(0):.1f}' y='44' width='{x(QUIET) - x(0):.1f}' height='172' fill='{PALE}'/>",
             ap.text(x(0), 30, 'leise Stimme: bis 0,06', 18, GREY, 'start')]
    for name, y, level in ROWS:
        parts.append(ap.text(x(QUIET) + 12, y - 22, name, 18, INK, 'start'))
        parts.append(f"<line x1='{x(0)}' y1='{y}' x2='{x(1)}' y2='{y}' stroke='{LINE}' stroke-width='2'/>")
        for k in range(TOP + 1):
            lx = x(level(k))
            parts.append(f"<line x1='{lx:.1f}' y1='{y - 12}' x2='{lx:.1f}' y2='{y + 12}' stroke='{PURPLE}' stroke-width='2.5'/>")
        for v, label in [(0, '0'), (0.5, '0,5'), (1, '1')]:
            anchor = 'start' if v == 0 else 'end' if v == 1 else 'middle'
            parts.append(ap.text(x(v), y + 34, label, 18, GREY, anchor))
    head = ap.svg_head(W, H, 'Die Stufen von 4 Bit, gleichmäßig und nach μ-law; gemacht von bilder/mulaw-stufen.py, hell und dunkel:',
                       'src/scripts/render-svg.rb src/content/audio-compression/mulaw-stufen.svg')
    with open(os.path.join(OUT, 'mulaw-stufen.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


main()
