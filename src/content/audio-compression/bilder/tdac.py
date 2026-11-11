"""How two blocks of the MDCT give the signal back: the first block after
imdct and the window (its second half has the mirror image of the signal
added), the second block (its first half has it subtracted), and their sum,
the signal again. Computed with the functions of stufen/mdct-2.py.

    src/scripts/drawings python3 src/content/audio-compression/bilder/tdac.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/tdac.svg
"""

import os
import runpy
import sys

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
sys.path.insert(0, os.path.join(HERE, '..', 'stufen'))     # wav.py, which mdct-2.py imports
import automaton_pictures as ap  # noqa: E402

code = runpy.run_path(os.path.join(HERE, '..', 'stufen', 'mdct-2.py'), run_name='drawing')
mdct, imdct, window = code['mdct'], code['imdct'], code['window']

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, PALE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#f3f4f6', '#7c3aed'
N = 32
W, ROW, TOP = 560, 112, 10
LEFT, RIGHT = 16, 544
t = np.arange(3 * N)
x = 0.55 * np.sin(2 * np.pi * t / 70 + 0.6) + 0.25 * np.sin(2 * np.pi * t / 23)
w = window(N)
first = w * imdct(mdct(w * x[:2 * N]))
second = w * imdct(mdct(w * x[N:3 * N]))
ROWS = [('Block 1: Fenster, mdct, imdct, Fenster', 0, first),
        ('Block 2, einen halben Block später', N, second),
        ('Summe: in der Mitte wieder das Original', N, (first[N:] + second[:N]))]


def main():
    H = TOP + len(ROWS) * ROW + 8
    xs = lambda i: LEFT + (RIGHT - LEFT) * i / (3 * N - 1)
    parts = [f"<rect x='{xs(N):.1f}' y='{TOP}' width='{xs(2 * N) - xs(N):.1f}' height='{len(ROWS) * ROW}' fill='{PALE}'/>"]
    for r, (label, start, values) in enumerate(ROWS):
        mid = TOP + r * ROW + 66
        ys = lambda v: mid - 40 * v
        parts.append(ap.text(LEFT, TOP + r * ROW + 18, label, 18, INK, 'start'))
        parts.append(f"<line x1='{LEFT}' y1='{mid}' x2='{RIGHT}' y2='{mid}' stroke='{LINE}' stroke-width='1.5'/>")
        original = ' '.join(f'{xs(i):.1f},{ys(x[i]):.1f}' for i in range(3 * N))
        parts.append(f"<polyline points='{original}' fill='none' stroke='{GREY}' stroke-width='2' stroke-dasharray='5 4'/>")
        block = ' '.join(f'{xs(start + i):.1f},{ys(v):.1f}' for i, v in enumerate(values))
        parts.append(f"<polyline points='{block}' fill='none' stroke='{PURPLE}' stroke-width='3'/>")
    head = ap.svg_head(W, H, 'Zwei Blöcke der MDCT und ihre Summe; gemacht von bilder/tdac.py, hell und dunkel:',
                       'src/scripts/render-svg.rb src/content/audio-compression/tdac.svg')
    with open(os.path.join(OUT, 'tdac.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


main()
