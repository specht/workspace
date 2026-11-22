"""Runs in a text (»Lauflängen«, the page's example): WWWWWWBBBWWWW as a row
of characters, each run under a bracket with its count and character, and
the code they make together, 6W3B4W.

    python3 src/content/rle/bilder/laeufe.py
    ruby src/scripts/render-svg.rb src/content/rle/laeufe.svg
"""

import os
import sys
from itertools import groupby

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import code  # noqa: E402

INK, GREY, BLUE, AMBER, WHITE, PALE, PALE_AMBER = '#1f2937', '#6b7280', '#2b5fb4', '#d97706', '#ffffff', '#eef3fb', '#fdecc8'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
TEXT = 'WWWWWWBBBWWWW'
CELL, X0 = 40, 30
W = X0 * 2 + CELL * len(TEXT)
H = 196

s = []
x = X0
runs = [(ch, len(list(g))) for ch, g in groupby(TEXT)]
for k, (ch, n) in enumerate(runs):
    color, fill = (BLUE, PALE) if k % 2 == 0 else (AMBER, PALE_AMBER)
    for i in range(n):
        s.append(f'<rect x="{x + i * CELL + 1.5}" y="20" width="{CELL - 3}" height="44" rx="5" fill="{fill}" stroke="{color}" stroke-width="1.8"/>')
        s.append(code(ch, x + i * CELL + CELL / 2, 49, size=18, anchor='middle', background=False))
    # the bracket under the run and its code
    x1, x2, y = x + 4, x + n * CELL - 4, 78
    s.append(f'<path d="M{x1},{y} L{x1},{y + 10} L{x2},{y + 10} L{x2},{y}" fill="none" stroke="{color}" stroke-width="2"/>')
    s.append(code(f'{n}{ch}', (x1 + x2) / 2, y + 38, size=20, anchor='middle', background=False))
    x += n * CELL
coded = ''.join(f'{n}{ch}' for ch, n in runs)
s.append(f'<text x="{X0}" y="{H - 22}" font-size="16" fill="{GREY}">zusammen:</text>')
s.append(code(coded, X0 + 92, H - 22, size=18, anchor='start', background=False))

with open(os.path.join(HERE, '..', 'laeufe.svg'), 'w') as f:
    f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" font-family="{FONT}">\n'
            '  <!-- Die Läufe in WWWWWWBBBWWWW und ihr Code.\n'
            '       Gemacht von bilder/laeufe.py; die dunkle Fassung laeufe-dark.svg\n'
            '       und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n  ' + '\n  '.join(s) + '\n</svg>\n')
print('laeufe')
