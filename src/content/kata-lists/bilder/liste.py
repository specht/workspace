"""A list and its indices (»Listen«, the page's example): [3, 4, 5] with the
indices 0, 1, 2 above, and below it the new list doubled makes of it,
[6, 8, 10], each element doubled; the old one stays as it is.

    python3 src/content/kata-lists/bilder/liste.py
    ruby src/scripts/render-svg.rb src/content/kata-lists/liste.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import code  # noqa: E402

INK, GREY, BLUE, AMBER, WHITE, PALE = '#1f2937', '#6b7280', '#2b5fb4', '#d97706', '#ffffff', '#eef3fb'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
CELL, X0 = 64, 150
W, H = 520, 232

s = []


def text(x, y, t, size=16, color=INK, anchor='middle', weight=400):
    s.append(f'<text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" text-anchor="{anchor}" font-weight="{weight}">{t}</text>')


def row(y, values, color, name):
    s.append(code(name, X0 - 18, y + 27, size=16, anchor='end', background=False))
    for i, v in enumerate(values):
        x = X0 + i * CELL
        s.append(f'<rect x="{x + 2}" y="{y}" width="{CELL - 4}" height="42" rx="6" fill="{PALE}" stroke="{color}" stroke-width="2"/>')
        text(x + CELL / 2, y + 28, v, 19, INK)


for i in range(3):
    text(X0 + i * CELL + CELL / 2, 28, f'Index {i}' if i == 0 else i, 14, GREY)
row(40, [3, 4, 5], BLUE, 'numbers')
row(160, [6, 8, 10], AMBER, 'result')
for i in range(3):
    x = X0 + i * CELL + CELL / 2
    s.append(f'<line x1="{x}" y1="88" x2="{x}" y2="146" stroke="{GREY}" stroke-width="1.8" marker-end="url(#head)"/>')
    text(x + 8, 124, '· 2', 15, GREY, 'start')
text(X0 + 3 * CELL + 20, 67, 'bleibt, wie sie ist', 14, GREY, 'start')
text(X0 + 3 * CELL + 20, 187, 'eine neue Liste', 14, GREY, 'start')

head = (f'<marker id="head" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto">'
        f'<path d="M0,0 L10,5 L0,10 z" fill="{GREY}"/></marker>')
with open(os.path.join(HERE, '..', 'liste.svg'), 'w') as f:
    f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" font-family="{FONT}">\n'
            '  <!-- Eine Liste mit ihren Indizes und die neue Liste daraus.\n'
            '       Gemacht von bilder/liste.py; die dunkle Fassung liste-dark.svg\n'
            '       und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <defs>{head}</defs>\n'
            f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n  ' + '\n  '.join(s) + '\n</svg>\n')
print('liste')
