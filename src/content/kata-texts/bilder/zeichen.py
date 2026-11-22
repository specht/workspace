"""A text as a row of characters (»Texte«, the page's example): Hallo, each
character with its index above and its number below (H 72, a 97, l 108,
o 111), and the new text bracket makes of it.

    python3 src/content/kata-texts/bilder/zeichen.py
    ruby src/scripts/render-svg.rb src/content/kata-texts/zeichen.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import code  # noqa: E402

INK, GREY, BLUE, AMBER, WHITE, PALE = '#1f2937', '#6b7280', '#2b5fb4', '#d97706', '#ffffff', '#eef3fb'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
TEXT = 'Hallo'
CELL, X0 = 58, 130
W, H = 560, 210

s = []


def text(x, y, t, size=16, color=INK, anchor='middle', weight=400):
    s.append(f'<text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" text-anchor="{anchor}" font-weight="{weight}">{t}</text>')


text(X0 - 18, 30, 'Index', 14, GREY, 'end')
text(X0 - 18, 70, 'Zeichen', 14, GREY, 'end')
text(X0 - 18, 114, 'Nummer', 14, GREY, 'end')
for i, ch in enumerate(TEXT):
    x = X0 + i * CELL
    text(x + CELL / 2, 30, i, 14, GREY)
    s.append(f'<rect x="{x + 2}" y="{44}" width="{CELL - 4}" height="42" rx="6" fill="{PALE}" stroke="{BLUE}" stroke-width="2"/>')
    s.append(code(ch, x + CELL / 2, 72, size=20, anchor='middle', background=False))
    text(x + CELL / 2, 114, ord(ch), 15, AMBER, weight=600)
text(X0 + len(TEXT) * CELL + 16, 70, f'Länge {len(TEXT)}', 14, GREY, 'start')
result = ''.join(f'[{c}]' for c in TEXT)
text(X0 - 18, 172, 'bracket', 14, GREY, 'end')
s.append(code(result, X0, 176, size=18, anchor='start', background=False))

with open(os.path.join(HERE, '..', 'zeichen.svg'), 'w') as f:
    f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" font-family="{FONT}">\n'
            '  <!-- Der Text Hallo als Reihe von Zeichen mit Index und Nummer.\n'
            '       Gemacht von bilder/zeichen.py; die dunkle Fassung zeichen-dark.svg\n'
            '       und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n  ' + '\n  '.join(s) + '\n</svg>\n')
print('zeichen')
