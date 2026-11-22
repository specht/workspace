"""Keys and values (»Dictionaries«, the page's example): Ada → 36, Alan → 41
and the new pair Grace → 85; Linus is no key, looking him up finds nothing.

    python3 src/content/kata-dicts/bilder/paare.py
    ruby src/scripts/render-svg.rb src/content/kata-dicts/paare.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import code  # noqa: E402

INK, GREY, BLUE, AMBER, WHITE, PALE, PALE_AMBER = '#1f2937', '#6b7280', '#2b5fb4', '#d97706', '#ffffff', '#eef3fb', '#fdecc8'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
W, H = 540, 290

s = []


def text(x, y, t, size=16, color=INK, anchor='middle', weight=400):
    s.append(f'<text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" text-anchor="{anchor}" font-weight="{weight}">{t}</text>')


def box(x, y, w, label, color, fill, dashed=False):
    dash = ' stroke-dasharray="6 4"' if dashed else ''
    s.append(f'<rect x="{x}" y="{y}" width="{w}" height="40" rx="7" fill="{fill}" stroke="{color}" stroke-width="2"{dash}/>')
    s.append(code(label, x + w / 2, y + 26, size=16, anchor='middle', background=False))


text(110, 30, 'Schlüssel', 16, GREY, weight=600)
text(390, 30, 'Wert', 16, GREY, weight=600)
pairs = [('Ada', '36', False), ('Alan', '41', False), ('Grace', '85', True)]
for i, (key, value, new) in enumerate(pairs):
    y = 48 + i * 58
    box(40, y, 140, key, BLUE, PALE)
    box(340, y, 100, value, AMBER, PALE_AMBER)
    s.append(f'<line x1="186" y1="{y + 20}" x2="330" y2="{y + 20}" stroke="{GREY}" stroke-width="1.8" marker-end="url(#head)"/>')
    if new:
        text(258, y + 13, 'neu dazu', 13, GREY)
y = 48 + 3 * 58 + 8
box(40, y, 140, 'Linus', GREY, WHITE, dashed=True)
text(200, y + 26, 'kein Schlüssel: nichts zum Nachschlagen', 14, GREY, 'start')

head = (f'<marker id="head" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto">'
        f'<path d="M0,0 L10,5 L0,10 z" fill="{GREY}"/></marker>')
with open(os.path.join(HERE, '..', 'paare.svg'), 'w') as f:
    f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" font-family="{FONT}">\n'
            '  <!-- Schlüssel und Werte eines Dictionarys.\n'
            '       Gemacht von bilder/paare.py; die dunkle Fassung paare-dark.svg\n'
            '       und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <defs>{head}</defs>\n'
            f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n  ' + '\n  '.join(s) + '\n</svg>\n')
print('paare')
