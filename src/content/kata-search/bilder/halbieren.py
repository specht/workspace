"""Binary search on the page's list (»Suchen«): [2, 5, 8, 12, 16, 23], looking
for 5. Each row is one look: the part still in question between low and
high, the middle marked, and what the look decides. Three looks, then 5 is
found at index 1. No code: the katas ask for it.

    python3 src/content/kata-search/bilder/halbieren.py
    ruby src/scripts/render-svg.rb src/content/kata-search/halbieren.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import code  # noqa: E402

INK, GREY, LINE, BLUE, AMBER, WHITE, PALE, FADE = '#1f2937', '#6b7280', '#d1d5db', '#2b5fb4', '#d97706', '#ffffff', '#eef3fb', '#f3f4f6'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
NUMBERS = [2, 5, 8, 12, 16, 23]
TARGET = 5
CELL, X0 = 56, 40
W = X0 + CELL * len(NUMBERS) + 290

s = []


def text(x, y, t, size=16, color=INK, anchor='middle', weight=400):
    s.append(f'<text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" text-anchor="{anchor}" font-weight="{weight}">{t}</text>')


# the looks, as a binary search makes them
rows = []
low, high = 0, len(NUMBERS) - 1
while low <= high:
    middle = (low + high) // 2
    value = NUMBERS[middle]
    if value == TARGET:
        rows.append((low, high, middle, f'{value} ist {TARGET}: gefunden, Index {middle}'))
        break
    if value < TARGET:
        rows.append((low, high, middle, f'{value} &lt; {TARGET}: rechts weiter'))
        low = middle + 1
    else:
        rows.append((low, high, middle, f'{value} &gt; {TARGET}: links weiter'))
        high = middle - 1

y = 40
for i in range(len(NUMBERS)):
    text(X0 + i * CELL + CELL / 2, y - 12, i, 13, GREY)
for low, high, middle, what in rows:
    for i, n in enumerate(NUMBERS):
        x = X0 + i * CELL
        inside = low <= i <= high
        fill = AMBER if i == middle else (PALE if inside else FADE)
        stroke = AMBER if i == middle else (BLUE if inside else LINE)
        s.append(f'<rect x="{x + 2}" y="{y}" width="{CELL - 4}" height="40" rx="6" fill="{fill}" fill-opacity="{0.25 if i == middle else 1}" '
                 f'stroke="{stroke}" stroke-width="{2.4 if i == middle else 1.4}"/>')
        text(x + CELL / 2, y + 26, n, 17, INK if inside else GREY, weight=700 if i == middle else 400)
    # low and high under their cells
    for idx, name in ((low, 'low'), (high, 'high')):
        dx = -12 if low == high and name == 'low' else 12 if low == high else 0
        s.append(code(name, X0 + idx * CELL + CELL / 2 + dx * 1.6, y + 60, size=13, anchor='middle', background=False))
    text(X0 + CELL * len(NUMBERS) + 20, y + 26, what, 16, INK, 'start')
    y += 86

H = y - 10
out = os.path.join(HERE, '..', 'halbieren.svg')
with open(out, 'w') as f:
    f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" font-family="{FONT}">\n'
            '  <!-- Binäre Suche nach 5 in [2, 5, 8, 12, 16, 23].\n'
            '       Gemacht von bilder/halbieren.py; die dunkle Fassung halbieren-dark.svg\n'
            '       und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n  ' + '\n  '.join(s) + '\n</svg>\n')
print(out)
