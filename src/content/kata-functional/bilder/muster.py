"""The three patterns of »Funktional« with the page's examples:

- map: [1, 2, 3] becomes [10, 20, 30], each element times 10, as long as
  the old list, which stays as it is;
- filter: of [4, 7, 10, 3] the elements greater than 5 stay, 7 and 10;
- reduce: [3, 4, 5] from the start value 0: 0 + 3 = 3, 3 + 4 = 7, 7 + 5 = 12.

    python3 src/content/kata-functional/bilder/muster.py
    for f in map filter reduce; do ruby src/scripts/render-svg.rb src/content/kata-functional/$f.svg; done
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import code  # noqa: E402

INK, GREY, BLUE, AMBER, WHITE, PALE, PALE_AMBER, LINE = '#1f2937', '#6b7280', '#2b5fb4', '#d97706', '#ffffff', '#eef3fb', '#fdecc8', '#d1d5db'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
CELL = 58


def text(s, x, y, t, size=15, color=INK, anchor='middle', weight=400):
    s.append(f'<text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" text-anchor="{anchor}" font-weight="{weight}">{t}</text>')


def cell(s, x, y, v, color, fill, faded=False):
    s.append(f'<rect x="{x + 2}" y="{y}" width="{CELL - 4}" height="40" rx="6" fill="{fill}" stroke="{LINE if faded else color}" stroke-width="2"/>')
    text(s, x + CELL / 2, y + 27, v, 18, GREY if faded else INK)


def arrow(s, x1, y1, x2, y2):
    s.append(f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" stroke="{GREY}" stroke-width="1.8" marker-end="url(#head)"/>')


def save(name, w, h, comment, s):
    head = (f'<marker id="head" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto">'
            f'<path d="M0,0 L10,5 L0,10 z" fill="{GREY}"/></marker>')
    with open(os.path.join(HERE, '..', name + '.svg'), 'w') as f:
        f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {w} {h}" width="{w}" height="{h}" font-family="{FONT}">\n'
                f'  <!-- {comment}.\n'
                f'       Gemacht von bilder/muster.py; die dunkle Fassung {name}-dark.svg\n'
                '       und die Schriften: src/scripts/render-svg.rb -->\n'
                f'  <defs>{head}</defs>\n'
                f'  <rect width="{w}" height="{h}" fill="{WHITE}"/>\n  ' + '\n  '.join(s) + '\n</svg>\n')
    print(name)


def map_picture():
    s, x0 = [], 120
    for i, v in enumerate([1, 2, 3]):
        cell(s, x0 + i * CELL, 20, v, BLUE, PALE)
        arrow(s, x0 + i * CELL + CELL / 2, 64, x0 + i * CELL + CELL / 2, 116)
        text(s, x0 + i * CELL + CELL / 2 + 6, 96, '· 10', 13, GREY, 'start')
        cell(s, x0 + i * CELL, 122, v * 10, AMBER, PALE_AMBER)
    text(s, x0 - 16, 46, 'vorher', 14, GREY, 'end')
    text(s, x0 - 16, 148, 'map', 14, GREY, 'end')
    text(s, x0 + 3 * CELL + 16, 46, 'bleibt, wie sie ist', 14, GREY, 'start')
    text(s, x0 + 3 * CELL + 16, 148, 'neu, genauso lang', 14, GREY, 'start')
    save('map', 480, 180, 'map: jedes Element umwandeln', s)


def filter_picture():
    s, x0 = [], 120
    numbers, kept = [4, 7, 10, 3], [7, 10]
    for i, v in enumerate(numbers):
        cell(s, x0 + i * CELL, 20, v, BLUE, PALE)
        stays = v > 5
        text(s, x0 + i * CELL + CELL / 2, 84, 'bleibt' if stays else 'fällt weg', 13, AMBER if stays else GREY, weight=600 if stays else 400)
    for k, v in enumerate(kept):
        i = numbers.index(v)
        arrow(s, x0 + i * CELL + CELL / 2, 92, x0 + k * CELL + CELL / 2 + 4, 120)
        cell(s, x0 + k * CELL, 124, v, AMBER, PALE_AMBER)
    text(s, x0 - 16, 46, 'vorher', 14, GREY, 'end')
    text(s, x0 - 16, 150, 'filter', 14, GREY, 'end')
    text(s, x0 + 4 * CELL + 16, 46, 'behalten, was größer als 5 ist', 14, GREY, 'start')
    save('filter', 560, 184, 'filter: behalten, was passt', s)


def reduce_picture():
    s, x0 = [], 120
    numbers = [3, 4, 5]
    for i, v in enumerate(numbers):
        cell(s, x0 + 60 + i * 120, 20, v, BLUE, PALE)
    total = 0
    xs = [x0 - 10]
    text(s, x0 - 16, 46, 'Liste', 14, GREY, 'end')
    text(s, x0 - 16, 148, 'Ergebnis', 14, GREY, 'end')
    # the start value, then each step: result so far plus the next element
    s.append(f'<rect x="{x0 - 4}" y="122" width="{CELL - 4}" height="40" rx="6" fill="{WHITE}" stroke="{AMBER}" stroke-width="2" stroke-dasharray="5 4"/>')
    text(s, x0 + CELL / 2 - 6, 149, 0, 18, INK)
    text(s, x0 + CELL / 2 - 6, 184, 'Startwert', 13, GREY)
    prev_x = x0 - 6 + CELL / 2
    for i, v in enumerate(numbers):
        total += v
        cx = x0 + 120 + i * 120
        cell(s, cx, 122, total, AMBER, PALE_AMBER)
        arrow(s, prev_x + 30, 142, cx + 2, 142)
        arrow(s, x0 + 60 + i * 120 + CELL / 2, 64, cx + CELL / 2 - 8, 118)
        text(s, (prev_x + 30 + cx) / 2, 184, f'{total - v} + {v}', 14, GREY)
        prev_x = cx + CELL / 2 - 28 + 28
    save('reduce', 580, 200, 'reduce: alles zu einem Wert', s)


map_picture()
filter_picture()
reduce_picture()
