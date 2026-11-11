"""What smooth_edges does to one row of pixels: two blocks, 100 and 108,
meet at a block edge (dashed); before, a step between the two pixels at the
edge; after, a ramp over the eight pixels around it, four on each side.
Each pixel a cell with its value and a bar above it, as tall as the value
above 98. Wide for a desktop (all 16 pixels), narrow for a phone (the eight
of the ramp).

    src/scripts/drawings python3 src/content/video-compression/bilder/deblock.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/deblock.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/deblock-wide.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, BLUE, WHITE = '#1f2937', '#6b7280', '#d1d5db', '#2b5fb4', '#ffffff'

BEFORE = [100.0] * 8 + [108.0] * 8
AFTER = list(BEFORE)
for j in range(4):
    AFTER[7 - j] += 8 * (7 - 2 * j) / 16
    AFTER[8 + j] -= 8 * (7 - 2 * j) / 16


def number(v):
    return f'{v:g}'.replace('.', ',')


def row(x0, y0, cell, values, first, label, size):
    """A row of cells from pixel first on, its bars above, its label above
    the bars; y0 is the top of the label."""
    bar_room = 10 * 9
    parts = [ap.text(x0, y0 + size, label, size, GREY, 'start')]
    base = y0 + size + 8 + bar_room
    for i, v in enumerate(values):
        x = x0 + i * cell
        h = (v - 98) * 9
        parts.append(f"<rect x='{x + 3:.1f}' y='{base - h:.1f}' width='{cell - 6:.1f}' height='{h:.1f}' fill='{BLUE}' fill-opacity='0.22' stroke='{BLUE}' stroke-width='1.2'/>")
        parts.append(f"<rect x='{x:.1f}' y='{base + 4:.1f}' width='{cell}' height='{cell * 0.7:.1f}' fill='{WHITE}' stroke='{LINE}' stroke-width='1.2'/>")
        parts.append(ap.text(x + cell / 2, base + 4 + cell * 0.35 + size * 0.36, number(v), size, INK))
    return parts, base + 4 + cell * 0.7


def picture(name, first, count, cell, size, margin):
    width = margin * 2 + count * cell
    parts = []
    y = 10
    rows = [('vorher: eine Stufe an der Kante', BEFORE), ('nachher: eine Rampe über acht Pixel', AFTER)]
    bottoms = []
    for label, values in rows:
        drawn, bottom = row(margin, y, cell, values[first:first + count], first, label, size)
        parts += drawn
        bottoms.append((y + size + 8, bottom))
        y = bottom + 22
    # the block edge between pixel 7 and 8, dashed through the bars and
    # cells of each row, not through the labels
    x = margin + (8 - first) * cell
    for top, bottom in bottoms:
        parts.append(f"<line x1='{x:.1f}' y1='{top - 2:.1f}' x2='{x:.1f}' y2='{bottom + 6:.1f}' "
                     f"stroke='{INK}' stroke-width='1.5' stroke-dasharray='5 4'/>")
    parts.append(ap.text(x, bottoms[-1][1] + 6 + size + 4, 'Kante zwischen zwei Blöcken', size, GREY))
    height = bottoms[-1][1] + 6 + size + 14
    head = ap.svg_head(round(width), round(height), 'smooth_edges an einer Kante: eine Stufe wird eine Rampe; gemacht von bilder/deblock.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/video-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


picture('deblock-wide', 0, 16, 62, 15, 16)
picture('deblock', 4, 8, 50, 13, 10)
