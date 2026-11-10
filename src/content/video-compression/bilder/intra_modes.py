"""The nine ways H.264 predicts a block of 4 × 4 from its neighbours, one
drawing each for the gallery of »Innerhalb eines Bildes«: the row above
(with the four pixels above and to the right, which the diagonals to the
left need), the column to the left and the corner, and the block they
predict. The neighbours are dark with one bright cell above and one on the
left, so that each way shows where it carries them; every cell is tinted
the stronger the larger its value. Each pixel follows the way's direction
back to the neighbours and takes the value there, between the two nearest
neighbours; H.264 itself smooths the neighbours a little first, so the
drawings show the directions, not the exact numbers. Arrows show the
direction; dc, the mean of the eight neighbours, has none.

    src/scripts/drawings python3 src/content/video-compression/bilder/intra_modes.py
    for k in 0 1 2 3 4 5 6 7 8; do src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/intra-mode-$k.svg; done
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, PURPLE, WHITE = '#1f2937', '#6b7280', '#d1d5db', '#7c3aed', '#ffffff'
TOP = [40, 40, 220, 40, 40, 220, 40, 40]       # A to H: above the block and above to the right
LEFT = [40, 220, 40, 40]                       # I to L
CORNER = 40                                    # M
S = 32                                         # a cell
X0, Y0 = 10, 10                                # the corner cell's top left
# number, name, the direction the values travel in (columns, rows), None for dc
MODES = [
    (0, 'vertical', (0, 1)), (1, 'horizontal', (1, 0)), (2, 'dc', None),
    (3, 'diagonal down left', (-1, 1)), (4, 'diagonal down right', (1, 1)),
    (5, 'vertical right', (0.5, 1)), (6, 'horizontal down', (1, 0.5)),
    (7, 'vertical left', (-0.5, 1)), (8, 'horizontal up', (1, -0.5)),
]


def along(positions, values, at):
    """the value at a place between the neighbours' centres, clamped at the ends"""
    at = max(positions[0], min(positions[-1], at))
    for (p0, v0), (p1, v1) in zip(zip(positions, values), zip(positions[1:], values[1:])):
        if p0 <= at <= p1:
            return v0 + (v1 - v0) * (at - p0) / (p1 - p0)
    return values[-1]


def source(px, py, d):
    """where a point of the block (in cells, the block from 0 to 4) comes from:
    back along d to the centre line of the row above (y = -0.5) or of the
    column to the left (x = -0.5), whichever comes first; returns the point
    and its value"""
    dx, dy = d
    hits = []
    if dy > 0:
        hits.append(((py + 0.5) / dy, 'top'))
    if dx > 0:
        hits.append(((px + 0.5) / dx, 'left'))
    t, side = min(hits)
    x, y = px - t * dx, py - t * dy
    if side == 'top':
        return (x, -0.5), along([-0.5] + [i + 0.5 for i in range(8)], [CORNER] + TOP, x)
    return (-0.5, y), along([-0.5] + [i + 0.5 for i in range(4)], [CORNER] + LEFT, y)


def predicted(mode):
    _, _, d = mode
    if d is None:
        mean = (sum(TOP[:4]) + sum(LEFT) + 4) // 8
        return [[mean] * 4 for _ in range(4)]
    return [[source(c + 0.5, r + 0.5, d)[1] for c in range(4)] for r in range(4)]


def cell(col, row, value, neighbour):
    x, y = X0 + (col + 1) * S, Y0 + (row + 1) * S
    parts = [f"<rect x='{x}' y='{y}' width='{S}' height='{S}' fill='{WHITE}' stroke='{INK if neighbour else LINE}' stroke-width='1.5'/>"]
    parts.append(f"<rect x='{x + 1}' y='{y + 1}' width='{S - 2}' height='{S - 2}' fill='{PURPLE}' fill-opacity='{0.06 + 0.6 * value / 255:.2f}'/>")
    return parts


def point(px, py):
    """a point in cells (the block from 0 to 4) on the drawing"""
    return X0 + S + px * S, Y0 + S + py * S


def arrows(d):
    """three arrows across the block along d, from the neighbours' centre line"""
    dx, dy = d
    n = math.hypot(dx, dy)
    ux, uy = dx / n, dy / n
    parts = []
    for k in (-1.3, 0, 1.3):
        px, py = 2 - uy * k, 2 + ux * k
        (sx, sy), _ = source(px, py, d)
        if sx > 7.5 or sy > 3.5:
            continue                          # beyond the neighbours: no arrow
        # where the line leaves the block
        exits = [((4 if v > 0 else 0) - p) / v for p, v in ((px, dx), (py, dy)) if v]
        t = min(e for e in exits if e > 0)
        ex, ey = px + (t - 0.25) * dx / 1, py + (t - 0.25) * dy / 1
        if not (0 <= ex <= 4 and 0 <= ey <= 4):
            continue
        a, b = point(sx, sy), point(ex, ey)
        parts.append(f"<line x1='{a[0]:.1f}' y1='{a[1]:.1f}' x2='{b[0] - ux * 6:.1f}' y2='{b[1] - uy * 6:.1f}' stroke='{INK}' stroke-width='2'/>")
        parts.append(ap.arrowhead(b[0], b[1], ux, uy, INK, 10, 5))
        parts.append(f"<circle cx='{a[0]:.1f}' cy='{a[1]:.1f}' r='3' fill='{INK}'/>")
    return parts


def draw(mode):
    number, name, d = mode
    w, h = X0 * 2 + 9 * S, Y0 * 2 + 5 * S
    parts = cell(-1, -1, CORNER, True)
    for i, v in enumerate(TOP):
        parts += cell(i, -1, v, True)
    for j, v in enumerate(LEFT):
        parts += cell(-1, j, v, True)
    for r, row in enumerate(predicted(mode)):
        for c, v in enumerate(row):
            parts += cell(c, r, v, False)
    if d is not None:
        parts += arrows(d)
    head = ap.svg_head(w, h, f'Vorhersage {number} von H.264 ({name}); gemacht von bilder/intra_modes.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/video-compression/intra-mode-{number}.svg')
    with open(os.path.join(OUT, f'intra-mode-{number}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')
    print(number, name, [[round(v) for v in row] for row in predicted(mode)])


for mode in MODES:
    draw(mode)
