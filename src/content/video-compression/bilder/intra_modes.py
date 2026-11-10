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
    """a cell tinted by its value; the neighbours a little apart from each
    other, the block's cells joined"""
    x, y = X0 + (col + 1) * S, Y0 + (row + 1) * S
    inset = 2 if neighbour else 0
    return [f"<rect x='{x + inset}' y='{y + inset}' width='{S - 2 * inset}' height='{S - 2 * inset}' rx='{3 if neighbour else 0}' fill='{WHITE}'/>",
            f"<rect x='{x + inset}' y='{y + inset}' width='{S - 2 * inset}' height='{S - 2 * inset}' rx='{3 if neighbour else 0}' fill='{PURPLE}' fill-opacity='{0.06 + 0.6 * value / 255:.2f}' stroke='{LINE}' stroke-width='1'/>"]


def point(px, py):
    """a point in cells (the block from 0 to 4) on the drawing"""
    return X0 + S + px * S, Y0 + S + py * S


def arrows(d):
    """one arrow along the path of a bright neighbour through the block: from
    the centre of the bright neighbour whose path runs furthest through the
    block, to where the path leaves it"""
    dx, dy = d
    n = math.hypot(dx, dy)
    ux, uy = dx / n, dy / n
    sources = [(i + 0.5, -0.5) for i, v in enumerate(TOP) if v == max(TOP)] + [(-0.5, j + 0.5) for j, v in enumerate(LEFT) if v == max(LEFT)]
    best = None
    for sx, sy in sources:
        # where the path enters and leaves the block (0 to 4 in both)
        t0, t1 = 0, math.inf
        for p, v in ((sx, dx), (sy, dy)):
            if v == 0:
                if not 0 <= p <= 4:
                    t0, t1 = 1, 0
                continue
            a, b = (0 - p) / v, (4 - p) / v
            t0, t1 = max(t0, min(a, b)), min(t1, max(a, b))
        if t1 > t0 and (best is None or t1 - t0 > best[0]):
            best = (t1 - t0, sx, sy, t1)
    if best is None:
        return []
    _, sx, sy, t1 = best
    a, b = point(sx, sy), point(sx + (t1 - 0.2) * dx, sy + (t1 - 0.2) * dy)
    return [f"<line x1='{a[0]:.1f}' y1='{a[1]:.1f}' x2='{b[0] - ux * 8:.1f}' y2='{b[1] - uy * 8:.1f}' stroke='{INK}' stroke-width='2.5' stroke-linecap='round'/>",
            ap.arrowhead(b[0], b[1], ux, uy, INK, 12, 6),
            f"<circle cx='{a[0]:.1f}' cy='{a[1]:.1f}' r='4' fill='{INK}'/>"]


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
    # the block itself outlined, so that it reads as one
    parts.append(f"<rect x='{X0 + S}' y='{Y0 + S}' width='{4 * S}' height='{4 * S}' fill='none' stroke='{INK}' stroke-width='2'/>")
    if d is not None:
        parts += arrows(d)
    head = ap.svg_head(w, h, f'Vorhersage {number} von H.264 ({name}); gemacht von bilder/intra_modes.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/video-compression/intra-mode-{number}.svg')
    with open(os.path.join(OUT, f'intra-mode-{number}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')
    print(number, name, [[round(v) for v in row] for row in predicted(mode)])


for mode in MODES:
    draw(mode)
