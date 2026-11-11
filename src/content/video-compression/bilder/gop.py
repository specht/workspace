"""Drawings of a group of pictures, the frames as tiles in the
colours of the bar charts of gop.py (I red, P blue, B green), arrows from a
frame to each frame it is predicted from (where it looks for support):

    gop-refs: I B B B P B B B P as they are shown; each P from the I or P
        before it, each B from the I or P on either side; arrows to an
        earlier frame above the row, to a later one below
    gop-order: the same frames, shown (above) and in the file (below), a
        line from each frame to its place in the file
    gop-pyramid: I B B B P as x264 makes them: the middle B from I and P,
        the outer two from the middle one and their other neighbour
    gop-seek: 16 frames, an I-frame every 8, a jump to frame 13 and what
        has to be computed for it (on a phone in two rows of 8)
    gop-mosh: datamoshing: the end of pan.h264 and the start of tilt.h264
        without its I-frame; P1 so encoded and so computed

Wide for a desktop, narrow (the name without -wide) for a phone, there
with one group, I B B B P, so that the tiles stay large enough.

    src/scripts/drawings python3 src/content/video-compression/bilder/gop.py
    for f in gop-refs gop-order gop-pyramid gop-seek gop-mosh; do for v in '' -wide; do
        src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/$f$v.svg; done; done
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, WHITE = '#1f2937', '#6b7280', '#ffffff'
COLOURS = {'I': '#dc2626', 'P': '#2b5fb4', 'B': '#16a34a'}

WIDE = dict(frames='IBBBPBBBP', tile=72, gap=24, letter=30, number=16, label=16, margin=24, lift=34,
            seek_tile=48, seek_gap=14, seek_letter=22)
NARROW = dict(frames='IBBBP', tile=60, gap=20, letter=30, number=16, label=17, margin=14, lift=28,
              seek_tile=38, seek_gap=12, seek_letter=19)


class Row:
    """A row of tiles; x of the first tile's left edge, y of the top. Each
    tile shows its kind with its number as an index, as in I₀ B₁ P₄."""

    def __init__(self, size, x, y, kinds, numbers):
        self.size, self.x, self.y, self.kinds, self.numbers = size, x, y, kinds, list(numbers)

    def centre(self, k):
        t, g = self.size['tile'], self.size['gap']
        return self.x + k * (t + g) + t / 2, self.y + t / 2

    def parts(self):
        out = []
        for k, (kind, number) in enumerate(zip(self.kinds, self.numbers)):
            out += tile(self.size, self.centre(k), kind, number)
        return out


def tile(s, centre, kind, number, dropped=False, strong=False):
    """One frame: a square in the colour of its kind, the kind with its
    number as an index. dropped: dashed and pale, a frame thrown away;
    strong: a thicker outline, the frame the drawing is about."""
    t = s['tile']
    cx, cy = centre
    colour = COLOURS[kind]
    fill = '0.04' if dropped else '0.12'
    dash = f" stroke-dasharray='{t * 0.09:.1f} {t * 0.07:.1f}'" if dropped else ''
    width = 4 if strong else 2
    out = [f"<rect x='{cx - t / 2:.1f}' y='{cy - t / 2:.1f}' width='{t}' height='{t}' rx='{t * 0.12:.1f}' "
           f"fill='{colour}' fill-opacity='{fill}' stroke='{colour}' stroke-width='{width}'{dash}/>"]
    opacity = " fill-opacity='0.45'" if dropped else ''
    out.append(f"<text x='{cx - s['letter'] * 0.12:.1f}' y='{cy + s['letter'] * 0.3:.1f}' text-anchor='middle' "
               f"font-family=\"{ap.MONO}\" font-size='{s['letter']}' fill='{colour}'{opacity}>{kind}"
               f"<tspan dy='0.3em' font-size='0.6em'>{number}</tspan></text>")
    return out


def step(s, a, b, colour=GREY):
    """A short straight arrow in the gap between two neighbouring tiles
    (centres a and b, side by side): b is predicted from a, so the arrow
    points from b back to a."""
    t = s['tile']
    x0, x1 = a[0] + t / 2, b[0] - t / 2 - 2
    head = min(9, (x1 - x0) * 0.6)
    return [f"<line x1='{x1:.1f}' y1='{b[1]:.1f}' x2='{x0 + head - 1:.1f}' y2='{a[1]:.1f}' stroke='{colour}' stroke-width='1.6'/>",
            ap.arrowhead(x0, a[1], -1, 0, colour, head, head / 2)]


def border(centre, half, towards):
    """Where the line from the centre of a square (half: half its side)
    towards a point leaves the square."""
    dx, dy = towards[0] - centre[0], towards[1] - centre[1]
    scale = half / max(abs(dx), abs(dy))
    return centre[0] + dx * scale, centre[1] + dy * scale


def references(row, pairs):
    """An arc for each (source, target) pair of places in the row, target
    predicted from source; the arrow points from the target to the source.
    Arcs to an earlier source go above the row, to a later one below. On
    each edge the feet are spread so that the arcs nest instead of
    crossing: above, left to right, the targets' feet (shortest arc
    first), then the sources' (longest first); below the mirror of that."""
    s = row.size
    t = s['tile']
    feet = {}   # (place, side) -> list of (order, pair)
    for a, b in pairs:
        span = abs(b - a)
        side = -1 if a < b else 1
        if side < 0:
            feet.setdefault((a, side), []).append(((1, -span), (a, b)))
            feet.setdefault((b, side), []).append(((0, span), (a, b)))
        else:
            feet.setdefault((a, side), []).append(((0, span), (a, b)))
            feet.setdefault((b, side), []).append(((1, -span), (a, b)))
    offset = {}
    for (place, side), items in feet.items():
        items.sort()
        n = len(items)
        for i, (_, pair) in enumerate(items):
            offset[(place, pair)] = 0.0 if n == 1 else t * 0.3 * (2 * i / (n - 1) - 1)
    out = []
    for a, b in pairs:
        side = -1 if a < b else 1
        ca, cb = row.centre(a), row.centre(b)
        pa = border(ca, t / 2, (ca[0] + offset[(a, (a, b))], ca[1] + side * t / 2))
        pb = border(cb, t / 2, (cb[0] + offset[(b, (a, b))], cb[1] + side * t / 2))
        h = s['lift'] * (0.55 + 0.62 * abs(b - a))

        def away(p, c):
            n = math.dist(p, c)
            return (p[0] - c[0]) / n, (p[1] - c[1]) / n
        da, db = away(pa, ca), away(pb, cb)
        k = h / max(abs(da[1]), 0.3)
        p1 = (pa[0] + da[0] * k * 0.35, pa[1] + da[1] * k)
        k = h / max(abs(db[1]), 0.3)
        p2 = (pb[0] + db[0] * k * 0.35, pb[1] + db[1] * k)
        # the arrow points from the predicted frame to the one it looks to
        out.append(ap.curve(pb, p2, p1, pa, GREY, 1.6))
    return out


def save(name, w, h, comment, parts):
    head = ap.svg_head(w, h, f'{comment}; gemacht von bilder/gop.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/video-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


def row_width(s, n):
    return n * s['tile'] + (n - 1) * s['gap']


def refs(s, name):
    frames = s['frames']
    n = len(frames)
    W = row_width(s, n) + 2 * s['margin']
    top = s['lift'] * 3.1 + 8
    row = Row(s, s['margin'], top, frames, range(n))
    pairs = [(k, k + 4) for k in range(0, n - 4, 4)]
    for first in range(0, n - 4, 4):
        for k in range(first + 1, first + 4):
            pairs += [(first, k), (first + 4, k)]
    parts = references(row, pairs) + row.parts()
    save(name, W, round(top + s['tile'] + s['lift'] * 2.5 + 8), 'Welche Bilder aus welchen vorhergesagt werden', parts)


def decode_order(kinds):
    order, waiting = [], []
    for k, kind in enumerate(kinds):
        if kind == 'B':
            waiting.append(k)
        else:
            order.append(k)
            order += waiting
            waiting = []
    return order + waiting


def order(s, name):
    frames = s['frames']
    n = len(frames)
    left = s['margin']
    W = row_width(s, n) + 2 * s['margin']
    label = s['label']
    shown = Row(s, left, label + 12, frames, range(n))
    stored = decode_order(frames)
    file_y = shown.y + s['tile'] * 2.6
    in_file = Row(s, left, file_y, [frames[k] for k in stored], stored)
    parts = [ap.text(left, label, 'gezeigt', label, GREY, 'start'),
             ap.text(left, file_y + s['tile'] + label + 10, 'in der Datei', label, GREY, 'start')]
    for place, k in enumerate(stored):
        a, b = shown.centre(k), in_file.centre(place)
        start = border(a, s['tile'] / 2, b)
        end = border(b, s['tile'] / 2, a)
        parts.append(f"<line x1='{start[0]:.1f}' y1='{start[1]:.1f}' x2='{end[0]:.1f}' y2='{end[1]:.1f}' "
                     f"stroke='{GREY}' stroke-width='1.6'/>")
        parts.append(ap.arrowhead(end[0], end[1], end[0] - start[0], end[1] - start[1], GREY))
    parts += shown.parts() + in_file.parts()
    save(name, W, round(file_y + s['tile'] + label + 18), 'Die Bilder, wie sie gezeigt werden und wie sie in der Datei stehen', parts)


def pyramid(s, name):
    kinds = 'IBBBP'
    n = len(kinds)
    W = row_width(s, n) + 2 * s['margin'] + 2 * s['gap']
    top = s['lift'] * 3.1 + 8
    row = Row(s, s['margin'] + s['gap'], top, kinds, range(n))
    pairs = [(0, 4), (0, 2), (4, 2), (0, 1), (2, 1), (2, 3), (4, 3)]
    parts = references(row, pairs) + row.parts()
    save(name, W, round(top + s['tile'] + s['lift'] * 2.0 + 8), 'Eine B-Pyramide: das mittlere B als Vorlage der beiden anderen', parts)


def seek(s, name, per_row):
    """16 frames, an I-frame every 8, each P from the one before it; a jump
    to frame 13 means computing 8 to 13. per_row: 16 in one row (desktop)
    or 8 in two (phone, the second row under the first)."""
    t, g = s['seek_tile'], s['seek_gap']
    size = dict(s, tile=t, letter=s['seek_letter'])
    kinds = ['I' if k % 8 == 0 else 'P' for k in range(16)]
    target, first = 13, 8
    row_h = t + s['label'] * 3.4
    top = s['label'] + 26
    W = per_row * t + (per_row - 1) * g + 2 * s['margin']

    def centre(k):
        r, c = divmod(k, per_row)
        return s['margin'] + c * (t + g) + t / 2, top + r * row_h + t / 2
    parts = []
    for k in range(16):
        parts += tile(size, centre(k), kinds[k], k, strong=(k == target))
        if kinds[k] == 'P' and k % per_row:
            parts += step(size, centre(k - 1), centre(k))
    # what has to be computed: a bracket under 8 to 13, in the row of 13
    a, b = centre(first), centre(target)
    if a[1] == b[1]:
        y = a[1] + t / 2 + 8
        x0, x1 = a[0] - t / 2, b[0] + t / 2
        parts.append(f"<path d='M {x0:.1f} {y:.1f} v 7 H {x1:.1f} v -7' fill='none' stroke='{GREY}' stroke-width='1.6'/>")
        parts.append(ap.text((x0 + x1) / 2, y + 7 + s['label'] + 4, 'rechnen, bevor 13 erscheint', s['label'], GREY))
    # where the jump lands
    x, y = b[0], b[1] - t / 2
    parts.append(ap.text(x, y - 24 - s['label'] * 0.2, 'Sprung', s['label'], ap.ACCENT))
    parts.append(f"<line x1='{x:.1f}' y1='{y - 20:.1f}' x2='{x:.1f}' y2='{y - 10:.1f}' stroke='{ap.ACCENT}' stroke-width='2'/>")
    parts.append(ap.arrowhead(x, y - 3, 0, 1, ap.ACCENT))
    rows = math.ceil(16 / per_row)
    H = top + (rows - 1) * row_h + t + s['label'] * 2 + 20
    save(name, W, round(H), 'Ein Sprung zu Bild 13: vom I-Frame davor an rechnen', parts)


def mosh(s, name, before, after):
    """Two clips joined, the I-frame of the second thrown away: its P1 was
    computed against its own I0 (dashed arc above), the decoder takes the
    last frame of the first clip instead (arc below, in the accent colour).
    Arrows point from a frame to the one it is predicted from.
    before: numbers of the first clip's last frames, after: of the second's
    first P-frames."""
    t, g = s['tile'], s['gap']
    join = g * 2.2
    top = s['label'] + 18 + s['lift'] * 1.4 + s['label'] + 14
    xs, frames = [], []
    x = s['margin'] + t / 2
    for number in before:
        frames.append(('P', number, False))
        xs.append(x)
        x += t + g
    x += join - g
    frames.append(('I', 0, True))
    xs.append(x)
    for number in after:
        x += t + g
        frames.append(('P', number, False))
        xs.append(x)
    W = xs[-1] + t / 2 + s['margin']
    cy = top + t / 2
    parts = [ap.text(xs[0] - t / 2, s['label'], 'pan.h264', s['label'], GREY, 'start'),
             ap.text(xs[len(before)] - t / 2, s['label'], 'tilt.h264', s['label'], GREY, 'start')]
    split = (xs[len(before) - 1] + xs[len(before)]) / 2
    parts.append(f"<line x1='{split:.1f}' y1='{s['label'] - 12:.1f}' x2='{split:.1f}' y2='{cy + t / 2 + 6:.1f}' "
                 f"stroke='{GREY}' stroke-width='1' stroke-dasharray='3 4'/>")
    for k, (kind, number, dropped) in enumerate(frames):
        parts += tile(s, (xs[k], cy), kind, number, dropped=dropped)
        if k and kind == 'P' and not frames[k - 1][2] and k != len(before):
            parts += step(s, (xs[k - 1], cy), (xs[k], cy))
    i, p1, last = len(before), len(before) + 1, len(before) - 1
    # so encoded: from I0 to P1, dashed, above
    a, b = (xs[i] + t * 0.12, cy - t / 2), (xs[p1] - t * 0.12, cy - t / 2)
    h = s['lift'] * 1.4
    arc = ap.curve(b, (b[0], b[1] - h), (a[0], a[1] - h), a, GREY, 1.6)
    parts.append(arc.replace("stroke-width='1.6'/>", "stroke-width='1.6' stroke-dasharray='5 4'/>", 1))
    parts.append(ap.text((a[0] + b[0]) / 2, a[1] - h - 6, 'so codiert', s['label'], GREY))
    # so decoded: from the first clip's last frame to P1, below
    a, b = (xs[last], cy + t / 2), (xs[p1], cy + t / 2)
    h = s['lift'] * 1.6
    parts.append(ap.curve(b, (b[0], b[1] + h), (a[0], a[1] + h), a, ap.ACCENT, 2))
    parts.append(ap.text((a[0] + b[0]) / 2, a[1] + h + s['label'] + 4, 'so gerechnet', s['label'], ap.ACCENT))
    H = cy + t / 2 + h + s['label'] * 2 + 10
    save(name, round(W), round(H), 'Datamoshing: das I-Frame des zweiten Ausschnitts fehlt', parts)


for size, suffix in [(WIDE, '-wide'), (NARROW, '')]:
    refs(size, f'gop-refs{suffix}')
    order(size, f'gop-order{suffix}')
    pyramid(size, f'gop-pyramid{suffix}')
seek(WIDE, 'gop-seek-wide', 16)
seek(NARROW, 'gop-seek', 8)
mosh(WIDE, 'gop-mosh-wide', [93, 94, 95], [1, 2, 3])
mosh(NARROW, 'gop-mosh', [95], [1, 2])
