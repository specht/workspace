"""The maps of the cave in »Ein Spiel am Telefon«, drawn in Python as SVG
beside the page, each wide (a row, for the desktop) and narrow (stacked,
for phones):

    python3 src/content/telefonspiel/bilder/karten.py
    ruby src/scripts/render-svg.rb src/content/telefonspiel/NAME.svg WIDTH

- hoehle-karte: the first cave (stufen/5-hoehle): in front of the cave, the
  1 to the treasure, the 2 home, no key back to the start.
- gang-karte: with the tunnel (stufen/6-gang) between the cave and the
  treasure; its 2 (the icy water) and no key lead back into the tunnel.

Every scene a box, every key an arrow labelled with it; a key that leads
back into the same scene is a loop above its box. The treasure, the good
end, has a blue frame; the way home is the other end.
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
INK = '#1f2937'
GREY = '#6b7280'
BLUE = '#2b5fb4'
PALE = '#eef3fb'
WHITE = '#ffffff'
BOX_W, BOX_H = 190, 54
box_w = BOX_W                 # narrower in the pictures for phones


def save(name, w, h, comment, parts):
    render = f'src/scripts/render-svg.rb src/content/telefonspiel/{name}.svg {2 * w}'
    head = ap.svg_head(w, h, comment + '; gemacht von bilder/karten.py, als Bild:', render)
    head = head.replace('<svg ', f"<svg font-family='{FONT}' ", 1)
    with open(os.path.join(OUT, name + '.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


def text(x, y, s, size=18, color=INK, anchor='middle', italic=False, weight=None):
    style = " font-style='italic'" if italic else ''
    style += f" font-weight='{weight}'" if weight else ''
    return f"<text x='{x:.1f}' y='{y:.1f}' font-size='{size}' fill='{color}' text-anchor='{anchor}'{style}>{s}</text>"


def scene(name, x, y, goal=False):
    stroke, width = (BLUE, 4) if goal else (INK, 2)
    return (f"<rect x='{x - box_w / 2:.1f}' y='{y - BOX_H / 2:.1f}' width='{box_w}' height='{BOX_H}' rx='10' "
            f"fill='{PALE}' stroke='{stroke}' stroke-width='{width}'/>" + text(x, y + 7, name, 20))


def border(cx, cy, dx, dy):
    """Where the ray from the centre of a box along (dx, dy) leaves it."""
    tx = box_w / 2 / abs(dx) if dx else math.inf
    ty = BOX_H / 2 / abs(dy) if dy else math.inf
    t = min(tx, ty)
    return cx + dx * t, cy + dy * t


def key(a, b, label, label_at=0.5):
    """An arrow from scene a to scene b, aimed at their centres, the tip on
    b's border; the key beside its middle."""
    (ax, ay), (bx, by) = a, b
    n = math.hypot(bx - ax, by - ay)
    ux, uy = (bx - ax) / n, (by - ay) / n
    sx, sy = border(ax, ay, ux, uy)
    ex, ey = border(bx, by, -ux, -uy)
    head = 13
    parts = [f"<line x1='{sx:.1f}' y1='{sy:.1f}' x2='{ex - ux * (head - 1):.1f}' y2='{ey - uy * (head - 1):.1f}' "
             f"stroke='{INK}' stroke-width='2.2'/>", ap.arrowhead(ex, ey, ux, uy, INK, head, 6.5)]
    mx, my = sx + (ex - sx) * label_at, sy + (ey - sy) * label_at
    if abs(uy) < 0.3:                    # across: the key above the arrow
        parts.append(text(mx, my - 10, label, 19, BLUE, weight=600))
    else:                                # down: the key to the right of it
        parts.append(text(mx + 12, my + 7, label, 19, BLUE, 'start', weight=600))
    return ''.join(parts)


def loop(at, label, side, note=None, feet=(22, 78), note_anchor='middle'):
    """A key back into the same scene: a loop above the box, on its left or
    right half (side -1 or 1), the tip coming down onto the box."""
    x, y = at
    top = y - BOX_H / 2
    a = x + side * feet[0]
    b = x + side * feet[1]
    start, end = (a, top), (b, top)
    if side < 0:
        start, end = end, start
    c1 = (start[0], top - 62)
    c2 = (end[0], top - 62)
    head = 12
    # the tip on the top edge, the head along the curve's last tangent
    parts = [f"<path d='M {start[0]:.1f} {start[1]:.1f} C {c1[0]:.1f} {c1[1]:.1f} {c2[0]:.1f} {c2[1]:.1f} "
             f"{end[0]:.1f} {end[1] - head + 1:.1f}' fill='none' stroke='{INK}' stroke-width='2.2'/>",
             ap.arrowhead(end[0], end[1], 0, 1, INK, head, 6)]
    lx = x + side * (feet[0] + feet[1]) / 2
    parts.append(text(lx, top - 56, label, 17, BLUE, weight=600))
    if note:
        nx = a if note_anchor == 'start' else lx
        parts.append(text(nx, top - 76, note, 15, GREY, note_anchor, italic=True))
    return ''.join(parts)


def hoehle(wide):
    global box_w
    if wide:
        box_w, w, h = BOX_W, 560, 300
        at = {'vor der Höhle': (130, 150), 'Schatz': (430, 150), 'nach Hause': (130, 260)}
        down, home = 0.5, 0.5
    else:
        box_w, w, h = 160, 420, 360
        at = {'vor der Höhle': (110, 130), 'nach Hause': (320, 130), 'Schatz': (110, 320)}
        down, home = 0.5, 0.5
    parts = [key(at['vor der Höhle'], at['Schatz'], '1', down),
             key(at['vor der Höhle'], at['nach Hause'], '2', home),
             loop(at['vor der Höhle'], 'keine Taste', -1, feet=(30, 80) if not wide else (22, 78))]
    parts += [scene(n, x, y, n == 'Schatz') for n, (x, y) in at.items()]
    name = 'hoehle-karte-wide' if wide else 'hoehle-karte'
    save(name, w, h, 'Die erste Höhle: jede Szene ein Kasten, jede Taste ein Pfeil', parts)


def gang(wide):
    global box_w
    if wide:
        box_w, w, h = BOX_W, 820, 300
        at = {'vor der Höhle': (125, 150), 'Gang': (410, 150), 'Schatz': (695, 150), 'nach Hause': (125, 262)}
        parts = [key(at['vor der Höhle'], at['Gang'], '1'),
                 key(at['vor der Höhle'], at['nach Hause'], '2'),
                 key(at['Gang'], at['Schatz'], '1'),
                 loop(at['vor der Höhle'], 'keine Taste', -1),
                 loop(at['Gang'], 'keine Taste', -1),
                 loop(at['Gang'], '2', 1, 'eiskaltes Wasser')]
    else:
        # the loops of the tunnel left and right of the arrow that comes down into it
        box_w, w, h = 160, 420, 600
        at = {'vor der Höhle': (110, 130), 'nach Hause': (320, 130), 'Gang': (110, 370), 'Schatz': (110, 545)}
        parts = [key(at['vor der Höhle'], at['Gang'], '1', 0.18),
                 key(at['vor der Höhle'], at['nach Hause'], '2'),
                 key(at['Gang'], at['Schatz'], '1'),
                 loop(at['vor der Höhle'], 'keine Taste', -1, feet=(30, 80)),
                 loop(at['Gang'], 'keine Taste', -1, feet=(32, 88)),
                 loop(at['Gang'], '2', 1, 'eiskaltes Wasser', feet=(22, 72), note_anchor='start')]
    parts += [scene(n, x, y, n == 'Schatz') for n, (x, y) in at.items()]
    name = 'gang-karte-wide' if wide else 'gang-karte'
    save(name, w, h, 'Die Höhle mit dem Gang: jede Szene ein Kasten, jede Taste ein Pfeil', parts)


for wide in (True, False):
    hoehle(wide)
    gang(wide)
