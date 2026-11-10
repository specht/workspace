"""The maps of the cave in »Ein Spiel am Telefon«, drawn in Python as SVG
beside the page, each wide (a row, for the desktop) and narrow (stacked,
for phones):

    python3 src/content/telefonspiel/bilder/karten.py
    ruby src/scripts/render-svg.rb src/content/telefonspiel/NAME.svg WIDTH

- hoehle-karte-anfang: the first cave as plain code (stufen/5-hoehle),
  without the loop: no key ends the game.
- hoehle-karte: the first cave (stufen/7-hoehle): in front of the cave, the
  1 to the treasure, the 2 home, no key back to the start.
- gang-karte: with the tunnel (stufen/8-gang) between the cave and the
  treasure; its 2 (the icy water) and no key lead back into the tunnel.

Every scene a box, every key an arrow labelled with it, »Start« with an
arrow onto the first scene; a key that leads
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


def loop(at, label, side, note=None, feet=(30, 66), note_anchor='middle'):
    """A key back into the same scene: a round loop above the box, on its
    left or right half (side -1 or 1), both ends inside the top edge (clear
    of the rounded corners); it leaves at the outer foot, swings out and
    comes down onto the inner one, the head along the curve."""
    x, y = at
    top = y - BOX_H / 2
    outer, inner = x + side * feet[1], x + side * feet[0]
    rise, flare = 64, 24                 # the peak at 3/4 of the rise
    start, end = (outer, top), (inner, top)
    c1 = (outer + side * flare, top - rise)
    c2 = (inner - side * flare, top - rise)
    parts = [ap.curve(start, c1, c2, end, INK, 2.2, 12, 6)]
    lx = x + side * (feet[0] + feet[1]) / 2
    parts.append(text(lx, top - rise * 0.75 - 9, label, 17, BLUE, weight=600))
    if note:
        nx = inner if note_anchor == 'start' else lx
        parts.append(text(nx, top - rise * 0.75 - 29, note, 15, GREY, note_anchor, italic=True))
    return ''.join(parts)


def start(at, beside_loop=True):
    """Where the game begins: an arrow from »Start« down onto the box, in its
    middle, or on its right half when a loop takes the left half."""
    x, y = at
    x += 40 if beside_loop else 0
    top = y - BOX_H / 2
    head = 13
    return (f"<line x1='{x:.1f}' y1='{top - 40:.1f}' x2='{x:.1f}' y2='{top - head + 1:.1f}' stroke='{INK}' stroke-width='2.2'/>"
            + ap.arrowhead(x, top, 0, 1, INK, head, 6.5) + text(x, top - 48, 'Start', 17, INK, weight=600))


def hoehle(wide, again=True):
    global box_w
    if wide:
        box_w, w, h = BOX_W, 560, 300
        at = {'vor der Höhle': (130, 150), 'Schatz': (430, 150), 'nach Hause': (130, 260)}
        down, home = 0.5, 0.5
    else:
        box_w, w, h = 160, 420, 360
        at = {'vor der Höhle': (110, 130), 'nach Hause': (320, 130), 'Schatz': (110, 320)}
        down, home = 0.5, 0.5
    parts = [start(at['vor der Höhle'], again), key(at['vor der Höhle'], at['Schatz'], '1', down),
             key(at['vor der Höhle'], at['nach Hause'], '2', home)]
    if again:
        parts.append(loop(at['vor der Höhle'], 'keine Taste', -1))
    parts += [scene(n, x, y, n == 'Schatz') for n, (x, y) in at.items()]
    name = 'hoehle-karte' + ('' if again else '-anfang') + ('-wide' if wide else '')
    save(name, w, h, 'Die erste Höhle: jede Szene ein Kasten, jede Taste ein Pfeil', parts)


def gang(wide):
    global box_w
    if wide:
        box_w, w, h = BOX_W, 820, 300
        at = {'vor der Höhle': (125, 150), 'Gang': (410, 150), 'Schatz': (695, 150), 'nach Hause': (125, 262)}
        parts = [start(at['vor der Höhle']), key(at['vor der Höhle'], at['Gang'], '1'),
                 key(at['vor der Höhle'], at['nach Hause'], '2'),
                 key(at['Gang'], at['Schatz'], '1'),
                 loop(at['vor der Höhle'], 'keine Taste', -1),
                 loop(at['Gang'], 'keine Taste', -1),
                 loop(at['Gang'], '2', 1, 'eiskaltes Wasser')]
    else:
        # the loops of the tunnel left and right of the arrow that comes down into it
        box_w, w, h = 160, 420, 600
        at = {'vor der Höhle': (110, 130), 'nach Hause': (320, 130), 'Gang': (110, 370), 'Schatz': (110, 545)}
        parts = [start(at['vor der Höhle']), key(at['vor der Höhle'], at['Gang'], '1', 0.18),
                 key(at['vor der Höhle'], at['nach Hause'], '2'),
                 key(at['Gang'], at['Schatz'], '1'),
                 loop(at['vor der Höhle'], 'keine Taste', -1),
                 loop(at['Gang'], 'keine Taste', -1),
                 loop(at['Gang'], '2', 1, 'eiskaltes Wasser', note_anchor='start')]
    parts += [scene(n, x, y, n == 'Schatz') for n, (x, y) in at.items()]
    name = 'gang-karte-wide' if wide else 'gang-karte'
    save(name, w, h, 'Die Höhle mit dem Gang: jede Szene ein Kasten, jede Taste ein Pfeil', parts)


for wide in (True, False):
    hoehle(wide, again=False)
    hoehle(wide)
    gang(wide)
