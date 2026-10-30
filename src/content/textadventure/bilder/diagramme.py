"""The pictures of »Ein Textadventure aus Objekten« drawn in Python, as SVG
beside the page.

    python3 src/content/textadventure/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/textadventure/NAME.svg WIDTH

- schule-karte: the night in the school (starter/schule.rb), every room a
  box and every connect an arrow of its own, labelled with its direction:
  two arrows between two rooms are two calls. The Keller dark, the Schulhof
  (the goal) with a blue frame, the locked doors in amber; the Schulhof has
  no way back.
- eine-zeile, eine-zeile-camel: what happens to »gehe westen« in Raum 204,
  call by call, as a sequence diagram: the loop gives the line to the game,
  the game to the player, the player asks the room for the exit, moves, and
  the text of the Flur goes back the same way. camel with exitTo (Dart,
  Java), the other with exit_to (Ruby, Python).
- welt-karte: the small world of »Deine Welt« (build_world), the same way
  as the school.
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402
from diagram_labels import code  # noqa: E402

OUT = os.path.join(HERE, '..')
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
INK = '#1f2937'
GREY = '#6b7280'
BLUE = '#2b5fb4'
AMBER = '#d97706'
PALE = '#eef3fb'
WHITE = '#ffffff'
DARK = '#0b0f19'   # a dark room: dark in both modes
LIGHT = '#fdfdfd'  # its name: light in both modes
BOX_W, BOX_H = 200, 54


def save(name, w, h, comment, parts, scale=2):
    render = f'src/scripts/render-svg.rb src/content/textadventure/{name}.svg {scale * w}'
    head = ap.svg_head(w, h, comment + '; gemacht von bilder/diagramme.py, als Bild:', render)
    head = head.replace('<svg ', f"<svg font-family='{FONT}' ", 1)
    with open(os.path.join(OUT, name + '.svg'), 'w') as f:
        f.write(head + f"<rect width='{w}' height='{h}' fill='{WHITE}'/>" + ''.join(parts) + '</svg>\n')


def text(x, y, s, size=18, color=INK, anchor='middle', italic=False):
    style = " font-style='italic'" if italic else ''
    return f"<text x='{x:.1f}' y='{y:.1f}' font-size='{size}' fill='{color}' text-anchor='{anchor}'{style}>{s}</text>"


def room(name, x, y, kind=None):
    """A room as a box around its centre (x, y): dark, the goal, or plain."""
    fill, stroke, width, ink = PALE, INK, 2, INK
    if kind == 'dark':
        fill, ink = DARK, LIGHT
    elif kind == 'goal':
        stroke, width = BLUE, 4
    return (f"<rect x='{x - BOX_W / 2:.1f}' y='{y - BOX_H / 2:.1f}' width='{BOX_W}' height='{BOX_H}' rx='10' "
            f"fill='{fill}' stroke='{stroke}' stroke-width='{width}'/>" + text(x, y + 7, name, 21, ink))


def border(cx, cy, dx, dy):
    """Where the ray from inside the box at (cx, cy) along (dx, dy) leaves it."""
    tx = BOX_W / 2 / abs(dx) if dx else math.inf
    ty = BOX_H / 2 / abs(dy) if dy else math.inf
    t = min(tx, ty)
    return cx + dx * t, cy + dy * t


def connect(a, b, direction, locked=False, offset=9, label_at=0.3):
    """One connect: an arrow from room a to room b, beside the line between
    their centres on its right-hand side, so that the way back is a second
    arrow beside it. Its tip is on b's border; the direction stands beside
    its first part, a locked door's arrow is amber."""
    (ax, ay), (bx, by) = a, b
    n = math.hypot(bx - ax, by - ay)
    ux, uy = (bx - ax) / n, (by - ay) / n
    nx, ny = -uy, ux
    sx, sy = border(ax + nx * offset, ay + ny * offset, ux, uy)
    ex, ey = border(bx + nx * offset, by + ny * offset, -ux, -uy)
    color = AMBER if locked else INK
    head = 12
    parts = [f"<line x1='{sx:.1f}' y1='{sy:.1f}' x2='{ex - ux * (head - 1):.1f}' y2='{ey - uy * (head - 1):.1f}' "
             f"stroke='{color}' stroke-width='2.2'/>",
             ap.arrowhead(ex, ey, ux, uy, color, head, 6)]
    # the label further out on the same side, near the start
    lx = sx + (ex - sx) * label_at + nx * 16
    ly = sy + (ey - sy) * label_at + ny * 16
    if abs(nx) > abs(ny):
        anchor = 'start' if nx > 0 else 'end'
        ly += 6
    else:
        anchor = 'middle'
        ly += 12 if ny > 0 else -2
    parts.append(code(direction, lx, ly, size=16, anchor=anchor, background=False))
    if locked:
        parts.append(text(lx, ly + 19, 'verschlossen', 14, AMBER, anchor, italic=True))
    return ''.join(parts)


def schule_karte():
    w, h = 980, 700
    at = {
        'Lehrerzimmer': (490, 60),
        'Flur': (490, 220),
        'Raum 204': (820, 220),
        'Treppenhaus': (490, 380),
        'Hausmeisterraum': (160, 520),
        'Eingangshalle': (490, 520),
        'Keller': (820, 520),
        'Schulhof': (490, 650),
    }
    parts = []
    # as in starter/schule.rb, one arrow per connect
    for a, b, direction, locked in [
        ('Raum 204', 'Flur', 'westen', False), ('Flur', 'Raum 204', 'osten', False),
        ('Flur', 'Lehrerzimmer', 'norden', True), ('Lehrerzimmer', 'Flur', 'süden', False),
        ('Flur', 'Treppenhaus', 'unten', False), ('Treppenhaus', 'Flur', 'oben', False),
        ('Treppenhaus', 'Eingangshalle', 'unten', False), ('Eingangshalle', 'Treppenhaus', 'oben', False),
        ('Eingangshalle', 'Hausmeisterraum', 'westen', False), ('Hausmeisterraum', 'Eingangshalle', 'osten', False),
        ('Eingangshalle', 'Keller', 'unten', False), ('Keller', 'Eingangshalle', 'oben', False),
        ('Eingangshalle', 'Schulhof', 'süden', True),
    ]:
        # a way without a way back runs through the middle
        parts.append(connect(at[a], at[b], direction, locked, offset=0 if b == 'Schulhof' else 9))
    kinds = {'Keller': 'dark', 'Schulhof': 'goal'}
    for name, (x, y) in at.items():
        parts.append(room(name, x, y, kinds.get(name)))
    save('schule-karte', w, h, 'Die Nacht in der Schule: jeder connect ein Pfeil', parts)


def welt_karte():
    w, h = 980, 330
    at = {
        'Zimmer 3': (160, 90),
        'Gang': (490, 90),
        'Küche': (820, 90),
        'Sicherungskasten': (490, 260),
    }
    parts = []
    for a, b, direction, locked in [
        ('Zimmer 3', 'Gang', 'flur', False), ('Gang', 'Zimmer 3', 'zimmer', False),
        ('Gang', 'Küche', 'küche', False), ('Küche', 'Gang', 'flur', False),
        ('Gang', 'Sicherungskasten', 'kasten', True),
    ]:
        parts.append(connect(at[a], at[b], direction, locked, offset=0 if b == 'Sicherungskasten' else 9))
    kinds = {'Gang': 'dark', 'Sicherungskasten': 'goal'}
    for name, (x, y) in at.items():
        parts.append(room(name, x, y, kinds.get(name)))
    save('welt-karte', w, h, 'Die kleine Welt von »Deine Welt«: jeder connect ein Pfeil', parts)


def eine_zeile(name, exit_to):
    w, h = 1000, 600
    columns = [('Spielschleife', None), ('game', 'Game'), ('player', 'Player'),
               ('Raum 204', 'Room'), ('Flur', 'Room')]
    xs = [95, 320, 530, 730, 910]
    top, bottom = 30, 570
    parts = []
    for (title, cls), x in zip(columns, xs):
        parts.append(f"<line x1='{x}' y1='{top + 60}' x2='{x}' y2='{bottom}' stroke='{GREY}' "
                     f"stroke-width='1.5' stroke-dasharray='6 6'/>")
    for (title, cls), x in zip(columns, xs):
        bw = 150
        parts.append(f"<rect x='{x - bw / 2}' y='{top}' width='{bw}' height='60' rx='10' fill='{PALE}' "
                     f"stroke='{INK}' stroke-width='2'/>")
        if cls:
            parts.append(text(x, top + 27, title, 19))
            parts.append(text(x, top + 49, cls, 15, GREY))
        else:
            parts.append(text(x, top + 37, title, 19))

    def call(i, j, y, label, back=False):
        """A call from column i to column j at height y (a return: dashed,
        grey); the label above it, in the middle, on its box, so that no
        lifeline runs through it."""
        x1, x2 = xs[i], xs[j]
        d = 1 if x2 > x1 else -1
        color = GREY if back else INK
        dash = " stroke-dasharray='7 5'" if back else ''
        out = [f"<line x1='{x1}' y1='{y}' x2='{x2 - d * 11}' y2='{y}' stroke='{color}' stroke-width='2'{dash}/>",
               ap.arrowhead(x2, y, d, 0, color, 12, 6)]
        out.append(code(label, (x1 + x2) / 2, y - 9, size=15, anchor='middle'))
        return ''.join(out)

    def self_call(i, y, label):
        x = xs[i]
        out = [f"<path d='M {x} {y} h 46 v 30 h -35' fill='none' stroke='{INK}' stroke-width='2'/>",
               ap.arrowhead(x, y + 30, -1, 0, INK, 12, 6),
               code(label, x + 56, y + 20, size=15, anchor='start')]
        return ''.join(out)

    y = 130
    parts.append(call(0, 1, y, 'handle("gehe westen")'))
    parts.append(call(1, 2, y + 50, 'go("westen")'))
    parts.append(call(2, 3, y + 100, f'{exit_to}("westen")'))
    parts.append(call(3, 2, y + 150, 'Flur', back=True))
    parts.append(self_call(2, y + 185, 'look()'))
    parts.append(call(2, 4, y + 270, 'describe()'))
    parts.append(call(4, 2, y + 320, '"Flur\\nNur die …"', back=True))
    parts.append(call(2, 1, y + 370, '"Flur\\nNur die …"', back=True))
    parts.append(call(1, 0, y + 420, '"Flur\\nNur die …"', back=True))
    save(name, w, h, 'Was mit »gehe westen« geschieht, Aufruf für Aufruf', parts)


schule_karte()
welt_karte()
eine_zeile('eine-zeile', 'exit_to')
eine_zeile('eine-zeile-camel', 'exitTo')
