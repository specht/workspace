"""The drawings of the Handbuch page »Gradientenabstieg«: the parabola
E(c) = (c − 0.7)² with four steps of gradient descent from c = 1.4, for the
learning rates 0.05, 0.2, 1.0 and 1.1, all on the same axes, so that the
steps can be compared.

    python3 src/content/reference/gradientenabstieg/bilder/diagramme.py
    for f in src/content/reference/gradientenabstieg/lernrate-*.svg; do
        case $f in *-dark.svg) ;; *) ruby src/scripts/render-svg.rb $f;; esac; done

The four stand in a gallery of two columns, which stretches each to about
530 px: drawn at that width, so that the labels stay as large as the text.
Each step is an arc beside the parabola (a chord would lie on the curve),
the points numbered by their step.
"""

import math as m
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', '..', 'scripts'))
from diagram_labels import math as tex  # noqa: E402  (also moves the script into docker/drawings)

OUT = os.path.join(HERE, '..')
INK, BLUE, GREY, LINE, WHITE = '#1f2937', '#2b5fb4', '#6b7280', '#d1d5db', '#ffffff'
W, H = 530, 330
C0, C1 = -0.75, 2.3            # the c the picture shows
E1 = 2.25                      # the highest E it shows
OX = 40                        # left edge of the plot
OY = H - 52                    # where E = 0 is
R = 5                          # radius of a dot
GAP = R + 1.5                  # where an arc starts and its tip ends, from a dot's centre
HEAD = 7 * 1.6                 # length of an arrowhead: markerWidth times stroke width
SX = (W - OX - 30) / (C1 - C0)
SY = (OY - 26) / E1


def X(c):
    return OX + (c - C0) * SX


def Y(e):
    return OY - e * SY


def bez(t, a, b, c):
    """a quadratic Bezier curve at t, one coordinate"""
    return (1 - t) ** 2 * a + 2 * (1 - t) * t * b + t ** 2 * c


def save(name, svg):
    with open(os.path.join(OUT, name + '.svg'), 'w') as f:
        f.write(svg + '</svg>\n')
    print(name)


def picture(rate, name, comment):
    s = (f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
         f'viewBox="0 0 {W} {H}" width="{W}" height="{H}" font-family="IBM Plex Sans, DejaVu Sans, sans-serif">\n'
         f'  <!-- {comment}\n'
         f'       Gemacht von bilder/diagramme.py; die dunkle Fassung {name}-dark.svg und die Schriften:\n'
         f'       src/scripts/render-svg.rb -->\n'
         f'  <defs><marker id="arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">'
         f'<path d="M0,0 L10,5 L0,10 z" fill="{INK}"/></marker>'
         f'<marker id="arrow-grey" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">'
         f'<path d="M0,0 L10,5 L0,10 z" fill="{GREY}"/></marker></defs>\n'
         f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n')
    # the axes: c to the right, E up, through c = 0
    s += f'  <line x1="{X(C0):.1f}" y1="{OY}" x2="{X(C1) + 14:.1f}" y2="{OY}" stroke="{GREY}" stroke-width="1.3" marker-end="url(#arrow-grey)"/>\n'
    s += f'  <line x1="{X(0):.1f}" y1="{OY + 6}" x2="{X(0):.1f}" y2="{Y(E1) - 10:.1f}" stroke="{GREY}" stroke-width="1.3" marker-end="url(#arrow-grey)"/>\n'
    s += '  ' + tex('c', X(C1) + 12, OY + 24, 17, INK, 'end') + '\n'
    s += '  ' + tex('E', X(0) + 10, Y(E1) + 4, 17, INK, 'start') + '\n'
    for c, t in [(0, '0'), (0.7, r'0{,}7'), (1.4, r'1{,}4')]:
        s += f'  <line x1="{X(c):.1f}" y1="{OY - 4}" x2="{X(c):.1f}" y2="{OY + 4}" stroke="{GREY}" stroke-width="1.3"/>\n'
        s += '  ' + tex(t, X(c), OY + 26, 16, INK, 'middle') + '\n'
    # the parabola, as far as it fits
    pts = [(X(c / 100), Y((c / 100 - 0.7) ** 2)) for c in range(int(C0 * 100), int(C1 * 100) + 1)]
    pts = [p for p in pts if p[1] >= Y(E1) - 2]
    s += f'  <polyline points="{" ".join(f"{x:.1f},{y:.1f}" for x, y in pts)}" fill="none" stroke="{BLUE}" stroke-width="3"/>\n'
    # four steps from c = 1.4
    c = 1.4
    cs = [c]
    for _ in range(4):
        c = c - rate * 2 * (c - 0.7)
        cs.append(c)
    P = [(X(c), Y((c - 0.7) ** 2)) for c in cs]
    # the arcs: first their control points, then where they start and end
    arcs = []
    sides = {}          # how many arcs bow to each side of the same two dots
    for (xa, ya), (xb, yb) in zip(P, P[1:]):
        dx, dy = xb - xa, yb - ya
        d = m.hypot(dx, dy)
        if d < 40:          # too short for an arc: the dots alone
            continue
        if any(arc['from'] == (xa, ya) and arc['to'] == (xb, yb) for arc in arcs):
            continue        # the same step again: the labels say it repeats
        ux, uy = dx / d, dy / d
        # bow away from the valley; a step back between the same two dots
        # bows to the other side, into the parabola, so that what arrives
        # at a dot and what leaves it come from different sides; each further
        # arc on the same side a little wider
        nx, ny = -uy, ux
        if ny > 0:
            nx, ny = -nx, -ny
        pair = (min((xa, ya), (xb, yb)), max((xa, ya), (xb, yb)))
        under = any(arc['from'] == (xb, yb) and arc['to'] == (xa, ya) and not arc['under'] for arc in arcs) \
            and not any(arc['from'] == (xa, ya) and arc['to'] == (xb, yb) and not arc['under'] for arc in arcs)
        if under:
            nx, ny = -nx, -ny
        j = sides.get((pair, under), 0)
        sides[(pair, under)] = j + 1
        bow = min(0.25 * d + 4, 60) + 14 * j
        arcs.append({'from': (xa, ya), 'to': (xb, yb), 'under': under,
                     'c': ((xa + xb) / 2 + nx * bow, (ya + yb) / 2 + ny * bow)})

    def off(px, py, qx, qy):
        """the point GAP from the centre (px, py) towards (qx, qy)"""
        length = m.hypot(qx - px, qy - py)
        return px + (qx - px) / length * GAP, py + (qy - py) / length * GAP

    def angle(p, q):
        return m.atan2(q[1] - p[1], q[0] - p[0])

    for arc in arcs:
        (xa, ya), (xb, yb), (cx, cy) = arc['from'], arc['to'], arc['c']
        # the curve leaves and reaches each dot along the line through its
        # centre: the head aims at the centre, its tip just before the outline
        x1, y1 = off(xa, ya, cx, cy)
        x2, y2 = off(xb, yb, cx, cy)
        # where the head of an arc arriving at this dot lies over the start
        # (a head is half as wide as long: from about 21 degrees between the
        # two on, they stay apart), the start moves forwards along the curve,
        # past the head
        heads = [other['c'] for other in arcs if other['to'] == (xa, ya)]
        if any(abs((angle((xa, ya), h) - angle((xa, ya), (cx, cy)) + m.pi) % (2 * m.pi) - m.pi) < m.radians(30)
               for h in heads):
            t = 0.0
            while t < 0.5 and m.hypot(bez(t, x1, cx, x2) - xa, bez(t, y1, cy, y2) - ya) < GAP + HEAD + 3:
                t += 0.005
            # the rest of the curve from t on: same shape, a later start
            x1, y1 = bez(t, x1, cx, x2), bez(t, y1, cy, y2)
            cx, cy = cx + (x2 - cx) * t, cy + (y2 - cy) * t
        s += (f'  <path d="M{x1:.1f},{y1:.1f} Q{cx:.1f},{cy:.1f} {x2:.1f},{y2:.1f}" fill="none" stroke="{INK}" '
              f'stroke-width="1.6" marker-end="url(#arrow)"/>\n')
    # the points, numbered; points on the same place share one label, and
    # where they crowd, only the first, the last and those with room get one
    placed = {}
    for k, (x, y) in enumerate(P):
        key = (round(x), round(y))
        placed.setdefault(key, []).append(k)
    spots = list(placed.items())
    labelled = [spots[0][0], spots[-1][0]]
    for spot, _ in spots[1:-1]:
        if all(m.hypot(spot[0] - a, spot[1] - b) >= 28 for a, b in labelled):
            labelled.append(spot)
    for (x, y), ks in spots:
        s += f'  <circle cx="{x}" cy="{y}" r="{R}" fill="{INK}"/>\n'
        if (x, y) not in labelled:
            continue
        right = x < X(0.7)
        s += '  ' + tex(', '.join(str(k) for k in ks), x + (-12 if right else 12), y + 20, 15, INK, 'end' if right else 'start') + '\n'
    save(name, s)


if __name__ == '__main__':
    picture(0.05, 'lernrate-0-05', 'Lernrate 0,05: vier kleine Schritte von 1,4 aus, alle noch weit rechts vom Tiefpunkt.')
    picture(0.2, 'lernrate-0-2', 'Lernrate 0,2: vier Schritte von 1,4 aus, jeder kleiner, nah an den Tiefpunkt bei 0,7.')
    picture(1.0, 'lernrate-1-0', 'Lernrate 1,0: von 1,4 nach 0 und zurück, immer hin und her, ohne anzukommen.')
    picture(1.1, 'lernrate-1-1', 'Lernrate 1,1: über den Tiefpunkt hinweg, jeder Sprung weiter, die Parabel hinauf.')
