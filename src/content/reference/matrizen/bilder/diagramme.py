"""The drawings of the Handbuch page »Matrizen«: what a 2 × 2 matrix does to
a letter F (turn, stretch, mirror, shear), and that the order of turning and
moving matters.

    python3 src/content/reference/matrizen/bilder/diagramme.py
    for f in src/content/reference/matrizen/*.svg; do
        case $f in *-dark.svg) ;; *) ruby src/scripts/render-svg.rb --svg $f;; esac; done

The page shows the SVGs themselves, sharp at every size; --svg writes the
dark one beside each.

Each picture: a grid, the F before (grey, dashed) and after (blue), and the
images of the two axes, (1, 0) in red and (0, 1) in green, which are the
columns of the matrix. y points up, as in the formulas. One unit of the
viewBox is one CSS pixel at the width the page gives the picture.
"""

import math as m
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', '..', 'scripts'))
from diagram_labels import math as tex  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, BLUE, GREY, LINE, PALE, WHITE = '#1f2937', '#2b5fb4', '#6b7280', '#d1d5db', '#eef3fb', '#ffffff'
RED, GREEN = '#dc2626', '#16a34a'

# the pictures stand two in a row in a gallery, which stretches them to
# about 530 px: drawn at that size, so that the labels (17 px) are not
# enlarged with them; on a phone, one below the other, they shrink to
# about 0.7, which leaves the labels at 12 px
ZOOM = 1.9
W, H = round(280 * ZOOM), round(230 * ZOOM)
UNIT = 38 * ZOOM               # pixels per unit
OX, OY = W / 2, 172 * ZOOM     # where the origin is: low, the F stands above it

# an F, asymmetric so that turning and mirroring show
F = [(0, 0), (0.5, 0), (0.5, 1), (1.2, 1), (1.2, 1.5), (0.5, 1.5), (0.5, 2), (1.5, 2), (1.5, 2.5), (0, 2.5)]


def px(p):
    return OX + p[0] * UNIT, OY - p[1] * UNIT


def apply(matrix, p, shift=(0, 0)):
    (a, b), (c, d) = matrix
    return a * p[0] + b * p[1] + shift[0], c * p[0] + d * p[1] + shift[1]


def head(comment, name):
    markers = ''.join(
        f'<marker id="arrow-{n}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" markerHeight="6" orient="auto-start-reverse">'
        f'<path d="M0,0 L10,5 L0,10 z" fill="{col}"/></marker>' for n, col in [('red', RED), ('green', GREEN), ('ink', INK)])
    return (f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
            f'viewBox="0 0 {W} {H}" width="{W}" height="{H}" font-family="IBM Plex Sans, DejaVu Sans, sans-serif">\n'
            f'  <!-- {comment}\n'
            f'       Gemacht von bilder/diagramme.py; die dunkle Fassung\n'
            f'       {name}-dark.svg und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <defs>{markers}</defs>\n'
            f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n')


def grid():
    s = ''
    for k in range(-4, 5):
        x, _ = px((k, 0))
        _, y = px((0, k))
        s += f'  <line x1="{x:.1f}" y1="0" x2="{x:.1f}" y2="{H}" stroke="{LINE}" stroke-width="1"/>\n'
        s += f'  <line x1="0" y1="{y:.1f}" x2="{W}" y2="{y:.1f}" stroke="{LINE}" stroke-width="1"/>\n'
    s += f'  <line x1="0" y1="{OY}" x2="{W - 3}" y2="{OY}" stroke="{GREY}" stroke-width="1.2" marker-end="url(#arrow-ink)"/>\n'
    s += f'  <line x1="{OX}" y1="{H}" x2="{OX}" y2="3" stroke="{GREY}" stroke-width="1.2" marker-end="url(#arrow-ink)"/>\n'
    # the names of the axes, at their arrowheads
    s += '  ' + tex('x', W - 8, OY - 9, 17, INK, 'end') + '\n'
    s += '  ' + tex('y', OX + 9, 17, 17, INK, 'start') + '\n'
    return s


def shape(points, fill, stroke, dash=None, width=2.2):
    d = f' stroke-dasharray="{dash}"' if dash else ''
    pts = ' '.join(f'{x:.1f},{y:.1f}' for x, y in map(px, points))
    return f'  <polygon points="{pts}" fill="{fill}" stroke="{stroke}" stroke-width="{width}" stroke-linejoin="round"{d}/>\n'


def arrow(p, color, name, start=(0, 0)):
    x1, y1 = px(start)
    x2, y2 = px(p)
    return f'  <line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" stroke="{color}" stroke-width="2.6" marker-end="url(#arrow-{name})"/>\n'


def save(name, svg):
    with open(os.path.join(OUT, name + '.svg'), 'w') as f:
        f.write(svg + '</svg>\n')


def one(name, matrix, comment):
    s = head(comment, name) + grid()
    s += shape(F, 'none', GREY, '5 4', 1.6)
    s += shape([apply(matrix, p) for p in F], PALE, BLUE)
    (a, b), (c, d) = matrix
    s += arrow((a, c), RED, 'red')
    s += arrow((b, d), GREEN, 'green')
    save(name, s)


def order(name, first_turn, comment):
    """turn by 90° and move by (2, 0), in one order or the other"""
    turn = ((0, -1), (1, 0))
    s = head(comment, name) + grid()
    s += shape(F, 'none', GREY, '5 4', 1.6)
    if first_turn:
        middle = [apply(turn, p) for p in F]
        end = [(x + 2, y) for x, y in middle]
    else:
        middle = [(x + 2, y) for x, y in F]
        end = [apply(turn, p) for p in middle]
    s += shape(middle, 'none', GREY, '2 4', 1.4)
    s += shape(end, PALE, BLUE)
    save(name, s)


if __name__ == '__main__':
    c, si = m.cos(m.radians(45)), m.sin(m.radians(45))
    one('drehen', ((c, -si), (si, c)),
        'Ein F, um 45 Grad gegen den Uhrzeigersinn gedreht; rot und grün die gedrehten Achsen, die Spalten der Matrix.')
    one('strecken', ((1.8, 0), (0, 0.6)),
        'Ein F, in x um 1,8 gestreckt, in y auf 0,6 gestaucht.')
    one('spiegeln', ((-1, 0), (0, 1)),
        'Ein F, an der y-Achse gespiegelt: die rote Achse zeigt nach links.')
    one('scheren', ((1, 0.6), (0, 1)),
        'Ein F, geschert: je höher ein Punkt, desto weiter rückt er nach rechts; die grüne Achse ist schräg.')
    order('drehen-verschieben', True,
          'Ein F, erst um 90 Grad gedreht (gepunktet), dann um 2 nach rechts verschoben (blau).')
    order('verschieben-drehen', False,
          'Ein F, erst um 2 nach rechts verschoben (gepunktet), dann um 90 Grad um den Ursprung gedreht (blau): Es landet oben.')


# ------------------------------------------------------------- the same in 3D

PURPLE = '#7c3aed'
AMBER = '#d97706'
ZOOM3 = 1.75                  # as ZOOM: two in a row
W3, H3 = round(300 * ZOOM3), round(260 * ZOOM3)
YAW, PITCH = m.radians(24), m.radians(16)
U3 = 46 * ZOOM3               # pixels per unit
O3 = (W3 / 2 - 20 * ZOOM3, H3 - 58 * ZOOM3)


def view(p):
    """orthographic, turned by YAW around y and tilted by PITCH: x to the
    right, y up, z towards the viewer, seen a little from the right above"""
    x, y, z = p
    x, z = x * m.cos(YAW) - z * m.sin(YAW), x * m.sin(YAW) + z * m.cos(YAW)
    y, z = y * m.cos(PITCH) - z * m.sin(PITCH), y * m.sin(PITCH) + z * m.cos(PITCH)
    return O3[0] + x * U3, O3[1] - y * U3, z


def block(depth=0.45, off=0.4):
    """the F as a block: its faces as lists of 3D points; moved off the
    axes a little, so that none of its edges lies on an axis"""
    front = [(x + off, y + off, depth) for x, y in F]
    back = [(x + off, y + off, 0) for x, y in F]
    faces = [front, back[::-1]]
    for i in range(len(F)):
        a, b = F[i], F[(i + 1) % len(F)]
        a, b = (a[0] + off, a[1] + off), (b[0] + off, b[1] + off)
        faces.append([(a[0], a[1], 0), (b[0], b[1], 0), (b[0], b[1], depth), (a[0], a[1], depth)])
    return faces


def transform3(matrix, p):
    return tuple(sum(matrix[r][c] * p[c] for c in range(3)) for r in range(3))


def head3(comment, name):
    markers = ''.join(
        f'<marker id="arrow-{n}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" markerHeight="6" orient="auto-start-reverse">'
        f'<path d="M0,0 L10,5 L0,10 z" fill="{col}"/></marker>' for n, col in [('red', RED), ('green', GREEN), ('blue', BLUE), ('amber', AMBER)])
    return (f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
            f'viewBox="0 0 {W3} {H3}" width="{W3}" height="{H3}" font-family="IBM Plex Sans, DejaVu Sans, sans-serif">\n'
            f'  <!-- {comment}\n'
            f'       Gemacht von bilder/diagramme.py; die dunkle Fassung\n'
            f'       {name}-dark.svg und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <defs>{markers}</defs>\n'
            f'  <rect width="{W3}" height="{H3}" fill="{WHITE}"/>\n')


def axes3(occluders):
    s = ''
    for (d, color, name, t) in [((3.0, 0, 0), RED, 'red', 'x'), ((0, 3.6, 0), GREEN, 'green', 'y'), ((0, 0, 2.6), BLUE, 'blue', 'z')]:
        s += line3((0, 0, 0), d, occluders, color, 1.6, arrow=name)
        x2, y2, _ = view(d)
        s += '  ' + tex(t, x2 + (8 if t != 'y' else 9), y2 + (14 if t == 'x' else 4), 17, INK, 'start') + '\n'
    return s


def fills(faces, color, ghost=False):
    """painter's algorithm: the faces from far to near, without edges; lit
    faces paler"""
    light = (-0.4, 0.8, 0.45)
    s = ''
    drawn = []
    for face in faces:
        pts = [view(p) for p in face]
        depth = sum(p[2] for p in pts) / len(pts)
        # the face's normal in world space (Newell), for its brightness
        nx = ny = nz = 0
        for i in range(len(face)):
            a, b = face[i], face[(i + 1) % len(face)]
            nx += (a[1] - b[1]) * (a[2] + b[2])
            ny += (a[2] - b[2]) * (a[0] + b[0])
            nz += (a[0] - b[0]) * (a[1] + b[1])
        length = m.sqrt(nx * nx + ny * ny + nz * nz) or 1
        lit = max(0, (nx * light[0] + ny * light[1] + nz * light[2]) / length)
        drawn.append((depth, pts, lit))
    for depth, pts, lit in sorted(drawn, key=lambda d: d[0]):
        poly = ' '.join(f'{x:.1f},{y:.1f}' for x, y, _ in pts)
        opacity = 0.08 if ghost else 0.55 - 0.42 * lit
        # on the dark page the blue is lighter: thinner, so edges stay readable
        dark = 0.08 if ghost else 0.32 - 0.24 * lit
        s += (f'  <polygon points="{poly}" fill="{color}" fill-opacity="{opacity:.2f}" '
              f'data-dark-fill-opacity="{dark:.2f}"/>\n')
    return s


def covered(point, faces):
    """whether a face lies in front of this point (in screen coordinates
    with depth, larger depth nearer)"""
    px, py, pz = point
    for face in faces:
        pts = [view(q) for q in face]
        inside = False
        for i in range(len(pts)):
            (x1, y1, _), (x2, y2, _) = pts[i], pts[i - 1]
            if (y1 > py) != (y2 > py) and px < x1 + (py - y1) * (x2 - x1) / (y2 - y1):
                inside = not inside
        if not inside:
            continue
        # the face's depth at this point: its plane through three corners
        (ax, ay, az), (bx, by, bz) = pts[0], pts[1]
        for cx, cy, cz in pts[2:]:
            det = (bx - ax) * (cy - ay) - (cx - ax) * (by - ay)
            if abs(det) > 1e-6:
                break
        else:
            continue                       # seen edge on
        u = ((px - ax) * (cy - ay) - (cx - ax) * (py - ay)) / det
        v = ((bx - ax) * (py - ay) - (px - ax) * (by - ay)) / det
        if az + u * (bz - az) + v * (cz - az) > pz + 1e-4:
            return True
    return False


def split(a, b, occluders, steps=60):
    """the stretch from a to b (3D) on screen, in runs: (t0, t1, hidden)"""
    pa, pb = view(a), view(b)
    runs, state, begin = [], None, 0
    for i in range(steps):
        t = (i + 0.5) / steps
        hide = covered(tuple(pa[c] + t * (pb[c] - pa[c]) for c in range(3)), occluders)
        if hide != state:
            if state is not None:
                runs.append((begin, i / steps, state))
            state, begin = hide, i / steps
    runs.append((begin, 1, state))
    return pa, pb, runs


def stroke(pa, pb, t0, t1, color, width, dash=None, extra=''):
    x1, y1 = pa[0] + t0 * (pb[0] - pa[0]), pa[1] + t0 * (pb[1] - pa[1])
    x2, y2 = pa[0] + t1 * (pb[0] - pa[0]), pa[1] + t1 * (pb[1] - pa[1])
    d = f' stroke-dasharray="{dash}"' if dash else ''
    if dash and dash.startswith('0'):
        d += ' stroke-linecap="round"'
    return (f'  <line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" stroke="{color}" '
            f'stroke-width="{width:.1f}"{d}{extra}/>\n')


def line3(a, b, occluders, color, width, seen=None, hidden='3 2.5', arrow=None):
    """a line in space, dashed (or dotted) where faces cover it; the arrow
    head on its last run"""
    pa, pb, runs = split(a, b, occluders)
    s = ''
    for i, (t0, t1, hide) in enumerate(runs):
        head = f' marker-end="url(#arrow-{arrow})"' if arrow and i == len(runs) - 1 else ''
        s += stroke(pa, pb, t0, t1, color, width * (0.8 if hide else 1), hidden if hide else seen, head)
    return s


def edges(faces, occluders, color, seen, hidden, width=1.2):
    """every edge of the faces, split where other faces cover it: the
    visible parts with the stroke style seen, the covered ones with hidden
    (a stroke-dasharray, or None for a solid line)"""
    unique = {}
    for face in faces:
        for i in range(len(face)):
            a, b = face[i - 1], face[i]
            key = tuple(sorted([tuple(round(c, 6) for c in a), tuple(round(c, 6) for c in b)]))
            unique[key] = (a, b)
    s = ''
    for hide in (True, False):
        for a, b in unique.values():
            pa, pb, runs = split(a, b, occluders)
            for t0, t1, h in runs:
                if h == hide:
                    s += stroke(pa, pb, t0, t1, color, width * (0.8 if hide else 1), hidden if hide else seen,
                                ' stroke-opacity="0.7"' if hide else '')
    return s


def one3(name, matrix, comment, axis=None):
    """first all faces, then all lines on top: every line solid where it
    is seen and dashed where a face covers it; the grey block before
    dashed where seen and dotted where covered"""
    before = block()
    after = [[transform3(matrix, p) for p in face] for face in block()]
    s = head3(comment, name)
    s += fills(before, GREY, ghost=True)
    s += fills(after, BLUE)
    s += axes3(after)
    if axis:
        k = axis
        s += line3(tuple(-2.9 * c for c in k), tuple(2.9 * c for c in k), after, AMBER, 2.2,
                   seen='7 4', hidden='0.1 3.5', arrow='amber')
        x2, y2, _ = view(tuple(2.9 * c for c in k))
        s += '  ' + tex('k', x2 + 6, y2 - 4, 17, AMBER, 'start') + '\n'
    s += edges(before, before + after, GREY, '4 3', '0.1 2.6', 1.1)
    s += edges(after, after, BLUE, None, '3 2.5')
    save(name, s)


def rodrigues(k, angle):
    """the matrix that turns by angle around the axis k (length 1)"""
    c, s_, t = m.cos(angle), m.sin(angle), 1 - m.cos(angle)
    x, y, z = k
    return [[c + x * x * t, x * y * t - z * s_, x * z * t + y * s_],
            [y * x * t + z * s_, c + y * y * t, y * z * t - x * s_],
            [z * x * t - y * s_, z * y * t + x * s_, c + z * z * t]]


def all3():
    a = m.radians(40)
    one3('drehen-3d', [[m.cos(a), 0, m.sin(a)], [0, 1, 0], [-m.sin(a), 0, m.cos(a)]],
         'Das F als Block, um 40 Grad um die y-Achse gedreht; grau gestrichelt der Block vorher, verdeckte Kanten gepunktet.')
    one3('strecken-3d', [[1.5, 0, 0], [0, 0.6, 0], [0, 0, 1.6]],
         'Das F als Block, in x und z gestreckt, in y gestaucht.')
    one3('spiegeln-3d', [[-1, 0, 0], [0, 1, 0], [0, 0, 1]],
         'Das F als Block, an der Ebene aus y- und z-Achse gespiegelt: Seine Arme zeigen nach -x.')
    one3('scheren-3d', [[1, 0.6, 0], [0, 1, 0], [0, 0, 1]],
         'Das F als Block, geschert: je höher, desto weiter nach rechts.')
    k = (1 / m.sqrt(3), 1 / m.sqrt(3), 1 / m.sqrt(3))
    one3('achse-3d', rodrigues(k, m.radians(60)),
         'Das F als Block, um 60 Grad um die Achse k = (1, 1, 1) gedreht, die Diagonale zwischen den drei Achsen (orange gestrichelt).', axis=k)


if __name__ == '__main__':
    all3()
