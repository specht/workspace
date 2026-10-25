"""The diagrams of »Raycasting in C«, as SVG beside the page.

    python3 src/content/raycaster/bilder/diagramme.py            # all
    python3 src/content/raycaster/bilder/diagramme.py dda dda-t  # some
    ruby src/scripts/render-svg.rb src/content/raycaster/NAME.svg WIDTH

Each picture is drawn at the size it is shown: one unit of the viewBox is
one CSS pixel at the width the page gives it (style='width: …em', the
viewBox width / 16), so labels at 16 are about as big as the text. Labels
read like the page: formulas through math() (LaTeX), names from the program
through code(), the right angle through right_angle() (diagram_labels.py).

Axes: the map and the screen have y pointing down, as in the code (map[y][x],
set_pixel(x, y)) and as the program draws the map; the pictures from above
are drawn the same way. The pictures from the side have the height going up.
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import math as tex, code, right_angle, _typeset  # noqa: E402
import re  # noqa: E402
from PIL import ImageFont  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, BLUE, GREY, RED, GREEN = '#1f2937', '#2b5fb4', '#6b7280', '#dc2626', '#16a34a'
LINE, PALE, WHITE, LIGHT = '#d1d5db', '#eef3fb', '#ffffff', '#f3f4f6'
FS = 16      # text
CS = 15      # code
MS = 17      # formulas
FONT = '/usr/share/fonts/truetype/ibm-plex/IBMPlexSans-Regular.ttf'
_fonts = {}


# ------------------------------------------------------------------ basics

def head(w, h, comment, name):
    marks = ''.join(
        f'    <marker id="a-{n}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" markerHeight="6" '
        f'orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="{c}"/></marker>\n'
        for n, c in [('ink', INK), ('blue', BLUE), ('grey', GREY), ('red', RED), ('green', GREEN)])
    return (f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
            f'viewBox="0 0 {w} {h}" width="{w}" height="{h}" '
            f'font-family="IBM Plex Sans, DejaVu Sans, sans-serif">\n'
            f'  <!-- {comment}\n'
            f'       Gemacht von bilder/diagramme.py; als Bild, hell und dunkel:\n'
            f'       src/scripts/render-svg.rb src/content/raycaster/{name}.svg {2 * w} -->\n'
            f'  <defs>\n{marks}  </defs>\n'
            f'  <rect width="{w}" height="{h}" fill="{WHITE}"/>\n')


NAMES = {INK: 'ink', BLUE: 'blue', GREY: 'grey', RED: 'red', GREEN: 'green'}


def arrow(x1, y1, x2, y2, color, width=2.5, dash=None):
    d = f' stroke-dasharray="{dash}"' if dash else ''
    return (f'  <line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" stroke="{color}" '
            f'stroke-width="{width}"{d} marker-end="url(#a-{NAMES[color]})"/>\n')


def arrow_to(x1, y1, x2, y2, gap, color, width=2.5, dash=None):
    """An arrow whose tip stops gap before (x2, y2), on the border of a dot there."""
    n = math.hypot(x2 - x1, y2 - y1)
    return arrow(x1, y1, x2 - (x2 - x1) / n * gap, y2 - (y2 - y1) / n * gap, color, width, dash)


def line(x1, y1, x2, y2, color, width=2, dash=None, cap=''):
    d = f' stroke-dasharray="{dash}"' if dash else ''
    c = f' stroke-linecap="{cap}"' if cap else ''
    return (f'  <line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" stroke="{color}" '
            f'stroke-width="{width}"{d}{c}/>\n')


def circle(x, y, r, fill, stroke=INK, width=2, dash=None):
    d = f' stroke-dasharray="{dash}"' if dash else ''
    return (f'  <circle cx="{x:.1f}" cy="{y:.1f}" r="{r:.1f}" fill="{fill}" stroke="{stroke}" '
            f'stroke-width="{width}"{d}/>\n')


def rect(x, y, w, h, fill, stroke='none', width=1.5, dash=None):
    d = f' stroke-dasharray="{dash}"' if dash else ''
    return (f'  <rect x="{x:.1f}" y="{y:.1f}" width="{w:.1f}" height="{h:.1f}" fill="{fill}" '
            f'stroke="{stroke}" stroke-width="{width}"{d}/>\n')


def text(x, y, s, color=INK, size=FS, anchor='middle'):
    return (f'  <text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" '
            f'text-anchor="{anchor}">{s}</text>\n')


def text_width(s, size=FS):
    if size not in _fonts:
        _fonts[size] = ImageFont.truetype(FONT, size)
    return _fonts[size].getlength(s)


def math_width(t, size=MS):
    svg = _typeset(t)
    return float(re.search(r"viewBox='([^']*)'", svg).group(1).split()[2]) * size / 10


def label(x, y, parts, color=INK, anchor='start', size=FS):
    """A label of several parts on one baseline: plain strings are text,
    ('c', s) is code, ('m', t) a formula."""
    widths = []
    for p in parts:
        if isinstance(p, str):
            widths.append(text_width(p, size))
        elif p[0] == 'c':
            widths.append(0.6 * CS * len(p[1]) + (0.3 * CS if p is parts[0] else 0.3 * CS))
        else:
            widths.append(math_width(p[1]))
    total = sum(widths)
    x = {'start': x, 'middle': x - total / 2, 'end': x - total}[anchor]
    out = ''
    for p, w in zip(parts, widths):
        if isinstance(p, str):
            out += f'  <text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" xml:space="preserve">{p}</text>\n'
        elif p[0] == 'c':
            out += '  ' + code(p[1], x + 0.15 * CS, y, CS) + '\n'
        else:
            out += '  ' + tex(p[1], x, y, MS, color) + '\n'
        x += w
    return out


def C(s, x, y, anchor='start'):
    return '  ' + code(s, x, y, CS, anchor) + '\n'


def M(t, x, y, color=INK, anchor='start', size=MS):
    return '  ' + tex(t, x, y, size, color, anchor) + '\n'


def angle(cx, cy, ux, uy, vx, vy, r=11, color=GREY):
    return '  ' + right_angle(cx, cy, ux, uy, vx, vy, r, color) + '\n'


def player(x, y, r=7):
    return circle(x, y, r, INK, INK)


def arc(cx, cy, r, a0, a1, color, width=1.5):
    """An arc from angle a0 to a1 in degrees, measured as on the screen
    (y down: a larger angle turns clockwise)."""
    x0, y0 = cx + r * math.cos(math.radians(a0)), cy + r * math.sin(math.radians(a0))
    x1, y1 = cx + r * math.cos(math.radians(a1)), cy + r * math.sin(math.radians(a1))
    large = 1 if abs(a1 - a0) > 180 else 0
    sweep = 1 if a1 > a0 else 0
    return (f'  <path d="M{x0:.1f},{y0:.1f} A{r},{r} 0 {large} {sweep} {x1:.1f},{y1:.1f}" fill="none" '
            f'stroke="{color}" stroke-width="{width}"/>\n')


def grid(x0, y0, cols, rows, cell, walls=()):
    s = ''
    for r in range(rows):
        for c in range(cols):
            s += rect(x0 + c * cell, y0 + r * cell, cell, cell, LINE if (c, r) in walls else WHITE, LINE)
    return s


def bracket(x1, y1, x2, y2, offset, color):
    """A dimension line parallel to (x1, y1)–(x2, y2), offset to the left of
    the direction of travel (negative: to the right), with end ticks.
    Returns the SVG and the point beside its middle for a label."""
    dx, dy = x2 - x1, y2 - y1
    n = math.hypot(dx, dy)
    nx, ny = dy / n, -dx / n
    ax, ay = x1 + nx * offset, y1 + ny * offset
    bx, by = x2 + nx * offset, y2 + ny * offset
    s = line(ax, ay, bx, by, color, 1.5)
    for px, py in [(ax, ay), (bx, by)]:
        s += line(px - nx * 5, py - ny * 5, px + nx * 5, py + ny * 5, color, 1.5)
    return s, ((ax + bx) / 2, (ay + by) / 2, nx * (1 if offset >= 0 else -1), ny * (1 if offset >= 0 else -1))


# ---------------------------------------------------------------- the map

def richtung():
    """The direction from the angle, on the map: cos to the right, sin down."""
    w, h = 400, 200
    s = head(w, h, 'Die Blickrichtung aus dem Winkel α = player_angle, auf der Karte: ein Pfeil der Länge 1, '
             'cos α nach rechts, sin α nach unten. y zeigt wie auf dem Bildschirm nach unten; '
             'ein größerer Winkel dreht im Uhrzeigersinn.', 'richtung')
    px, py = 46, 56
    L = 200
    ang = 33
    tx, ty = px + L * math.cos(math.radians(ang)), py + L * math.sin(math.radians(ang))
    s += line(px, py, tx + 36, py, LINE, 1.5)
    s += line(px, py, tx, py, RED, 3)
    s += line(tx, py, tx, ty, GREEN, 3)
    s += angle(tx, py, -1, 0, 0, 1, 12)
    s += M(r'\cos\alpha', (px + tx) / 2, py - 12, RED, 'middle')
    s += M(r'\sin\alpha', tx + 10, (py + ty) / 2 + 6, GREEN)
    s += arc(px, py, 62, 0, ang, GREY)
    a = math.radians(ang / 2)
    s += M(r'\alpha', px + 74 * math.cos(a), py + 74 * math.sin(a) + 6, GREY, 'middle')
    s += arrow(px, py, tx, ty, BLUE, 3)
    mx, my = px + L * 0.5 * math.cos(math.radians(ang)), py + L * 0.5 * math.sin(math.radians(ang))
    s += M('1', mx - 10, my + 22, BLUE, 'end')
    s += player(px, py)
    # the axes of the map, top right
    ox, oy = 326, 24
    s += arrow(ox, oy, ox + 42, oy, INK, 2)
    s += M('x', ox + 50, oy + 5, INK)
    s += arrow(ox, oy, ox, oy + 42, INK, 2)
    s += M('y', ox - 8, oy + 44, INK, 'end')
    return s + '</svg>\n'


def seitwaerts():
    """(x, y) and, turned by 90 degrees to the right, (−y, x)."""
    w, h = 440, 236
    s = head(w, h, 'Aus der Richtung (x, y) wird, um 90 Grad nach rechts gedreht, (−y, x): dasselbe Dreieck, '
             'gedreht. Auf der Karte zeigt y nach unten.', 'seitwaerts')
    px, py = 212, 44
    L = 172
    ang = 30
    dx, dy = math.cos(math.radians(ang)), math.sin(math.radians(ang))
    ax, ay = px + L * dx, py + L * dy          # (x, y)
    bx, by = px - L * dy, py + L * dx          # (-y, x)
    s += line(px, py, ax, py, BLUE, 1.5, '5 4')
    s += line(ax, py, ax, ay, BLUE, 1.5, '5 4')
    s += M('x', (px + ax) / 2, py - 10, BLUE, 'middle')
    s += M('y', ax + 9, (py + ay) / 2 + 5, BLUE)
    s += line(px, py, bx, py, RED, 1.5, '5 4')
    s += line(bx, py, bx, by, RED, 1.5, '5 4')
    s += M('-y', (px + bx) / 2, py - 10, RED, 'middle')
    s += M('x', bx - 9, (py + by) / 2 + 5, RED, 'end')
    s += angle(px, py, dx, dy, -dy, dx, 26, INK)
    s += arrow(px, py, ax, ay, BLUE, 3)
    s += arrow(px, py, bx, by, RED, 3)
    s += label(ax + 6, ay + 28, ['vorwärts ', ('m', '(x, y)')], BLUE, 'middle')
    s += label(bx + 6, by + 28, ['nach rechts ', ('m', '(-y, x)')], RED, 'middle')
    s += player(px, py)
    ox, oy = 392, 20
    s += arrow(ox, oy, ox + 30, oy, INK, 2)
    s += M('x', ox + 34, oy + 18, INK, 'end')
    s += arrow(ox, oy, ox, oy + 30, INK, 2)
    s += M('y', ox - 7, oy + 32, INK, 'end')
    return s + '</svg>\n'


def ebene():
    """Camera from above, looking up: dir, the camera plane, plane, rays."""
    w, h = 440, 236
    s = head(w, h, 'Von oben, die Spielfigur schaut nach oben: dir mit der Länge 1, an seiner Spitze senkrecht '
             'dazu die Kamera-Ebene; plane reicht von ihrer Mitte bis zu ihrem rechten Ende. Jeder Strahl geht '
             'durch einen Punkt der Ebene, camera_x von -1 (linker Bildrand) bis 1 (rechter Bildrand); '
             'zusammen etwa 83 Grad.', 'ebene')
    px, py = 220, 208
    L = 124
    P = 0.89 * L
    tipy = py - L
    for k in range(5):
        qx = px + P * (-1 + k / 2)
        f = 1.32
        s += line(px, py, px + (qx - px) * f, py + (tipy - py) * f, GREY, 1.5, '5 4')
    s += arc(px, py, 42, -90 - 41.7, -90 + 41.7, GREY)
    s += M(r'83^\circ', px - 40, py - 20, GREY, 'end')
    s += line(px - P, tipy, px + P, tipy, RED, 3.5)
    s += angle(px, tipy, 0, 1, 1, 0, 13, INK)
    for k in range(5):
        s += circle(px + P * (-1 + k / 2), tipy, 4.5, WHITE, RED, 2)
    # plane: from the middle of the camera plane to its right end
    s += arrow(px, tipy - 44, px + P, tipy - 44, RED, 2.5)
    s += line(px, tipy - 50, px, tipy - 38, RED, 2)
    s += C('plane', px + P / 2, tipy - 56, 'middle')
    s += arrow_to(px, py, px, tipy, 1.5, BLUE, 3)
    s += C('dir', px - 12, py - L * 0.42, 'end')
    s += C('camera_x', px - P - 14, tipy + 5, 'end')
    s += C('-1', px - P - 8, tipy + 26, 'end')
    s += C('0', px - 10, tipy + 26, 'end')
    s += C('1', px + P + 8, tipy + 26)
    s += player(px, py)
    s += text(px + 14, py + 5, 'Spielfigur', INK, FS, 'start')
    return s + '</svg>\n'


def strahl_summe():
    """One ray: dir + plane * camera_x for camera_x = 0.5."""
    w, h = 400, 292
    s = head(w, h, 'Ein Strahl für camera_x = 0.5: von der Spielfigur erst dir, dann das 0,5-Fache von plane; '
             'ray ist die Summe der beiden Pfeile.', 'strahl-summe')
    px, py = 170, 256
    L = 150
    P = 0.89 * L
    tipy = py - L
    c = 0.5
    qx = px + P * c
    s += line(px - P, tipy, px + P, tipy, RED, 1.5, '4 4')
    s += text(px - P, tipy + 26, 'Kamera-Ebene', RED, FS, 'start')
    s += line(qx, tipy, px + (qx - px) * 1.62, py + (tipy - py) * 1.62, GREY, 1.5, '5 4')
    s += arrow(px, py, px, tipy + 1, BLUE, 3)
    s += C('dir', px - 12, py - L * 0.5, 'end')
    s += arrow_to(px, tipy, qx, tipy, 6.5, RED, 3)
    s += C('plane * camera_x', qx - 4, tipy - 16, 'end')
    s += arrow_to(px, py, qx, tipy, 6.5, INK, 2.5)
    s += C('ray', px + (qx - px) * 0.5 + 14, py + (tipy - py) * 0.5 + 6)
    s += circle(qx, tipy, 4.5, WHITE, INK, 2)
    s += C('camera_x = 0.5', qx + 12, tipy + 26)
    s += player(px, py)
    s += text(px + 14, py + 5, 'Spielfigur', INK, FS, 'start')
    return s + '</svg>\n'


# One grid, one player and one ray for the three pictures of the DDA: small
# steps miss the corner of the block (schritte), the DDA from grid line to
# grid line finds it (dda), and how far t goes per cell (dda-t). In map
# coordinates, y down: the ray goes to the right and up the page.
GRID_COLS, GRID_ROWS = 7, 5
# walls along the top and the right: a corner of a room, so every ray ends on a wall
GRID_WALLS = {(c, 0) for c in range(GRID_COLS)} | {(GRID_COLS - 1, r) for r in range(GRID_ROWS)} | {(4, 2)}
BLOCK = (4, 2)
# chosen so that the ray clips the corner of BLOCK between two steps of 0.45,
# no two crossings lie on top of each other, and neither delta is 2 (the
# answer of the task delta)
RAY_P = (0.57, 4.51)
RAY_D = (0.84, -0.59)
STEP = 0.45
CELL = 64
GX, GY = 18, 18


def ray_cells():
    """The DDA as in cast_ray: [(t, side, cell_x, cell_y)] up to the wall."""
    px, py = RAY_P
    rx, ry = RAY_D
    cx, cy = int(px), int(py)
    dx, dy = abs(1 / rx), abs(1 / ry)
    nx = (cx + 1 - px) * dx if rx > 0 else (px - cx) * dx
    ny = (cy + 1 - py) * dy if ry > 0 else (py - cy) * dy
    out = []
    while (cx, cy) not in GRID_WALLS:
        if not (0 <= cx < GRID_COLS and 0 <= cy < GRID_ROWS):
            raise ValueError('the ray leaves the grid without hitting a wall')
        if nx < ny:
            t = nx; nx += dx; cx += 1 if rx > 0 else -1; side = 0
        else:
            t = ny; ny += dy; cy += 1 if ry > 0 else -1; side = 1
        out.append((t, side, cx, cy))
    return out


def X(x):
    return GX + x * CELL


def Y(y):
    return GY + y * CELL


def on_ray(t):
    return RAY_P[0] + t * RAY_D[0], RAY_P[1] + t * RAY_D[1]


def block_clip():
    """t where the ray enters BLOCK (through its left side) and leaves it
    (through its top)."""
    (px, py), (rx, ry) = RAY_P, RAY_D
    t0 = (BLOCK[0] - px) / rx
    t1 = (BLOCK[1] - py) / ry
    if not t0 < t1:
        raise ValueError('the ray does not clip the corner of the block')
    return t0, t1


def schritte():
    w, h = 2 * GX + GRID_COLS * CELL, 2 * GY + GRID_ROWS * CELL
    s = head(w, h, 'Schritt für Schritt: Die Punkte liegen einen Schritt auseinander. Der Strahl streift die Ecke '
             'eines Wandkästchens zwischen zwei Punkten, das Programm merkt es nicht und läuft weiter bis zur Wand '
             'dahinter. Dieselbe Karte, dieselbe Figur und derselbe Strahl wie in dda und dda-t.', 'schritte')
    s += grid(GX, GY, GRID_COLS, GRID_ROWS, CELL, GRID_WALLS)
    px, py = RAY_P
    rx, ry = RAY_D
    n = math.hypot(rx, ry)
    step = STEP
    pts = []
    d = step
    while True:
        x, y = px + rx / n * d, py + ry / n * d
        if not (0 <= x < GRID_COLS and 0 <= y < GRID_ROWS):
            raise ValueError('the steps leave the grid without hitting a wall')
        pts.append((x, y))
        if (int(x), int(y)) in GRID_WALLS:
            break
        d += step
    assert all((int(x), int(y)) != BLOCK for x, y in pts), 'a step lands in the block'
    hx, hy = pts[-1]
    s += line(X(px), Y(py), X(hx), Y(hy), BLUE, 2.5)
    # the piece of the ray inside the block, between two steps
    t0, t1 = block_clip()
    (ax, ay), (bx, by) = on_ray(t0), on_ray(t1)
    s += line(X(ax), Y(ay), X(bx), Y(by), RED, 6, cap='round')
    for (x, y) in pts:
        s += circle(X(x), Y(y), 4.5, BLUE, WHITE, 1.5)
    s += text(X(BLOCK[0] + 0.5), Y(BLOCK[1] + 1) + 22, 'übersehen', RED, FS, 'middle')
    s += player(X(px), Y(py))
    s += text(X(px) + 16, Y(py) + 24, 'Spielfigur', INK, FS, 'start')
    return s + '</svg>\n'


def dda():
    w, h = 2 * GX + GRID_COLS * CELL, GY + GRID_ROWS * CELL + 82
    s = head(w, h, 'Von Kästchen zu Kästchen: derselbe Strahl wie bei den kleinen Schritten. Er geht von Gitterlinie '
             'zu Gitterlinie, immer über die nähere: rote Punkte auf senkrechten Linien (next_x, side 0), grüne auf '
             'waagrechten (next_y, side 1). Das Kästchen, dessen Ecke die Schritte übersehen haben, ist das erste '
             'Wandkästchen, in das er kommt.', 'dda')
    s += grid(GX, GY, GRID_COLS, GRID_ROWS, CELL, GRID_WALLS)
    s += rect(X(BLOCK[0]), Y(BLOCK[1]), CELL, CELL, 'none', RED, 2.5)
    px, py = RAY_P
    cells = ray_cells()
    hx, hy = on_ray(cells[-1][0])
    s += line(X(px), Y(py), X(hx), Y(hy), BLUE, 2.5)
    for (t, side, _, _) in cells:
        x, y = on_ray(t)
        s += circle(X(x), Y(y), 5.5, RED if side == 0 else GREEN, WHITE, 1.5)
    s += text(X(BLOCK[0] + 0.5), Y(BLOCK[1] + 1) + 22, 'getroffen', RED, FS, 'middle')
    s += player(X(px), Y(py))
    s += text(X(px) + 16, Y(py) + 24, 'Spielfigur', INK, FS, 'start')
    ly = Y(GRID_ROWS) + 32
    s += circle(GX + 8, ly - 5, 5.5, RED, WHITE, 1.5)
    s += label(GX + 22, ly, ['senkrechte Linie: ', ('c', 'next_x'), ', ', ('c', 'side = 0')])
    s += circle(GX + 8, ly + 25, 5.5, GREEN, WHITE, 1.5)
    s += label(GX + 22, ly + 30, ['waagrechte Linie: ', ('c', 'next_y'), ', ', ('c', 'side = 1')])
    return s + '</svg>\n'


def dda_t():
    w, h = 2 * GX + GRID_COLS * CELL, GY + GRID_ROWS * CELL + 82
    s = head(w, h, 'Ein Punkt auf dem Strahl ist Spielfigur + t · ray; der Pfeil ray reicht bis t = 1, Striche '
             'markieren t = 2, 3, 4. Rot: ein Kästchen nach rechts, x wächst um 1 (waagrechter Schritt); das Stück '
             'Strahl daneben ist delta_x lang, in t gemessen. Grün: ein Kästchen nach oben, y ändert sich um 1 '
             '(senkrechter Schritt), t wächst um delta_y. Derselbe Strahl wie bei den Schritten und der DDA.', 'dda-t')
    s += grid(GX, GY, GRID_COLS, GRID_ROWS, CELL, GRID_WALLS)
    px, py = RAY_P
    rx, ry = RAY_D
    cells = ray_cells()
    t_hit = cells[-1][0]
    hx, hy = on_ray(t_hit)
    s += line(X(px), Y(py), X(hx), Y(hy), GREY, 1.5, '5 4')
    reds = [t for t, side, _, _ in cells if side == 0]
    greens = [t for t, side, _, _ in cells if side == 1]
    n = math.hypot(rx, ry)
    # to the lower right of the ray on the page (y down)
    ux, uy = -ry / n, rx / n
    if uy < 0:
        ux, uy = -ux, -uy

    def step(ta, tb, color, horizontal, name):
        """The triangle of one step between the crossings ta and tb, above the
        ray: the leg of length 1 solid, the other dashed; the piece of the ray
        between them gets a bracket below the ray."""
        (ax, ay), (bx, by) = on_ray(ta), on_ray(tb)
        out = ''
        if horizontal:      # up along x = ax, then 1 to the right
            out += line(X(ax), Y(ay), X(ax), Y(by), color, 1.5, '4 3')
            out += line(X(ax), Y(by), X(bx), Y(by), color, 3)
            out += angle(X(ax), Y(by), 0, 1, 1, 0, 11, color)
            out += M('1', X((ax + bx) / 2), Y(by) - 10, color, 'middle')
        else:               # 1 up along x = ax, then to the right
            out += line(X(ax), Y(ay), X(ax), Y(by), color, 3)
            out += line(X(ax), Y(by), X(bx), Y(by), color, 1.5, '4 3')
            out += angle(X(ax), Y(by), 0, 1, 1, 0, 11, color)
            out += M('1', X(ax) - 9, Y((ay + by) / 2) + 6, color, 'end')
        b, (lx, ly, nx, ny) = bracket(X(ax), Y(ay), X(bx), Y(by), -46, color)
        out += b
        # beside the middle of the bracket, on the side away from the ray
        out += C(name, lx + nx * 10 + 2, ly + ny * 10 + 14)
        return out
    s += step(greens[0], greens[1], GREEN, False, 'delta_y')
    s += step(reds[-2], reds[-1], RED, True, 'delta_x')
    # t along the ray: the arrow ray reaches t = 1, ticks at 2, 3, ...
    for t in range(2, int(t_hit) + 1):
        x, y = on_ray(t)
        s += line(X(x) + ux * 6, Y(y) + uy * 6, X(x) - ux * 6, Y(y) - uy * 6, INK, 2)
    s += arrow(X(px), Y(py), X(px + rx), Y(py + ry), BLUE, 3)
    s += C('ray', X(px + rx / 2) - 14, Y(py + ry / 2) - 8, 'end')
    for t in reds:
        x, y = on_ray(t)
        s += circle(X(x), Y(y), 5, RED, WHITE, 1.5)
    for t in greens:
        x, y = on_ray(t)
        s += circle(X(x), Y(y), 5, GREEN, WHITE, 1.5)
    for t in range(1, int(t_hit) + 1):
        x, y = on_ray(t)
        s += M('t = %d' % t if t == 1 else str(t), X(x) + ux * 15 + 2, Y(y) + uy * 15 + 6, INK, 'start', 15)
    s += player(X(px), Y(py))
    ly = Y(GRID_ROWS) + 32
    s += line(GX, ly - 5, GX + 18, ly - 5, RED, 3)
    s += label(GX + 26, ly, [('m', 'x'), ' wächst um 1, ', ('m', 't'), ' um ', ('c', 'delta_x')])
    s += line(GX + 9, ly + 16, GX + 9, ly + 34, GREEN, 3)
    s += label(GX + 26, ly + 30, [('m', 'y'), ' ändert sich um 1, ', ('m', 't'), ' wächst um ', ('c', 'delta_y')])
    return s + '</svg>\n'


# ---------------------------------------------------------------- walls

def wandhoehe():
    """From the side: a wall twice as far away is half as high in the picture."""
    u = 120
    w, h = 440, 212
    ex, fy = 50, 170          # eye x, floor y
    eyey = fy - 0.5 * u
    s = head(w, h, 'Von der Seite: Das Bild steht 1 vor dem Auge und ist H Pixel hoch; eine Wand der Höhe 1 im '
             'Abstand 1 füllt es genau. Steht sie doppelt so weit weg, treffen die Strahlen zu ihrer Ober- und '
             'Unterkante das Bild nur halb so weit auseinander: H / 2.', 'wandhoehe')
    s += line(20, fy, w - 20, fy, INK, 2.5)
    s += line(ex, eyey, w - 20, eyey, GREY, 1.2, '5 5')
    sx = ex + u
    wx = ex + 2 * u
    s += line(sx, fy - u, sx, fy, INK, 3)
    s += label(sx, fy - u - 12, ['Bild, ', ('c', 'H'), ' Pixel hoch'], INK, 'middle')
    s += rect(wx, fy - u, 16, u, LINE, INK, 1.5)
    s += label(wx + 26, fy - u + 18, ['Wand, Höhe ', ('m', '1')])
    for yy in (fy - u, fy):
        s += line(ex, eyey, wx, yy, BLUE, 2)
        t = (sx - ex) / (wx - ex)
        s += circle(sx, eyey + t * (yy - eyey), 4, WHITE, BLUE, 2)
    a = eyey + 0.5 * (fy - u - eyey)
    b = eyey + 0.5 * (fy - eyey)
    s += line(sx + 12, a, sx + 12, b, BLUE, 1.5)
    s += line(sx + 7, a, sx + 17, a, BLUE, 1.5)
    s += line(sx + 7, b, sx + 17, b, BLUE, 1.5)
    s += C('H / 2', sx + 26, eyey + 26)
    s += player(ex, eyey)
    s += text(ex, eyey - 14, 'Auge', INK, FS, 'middle')
    for k, xx in [(0, ex), (1, sx), (2, wx)]:
        s += line(xx, fy, xx, fy + 6, INK, 1.5)
        s += M(str(k), xx, fy + 26, GREY, 'middle')
    s += text(wx + 30, fy + 26, 'Abstand', GREY, FS, 'start')
    return s + '</svg>\n'


def fischauge():
    """A straight wall: the edge ray is longer; along dir both hits are
    equally far, the green shadow as in the dot product pictures."""
    w, h = 440, 280
    s = head(w, h, 'Eine gerade Wand vor der Spielfigur. Der Strahl zur Mitte und der zum Rand treffen dieselbe Wand; '
             'der zum Rand ist länger. Entlang dir gemessen (grün, der Schatten wie beim Skalarprodukt) sind beide '
             'Treffpunkte gleich weit vor der Figur.', 'fischauge')
    px, py = 150, 244
    wy = 50
    s += rect(20, wy - 18, w - 40, 18, LINE)
    s += line(20, wy, w - 20, wy, INK, 2.5)
    s += text(w - 24, wy - 26, 'eine gerade Wand', GREY, FS, 'end')
    qx = 390
    s += line(px, py, px, wy, GREY, 1.5, '5 4')
    s += arrow_to(px, py, qx, wy, 6.5, GREY, 2, '6 4')
    s += circle(qx, wy, 4.5, WHITE, INK, 2)
    s += circle(px, wy, 4.5, WHITE, INK, 2)
    s += angle(px, wy, 0, 1, 1, 0, 13, GREY)
    s += text(px + (qx - px) * 0.55 + 10, py + (wy - py) * 0.55 + 14, 'direkt: länger', GREY, FS, 'start')
    gx = px - 16
    s += line(gx, py, gx, wy, GREEN, 5, cap='round')
    s += label(gx - 10, (py + wy) / 2 - 6, ['entlang ', ('c', 'dir'), ':'], GREEN, 'end')
    s += text(gx - 10, (py + wy) / 2 + 16, 'gleich weit', GREEN, FS, 'end')
    s += arrow(px, py, px, py - 64, BLUE, 3)
    s += C('dir', px + 12, py - 30)
    s += player(px, py)
    s += text(px + 14, py + 5, 'Spielfigur', INK, FS, 'start')
    return s + '</svg>\n'


def skalar(name, deg, comment):
    """dir to the right, a unit vector a at deg; the projection of a on dir.
    Pure geometry, no map: a turns up the page (counterclockwise)."""
    w, h = 250, 206
    s = head(w, h, comment, name)
    ox, oy = 30, 176
    L = 160
    ax, ay = ox + L * math.cos(math.radians(-deg)), oy + L * math.sin(math.radians(-deg))
    foot = ox + L * math.cos(math.radians(deg))
    if 0 < deg < 90:
        s += line(ax, ay, foot, oy, GREY, 1.5, '4 4')
        s += angle(foot, oy, -1, 0, 0, -1, 10, GREY)
    if deg == 90:
        s += angle(ox, oy, 1, 0, 0, -1, 16, GREY)
    if deg == 0:
        s += line(ox, oy + 14, ox + L, oy + 14, GREEN, 5, cap='round')
    elif foot - ox > 1:
        s += line(ox, oy + 14, foot, oy + 14, GREEN, 5, cap='round')
    else:
        s += circle(ox, oy + 14, 3.5, GREEN, GREEN, 1)
    s += arrow(ox, oy, ox + L, oy, BLUE, 3)
    if deg == 0:
        s += C('dir', ox + L + 8, oy + 12)
        s += arrow(ox, oy - 16, ox + L, oy - 16, INK, 3)
        s += C('a', ox + L + 8, oy - 11)
    else:
        s += C('dir', ox + L + 8, oy + 5)
        s += arrow(ox, oy, ax, ay, INK, 3)
        s += C('a', ax + 10, ay + 6)
    if 0 < deg < 90:
        s += arc(ox, oy, 30, -deg, 0, GREY)
        s += M(r'%d^\circ' % deg, ox + 37, oy - 9, GREY)
    s += circle(ox, oy, 3.5, INK, INK, 1)
    return s + '</svg>\n'


def gleiten():
    """Four corners of a square; x and y separately: sliding along a wall."""
    cell = 130
    cols, rows = 3, 2
    x0, y0 = 10, 20
    w, h = x0 + cols * cell + 10, y0 + rows * cell + 46
    walls = {(2, r) for r in range(rows)}
    s = head(w, h, 'Von oben: die Spielfigur mit dem Quadrat um sie herum, an jeder Ecke ein Punkt, dicht vor einer '
             'Wand rechts. Sie will schräg nach rechts oben. Nur in x verschoben stünden zwei Ecken in der Wand, also '
             'bleibt x, wie es ist; nur in y verschoben ist alles frei, also bewegt sie sich nach oben: Sie gleitet an '
             'der Wand entlang.', 'gleiten')
    s += grid(x0, y0, cols, rows, cell, walls)

    def GXf(x):
        return x0 + x * cell

    def GYf(y):
        return y0 + y * cell
    r = 0.25
    px, py = 1.55, 1.42
    mx, my = 0.6, -0.72

    def corners(cx, cy, col, dash):
        out = rect(GXf(cx - r), GYf(cy - r), 2 * r * cell, 2 * r * cell, 'none', col, 1.5, dash)
        for sx in (-1, 1):
            for sy in (-1, 1):
                x, y = cx + sx * r, cy + sy * r
                in_wall = x >= 2
                out += circle(GXf(x), GYf(y), 4.5, RED if in_wall else WHITE, RED if in_wall else col, 1.5)
        return out
    s += circle(GXf(px), GYf(py), r * cell, PALE, INK, 1.5)
    s += arrow_to(GXf(px), GYf(py), GXf(px + mx), GYf(py + my), 4, GREY, 1.5, '5 4')
    s += arrow_to(GXf(px), GYf(py), GXf(px + mx), GYf(py), 4, RED, 2.5)
    s += arrow_to(GXf(px), GYf(py), GXf(px), GYf(py + my), 4, GREEN, 2.5)
    s += corners(px + mx, py, RED, '4 3')
    s += circle(GXf(px + mx), GYf(py), 3, RED, RED, 1)
    s += corners(px, py + my, GREEN, '4 3')
    s += circle(GXf(px), GYf(py + my), 3, GREEN, GREEN, 1)
    s += corners(px, py, INK, None)
    s += player(GXf(px), GYf(py), 4)
    s += text(GXf(px - r) - 12, GYf(py + my) + 5, 'nur y: frei', GREEN, FS, 'end')
    s += text(GXf(px - r) - 12, GYf(py) + 5, 'Spielfigur', INK, FS, 'end')
    s += text(GXf(px + mx), GYf(rows) + 30, 'nur x: zwei Ecken in der Wand', RED, FS, 'middle')
    s += text(GXf(2.5), GYf(0.25) + 5, 'Wand', INK, FS, 'middle')
    return s + '</svg>\n'


def textur_u():
    """Where on the wall: u is the fractional part of hit.y or hit.x."""
    cell = 130
    x0, y0 = 40, 40
    w, h = x0 + 3 * cell + 20, y0 + 3 * cell + 16
    walls = {(1, 1)}
    s = head(w, h, 'Von oben, auf der Karte (y nach unten): ein Wandkästchen. Ein Strahl von links trifft seine linke '
             'Seite bei hit.y = 1.35, dort ist u = 0.35; ein Strahl von unten trifft die untere Seite bei '
             'hit.x = 1.6, dort ist u = 0.6. u läuft an jeder Seite von 0 bis 1.', 'textur-u')
    for r in range(3):
        for c in range(3):
            s += rect(x0 + c * cell, y0 + r * cell, cell, cell, LINE if (c, r) in walls else WHITE, LINE)

    def TX(x):
        return x0 + x * cell

    def TY(y):
        return y0 + y * cell
    for k in range(4):
        s += M(str(k), TX(0) - 10, TY(k) + 6, GREY, 'end', 15)
        s += M(str(k), TX(k), TY(0) - 12, GREY, 'middle', 15)
    s += arrow(TX(0.3), TY(0) - 18, TX(0.7), TY(0) - 18, GREY, 1.5)
    s += M('x', TX(0.5), TY(0) - 26, GREY, 'middle', 15)
    s += arrow(TX(0) - 20, TY(0.3), TX(0) - 20, TY(0.7), GREY, 1.5)
    s += M('y', TX(0) - 27, TY(0.5) + 5, GREY, 'end', 15)
    s += text(TX(1.62), TY(1.5) - 4, 'Wand', INK, FS, 'middle')
    s += arrow(TX(1) + 12, TY(1) + 10, TX(1) + 12, TY(2) - 34, GREY, 1.5)
    s += C('u', TX(1) + 22, TY(1) + 26)
    s += arrow(TX(1) + 34, TY(2) - 12, TX(2) - 10, TY(2) - 12, GREY, 1.5)
    s += C('u', TX(2) - 22, TY(2) - 22, 'middle')
    ay = 1.35
    s += arrow_to(TX(0.25), TY(2.15), TX(1), TY(ay), 6.5, BLUE, 2.5)
    s += circle(TX(1), TY(ay), 4.5, WHITE, BLUE, 2)
    s += C('hit.y = 1.35', TX(0.06), TY(ay) - 18)
    s += C('u = 0.35', TX(0.06), TY(ay) + 4)
    bx = 1.6
    s += arrow_to(TX(2.45), TY(2.85), TX(bx), TY(2), 6.5, GREEN, 2.5)
    s += circle(TX(bx), TY(2), 4.5, WHITE, GREEN, 2)
    s += C('hit.x = 1.6', TX(2) + 12, TY(2.18) + 4)
    s += C('u = 0.6', TX(2) + 12, TY(2.18) + 26)
    return s + '</svg>\n'


UV_ROOM = [
    '##########',
    '#....#...#',
    '#........#',
    '#........#',
    '#........#',
    '##########',
]
UV_FACE = (3, 0, 1)          # the bottom side of cell (3, 0): seen from below, side 1
UV_GRID = 8                  # grid lines on the wall and on the texture
UV_POINT = (0.625, 0.375)    # a point (u, v) shown on both
UV_EYE = (2.3, 2.75)         # where the player stands
UV_ANGLE = 38                # and looks, in degrees from north, clockwise


def uv_view(w, h, px, py, angle_deg, plane=0.66):
    """The view from (px, py), cast like the raycaster, one ray per column:
    for each column its x, the depth t along dir, the face (mx, my, side) and
    u on it, flipped as in the program, so that u grows to the right."""
    a = math.radians(angle_deg)
    dx, dy = math.sin(a), -math.cos(a)
    qx, qy = -dy * plane, dx * plane       # to the right of dir (y down)
    cols = []
    for c in range(w + 1):
        cam = 2 * c / w - 1
        rx, ry = dx + qx * cam, dy + qy * cam
        mx, my = int(px), int(py)
        ddx = abs(1 / rx) if rx else 1e30
        ddy = abs(1 / ry) if ry else 1e30
        sx = 1 if rx > 0 else -1
        sy = 1 if ry > 0 else -1
        nx = (mx + (sx > 0) - px) * sx * ddx
        ny = (my + (sy > 0) - py) * sy * ddy
        while True:
            if nx < ny:
                t = nx; nx += ddx; mx += sx; side = 0
            else:
                t = ny; ny += ddy; my += sy; side = 1
            if UV_ROOM[my][mx] == '#':
                break
        hx, hy = px + t * rx, py + t * ry
        wall = hy if side == 0 else hx
        u = wall - math.floor(wall)
        if (side == 0 and rx < 0) or (side == 1 and ry > 0):
            u = 1 - u
        cols.append((c, t, (mx, my, side), u))
    return cols, (w / 2) / plane


def textur_uv():
    """A wall in the room, seen as the raycaster sees it; on one of its
    squares the grid of the texture, u to the right and v down."""
    w, h = 560, 330
    x0, y0 = 0, 0
    cy = h / 2
    s = head(w, h, 'Blick in einen Raum, so wie der Raycaster ihn zeichnet: Wände, Boden und Decke. Auf einem '
             'Kästchen der Wand liegt das Gitter der Textur; u läuft auf ihm von links nach rechts von 0 bis 1 (rot), '
             'v von oben nach unten von 0 bis 1 (grün), wie x und y auf dem Bildschirm. Ein Punkt bei u = 0.625, '
             'v = 0.375.', 'textur-uv')
    cols, f = uv_view(w, h, UV_EYE[0], UV_EYE[1], UV_ANGLE)

    def big(label, x, y, anchor):
        # larger than elsewhere: on a phone the picture is half as wide
        return '  ' + code(label, x, y, 19, anchor) + '\n'

    def top(t):
        return cy - f * 0.5 / t

    def bot(t):
        return cy + f * 0.5 / t
    s += rect(x0, y0, w, h, LIGHT)
    s += rect(x0, cy, w, h - cy, PALE)
    # the grid of the map on the floor, under the walls
    a = math.radians(UV_ANGLE)
    ddx, ddy = math.sin(a), -math.cos(a)

    def floor_at(wx, wy):
        rx, ry = wx - UV_EYE[0], wy - UV_EYE[1]
        t = rx * ddx + ry * ddy
        return w / 2 + f * (rx * -ddy + ry * ddx) / t, bot(t), t

    def floor_line(xa, ya, xb, yb):
        # cut off just in front of the eye, where the line would turn round
        pa, pb = floor_at(xa, ya), floor_at(xb, yb)
        if pa[2] < 0.2 and pb[2] < 0.2:
            return ''
        if pa[2] < 0.2 or pb[2] < 0.2:
            if pa[2] < 0.2:
                (xa, ya, pa), (xb, yb, pb) = (xb, yb, pb), (xa, ya, pa)
            k = (pa[2] - 0.2) / (pa[2] - pb[2])
            pb = floor_at(xa + k * (xb - xa), ya + k * (yb - ya))
        return line(pa[0], pa[1], pb[0], pb[1], LINE, 1.5)
    for k in range(1, len(UV_ROOM) - 1):
        s += floor_line(1, k, len(UV_ROOM[0]) - 1, k)
    for k in range(1, len(UV_ROOM[0]) - 1):
        s += floor_line(k, 1, k, len(UV_ROOM) - 1)
    pts = [(x, top(t)) for x, t, _, _ in cols] + [(x, bot(t)) for x, t, _, _ in reversed(cols)]
    s += '  <polygon points="' + ' '.join(f'{x:.1f},{y:.1f}' for x, y in pts) + f'" fill="{LINE}"/>\n'

    # the face with the grid: the columns that hit it
    if not any(c[2] == UV_FACE for c in cols):
        raise ValueError('the face is not in view')

    def at(u, v):
        """The point (u, v) of the face on the screen: the face is the line
        y = my + 1 below its cell, seen from below, u growing with x."""
        mx, my, _ = UV_FACE
        a = math.radians(UV_ANGLE)
        dx, dy = math.sin(a), -math.cos(a)
        rx, ry = -dy, dx                      # to the right of dir, length 1
        wx, wy = mx + u - UV_EYE[0], my + 1 - UV_EYE[1]
        t = wx * dx + wy * dy
        x = w / 2 + f * (wx * rx + wy * ry) / t
        return x, top(t) + v * (bot(t) - top(t))
    pts = [at(0, 0), at(1, 0), at(1, 1), at(0, 1)]
    s += '  <polygon points="' + ' '.join(f'{x:.1f},{y:.1f}' for x, y in pts) + f'" fill="{WHITE}"/>\n'
    for k in range(1, UV_GRID):
        g = k / UV_GRID
        (xa, ya), (xb, yb) = at(g, 0), at(g, 1)
        s += line(xa, ya, xb, yb, GREY, 1)
        (xa, ya), (xb, yb) = at(0, g), at(1, g)
        s += line(xa, ya, xb, yb, GREY, 1)

    # the outline of all walls: top and bottom, and the corners
    pieces = [[cols[0]]]
    for p, q in zip(cols, cols[1:]):
        if p[2] != q[2] and (p[2][2] != q[2][2] or abs(p[1] - q[1]) > 0.05):
            pieces.append([])
        pieces[-1].append(q)
    for piece in pieces:
        for edge in (top, bot):
            s += ('  <polyline points="' + ' '.join(f'{x:.1f},{edge(t):.1f}' for x, t, _, _ in piece[::2] + [piece[-1]])
                  + f'" fill="none" stroke="{INK}" stroke-width="2"/>\n')
    for (xa, ta, fa, _), (xb, tb, fb, _) in zip(cols, cols[1:]):
        if fa != fb and (fa[2] != fb[2] or abs(ta - tb) > 0.05):
            d = min(ta, tb)
            x = xa if ta < tb else xb
            s += line(x, top(d), x, bot(d), INK, 2)
    # the face itself, a little stronger
    for u in (0, 1):
        (xa, ya), (xb, yb) = at(u, 0), at(u, 1)
        s += line(xa, ya, xb, yb, INK, 2.5)

    # u along the top, v down the left side, just outside the face
    (ax, ay), (bx, by) = at(0, 0), at(1, 0)
    n = math.hypot(bx - ax, by - ay)
    ox, oy = (by - ay) / n * 14, -(bx - ax) / n * 14          # away from the face, upwards
    s += arrow(ax + ox, ay + oy, bx + ox - (bx - ax) / n * 4, by + oy - (by - ay) / n * 4, RED, 2.5)
    s += big('u = 0', ax + ox * 2.6, ay + oy * 2.6 + 5, 'middle')
    s += big('u = 1', bx + ox * 2.6, by + oy * 2.6 + 5, 'middle')
    (cx_, cy_), (dx_, dy_) = at(0, 0), at(0, 1)
    s += arrow(cx_ - 14, cy_, dx_ - 14, dy_ - 4, GREEN, 2.5)
    s += big('v = 0', cx_ - 24, cy_ + 14, 'end')
    s += big('v = 1', dx_ - 24, dy_ - 2, 'end')

    px_, py_ = at(*UV_POINT)
    s += circle(px_, py_, 5, BLUE, WHITE, 2)
    s += rect(x0, y0, w, h, 'none', INK, 2)
    return s + '</svg>\n'


def texturraum():
    """The texture flat, in texture space: u to the right, v down, both from
    0 to 1, like x and y on the screen."""
    side = 200
    gx, gy = 84, 46
    w, h = gx + side + 16, gy + side + 18
    s = head(w, h, 'Die Textur flach im Texturraum: ein Quadrat mit demselben Gitter, u läuft nach rechts von 0 bis 1 '
             '(rot), v nach unten von 0 bis 1 (grün), wie x und y auf dem Bildschirm. Derselbe Punkt bei u = 0.625, '
             'v = 0.375.', 'texturraum')
    s += rect(gx, gy, side, side, WHITE, INK, 2)
    for k in range(1, UV_GRID):
        g = gx + k * side / UV_GRID
        s += line(g, gy, g, gy + side, GREY, 1)
        g = gy + k * side / UV_GRID
        s += line(gx, g, gx + side, g, GREY, 1)
    s += arrow(gx, gy - 14, gx + side - 4, gy - 14, RED, 2.5)
    s += C('u = 0', gx, gy - 26, 'start')
    s += C('u = 1', gx + side, gy - 26, 'end')
    s += arrow(gx - 14, gy, gx - 14, gy + side - 4, GREEN, 2.5)
    s += C('v = 0', gx - 24, gy + 12, 'end')
    s += C('v = 1', gx - 24, gy + side - 2, 'end')
    px_, py_ = gx + UV_POINT[0] * side, gy + UV_POINT[1] * side
    s += line(gx, py_, px_, py_, BLUE, 1.5, '4 3')
    s += line(px_, gy, px_, py_, BLUE, 1.5, '4 3')
    s += circle(px_, py_, 5, BLUE, WHITE, 2)
    return s + '</svg>\n'


# ---------------------------------------------------------------- floor

def boden_seite():
    """From the side: similar triangles give row_distance."""
    u = 150
    w, h = 440, 236
    ex, fy = 44, 186
    eyey = fy - 0.5 * u
    s = head(w, h, 'Von der Seite: Die Augen sind 0,5 über dem Boden, das Bild steht 1 vor ihnen. Eine Bildzeile '
             'y − H/2 Pixel unter dem Horizont, im Bild (y − H/2) / H tiefer, sieht den Boden in row_distance(y): '
             'ähnliche Dreiecke. Je näher die Zeile am Horizont, desto weiter weg.', 'boden-seite')
    s += line(20, fy, w - 10, fy, INK, 2.5)
    s += line(ex, eyey, w - 10, eyey, GREY, 1.2, '5 5')
    s += text(w - 12, eyey - 8, 'Horizont', GREY, FS, 'end')
    sx = ex + u
    s += line(sx, eyey - 0.5 * u, sx, fy, INK, 3)
    s += text(sx, eyey - 0.5 * u - 10, 'Bild', INK, FS, 'middle')
    drop = 0.23
    fxx = ex + u * 0.5 / drop
    s += line(ex, eyey, fxx, fy, BLUE, 2)
    s += circle(sx, eyey + drop * u, 4, WHITE, BLUE, 2)
    s += circle(fxx, fy, 4, WHITE, BLUE, 2)
    s += M('1', (ex + sx) / 2, eyey - 9, INK, 'middle')
    s += line(sx + 10, eyey, sx + 10, eyey + drop * u, BLUE, 1.5)
    s += line(sx + 6, eyey + drop * u, sx + 14, eyey + drop * u, BLUE, 1.5)
    s += M(r'\dfrac{y - H/2}{H}', sx + 18, eyey + drop * u / 2 + 6, BLUE)
    s += line(ex - 16, eyey, ex - 16, fy, INK, 1.5)
    s += line(ex - 20, eyey, ex - 12, eyey, INK, 1.5)
    s += M('0{,}5', ex - 8, (eyey + fy) / 2 + 12, INK)
    s += angle(sx, eyey, -1, 0, 0, 1, 11, GREY)
    s += angle(ex, fy, 0, -1, 1, 0, 11, GREY)
    s += line(ex, fy + 14, fxx, fy + 14, BLUE, 1.5)
    s += line(ex, fy + 9, ex, fy + 19, BLUE, 1.5)
    s += line(fxx, fy + 9, fxx, fy + 19, BLUE, 1.5)
    s += C('row_distance(y)', (ex + fxx) / 2, fy + 40, 'middle')
    s += player(ex, eyey)
    s += text(ex + 4, eyey - 14, 'Auge', INK, FS, 'middle')
    return s + '</svg>\n'


def boden_zeile():
    """From above: one row of the picture is a line across the floor."""
    w, h = 440, 300
    s = head(w, h, 'Von oben: Eine Bildzeile zeigt den Boden auf einer Strecke quer vor der Figur, parallel zur '
             'Kamera-Ebene, im Abstand distance. Ihr linkes Ende liegt bei distance · (dir − plane), ihr rechtes bei '
             'distance · (dir + plane); dazwischen folgen die Pixel in gleichen Schritten.', 'boden-zeile')
    px, py = 230, 266
    L = 72
    P = 0.89 * L
    d = 2.6
    tipy = py - L
    lx, ly = px - d * P, py - d * L
    rx = px + d * P
    s += line(px, py, lx, ly, GREY, 1.5, '5 4')
    s += line(px, py, rx, ly, GREY, 1.5, '5 4')
    s += line(px - P, tipy, px + P, tipy, RED, 2.5)
    s += circle(px - P, tipy, 4, WHITE, RED, 2)
    s += circle(px + P, tipy, 4, WHITE, RED, 2)
    s += C('dir - plane', px - P - 12, tipy + 5, 'end')
    s += C('dir + plane', px + P + 12, tipy + 5)
    s += arrow_to(px, py, px, tipy, 1, BLUE, 2.5)
    s += C('dir', px + 10, py - L / 2 + 6)
    dx_ = 22
    s += line(dx_, py, dx_, ly, GREY, 1.5)
    s += line(dx_ - 6, py, dx_ + 6, py, GREY, 1.5)
    s += line(dx_ - 6, ly, dx_ + 6, ly, GREY, 1.5)
    s += line(dx_ + 8, py, px - 12, py, GREY, 1, '2 4')
    s += C('distance', dx_ + 12, (py + ly) / 2 + 5)
    s += line(lx, ly, rx, ly, BLUE, 3)
    n = 9
    for k in range(n):
        x = lx + (k + 0.5) * (rx - lx) / n
        s += circle(x, ly, 4, BLUE, WHITE, 1.5)
    s += circle(lx, ly, 5, WHITE, BLUE, 2)
    s += C('left_x, left_y', lx - 4, ly - 16)
    s += text(rx + 4, ly - 16, 'eine Bildzeile', BLUE, FS, 'end')
    x1 = lx + 5.5 * (rx - lx) / n
    x2 = lx + 6.5 * (rx - lx) / n
    b, (mx, my, _, _) = bracket(x1, ly, x2, ly, -14, GREY)
    s += b
    s += C('step_x, step_y', mx, my + 26, 'middle')
    s += player(px, py)
    s += text(px + 14, py + 5, 'Spielfigur', INK, FS, 'start')
    return s + '</svg>\n'


# ------------------------------------------------- lines of constant depth

ROOM = ["##############",
        "#............#",
        "#............#",
        "#....##......#",
        "#....##......#",
        "#............#",
        "#####...######",
        "#####...######",
        "#####...######",
        "#####...######",
        "##############"]


def tiefe(name, angle_deg, comment):
    """The view down a corridor into a hall, cast like the raycaster, one
    ray per column. Every column of the picture shows a wall at one depth,
    every row the floor or ceiling at one depth: a plain stripe pattern,
    the same distance on the screen for both."""
    w, h = 960, 480
    s = head(w, h, comment, name)
    x0, y0, x1, y1 = 20, 20, 940, 460
    W = x1 - x0
    cy = (y0 + y1) / 2
    px, py = 6.5, 9.6
    a = math.radians(angle_deg)
    dx, dy = math.sin(a), -math.cos(a)
    plane = 0.75
    qx, qy = -dy * plane, dx * plane       # to the right of dir (y down)
    f = (W / 2) / plane
    cols = []
    for c in range(0, W + 1):
        cam = 2 * (c) / W - 1
        rx, ry = dx + qx * cam, dy + qy * cam
        mx, my = int(px), int(py)
        ddx = abs(1 / rx) if rx else 1e30
        ddy = abs(1 / ry) if ry else 1e30
        sx = 1 if rx > 0 else -1
        sy = 1 if ry > 0 else -1
        nx = ((mx + 1 - px) if rx > 0 else (px - mx)) * ddx
        ny = ((my + 1 - py) if ry > 0 else (py - my)) * ddy
        while True:
            if nx < ny:
                t = nx; nx += ddx; mx += sx; side = 0
            else:
                t = ny; ny += ddy; my += sy; side = 1
            if not (0 <= my < len(ROOM) and 0 <= mx < len(ROOM[my])):
                raise ValueError('a ray leaves the room without hitting a wall')
            if ROOM[my][mx] == '#':
                break
        cols.append((x0 + c, t, (mx, my, side)))   # t: the depth along dir

    def top(d):
        return max(y0, cy - f * 0.5 / d)

    def bot(d):
        return min(y1, cy + f * 0.5 / d)
    s += rect(x0, y0, W, y1 - y0, LIGHT)
    s += rect(x0, cy, W, y1 - cy, PALE)
    pts = [(x, top(d)) for x, d, _ in cols] + [(x, bot(d)) for x, d, _ in reversed(cols)]
    s += '  <polygon points="' + ' '.join(f'{x:.1f},{y:.1f}' for x, y in pts) + f'" fill="{LINE}"/>\n'
    gap = 23
    # rows: floor and ceiling, wherever no wall covers the row
    for k in range(1, int((y1 - y0) / gap) + 1):
        y = y0 + k * gap - (((y1 - y0) % gap) / 2 if False else 0)
        if not (y0 < y < y1) or abs(y - cy) < 1:
            continue
        run = None
        for x, t, _ in cols + [(None, 0, None)]:
            free = x is not None and not (top(t) <= y <= bot(t))
            if free:
                run = x if run is None else run
                last = x
            elif run is not None:
                s += line(run, y, last, y, GREEN, 1.5)
                run = None
    # columns: on the walls
    for k in range(1, int(W / gap) + 1):
        c = k * gap
        x, t, _ = cols[c]
        s += line(x, top(t), x, bot(t), BLUE, 1.5)
    # the outline: top and bottom of the walls, and the corners
    pieces = [[cols[0]]]
    for p, q in zip(cols, cols[1:]):
        if p[2] != q[2] and abs(p[1] - q[1]) > 0.05:
            pieces.append([])
        pieces[-1].append(q)
    for piece in pieces:
        for edge in (top, bot):
            s += ('  <polyline points="' + ' '.join(f'{x:.1f},{edge(t):.1f}' for x, t, _ in piece[::2] + [piece[-1]])
                  + f'" fill="none" stroke="{INK}" stroke-width="2"/>\n')
    for (xa, ta, fa), (xb, tb, fb) in zip(cols, cols[1:]):
        if fa != fb and (fa[2] != fb[2] or abs(ta - tb) > 0.05):
            d = min(ta, tb)
            x = xa if ta < tb else xb
            s += line(x, top(d), x, bot(d), INK, 2)
    s += rect(x0, y0, W, y1 - y0, 'none', INK, 2)
    return s + '</svg>\n'


PICTURES = {
    'richtung': richtung,
    'seitwaerts': seitwaerts,
    'ebene': ebene,
    'strahl-summe': strahl_summe,
    'schritte': schritte,
    'dda': dda,
    'dda-t': dda_t,
    'wandhoehe': wandhoehe,
    'fischauge': fischauge,
    'skalar-gleich': lambda: skalar('skalar-gleich', 0, 'Skalarprodukt zweier Vektoren der Länge 1: a zeigt wie '
                                    'dir, der Schatten von a auf dir ist ganz lang: 1.'),
    'skalar-schraeg': lambda: skalar('skalar-schraeg', 60, 'Skalarprodukt zweier Vektoren der Länge 1: a steht '
                                     '60 Grad schräg zu dir, der Schatten von a auf dir ist cos 60° = 0,5 lang.'),
    'skalar-senkrecht': lambda: skalar('skalar-senkrecht', 90, 'Skalarprodukt zweier Vektoren der Länge 1: a steht '
                                       'senkrecht auf dir, der Schatten ist ein Punkt: 0.'),
    'gleiten': gleiten,
    'textur-u': textur_u,
    'textur-uv': textur_uv,
    'texturraum': texturraum,
    'boden-seite': boden_seite,
    'boden-zeile': boden_zeile,
    'tiefe-gerade': lambda: tiefe('tiefe-gerade', 0, 'Linien gleicher Tiefe, geradeaus: Jede Bildspalte zeigt eine '
                                  'Wand in einer Tiefe (blau), jede Bildzeile Boden oder Decke in einer Tiefe (grün); '
                                  'ein Streifenmuster mit gleichen Abständen auf dem Bildschirm. Wie im Raycaster '
                                  'gerechnet, ein Strahl je Spalte.'),
    'tiefe-gedreht': lambda: tiefe('tiefe-gedreht', -28, 'Linien gleicher Tiefe, nach links gedreht: Die Wände '
                                   'stehen schräg im Bild, die Linien bleiben dieselben Spalten und Zeilen.'),
}

if __name__ == '__main__':
    only = set(sys.argv[1:])
    for name, fn in PICTURES.items():
        if only and name not in only:
            continue
        with open(os.path.join(OUT, name + '.svg'), 'w') as f:
            f.write(fn())
        print(name)
