"""The drawings of »Gaussian Splatting«: the bell curve, a stretched and
turned blob, gradient descent on a parabola, a sphere becoming a turned
ellipsoid, the pinhole camera and blending from front to back.

    python3 src/content/gaussian-splatting/bilder/diagramme.py
    for f in glocke ellipse bergab kovarianz kamera mischen; do
        ruby src/scripts/render-svg.rb src/content/gaussian-splatting/$f.svg WIDTH; done

with WIDTH twice the width of each viewBox. One unit of the viewBox is one
CSS pixel at the width the page gives the picture (width / 16 em). On the
screen y points down, as in the code (the ellipse, the camera). The parabola
starts elsewhere than the task »Der erste Schritt bergab« (at c = 1.4, with
the rate 0.2), so that it does not show the answer.
"""

import math as m
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import math as tex, code  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, BLUE, GREY, LINE, PALE, WHITE = '#1f2937', '#2b5fb4', '#6b7280', '#d1d5db', '#eef3fb', '#ffffff'
RED, GREEN = '#dc2626', '#16a34a'


def head(w, h, comment, name):
    return (f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
            f'viewBox="0 0 {w} {h}" width="{w}" height="{h}" font-family="IBM Plex Sans, DejaVu Sans, sans-serif">\n'
            f'  <!-- {comment}\n'
            f'       Gemacht von bilder/diagramme.py; als Bild, hell und dunkel:\n'
            f'       src/scripts/render-svg.rb src/content/gaussian-splatting/{name}.svg {2 * w} -->\n'
            f'  <defs><marker id="arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">'
            f'<path d="M0,0 L10,5 L0,10 z" fill="{INK}"/></marker>'
            f'<marker id="arrow-blue" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">'
            f'<path d="M0,0 L10,5 L0,10 z" fill="{BLUE}"/></marker></defs>\n'
            f'  <rect width="{w}" height="{h}" fill="{WHITE}"/>\n')


def line(x1, y1, x2, y2, color=INK, width=1.5, dash=None, arrow=None):
    d = f' stroke-dasharray="{dash}"' if dash else ''
    a = f' marker-end="url(#{arrow})"' if arrow else ''
    return f'  <line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" stroke="{color}" stroke-width="{width}"{d}{a}/>\n'


def polyline(pts, color, width, dash=None, fill='none'):
    d = f' stroke-dasharray="{dash}"' if dash else ''
    return (f'  <polyline points="{" ".join(f"{x:.1f},{y:.1f}" for x, y in pts)}" fill="{fill}" stroke="{color}" '
            f'stroke-width="{width}" stroke-linejoin="round"{d}/>\n')


def text(x, y, s, color=INK, anchor='middle', size=15):
    return f'  <text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" text-anchor="{anchor}">{s}</text>\n'


def label(t, x, y, size=16, color=INK, anchor='middle'):
    return '  ' + tex(t, x, y, size, color, anchor) + '\n'


def dot(x, y, color=INK, r=4):
    return f'  <circle cx="{x:.1f}" cy="{y:.1f}" r="{r}" fill="{color}"/>\n'


def save(name, svg):
    with open(os.path.join(OUT, name + '.svg'), 'w') as f:
        f.write(svg + '</svg>\n')


# ------------------------------------------------------------- the bell curve

def glocke():
    w, h = 480, 230
    s = head(w, h, 'Die Glockenkurve g(x) über der x-Achse von −3σ bis 3σ: 1 in der Mitte, 0,61 bei ±σ, fast 0 bei ±3σ.', 'glocke')
    ox, oy, sx, sy = 240, 180, 62, 130

    def X(x):
        return ox + x * sx

    def Y(g):
        return oy - g * sy
    s += line(X(-3.6), oy, X(3.6), oy, INK, 1.5, arrow='arrow')
    s += label('x', X(3.6) + 4, oy + 18, 16, INK, 'end')
    s += line(ox, oy, ox, Y(1.18), INK, 1.5, arrow='arrow')
    s += label('g(x)', ox + 10, Y(1.15), 16, INK, 'start')
    g61 = m.exp(-0.5)
    s += line(X(-1), Y(g61), X(1), Y(g61), GREY, 1.2, '5 4')
    s += line(X(-1), oy, X(-1), Y(g61), GREY, 1.2, '5 4')
    s += line(X(1), oy, X(1), Y(g61), GREY, 1.2, '5 4')
    pts = [(X(x / 50), Y(m.exp(-(x / 50) ** 2 / 2))) for x in range(-170, 171)]
    s += polyline(pts, BLUE, 3)
    for k, t in [(-3, r'-3\sigma'), (-2, r'-2\sigma'), (-1, r'-\sigma'), (1, r'\sigma'), (2, r'2\sigma'), (3, r'3\sigma')]:
        s += line(X(k), oy - 4, X(k), oy + 4)
        s += label(t, X(k), oy + 24)
    s += label('0', ox, oy + 24)
    s += label(r'0{,}61', X(1) + 10, Y(g61) - 8, 16, GREY, 'start')
    save('glocke', s)


# ------------------------------------------------------------- a stretched, turned blob

def ellipse():
    w, h = 480, 330
    s = head(w, h, 'Ein länglicher Fleck als Ellipse, um θ gedreht (auf dem Bildschirm zeigt y nach unten, '
             'ein positiver Winkel dreht im Uhrzeigersinn). Ein Pixel P hat den Abstand Δx, Δy zur Mitte; '
             'entlang der langen Achse liegt es u · s_x weit, quer dazu v · s_y.', 'ellipse')
    cx, cy = 200, 150
    theta = m.radians(28)
    ax, ay = m.cos(theta), m.sin(theta)            # the long axis (y down)
    bx, by = -m.sin(theta), m.cos(theta)           # across it
    rx, ry = 150, 58
    s += (f'  <ellipse cx="{cx}" cy="{cy}" rx="{rx}" ry="{ry}" transform="rotate({m.degrees(theta):.1f} {cx} {cy})" '
          f'fill="{PALE}" stroke="{BLUE}" stroke-width="2.5"/>\n')
    # the screen's axes in a corner
    s += line(28, 26, 82, 26, INK, 1.5, arrow='arrow')
    s += label('x', 90, 31, 16, INK, 'start')
    s += line(28, 26, 28, 80, INK, 1.5, arrow='arrow')
    s += label('y', 28, 98, 16, INK, 'middle')
    # the long and the short axis
    s += line(cx - ax * (rx + 30), cy - ay * (rx + 30), cx + ax * (rx + 30), cy + ay * (rx + 30), GREY, 1.2, '6 4')
    s += line(cx - bx * (ry + 26), cy - by * (ry + 26), cx + bx * (ry + 26), cy + by * (ry + 26), GREY, 1.2, '6 4')
    # theta between the horizontal and the long axis
    s += line(cx, cy, cx + 120, cy, GREY, 1.2, '3 3')
    r = 70
    s += f'  <path d="M{cx + r},{cy} A{r},{r} 0 0 1 {cx + r * ax:.1f},{cy + r * ay:.1f}" fill="none" stroke="{INK}" stroke-width="1.3"/>\n'
    s += label(r'\theta', cx + 78, cy + 23, 17)
    # half axes s_x and s_y
    s += line(cx, cy, cx - ax * rx, cy - ay * rx, BLUE, 2)
    s += label('s_x', cx - ax * rx / 2 - 8, cy - ay * rx / 2 - 10, 17, BLUE)
    s += line(cx, cy, cx - bx * ry, cy - by * ry, BLUE, 2)
    s += label('s_y', cx - bx * ry / 2 + 16, cy - by * ry / 2 + 2, 17, BLUE)
    # a pixel P with dx, dy and its u, v: u along the long axis, v across
    px, py = cx + 235, cy + 48
    dxv, dyv = px - cx, py - cy
    along = dxv * ax + dyv * ay
    fx, fy = cx + ax * along, cy + ay * along
    s += line(cx, cy, px, cy, INK, 1.3)
    s += line(px, cy, px, py, INK, 1.3)
    s += label(r'\Delta x', (cx + px) / 2 + 40, cy - 8, 16)
    s += label(r'\Delta y', px + 8, cy + 30, 16, INK, 'start')
    s += line(cx, cy, fx, fy, BLUE, 2.4)
    s += line(fx, fy, px, py, BLUE, 2.4, '5 4')
    s += label(r'u \cdot s_x', fx - 26, fy + 28, 16, BLUE, 'middle')
    s += label(r'v \cdot s_y', (fx + px) / 2 + 14, (fy + py) / 2 + 22, 16, BLUE, 'start')
    s += dot(cx, cy)
    s += dot(px, py)
    s += label('P', px + 8, py + 18, 17, INK, 'start')
    save('ellipse', s)


# ------------------------------------------------------------- downhill

def bergab():
    """The steps as arcs beside the parabola (a straight chord would lie
    almost on the curve), each point with a dashed line down to the axis,
    where the steps can be read as distances: c₀ to c₃ named, the later
    ones too close together for a name."""
    w, h = 560, 300
    s = head(w, h, 'Die Parabel E(c) = (c − 0,7)² und vier Schritte des Gradientenabstiegs von c₀ = 1,4 aus, mit der Lernrate 0,2: '
             'jeder Schritt ein Bogen neben der Kurve, jeder kleiner als der vorige, auf den Tiefpunkt bei 0,7 zu.', 'bergab')
    ox, oy, sx, sy = 40, 238, 320, 380

    def X(c):
        return ox + c * sx

    def Y(e):
        return oy - e * sy
    s += line(X(-0.05), oy, X(1.6), oy, INK, 1.5, arrow='arrow')
    s += label('c', X(1.6) - 2, oy + 24, 18, INK, 'end')
    s += line(ox, oy, ox, Y(0.56), INK, 1.5, arrow='arrow')
    s += label('E', ox + 12, Y(0.53), 18, INK, 'start')
    pts = [(X(c / 100), Y((c / 100 - 0.7) ** 2)) for c in range(-3, 156)]
    s += polyline([p for p in pts if p[1] > Y(0.55)], BLUE, 3)
    s += line(X(0.7), oy - 5, X(0.7), oy + 5)
    s += label(r'0{,}7', X(0.7), oy + 28, 17)
    s += label('0', ox, oy + 28, 17)
    c = 1.4
    steps = [c]
    for _ in range(4):
        c = c - 0.2 * 2 * (c - 0.7)
        steps.append(c)
    P = [(X(c), Y((c - 0.7) ** 2)) for c in steps]
    for k, (x, y) in enumerate(P):
        s += line(x, y + 7, x, oy - 1, GREY, 1.2, dash='3 4')
        if k < 4:
            s += label(f'c_{k}', x, oy + 28, 17)
    for (xa, ya), (xb, yb) in zip(P, P[1:]):
        dx, dy = xb - xa, yb - ya
        d = m.hypot(dx, dy)
        if d < 40:   # too short for an arc: the dots alone
            continue
        ux, uy = dx / d, dy / d
        # start and end a little off the dots; bow up and to the left,
        # away from the curve, which bends the other way
        x1, y1 = xa + 8 * ux, ya + 8 * uy
        x2, y2 = xb - 9 * ux, yb - 9 * uy
        bow = 0.22 * d + 3
        cx, cy = (x1 + x2) / 2 - uy * bow, (y1 + y2) / 2 + ux * bow
        s += (f'  <path d="M{x1:.1f},{y1:.1f} Q{cx:.1f},{cy:.1f} {x2:.1f},{y2:.1f}" fill="none" stroke="{INK}" '
              f'stroke-width="1.6" marker-end="url(#arrow)"/>\n')
    for x, y in P:
        s += dot(x, y, INK, 5)
    save('bergab', s)


# ------------------------------------------------------------- sphere, stretched, turned

def kovarianz():
    w, h = 600, 210
    s = head(w, h, 'Eine Kugel, mit S entlang der Achsen zu einem Ellipsoid gestreckt und mit R gedreht.', 'kovarianz')
    cy = 105

    def body(cx, rx, ry, angle):
        t = f' transform="rotate({angle} {cx} {cy})"' if angle else ''
        out = f'  <ellipse cx="{cx}" cy="{cy}" rx="{rx}" ry="{ry}"{t} fill="{PALE}" stroke="{BLUE}" stroke-width="2.5"/>\n'
        # an equator, to show that it is round in depth
        out += f'  <ellipse cx="{cx}" cy="{cy}" rx="{rx}" ry="{ry * 0.28:.1f}"{t} fill="none" stroke="{BLUE}" stroke-width="1.2" stroke-dasharray="5 4"/>\n'
        out += (f'  <ellipse cx="{cx}" cy="{cy}" rx="{rx * 0.3:.1f}" ry="{ry}"{t} fill="none" stroke="{BLUE}" '
                f'stroke-width="1.2" stroke-dasharray="5 4"/>\n')
        return out
    s += body(90, 55, 55, 0)
    s += body(300, 92, 32, 0)
    s += body(510, 92, 32, -32)
    s += line(160, cy, 196, cy, INK, 1.6, arrow='arrow')
    s += label('S', 178, cy - 12, 18)
    s += line(404, cy, 440, cy, INK, 1.6, arrow='arrow')
    s += label('R', 422, cy - 12, 18)
    s += text(90, 196, 'Kugel', GREY)
    s += text(300, 196, 'gestreckt', GREY)
    s += text(510, 196, 'gedreht', GREY)
    save('kovarianz', s)


# ------------------------------------------------------------- the pinhole camera

def kamera():
    w, h = 520, 280
    s = head(w, h, 'Die Kamera von der Seite: das Auge links, z nach rechts, y nach unten, das Bild im Abstand f. '
             'Ein Punkt P liegt z vor dem Auge und y unter der Blickrichtung; die Gerade zum Auge trifft das Bild '
             'f · y / z unter der Mitte.', 'kamera')
    ex, ey = 50, 80
    f = 130
    pz, py = 380, 130
    s += line(ex, ey, ex + 440, ey, INK, 1.5, arrow='arrow')
    s += label('z', ex + 446, ey + 6, 17, INK, 'start')
    s += line(ex, ey, ex, ey + 180, INK, 1.5, arrow='arrow')
    s += label('y', ex - 10, ey + 180, 17, INK, 'end')
    # the picture
    s += line(ex + f, ey - 50, ex + f, ey + 150, BLUE, 3)
    s += text(ex + f + 8, ey - 22, 'Bild', BLUE, 'start')
    # the point and the ray to the eye
    px, pyy = ex + pz, ey + py
    hit = ey + f * py / pz
    s += line(px, pyy, ex, ey, GREY, 1.4, '6 4')
    s += dot(ex, ey, INK, 5)
    s += text(ex - 6, ey - 12, 'Auge', INK, 'start')
    s += dot(px, pyy, INK, 5)
    s += label('P', px + 10, pyy + 6, 17, INK, 'start')
    s += dot(ex + f, hit, BLUE, 5)
    # dimensions: f, z, y and f y / z
    s += line(ex, ey + 168, ex + f, ey + 168, GREY, 1.2, arrow='arrow')
    s += label('f', ex + f / 2, ey + 188, 17, GREY)
    s += line(ex + f, ey + 168, ex, ey + 168, GREY, 1.2, arrow='arrow')
    s += line(px, ey, px, pyy, GREY, 1.2, '3 3')
    s += label('y', px + 10, ey + py / 2 + 6, 17, GREY, 'start')
    s += line(ex, ey - 56, px, ey - 56, GREY, 1.2, arrow='arrow')
    s += line(px, ey - 56, ex, ey - 56, GREY, 1.2, arrow='arrow')
    s += label('z', (ex + px) / 2 + 60, ey - 64, 17, GREY)
    s += line(ex + f + 14, ey, ex + f + 14, hit, BLUE, 1.2, arrow='arrow-blue')
    s += line(ex + f + 14, hit, ex + f + 14, ey, BLUE, 1.2, arrow='arrow-blue')
    s += label(r'f \cdot \frac{y}{z}', ex + f + 22, (ey + hit) / 2 + 8, 16, BLUE, 'start')
    save('kamera', s)


# ------------------------------------------------------------- front to back

def mischen():
    w, h = 580, 300
    s = head(w, h, 'Drei halb durchsichtige Scheiben hintereinander, rot, grün und blau, jede mit α = 0,5; davor, '
             'wie viel Licht noch ankommt, T = 1, 0,5, 0,25 und dahinter 0,125. Unten das Pixel als Streifen: '
             'die Hälfte Rot, ein Viertel Grün, ein Achtel Blau, ein Achtel Hintergrund.', 'mischen')
    cy = 105
    s += dot(36, cy, INK, 6)
    s += text(36, cy + 28, 'Auge', INK)
    s += line(58, cy, 540, cy, GREY, 1.2, '6 4')
    for cx, color in [(170, RED), (290, GREEN), (410, BLUE)]:
        s += (f'  <ellipse cx="{cx}" cy="{cy}" rx="22" ry="62" fill="{color}" fill-opacity="0.5" '
              f'stroke="{color}" stroke-width="2"/>\n')
        s += label(r'\alpha = 0{,}5', cx, cy - 74, 15)
    for x, t in [(110, 'T = 1'), (230, r'T = 0{,}5'), (350, r'T = 0{,}25'), (470, r'T = 0{,}125')]:
        s += label(t, x, cy + 88, 15, GREY)
    # the pixel: the parts of its colour
    x0, y0, bw, bh = 60, 228, 460, 28
    parts = [(0.5, RED, r'0{,}5'), (0.25, GREEN, r'0{,}25'), (0.125, BLUE, r'0{,}125'), (0.125, LINE, r'0{,}125')]
    x = x0
    for share, color, t in parts:
        s += f'  <rect x="{x:.1f}" y="{y0}" width="{share * bw:.1f}" height="{bh}" fill="{color}"/>\n'
        s += label(t, x + share * bw / 2, y0 + bh + 22, 15)
        x += share * bw
    s += f'  <rect x="{x0}" y="{y0}" width="{bw}" height="{bh}" fill="none" stroke="{INK}" stroke-width="1.2"/>\n'
    s += text(x0, y0 - 10, 'das Pixel:', INK, 'start')
    s += text(x0 + bw, y0 - 10, 'Hintergrund', GREY, 'end')
    save('mischen', s)


if __name__ == '__main__':
    glocke()
    ellipse()
    bergab()
    kovarianz()
    kamera()
    mischen()
