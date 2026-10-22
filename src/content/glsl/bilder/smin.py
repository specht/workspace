"""Two pictures for »Tropfen, die ineinanderfließen«: smin along the line
through two drops, and metaballs in 2D, apart, almost joined and joined.

    python3 src/content/glsl/bilder/smin.py
    ruby src/scripts/render-svg.rb src/content/glsl/smin.svg WIDTH

The drops have the page's radius 0.42 and the page's k = 0.6. Two drops
start to mix when their near surfaces are closer than k (centres closer than
2r + k = 1.44) and join when smin between them drops below 0 (centres closer
than 2r + k/2 = 1.14). No picture shows numbers, so none answers the task
smin. One unit of the viewBox is one CSS pixel at the page width (/ 16 em).
"""

import math
import os
import sys

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import math as tex, code  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, BLUE, GREY, LINE, PALE, WHITE = '#1f2937', '#2b5fb4', '#6b7280', '#d1d5db', '#eef3fb', '#ffffff'
R = 0.42
K = 0.6


def smin(a, b, k=K):
    h = np.clip(0.5 + 0.5 * (b - a) / k, 0.0, 1.0)
    return b * (1 - h) + a * h - k * h * (1.0 - h)


def head(w, h, comment, name):
    return (f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
            f'viewBox="0 0 {w} {h}" width="{w}" height="{h}" font-family="IBM Plex Sans, DejaVu Sans, sans-serif">\n'
            f'  <!-- {comment}\n'
            f'       Gemacht von bilder/smin.py; als Bild, hell und dunkel:\n'
            f'       src/scripts/render-svg.rb src/content/glsl/{name}.svg {2 * w} -->\n'
            f'  <rect width="{w}" height="{h}" fill="{WHITE}"/>\n')


def polyline(pts, color, width, dash=None, fill='none'):
    d = f' stroke-dasharray="{dash}"' if dash else ''
    return (f'  <polyline points="{" ".join(f"{x:.1f},{y:.1f}" for x, y in pts)}" fill="{fill}" stroke="{color}" '
            f'stroke-width="{width}" stroke-linejoin="round"{d}/>\n')


def text(x, y, s, color=INK, anchor='middle', size=16):
    return f'  <text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" text-anchor="{anchor}">{s}</text>\n'


# ------------------------------------------------------------- smin

def smin_graph():
    """The distances a and b to two drops along the line through both, their
    min with its crease and smin, rounded and lower where |a - b| < k."""
    w, h = 480, 250
    s = head(w, h, 'Abstand zu zwei Tropfen auf der Linie durch beide: a und b gestrichelt, min(a, b) mit einem Knick '
             'in der Mitte, smin(a, b, k) blau, rund und tiefer, aber nur, wo a und b weniger als k auseinander sind '
             '(grau hinterlegt). Unter der Linie Abstand 0 liegt das Innere eines Tropfens.', 'smin')
    x0, x1 = -1.25, 1.25
    c = 0.6                                     # the centres at -c and +c
    sx = (w - 130) / (x1 - x0)                # room on the right for the axis label
    sy = 150
    ox, oy = 30, 168                            # where position 0 and distance 0 are

    def X(x):
        return ox + (x - x0) * sx

    def Y(d):
        return oy - d * sy
    xs = np.linspace(x0, x1, 501)
    a = np.abs(xs + c) - R
    b = np.abs(xs - c) - R
    # where smin differs from min: |a - b| < k
    band = xs[np.abs(a - b) < K]
    s += f'  <rect x="{X(band[0]):.1f}" y="{Y(0.75):.1f}" width="{X(band[-1]) - X(band[0]):.1f}" height="{Y(-0.3) - Y(0.75):.1f}" fill="{PALE}"/>\n'
    s += text((X(band[0]) + X(band[-1])) / 2, Y(0.75) - 8, '', GREY)
    # the drops on the line, and distance 0
    for cc in (-c, c):
        s += f'  <rect x="{X(cc - R):.1f}" y="{Y(0) - 5:.1f}" width="{(2 * R) * sx:.1f}" height="10" rx="5" fill="{LINE}"/>\n'
    s += f'  <line x1="{X(x0):.1f}" y1="{Y(0):.1f}" x2="{X(x1):.1f}" y2="{Y(0):.1f}" stroke="{INK}" stroke-width="1.5"/>\n'
    s += text(X(x1) + 8, Y(0) + 5, 'Abstand 0', GREY, 'start', 15)
    for cc in (-c, c):
        s += text(X(cc + (-0.22 if cc < 0 else 0.22)), Y(0) - 12, 'Tropfen', GREY, 'middle', 15)
    s += polyline([(X(x), Y(d)) for x, d in zip(xs, a) if d < 0.75], GREY, 1.6, '5 4')
    s += polyline([(X(x), Y(d)) for x, d in zip(xs, b) if d < 0.75], GREY, 1.6, '5 4')
    m = np.minimum(a, b)
    s += polyline([(X(x), Y(d)) for x, d in zip(xs, m) if d < 0.75], INK, 2.2)
    sm = smin(a, b)
    s += polyline([(X(x), Y(d)) for x, d in zip(xs, sm) if d < 0.75], BLUE, 3)
    # labels
    # a grows to the right of its drop, b to the left of its own
    s += '  ' + code('a', X(0.5) + 10, Y(0.62), 15) + '\n'
    s += '  ' + code('b', X(-0.5) - 10, Y(0.62), 15, 'end') + '\n'
    s += '  ' + code('min(a, b)', X(0) + 26, Y(0.34) - 4, 15) + '\n'
    s += '  ' + code('smin(a, b, k)', X(0), Y(-0.06) + 46, 15, 'middle') + '\n'
    s += f'  <line x1="{X(0):.1f}" y1="{Y(float(smin(np.array(c - R), np.array(c - R)))) + 4:.1f}" x2="{X(0):.1f}" y2="{Y(-0.06) + 28:.1f}" stroke="{BLUE}" stroke-width="1.2"/>\n'
    lab = Y(0.75) - 10
    s += '  ' + tex(r'|a - b| < k', X(0), lab, 16, GREY, 'middle') + '\n'
    return s + '</svg>\n'


# ------------------------------------------------------------- metaballs in 2D

def contours(field, xs, ys, limit=20000):
    """Closed outlines where field = 0 (marching squares, linear on the
    edges), as lists of (x, y) in field coordinates."""
    ny, nx = field.shape
    segs = []
    for j in range(ny - 1):
        for i in range(nx - 1):
            v = [field[j, i], field[j, i + 1], field[j + 1, i + 1], field[j + 1, i]]
            corners = [(xs[i], ys[j]), (xs[i + 1], ys[j]), (xs[i + 1], ys[j + 1]), (xs[i], ys[j + 1])]
            pts = []
            for e in range(4):
                p, q = v[e], v[(e + 1) % 4]
                if (p < 0) != (q < 0):
                    t = p / (p - q)
                    (ax, ay), (bx, by) = corners[e], corners[(e + 1) % 4]
                    pts.append((round(ax + (bx - ax) * t, 9), round(ay + (by - ay) * t, 9)))
            if len(pts) == 2:
                segs.append((pts[0], pts[1]))
            elif len(pts) == 4:                  # a saddle: pair them by the centre value
                if (sum(v) / 4 < 0) == (v[0] < 0):
                    segs += [(pts[0], pts[3]), (pts[1], pts[2])]
                else:
                    segs += [(pts[0], pts[1]), (pts[2], pts[3])]
    # chain the segments into loops
    ends = {}
    for k, (p, q) in enumerate(segs):
        ends.setdefault(p, []).append(k)
        ends.setdefault(q, []).append(k)
    used = [False] * len(segs)
    loops = []
    for k in range(len(segs)):
        if used[k]:
            continue
        used[k] = True
        loop = [segs[k][0], segs[k][1]]
        for _ in range(limit):
            nxt = [m for m in ends[loop[-1]] if not used[m]]
            if not nxt:
                break
            m = nxt[0]
            used[m] = True
            p, q = segs[m]
            loop.append(q if p == loop[-1] else p)
        else:
            raise ValueError('an outline did not close')
        loops.append(loop)
    return loops


def metaballs():
    w, h = 528, 128
    s = head(w, h, 'Zwei Tropfen in 2D, mit smin wie im Shader (Radius 0.42, k = 0.6), gestrichelt die Kreise allein. '
             'Auseinander: genau zwei Kreise. Fast verbunden: Beide ziehen sich spitz zueinander aus, bis auf einen haarfeinen Spalt. '
             'Verbunden: Ein Hals verbindet sie.', 'metaballs')
    # fast verbunden: just above 2r + k/2 = 1.14, the gap is about 0.07 (4.7 px)
    cases = [(1.7, 'auseinander'), (1.144, 'fast verbunden'), (0.98, 'verbunden')]
    widths = [dist + 2 * R + 0.2 for dist, _ in cases]          # each panel as wide as its case
    gap = 22
    scale = (w - 2 * gap - 8) / sum(widths)
    left = 4
    for (dist, name), pw in zip(cases, widths):
        xs = np.linspace(-pw / 2, pw / 2, int(pw * 100) + 1)
        ys = np.linspace(-0.55, 0.55, 111)
        gx, gy = np.meshgrid(xs, ys)
        cx0 = left + pw * scale / 2
        cy0 = 8 + 0.55 * scale
        field = smin(np.hypot(gx + dist / 2, gy) - R, np.hypot(gx - dist / 2, gy) - R)
        loops = [[(cx0 + x * scale, cy0 - y * scale) for x, y in loop] for loop in contours(field, xs, ys)]
        # the shape, the circles alone dashed, the outline on top: the dashes
        # show only where the shape leaves the circles
        for pts in loops:
            s += polyline(pts, 'none', 0, fill=PALE)
        for c in (-dist / 2, dist / 2):
            s += f'  <circle cx="{cx0 + c * scale:.1f}" cy="{cy0:.1f}" r="{R * scale:.1f}" fill="none" stroke="{GREY}" stroke-width="1.3" stroke-dasharray="4 4"/>\n'
        for pts in loops:
            s += polyline(pts, INK, 2.2)
        s += text(cx0, h - 12, name)
        left += pw * scale + gap
    return s + '</svg>\n'


if __name__ == '__main__':
    for name, fn in [('smin', smin_graph), ('metaballs', metaballs)]:
        with open(os.path.join(OUT, name + '.svg'), 'w') as f:
            f.write(fn())
        print(name)
