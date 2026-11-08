"""The drawings of the Handbuch page »Vektoren«: points and directions, the
axes in space and on the screen, the ways of calculating, the dot and the
cross product, mirroring, a ray, turning in 2D.

    python3 src/content/reference/vektoren/bilder/diagramme.py
    for f in src/content/reference/vektoren/*.svg; do
        ruby src/scripts/render-svg.rb $f $(( 2 * $(grep -o 'width="[0-9]*"' $f | head -1 | tr -dc 0-9) )); done

The colours: the first vector blue, the second purple, a result amber,
helper lines grey and dashed; the axes red (x), green (y) and blue (z), as
on the pages »Matrizen« and »3D-Grafik mit WebGL«. y points up, except on
the screen. One unit of the viewBox is one CSS pixel at the width the page
gives the picture.
"""

import math as m
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', '..', 'scripts'))
from diagram_labels import math as tex, code, right_angle  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, BLUE, GREY, LINE, PALE, WHITE = '#1f2937', '#2b5fb4', '#6b7280', '#d1d5db', '#eef3fb', '#ffffff'
RED, GREEN, AMBER, PURPLE = '#dc2626', '#16a34a', '#d97706', '#7c3aed'
COLOURS = {'ink': INK, 'blue': BLUE, 'grey': GREY, 'red': RED, 'green': GREEN, 'amber': AMBER, 'purple': PURPLE}
NAMES = {v: k for k, v in COLOURS.items()}


class Picture:
    """one drawing: a viewBox of w × h pixels, the origin at (ox, oy) in
    pixels, unit pixels per unit, y up. zoom enlarges the drawing but not
    its labels and lines: for a gallery, which stretches every picture to
    its column (about 360 px in three columns, 530 px in two), so that the
    labels stay about as large as the page's text"""

    def __init__(self, name, comment, w, h, ox, oy, unit, zoom=1):
        w, h, ox, oy, unit = round(w * zoom), round(h * zoom), ox * zoom, oy * zoom, unit * zoom
        self.zoom = zoom
        self.name, self.w, self.h, self.ox, self.oy, self.unit = name, w, h, ox, oy, unit
        markers = ''.join(
            f'<marker id="arrow-{n}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="5.5" markerHeight="5.5" '
            f'orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="{c}"/></marker>'
            for n, c in COLOURS.items())
        self.s = (f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
                  f'viewBox="0 0 {w} {h}" width="{w}" height="{h}" font-family="IBM Plex Sans, DejaVu Sans, sans-serif">\n'
                  f'  <!-- {comment}\n'
                  f'       Gemacht von bilder/diagramme.py; als Bild, hell und dunkel:\n'
                  f'       src/scripts/render-svg.rb src/content/reference/vektoren/{name}.svg {2 * w} -->\n'
                  f'  <defs>{markers}</defs>\n'
                  f'  <rect width="{w}" height="{h}" fill="{WHITE}"/>\n')

    def px(self, p):
        return self.ox + p[0] * self.unit, self.oy - p[1] * self.unit

    def add(self, svg):
        self.s += '  ' + svg + '\n'

    def grid(self, labels=True):
        """grid lines at the whole numbers over the whole picture, the axes
        through the origin"""
        xs = range(m.floor(-self.ox / self.unit), m.ceil((self.w - self.ox) / self.unit) + 1)
        ys = range(m.floor((self.oy - self.h) / self.unit), m.ceil(self.oy / self.unit) + 1)
        for k in xs:
            x, _ = self.px((k, 0))
            self.add(f'<line x1="{x:.1f}" y1="0" x2="{x:.1f}" y2="{self.h}" stroke="{LINE}" stroke-width="1"/>')
        for k in ys:
            _, y = self.px((0, k))
            self.add(f'<line x1="0" y1="{y:.1f}" x2="{self.w}" y2="{y:.1f}" stroke="{LINE}" stroke-width="1"/>')
        self.axes(labels)

    def axes(self, labels=True):
        self.add(f'<line x1="0" y1="{self.oy}" x2="{self.w - 3}" y2="{self.oy}" stroke="{GREY}" stroke-width="1.2" marker-end="url(#arrow-grey)"/>')
        self.add(f'<line x1="{self.ox}" y1="{self.h}" x2="{self.ox}" y2="3" stroke="{GREY}" stroke-width="1.2" marker-end="url(#arrow-grey)"/>')
        if labels:
            self.add(tex('x', self.w - 8, self.oy - 9, 17, INK, 'end'))
            self.add(tex('y', self.ox + 9, 17, 17, INK, 'start'))

    def arrow(self, a, b, color, width=2.4, dash=None, head=True):
        (x1, y1), (x2, y2) = self.px(a), self.px(b)
        d = f' stroke-dasharray="{dash}"' if dash else ''
        h = f' marker-end="url(#arrow-{NAMES[color]})"' if head else ''
        self.add(f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" stroke="{color}" '
                 f'stroke-width="{width}"{d}{h} stroke-linecap="round"/>')

    def line(self, a, b, color=GREY, width=1.3, dash='4 3'):
        self.arrow(a, b, color, width, dash, head=False)

    def dot(self, p, color=INK, r=3.6):
        x, y = self.px(p)
        self.add(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{r}" fill="{color}"/>')

    def label(self, t, p, dx=0, dy=0, color=INK, anchor='start', size=17):
        # two columns shrink to about 0.72 on a phone: at least 16 there, 11.5 on the screen
        if self.zoom >= 1.6:
            size = max(size, 16)
        x, y = self.px(p)
        self.add(tex(t, x + dx, y + dy, size, color, anchor))

    def arc(self, centre, radius, a0, a1, color=GREY, width=1.3):
        """an arc around centre (units), radius in pixels, angles in degrees, counter-clockwise"""
        cx, cy = self.px(centre)
        radius *= self.zoom
        x0, y0 = cx + radius * m.cos(m.radians(a0)), cy - radius * m.sin(m.radians(a0))
        x1, y1 = cx + radius * m.cos(m.radians(a1)), cy - radius * m.sin(m.radians(a1))
        large = 1 if (a1 - a0) % 360 > 180 else 0
        self.add(f'<path d="M{x0:.1f},{y0:.1f} A{radius},{radius} 0 {large} 0 {x1:.1f},{y1:.1f}" fill="none" '
                 f'stroke="{color}" stroke-width="{width}"/>')

    def save(self):
        with open(os.path.join(OUT, self.name + '.svg'), 'w') as f:
            f.write(self.s + '</svg>\n')
        print(self.name)


def unit_vector(degrees, length=1):
    return length * m.cos(m.radians(degrees)), length * m.sin(m.radians(degrees))


# ------------------------------------------------------------ points, axes

def point_and_direction():
    p = Picture('punkt-richtung', 'Ein Punkt P = (3, 2) hat einen Ort. Die Richtung d = (2, 1) hat keinen: Alle drei blauen '
                'Pfeile sind derselbe Vektor, nur an verschiedenen Stellen gezeichnet.', 340, 230, 34, 196, 40)
    p.grid()
    for start in [(0.4, 2.6), (2.5, 0.5), (4.6, 2.2)]:
        p.arrow(start, (start[0] + 2, start[1] + 1), BLUE)
    p.label(r'\vec d = (2, 1)', (4.6 + 1, 2.2 + 0.5), -4, -14, BLUE, 'end')
    p.dot((3, 2))
    p.label('P = (3, 2)', (3, 2), 8, 18)
    p.save()


VIEW_YAW, VIEW_PITCH = m.radians(24), m.radians(16)


def view(q, origin, unit, yaw=VIEW_YAW, pitch=VIEW_PITCH):
    """orthographic, seen a little from the right above: x right, y up, z towards the viewer"""
    x, y, z = q
    x, z = x * m.cos(yaw) - z * m.sin(yaw), x * m.sin(yaw) + z * m.cos(yaw)
    y, z = y * m.cos(pitch) - z * m.sin(pitch), y * m.sin(pitch) + z * m.cos(pitch)
    return origin[0] + x * unit, origin[1] - y * unit, z


def axes_in_space():
    p = Picture('achsen-raum', 'Die Achsen im Raum: x nach rechts (rot), y nach oben (grün), z zum Betrachter (blau). '
                'Eine Kamera vorn auf der z-Achse schaut in Richtung -z, in die Szene hinein.', 510, 300, 0, 0, 1)
    o, u = (250, 175), 70
    v = lambda q: view(q, o, u)[:2]
    ox, oy = v((0, 0, 0))
    # the negative z axis behind the origin, where the camera looks
    bx, by = v((0, 0, -2.0))
    p.add(f'<line x1="{ox:.1f}" y1="{oy:.1f}" x2="{bx:.1f}" y2="{by:.1f}" stroke="{BLUE}" stroke-width="1.4" stroke-dasharray="4 3"/>')
    p.add(tex('-z', bx + 6, by + 2, 17, INK, 'start'))
    for d, color, t, dx, dy in [((2.2, 0, 0), RED, 'x', 8, 12), ((0, 1.9, 0), GREEN, 'y', 9, 4), ((0, 0, 1.7), BLUE, 'z', 6, 16)]:
        x2, y2 = v(d)
        p.add(f'<line x1="{ox:.1f}" y1="{oy:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" stroke="{color}" stroke-width="2.2" '
              f'marker-end="url(#arrow-{NAMES[color]})"/>')
        p.add(tex(t, x2 + dx, y2 + dy, 17, INK, 'start'))
    # a camera on the z axis, in front of the arrow, looking along -z
    cx, cy = v((0, 0, 3.2))
    angle = m.degrees(m.atan2(oy - cy, ox - cx))
    p.add(f'<g transform="translate({cx:.1f} {cy:.1f}) rotate({angle:.1f})">'
          f'<rect x="-15" y="-10" width="24" height="20" rx="3" fill="{INK}"/>'
          f'<path d="M9,-6 L18,-10 L18,10 L9,6 Z" fill="{INK}"/></g>')
    p.add(f'<text x="{cx + 4:.1f}" y="{cy + 30:.1f}" font-size="16" fill="{GREY}" text-anchor="middle">Kamera</text>')
    p.save()


def axes_on_screen():
    p = Picture('achsen-bildschirm', 'Auf dem Bildschirm beginnt die Zählung oben links: x nach rechts, y nach unten, wie die '
                'Pixelzeilen. Das Pixel (3, 2) ist markiert.', 510, 300, 0, 0, 1)
    left, top, size, cols, rows = 110, 50, 38, 9, 6
    p.add(f'<rect x="{left}" y="{top}" width="{cols * size}" height="{rows * size}" fill="{PALE}" stroke="{INK}" stroke-width="1.6"/>')
    for k in range(1, cols):
        p.add(f'<line x1="{left + k * size}" y1="{top}" x2="{left + k * size}" y2="{top + rows * size}" stroke="{LINE}" stroke-width="1"/>')
    for k in range(1, rows):
        p.add(f'<line x1="{left}" y1="{top + k * size}" x2="{left + cols * size}" y2="{top + k * size}" stroke="{LINE}" stroke-width="1"/>')
    p.add(f'<rect x="{left + 3 * size}" y="{top + 2 * size}" width="{size}" height="{size}" fill="{AMBER}"/>')
    p.add(tex('(3, 2)', left + 4 * size + 6, top + 3 * size - 11, 17, INK, 'start'))
    p.add(f'<line x1="{left}" y1="{top - 14}" x2="{left + 160}" y2="{top - 14}" stroke="{RED}" stroke-width="2.2" marker-end="url(#arrow-red)"/>')
    p.add(tex('x', left + 168, top - 9, 17, INK, 'start'))
    p.add(f'<line x1="{left - 14}" y1="{top}" x2="{left - 14}" y2="{top + 160}" stroke="{GREEN}" stroke-width="2.2" marker-end="url(#arrow-green)"/>')
    p.add(tex('y', left - 19, top + 180, 17, INK, 'middle'))
    p.add(tex('(0, 0)', left - 6, top - 22, 16, GREY, 'end'))
    p.save()


# --------------------------------------------------------------- calculating

def adding():
    p = Picture('addieren', 'Addieren: b an die Spitze von a gesetzt; a + b führt vom Anfang von a zur Spitze von b. '
                'Gestrichelt dieselben Pfeile in der anderen Reihenfolge.', 240, 190, 28, 166, 36, zoom=1.5)
    p.grid()
    a, b = (3, 1), (1, 2.2)
    s = (a[0] + b[0], a[1] + b[1])
    p.arrow((0, 0), b, PURPLE, 1.4, '4 3')
    p.arrow(b, s, BLUE, 1.4, '4 3')
    p.arrow((0, 0), s, AMBER, 2.6)
    p.arrow((0, 0), a, BLUE)
    p.arrow(a, s, PURPLE)
    p.label(r'\vec a', (1.6, 0.5), 0, 18, BLUE, 'middle')
    p.label(r'\vec b', (a[0] + 0.5, a[1] + 1.1), 8, 6, PURPLE)
    p.label(r'\vec a + \vec b', s, -4, -10, AMBER, 'end')
    p.save()


def subtracting():
    p = Picture('subtrahieren', 'Subtrahieren: Von den Punkten a und b aus gesehen zeigt b - a von a nach b. '
                'Grau gestrichelt die Pfeile vom Ursprung zu den Punkten.', 240, 190, 28, 166, 36, zoom=1.5)
    p.grid()
    a, b = (1, 2.4), (4.6, 1)
    p.arrow((0, 0), a, GREY, 1.3, '4 3')
    p.arrow((0, 0), b, GREY, 1.3, '4 3')
    p.arrow(a, b, AMBER, 2.6)
    p.dot(a)
    p.dot(b)
    p.label(r'\vec a', a, -6, -8, INK, 'end')
    p.label(r'\vec b', b, 4, -10, INK, 'start')
    p.label(r'\vec b - \vec a', ((a[0] + b[0]) / 2, (a[1] + b[1]) / 2), 6, -10, AMBER)
    p.save()


def stretching():
    p = Picture('strecken', 'Strecken: 2 mal a ist doppelt so lang in dieselbe Richtung, -0,5 mal a halb so lang in die '
                'Gegenrichtung.', 240, 190, 84, 126, 36, zoom=1.5)
    p.grid()
    a = (1.5, 0.75)
    # side by side, each a little off the same line
    off = (-0.12, 0.24)
    p.arrow((0, 0), (2 * a[0], 2 * a[1]), AMBER, 2.6)
    p.arrow(off, (a[0] + off[0], a[1] + off[1]), BLUE)
    p.arrow((0, 0), (-0.5 * a[0], -0.5 * a[1]), PURPLE)
    p.label(r'\vec a', (a[0] + off[0], a[1] + off[1]), -8, -8, BLUE, 'end')
    p.label(r'2\,\vec a', (2 * a[0], 2 * a[1]), 2, 20, AMBER)
    p.label(r'-0{,}5\,\vec a', (-0.5 * a[0], -0.5 * a[1]), -2, 22, PURPLE, 'middle')
    p.save()


def length():
    p = Picture('laenge', 'Die Länge mit dem Satz des Pythagoras: a = (4, 3) ist die lange Seite eines rechtwinkligen '
                'Dreiecks mit den Seiten 4 und 3, also 5 lang.', 240, 205, 30, 176, 34, zoom=1.5)
    p.grid()
    a = (4, 3)
    p.line((0, 0), (4, 0), INK, 1.6, None)
    p.line((4, 0), a, INK, 1.6, None)
    x, y = p.px((4, 0))
    p.add(right_angle(x, y, -1, 0, 0, -1, 12, INK))
    p.arrow((0, 0), a, BLUE, 2.6)
    p.label('4', (2, 0), 0, 18, INK, 'middle')
    p.label('3', (4, 1.5), 8, 6, INK)
    p.label(r'|\vec a| = \sqrt{4^2 + 3^2} = 5', (0.35, 3.85), 0, 0, BLUE, 'start', 16)
    p.save()


def normalising():
    p = Picture('normalisieren', 'Normalisieren: a durch seine Länge geteilt hat dieselbe Richtung und die Länge 1; '
                'seine Spitze liegt auf dem Kreis mit Radius 1.', 240, 205, 60, 135, 52, zoom=1.5)
    p.grid()
    cx, cy = p.px((0, 0))
    p.add(f'<circle cx="{cx}" cy="{cy}" r="{p.unit}" fill="none" stroke="{GREY}" stroke-width="1.3" stroke-dasharray="4 3"/>')
    a = (3, 1.6)
    n = m.hypot(*a)
    p.arrow((0, 0), a, BLUE)
    p.arrow((0, 0), (a[0] / n, a[1] / n), AMBER, 3)
    p.label(r'\vec a', a, -2, -8, BLUE, 'end')
    p.label(r'\frac{\vec a}{|\vec a|}', (a[0] / n, a[1] / n), -18, -14, AMBER, 'end', 20)
    p.line((0, 0), (0, -1), GREY, 1.3, None)
    p.label('1', (0, -0.5), 6, 5, GREY, 'start', 14)
    p.save()


def blending():
    p = Picture('ueberblenden', 'Überblenden: a + t (b - a) wandert mit t von a (t = 0) nach b (t = 1); bei t = 0,5 liegt '
                'es genau in der Mitte.', 240, 190, 28, 166, 36, zoom=1.5)
    p.grid()
    a, b = (0.6, 0.8), (5, 3.4)
    p.line(a, b, GREY, 1.4, None)
    for t in (0, 0.25, 0.5, 0.75, 1):
        q = (a[0] + t * (b[0] - a[0]), a[1] + t * (b[1] - a[1]))
        p.dot(q, AMBER if 0 < t < 1 else INK, 4.2 if t in (0, 1) else 3.4)
    p.label(r'\vec a', a, 2, 22, INK, 'start')
    p.label(r'\vec b', b, -8, -8, INK, 'end')
    for t, text in [(0.25, r't = 0{,}25'), (0.5, r't = 0{,}5'), (0.75, r't = 0{,}75')]:
        q = (a[0] + t * (b[0] - a[0]), a[1] + t * (b[1] - a[1]))
        p.label(text, q, 8, 16, AMBER, 'start', 14)
    p.save()


# --------------------------------------------------------------- dot product

def projection():
    p = Picture('skalar-projektion', 'Das Skalarprodukt mit einem Einheitsvektor n: Fällt man von der Spitze von a das Lot '
                'auf die Richtung von n, ist dot(a, n) die Länge bis dorthin (orange).', 320, 210, 40, 178, 62, zoom=1.6)
    nd, ad = 12, 58                      # directions of n and a in degrees
    n = unit_vector(nd)
    a = unit_vector(ad, 2.5)
    d = a[0] * n[0] + a[1] * n[1]
    foot = (d * n[0], d * n[1])
    far = unit_vector(nd, 5.6)
    p.line((0, 0), far, LINE, 1.2, None)
    p.line(a, foot)
    fx, fy = p.px(foot)
    # in screen coordinates (y down): back along n, and up to the tip of a
    up = (a[0] - foot[0], a[1] - foot[1])
    lu = m.hypot(*up)
    p.add(right_angle(fx, fy, -n[0], n[1], up[0] / lu, -up[1] / lu, 11, GREY))
    # the length dot(a, n): a dimension line a little below the direction of n
    off = (n[1] * 0.3, -n[0] * 0.3)
    p.arrow((off[0], off[1]), (foot[0] + off[0], foot[1] + off[1]), AMBER, 2.4, head=False)
    for q in ((0, 0), foot):
        p.line((q[0] + off[0] * 0.6, q[1] + off[1] * 0.6), (q[0] + off[0] * 1.4, q[1] + off[1] * 1.4), AMBER, 2, None)
    p.arrow((0, 0), a, BLUE)
    p.arrow((0, 0), n, PURPLE, 2.8)
    p.arc((0, 0), 30, nd, ad)
    p.label(r'\theta', unit_vector((nd + ad) / 2, 0.66), 0, 6, GREY, 'middle', 15)
    p.label(r'\vec a', a, -6, 0, BLUE, 'end')
    p.label(r'\vec n', n, 10, 2, PURPLE, 'start')
    x, y = p.px((foot[0] * 0.55 + off[0], foot[1] * 0.55 + off[1]))
    p.add(code('dot(a, n)', x, y + 24, 16))
    p.save()


def signs():
    p = Picture('skalar-vorzeichen', 'Für zwei Einheitsvektoren: a zeigt nach rechts, b in verschiedene Richtungen. '
                'Dieselbe Richtung gibt 1, senkrecht 0, entgegengesetzt -1; auf der hellen Seite ist es positiv.', 320, 210, 160, 120, 74, zoom=1.6)
    cx, cy = p.px((0, 0))
    r = p.unit
    p.add(f'<path d="M{cx},{cy - r - 12} A{r + 12},{r + 12} 0 0 1 {cx},{cy + r + 12} Z" fill="{PALE}"/>')
    p.add(f'<circle cx="{cx}" cy="{cy}" r="{r}" fill="none" stroke="{GREY}" stroke-width="1.2" stroke-dasharray="4 3"/>')
    for deg, value, dx, dy, anchor in [(45, '0{,}71', 6, -4, 'start'), (90, '0', 0, -10, 'middle'),
                                       (135, '-0{,}71', -6, -4, 'end'), (180, '-1', -10, 6, 'end'),
                                       (-60, '0{,}5', 6, 14, 'start')]:
        b = unit_vector(deg)
        p.arrow((0, 0), b, PURPLE, 2)
        p.label(value, b, dx, dy, PURPLE, anchor, 15)
    p.arrow((0, 0), (1, 0), BLUE, 3)
    p.label(r'\vec a', (1, 0), 8, 6, BLUE)
    p.label('1', (1, 0), 8, -12, PURPLE, 'start', 15)
    p.dot((0, 0), INK, 3)
    p.save()


# ------------------------------------------------------------- cross product

def cross():
    p = Picture('kreuzprodukt', 'Das Kreuzprodukt: a und b liegen in einer Ebene und spannen ein Parallelogramm auf. '
                'a × b steht senkrecht darauf (kürzer gezeichnet, als es ist); b × a zeigt in die Gegenrichtung.',
                280, 270, 0, 0, 1)
    o, u, pitch = (90, 160), 50, m.radians(34)
    v = lambda q: view(q, o, u, pitch=pitch)[:2]
    a, b = (2.0, 0, 0.3), (0.9, 0, -1.6)
    c = (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0])
    # drawn 1.9 long: its true length, the area, would not fit
    lc = m.sqrt(sum(x * x for x in c))
    c = tuple(1.9 * x / lc for x in c)
    corners = [v((0, 0, 0)), v(a), v((a[0] + b[0], 0, a[2] + b[2])), v(b)]
    p.add(f'<polygon points="{" ".join(f"{x:.1f},{y:.1f}" for x, y in corners)}" fill="{PALE}" stroke="{LINE}" stroke-width="1.2"/>')
    ox, oy = v((0, 0, 0))

    def arrow3(q, color, width=2.4, dash=None):
        x2, y2 = v(q)
        d = f' stroke-dasharray="{dash}"' if dash else ''
        p.add(f'<line x1="{ox:.1f}" y1="{oy:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" stroke="{color}" stroke-width="{width}"{d} '
              f'marker-end="url(#arrow-{NAMES[color]})"/>')
        return x2, y2
    x, y = arrow3((-c[0], -c[1], -c[2]), GREY, 1.6, '5 4')
    p.add(tex(r'\vec b \times \vec a', x + 10, y - 4, 16, GREY, 'start'))
    x, y = arrow3(a, BLUE)
    p.add(tex(r'\vec a', x + 6, y + 14, 17, BLUE, 'start'))
    x, y = arrow3(b, PURPLE)
    p.add(tex(r'\vec b', x + 8, y - 4, 17, PURPLE, 'start'))
    x, y = arrow3(c, AMBER, 2.8)
    p.add(tex(r'\vec a \times \vec b', x + 10, y + 6, 17, AMBER, 'start'))
    # the right angles where the result stands on the plane
    for q in (a, b):
        qx, qy = v(q)
        length = m.hypot(qx - ox, qy - oy)
        p.add(right_angle(ox, oy, 0, -1, (qx - ox) / length, (qy - oy) / length, 13, GREY))
    p.save()


# ---------------------------------------------------------------- mirroring

def mirroring():
    p = Picture('spiegeln', 'Spiegeln an einer Fläche mit der Normale n: d läuft auf die Fläche zu; ohne Fläche liefe es '
                'gestrichelt weiter. Zweimal den Teil von d senkrecht zur Fläche abgezogen, -2 (d · n) n, kommt r heraus, '
                'im selben Winkel auf der anderen Seite.', 400, 250, 160, 140, 44)
    p.add(f'<rect x="0" y="{p.oy}" width="{p.w}" height="{p.h - p.oy}" fill="{PALE}"/>')
    p.line((-4, 0), (5.5, 0), INK, 2, None)
    d = (2.2, -1.6)
    p.arrow((-d[0], -d[1]), (0, 0), BLUE)
    p.arrow((0, 0), (0, 1.4), INK, 2.2)
    p.label(r'\vec n', (0, 1.4), 8, 6, INK)
    p.arrow((0, 0), d, BLUE, 1.4, '5 4')
    r = (d[0], -d[1])
    p.arrow(d, r, PURPLE, 2.2)
    p.arrow((0, 0), r, AMBER, 2.6)
    p.label(r'\vec d', (-d[0] / 2, -d[1] / 2), -8, -6, BLUE, 'end')
    p.label(r'\vec r', (r[0] / 2, r[1] / 2), -10, -8, AMBER, 'end')
    p.label(r'-2\,(\vec d \cdot \vec n)\,\vec n', (d[0], d[1] / 2), 10, 6, PURPLE, 'start', 16)
    p.save()


# -------------------------------------------------------------------- rays

def ray():
    p = Picture('strahl', 'Ein Strahl p(t) = origin + t · dir mit einer Richtung der Länge 1: t zählt die Schritte vom '
                'Startpunkt aus. Hinter dem Startpunkt (t kleiner 0) zählt nichts. Er trifft den Kreis zweimal; '
                'der Treffer ist das kleinere t.', 420, 220, 70, 160, 46)
    deg = 12
    dirv = unit_vector(deg)
    pt = lambda t: (t * dirv[0], t * dirv[1])
    centre, radius = (5.6, 1.6), 1.25
    cx, cy = p.px(centre)
    p.add(f'<circle cx="{cx:.1f}" cy="{cy:.1f}" r="{radius * p.unit:.1f}" fill="{PALE}" stroke="{INK}" stroke-width="1.6"/>')
    # hits: |o + t d - c| = r
    bq = -(dirv[0] * centre[0] + dirv[1] * centre[1])
    cq = centre[0] ** 2 + centre[1] ** 2 - radius ** 2
    t1, t2 = -bq - m.sqrt(bq * bq - cq), -bq + m.sqrt(bq * bq - cq)
    p.line(pt(-1.2), (0, 0), GREY, 1.4, '4 3')
    p.line((0, 0), pt(8.0), AMBER, 2, None)
    for t in range(1, 8):
        x, y = p.px(pt(t))
        nx, ny = -dirv[1] * 5, -dirv[0] * 5
        p.add(f'<line x1="{x - nx:.1f}" y1="{y + ny:.1f}" x2="{x + nx:.1f}" y2="{y - ny:.1f}" stroke="{AMBER}" stroke-width="1.6"/>')
        if t <= 3:
            p.label(f't = {t}', pt(t), 0, 22, AMBER, 'middle', 13)
    p.label('t < 0', pt(-0.7), 0, 22, GREY, 'middle', 13)
    p.arrow((0, 0), dirv, BLUE, 3)
    p.dot((0, 0))
    x, y = p.px((0, 0))
    p.add(code('origin', x - 8, y - 12, 14, 'end'))
    x, y = p.px(dirv)
    p.add(code('dir', x - 4, y - 14, 14, 'middle'))
    p.dot(pt(t1), INK, 4.6)
    p.dot(pt(t2), GREY, 3.6)
    p.label(r't_1', pt(t1), -8, -10, INK, 'end', 15)
    p.label(r't_2', pt(t2), 8, -10, GREY, 'start', 15)
    p.save()


# ------------------------------------------------------------------ turning

def unit_circle():
    p = Picture('einheitskreis', 'Die Richtung zum Winkel alpha: auf dem Kreis mit Radius 1 der Punkt (cos alpha, sin alpha). '
                'Gestrichelt seine Abstände zu den Achsen.', 320, 250, 130, 135, 100, zoom=1.6)
    p.axes()
    cx, cy = p.px((0, 0))
    p.add(f'<circle cx="{cx}" cy="{cy}" r="{p.unit}" fill="none" stroke="{GREY}" stroke-width="1.2" stroke-dasharray="4 3"/>')
    alpha = 38
    q = unit_vector(alpha)
    p.line(q, (q[0], 0))
    p.line(q, (0, q[1]))
    p.arc((0, 0), 30, 0, alpha)
    p.label(r'\alpha', unit_vector(alpha / 2, 0.38), 0, 6, GREY, 'middle', 15)
    p.arrow((0, 0), q, BLUE, 2.6)
    p.label(r'(\cos\alpha, \sin\alpha)', q, 6, -10, BLUE, 'start', 16)
    p.label(r'\cos\alpha', (q[0] / 2, 0), 0, 20, INK, 'middle', 15)
    p.label(r'\sin\alpha', (0, q[1] / 2), -6, 6, INK, 'end', 15)
    p.label('1', (1, 0), 4, 20, GREY, 'start', 14)
    p.save()


def turning():
    p = Picture('drehen-2d', 'Drehen in 2D: v um den Winkel alpha gegen den Uhrzeigersinn gedreht (orange), und um 90 Grad '
                'gedreht, (-y, x), mit dem rechten Winkel (lila).', 320, 250, 150, 175, 44, zoom=1.6)
    p.grid()
    v = (2.4, 1)
    alpha = 40
    ca, sa = m.cos(m.radians(alpha)), m.sin(m.radians(alpha))
    w = (v[0] * ca - v[1] * sa, v[0] * sa + v[1] * ca)
    q = (-v[1], v[0])
    base = m.degrees(m.atan2(v[1], v[0]))
    p.arc((0, 0), 44, base, base + alpha)
    p.label(r'\alpha', unit_vector(base + alpha / 2, 1.4), 0, 6, GREY, 'middle', 15)
    ox, oy = p.px((0, 0))
    lv, lq = m.hypot(*v), m.hypot(*q)
    p.add(right_angle(ox, oy, v[0] / lv, -v[1] / lv, q[0] / lq, -q[1] / lq, 13, PURPLE))
    p.arrow((0, 0), q, PURPLE, 2)
    p.arrow((0, 0), w, AMBER, 2.6)
    p.arrow((0, 0), v, BLUE, 2.6)
    p.label('(x, y)', v, 6, 14, BLUE, 'start', 15)
    p.label('(-y, x)', q, -6, -6, PURPLE, 'end', 15)
    p.label(r'\text{um } \alpha \text{ gedreht}', w, 6, -6, AMBER, 'start', 15)
    p.save()


if __name__ == '__main__':
    point_and_direction()
    axes_in_space()
    axes_on_screen()
    adding()
    subtracting()
    stretching()
    length()
    normalising()
    blending()
    projection()
    signs()
    cross()
    mirroring()
    ray()
    unit_circle()
    turning()
