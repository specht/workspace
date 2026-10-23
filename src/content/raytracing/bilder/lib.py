"""Drawing helpers for the diagrams of »Raytracing in C« (diagramme.py).

Each picture is drawn at the size it is shown: one unit of the viewBox is one
CSS pixel when the page gives it style='width: W/16 em', so text at 16 is
about as big as the text around it. Labels read like the page: formulas
through tex() (LaTeX, diagram_labels.math), names from the program through
code() (diagram_labels.code), right angles through right_angle() (the German
mark: a quarter arc with a dot).

Axes as everywhere on the site: in 2D x to the right and y up; in 3D x to the
right, y up and z towards the viewer. The pictures in 3D use View, an
orthographic projection from a viewpoint, so that a vector that is computed
in 3D (a cross product, the camera's right) is drawn where it really points.
"""

import math
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import diagram_labels  # noqa: E402
from PIL import ImageFont  # noqa: E402

OUT = os.path.abspath(os.path.join(HERE, '..'))
ROOT = os.path.abspath(os.path.join(HERE, '..', '..', '..', '..'))

# the diagram palette (render-svg.rb maps it for dark mode)
INK = '#1f2937'
BLUE = '#2b5fb4'
GREY = '#6b7280'
RED = '#dc2626'
GREEN = '#16a34a'
AMBER = '#d97706'
PALE = '#eef3fb'
LINE = '#d1d5db'
LIGHT = '#f3f4f6'
WHITE = '#ffffff'
SHADOW = '#0b0f19'

FS = 16   # text
CS = 15   # code
MS = 17   # formulas
FONT = '/usr/share/fonts/truetype/ibm-plex/IBMPlexSans-Regular.ttf'
_fonts = {}


# ---------------------------------------------------------------- 2D vectors

def add(a, b):
    return (a[0] + b[0], a[1] + b[1])


def sub(a, b):
    return (a[0] - b[0], a[1] - b[1])


def mul(a, s):
    return (a[0] * s, a[1] * s)


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1]


def length(a):
    return math.hypot(a[0], a[1])


def unit(a):
    n = length(a)
    return (a[0] / n, a[1] / n)


def perp(a):
    """a turned by 90°, counterclockwise on the screen"""
    return (a[1], -a[0])


def polar(deg, r=1.0):
    """A direction on the screen: 0° to the right, 90° up (y of the screen
    points down, so up is negative)."""
    t = math.radians(deg)
    return (math.cos(t) * r, -math.sin(t) * r)


def lerp(a, b, t):
    return (a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t)


def ray_circle(o, d, c, r, far=False):
    """The t > 0 where o + t·d (d of length 1) meets the circle, or None."""
    oc = sub(o, c)
    b = dot(oc, d)
    cc = dot(oc, oc) - r * r
    disc = b * b - cc
    if disc < 0:
        return None
    s = math.sqrt(disc)
    ts = [t for t in (-b - s, -b + s) if t > 1e-6]
    if not ts:
        return None
    return ts[-1] if far else ts[0]


def refract(d, n, eta):
    """As glass() in the program: d and n of length 1, n against d."""
    cos_in = -dot(d, n)
    k = 1 - eta * eta * (1 - cos_in * cos_in)
    if k < 0:
        return None
    return add(mul(d, eta), mul(n, eta * cos_in - math.sqrt(k)))


def reflect(d, n):
    return sub(d, mul(n, 2 * dot(d, n)))


# ---------------------------------------------------------------- 3D

def add3(a, b):
    return tuple(x + y for x, y in zip(a, b))


def sub3(a, b):
    return tuple(x - y for x, y in zip(a, b))


def mul3(a, s):
    return tuple(x * s for x in a)


def dot3(a, b):
    return sum(x * y for x, y in zip(a, b))


def cross3(a, b):
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0])


def norm3(a):
    n = math.sqrt(dot3(a, a))
    return tuple(x / n for x in a)


class View:
    """Orthographic projection: the viewer stands in the direction given by
    yaw (around y, from +z towards +x) and pitch (above the floor), looks at
    origin_px; scale pixels per unit. x right, y up, z towards the viewer."""

    def __init__(self, yaw, pitch, scale, origin_px):
        y, p = math.radians(yaw), math.radians(pitch)
        self.back = (math.sin(y) * math.cos(p), math.sin(p), math.cos(y) * math.cos(p))
        self.right = norm3(cross3((0, 1, 0), self.back))
        self.up = cross3(self.back, self.right)
        self.scale = scale
        self.o = origin_px

    def __call__(self, p):
        return (self.o[0] + dot3(p, self.right) * self.scale, self.o[1] - dot3(p, self.up) * self.scale)

    def depth(self, p):
        """larger is nearer to the viewer"""
        return dot3(p, self.back)


# ---------------------------------------------------------------- text sizes

def text_width(s, size=FS):
    if size not in _fonts:
        _fonts[size] = ImageFont.truetype(FONT, size)
    return _fonts[size].getlength(s)


def tex_width(t, size=MS):
    svg = diagram_labels._typeset(t)
    return float(re.search(r"viewBox='([^']*)'", svg).group(1).split()[2]) * size / 10


def code_width(s, size=CS):
    return 0.6 * size * len(s)


def f(x):
    s = '%.1f' % x
    return '0.0' if s == '-0.0' else s


# ---------------------------------------------------------------- the picture

class Svg:
    def __init__(self, name, w, h, comment):
        self.name, self.w, self.h, self.comment = name, w, h, comment
        self.parts = []

    def raw(self, s):
        self.parts.append(s)

    # lines and shapes
    def line(self, a, b, col=INK, w=2, dash=None, cap=None, opacity=None):
        extra = ' stroke-dasharray="%s"' % dash if dash else ''
        extra += ' stroke-linecap="%s"' % cap if cap else ''
        extra += ' stroke-opacity="%s"' % opacity if opacity else ''
        self.raw('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="%s" stroke-width="%s"%s/>'
                 % (f(a[0]), f(a[1]), f(b[0]), f(b[1]), col, w, extra))

    def head(self, tip, d, col, size):
        """A filled arrowhead with its tip at tip, pointing along d."""
        d = unit(d)
        p = perp(d)
        base = sub(tip, mul(d, size))
        a = add(base, mul(p, size * 0.4))
        b = sub(base, mul(p, size * 0.4))
        self.raw('<polygon points="%s,%s %s,%s %s,%s" fill="%s"/>'
                 % (f(tip[0]), f(tip[1]), f(a[0]), f(a[1]), f(b[0]), f(b[1]), col))

    def arrow(self, a, b, col=INK, w=2.2, dash=None, size=None, gap=0):
        """An arrow from a to b; gap stops the tip that far before b (on the
        border of a dot or circle there)."""
        size = size or (7 + 2.2 * w)
        d = unit(sub(b, a))
        b = sub(b, mul(d, gap))
        self.line(a, sub(b, mul(d, size * 0.7)), col, w, dash)
        self.head(b, d, col, size)

    def polyline(self, pts, col=INK, w=2, dash=None, fill='none', opacity=None):
        extra = ' stroke-dasharray="%s"' % dash if dash else ''
        extra += ' fill-opacity="%s"' % opacity if opacity is not None else ''
        s = ' '.join('%s,%s' % (f(x), f(y)) for x, y in pts)
        self.raw('<polyline points="%s" fill="%s" stroke="%s" stroke-width="%s" stroke-linejoin="round"%s/>'
                 % (s, fill, col, w, extra))

    def polygon(self, pts, fill=PALE, stroke=None, w=2, opacity=None, dash=None):
        s = ' '.join('%s,%s' % (f(x), f(y)) for x, y in pts)
        extra = ' stroke="%s" stroke-width="%s"' % (stroke, w) if stroke else ''
        extra += ' fill-opacity="%s"' % opacity if opacity is not None else ''
        extra += ' stroke-dasharray="%s"' % dash if dash else ''
        self.raw('<polygon points="%s" fill="%s"%s stroke-linejoin="round"/>' % (s, fill, extra))

    def rect(self, x, y, w, h, fill=PALE, stroke=None, sw=1.5, opacity=None):
        extra = ' stroke="%s" stroke-width="%s"' % (stroke, sw) if stroke else ''
        extra += ' fill-opacity="%s"' % opacity if opacity is not None else ''
        self.raw('<rect x="%s" y="%s" width="%s" height="%s" fill="%s"%s/>' % (f(x), f(y), f(w), f(h), fill, extra))

    def circle(self, c, r, fill='none', stroke=INK, w=2, dash=None, opacity=None):
        extra = ' stroke-dasharray="%s"' % dash if dash else ''
        extra += ' fill-opacity="%s"' % opacity if opacity is not None else ''
        st = ' stroke="%s" stroke-width="%s"' % (stroke, w) if stroke else ''
        self.raw('<circle cx="%s" cy="%s" r="%s" fill="%s"%s%s/>' % (f(c[0]), f(c[1]), f(r), fill, st, extra))

    def dot(self, c, r=4, col=INK):
        self.circle(c, r, fill=col, stroke=None)

    def hollow(self, c, r=4.5, col=INK):
        self.circle(c, r, fill=WHITE, stroke=col, w=2)

    def path(self, d, col=INK, w=2, dash=None, fill='none'):
        extra = ' stroke-dasharray="%s"' % dash if dash else ''
        self.raw('<path d="%s" fill="%s" stroke="%s" stroke-width="%s"%s/>' % (d, fill, col, w, extra))

    def arc(self, c, r, a0, a1, col=GREY, w=1.5):
        """An arc around c from the screen angle a0 to a1 (degrees, 0° right,
        90° up), the short way."""
        p0, p1 = add(c, polar(a0, r)), add(c, polar(a1, r))
        large = 1 if abs(a1 - a0) > 180 else 0
        sweep = 0 if a1 > a0 else 1
        self.path('M%s,%s A%s,%s 0 %d %d %s,%s' % (f(p0[0]), f(p0[1]), f(r), f(r), large, sweep, f(p1[0]), f(p1[1])), col, w)

    def cross(self, c, r=6, col=RED, w=2.5, along=None):
        """A cross at c; along: the direction of a line through it, so that its
        arms stand 45 degrees on either side of that line and none lies on it."""
        d = unit(along) if along else (1, 0)
        k = 1 / math.sqrt(2)
        for a in ((d[0] * k - d[1] * k, d[0] * k + d[1] * k), (d[0] * k + d[1] * k, -d[0] * k + d[1] * k)):
            arm = (a[0] * r * math.sqrt(2), a[1] * r * math.sqrt(2))
            self.line(sub(c, arm), add(c, arm), col, w, cap='round')

    def right_angle(self, corner, u, v, radius=13, col=INK):
        """The German mark at corner between the screen directions u and v."""
        u, v = unit(u), unit(v)
        self.raw(diagram_labels.right_angle(corner[0], corner[1], u[0], u[1], v[0], v[1], radius, col))

    def right_angle_3d(self, view, corner, u, v, radius=0.3, col=INK, samples=12):
        """The German mark in 3D: a quarter circle in the plane of u and v
        (both of length 1, at right angles), projected, with a dot."""
        pts = []
        for i in range(samples + 1):
            t = math.pi / 2 * i / samples
            pts.append(view(add3(corner, add3(mul3(u, radius * math.cos(t)), mul3(v, radius * math.sin(t))))))
        self.polyline(pts, col, 1.2)
        m = add3(corner, mul3(add3(u, v), radius * 0.42))
        self.dot(view(m), 1.6, col)

    # labels
    def text(self, p, s, col=INK, anchor='middle', size=FS, italic=False):
        s = s.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
        extra = ' font-style="italic"' if italic else ''
        self.raw('<text x="%s" y="%s" font-size="%s" fill="%s" text-anchor="%s" xml:space="preserve"%s>%s</text>'
                 % (f(p[0]), f(p[1]), size, col, anchor, extra, s))

    def code(self, p, s, anchor='start', size=CS):
        self.raw(diagram_labels.code(s, p[0], p[1], size, anchor))

    def tex(self, p, t, col=INK, anchor='start', size=MS):
        self.raw(diagram_labels.math(t, p[0], p[1], size, col, anchor))

    def label(self, p, parts, col=INK, anchor='start'):
        """Several parts on one baseline: a string is text, ('c', s) code,
        ('m', t) a formula."""
        widths = []
        for q in parts:
            if isinstance(q, str):
                widths.append(text_width(q))
            elif q[0] == 'c':
                widths.append(code_width(q[1]) + 0.6 * CS)
            else:
                widths.append(tex_width(q[1]))
        total = sum(widths)
        x = {'start': p[0], 'middle': p[0] - total / 2, 'end': p[0] - total}[anchor]
        for q, w in zip(parts, widths):
            if isinstance(q, str):
                self.text((x, p[1]), q, col, 'start')
            elif q[0] == 'c':
                self.code((x + 0.3 * CS, p[1]), q[1])
            else:
                self.tex((x, p[1]), q[1], col)
            x += w
        return total

    def save(self, page_width=None):
        head = ('<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
                'viewBox="0 0 %d %d" width="%d" height="%d" font-family="IBM Plex Sans, DejaVu Sans, sans-serif">\n'
                '  <!-- %s\n'
                '       Gemacht von bilder/diagramme.py; auf der Seite mit style=\'width: %sem\'.\n'
                '       Als Bild, hell und dunkel: src/scripts/render-svg.rb src/content/raytracing/%s.svg %d -->\n'
                '  <rect width="%d" height="%d" fill="%s"/>\n'
                % (self.w, self.h, self.w, self.h, self.comment, ('%g' % (self.w / 16)), self.name,
                   2 * self.w, self.w, self.h, WHITE))
        body = ''.join('  %s\n' % p for p in self.parts)
        path = os.path.join(OUT, self.name + '.svg')
        with open(path, 'w') as fh:
            fh.write(head + body + '</svg>\n')
        return path


def render(path, w):
    subprocess.run(['ruby', os.path.join(ROOT, 'src', 'scripts', 'render-svg.rb'), path, str(2 * w)], check=True,
                   stdout=subprocess.DEVNULL)
