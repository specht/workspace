"""The globe of »Routenplaner«: Längengrade (lines of longitude, from pole
to pole) and Breitengrade (lines of latitude, parallel to the equator), and
where Berlin is: 13,4° Länge east of Greenwich, 52,5° Breite north of the
equator. The Längengrade come together at the poles, which is why a degree
of Länge is shorter in Berlin than at the equator.

    python3 src/content/routenplaner/bilder/globus.py
    ruby src/scripts/render-svg.rb src/content/routenplaner/globus.svg

A wireframe: the lines on the far side thin and dotted. One unit of the
viewBox is one CSS pixel at the width the page gives the picture (36em).
"""

import math as m
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import code  # noqa: E402

OUT = os.path.join(HERE, '..', 'globus.svg')
INK, BLUE, GREY, LINE, WHITE, AMBER = '#1f2937', '#2b5fb4', '#6b7280', '#d1d5db', '#ffffff', '#d97706'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'

W, H = 630, 410
CX, CY, R = 215, 216, 180
# seen from above the equator and a little east of Greenwich
TILT, FACING = m.radians(24), 6

BERLIN = (52.5, 13.4)


def project(lat, lon):
    """screen x, y and whether the point faces the viewer"""
    la, lo = m.radians(lat), m.radians(lon - FACING)
    x, y, z = m.cos(la) * m.sin(lo), m.sin(la), m.cos(la) * m.cos(lo)
    y, z = y * m.cos(TILT) - z * m.sin(TILT), y * m.sin(TILT) + z * m.cos(TILT)
    return CX + R * x, CY - R * y, z >= 0


def runs(points):
    """a curve given as (lat, lon) split into the parts in front and behind"""
    parts, current, front = [], [], None
    for p in points:
        x, y, f = project(*p)
        if front is not None and f != front:
            parts.append((front, current))
            current = current[-1:]
        current.append((x, y))
        front = f
    parts.append((front, current))
    return parts


def path(xy):
    return 'M' + ' L'.join(f'{x:.1f},{y:.1f}' for x, y in xy)


svg = []


def add(s):
    svg.append('  ' + s)


def curve(points, color, width, hidden=True, marker=None):
    for front, xy in runs(points):
        if len(xy) < 2:
            continue
        if front:
            mk = f' marker-end="url(#{marker})"' if marker else ''
            add(f'<path d="{path(xy)}" fill="none" stroke="{color}" stroke-width="{width}" stroke-linecap="round"{mk}/>')
        elif hidden:
            add(f'<path d="{path(xy)}" fill="none" stroke="{color}" stroke-width="0.9" stroke-dasharray="1.5 3.5" '
                f'stroke-linecap="round" opacity="0.6"/>')


def text(t, x, y, color=INK, size=18, anchor='start', weight=400, halo=False):
    # on the globe: a white edge, so that the lines behind do not cut the letters
    h = f' stroke="{WHITE}" stroke-width="5" stroke-linejoin="round" paint-order="stroke"' if halo else ''
    add(f'<text x="{x:.1f}" y="{y:.1f}" fill="{color}" font-size="{size}" font-weight="{weight}" '
        f'text-anchor="{anchor}"{h}>{t}</text>')


def steps(a, b, n=120):
    return [a + (b - a) * i / n for i in range(n + 1)]


# the outline of the globe
add(f'<circle cx="{CX}" cy="{CY}" r="{R}" fill="{WHITE}" stroke="{INK}" stroke-width="1.6"/>')

# the grid: a Längengrad every 30 degrees, a Breitengrad every 30 degrees
for lon in range(-180, 180, 30):
    curve([(lat, lon) for lat in steps(-90, 90)], BLUE, 1.1)
for lat in (-60, -30, 30, 60):
    curve([(lat, lon) for lon in steps(-180, 180, 240)], AMBER, 1.1)

# the equator and the Nullmeridian, stronger
curve([(0, lon) for lon in steps(-180, 180, 240)], AMBER, 2)
curve([(lat, 0) for lat in steps(-90, 90)], BLUE, 2)

# how far: along the equator to the east, then up Berlin's Längengrad to the north
curve([(0, lon) for lon in steps(0, BERLIN[1], 30)], AMBER, 4, marker='head-amber')
lats = steps(0, BERLIN[0], 60)
bx, by, _ = project(*BERLIN)
# the arrow's tip on the edge of Berlin's dot
trimmed = [p for p in lats if m.dist(project(p, BERLIN[1])[:2], (bx, by)) > 6.5]
curve([(lat, BERLIN[1]) for lat in trimmed], BLUE, 4, marker='head-blue')

add(f'<circle cx="{bx:.1f}" cy="{by:.1f}" r="5" fill="{INK}" stroke="{WHITE}" stroke-width="1.5"/>')

# labels on the globe
text('Berlin', bx + 11, by + 6, INK, 18, 'start', 600, halo=True)
px, py, _ = project(90, 0)
add(f'<circle cx="{px:.1f}" cy="{py:.1f}" r="3.5" fill="{INK}"/>')
text('Nordpol', px - 12, py - 4, INK, 18, 'end', halo=True)
ex, ey, _ = project(0, -36)
text('Äquator', ex, ey + 24, AMBER, 18, 'middle', 600, halo=True)
# the Nullmeridian named beside it, in the south
gx, gy, _ = project(-30, 0)
text('Nullmeridian', gx - 10, gy, BLUE, 18, 'end', 600, halo=True)
text('(Greenwich)', gx - 10, gy + 22, BLUE, 18, 'end', 400, halo=True)
fx, fy, _ = project(0, BERLIN[1] / 2)
text('13,4°', fx, fy + 25, AMBER, 18, 'middle', 600, halo=True)
mx, my, _ = project(BERLIN[0] / 2, BERLIN[1])
text('52,5°', mx + 10, my + 6, BLUE, 18, 'start', 600, halo=True)

# the explanation on the right
x0 = 432
blocks = [
    (BLUE, 'Längengrade', 'lines of longitude', ['laufen von Pol zu Pol.', 'Die Länge zählt', 'nach Osten:'], 'lon'),
    (AMBER, 'Breitengrade', 'lines of latitude', ['laufen parallel zum', 'Äquator. Die Breite', 'zählt nach Norden:'], 'lat'),
]
y = 52
for color, name, english, lines, short in blocks:
    add(f'<line x1="{x0}" y1="{y - 5}" x2="{x0 + 22}" y2="{y - 5}" stroke="{color}" stroke-width="4" stroke-linecap="round"/>')
    text(name, x0 + 30, y, color, 20, 'start', 600)
    text(english, x0, y + 25, GREY, 18, 'start')
    for i, line in enumerate(lines):
        text(line, x0, y + 56 + i * 23, INK, 18)
    svg.append('  ' + code(short, x0, y + 56 + len(lines) * 23 + 4, 18))
    y += 205

head = ''.join(f'<marker id="head-{n}" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="3.6" markerHeight="3.6" '
               f'orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="{c}"/></marker>'
               for n, c in (('blue', BLUE), ('amber', AMBER)))
with open(OUT, 'w') as f:
    f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" '
            f'font-family="{FONT}">\n'
            '  <!-- Der Globus mit Längen- und Breitengraden und Berlin.\n'
            '       Gemacht von bilder/globus.py; die dunkle Fassung globus-dark.svg\n'
            '       und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <defs>{head}</defs>\n'
            f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n'
            + '\n'.join(svg) + '\n</svg>\n')
print(OUT)
