"""From ways to a graph (»Routenplaner«): the three ways of data/test.tsv on
the left, a graph made of them on the right.

- graph.svg (»Vom Weg zum Graphen«): build_graph's, every edge in both
  directions with its length in metres;
- graph-auto.svg (»Zu Fuß, mit dem Rad, mit dem Auto«): build_travel_graph's
  for the car: no Fußweg, the Birkenweg (oneway: yes) only in the direction
  of its points, the lengths in seconds at 25 km/h, the car's speed in a
  residential street.

    python3 src/content/routenplaner/bilder/graph.py
    for f in graph graph-auto; do ruby src/scripts/render-svg.rb src/content/routenplaner/$f.svg; done

One unit of the viewBox is one CSS pixel at the width the page gives the
picture (36em); the text is large enough to read on a phone.
"""

import math as m
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import diagram_labels  # noqa: E402,F401  (runs the script in docker/drawings)

INK, BLUE, GREY, LINE, WHITE, PALE = '#1f2937', '#2b5fb4', '#6b7280', '#d1d5db', '#ffffff', '#eef3fb'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'

W, H = 640, 262
# the points of data/test.tsv: x east, y north, in steps of 0.001 degrees
POINTS = {'1': (0, 0), '2': (1, 0), '3': (2, 0), '4': (1, 1), '5': (2, 1)}
WAYS = [('Ahornweg', ['1', '2', '3']), ('Fußweg', ['2', '4']), ('Birkenweg', ['3', '5', '4'])]
# metres between two points, as distance computes them; seconds by car at
# 25 km/h (67.8 / (25 / 3.6), 111.2 / (25 / 3.6)): horizontal, vertical
LENGTH = {False: ('67,8 m', '111,2 m'), True: ('9,8 s', '16,0 s')}
NODE = 15

svg = []


def add(s):
    svg.append('  ' + s)


def text(t, x, y, color=INK, size=18, anchor='middle', weight=400):
    add(f'<text x="{x:.1f}" y="{y:.1f}" fill="{color}" font-size="{size}" font-weight="{weight}" '
        f'text-anchor="{anchor}">{t}</text>')


def place(left, p):
    x, y = POINTS[p]
    return left + 30 + x * 105, 200 - y * 115


def unit(a, b):
    (xa, ya), (xb, yb) = a, b
    d = m.dist(a, b)
    return (xb - xa) / d, (yb - ya) / d


def ways(left, car):
    """the ways, as on a map; for the car the Birkenweg's direction"""
    text('drei Wege', left + 135, 34, INK, 18, 'middle', 600)
    for name, points in WAYS:
        xy = [place(left, p) for p in points]
        # the Fußweg dashed, the streets solid
        style = ' stroke-dasharray="8 6" stroke-linecap="butt"' if name == 'Fußweg' else ' stroke-linecap="round"'
        add(f'<path d="M{" L".join(f"{x:.1f},{y:.1f}" for x, y in xy)}" fill="none" stroke="{GREY}" '
            f'stroke-width="7" stroke-linejoin="round" opacity="0.45"{style}/>')
        if car and name == 'Birkenweg':
            # a chevron on each part: only in the direction of its points
            for a, b in zip(xy, xy[1:]):
                mx, my = (a[0] + b[0]) / 2, (a[1] + b[1]) / 2
                ux, uy = unit(a, b)
                add(f'<path d="M{mx - ux * 9 - uy * 7:.1f},{my - uy * 9 + ux * 7:.1f} L{mx + ux * 5:.1f},{my + uy * 5:.1f} '
                    f'L{mx - ux * 9 + uy * 7:.1f},{my - uy * 9 - ux * 7:.1f}" fill="none" stroke="{INK}" stroke-width="2.2" '
                    f'stroke-linecap="round" stroke-linejoin="round"/>')
    for p in POINTS:
        x, y = place(left, p)
        add(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="4" fill="{INK}"/>')
        text(p, x + (-12 if p in '14' else 12), y + (24 if y > 150 else -12), INK, 16, 'middle')
    x1, y1 = place(left, '1')
    x2, y2 = place(left, '2')
    x4, y4 = place(left, '4')
    x5, y5 = place(left, '5')
    text('Ahornweg', (x1 + x2) / 2, y1 + 28, GREY, 17, 'middle')
    text('Fußweg', x2 - 10, (y2 + y4) / 2 + 6, GREY, 17, 'end')
    text('Birkenweg', (x4 + x5) / 2, y5 - 22, GREY, 17, 'middle')


def graph(left, car):
    """the graph: nodes, edges with arrows in the directions they hold"""
    text('für das Auto' if car else 'der Graph', left + 135, 34, INK, 18, 'middle', 600)
    for name, points in WAYS:
        if car and name == 'Fußweg':
            continue
        both = not (car and name == 'Birkenweg')
        for a, b in zip(points, points[1:]):
            pa, pb = place(left, a), place(left, b)
            ux, uy = unit(pa, pb)
            gap = NODE + 2
            start = ' marker-start="url(#head-ink)"' if both else ''
            add(f'<line x1="{pa[0] + ux * gap:.1f}" y1="{pa[1] + uy * gap:.1f}" x2="{pb[0] - ux * gap:.1f}" y2="{pb[1] - uy * gap:.1f}" '
                f'stroke="{INK}" stroke-width="1.8"{start} marker-end="url(#head-ink)"/>')
            mx, my = (pa[0] + pb[0]) / 2, (pa[1] + pb[1]) / 2
            if abs(ux) < 0.5:
                text(LENGTH[car][1], mx - 8, my + 6, BLUE, 16, 'end')
            else:
                text(LENGTH[car][0], mx, my + (24 if my > 150 else -10), BLUE, 16, 'middle')
    for p in POINTS:
        x, y = place(left, p)
        add(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{NODE}" fill="{PALE}" stroke="{BLUE}" stroke-width="2"/>')
        text(p, x, y + 6, INK, 17, 'middle', 600)


def draw(out, car):
    svg.clear()
    ways(18, car)
    add(f'<path d="M290,128 L338,128" stroke="{GREY}" stroke-width="2" marker-end="url(#head)"/>')
    graph(362, car)
    head = (f'<marker id="head" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" markerHeight="6" orient="auto">'
            f'<path d="M0,0 L10,5 L0,10 z" fill="{GREY}"/></marker>'
            f'<marker id="head-ink" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="5" markerHeight="5" '
            f'orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="{INK}"/></marker>')
    what = 'der Graph für das Auto daraus' if car else 'der Graph daraus'
    with open(os.path.join(HERE, '..', out + '.svg'), 'w') as f:
        f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" '
                f'font-family="{FONT}">\n'
                f'  <!-- Die drei Wege aus data/test.tsv und {what}.\n'
                f'       Gemacht von bilder/graph.py; die dunkle Fassung {out}-dark.svg\n'
                '       und die Schriften: src/scripts/render-svg.rb -->\n'
                f'  <defs>{head}</defs>\n'
                f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n'
                + '\n'.join(svg) + '\n</svg>\n')
    print(out)


draw('graph', False)
draw('graph-auto', True)
