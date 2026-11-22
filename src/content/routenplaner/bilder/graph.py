"""From ways to a graph (»Routenplaner«, »Vom Weg zum Graphen«): the three
ways of data/test.tsv on the left, the graph build_graph makes of them on
the right, every edge in both directions with its length in metres.

    python3 src/content/routenplaner/bilder/graph.py
    ruby src/scripts/render-svg.rb src/content/routenplaner/graph.svg

One unit of the viewBox is one CSS pixel at the width the page gives the
picture (36em); the text is large enough to read on a phone.
"""

import math as m
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import diagram_labels  # noqa: E402,F401  (runs the script in docker/drawings)

OUT = os.path.join(HERE, '..', 'graph.svg')
INK, BLUE, GREY, LINE, WHITE, PALE = '#1f2937', '#2b5fb4', '#6b7280', '#d1d5db', '#ffffff', '#eef3fb'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'

W, H = 640, 262
# the points of data/test.tsv: x east, y north, in steps of 0.001 degrees
POINTS = {'1': (0, 0), '2': (1, 0), '3': (2, 0), '4': (1, 1), '5': (2, 1)}
WAYS = [('Ahornweg', ['1', '2', '3']), ('Fußweg', ['2', '4']), ('Birkenweg', ['3', '5', '4'])]
# metres between two points, as distance computes them
LENGTH = {0: '67,8 m', 1: '111,2 m'}

svg = []


def add(s):
    svg.append('  ' + s)


def text(t, x, y, color=INK, size=18, anchor='middle', weight=400):
    add(f'<text x="{x:.1f}" y="{y:.1f}" fill="{color}" font-size="{size}" font-weight="{weight}" '
        f'text-anchor="{anchor}">{t}</text>')


def place(left, p):
    x, y = POINTS[p]
    return left + 30 + x * 105, 200 - y * 115


# left: the ways, as on a map
L = 18
text('drei Wege', L + 135, 34, INK, 18, 'middle', 600)
for name, points in WAYS:
    xy = [place(L, p) for p in points]
    # the Fußweg dashed, the streets solid
    style = ' stroke-dasharray="8 6" stroke-linecap="butt"' if name == 'Fußweg' else ' stroke-linecap="round"'
    add(f'<path d="M{" L".join(f"{x:.1f},{y:.1f}" for x, y in xy)}" fill="none" stroke="{GREY}" '
        f'stroke-width="7" stroke-linejoin="round" opacity="0.45"{style}/>')
for p in POINTS:
    x, y = place(L, p)
    add(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="4" fill="{INK}"/>')
    text(p, x + (-12 if p in '14' else 12), y + (24 if y > 150 else -12), INK, 16, 'middle')
x1, y1 = place(L, '1')
x3, _ = place(L, '3')
x2, _ = place(L, '2')
text('Ahornweg', (x1 + x2) / 2, y1 + 28, GREY, 17, 'middle')
x2, y2 = place(L, '2')
x4, y4 = place(L, '4')
text('Fußweg', x2 - 10, (y2 + y4) / 2 + 6, GREY, 17, 'end')
x5, y5 = place(L, '5')
text('Birkenweg', (x4 + x5) / 2, y5 - 22, GREY, 17, 'middle')

# the arrow between
add(f'<path d="M290,128 L338,128" stroke="{GREY}" stroke-width="2" marker-end="url(#head)"/>')

# right: the graph
R = 362
text('der Graph', R + 135, 34, INK, 18, 'middle', 600)
edges = []
for _, points in WAYS:
    edges += list(zip(points, points[1:]))
NODE = 15
for a, b in edges:
    (xa, ya), (xb, yb) = place(R, a), place(R, b)
    d = m.dist((xa, ya), (xb, yb))
    ux, uy = (xb - xa) / d, (yb - ya) / d
    gap = NODE + 2
    add(f'<line x1="{xa + ux * gap:.1f}" y1="{ya + uy * gap:.1f}" x2="{xb - ux * gap:.1f}" y2="{yb - uy * gap:.1f}" '
        f'stroke="{INK}" stroke-width="1.8" marker-start="url(#head-ink)" marker-end="url(#head-ink)"/>')
    vertical = abs(ux) < 0.5
    mx, my = (xa + xb) / 2, (ya + yb) / 2
    if vertical:
        text(LENGTH[1], mx - 8, my + 6, BLUE, 16, 'end')
    else:
        text(LENGTH[0], mx, my + (24 if my > 150 else -10), BLUE, 16, 'middle')
for p in POINTS:
    x, y = place(R, p)
    add(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{NODE}" fill="{PALE}" stroke="{BLUE}" stroke-width="2"/>')
    text(p, x, y + 6, INK, 17, 'middle', 600)

head = (f'<marker id="head" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" markerHeight="6" orient="auto">'
        f'<path d="M0,0 L10,5 L0,10 z" fill="{GREY}"/></marker>'
        f'<marker id="head-ink" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="5" markerHeight="5" '
        f'orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="{INK}"/></marker>')
with open(OUT, 'w') as f:
    f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" '
            f'font-family="{FONT}">\n'
            '  <!-- Die drei Wege aus data/test.tsv und der Graph daraus.\n'
            '       Gemacht von bilder/graph.py; die dunkle Fassung graph-dark.svg\n'
            '       und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <defs>{head}</defs>\n'
            f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n'
            + '\n'.join(svg) + '\n</svg>\n')
print(OUT)
