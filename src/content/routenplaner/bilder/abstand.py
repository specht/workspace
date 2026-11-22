"""The distance of two points on a city map (»Routenplaner«, »Wie weit ist
es?«): on a few kilometres the Earth is nearly flat, so the two steps, east
and north, are the legs of a right triangle and the distance is its
hypotenuse. The step east counts less the further north: times cos(Breite).

    python3 src/content/routenplaner/bilder/abstand.py
    ruby src/scripts/render-svg.rb src/content/routenplaner/abstand.svg

Labels typeset by LaTeX (diagram_labels.math), x east, y north.
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import math as tex, right_angle  # noqa: E402

INK, BLUE, GREY, WHITE, AMBER = '#1f2937', '#2b5fb4', '#6b7280', '#ffffff', '#d97706'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
W, H = 520, 290

# the two points: A south-west, B north-east; C the corner of the right angle
A, B = (70, 230), (400, 60)
C = (B[0], A[1])

s = []
s.append(f'<line x1="{A[0]}" y1="{A[1]}" x2="{C[0]}" y2="{C[1]}" stroke="{AMBER}" stroke-width="2.6"/>')
s.append(f'<line x1="{C[0]}" y1="{C[1]}" x2="{B[0]}" y2="{B[1]}" stroke="{BLUE}" stroke-width="2.6"/>')
s.append(f'<line x1="{A[0]}" y1="{A[1]}" x2="{B[0]}" y2="{B[1]}" stroke="{INK}" stroke-width="3"/>')
s.append(right_angle(C[0], C[1], -1, 0, 0, -1, 16, GREY))
for p in (A, B):
    s.append(f'<circle cx="{p[0]}" cy="{p[1]}" r="6" fill="{INK}" stroke="{WHITE}" stroke-width="2"/>')
# labels: the legs and the hypotenuse
s.append(tex(r'\Delta\text{Länge} \cdot \cos(\text{Breite})', (A[0] + C[0]) / 2, A[1] + 32, 18, AMBER, 'middle'))
s.append(tex(r'\Delta\text{Breite}', C[0] + 14, (C[1] + B[1]) / 2 + 6, 18, BLUE, 'start'))
s.append(tex(r'd', (A[0] + B[0]) / 2 - 14, (A[1] + B[1]) / 2 - 10, 20, INK, 'end'))
s.append(f'<text x="{A[0] - 12}" y="{A[1] + 6}" font-size="17" fill="{INK}" text-anchor="end">A</text>')
s.append(f'<text x="{B[0] - 12}" y="{B[1] - 6}" font-size="17" fill="{INK}" text-anchor="end">B</text>')
# where north and east are: two small arrows in the free corner
O = (34, 96)
s.append(f'<defs><marker id="compass" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto">'
         f'<path d="M0,0 L10,5 L0,10 z" fill="{GREY}"/></marker></defs>')
s.append(f'<line x1="{O[0]}" y1="{O[1]}" x2="{O[0]}" y2="{O[1] - 52}" stroke="{GREY}" stroke-width="1.6" marker-end="url(#compass)"/>')
s.append(f'<line x1="{O[0]}" y1="{O[1]}" x2="{O[0] + 52}" y2="{O[1]}" stroke="{GREY}" stroke-width="1.6" marker-end="url(#compass)"/>')
s.append(f'<text x="{O[0]}" y="{O[1] - 60}" font-size="15" fill="{GREY}" text-anchor="middle">Norden</text>')
s.append(f'<text x="{O[0] + 58}" y="{O[1] + 5}" font-size="15" fill="{GREY}">Osten</text>')

out = os.path.join(HERE, '..', 'abstand.svg')
with open(out, 'w') as f:
    f.write(f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 {W} {H}" width="{W}" height="{H}" font-family="{FONT}">\n'
            '  <!-- Der Abstand zweier Punkte als Hypotenuse eines rechtwinkligen Dreiecks.\n'
            '       Gemacht von bilder/abstand.py; die dunkle Fassung abstand-dark.svg\n'
            '       und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n  '
            + '\n  '.join(s) + '\n</svg>\n')
print(out)
