# The map of the Kranichsee in »Bilder im Text«: a picture inside the
# students' document, not on the page itself. Writes karte.svg and, with
# rsvg-convert, karte.png (1200 × 800, 3:2), which dokumente.py puts into the
# documents. Run in the drawings image:
#
#     src/scripts/drawings python3 src/content/pictures-in-text/bilder/karte.py
#
# The shore is one smooth closed curve through a few points; the path round
# the lake is the same curve pushed outward, so the two never cross.

import math
import os
import subprocess

HERE = os.path.dirname(os.path.abspath(__file__))
CX, CY = 590, 410

# the shore: (angle in degrees, distance from the centre); the bay in the
# west is the shallow water where the cranes sleep
SHORE = [(0, 300), (30, 250), (60, 230), (90, 245), (120, 270), (150, 330), (168, 400),
         (180, 410), (194, 380), (215, 290), (245, 260), (275, 255), (305, 265), (335, 290)]


def point(angle, r, grow=0.0):
    a = math.radians(angle)
    # the y axis points down: positive angles go clockwise, 90° is south
    return (CX + (r + grow) * math.cos(a), CY + (r + grow) * math.sin(a) * 0.72)


def smooth(points):
    """a closed Catmull-Rom curve through the points, as cubic Béziers"""
    n = len(points)
    d = f'M{points[0][0]:.1f} {points[0][1]:.1f}'
    for i in range(n):
        p0, p1, p2, p3 = points[i - 1], points[i], points[(i + 1) % n], points[(i + 2) % n]
        c1 = (p1[0] + (p2[0] - p0[0]) / 6, p1[1] + (p2[1] - p0[1]) / 6)
        c2 = (p2[0] - (p3[0] - p1[0]) / 6, p2[1] - (p3[1] - p1[1]) / 6)
        d += f'C{c1[0]:.1f} {c1[1]:.1f} {c2[0]:.1f} {c2[1]:.1f} {p2[0]:.1f} {p2[1]:.1f}'
    return d + 'Z'


def main():
    shore = [point(a, r) for a, r in SHORE]
    path = [point(a, r, 46) for a, r in SHORE]
    tower = point(270, 255, 46)
    hostel = point(90, 245, 46)
    bay = point(180, 330)
    moor = point(5, 300, 150)
    # the Moorpfad leaves the path north of the marsh and comes back south of it
    start = point(-22, 293, 46)
    end = point(28, 252, 46)
    svg = f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1200 800" width="1200" height="800" font-family="PT Sans, sans-serif">
  <!-- made by karte.py -->
  <defs>
    <pattern id="marsh" width="34" height="22" patternUnits="userSpaceOnUse">
      <path d="M6 16h14M9 16v-6M13 16v-8M17 16v-5" stroke="#6f8f5a" stroke-width="2" fill="none" stroke-linecap="round"/>
    </pattern>
    <pattern id="shallow" width="18" height="10" patternUnits="userSpaceOnUse">
      <path d="M2 6q3-3 6 0t6 0" stroke="#6fa6cc" stroke-width="1.6" fill="none"/>
    </pattern>
  </defs>
  <rect width="1200" height="800" fill="#f1f3e6"/>
  <g fill="#c4dba6" stroke="#a5c385" stroke-width="2">
    <path d="M0 0h360c-20 60-90 80-150 90-70 12-120 50-160 100-20 26-40 40-50 42z"/>
    <path d="M1200 0v240c-60-20-100-60-130-110-24-40-60-70-120-86-30-8-50-24-60-44z"/>
    <path d="M0 650c70-26 150-16 200 26 34 30 50 70 56 124H0z"/>
  </g>
  <g fill="#a9c98a">
    <circle cx="60" cy="50" r="15"/><circle cx="110" cy="90" r="13"/><circle cx="180" cy="40" r="14"/><circle cx="250" cy="34" r="12"/><circle cx="46" cy="130" r="12"/>
    <circle cx="1130" cy="50" r="15"/><circle cx="1070" cy="96" r="13"/><circle cx="1160" cy="150" r="14"/><circle cx="1000" cy="36" r="12"/>
    <circle cx="50" cy="720" r="14"/><circle cx="120" cy="700" r="12"/><circle cx="170" cy="756" r="13"/>
  </g>
  <ellipse cx="{moor[0]:.0f}" cy="{moor[1]:.0f}" rx="120" ry="150" fill="#dfe8cf"/>
  <ellipse cx="{moor[0]:.0f}" cy="{moor[1]:.0f}" rx="120" ry="150" fill="url(#marsh)"/>
  <path d="{smooth(shore)}" fill="#a8cfe8" stroke="#5f93bd" stroke-width="4"/>
  <ellipse cx="{bay[0]:.0f}" cy="{bay[1]:.0f}" rx="70" ry="44" fill="url(#shallow)"/>
  <path d="{smooth(path)}" fill="none" stroke="#9a7a52" stroke-width="5" stroke-dasharray="3 10" stroke-linecap="round"/>
  <path d="M{start[0]:.0f} {start[1]:.0f}C{moor[0] + 90:.0f} {moor[1] - 140:.0f} {moor[0] + 90:.0f} {moor[1] + 140:.0f} {end[0]:.0f} {end[1]:.0f}" fill="none" stroke="#7a5a32" stroke-width="7" stroke-dasharray="14 7"/>
  <g transform="translate({hostel[0]:.0f} {hostel[1] + 40:.0f})">
    <path d="M-34 0v-36l34-26 34 26v36z" fill="#d9734e" stroke="#8f4a31" stroke-width="3" stroke-linejoin="round"/>
    <rect x="-9" y="-20" width="18" height="20" fill="#f1f3e6"/>
  </g>
  <g transform="translate({tower[0]:.0f} {tower[1] - 4:.0f})">
    <path d="M-16 0l8-62h16l8 62z" fill="#8a6a43" stroke="#5e472b" stroke-width="3" stroke-linejoin="round"/>
    <rect x="-20" y="-76" width="40" height="16" fill="#8a6a43" stroke="#5e472b" stroke-width="3"/>
  </g>
  <g fill="#2f3b2a" font-size="30" font-weight="bold">
    <text x="{hostel[0] + 48:.0f}" y="{hostel[1] + 30:.0f}">Jugendherberge</text>
    <text x="{tower[0] + 34:.0f}" y="{tower[1] - 30:.0f}">Aussichtsturm</text>
    <text x="{moor[0] + 10:.0f}" y="{moor[1] + 190:.0f}" text-anchor="middle">Moorpfad</text>
  </g>
  <g fill="#2f6690" font-size="22" text-anchor="middle">
    <text x="{bay[0] + 10:.0f}" y="{bay[1] + 70:.0f}">Schlafplatz</text>
    <text x="{bay[0] + 10:.0f}" y="{bay[1] + 96:.0f}">der Kraniche</text>
  </g>
  <text x="{CX + 30}" y="{CY + 10}" text-anchor="middle" font-size="46" font-style="italic" fill="#2f6690" letter-spacing="2">Kranichsee</text>
  <g transform="translate(1130 700)">
    <path d="M0-50l16 40-16-10-16 10z" fill="#2f3b2a"/>
    <text y="18" text-anchor="middle" font-size="26" font-weight="bold" fill="#2f3b2a">N</text>
  </g>
  <g transform="translate(860 760)" fill="#2f3b2a">
    <rect width="80" height="10"/>
    <rect x="80" width="80" height="10" fill="#f1f3e6" stroke="#2f3b2a" stroke-width="2"/>
    <text x="0" y="-8" font-size="20" text-anchor="middle">0</text>
    <text x="160" y="-8" font-size="20" text-anchor="middle">500 m</text>
  </g>
</svg>
'''
    with open(os.path.join(HERE, 'karte.svg'), 'w') as f:
        f.write(svg)
    subprocess.run(['rsvg-convert', '-w', '1200', '-h', '800', os.path.join(HERE, 'karte.svg'),
                    '-o', os.path.join(HERE, 'karte.png')], check=True)


if __name__ == '__main__':
    main()
