"""Why the shift of a tone does not change its strength (»Mit einem Testton
vergleichen«): the same tone three times, shifted by 0, an eighth and a
quarter of an oscillation against the cosine test tone, each in its own
colour; beside it, for each, the pair (c, s) as a point: they all lie on
one circle, whose radius is the strength. A tone A·cos(2πkn/N − φ) gives
c = A·N/2·cos φ and s = A·N/2·sin φ, and c² + s² stays the same. Wide for
a desktop (waves left, circle right), narrow for a phone (circle under
the waves).

    src/scripts/drawings python3 src/content/audio-compression/bilder/phase.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/phase.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/phase-wide.svg
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, BLUE, PURPLE, AMBER = '#1f2937', '#6b7280', '#d1d5db', '#2b5fb4', '#7c3aed', '#d97706'
SHIFTS = [(0, 'nicht verschoben', BLUE), (math.pi / 4, 'um ⅛ Schwingung', PURPLE), (math.pi / 2, 'um ¼ Schwingung', AMBER)]


def waves(parts, x0, y0, w, size):
    h, gap = 46, 28
    for i, (phi, label, colour) in enumerate(SHIFTS):
        top = y0 + i * (h + gap)
        mid = top + size + 6 + h / 2
        parts.append(ap.text(x0, top + size, label, size, colour, 'start'))
        parts.append(f"<line x1='{x0}' y1='{mid}' x2='{x0 + w}' y2='{mid}' stroke='{LINE}' stroke-width='1'/>")
        pts = ' '.join(f'{x0 + w * t / 200:.1f},{mid - h / 2 * math.cos(2 * math.pi * 2 * t / 200 - phi):.1f}' for t in range(201))
        parts.append(f"<polyline points='{pts}' fill='none' stroke='{colour}' stroke-width='2.2'/>")
    return y0 + 3 * (h + gap) - gap + size + 6


def circle(parts, cx, cy, r, size):
    parts.append(f"<line x1='{cx - 12}' y1='{cy}' x2='{cx + r + 22}' y2='{cy}' stroke='{GREY}' stroke-width='1.4'/>")
    parts.append(f"<line x1='{cx}' y1='{cy + 12}' x2='{cx}' y2='{cy - r - 22}' stroke='{GREY}' stroke-width='1.4'/>")
    parts.append(ap.arrowhead(cx + r + 22, cy, 1, 0, GREY, 8, 4))
    parts.append(ap.arrowhead(cx, cy - r - 22, 0, -1, GREY, 8, 4))
    parts.append(ap.text(cx + r + 26, cy + size * 0.35, 'c', size + 2, INK, 'start'))
    parts.append(ap.text(cx + 8, cy - r - 18, 's', size + 2, INK, 'start'))
    # the quarter of the circle the points lie on
    parts.append(f"<path d='M {cx + r} {cy} A {r} {r} 0 0 0 {cx} {cy - r}' fill='none' stroke='{INK}' stroke-width='1.5' stroke-dasharray='5 4'/>")
    for phi, _, colour in SHIFTS:
        px, py = cx + r * math.cos(phi), cy - r * math.sin(phi)
        parts.append(f"<line x1='{cx}' y1='{cy}' x2='{px:.1f}' y2='{py:.1f}' stroke='{colour}' stroke-width='2'/>")
        parts.append(f"<circle cx='{px:.1f}' cy='{py:.1f}' r='6' fill='{colour}'/>")
    parts.append(ap.text(cx + r * 0.42, cy + size + 10, 'immer gleich lang', size, INK))


def picture(name, wide):
    size = 15 if wide else 13
    parts = []
    if wide:
        bottom = waves(parts, 20, 10, 380, size)
        r = 110
        circle(parts, 470, 40 + r, r, size)
        width, height = 470 + r + 60, max(bottom, 40 + r + size + 24)
    else:
        bottom = waves(parts, 20, 10, 400, size)
        r = 90
        circle(parts, 40, bottom + 30 + r, r, size)
        width, height = 440, bottom + 30 + r + size + 24
    head = ap.svg_head(round(width), round(height), 'Ein Ton, verschieden weit verschoben: seine beiden Summen c und s liegen auf einem Kreis; gemacht von bilder/phase.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/audio-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


picture('phase-wide', True)
picture('phase', False)
