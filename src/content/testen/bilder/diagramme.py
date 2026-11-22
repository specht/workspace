"""The drawings of »Testen«:

- klassen: the page's example for classes and boundaries, a function that
  says whether someone is of age. The ages as a number line, the two
  classes (under 18, from 18) in their colours, a representative of each
  and the boundary with its two sides, 17 and 18. Not the points of grade:
  their boundaries are a task.
- tdd: test-driven development as a cycle: red, green, refactor, and again.

    python3 src/content/testen/bilder/diagramme.py
    for f in klassen tdd; do ruby src/scripts/render-svg.rb src/content/testen/$f.svg; done
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import diagram_labels  # noqa: E402,F401  (runs the script in docker/drawings)

INK, GREY, BLUE, AMBER, RED, GREEN, WHITE = '#1f2937', '#6b7280', '#2b5fb4', '#d97706', '#dc2626', '#16a34a', '#ffffff'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'


def text(x, y, t, size=17, color=INK, anchor='middle', weight=400):
    return f'<text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" text-anchor="{anchor}" font-weight="{weight}">{t}</text>'


def save(name, w, h, comment, parts, defs=''):
    with open(os.path.join(HERE, '..', name + '.svg'), 'w') as f:
        f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {w} {h}" width="{w}" height="{h}" font-family="{FONT}">\n'
                f'  <!-- {comment}.\n'
                f'       Gemacht von bilder/diagramme.py; die dunkle Fassung {name}-dark.svg\n'
                '       und die Schriften: src/scripts/render-svg.rb -->\n'
                f'  <defs>{defs}</defs>\n'
                f'  <rect width="{w}" height="{h}" fill="{WHITE}"/>\n  ' + '\n  '.join(parts) + '\n</svg>\n')
    print(name)


def klassen():
    w, h = 660, 180
    x0, per = 40, 18          # age 0 at x0, pixels per year
    axis = 82

    def x(age):
        return x0 + age * per
    parts = []
    # the two classes as bands over the line
    parts.append(f'<rect x="{x(0)}" y="{axis - 44}" width="{x(17.5) - x(0)}" height="30" rx="6" fill="{BLUE}" fill-opacity="0.14"/>')
    parts.append(f'<rect x="{x(17.5)}" y="{axis - 44}" width="{x(32) - x(17.5)}" height="30" rx="6" fill="{AMBER}" fill-opacity="0.16"/>')
    parts.append(text((x(0) + x(17.5)) / 2, axis - 23, 'unter 18: nicht volljährig', 16, BLUE, weight=600))
    parts.append(text((x(17.5) + x(32)) / 2, axis - 23, 'ab 18: volljährig', 16, AMBER, weight=600))
    # the number line
    parts.append(f'<line x1="{x(0)}" y1="{axis}" x2="{x(32) + 8}" y2="{axis}" stroke="{GREY}" stroke-width="1.6" marker-end="url(#head)"/>')
    for age in range(0, 33, 1):
        tick = 7 if age % 5 == 0 else 4
        parts.append(f'<line x1="{x(age)}" y1="{axis - tick}" x2="{x(age)}" y2="{axis + tick}" stroke="{GREY}" stroke-width="1"/>')
    for age in (0, 5, 10, 25, 30):
        parts.append(text(x(age), axis + 26, age, 14, GREY))
    # a representative of each class, and the boundary's two sides
    for age, color, what in [(9, BLUE, 'ein Vertreter'), (25, AMBER, 'ein Vertreter')]:
        parts.append(f'<circle cx="{x(age)}" cy="{axis}" r="6" fill="{color}"/>')
        parts.append(text(x(age), axis + 50, what, 14, color))
    for age, color, anchor, dx in [(17, BLUE, 'end', 6), (18, AMBER, 'start', -6)]:
        parts.append(f'<circle cx="{x(age)}" cy="{axis}" r="7" fill="{color}" stroke="{WHITE}" stroke-width="2"/>')
        parts.append(text(x(age) + dx, axis + 26, age, 15, color, anchor, weight=700))
    parts.append(f'<line x1="{x(17.5)}" y1="{axis - 54}" x2="{x(17.5)}" y2="{axis + 34}" stroke="{INK}" stroke-width="1.6" stroke-dasharray="5 4"/>')
    parts.append(text(x(17.5), axis + 74, 'die Grenze: beide Seiten testen', 15, INK, weight=600))
    parts.append(text(x(32) + 10, axis + 26, 'Alter', 14, GREY, 'end'))
    head = (f'<marker id="head" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto">'
            f'<path d="M0,0 L10,5 L0,10 z" fill="{GREY}"/></marker>')
    save('klassen', w, h, 'Die Klassen der Volljährigkeit und ihre Grenze', parts, head)


def tdd():
    w, h = 660, 340
    cx, cy, r = 330, 196, 112
    # name, what it means, colour, angle on the circle, where the text goes
    steps = [('Rot', 'ein Test, der scheitert', RED, -90, 'above'),
             ('Grün', 'gerade genug Code', GREEN, 30, 'right'),
             ('Aufräumen', 'lesbarer, Tests grün', BLUE, 150, 'left')]
    parts = []
    for name, what, color, angle, side in steps:
        a = math.radians(angle)
        nx, ny = cx + r * math.cos(a), cy + r * math.sin(a)
        # the arrow to the next step, along the circle
        a0, a1 = math.radians(angle + 30), math.radians(angle + 120 - 30)
        x1, y1 = cx + r * math.cos(a0), cy + r * math.sin(a0)
        x2, y2 = cx + r * math.cos(a1), cy + r * math.sin(a1)
        parts.append(f'<path d="M{x1:.1f},{y1:.1f} A{r},{r} 0 0 1 {x2:.1f},{y2:.1f}" fill="none" stroke="{GREY}" stroke-width="2" marker-end="url(#head)"/>')
        parts.append(f'<circle cx="{nx:.1f}" cy="{ny:.1f}" r="48" fill="{color}" fill-opacity="0.13" stroke="{color}" stroke-width="2.6"/>')
        parts.append(text(nx, ny + 6, name, 17 if len(name) < 6 else 15, color, weight=700))
        if side == 'above':
            parts.append(text(nx, ny - 60, what, 15, INK))
        elif side == 'right':
            parts.append(text(nx + 60, ny + 5, what, 15, INK, 'start'))
        else:
            parts.append(text(nx - 60, ny + 5, what, 15, INK, 'end'))
    head = (f'<marker id="head" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto">'
            f'<path d="M0,0 L10,5 L0,10 z" fill="{GREY}"/></marker>')
    save('tdd', w, h, 'Rot, grün, aufräumen als Kreislauf', parts, head)


klassen()
tdd()
