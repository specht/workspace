"""The drawings of »Einen Weg suchen« (Routenplaner):

- losgehen: walk on a small graph of one-way edges, 1 to 2 and 3, 2 to 4,
  3 to 5, looking for 5. On the left the graph, on the right the calls as a
  staircase: walk(2, 5) finds nothing, because 4 has no neighbours; walk(3, 5)
  reaches the goal, and each call on the way back puts its node in front.
  Not the graph of the task's check: that one is made up separately.
- kreis: walk on data/detour.tsv from 11 to 16. The first neighbour of 11 is
  12, and the first neighbour of 12 is 11 again: the calls go 11, 12, 11, 12
  until the stack is full. After the prediction (»kreis«), which it answers.
- ringe: the wave on data/detour.tsv from 11: every node in the ring of its
  number of edges from the start, 16 in ring 3 over the Eichenweg.

    src/scripts/drawings python3 src/content/routenplaner/bilder/suche.py
    for f in losgehen kreis ringe; do src/scripts/drawings ruby src/scripts/render-svg.rb src/content/routenplaner/$f.svg; done
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import code  # noqa: E402

INK, GREY, LINE, BLUE, AMBER, WHITE, PALE = '#1f2937', '#6b7280', '#d1d5db', '#2b5fb4', '#d97706', '#ffffff', '#eef3fb'
GREEN, PURPLE, RED = '#16a34a', '#7c3aed', '#dc2626'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'


def text(x, y, t, size=16, color=INK, anchor='middle', weight=400):
    return f'<text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" text-anchor="{anchor}" font-weight="{weight}">{t}</text>'


def save(name, w, h, comment, parts):
    with open(os.path.join(HERE, '..', name + '.svg'), 'w') as f:
        f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {w} {h}" width="{w}" height="{h}" font-family="{FONT}">\n'
                f'  <!-- {comment}.\n'
                f'       Gemacht von bilder/suche.py; die dunkle Fassung {name}-dark.svg\n'
                '       und die Schriften: src/scripts/render-svg.rb -->\n'
                f'  <rect width="{w}" height="{h}" fill="{WHITE}"/>\n  ' + '\n  '.join(parts) + '\n</svg>\n')
    print(name)


def node(parts, x, y, label, fill=PALE, stroke=BLUE, r=17, color=INK, width=1.8):
    # a highlighted node: amber, see-through, so that it works in both modes
    opacity = ' fill-opacity="0.22"' if fill == AMBER else ''
    parts.append(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{r}" fill="{fill}"{opacity} stroke="{stroke}" stroke-width="{width}"/>')
    parts.append(text(x, y + 6, label, 16, color, weight=600))


def edge(parts, a, b, r=17, color=GREY, width=1.8, head=True, gap=4):
    """A straight edge from the border of a to the border of b, with a
    filled head whose tip touches b's border."""
    (x1, y1), (x2, y2) = a, b
    d = math.hypot(x2 - x1, y2 - y1)
    ux, uy = (x2 - x1) / d, (y2 - y1) / d
    sx, sy = x1 + ux * (r + gap), y1 + uy * (r + gap)
    tx, ty = x2 - ux * (r + 2), y2 - uy * (r + 2)
    if head:
        bx, by = tx - ux * 11, ty - uy * 11
        parts.append(f'<line x1="{sx:.1f}" y1="{sy:.1f}" x2="{bx:.1f}" y2="{by:.1f}" stroke="{color}" stroke-width="{width}"/>')
        px, py = -uy * 6, ux * 6
        parts.append(f'<path d="M{tx:.1f},{ty:.1f} L{bx + px:.1f},{by + py:.1f} L{bx - px:.1f},{by - py:.1f} z" fill="{color}"/>')
    else:
        parts.append(f'<line x1="{sx:.1f}" y1="{sy:.1f}" x2="{tx:.1f}" y2="{ty:.1f}" stroke="{color}" stroke-width="{width}"/>')


def staircase(parts, x0, y0, rows, box_w=150, step_x=34, step_y=50, answer_x=None):
    """Calls as a staircase: rows of (depth, call, answer, colour of the
    answer). A line from each call down to the calls it makes."""
    last_at_depth = {}
    for i, (depth, call, answer, color) in enumerate(rows):
        x, y = x0 + depth * step_x, y0 + i * step_y
        if depth > 0:
            px, py = last_at_depth[depth - 1]
            # down from the caller, then right to this call
            parts.append(f'<path d="M{px + 14},{py + 34} V{y + 17} H{x - 2}" fill="none" stroke="{BLUE}" stroke-width="1.6"/>')
        parts.append(f'<rect x="{x}" y="{y}" width="{box_w}" height="34" rx="7" fill="{PALE}" stroke="{BLUE}" stroke-width="1.6"/>')
        parts.append(code(call, x + box_w / 2, y + 23, size=16, anchor='middle', background=False))
        last_at_depth[depth] = (x, y)
        if answer:
            ax = answer_x if answer_x is not None else x + box_w + 16
            parts.append(f'<line x1="{x + box_w + 6}" y1="{y + 17}" x2="{ax - 10}" y2="{y + 17}" stroke="{color}" stroke-width="1.4" stroke-dasharray="4 4"/>')
            parts.append(text(ax, y + 23, answer, 16, color, 'start', weight=600))


def losgehen():
    w, h = 770, 290
    parts = []
    at = {'1': (50, 150), '2': (140, 80), '3': (140, 220), '4': (230, 80), '5': (230, 220)}
    for a, b in [('1', '2'), ('1', '3'), ('2', '4'), ('3', '5')]:
        edge(parts, at[a], at[b])
    for name, (x, y) in at.items():
        goal = name == '5'
        node(parts, x, y, name, AMBER if goal else PALE, AMBER if goal else BLUE, width=2.6 if goal else 1.8)
    parts.append(text(230, 268, 'das Ziel', 14, AMBER, weight=600))
    parts.append(text(230, 124, 'keine Nachbarn', 14, GREY))
    rows = [(0, 'walk(1, 5)', 'gibt [1, 3, 5] zurück', AMBER),
            (1, 'walk(2, 5)', 'findet keinen Weg', GREY),
            (2, 'walk(4, 5)', 'findet keinen Weg', GREY),
            (1, 'walk(3, 5)', 'gibt [3, 5] zurück', AMBER),
            (2, 'walk(5, 5)', 'am Ziel: gibt [5] zurück', AMBER)]
    staircase(parts, 300, 20, rows, box_w=128, answer_x=548)
    save('losgehen', w, h, 'walk sucht auf einem kleinen Graphen von 1 nach 5', parts)


def detour_positions():
    # data/detour.tsv as on a map: the Lindenstraße below from 11 to 16, the
    # Eichenweg from 11 up, across and down to 16
    return {'11': (40, 190), '12': (120, 190), '13': (200, 190), '14': (280, 190), '15': (360, 190), '16': (440, 190),
            '21': (40, 60), '22': (440, 60)}


DETOUR_EDGES = [('11', '12'), ('12', '13'), ('13', '14'), ('14', '15'), ('15', '16'), ('11', '21'), ('21', '22'), ('22', '16')]


def kreis():
    w, h = 760, 340
    parts = []
    at = detour_positions()
    for a, b in DETOUR_EDGES:
        hot = {a, b} == {'11', '12'}
        edge(parts, at[a], at[b], color=AMBER if hot else LINE, width=3 if hot else 1.8, head=False)
    for name, (x, y) in at.items():
        goal = name == '16'
        hot = name in ('11', '12')
        node(parts, x, y, name, AMBER if hot else PALE, AMBER if hot or goal else BLUE, width=2.6 if hot or goal else 1.8)
    parts.append(text(240, 230, 'Lindenstraße', 14, GREY))
    parts.append(text(240, 50, 'Eichenweg', 14, GREY))
    parts.append(text(440, 236, 'das Ziel', 14, AMBER, weight=600))
    # the first neighbour of 11 is 12, the first neighbour of 12 is 11
    parts.append(text(24, 272, 'erster Nachbar von 11: 12', 14, INK, 'start'))
    parts.append(text(24, 294, 'erster Nachbar von 12: 11', 14, INK, 'start'))
    rows = [(0, 'walk(11, 16)', '', INK), (1, 'walk(12, 16)', '', INK), (2, 'walk(11, 16)', '', INK),
            (3, 'walk(12, 16)', '', INK), (4, 'walk(11, 16)', '', INK)]
    x0 = 500
    staircase(parts, x0, 16, rows, box_w=138, step_x=22, step_y=46)
    y = 16 + 5 * 46
    parts.append(text(x0 + 5 * 22 + 40, y + 10, '…', 22, GREY))
    parts.append(text(x0 + 116, y + 46, 'bis der Stack voll ist:', 15, INK))
    parts.append(text(x0 + 116, y + 70, 'Stack Overflow', 17, RED, weight=700))
    save('kreis', w, h, 'walk geht auf data/detour.tsv zwischen 11 und 12 hin und her', parts)


def ringe():
    w, h = 760, 270
    parts = []
    at = detour_positions()
    ring = {'11': 0, '12': 1, '21': 1, '13': 2, '22': 2, '14': 3, '16': 3, '15': 4}
    colors = [INK, BLUE, GREEN, PURPLE, GREY]
    for a, b in DETOUR_EDGES:
        edge(parts, at[a], at[b], color=LINE, head=False)
    for name, (x, y) in at.items():
        k = ring[name]
        later = k == 4
        node(parts, x, y, name, WHITE if later else PALE, colors[k], color=GREY if later else INK, width=1.4 if later else 2.4)
        below = y + 38 if y > 100 else y - 26
        parts.append(text(x, below, f'Ring {k}', 13, colors[k], weight=600))
    parts.append(f'<circle cx="440" cy="190" r="23" fill="none" stroke="{AMBER}" stroke-width="2.4"/>')
    parts.append(text(440, 252, 'das Ziel', 14, AMBER, weight=600))
    # the rings as a list
    x0, y0 = 530, 40
    rows = [('Ring 0', '11', 'der Start'), ('Ring 1', '12, 21', 'seine Nachbarn'), ('Ring 2', '13, 22', 'deren neue Nachbarn'),
            ('Ring 3', '14, 16', '16 ist das Ziel')]
    for i, (name, nodes, what) in enumerate(rows):
        y = y0 + i * 46
        parts.append(text(x0, y, name, 15, colors[i], 'start', weight=700))
        parts.append(text(x0 + 66, y, nodes, 15, INK, 'start', weight=600))
        parts.append(text(x0 + 66, y + 19, what, 13, GREY, 'start'))
    parts.append(text(x0, y0 + 4 * 46, '15 käme erst in Ring 4 dran.', 13, GREY, 'start'))
    save('ringe', w, h, 'Die Welle auf data/detour.tsv, Ring für Ring', parts)


losgehen()
kreis()
ringe()
