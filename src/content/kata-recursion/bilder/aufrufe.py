"""The calls of halvings(20) (»Rekursion«, the page's example) as a
staircase: each call waits for the next one below it, halvings(1) returns 0
at once, and on the way back each adds its 1: 1, 2, 3, 4.

    python3 src/content/kata-recursion/bilder/aufrufe.py
    ruby src/scripts/render-svg.rb src/content/kata-recursion/aufrufe.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import code  # noqa: E402

INK, GREY, BLUE, AMBER, WHITE, PALE = '#1f2937', '#6b7280', '#2b5fb4', '#d97706', '#ffffff', '#eef3fb'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'

calls = [20, 10, 5, 2, 1]
STEP_X, STEP_Y, BOX_W, BOX_H = 52, 58, 168, 38
X0, Y0 = 24, 30
W = X0 + STEP_X * (len(calls) - 1) + BOX_W + 290
H = Y0 + STEP_Y * len(calls) + 4

s = []


def text(x, y, t, size=15, color=INK, anchor='start', weight=400):
    s.append(f'<text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" text-anchor="{anchor}" font-weight="{weight}">{t}</text>')


def arrow(x1, y1, x2, y2, color):
    s.append(f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" stroke="{color}" stroke-width="2" marker-end="url(#head-{color[1:]})"/>')


results = []
value = 0
for k in range(len(calls) - 1, -1, -1):
    value = 0 if k == len(calls) - 1 else value + 1
    results.insert(0, value)

for k, n in enumerate(calls):
    x, y = X0 + k * STEP_X, Y0 + k * STEP_Y
    s.append(f'<rect x="{x}" y="{y}" width="{BOX_W}" height="{BOX_H}" rx="7" fill="{PALE}" stroke="{BLUE}" stroke-width="1.8"/>')
    s.append(code(f'halvings({n})', x + BOX_W / 2, y + 25, size=16, anchor='middle', background=False))
    if k + 1 < len(calls):
        # down: it calls the next one and waits
        arrow(x + 26, y + BOX_H, x + 26 + STEP_X - 4, y + STEP_Y - 2, BLUE)
    # back: what it returns
    rx = X0 + (len(calls) - 1) * STEP_X + BOX_W + 26
    if k == len(calls) - 1:
        what = 'gibt sofort 0 zurück'
    else:
        what = f'gibt 1 + {results[k + 1]} = {results[k]} zurück'
    s.append(f'<line x1="{x + BOX_W + 6}" y1="{y + BOX_H / 2}" x2="{rx - 10}" y2="{y + BOX_H / 2}" stroke="{AMBER}" stroke-width="1.4" stroke-dasharray="4 4"/>')
    text(rx, y + BOX_H / 2 + 5, what, 16, AMBER, weight=600)
# the way back up, beside the answers
ax = W - 18
arrow(ax, Y0 + (len(calls) - 1) * STEP_Y + BOX_H / 2, ax, Y0 + BOX_H / 2 + 6, AMBER)
text(ax - 8, Y0 + (len(calls) - 1) * STEP_Y + BOX_H / 2 + 26, 'von innen nach außen', 14, GREY, 'end')

heads = ''.join(f'<marker id="head-{c[1:]}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto">'
                f'<path d="M0,0 L10,5 L0,10 z" fill="{c}"/></marker>' for c in (BLUE, AMBER))
out = os.path.join(HERE, '..', 'aufrufe.svg')
H += 26
with open(out, 'w') as f:
    f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" font-family="{FONT}">\n'
            '  <!-- Die Aufrufe von halvings(20) als Treppe.\n'
            '       Gemacht von bilder/aufrufe.py; die dunkle Fassung aufrufe-dark.svg\n'
            '       und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <defs>{heads}</defs>\n'
            f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n  ' + '\n  '.join(s) + '\n</svg>\n')
print(out)
