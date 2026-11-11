"""The codec of the page as one flow (»Quantisieren nach Gehör«): samples,
blocks with window and MDCT, rounding with one step per band, counting the
bits, the file; from the MDCT a branch: the loudness of each band and the
masking threshold (allowed), which decides each band's step. Under each
box the function of the page that does it. Wide for a desktop (a row with
the branch under it), narrow for a phone (top to bottom, the branch beside).

    src/scripts/drawings python3 src/content/audio-compression/bilder/codec_flow.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/codec-flow.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/codec-flow-wide.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, WHITE, PURPLE = '#1f2937', '#6b7280', '#ffffff', '#7c3aed'


def box(cx, cy, w, h, lines, code, size, colour=INK):
    x0, y0 = cx - w / 2, cy - h / 2
    parts = [f"<rect x='{x0:.1f}' y='{y0:.1f}' width='{w}' height='{h}' rx='6' fill='{WHITE}' stroke='{colour}' stroke-width='1.6'/>"]
    first = cy - (len(lines) - 1) * (size + 3) / 2
    for i, line in enumerate(lines):
        parts.append(ap.text(cx, first + i * (size + 3) + size * 0.35, line, size, colour))
    if code:
        parts.append(f"<text x='{cx:.1f}' y='{y0 + h + size + 4:.1f}' text-anchor='middle' font-family=\"{ap.MONO}\" font-size='{size - 2}' fill='{GREY}'>{code}</text>")
    return parts


def arrow(x1, y1, x2, y2, colour=INK):
    dx, dy = x2 - x1, y2 - y1
    n = (dx * dx + dy * dy) ** 0.5
    ux, uy = dx / n, dy / n
    return [f"<line x1='{x1:.1f}' y1='{y1:.1f}' x2='{x2 - ux * 7:.1f}' y2='{y2 - uy * 7:.1f}' stroke='{colour}' stroke-width='1.8'/>",
            ap.arrowhead(x2, y2, ux, uy, colour, 9, 4.5)]


STEPS = [(['Samples'], None), (['Blöcke, Fenster,', 'MDCT'], 'encode'), (['runden, eine', 'Stufe je Band'], 'quantise'),
         (['Bits zählen'], 'entropy'), (['Datei'], None)]
BRANCH = [(['Lautstärke', 'je Band'], 'level'), (['Mithörschwelle'], 'allowed')]


def wide(name, size=15):
    w, h, gap = 172, 56, 30
    xs = [20 + w / 2 + i * (w + gap) for i in range(len(STEPS))]
    cy = 40
    parts = []
    for i, (lines, code) in enumerate(STEPS):
        parts += box(xs[i], cy, w, h, lines, code, size)
        if i:
            parts += arrow(xs[i - 1] + w / 2, cy, xs[i] - w / 2, cy)
    # the branch: under the MDCT and the rounding
    by = cy + h + 70
    bx = [xs[1], xs[2]]
    for (lines, code), x in zip(BRANCH, bx):
        parts += box(x, by, w, h, lines, code, size, PURPLE)
    parts += arrow(xs[1], cy + h / 2 + size + 8, xs[1], by - h / 2, PURPLE)
    parts += arrow(bx[0] + w / 2, by, bx[1] - w / 2, by, PURPLE)
    parts += arrow(xs[2], by - h / 2, xs[2], cy + h / 2 + size + 8, PURPLE)
    parts.append(ap.text(xs[2] + 10, (by - h / 2 + cy + h / 2 + size + 8) / 2 + 4, 'Stufe je Band', size - 2, PURPLE, 'start'))
    save(name, xs[-1] + w / 2 + 20, by + h / 2 + size + 16, parts)


def narrow(name, size=13):
    w, h, gap = 170, 48, 34
    cx = 20 + w / 2
    ys = [20 + h / 2 + i * (h + gap + size) for i in range(len(STEPS))]
    parts = []
    for i, (lines, code) in enumerate(STEPS):
        parts += box(cx, ys[i], w, h, lines, code, size)
        if i:
            parts += arrow(cx, ys[i - 1] + h / 2 + size + 6, cx, ys[i] - h / 2)
    bx = cx + w / 2 + 30 + 75
    # the branch beside the MDCT and the rounding
    y1, y2 = ys[1], ys[2]
    parts += box(bx, y1 + 10, 150, h, BRANCH[0][0], BRANCH[0][1], size, PURPLE)
    parts += box(bx, y2 + 10, 150, h, BRANCH[1][0], BRANCH[1][1], size, PURPLE)
    parts += arrow(cx + w / 2, y1, bx - 75, y1 + 10, PURPLE)
    parts += arrow(bx, y1 + 10 + h / 2 + size + 6, bx, y2 + 10 - h / 2, PURPLE)
    parts += arrow(bx - 75, y2 + 10, cx + w / 2, y2, PURPLE)
    save(name, bx + 75 + 12, ys[-1] + h / 2 + 14, parts)


def save(name, w, h, parts):
    head = ap.svg_head(round(w), round(h), 'Der Codec der Seite als Ablauf; gemacht von bilder/codec_flow.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/audio-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


wide('codec-flow-wide')
narrow('codec-flow')
