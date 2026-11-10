"""The three ways of predicting a block of 4 × 4 from its neighbours, as in
blocks.py: vertical (each column repeats the pixel above it), horizontal
(each row repeats the pixel to its left) and dc (every pixel is the mean of
the eight neighbours). The neighbours are numbers in white cells, the
predicted pixels numbers in cells tinted the stronger the larger they are,
arrows where the values come from. Wide for a desktop (side by side),
narrow for a phone (one under the other).

    src/scripts/drawings python3 src/content/video-compression/bilder/intra.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/intra.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/intra-wide.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, PURPLE, WHITE = '#1f2937', '#6b7280', '#d1d5db', '#7c3aed', '#ffffff'
ABOVE = [40, 80, 160, 200]
LEFT = [200, 160, 100, 60]
DC = (sum(ABOVE) + sum(LEFT)) // 8          # 125
MODES = [
    ('vertical', lambda r, c: ABOVE[c]),
    ('horizontal', lambda r, c: LEFT[r]),
    ('dc', lambda r, c: DC),
]


def cell(x, y, w, h, value, tinted):
    parts = [f"<rect x='{x}' y='{y}' width='{w}' height='{h}' fill='{WHITE}' stroke='{LINE}' stroke-width='1.5'/>"]
    if tinted:
        parts.append(f"<rect x='{x}' y='{y}' width='{w}' height='{h}' fill='{PURPLE}' fill-opacity='{0.08 + 0.5 * value / 255:.2f}'/>")
    parts.append(ap.text(x + (w - 12) / 2 if tinted else x + w / 2, y + h / 2 + 6, value, 17, INK))
    return parts


def panel(x0, y0, w, h, mode):
    """w and h: the size of a cell; the predicted numbers stand a little to
    the left, the arrows of vertical run down the right of their column"""
    name, value = mode
    parts = [ap.text(x0 + 2.5 * w, y0 - 14, name, 20, INK)]
    for c in range(4):
        parts += cell(x0 + (c + 1) * w, y0, w, h, ABOVE[c], False)
    for r in range(4):
        parts += cell(x0, y0 + (r + 1) * h, w, h, LEFT[r], False)
    for r in range(4):
        for c in range(4):
            parts += cell(x0 + (c + 1) * w, y0 + (r + 1) * h, w, h, value(r, c), True)
    if name == 'vertical':
        for c in range(4):
            x = x0 + (c + 2) * w - 9
            parts.append(f"<line x1='{x:.1f}' y1='{y0 + h - 4:.1f}' x2='{x:.1f}' y2='{y0 + 5 * h - 12:.1f}' stroke='{GREY}' stroke-width='1.6'/>")
            parts.append(ap.arrowhead(x, y0 + 5 * h - 6, 0, 1, GREY))
    elif name == 'horizontal':
        for r in range(4):
            y = y0 + (r + 2) * h - 8
            parts.append(f"<line x1='{x0 + w - 4:.1f}' y1='{y:.1f}' x2='{x0 + 5 * w - 12:.1f}' y2='{y:.1f}' stroke='{GREY}' stroke-width='1.6'/>")
            parts.append(ap.arrowhead(x0 + 5 * w - 6, y, 1, 0, GREY))
    else:
        parts.append(ap.text(x0 + 5 * w, y0 + 5 * h + 28, f'Mittelwert der 8 Nachbarn: {DC}', 15, GREY, 'end'))
    return parts


def wide():
    W, w, h = 960, 56, 44
    parts = []
    for k, mode in enumerate(MODES):
        parts += panel(20 + k * 320, 44, w, h, mode)
    save('intra-wide', W, 44 + 5 * h + 44, parts)


def narrow():
    W, w, h = 440, 60, 46
    parts = []
    for k, mode in enumerate(MODES):
        parts += panel((W - 5 * w) // 2, 44 + k * (5 * h + 84), w, h, mode)
    save('intra', W, 44 + 3 * (5 * h + 84) - 40, parts)


def save(name, w, h, parts):
    head = ap.svg_head(w, h, 'Vorhersage eines Blocks aus seinen Nachbarn: vertical, horizontal, dc; gemacht von bilder/intra.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/video-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


wide()
narrow()
