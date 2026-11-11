"""The three common forms of Chroma Subsampling on a strip of 4 × 2 pixels,
the strip the names count on: in every pixel a small square for its Y, and
for each value of Cb and Cr a dot, half blue and half red, in the middle of
a tinted area over the pixels that share it. Beside each row the number of
new colour values in it, so that the name reads off the picture: 4 pixels
wide, a values in the first row, b new ones in the second. Under each form
the values for the eight pixels and the bytes per pixel. Wide for a desktop
(the three side by side), narrow for a phone (one under the other).

    src/scripts/drawings python3 src/content/video-compression/bilder/subsampling.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/subsampling.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/subsampling-wide.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#7c3aed'
TINTS = ['#ede9fe', '#ddd6fe']
BLUE, RED = '#2b5fb4', '#dc2626'
# name, the areas sharing one Cb and Cr (column, row, columns, rows), new values per row, values, bytes per pixel
FORMS = [
    ('4:4:4', [(c, r, 1, 1) for r in range(2) for c in range(4)], [4, 4], '8 Y, 8 Cb, 8 Cr', '3 Bytes je Pixel'),
    ('4:2:2', [(c, r, 2, 1) for r in range(2) for c in (0, 2)], [2, 2], '8 Y, 4 Cb, 4 Cr', '2 Bytes je Pixel'),
    ('4:2:0', [(c, 0, 2, 2) for c in (0, 2)], [2, 0], '8 Y, 2 Cb, 2 Cr', '1,5 Bytes je Pixel'),
]


def strip(x, y, cell, form):
    """the pixels of one form with their top left corner at x, y"""
    name, areas, new, _, _ = form
    parts = []
    for r in range(2):
        for c in range(4):
            parts.append(f"<rect x='{x + c * cell}' y='{y + r * cell}' width='{cell}' height='{cell}' fill='#ffffff' stroke='{LINE}' stroke-width='1.5'/>")
    inset = 5
    for k, (c, r, w, h) in enumerate(areas):
        tint = TINTS[(c // w + r) % 2]
        parts.append(f"<rect x='{x + c * cell + inset}' y='{y + r * cell + inset}' width='{w * cell - 2 * inset}' height='{h * cell - 2 * inset}' "
                     f"rx='7' fill='{tint}' stroke='{PURPLE}' stroke-width='2'/>")
    for r in range(2):
        for c in range(4):
            parts.append(f"<rect x='{x + c * cell + 8}' y='{y + r * cell + 8}' width='9' height='9' fill='{INK}'/>")
    for c, r, w, h in areas:
        cx, cy, rad = x + (c + w / 2) * cell, y + (r + h / 2) * cell, 8
        parts.append(f"<path d='M {cx:.1f} {cy - rad:.1f} A {rad} {rad} 0 0 0 {cx:.1f} {cy + rad:.1f} Z' fill='{BLUE}'/>")
        parts.append(f"<path d='M {cx:.1f} {cy - rad:.1f} A {rad} {rad} 0 0 1 {cx:.1f} {cy + rad:.1f} Z' fill='{RED}'/>")
    for r in range(2):
        parts.append(ap.text(x + 4 * cell + 22, y + r * cell + cell / 2 + 7, new[r], 22, PURPLE))
    return parts


def legend(x, y, size):
    return [f"<rect x='{x}' y='{y - 10}' width='9' height='9' fill='{INK}'/>",
            ap.text(x + 22, y, 'Y, die Helligkeit eines Pixels', size, INK, 'start'),
            f"<path d='M {x + 4.5} {y + 15} A 8 8 0 0 0 {x + 4.5} {y + 31} Z' fill='{BLUE}'/>",
            f"<path d='M {x + 4.5} {y + 15} A 8 8 0 0 1 {x + 4.5} {y + 31} Z' fill='{RED}'/>",
            ap.text(x + 22, y + 30, 'Cb und Cr, die Farbe der Fläche', size, INK, 'start')]


def wide():
    W, cell = 960, 56
    parts = legend(20, 28, 16)
    parts.append(ap.text(W - 20, 28, 'neue Farbwerte', 16, PURPLE, 'end'))
    parts.append(ap.text(W - 20, 48, 'je Zeile', 16, PURPLE, 'end'))
    for k, form in enumerate(FORMS):
        x0 = 24 + k * 320
        parts.append(ap.text(x0 + 2 * cell, 102, form[0], 26, INK))
        parts += strip(x0, 122, cell, form)
        parts.append(ap.text(x0 + 2 * cell, 266, form[3], 16, GREY))
        parts.append(ap.text(x0 + 2 * cell, 290, form[4], 16, INK))
    save('subsampling-wide', W, 308, parts)


def narrow():
    W, cell = 440, 52
    parts = legend(16, 26, 15)
    parts.append(ap.text(W - 16, 88, 'neue Farbwerte je Zeile', 15, PURPLE, 'end'))
    for k, form in enumerate(FORMS):
        top = 110 + k * 180
        parts.append(ap.text(16, top + 60, form[0], 24, INK, 'start'))
        parts += strip(104, top, cell, form)
        parts.append(ap.text(16, top + 138, form[3], 15, GREY, 'start'))
        parts.append(ap.text(W - 16, top + 138, form[4], 15, INK, 'end'))
    save('subsampling', W, 110 + 3 * 180 - 26, parts)


def save(name, w, h, parts):
    head = ap.svg_head(w, h, 'Chroma Subsampling 4:4:4, 4:2:2 und 4:2:0 auf 4 × 2 Pixeln; gemacht von bilder/subsampling.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/video-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


wide()
narrow()
