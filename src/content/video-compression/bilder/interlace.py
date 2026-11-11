"""How interlace builds a frame (»Halbbilder«), without content, so that it
does not give away what combing looks like: three frames of eight rows.
Frame k with its even rows (0, 2, 4, 6) blue and the odd ones pale, frame
k + 1 with its odd rows (1, 3, 5, 7) purple and the even ones pale, and the
frame interlace makes of them, in the middle: blue and purple rows taking
turns, each from its own moment, the arrows coming from both sides. Row
numbers beside the first. Wide for a desktop, narrow for a phone (the
same, smaller).

    src/scripts/drawings python3 src/content/video-compression/bilder/interlace.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/interlace.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/interlace-wide.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, WHITE, BLUE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#ffffff', '#2b5fb4', '#7c3aed'
ROWS = 8


def frame(x0, y0, w, rh, take, colour, size, label):
    """take: which rows count (0 even, 1 odd, None all, from both colours)"""
    parts = [ap.text(x0 + w / 2, y0 - 10, label, size, INK)]
    for r in range(ROWS):
        y = y0 + r * rh
        if take is None:
            c, fill = (BLUE, '0.35') if r % 2 == 0 else (PURPLE, '0.35')
        elif r % 2 == take:
            c, fill = colour, '0.35'
        else:
            c, fill = LINE, '0.25'
        stroke = LINE if c == LINE else c
        parts.append(f"<rect x='{x0}' y='{y + 1}' width='{w}' height='{rh - 2}' rx='2' fill='{c}' fill-opacity='{fill}' stroke='{stroke}' stroke-width='1.2'/>")
    return parts


def picture(name, width, size, w, gap, rh):
    left = 40
    y0 = 20 + size + 6
    parts = []
    for r in range(ROWS):
        parts.append(ap.text(left - 10, y0 + r * rh + rh / 2 + size * 0.35, str(r), size, GREY, 'end'))
    # frame k on the left, the result in the middle, frame k + 1 on the right:
    # its rows come in from both sides without crossing
    xs = [left, left + w + gap, left + 2 * (w + gap)]
    parts += frame(xs[0], y0, w, rh, 0, BLUE, size, 'Bild k')
    parts += frame(xs[1], y0, w, rh, None, None, size, 'zusammen')
    parts += frame(xs[2], y0, w, rh, 1, PURPLE, size, 'Bild k + 1')
    for r in range(ROWS):
        y = y0 + r * rh + rh / 2
        if r % 2 == 0:
            x1, x2, d, colour = xs[0] + w + 4, xs[1] - 4, 1, BLUE
        else:
            x1, x2, d, colour = xs[2] - 4, xs[1] + w + 4, -1, PURPLE
        parts.append(f"<line x1='{x1}' y1='{y:.1f}' x2='{x2 - 8 * d}' y2='{y:.1f}' stroke='{colour}' stroke-width='1.6'/>")
        parts.append(ap.arrowhead(x2, y, d, 0, colour, 8, 4))
    height = y0 + ROWS * rh + size + 22
    parts.append(ap.text(xs[1] + w / 2, height - 8, 'gerade Zeilen aus Bild k, ungerade aus Bild k + 1', size - 1, GREY, 'middle'))
    head = ap.svg_head(round(xs[2] + w + 20), round(height), 'Halbbilder: die geraden Zeilen aus einem Bild, die ungeraden aus dem nächsten; gemacht von bilder/interlace.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/video-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


picture('interlace-wide', 760, 15, 180, 70, 26)
picture('interlace', 440, 13, 100, 34, 22)
