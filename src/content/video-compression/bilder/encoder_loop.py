"""Why every encoder contains a decoder (»Was der Unterschied kostet«), two
drawings from one script.

encoder-loop: frame k minus the template, the difference rounded (DCT,
step) and into the file; under it, in a dashed frame »der Decoder im
Encoder«, the rounded difference calculated back, plus the template, gives
the frame the decoder builds, and that becomes the template for frame
k + 1, the one subtracted from it and added to it.

drift: the mean error of frames 0, 4, …, 20 of the pan as difference.py
prints it: encode_naive, which takes the difference to the original,
grows from 3,3 to 13,2; encode, which takes it to the frame built, stays
at 3,4.

Wide for a desktop, narrow for a phone.

    src/scripts/drawings python3 src/content/video-compression/bilder/encoder_loop.py
    for f in encoder-loop encoder-loop-wide drift drift-wide; do
        src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/$f.svg; done
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, WHITE, PURPLE, BLUE = '#1f2937', '#6b7280', '#d1d5db', '#ffffff', '#7c3aed', '#2b5fb4'
NAIVE = [3.3, 5.8, 8.0, 9.8, 11.6, 13.2]
GOOD = [3.3, 3.4, 3.4, 3.4, 3.4, 3.5]


def box(x0, y0, x1, y1, lines, size, colour=INK):
    parts = [f"<rect x='{x0}' y='{y0}' width='{x1 - x0}' height='{y1 - y0}' rx='6' fill='{WHITE}' stroke='{colour}' stroke-width='1.6'/>"]
    mid = (y0 + y1) / 2 - (len(lines) - 1) * (size + 3) / 2
    for i, line in enumerate(lines):
        parts.append(ap.text((x0 + x1) / 2, mid + i * (size + 3) + size * 0.35, line, size, colour))
    return parts


def circle(cx, cy, sign, size):
    return [f"<circle cx='{cx}' cy='{cy}' r='15' fill='{WHITE}' stroke='{INK}' stroke-width='1.6'/>",
            ap.text(cx, cy + size * 0.38, sign, size + 4, INK)]


def arrow(points, colour=INK, head=9):
    """a polyline whose last segment ends in an arrowhead at the last point"""
    (x1, y1), (x2, y2) = points[-2], points[-1]
    dx, dy = x2 - x1, y2 - y1
    length = (dx * dx + dy * dy) ** 0.5
    ux, uy = dx / length, dy / length
    end = (x2 - ux * head * 0.8, y2 - uy * head * 0.8)
    path = ' '.join(f"{'M' if i == 0 else 'L'} {x:.1f} {y:.1f}" for i, (x, y) in enumerate(points[:-1] + [end]))
    return [f"<path d='{path}' fill='none' stroke='{colour}' stroke-width='1.8'/>",
            ap.arrowhead(x2, y2, ux, uy, colour, head, head / 2)]


def loop(name, s, size, short=False):
    """s: a scale for every x; the layout is the same in both; short: the
    shorter words of a phone"""
    X = lambda v: round(v * s, 1)
    parts = []
    top, row = 40, 50                     # the first row: frame, minus, rounding, file
    cy = top + row / 2
    parts += box(X(10), top, X(130), top + row, ['Bild k'], size)
    parts += circle(X(190), cy, '−', size)
    parts += box(X(250), top, X(500), top + row, ['runden', '(DCT)'] if short else ['Unterschied runden', '(DCT, Schritt)'], size)
    parts += box(X(570), top, X(700), top + row, ['Datei'] if short else ['in die Datei'], size)
    parts += arrow([(X(130), cy), (X(190) - 15, cy)])
    parts += arrow([(X(190) + 15, cy), (X(250), cy)])
    parts += arrow([(X(500), cy), (X(570), cy)])
    # the decoder in the encoder
    mid = X(375)
    back_top, back = top + row + 40, 44
    parts += box(X(250), back_top, X(500), back_top + back, ['zurückrechnen'], size)
    parts += arrow([(mid, top + row), (mid, back_top)])
    plus_y = back_top + back + 50
    parts += circle(mid, plus_y, '+', size)
    parts += arrow([(mid, back_top + back), (mid, plus_y - 15)])
    tmpl = (X(110), plus_y - 22, X(270), plus_y + 22)
    parts += box(*tmpl, ['Vorlage'] if short else ['Vorlage:', 'zuletzt gebaut'], size)
    parts += arrow([(mid - 15, plus_y), (tmpl[2], plus_y)])
    if not short:
        parts.append(ap.text((mid - 15 + tmpl[2]) / 2, plus_y - 8, 'gebaut', size - 2, GREY))
    # the template: up into the minus, and round below into the plus
    vx = X(190)
    parts += arrow([(vx, tmpl[1]), (vx, cy + 15)])
    low = plus_y + 50
    parts += arrow([(X(150), tmpl[3]), (X(150), low), (mid, low), (mid, plus_y + 15)])
    parts.append(ap.text(vx - 8, (tmpl[1] + cy + 15) / 2 + 12, 'für', size - 2, GREY, 'end'))
    parts.append(ap.text(vx - 8, (tmpl[1] + cy + 15) / 2 + 12 + size, 'Bild k + 1', size - 2, GREY, 'end'))
    # the frame around the decoder, drawn first so it lies under the rest
    frame_top, frame_bottom = top + row + 20, low + size + 18
    parts.insert(0, f"<rect x='{X(90) if not short else 4}' y='{frame_top}' width='{X(520) - (X(90) if not short else 4)}' height='{frame_bottom - frame_top}' rx='10' fill='{PURPLE}' fill-opacity='0.05' stroke='{PURPLE}' stroke-width='1.5' stroke-dasharray='7 5'/>")
    parts.append(ap.text(X(510), frame_bottom - 8, 'der Decoder im Encoder', size, PURPLE, 'end'))
    width, height = X(710), frame_bottom + 8
    save(name, width, height, parts, 'Der Encoder rechnet den Unterschied zum Bild, das der Decoder baut')


def drift(name, width, size):
    left, right, top, bottom = 50, width - 40, 20 + 2 * (size + 8), 220
    xs = [left + (right - left) * i / 5 for i in range(6)]
    y = lambda v: bottom - (bottom - top) * v / 14
    parts = []
    for k, (label, colour) in enumerate([('encode_naive: Unterschied zum Original', GREY), ('encode: Unterschied zum gebauten Bild', PURPLE)]):
        ly = 16 + k * (size + 8)
        parts.append(f"<line x1='{left}' y1='{ly - 4}' x2='{left + 24}' y2='{ly - 4}' stroke='{colour}' stroke-width='3'/>")
        parts.append(ap.text(left + 32, ly, label, size, INK, 'start'))
    for v in (0, 4, 8, 12):
        parts.append(f"<line x1='{left}' y1='{y(v):.1f}' x2='{right}' y2='{y(v):.1f}' stroke='{LINE}' stroke-width='1'/>")
        parts.append(ap.text(left - 8, y(v) + size * 0.35, str(v), size, GREY, 'end'))
    for values, colour in ((NAIVE, GREY), (GOOD, PURPLE)):
        parts.append(f"<polyline points='{' '.join(f'{x:.1f},{y(v):.1f}' for x, v in zip(xs, values))}' fill='none' stroke='{colour}' stroke-width='3'/>")
        for x, v in zip(xs, values):
            parts.append(f"<circle cx='{x:.1f}' cy='{y(v):.1f}' r='4.5' fill='{colour}'/>")
    for i, x in enumerate(xs):
        parts.append(ap.text(x, bottom + size + 8, f'Bild {4 * i}', size, GREY))
    parts.append(ap.text(8, top - 10, 'mittlerer Fehler', size, GREY, 'start'))
    save(name, width, bottom + size + 18, parts, 'Der mittlere Fehler von encode_naive und encode Bild für Bild')


def save(name, w, h, parts, what):
    head = ap.svg_head(round(w), round(h), f'{what}; gemacht von bilder/encoder_loop.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/video-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


loop('encoder-loop-wide', 1.0, 15)
loop('encoder-loop', 0.62, 12, True)
drift('drift-wide', 700, 15)
drift('drift', 440, 13)
