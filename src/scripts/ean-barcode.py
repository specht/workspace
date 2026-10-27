#!/usr/bin/env python3
"""An EAN-13 barcode as SVG, with the digits under the bars, on white like a
label (check-digits/barcode.svg), and the two pictures that explain it on the
page (check-digits/aufbau.svg, ziffer.svg). Usage:

    python3 src/scripts/ean-barcode.py 4260111020434 > src/content/check-digits/barcode.svg
    python3 src/scripts/ean-barcode.py --aufbau 4260111020434 > src/content/check-digits/aufbau.svg
    python3 src/scripts/ean-barcode.py --ziffer 4 > src/content/check-digits/ziffer.svg

then src/scripts/render-svg.rb src/content/check-digits/aufbau.svg (and
ziffer.svg) for the light and dark pictures. Their colours are those of
render-svg.rb: the label stays white and the bars dark in both modes, the
notes around it follow the page.

The bars follow the GS1 encoding: the first digit picks which of the left six
use the code sets L or G, the right six use R; guard bars at the start, in
the middle and at the end reach lower than the others.
"""
import sys

L = ['0001101', '0011001', '0010011', '0111101', '0100011',
     '0110001', '0101111', '0111011', '0110111', '0001011']
R = [''.join('1' if b == '0' else '0' for b in code) for code in L]
G = [code[::-1] for code in R]
PARITY = ['LLLLLL', 'LLGLGG', 'LLGGLG', 'LLGGGL', 'LGLLGG',
          'LGGLLG', 'LGGGLL', 'LGLGLG', 'LGLGGL', 'LGGLGL']


def modules(code):
    digits = [int(c) for c in code]
    first, left, right = digits[0], digits[1:7], digits[7:]
    bits = [('101', True)]
    for d, p in zip(left, PARITY[first]):
        bits.append(((L if p == 'L' else G)[d], False))
    bits.append(('01010', True))
    for d in right:
        bits.append((R[d], False))
    bits.append(('101', True))
    return bits


def svg(code, unit=4):
    quiet = 11 * unit
    width = quiet + 95 * unit + 7 * unit
    size = 6 * unit                       # the digits
    top = 2 * unit
    bar_h = 22 * unit                     # the ordinary bars end above the digits,
    guard_h = bar_h + size // 2           # the guard bars reach down between them
    text_y = top + bar_h + round(size * 0.85)
    out = []
    x = quiet
    for pattern, guard in modules(code):
        for bit in pattern:
            if bit == '1':
                out.append(f'<rect x="{x}" y="{top}" width="{unit}" height="{guard_h if guard else bar_h}"/>')
            x += unit
    font = f'font-family="\'0xProto Nerd Font Mono\', ui-monospace, monospace" font-size="{size}"'
    texts = [(quiet - 3 * unit, code[0], 'end')]
    for k in range(6):
        texts.append((quiet + (3 + 7 * k + 3.5) * unit, code[1 + k], 'middle'))
        texts.append((quiet + (50 + 7 * k + 3.5) * unit, code[7 + k], 'middle'))
    for tx, digit, anchor in texts:
        out.append(f'<text x="{tx:.1f}" y="{text_y}" text-anchor="{anchor}" {font}>{digit}</text>')
    height = text_y + 3 * unit
    label = f'Ein Strichcode mit dem EAN-13-Code {code[0]} {code[1:7]} {code[7:]}'
    return (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {width} {height}" width="{width}" height="{height}" '
            f'role="img" aria-label="{label}"><rect width="{width}" height="{height}" fill="#fff"/>'
            f'<g fill="#111">{"".join(out)}</g></svg>\n')


SANS = "'IBM Plex Sans', system-ui, sans-serif"
MONO = "'0xProto Nerd Font Mono', ui-monospace, monospace"
LABEL, BAR = '#fdfdfd', '#0b0f19'          # a printed label: the same in both modes
INK, GREY, LINE, BLUE, AMBER = '#1f2937', '#6b7280', '#d1d5db', '#2b5fb4', '#d97706'


def text(x, y, content, size=15, colour=INK, anchor='middle', family=SANS, weight='400'):
    return (f'<text x="{x:.1f}" y="{y:.1f}" text-anchor="{anchor}" font-family="{family}" '
            f'font-size="{size}" font-weight="{weight}" fill="{colour}">{content}</text>')


def region(x0, x1, y0, y1, colour, label):
    """A part of the code: a pale band of its colour behind the bars from
    x0 to x1, its name above it. The band is the colour itself, faint, so
    that it stays pale on the white label in dark mode too."""
    return (f'<rect x="{x0:.1f}" y="{y0}" width="{x1 - x0:.1f}" height="{y1 - y0}" fill="{colour}" fill-opacity="0.16"/>'
            f'<rect x="{x0:.1f}" y="{y0}" width="{x1 - x0:.1f}" height="4" fill="{colour}"/>'
            + text((x0 + x1) / 2, y0 - 9, label, 14, colour))


def aufbau(code, unit=6):
    """The barcode with what its parts are: the guard bars at the edges and
    in the middle, the six digits left and right, 95 modules in all."""
    margin, top = 30, 92
    x0 = margin + 11 * unit                # where the modules start
    bar_h, size = 22 * unit, 6 * unit
    guard_h = bar_h + size // 2
    width = x0 + 95 * unit + 7 * unit + margin
    out = [f'<rect width="{width}" height="{top + guard_h + 70}" fill="#ffffff"/>',
           f'<rect x="{margin}" y="{top - 10}" width="{width - 2 * margin}" height="{guard_h + 10 + size + 8}" rx="6" fill="{LABEL}" stroke="{LINE}" stroke-width="1.5"/>']
    # the parts behind the bars, over the whole height of the label
    m = lambda i: x0 + i * unit
    y0, y1 = top - 10, top + guard_h + size + 8
    for a, b, colour, label in [(0, 3, AMBER, 'Rand'), (3, 45, BLUE, 'sechs Ziffern, je 7 Module'), (45, 50, AMBER, 'Mitte'),
                                (50, 92, BLUE, 'sechs Ziffern, je 7 Module'), (92, 95, AMBER, 'Rand')]:
        out.append(region(m(a), m(b), y0, y1, colour, label))
    x = x0
    for pattern, guard in modules(code):
        for bit in pattern:
            if bit == '1':
                out.append(f'<rect x="{x}" y="{top}" width="{unit}" height="{guard_h if guard else bar_h}" fill="{BAR}"/>')
            x += unit
    y_text = top + bar_h + round(size * 0.85)
    out.append(text(x0 - 3 * unit, y_text, code[0], size, BAR, 'end', MONO))
    for k in range(6):
        out.append(text(x0 + (3 + 7 * k + 3.5) * unit, y_text, code[1 + k], size, BAR, 'middle', MONO))
        out.append(text(x0 + (50 + 7 * k + 3.5) * unit, y_text, code[7 + k], size, BAR, 'middle', MONO))
    # 95 modules: a scale under the label
    yb = top + guard_h + size + 30
    out.append(f'<path d="M{m(0)} {yb - 5} V{yb} H{m(95)} V{yb - 5}" fill="none" stroke="{GREY}" stroke-width="1.5"/>')
    out.append(text((m(0) + m(95)) / 2, yb + 20, '95 Module: 3 + 6 · 7 + 5 + 6 · 7 + 3', 14, GREY))
    height = yb + 32
    out[0] = f'<rect width="{width}" height="{height}" fill="#ffffff"/>'
    return (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {width} {height}" width="{width}" height="{height}">'
            + ''.join(out) + '</svg>\n')


def ziffer(digit, cell=34):
    """One digit in its three code sets: 7 modules each, dark ones counted."""
    rows = [('L', 'links', L[digit]), ('G', 'links', G[digit]), ('R', 'rechts', R[digit])]
    x0, y0, gap = 150, 30, 74
    width = x0 + 7 * cell + 230
    out = []
    for r, (name, side, pattern) in enumerate(rows):
        y = y0 + r * gap
        out.append(text(20, y + cell * 0.68, f'{name}', 22, INK, 'start', MONO, '600'))
        out.append(text(52, y + cell * 0.68, f'{side}', 15, GREY, 'start'))
        for i, bit in enumerate(pattern):
            out.append(f'<rect x="{x0 + i * cell}" y="{y}" width="{cell}" height="{cell}" fill="{BAR if bit == "1" else LABEL}" stroke="{LINE}" stroke-width="1.5"/>')
            out.append(text(x0 + (i + 0.5) * cell, y + cell + 19, bit, 15, INK, 'middle', MONO))
        dark = pattern.count('1')
        out.append(text(x0 + 7 * cell + 24, y + cell * 0.68, f'{dark} dunkle Module, {"ungerade" if dark % 2 else "gerade"}', 15, INK, 'start'))
    height = y0 + 2 * gap + cell + 34
    return (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {width} {height}" width="{width}" height="{height}">'
            f'<rect width="{width}" height="{height}" fill="#ffffff"/>' + ''.join(out) + '</svg>\n')


if __name__ == '__main__':
    if len(sys.argv) == 3 and sys.argv[1] == '--aufbau':
        sys.stdout.write(aufbau(sys.argv[2]))
        sys.exit()
    if len(sys.argv) == 3 and sys.argv[1] == '--ziffer':
        sys.stdout.write(ziffer(int(sys.argv[2])))
        sys.exit()
    code = sys.argv[1] if len(sys.argv) > 1 else ''
    if len(code) != 13 or not code.isdigit():
        sys.exit('Usage: ean-barcode.py CODE (13 digits)')
    sys.stdout.write(svg(code))
