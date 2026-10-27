#!/usr/bin/env python3
"""An EAN-13 barcode as SVG, with the digits under the bars, on white like a
label (check-digits/barcode.svg). Usage:

    python3 src/scripts/ean-barcode.py 4260111020434 > src/content/check-digits/barcode.svg

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


if __name__ == '__main__':
    code = sys.argv[1] if len(sys.argv) > 1 else ''
    if len(code) != 13 or not code.isdigit():
        sys.exit('Usage: ean-barcode.py CODE (13 digits)')
    sys.stdout.write(svg(code))
