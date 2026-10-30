"""The pictures of »Zwei Faktoren: TOTP selbst gebaut« drawn in Python, as
SVG beside the page.

    python3 src/content/totp/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/totp/NAME.svg WIDTH

- base32-bits: JBSWY3DP, the first eight characters of the key in the task
  »Wie viele Bytes?«: each character 5 bits, the 40 bits in a row, the same
  bits taken 8 at a time as the bytes 72 101 108 108 111, »Hello«.
- hmac-offset: the 20 bytes of the HMAC from the example (key
  12345678901234567890, counter 1); the last byte ab, its lowest 4 bits b =
  11 the offset, and the 4 bytes from index 11 that become the code.
- zeitfenster: the Unix time cut into windows of 30 seconds, each a value of
  the counter; the time 59 of the RFC's first test value in window 1.
All values are computed here, not typed in.
"""

import base64
import hashlib
import hmac
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402
from diagram_labels import code  # noqa: E402

OUT = os.path.join(HERE, '..')
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
INK = '#1f2937'
GREY = '#6b7280'
BLUE = '#2b5fb4'
AMBER = '#d97706'
PALE = '#eef3fb'
LIGHT = '#f3f4f6'
WHITE = '#ffffff'
MONO = "'0xProto Nerd Font Mono', 'DejaVu Sans Mono', monospace"


def save(name, w, h, comment, parts, scale=2):
    render = f'src/scripts/render-svg.rb src/content/totp/{name}.svg {scale * w}'
    head = ap.svg_head(w, h, comment + '; gemacht von bilder/diagramme.py, als Bild:', render)
    head = head.replace('<svg ', f"<svg font-family='{FONT}' ", 1)
    with open(os.path.join(OUT, name + '.svg'), 'w') as f:
        f.write(head + f"<rect width='{w}' height='{h}' fill='{WHITE}'/>" + ''.join(parts) + '</svg>\n')


def text(x, y, s, size=16, color=INK, anchor='middle', mono=False):
    family = f" font-family=\"{MONO}\"" if mono else ''
    return f"<text x='{x:.1f}' y='{y:.1f}' font-size='{size}' fill='{color}' text-anchor='{anchor}'{family}>{s}</text>"


def box(x, y, w, h, fill=WHITE, stroke=INK, width=1.5):
    return (f"<rect x='{x:.1f}' y='{y:.1f}' width='{w:.1f}' height='{h:.1f}' fill='{fill}' "
            f"stroke='{stroke}' stroke-width='{width}'/>")


def base32_bits():
    alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567'
    word = 'JBSWY3DP'
    bits = ''.join(format(alphabet.index(c), '05b') for c in word)
    data = base64.b32decode(word)
    bw = 22
    x0 = 120
    w, h = x0 + len(bits) * bw + 40, 300
    parts = [text(x0 - 16, 62, 'Zeichen', 15, GREY, 'end'), text(x0 - 16, 147, 'Bits', 15, GREY, 'end'),
             text(x0 - 16, 232, 'Bytes', 15, GREY, 'end')]
    for i, c in enumerate(word):
        x = x0 + i * 5 * bw
        parts.append(box(x, 30, 5 * bw, 46, PALE if i % 2 == 0 else WHITE))
        parts.append(code(c, x + 2.5 * bw, 60, size=20, anchor='middle', background=False))
    for i, b in enumerate(bits):
        x = x0 + i * bw
        shade = PALE if (i // 5) % 2 == 0 else WHITE
        parts.append(box(x, 125, bw, 30, shade, GREY, 1))
        parts.append(text(x + bw / 2, 146, b, 15, INK, mono=True))
    for i, byte in enumerate(data):
        x = x0 + i * 8 * bw
        parts.append(box(x, 200, 8 * bw, 46, LIGHT if i % 2 == 0 else WHITE, BLUE, 2))
        parts.append(text(x + 4 * bw, 230, str(byte), 18))
        parts.append(code(chr(byte), x + 4 * bw, 278, size=17, anchor='middle', background=False))
    # the bits of a byte read across the borders of the characters
    for i in range(len(data) + 1):
        x = x0 + i * 8 * bw
        parts.append(f"<line x1='{x}' y1='160' x2='{x}' y2='196' stroke='{BLUE}' stroke-width='1.5' stroke-dasharray='4 3'/>")
    save('base32-bits', w, h, 'JBSWY3DP: 8 Zeichen zu 5 Bits, 40 Bits, 5 Bytes', parts)


def hmac_offset():
    digest = hmac.new(b'12345678901234567890', (1).to_bytes(8, 'big'), hashlib.sha1).digest()
    offset = digest[-1] & 0x0F
    value = int.from_bytes(digest[offset:offset + 4], 'big') & 0x7FFFFFFF
    cw = 46
    x0 = 30
    w, h = x0 * 2 + 20 * cw, 290
    parts = []
    for i, byte in enumerate(digest):
        x = x0 + i * cw
        if i == len(digest) - 1:
            fill, stroke, width = WHITE, AMBER, 3
        elif offset <= i < offset + 4:
            fill, stroke, width = PALE, BLUE, 2.5
        else:
            fill, stroke, width = WHITE, GREY, 1.2
        parts.append(box(x, 70, cw, 44, fill, stroke, width))
        parts.append(text(x + cw / 2, 98, format(byte, '02x'), 17, INK, mono=True))
        parts.append(text(x + cw / 2, 58, i, 12, GREY))
    # from the low half of the last byte to the byte it names
    last = x0 + 19.5 * cw
    first = x0 + (offset + 0.5) * cw
    parts.append(ap.curve((last, 118), (last, 175), (first + 60, 185), (first + 4, 122), color=AMBER, width=2, length=11, half=5.5))
    parts.append(text(last + 18, 212, f'untere 4 Bits: {format(digest[-1] & 0x0F, "x")} = {offset}', 15, AMBER, 'end'))
    parts.append(code(f'{digest[offset:offset + 4].hex(" ")}  →  {value:08x} = {value}  →  {value % 10**6:06d}',
                      x0 + 4 * cw, 262, size=16, anchor='start', background=False))
    save('hmac-offset', w, h, 'Die HMAC zu Zähler 1: der Offset im letzten Byte und die 4 Bytes ab dort', parts)


def zeitfenster():
    w, h = 1000, 210
    x0, sec = 60, 5.0   # pixels per second
    parts = []
    for k in range(5):
        x = x0 + k * 30 * sec
        parts.append(box(x, 70, 30 * sec, 50, PALE if k % 2 == 0 else WHITE, INK, 1.5))
        parts.append(text(x + 15 * sec, 101, f'Zähler {k}', 17))
        parts.append(text(x, 58, k * 30, 14, GREY))
    parts.append(text(x0 + 150 * sec, 58, 150, 14, GREY))
    parts.append(text(x0 + 150 * sec + 14, 101, 'Unix-Zeit in Sekunden →', 14, GREY, 'start'))
    t = 59
    x = x0 + t * sec
    parts.append(f"<line x1='{x:.1f}' y1='64' x2='{x:.1f}' y2='150' stroke='{BLUE}' stroke-width='2.5'/>")
    parts.append(text(x, 172, f'Zeit {t}: geteilt durch 30, abgerundet {t // 30}', 16, BLUE))
    save('zeitfenster', w, h, 'Die Unix-Zeit in Fenstern zu 30 Sekunden, jedes ein Wert des Zählers', parts)


base32_bits()
hmac_offset()
zeitfenster()
