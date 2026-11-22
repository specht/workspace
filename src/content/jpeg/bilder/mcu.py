"""An MCU of 44-baseline.jpg (»JPEG-Decoder«, »Chroma Subsampling«): Y with
the sampling factors 2 × 2, Cb and Cr with 1 × 1. The MCU covers 16 × 16
pixels: Y in four blocks of 8 × 8 values, one value per pixel; Cb and Cr in
one block each, every value for 2 × 2 pixels. The order of the four Y
blocks is left out: a task asks for it.

    python3 src/content/jpeg/bilder/mcu.py
    ruby src/scripts/render-svg.rb src/content/jpeg/mcu.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import diagram_labels  # noqa: E402,F401  (runs the script in docker/drawings)

INK, GREY, LINE, WHITE, BLUE, RED, PALE = '#1f2937', '#6b7280', '#d1d5db', '#ffffff', '#2b5fb4', '#dc2626', '#f3f4f6'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
CELL = 11          # one pixel
SIDE = 16 * CELL   # the MCU
W, H = 680, 300

s = []


def text(x, y, t, size=17, color=INK, anchor='middle', weight=400):
    s.append(f'<text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" text-anchor="{anchor}" font-weight="{weight}">{t}</text>')


def panel(x0, y0, name, color, step, blocks, lines):
    """the 16 × 16 pixels of the MCU; a thin line every step pixels (one
    value), a thick one around each block of 8 × 8 values"""
    s.append(f'<rect x="{x0}" y="{y0}" width="{SIDE}" height="{SIDE}" fill="{color}" fill-opacity="0.10"/>')
    for k in range(1, 16 // step):
        p = k * step * CELL
        s.append(f'<line x1="{x0 + p}" y1="{y0}" x2="{x0 + p}" y2="{y0 + SIDE}" stroke="{color}" stroke-opacity="0.45" stroke-width="0.8"/>')
        s.append(f'<line x1="{x0}" y1="{y0 + p}" x2="{x0 + SIDE}" y2="{y0 + p}" stroke="{color}" stroke-opacity="0.45" stroke-width="0.8"/>')
    for k in range(1, blocks):
        p = k * SIDE // blocks
        s.append(f'<line x1="{x0 + p}" y1="{y0}" x2="{x0 + p}" y2="{y0 + SIDE}" stroke="{color}" stroke-width="2.6"/>')
        s.append(f'<line x1="{x0}" y1="{y0 + p}" x2="{x0 + SIDE}" y2="{y0 + p}" stroke="{color}" stroke-width="2.6"/>')
    s.append(f'<rect x="{x0}" y="{y0}" width="{SIDE}" height="{SIDE}" fill="none" stroke="{color}" stroke-width="2.6"/>')
    text(x0 + SIDE / 2, y0 - 16, name, 19, color, weight=600)
    for i, line in enumerate(lines):
        text(x0 + SIDE / 2, y0 + SIDE + 28 + i * 21, line, 15, INK)


y0 = 52
panel(28, y0, 'Y', INK, 1, 2, ['4 Blöcke, ein Wert', 'für jedes Pixel'])
panel(28 + SIDE + 54, y0, 'Cb', BLUE, 2, 1, ['1 Block, ein Wert', 'für 2 × 2 Pixel'])
panel(28 + 2 * (SIDE + 54), y0, 'Cr', RED, 2, 1, ['1 Block, ein Wert', 'für 2 × 2 Pixel'])

out = os.path.join(HERE, '..', 'mcu.svg')
with open(out, 'w') as f:
    f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" font-family="{FONT}">\n'
            '  <!-- Eine MCU mit 2 × 2 Subsampling: vier Blöcke Y, je einer Cb und Cr.\n'
            '       Gemacht von bilder/mcu.py; die dunkle Fassung mcu-dark.svg\n'
            '       und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n  '
            + '\n  '.join(s) + '\n</svg>\n')
print(out)
