"""Vectors in quarter pixels (»Wie H.264 sucht«): a row of seven pixels,
grey as bright as their value, with the value in each; under it the same
row a quarter pixel further, each new value between two old ones, three
quarters of the one it lies over and a quarter of the next, rounded. For
one of them the two lines to its parents with ¾ and ¼. Simplified: H.264
makes half pixels from six neighbours and quarters from two. Wide for a
desktop, narrow for a phone.

    src/scripts/drawings python3 src/content/video-compression/bilder/quarter_pixel.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/quarter-pixel.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/quarter-pixel-wide.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, PURPLE, WHITE = '#1f2937', '#6b7280', '#d1d5db', '#7c3aed', '#ffffff'
ROW = [40, 60, 120, 200, 220, 220, 200]
SHIFTED = [round(0.75 * a + 0.25 * b) for a, b in zip(ROW, ROW[1:])]
SHOW = 2                       # the new value whose parents are drawn


def grey(v):
    """a pixel's grey is its value, the same in both themes: rgb(), which
    render-svg.rb's dark palette leaves as it is"""
    return f'rgb({v},{v},{v})'


def cell(x, y, c, v, size):
    # the number on a cell keeps its colour too, as the cell does
    ink = 'rgb(31,41,55)' if v > 130 else 'rgb(255,255,255)'
    return [f"<rect x='{x:.1f}' y='{y}' width='{c}' height='{c}' fill='{grey(v)}' stroke='{LINE}' stroke-width='1'/>",
            ap.text(x + c / 2, y + c / 2 + size * 0.35, str(v), size, ink)]


def picture(name, c, size):
    left, top = 20 + 6 * size, 30
    parts = [ap.text(10, top + c / 2 + size * 0.35, 'Pixel', size, GREY, 'start')]
    for i, v in enumerate(ROW):
        parts += cell(left + i * c, top, c, v, size)
    y2 = top + c + 60
    parts.append(ap.text(10, y2 + c / 2 + size * 0.35, '+ ¼', size, PURPLE, 'start'))
    for i, v in enumerate(SHIFTED):
        x = left + (i + 0.25) * c
        parts += cell(x, y2, c, v, size)
        parts.append(f"<rect x='{x:.1f}' y='{y2}' width='{c}' height='{c}' fill='none' stroke='{PURPLE}' stroke-width='1.5'/>")
    # the parents of one new value
    nx = left + (SHOW + 0.25) * c + c / 2
    for k, weight in ((0, '¾'), (1, '¼')):
        px = left + (SHOW + k) * c + c / 2
        parts.append(f"<line x1='{px:.1f}' y1='{top + c + 2}' x2='{nx:.1f}' y2='{y2 - 2}' stroke='{PURPLE}' stroke-width='2'/>")
        parts.append(ap.text((px + nx) / 2 + (-14 if k == 0 else 14), (top + c + y2) / 2 + 4, weight, size + 2, PURPLE))
    eq = f'¾ · {ROW[SHOW]} + ¼ · {ROW[SHOW + 1]} = {0.75 * ROW[SHOW] + 0.25 * ROW[SHOW + 1]:g}'.replace('.', ',')
    parts.append(ap.text(left, y2 + c + size + 14, eq, size, PURPLE, 'start'))
    width = left + (len(ROW) + 0.5) * c + 20
    height = y2 + c + size + 24
    head = ap.svg_head(round(width), round(height), 'Eine Reihe Pixel und dieselbe Reihe um einen Viertelpixel verschoben; gemacht von bilder/quarter_pixel.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/video-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


picture('quarter-pixel-wide', 72, 16)
picture('quarter-pixel', 48, 13)
