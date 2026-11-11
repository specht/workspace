"""The FFT as a tree (»Schneller: die FFT«): eight samples x0 to x7 in a row
of cells; split into the samples with an even index and those with an odd
one, each half split again the same way, down to single samples, whose DFT
is the sample itself. Lines from each group to its two halves, even to the
left in blue, odd to the right in purple; beside each level how many DFTs of
what size it has. Read from the bottom up, it is how fft puts them together:
three levels, each with eight multiplications. Wide for a desktop, narrow
for a phone (smaller cells, the counts under the tree).

    src/scripts/drawings python3 src/content/audio-compression/bilder/fft_tree.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/fft-tree.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/fft-tree-wide.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, BLUE, PURPLE, WHITE = '#1f2937', '#6b7280', '#d1d5db', '#2b5fb4', '#7c3aed', '#ffffff'


def levels():
    """the groups of each level, each a list of indices"""
    out = [[list(range(8))]]
    while len(out[-1][0]) > 1:
        out.append([half for group in out[-1] for half in (group[0::2], group[1::2])])
    return out


def placed(level, left, cell, total):
    """the x where each group of a level starts: every level as wide as the
    bottom row, its groups spread with equal gaps"""
    m = len(level)
    gap = (total - 8 * cell) / (m - 1) if m > 1 else 0
    return [left + j * (len(level[0]) * cell + gap) for j in range(m)]


def picture(name, cell, gap, row, size, side):
    groups = levels()
    left = 10
    total = 8 * cell + 7 * gap
    parts = []
    counts = ['eine DFT aus 8 Samples', 'zwei aus 4', 'vier aus 2', 'acht aus 1: das Sample selbst']
    for depth, level in enumerate(groups):
        y = 10 + depth * row
        xs = placed(level, left, cell, total)
        if depth + 1 < len(groups):
            nxt = groups[depth + 1]
            nxs = dict(zip(map(tuple, nxt), placed(nxt, left, cell, total)))
            for x, g in zip(xs, level):
                for half, colour in ((g[0::2], BLUE), (g[1::2], PURPLE)):
                    hx = nxs[tuple(half)] + len(half) * cell / 2
                    parts.append(f"<line x1='{x + len(g) * cell / 2:.1f}' y1='{y + cell + 2}' x2='{hx:.1f}' y2='{y + row - 3}' stroke='{colour}' stroke-width='2'/>")
        for x, g in zip(xs, level):
            for k, i in enumerate(g):
                cx = x + k * cell
                parts.append(f"<rect x='{cx:.1f}' y='{y}' width='{cell}' height='{cell}' fill='{WHITE}' stroke='{INK}' stroke-width='1.4'/>")
                parts.append(f"<text x='{cx + cell * 0.42:.1f}' y='{y + cell * 0.66:.1f}' text-anchor='middle' font-family=\"{ap.MONO}\" font-size='{size}' fill='{INK}'>x<tspan dy='0.3em' font-size='0.65em'>{i}</tspan></text>")
        if side:
            parts.append(ap.text(left + total + 24, y + cell * 0.66, counts[depth], size, GREY, 'start'))
    bottom = 10 + (len(groups) - 1) * row + cell
    width = left + total + (24 + 0.62 * size * 30 if side else 0) + 10
    if not side:
        for depth, text in enumerate(counts):
            parts.append(ap.text(left, bottom + 28 + depth * (size + 8), f"{'Reihe ' + str(depth + 1)}: {text}", size, GREY, 'start'))
        bottom += 28 + 4 * (size + 8)
    head = ap.svg_head(round(width), round(bottom + 10), 'Die FFT als Baum: acht Samples, geteilt in gerade und ungerade Indizes bis zu einzelnen Samples; gemacht von bilder/fft_tree.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/audio-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


picture('fft-tree-wide', 44, 8, 78, 17, True)
picture('fft-tree', 40, 6, 70, 15, False)
