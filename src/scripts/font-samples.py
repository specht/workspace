#!/usr/bin/env python3
"""Samples of fonts for »Schrift mit Absicht« and the person boxes of
Stanley Morison and Matthew Carter: a few words in a font, as outlines, so
that the page shows the font itself and needs no font file (the Microsoft
fonts may not be served; the outlines of a few words are a picture of them).

    src/scripts/drawings python3 src/scripts/font-samples.py
    for f in …; do src/scripts/render-svg.rb $f; done

The fonts come from the drawings image (docker/drawings: the Microsoft core
fonts, Open Sans, Roboto Slab, Space Grotesk, Didact Gothic, VT323) and from
the site (IBM Plex Sans, XCharter, 0xProto Nerd Font Mono).
"""

# runs in docker/drawings, the same on every machine (src/scripts/drawings)
import os as _os, sys as _sys  # noqa: E401
_sys.path.insert(0, _os.path.dirname(_os.path.abspath(__file__)))
import drawings_container  # noqa: E402
drawings_container.ensure()

import subprocess  # noqa: E402
from pathlib import Path  # noqa: E402

from fontTools.pens.svgPathPen import SVGPathPen  # noqa: E402
from fontTools.pens.transformPen import TransformPen  # noqa: E402
from fontTools.ttLib import TTFont  # noqa: E402

REPO = Path(__file__).resolve().parent.parent.parent
CONTENT = REPO / 'src' / 'content'
TEXT, GREY, BG = '#1f2937', '#6b7280', '#ffffff'

_fonts = {}


def font(pattern):
    """a font by its fontconfig name ('Times New Roman:style=Regular'); a
    variable font at its regular weight"""
    if pattern not in _fonts:
        path = subprocess.run(['fc-match', '-f', '%{file}', pattern], capture_output=True, text=True, check=True).stdout
        f = TTFont(path)
        if 'fvar' in f:
            from fontTools.varLib.instancer import instantiateVariableFont
            f = instantiateVariableFont(f, {'wght': 400})
        family = pattern.split(':')[0].lower().replace(' ', '')
        found = f['name'].getDebugName(1).lower().replace(' ', '')
        assert found.startswith(family[:6]), f'{pattern}: fontconfig gave {path}'
        _fonts[pattern] = f
    return _fonts[pattern]


def outline(pattern, s, size, x, y):
    """the text as one path, its baseline at y; returns (d, width)"""
    f = font(pattern)
    glyphs, cmap = f.getGlyphSet(), f.getBestCmap()
    scale = size / f['head'].unitsPerEm
    pen = SVGPathPen(glyphs)
    for c in s:
        name = cmap.get(ord(c), '.notdef')
        glyphs[name].draw(TransformPen(pen, (scale, 0, 0, -scale, x, y)))
        x += glyphs[name].width * scale
    return pen.getCommands(), x


def width(pattern, s, size):
    return outline(pattern, s, size, 0, 0)[1]


def write(path, w, h, body, about):
    out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {w:.0f} {h:.0f}' width='{w:.0f}' height='{h:.0f}'>",
           f"  <!-- {about}; made by src/scripts/font-samples.py, then src/scripts/render-svg.rb -->"]
    out += body
    out.append('</svg>')
    path.write_text('\n'.join(out) + '\n', encoding='utf-8')
    print(path.relative_to(REPO))


def sample(path, pattern, s, size=30, about=''):
    """one line of text in a font, nothing else (a cell of a table)"""
    # the same box for every font, so that all are shown at one size
    asc, desc = 0.95 * size, 0.3 * size
    d, w = outline(pattern, s, size, 2, asc + 2)
    write(path, w + 4, asc + desc + 4, [f"<path fill='{TEXT}' d='{d}'/>"], f'{s} in {pattern}')


def names(path, rows, about, size=26, gap=34):
    """fonts side by side, each its name written in itself, and in grey
    small below it what it is"""
    body, x = [], 2
    base = 0.95 * size + 2
    for pattern, label, note in rows:
        d, end = outline(pattern, label, size, x, base)
        body.append(f"<path fill='{TEXT}' d='{d}'/>")
        if note:
            nd, nend = outline('IBM Plex Sans:style=Regular', note, 14, x, base + 24)
            body.append(f"<path fill='{GREY}' d='{nd}'/>")
            end = max(end, nend)
        x = end + gap
    write(path, x - gap + 4, base + 32, body, about)


# the table of names (»Was im Namen steckt«): Kuchenverkauf in each
TABLE = [
    ('open-sans', 'Open Sans:style=Regular', 'Kuchenverkauf'),
    ('ibm-plex-sans', 'IBM Plex Sans:style=Regular', 'Kuchenverkauf'),
    ('xcharter', 'XCharter:style=Roman', 'Kuchenverkauf'),
    ('dejavu-serif', 'DejaVu Serif:style=Book', 'Kuchenverkauf'),
    ('space-grotesk', 'Space Grotesk', 'Kuchenverkauf'),
    ('didact-gothic', 'Didact Gothic:style=Regular', 'Kuchenverkauf'),
    ('dejavu-sans-mono', 'DejaVu Sans Mono:style=Book', 'Kuchenverkauf'),
    ('roboto-slab', 'Roboto Slab:style=Regular', 'Kuchenverkauf'),
    # a Nerd Font: the letters and symbols for the terminal (a folder, a
    # branch of Git, Python)
    ('nerd-font', '0xProto Nerd Font Mono:style=Regular', '\uf07b kuchen  \ue725 main  \ue606'),
]

def main():
    folder = CONTENT / 'character-formatting' / 'schriften'
    folder.mkdir(exist_ok=True)
    for slug, pattern, s in TABLE:
        sample(folder / f'{slug}.svg', pattern, s)
    personen = CONTENT / 'reference' / 'personen'
    names(personen / 'morison-schrift.svg', [('Times New Roman:style=Regular', 'Times New Roman', 'mit Serifen, 1932')],
          'Times New Roman, in ihrer eigenen Schrift')
    names(personen / 'carter-schriften.svg', [('Georgia:style=Regular', 'Georgia', 'mit Serifen, 1993'),
                                              ('Verdana:style=Regular', 'Verdana', 'ohne Serifen, 1996'),
                                              ('XCharter:style=Roman', 'Charter', 'mit Serifen, 1987')],
          'Georgia, Verdana und Charter, jede in ihrer eigenen Schrift')


if __name__ == '__main__':
    main()
