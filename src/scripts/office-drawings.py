#!/usr/bin/env python3
"""The drawings of the word processing sheets (section Office,
docs/plan-textverarbeitung.md): what the sheets explain in words and a
drawing shows at a glance. Sheets of paper stay white in both modes, with
their own colours (render-svg.rb, »on a sheet of paper«); labels beside them
take the site's diagram palette.

    src/scripts/drawings python3 src/scripts/office-drawings.py [NAME …]
    for f in …; do src/scripts/render-svg.rb $f; done

Each drawing is written as SVG into its sheet's folder; render-svg.rb then
embeds the fonts and writes the dark version.
"""

# runs in docker/drawings, the same on every machine (src/scripts/drawings)
import os as _os, sys as _sys  # noqa: E401
_sys.path.insert(0, _os.path.dirname(_os.path.abspath(__file__)))
import drawings_container  # noqa: E402
drawings_container.ensure()

import io  # noqa: E402
import sys  # noqa: E402
from pathlib import Path  # noqa: E402
from xml.sax.saxutils import escape  # noqa: E402

from fontTools.pens.boundsPen import BoundsPen  # noqa: E402
from fontTools.pens.recordingPen import RecordingPen  # noqa: E402
from fontTools.pens.svgPathPen import SVGPathPen  # noqa: E402
from fontTools.ttLib import TTFont  # noqa: E402
from PIL import ImageFont  # noqa: E402

REPO = Path(__file__).resolve().parent.parent.parent
CONTENT = REPO / 'src' / 'content'
FONTS = REPO / 'src' / 'static' / 'include' / 'fonts'

SANS = "'IBM Plex Sans', 'DejaVu Sans', sans-serif"
SERIF = "'XCharter', 'DejaVu Serif', serif"
# the site's diagram palette (render-svg.rb), beside the sheets
BG, TEXT, BLUE, GREY, AMBER, RED = '#ffffff', '#1f2937', '#2b5fb4', '#6b7280', '#d97706', '#dc2626'
# on a sheet of paper, the same in both modes
PAPER, INK, PBLUE, PGREEN, BAR, EDGE = '#fdfdfd', '#1b1f24', '#2457a6', '#2e6b3a', '#9aa3ae', '#c9ced6'
PAMBER, PAMBER_BG, PBLUE_BG, FIELD = '#b45309', '#fdecc8', '#e3ecfa', '#e6e8eb'


def _ttf(path):
    font = TTFont(str(path))
    font.flavor = None
    out = io.BytesIO()
    font.save(out)
    out.seek(0)
    return out


_FONT_FILES = {
    ('sans', '400'): FONTS / 'IBM_Plex_Sans' / 'IBMPlexSans-Regular.ttf',
    ('sans', '600'): FONTS / 'IBM_Plex_Sans' / 'IBMPlexSans-SemiBold.ttf',
    ('sans', '700'): FONTS / 'IBM_Plex_Sans' / 'IBMPlexSans-Bold.ttf',
    ('serif', '400'): FONTS / 'XCharter' / 'XCharter-Roman.woff2',
}
_fonts = {}


def measure(text, size, weight='400', family='sans'):
    key = (family, weight, size)
    if key not in _fonts:
        path = _FONT_FILES[(family, weight)]
        source = _ttf(path) if path.suffix == '.woff2' else str(path)
        _fonts[key] = ImageFont.truetype(source, size * 4)
    return _fonts[key].getlength(text) / 4


def text(x, y, s, size=15, colour=TEXT, anchor='start', weight='400', family=SANS, extra=''):
    return (f"<text x='{x:.1f}' y='{y:.1f}' text-anchor='{anchor}' font-family=\"{family}\" font-size='{size}' "
            f"font-weight='{weight}' fill='{colour}'{extra}>{escape(s)}</text>")


def rect(x, y, w, h, fill, stroke='none', width=1, r=0, extra=''):
    return (f"<rect x='{x:.1f}' y='{y:.1f}' width='{w:.1f}' height='{h:.1f}' rx='{r}' fill='{fill}' "
            f"stroke='{stroke}' stroke-width='{width}'{extra}/>")


def line(x1, y1, x2, y2, colour, width=1.5, extra=''):
    return f"<line x1='{x1:.1f}' y1='{y1:.1f}' x2='{x2:.1f}' y2='{y2:.1f}' stroke='{colour}' stroke-width='{width}'{extra}/>"


def arrow(x1, y1, x2, y2, colour=GREY, width=1.5, head=9, dashed=False):
    """a straight arrow, its head at (x2, y2)"""
    import math
    a = math.atan2(y2 - y1, x2 - x1)
    bx, by = x2 - head * math.cos(a), y2 - head * math.sin(a)
    px, py = -math.sin(a) * head * 0.42, math.cos(a) * head * 0.42
    dash = " stroke-dasharray='5 4'" if dashed else ''
    return (line(x1, y1, bx, by, colour, width, dash) +
            f"<polygon points='{x2:.1f},{y2:.1f} {bx + px:.1f},{by + py:.1f} {bx - px:.1f},{by - py:.1f}' fill='{colour}'/>")


def paper(x, y, w, h):
    return rect(x, y, w, h, PAPER, EDGE, 1.2, 3)


def bar(x, y, w, colour=BAR, h=5):
    """a line of text, as a grey bar"""
    return rect(x, y - h, w, h, colour, r=2)


def svg(name, folder, w, h, body, about):
    out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {w} {h}' width='{w}' height='{h}'>",
           f"  <!-- {about}; made by src/scripts/office-drawings.py, then src/scripts/render-svg.rb -->",
           rect(0, 0, w, h, BG)]
    out += body
    out.append('</svg>')
    path = CONTENT / folder / f'{name}.svg'
    path.write_text('\n'.join(out) + '\n', encoding='utf-8')
    print(path.relative_to(REPO))


_serif_font = None


def serif_text(x, y, s, size, colour, anchor='start'):
    """text in XCharter as outlines: svg_fonts.py embeds only the site's
    fonts it knows, and a serif must look the same everywhere"""
    global _serif_font
    if _serif_font is None:
        _serif_font = TTFont(_ttf(FONTS / 'XCharter' / 'XCharter-Roman.woff2'))
    glyphs, cmap = _serif_font.getGlyphSet(), _serif_font.getBestCmap()
    scale = size / _serif_font['head'].unitsPerEm
    width = sum(glyphs[cmap[ord(c)]].width for c in s) * scale
    if anchor == 'end':
        x -= width
    paths = []
    for c in s:
        name = cmap[ord(c)]
        pen = SVGPathPen(glyphs)
        glyphs[name].draw(pen)
        if pen.getCommands():
            paths.append(f"<path transform='translate({x:.2f} {y:.2f}) scale({scale:.5f} {-scale:.5f})' d='{pen.getCommands()}'/>")
        x += glyphs[name].width * scale
    return f"<g fill='{colour}'>{''.join(paths)}</g>", width


def glyph_path(char, family_file, size, x, y):
    """a letter as a path (its outline), its origin at (x, y)"""
    font = TTFont(str(family_file))
    glyphs = font.getGlyphSet()
    name = font.getBestCmap()[ord(char)]
    pen = SVGPathPen(glyphs)
    glyphs[name].draw(pen)
    scale = size / font['head'].unitsPerEm
    return pen.getCommands(), scale, glyphs[name].width * scale


# ---- Was das Auge nicht sieht

def abstand():
    """an empty paragraph against spacing after a paragraph"""
    w, h = 560, 236
    body = []
    for i, (title, spaced) in enumerate([('mit einem leeren Absatz', False), ('mit Abstand danach', True)]):
        x = 18 + i * 280
        body.append(text(x + 120, 24, title, 15, TEXT, 'middle', '600'))
        body.append(paper(x, 38, 244, 182))
        tx, y = x + 22, 76
        body.append(text(tx, y, 'Liebe Eltern,', 15, INK))
        mark_x = tx + measure('Liebe Eltern,', 15) + 2
        body.append(text(mark_x, y, '¶', 15, PBLUE))
        if spaced:
            # the paragraph and its spacing below it, as one (behind the text)
            body.insert(-2, rect(tx - 6, y - 16, 124, 16 + 6 + 30, PBLUE_BG, r=3))
            gy1, gy2 = y + 6, y + 36
            bx = tx + 128
            body.append(line(bx, gy1, bx + 8, gy1, PAMBER, 1.5) + line(bx + 8, gy1, bx + 8, gy2, PAMBER, 1.5) +
                        line(bx, gy2, bx + 8, gy2, PAMBER, 1.5))
            body.append(text(bx + 14, (gy1 + gy2) / 2 + 5, '12 pt', 14, PAMBER, weight='600'))
            y = gy2 + 16
        else:
            y += 26
            body.append(rect(tx - 6, y - 16, 30, 22, PAMBER_BG, r=3))
            body.append(text(tx, y, '¶', 15, PBLUE))
            body.append(text(tx + 30, y, 'leerer Absatz', 14, PAMBER, weight='600'))
            y += 26
        for k, length in enumerate([196, 188, 120]):
            body.append(bar(tx, y + k * 22, length))
        body.append(text(tx + 124, y + 2 * 22 + 1, '¶', 15, PBLUE))
    svg('abstand', 'formatting-marks', w, h, body,
        'Ein leerer Absatz gegen Abstand danach')


# ---- Schrift mit Absicht

def serifen():
    """the same word with and without serifs, the serifs marked"""
    w, h = 560, 214
    size = 92
    body = []
    serif_file = _ttf(FONTS / 'XCharter' / 'XCharter-Roman.woff2')
    serif_font = TTFont(serif_file)
    for i, (label, names, family) in enumerate([('mit Serifen', 'Times New Roman, Georgia', 'serif'),
                                                ('ohne Serifen', 'Arial, Verdana, Calibri', 'sans')]):
        cx = 140 + i * 280
        word = 'Hand'
        font = serif_font if family == 'serif' else TTFont(str(FONTS / 'IBM_Plex_Sans' / 'IBMPlexSans-Regular.ttf'))
        file_size = size
        glyphs = font.getGlyphSet()
        cmap = font.getBestCmap()
        scale = file_size / font['head'].unitsPerEm
        total = sum(glyphs[cmap[ord(c)]].width for c in word) * scale
        x = cx - total / 2
        base = 128
        paths, serifs = [], []
        for c in word:
            name = cmap[ord(c)]
            pen = SVGPathPen(glyphs)
            glyphs[name].draw(pen)
            paths.append(f"<path transform='translate({x:.2f} {base}) scale({scale:.5f} {-scale:.5f})' d='{pen.getCommands()}'/>")
            if family == 'serif':
                # a serif ends in a flat edge: a straight horizontal piece
                # of the outline at the baseline or at the top of a stem
                record = RecordingPen()
                glyphs[name].draw(record)
                # (the last edge of an outline is the line that closes
                # it; a serif at the top of n or d is slanted)
                last = start = None
                for op, pts in record.value:
                    if op == 'moveTo':
                        last = start = pts[-1]
                        continue
                    point = start if op in ('closePath', 'endPath') else pts[-1]
                    straight = op in ('lineTo', 'closePath', 'endPath')
                    if straight and last and point and abs(point[0] - last[0]) > 40 and abs(point[1] - last[1]) < 0.3 * abs(point[0] - last[0]):
                        y = (point[1] + last[1]) / 2
                        if abs(y) < 2 or y > font['OS/2'].sxHeight - 40:
                            serifs.append((x + (point[0] + last[0]) / 2 * scale, base - y * scale, abs(point[0] - last[0]) * scale))
                    last = point
            x += glyphs[name].width * scale
        body.append(f"<g fill='{INK}'>{''.join(paths)}</g>")
        # one ring per serif (its upper and lower edge are both found)
        kept = []
        for found in sorted(serifs, key=lambda f: -f[2]):
            if all(abs(found[0] - k[0]) > 12 or abs(found[1] - k[1]) > 16 for k in kept):
                kept.append(found)
        for sx, sy, sw in kept:
            body.append(f"<ellipse cx='{sx:.1f}' cy='{sy:.1f}' rx='{sw / 2 + 5:.1f}' ry='7' fill='none' stroke='{BLUE}' stroke-width='1.8'/>")
        body.append(text(cx, 168, label, 16, TEXT, 'middle', '600'))
        body.append(text(cx, 190, names, 14, GREY, 'middle'))
    # what the blue is
    body.append(text(140, 26, 'Serifen: die Füßchen an den Enden', 14, BLUE, 'middle'))
    svg('serifen', 'character-formatting', w, h, body, 'Hand mit und ohne Serifen')


def hervorheben():
    """underlining across descenders, and the shape of a word in capitals"""
    w, h = 560, 262
    body = []
    # underlined: the line runs through g, p and y
    word, size = 'Gruppe gegen Typen', 30
    x0, base = 24, 54
    body.append(text(x0, base, word, size, TEXT))
    under = base + 4
    body.append(line(x0, under, x0 + measure(word, size), under, TEXT, 1.8))
    x = x0
    for c in word:
        cw = measure(c, size)
        if c in 'gpy':
            body.append(f"<circle cx='{x + cw / 2:.1f}' cy='{under + 1:.1f}' r='7' fill='none' stroke='{RED}' stroke-width='1.6'/>")
        x += cw
    body.append(text(x0 + measure(word, size) + 18, base - 4, 'unterstrichen:', 14, GREY))
    body.append(text(x0 + measure(word, size) + 18, base + 14, 'die Linie schneidet g, p, y', 14, GREY))
    # the shape of a word: lower case has one, capitals are a rectangle
    size = 32
    up = set('bdfhklt') | set('ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ')
    down = set('gjpqy')
    for row, (word, notes) in enumerate([('Kuchenverkauf', ('eine Form, an der', 'man das Wort erkennt')),
                                         ('KUCHENVERKAUF', ('jedes Wort', 'ein Rechteck'))]):
        base = 150 + row * 86
        body.append(text(x0, base, word, size, TEXT))
        # the outline: the height of each letter, a step at each change
        asc, xh, desc = base - size * 0.73, base - size * 0.53, base + size * 0.24
        top, bottom = [], []
        x = x0
        for c in word:
            cw = measure(c, size)
            top.append((x, x + cw, asc if c in up else xh))
            bottom.append((x, x + cw, desc if c in down else base + 2))
            x += cw
        pts = []
        for a, b, y in top:
            pts += [(a, y - 3), (b, y - 3)]
        for a, b, y in reversed(bottom):
            pts += [(b, y + 3), (a, y + 3)]
        d = 'M ' + ' L '.join(f'{px:.1f} {py:.1f}' for px, py in pts) + ' Z'
        body.append(f"<path d='{d}' fill='none' stroke='{BLUE}' stroke-width='1.8' stroke-linejoin='round'/>")
        body.append(text(x + 18, base - 6, notes[0], 14, GREY))
        body.append(text(x + 18, base + 12, notes[1], 14, GREY))
    svg('hervorheben', 'character-formatting', w, h, body,
        'Unterstreichung durch die Unterlängen, und die Form eines Worts in Klein- und Großbuchstaben')


# ---- Listen und Tabstopps

def liste():
    """a list typed with dashes against one the program keeps"""
    w, h = 560, 220
    body = []
    items = ['Schlafsack', 'Regenjacke, falls es am Mittwoch regnet', 'Taschenlampe']
    for i, (title, typed) in enumerate([('getippt', True), ('eine Liste des Programms', False)]):
        x = 18 + i * 280
        body.append(text(x + 122, 24, title, 15, TEXT, 'middle', '600'))
        body.append(paper(x, 38, 244, 166))
        tx, y = x + 18, 70
        indent = tx + 18
        for item in items:
            words = item.split(' ')
            # break the second item after »es«, as a narrow column would
            lines = [' '.join(words[:3]), ' '.join(words[3:])] if len(words) > 3 else [item]
            if typed:
                body.append(text(tx, y, '- ' + lines[0], 14, INK))
                for rest in lines[1:]:
                    y += 21
                    body.append(text(tx, y, rest, 14, INK))
            else:
                body.append(text(tx + 3, y, '•', 14, INK))
                body.append(text(indent, y, lines[0], 14, INK))
                for rest in lines[1:]:
                    y += 21
                    body.append(text(indent, y, rest, 14, INK))
            y += 27
        if typed:
            body.append(line(tx - 3, 52, tx - 3, 176, PAMBER, 1.2, " stroke-dasharray='3 3'"))
            body.append(text(tx - 3, 190, 'die zweite Zeile beginnt am Rand', 13, PAMBER))
        else:
            body.append(line(indent - 3, 52, indent - 3, 176, PBLUE, 1.2, " stroke-dasharray='3 3'"))
            body.append(text(indent - 3, 190, 'jede Zeile beginnt hier', 13, PBLUE))
    svg('liste', 'lists-and-tabs', w, h, body, 'Eine getippte Liste gegen eine Liste des Programms')


def tabstopp():
    """the ruler with a right-aligned tab stop; one tab jumps to it, in two fonts"""
    w, h = 560, 222
    body = []
    x0, x1 = 30, 530         # the text area: 0 to 16 cm
    cm = (x1 - x0) / 16
    # the ruler
    body.append(rect(x0 - 10, 26, x1 - x0 + 20, 26, '#f3f4f6', '#d1d5db', 1, 2))
    for c in range(0, 17):
        x = x0 + c * cm
        body.append(line(x, 44, x, 52, GREY, 1))
        if c and c % 2 == 0 and c < 16:
            body.append(text(x, 41, str(c), 11, GREY, 'middle'))
    # the right tab stop at 16 cm: ┘
    stop = x1
    body.append(f"<path d='M {stop - 9} 50 L {stop} 50 L {stop} 40' fill='none' stroke='{BLUE}' stroke-width='2.6'/>")
    body.append(text(stop - 12, 18, 'rechtsbündiger Tabstopp bei 16 cm', 13, BLUE, 'end'))
    body.append(line(stop, 58, stop, 196, BLUE, 1.2, " stroke-dasharray='4 4'"))
    body.append(paper(x0 - 18, 64, x1 - x0 + 36, 104))
    rows = [('Busfahrt hin und zurück', '48,00 €', SANS, 'sans'), ('Jugendherberge', '156,00 €', SERIF, 'serif')]
    for k, (item, price, family, key) in enumerate(rows):
        y = 100 + k * 40
        if key == 'serif':
            drawn, iw = serif_text(x0, y, item, 16, INK)
            body.append(drawn)
            end = x0 + iw
        else:
            body.append(text(x0, y, item, 16, INK, family=family))
            end = x0 + measure(item, 16)
        # the tab: its mark, then the fill to the price
        body.append(text(end + 6, y, '→', 15, PBLUE))
        pw = serif_text(0, 0, price, 16, INK)[1] if key == 'serif' else measure(price, 16)
        dots_from, dots_to = end + 26, stop - pw - 6
        n = int((dots_to - dots_from) / 6)
        body.append(text(dots_from, y, '.' * n, 16, INK, family=SANS, extra=" letter-spacing='1.6'"))
        body.append(serif_text(stop, y, price, 16, INK, 'end')[0] if key == 'serif' else text(stop, y, price, 16, INK, 'end'))
    body.append(text(x0, 194, 'ein Tab (→) springt zum Tabstopp; die Punkte sind das Füllzeichen', 13, GREY))
    body.append(text(x0, 212, 'jeder Preis endet am Tabstopp, in jeder Schrift', 13, GREY))
    svg('tabstopp', 'lists-and-tabs', w, h, body, 'Ein rechtsbündiger Tabstopp mit Füllzeichen')


# ---- Tabellen

def zelle():
    """a table: a cell, a row, a column, and a long entry that makes its row taller"""
    w, h = 560, 232
    body = []
    x0, y0 = 30, 34
    cols = [92, 62, 200]
    rows = [30, 30, 50, 30]
    xs = [x0]
    for c in cols:
        xs.append(xs[-1] + c)
    ys = [y0]
    for r in rows:
        ys.append(ys[-1] + r)
    body.append(paper(x0 - 14, y0 - 14, sum(cols) + 28, sum(rows) + 28))
    # the column and the row, behind
    body.append(rect(xs[1], ys[0], cols[1], sum(rows), PAMBER_BG))
    body.append(rect(xs[0], ys[3], sum(cols), rows[3], PBLUE_BG))
    data = [('Tag', 'Zeit', 'Programm'), ('Montag', '14:00', 'Anreise mit dem Bus'),
            ('Mittwoch', '9:00', ['Wanderung um den See, mit', 'Picknick am Aussichtsturm']), ('Donnerstag', '10:00', 'Kletterwald')]
    for r, row in enumerate(data):
        for c, cell in enumerate(row):
            lines = cell if isinstance(cell, list) else [cell]
            for k, t in enumerate(lines):
                body.append(text(xs[c] + 7, ys[r] + 20 + k * 20, t, 14, INK, weight='700' if r == 0 else '400'))
    for x in xs:
        body.append(line(x, ys[0], x, ys[-1], EDGE, 1.2))
    for y in ys:
        body.append(line(xs[0], y, xs[-1], y, EDGE, 1.2))
    # the cell
    body.append(rect(xs[2] + 1, ys[2] + 1, cols[2] - 2, rows[2] - 2, 'none', PAMBER, 2.2))
    lx = xs[-1] + 30
    body.append(arrow(lx - 4, ys[2] + 12, xs[-1] - 8, ys[2] + 18, PAMBER))
    body.append(text(lx, ys[2] + 10, 'eine Zelle:', 13, TEXT, weight='600'))
    body.append(text(lx, ys[2] + 27, 'wird der Eintrag länger,', 13, GREY))
    body.append(text(lx, ys[2] + 43, 'wird sie höher', 13, GREY))
    body.append(arrow(lx - 4, ys[3] + 20, xs[-1] + 4, ys[3] + 16, BLUE))
    body.append(text(lx, ys[3] + 24, 'eine Zeile', 13, TEXT, weight='600'))
    body.append(arrow(xs[1] + cols[1] / 2 + 30, ys[-1] + 34, xs[1] + cols[1] / 2 + 4, ys[-1] + 4, AMBER))
    body.append(text(xs[1] + cols[1] / 2 + 34, ys[-1] + 38, 'eine Spalte', 13, TEXT, weight='600'))
    svg('zelle', 'word-tables', w, h, body, 'Eine Tabelle mit Zelle, Zeile und Spalte')


def kopfzeile():
    """a table over two pages, its header row on both"""
    w, h = 560, 252
    body = []
    pw, ph = 200, 226
    days = ['Montag', 'Dienstag', 'Mittwoch', 'Donnerstag', 'Freitag']
    for p in range(2):
        x, y = 40 + p * 270, 12
        body.append(paper(x, y, pw, ph))
        tx, tw = x + 20, pw - 40
        if p == 0:
            for k in range(5):
                body.append(bar(tx, y + 30 + k * 14, tw - (k % 3) * 22))
            ty = y + 108
            shown = days[:3]
        else:
            ty = y + 24
            shown = days[3:]
        # the header row, then the rows
        body.append(rect(tx, ty, tw, 22, PBLUE_BG))
        body.append(text(tx + 6, ty + 16, 'Tag', 12, INK, weight='700'))
        body.append(text(tx + 80, ty + 16, 'Programm', 12, INK, weight='700'))
        for k, day in enumerate(shown):
            ry = ty + 22 + k * 22
            body.append(text(tx + 6, ry + 16, day, 12, INK))
            body.append(bar(tx + 80, ry + 13, 64 - (k % 2) * 16, h=4))
        rows = len(shown) + 1
        for k in range(rows + 1):
            body.append(line(tx, ty + k * 22, tx + tw, ty + k * 22, EDGE, 1))
        body.append(line(tx, ty, tx, ty + rows * 22, EDGE, 1) + line(tx + 74, ty, tx + 74, ty + rows * 22, EDGE, 1) +
                    line(tx + tw, ty, tx + tw, ty + rows * 22, EDGE, 1))
        if p == 1:
            body.append(rect(tx - 3, ty - 3, tw + 6, 28, 'none', PAMBER, 2.2, 3))
            body.append(text(tx, ty + 128, 'die Kopfzeile', 13, TEXT, weight='600'))
            body.append(text(tx, ty + 145, 'steht wieder oben', 13, GREY))
            body.append(arrow(tx + tw + 12, ty + 118, tx + tw + 8, ty + 30, PAMBER))
    body.append(text(253, 130, '→', 26, GREY, 'middle'))
    svg('kopfzeile', 'word-tables', w, h, body, 'Eine Tabelle über zwei Seiten, die Kopfzeile auf beiden')


# ---- Bilder im Text

def _landscape(x, y, w, h):
    """a picture: sky, a hill and the sun (a circle that shows when it is stretched)"""
    r = min(w, h) * 0.16
    sun = (f"<ellipse cx='{x + w * 0.72:.1f}' cy='{y + h * 0.32:.1f}' rx='{h * 0.16 * (w / h) / 1.5:.1f}' ry='{h * 0.16:.1f}' "
           f"fill='{PAMBER}'/>")
    hill = (f"<path d='M {x:.1f} {y + h:.1f} L {x:.1f} {y + h * 0.7:.1f} Q {x + w * 0.35:.1f} {y + h * 0.35:.1f} "
            f"{x + w:.1f} {y + h * 0.78:.1f} L {x + w:.1f} {y + h:.1f} Z' fill='{PGREEN}'/>")
    return rect(x, y, w, h, PBLUE_BG) + sun + hill + rect(x, y, w, h, 'none', EDGE, 1)


def _handles(x, y, w, h, active):
    out = []
    for hx, hy, name in [(x, y, 'tl'), (x + w / 2, y, 't'), (x + w, y, 'tr'), (x, y + h / 2, 'l'), (x + w, y + h / 2, 'r'),
                         (x, y + h, 'bl'), (x + w / 2, y + h, 'b'), (x + w, y + h, 'br')]:
        colour = PAMBER if name == active else PAPER
        out.append(rect(hx - 4, hy - 4, 8, 8, colour, PBLUE, 1.4))
    return ''.join(out)


def seitenverhaeltnis():
    """a picture 3 to 2, pulled at a side (distorted) and at a corner (larger, the same shape)"""
    w, h = 560, 200
    body = []
    base = 150
    # the original
    ow, oh = 108, 72
    x = 22
    body.append(_landscape(x, base - oh, ow, oh) + _handles(x, base - oh, ow, oh, ''))
    body.append(text(x + ow / 2, base + 26, 'das Bild, 3 zu 2', 13, TEXT, 'middle', '600'))
    # pulled at the side
    x = 172
    sw, sh = 168, 72
    body.append(rect(x, base - oh, ow, oh, 'none', GREY, 1, extra=" stroke-dasharray='3 3'"))
    body.append(_landscape(x, base - sh, sw, sh) + _handles(x, base - sh, sw, sh, 'r'))
    body.append(arrow(x + ow + 6, base - sh / 2 - 22, x + sw - 8, base - sh / 2 - 22, PAMBER))
    body.append(text(x + sw / 2, base + 26, 'an der Seite gezogen:', 13, TEXT, 'middle', '600'))
    body.append(text(x + sw / 2, base + 43, 'nur breiter, verzerrt', 13, GREY, 'middle'))
    # pulled at the corner
    x = 380
    cw, ch = 156, 104
    body.append(rect(x, base - oh, ow, oh, 'none', GREY, 1, extra=" stroke-dasharray='3 3'"))
    body.append(_landscape(x, base - ch, cw, ch) + _handles(x, base - ch, cw, ch, 'tr'))
    body.append(arrow(x + ow + 2, base - oh - 2, x + cw - 8, base - ch + 8, PAMBER))
    body.append(text(x + cw / 2, base + 26, 'an der Ecke gezogen:', 13, TEXT, 'middle', '600'))
    body.append(text(x + cw / 2, base + 43, 'größer, dieselbe Form', 13, GREY, 'middle'))
    svg('seitenverhaeltnis', 'pictures-in-text', w, h, body,
        'Ein Bild an der Seite und an der Ecke gezogen')


def umbruch():
    """a picture in the line, against text that flows around it"""
    w, h = 560, 250
    body = []
    for i, (title, around) in enumerate([('in der Zeile', False), ('Text fließt herum', True)]):
        x = 18 + i * 280
        body.append(text(x + 122, 24, title, 15, TEXT, 'middle', '600'))
        body.append(paper(x, 38, 244, 196))
        tx, tw = x + 18, 208
        y = 62
        for k in range(2):
            body.append(bar(tx, y, tw - (k % 2) * 30))
            y += 14
        if around:
            pw, ph = 92, 62
            px = tx + tw - pw
            body.append(_landscape(px, y - 6, pw, ph))
            ly = y
            for k in range(5):
                body.append(bar(tx, ly + 2, tw - pw - 12 - (k % 2) * 14))
                ly += 14
            for k in range(4):
                body.append(bar(tx, ly + 2, tw - (k % 3) * 20))
                ly += 14
        else:
            pw, ph = 92, 62
            # the picture stands on the line like a big letter, a word beside it
            body.append(_landscape(tx, y - 4, pw, ph))
            body.append(bar(tx + pw + 6, y + ph - 4, 40))
            body.append(rect(tx + pw + 52, y - 4, tw - pw - 52, ph, PAMBER_BG, r=3, extra=" fill-opacity='0.8'"))
            body.append(text(tx + pw + 52 + (tw - pw - 52) / 2, y + ph / 2, 'leer', 13, PAMBER, 'middle', '600'))
            ly = y + ph + 14
            for k in range(3):
                body.append(bar(tx, ly, tw - (k % 3) * 20))
                ly += 14
    svg('umbruch', 'pictures-in-text', w, h, body, 'Ein Bild in der Zeile gegen Text, der um das Bild fließt')


# ---- Formatvorlagen

def vorlage():
    """one style, many headings: change the style, and every heading changes; by hand wins"""
    w, h = 560, 268
    body = []
    # the style
    sx, sy = 16, 88
    body.append(rect(sx, sy, 170, 92, '#eef3fb', BLUE, 1.4, 6))
    body.append(text(sx + 12, sy + 24, 'Formatvorlage', 12, GREY))
    body.append(text(sx + 12, sy + 46, 'Überschrift 1', 16, TEXT, weight='600'))
    body.append(text(sx + 12, sy + 68, 'grün, 16 pt, fett', 13, TEXT))
    # the sheet with its headings
    px, py = 262, 14
    body.append(paper(px, py, 282, 240))
    heads = [('Montag: Anreise', PGREEN, False), ('Dienstag: Kletterwald', PGREEN, False), ('Mittwoch: Wanderung', PBLUE, True)]
    y = py + 36
    targets = []
    for title, colour, by_hand in heads:
        body.append(text(px + 18, y, title, 16, colour, weight='700'))
        targets.append((px + 12, y - 6, by_hand))
        if by_hand:
            hw = measure(title, 16, '700')
            body.append(text(px + 24 + hw, y - 2, 'blau von Hand', 12, PAMBER, weight='600'))
        y += 16
        for k in range(2):
            body.append(bar(px + 18, y + 6, 236 - k * 50))
            y += 14
        y += 32
    for tx, ty, by_hand in targets:
        body.append(arrow(sx + 172, sy + 46, tx - 2, ty, AMBER if by_hand else BLUE, 1.5, dashed=by_hand))
    body.append(text(sx, sy + 120, 'Ändert sich die Vorlage,', 13, GREY))
    body.append(text(sx, sy + 137, 'ändert sich jede Überschrift', 13, GREY))
    body.append(text(sx, sy + 154, 'mit ihr, außer dort, wo etwas', 13, GREY))
    body.append(text(sx, sy + 171, 'von Hand gesetzt ist.', 13, GREY))
    svg('vorlage', 'paragraph-styles', w, h, body, 'Eine Formatvorlage für viele Überschriften, Formatierung von Hand gewinnt')


# ---- Seiten und Verzeichnisse

def verzeichnis():
    """the table of contents comes from the headings and their pages"""
    w, h = 560, 262
    body = []
    # the title page with the contents
    tx0, ty0 = 14, 14
    body.append(paper(tx0, ty0, 236, 234))
    body.append(text(tx0 + 20, ty0 + 40, 'Reisebericht der 7b', 17, PGREEN, weight='700'))
    body.append(text(tx0 + 20, ty0 + 78, 'Inhalt', 14, INK, weight='700'))
    entries = [('Montag: Anreise', 2), ('Dienstag: Kletterwald', 3), ('Mittwoch: Wanderung', 4)]
    lines_y = []
    for k, (title, page) in enumerate(entries):
        y = ty0 + 106 + k * 26
        body.append(text(tx0 + 20, y, title, 12, INK))
        end = tx0 + 20 + measure(title, 12)
        right = tx0 + 214
        n = int((right - 14 - end) / 4.2)
        body.append(text(end + 3, y, '.' * max(n, 0), 12, INK, extra=" letter-spacing='1'"))
        body.append(text(right, y, str(page), 12, INK, 'end'))
        lines_y.append(y)
    # the pages of the days
    for k, (title, page) in enumerate(entries):
        px, py = 364, 10 + k * 84
        body.append(paper(px, py, 182, 76))
        body.append(text(px + 12, py + 22, title, 11, PGREEN, weight='700'))
        for j in range(2):
            body.append(bar(px + 12, py + 38 + j * 11, 140 - j * 30, h=4))
        body.append(text(px + 91, py + 68, str(page), 10, INK, 'middle'))
        body.append(arrow(px - 4, py + 18, tx0 + 240, lines_y[k] - 4, BLUE, 1.4))
    body.append(text(tx0 + 20, ty0 + 206, 'aus den Überschriften', 12, BLUE))
    body.append(text(tx0 + 20, ty0 + 222, 'und den Seiten, auf denen sie stehen', 12, BLUE))
    svg('verzeichnis', 'pages-and-contents', w, h, body, 'Das Inhaltsverzeichnis entsteht aus den Überschriften und ihren Seiten')


def fusszeile():
    """a typed page number against a field in the footer"""
    w, h = 560, 290
    body = []
    for row, (title, field) in enumerate([('getippt', False), ('ein Feld', True)]):
        y0 = 12 + row * 142
        body.append(text(16, y0 + 44, title, 15, TEXT, weight='600'))
        for p in range(3):
            x = 110 + p * 148
            body.append(paper(x, y0, 132, 104))
            for j in range(4):
                body.append(bar(x + 12, y0 + 20 + j * 12, 108 - (j + p) % 3 * 16, h=4))
            # the footer, the same on every page
            body.append(line(x + 8, y0 + 76, x + 124, y0 + 76, EDGE, 1, " stroke-dasharray='3 3'"))
            label = 'Seite '
            lw = measure(label, 12)
            fx = x + 66 - (lw + 12) / 2
            body.append(text(fx, y0 + 94, label, 12, INK))
            number = str(p + 1) if field else '1'
            if field:
                body.append(rect(fx + lw - 1, y0 + 82, 12, 16, FIELD, r=2))
            body.append(text(fx + lw + 5, y0 + 94, number, 12, PAMBER if not field and p else INK, 'middle',
                             weight='700' if not field and p else '400'))
        note = ('auf jeder Seite »Seite 1«' if not field else 'auf jeder Seite ihre eigene Zahl')
        body.append(text(110, y0 + 124, note, 13, GREY))
    svg('fusszeile', 'headers-and-footnotes', w, h, body, 'Die Seitenzahl getippt und als Feld in der Fußzeile')



# ---- Kopfzeile, Fußzeile, Fußnoten

def seite():
    """a page and its parts: the header, the text with a footnote's number,
    the footnote above the footer, the footer"""
    w, h = 420, 430
    body = []
    px, py, pw, ph = 16, 14, 272, 400
    body.append(paper(px, py, pw, ph))
    tx, tw = px + 22, pw - 44
    # the header: its own area above the text
    body.append(rect(tx - 6, py + 12, tw + 12, 30, PBLUE_BG, r=3))
    body.append(text(tx, py + 32, 'Reisebericht der 7b', 11, INK))
    body.append(text(tx + tw, py + 32, 'Stand: 12.11.2026', 11, INK, 'end'))
    # the text
    y = py + 74
    body.append(text(tx, y, 'Dienstag: Kletterwald', 14, PGREEN, weight='700'))
    y += 22
    for k in range(3):
        body.append(bar(tx, y, tw - (k % 2) * 30))
        y += 14
    body.append(text(tx, y + 2, 'Immer ein Karabiner', 11, INK))
    kw = measure('Immer ein Karabiner', 11)
    body.append(text(tx + kw + 1, y - 3, '1', 8, PAMBER, weight='700'))
    body.append(bar(tx + kw + 10, y, tw - kw - 10))
    y += 14
    for k in range(4):
        body.append(bar(tx, y, tw - (k % 3) * 24))
        y += 14
    # the footnote, above the footer, under a short line
    fy = py + ph - 76
    body.append(line(tx, fy, tx + 70, fy, INK, 0.8))
    body.append(rect(tx - 6, fy + 5, tw + 12, 26, PAMBER_BG, r=3))
    body.append(text(tx, fy + 22, '1', 8, PAMBER, weight='700'))
    body.append(text(tx + 7, fy + 25, 'Ein Karabiner ist ein Haken mit …', 10, INK))
    # the footer
    body.append(rect(tx - 6, py + ph - 40, tw + 12, 28, PBLUE_BG, r=3))
    body.append(text(px + pw / 2, py + ph - 21, 'Seite 3', 11, INK, 'middle'))
    # what each part is
    lx = px + pw + 22
    # (the footnote's label a little higher, above its two lines of note,
    # so that the footer's label has its own place below)
    for label, at, ly, colour in [('Kopfzeile', py + 27, py + 27, BLUE), ('Text', py + 150, py + 150, GREY),
                                  ('Fußnote', fy + 18, fy - 34, AMBER), ('Fußzeile', py + ph - 26, py + ph - 26, BLUE)]:
        body.append(line(px + pw - 8, at, lx - 6, ly, colour, 1.2))
        body.append(text(lx, ly + 5, label, 15, TEXT, weight='600'))
    body.append(text(lx, py + 27 + 22, 'auf jeder Seite', 12, GREY))
    body.append(text(lx, py + ph - 26 + 22, 'auf jeder Seite', 12, GREY))
    body.append(text(lx, fy - 34 + 22, 'auf der Seite', 12, GREY))
    body.append(text(lx, fy - 34 + 38, 'ihres Worts', 12, GREY))
    svg('seite', 'headers-and-footnotes', w, h, body, 'Eine Seite mit Kopfzeile, Text, Fußnote und Fußzeile')


DRAWINGS = {f.__name__: f for f in [abstand, serifen, hervorheben, liste, tabstopp, zelle, kopfzeile,
                                      seitenverhaeltnis, umbruch, vorlage, verzeichnis, fusszeile, seite]}

if __name__ == '__main__':
    for name in sys.argv[1:] or DRAWINGS:
        DRAWINGS[name]()
