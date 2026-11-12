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
    # one font at 400 px, scaled to the size asked for (which may be a
    # fraction: 12 pt drawn at a ruler's scale)
    key = (family, weight)
    if key not in _fonts:
        path = _FONT_FILES[(family, weight)]
        source = _ttf(path) if path.suffix == '.woff2' else str(path)
        _fonts[key] = ImageFont.truetype(source, 400)
    return _fonts[key].getlength(text) * size / 400


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



def two_ways(name, folder, wide, about, panels, gap=14, shared=()):
    """a drawing of panels side by side for the desktop (name-wide.svg) and
    the same panels one under another for a phone (name.svg), so that its
    text stays readable there (docs/checkliste.md, »Auf dem Handy«);
    panels: [(elements, (x, y, w, h)), …] in the desktop's coordinates;
    shared: what only the row needs (an arrow between two panels)"""
    w, h = wide
    svg(f'{name}-wide', folder, w, h, [e for els, _ in panels for e in els] + list(shared), about)
    body, y, width = [], 10, 0
    for els, (x0, y0, pw, ph) in panels:
        body.append(f"<g transform='translate({10 - x0:.1f} {y - y0:.1f})'>" + ''.join(els) + '</g>')
        y += ph + gap
        width = max(width, pw)
    svg(name, folder, round(width + 20), round(y - gap + 10), body, about)


# ---- Was das Auge nicht sieht

def abstand():
    """an empty paragraph against spacing after a paragraph"""
    w, h = 560, 236
    panels = []
    for i, (title, spaced) in enumerate([('mit einem leeren Absatz', False), ('mit Abstand danach', True)]):
        x = 18 + i * 280
        body = []
        panels.append((body, (x, 4, 244, 222)))
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
    two_ways('abstand', 'formatting-marks', (w, h), 'Ein leerer Absatz gegen Abstand danach', panels)


# ---- Schrift mit Absicht

def serifen():
    """the same word with and without serifs, the serifs marked"""
    w, h = 560, 214
    size = 92
    panels = []
    serif_file = _ttf(FONTS / 'XCharter' / 'XCharter-Roman.woff2')
    serif_font = TTFont(serif_file)
    for i, (label, names, family) in enumerate([('mit Serifen', 'Times New Roman, Georgia', 'serif'),
                                                ('ohne Serifen', 'Arial, Verdana, Calibri', 'sans')]):
        cx = 140 + i * 280
        body = []
        panels.append((body, (cx - 130, 4 if i == 0 else 36, 260, 200 if i == 0 else 168)))
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
    panels[0][0].append(text(140, 26, 'Serifen: die Füßchen an den Enden', 14, BLUE, 'middle'))
    two_ways('serifen', 'character-formatting', (w, h), 'Hand mit und ohne Serifen', panels)


def hervorheben(phone=False):
    """underlining across descenders, and the shape of a word in capitals"""
    w, h = (360, 336) if phone else (560, 262)
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
    if phone:
        body.append(text(x0, base + 34, 'unterstrichen: die Linie schneidet g, p, y', 14, GREY))
    else:
        body.append(text(x0 + measure(word, size) + 18, base - 4, 'unterstrichen:', 14, GREY))
        body.append(text(x0 + measure(word, size) + 18, base + 14, 'die Linie schneidet g, p, y', 14, GREY))
    # the shape of a word: lower case has one, capitals are a rectangle
    size = 32
    up = set('bdfhklt') | set('ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ')
    down = set('gjpqy')
    for row, (word, notes) in enumerate([('Kuchenverkauf', ('eine Form, an der', 'man das Wort erkennt')),
                                         ('KUCHENVERKAUF', ('jedes Wort', 'ein Rechteck'))]):
        base = (150 + row * 104) if phone else (150 + row * 86)
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
        if phone:
            body.append(text(x0, base + 34, f'{notes[0]} {notes[1]}', 14, GREY))
        else:
            body.append(text(x + 18, base - 6, notes[0], 14, GREY))
            body.append(text(x + 18, base + 12, notes[1], 14, GREY))
    svg(variant('hervorheben', phone), 'character-formatting', w, h, body,
        'Unterstreichung durch die Unterlängen, und die Form eines Worts in Klein- und Großbuchstaben')


# ---- Listen und Tabstopps

def liste():
    """a list typed with dashes against one the program keeps"""
    w, h = 560, 220
    panels = []
    items = ['Schlafsack', 'Regenjacke, falls es am Mittwoch regnet', 'Taschenlampe']
    for i, (title, typed) in enumerate([('getippt', True), ('eine Liste des Programms', False)]):
        x = 18 + i * 280
        body = []
        panels.append((body, (x, 4, 244, 210)))
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
    two_ways('liste', 'lists-and-tabs', (w, h), 'Eine getippte Liste gegen eine Liste des Programms', panels)


def tabstopp(phone=False):
    """the ruler with a right-aligned tab stop; one tab jumps to it, in two fonts"""
    w, h = (360, 262) if phone else (560, 222)
    body = []
    x0, x1 = (26, 334) if phone else (30, 530)         # the text area: 0 to 16 cm
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
    if phone:
        for k, t in enumerate(['ein Tab (→) springt zum Tabstopp;', 'die Punkte sind das Füllzeichen;', 'jeder Preis endet am Tabstopp,',
                               'in jeder Schrift']):
            body.append(text(x0 - 10, 194 + k * 18, t, 13, GREY))
    else:
        body.append(text(x0, 194, 'ein Tab (→) springt zum Tabstopp; die Punkte sind das Füllzeichen', 13, GREY))
        body.append(text(x0, 212, 'jeder Preis endet am Tabstopp, in jeder Schrift', 13, GREY))
    svg(variant('tabstopp', phone), 'lists-and-tabs', w, h, body, 'Ein rechtsbündiger Tabstopp mit Füllzeichen')


# ---- Tabellen

def zelle(phone=False):
    """a table: a cell, a row, a column, and a long entry that makes its row taller"""
    w, h = (360, 318) if phone else (560, 232)
    body = []
    x0, y0 = (18, 22) if phone else (30, 34)
    cols = [84, 52, 190] if phone else [92, 62, 200]
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
    if phone:
        ly = ys[-1] + 44
        body.append(rect(x0, ly - 12, 18, 14, 'none', PAMBER, 2.2))
        body.append(text(x0 + 28, ly, 'eine Zelle: wird der Eintrag länger,', 13, TEXT))
        body.append(text(x0 + 28, ly + 17, 'wird sie höher', 13, TEXT))
        body.append(rect(x0, ly + 30, 18, 14, PBLUE_BG, EDGE, 1))
        body.append(text(x0 + 28, ly + 42, 'eine Zeile', 13, TEXT))
        body.append(rect(x0, ly + 55, 18, 14, PAMBER_BG, EDGE, 1))
        body.append(text(x0 + 28, ly + 67, 'eine Spalte', 13, TEXT))
        svg('zelle', 'word-tables', w, h, body, 'Eine Tabelle mit Zelle, Zeile und Spalte')
        return
    lx = xs[-1] + 30
    body.append(arrow(lx - 4, ys[2] + 12, xs[-1] - 8, ys[2] + 18, PAMBER))
    body.append(text(lx, ys[2] + 10, 'eine Zelle:', 13, TEXT, weight='600'))
    body.append(text(lx, ys[2] + 27, 'wird der Eintrag länger,', 13, GREY))
    body.append(text(lx, ys[2] + 43, 'wird sie höher', 13, GREY))
    body.append(arrow(lx - 4, ys[3] + 20, xs[-1] + 4, ys[3] + 16, BLUE))
    body.append(text(lx, ys[3] + 24, 'eine Zeile', 13, TEXT, weight='600'))
    body.append(arrow(xs[1] + cols[1] / 2 + 30, ys[-1] + 34, xs[1] + cols[1] / 2 + 4, ys[-1] + 4, AMBER))
    body.append(text(xs[1] + cols[1] / 2 + 34, ys[-1] + 38, 'eine Spalte', 13, TEXT, weight='600'))
    svg('zelle-wide', 'word-tables', w, h, body, 'Eine Tabelle mit Zelle, Zeile und Spalte')


def kopfzeile():
    """a table over two pages, its header row on both"""
    w, h = 560, 252
    panels = []
    pw, ph = 200, 226
    days = ['Montag', 'Dienstag', 'Mittwoch', 'Donnerstag', 'Freitag']
    for p in range(2):
        x, y = 40 + p * 270, 12
        body = []
        panels.append((body, (x, y, pw + 34 if p else pw, ph)))
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
    two_ways('kopfzeile', 'word-tables', (w, h), 'Eine Tabelle über zwei Seiten, die Kopfzeile auf beiden', panels,
             shared=[text(253, 130, '→', 26, GREY, 'middle')])


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
    panels = [([], (14, 70, 124, 112)), ([], (164, 58, 184, 142)), ([], (372, 38, 172, 162))]
    base = 150
    # the original
    ow, oh = 108, 72
    x = 22
    body = panels[0][0]
    body.append(_landscape(x, base - oh, ow, oh) + _handles(x, base - oh, ow, oh, ''))
    body.append(text(x + ow / 2, base + 26, 'das Bild, 3 zu 2', 13, TEXT, 'middle', '600'))
    # pulled at the side
    x = 172
    body = panels[1][0]
    sw, sh = 168, 72
    body.append(rect(x, base - oh, ow, oh, 'none', GREY, 1, extra=" stroke-dasharray='3 3'"))
    body.append(_landscape(x, base - sh, sw, sh) + _handles(x, base - sh, sw, sh, 'r'))
    body.append(arrow(x + ow + 6, base - sh / 2 - 22, x + sw - 8, base - sh / 2 - 22, PAMBER))
    body.append(text(x + sw / 2, base + 26, 'an der Seite gezogen:', 13, TEXT, 'middle', '600'))
    body.append(text(x + sw / 2, base + 43, 'nur breiter, verzerrt', 13, GREY, 'middle'))
    # pulled at the corner
    x = 380
    body = panels[2][0]
    cw, ch = 156, 104
    body.append(rect(x, base - oh, ow, oh, 'none', GREY, 1, extra=" stroke-dasharray='3 3'"))
    body.append(_landscape(x, base - ch, cw, ch) + _handles(x, base - ch, cw, ch, 'tr'))
    body.append(arrow(x + ow + 2, base - oh - 2, x + cw - 8, base - ch + 8, PAMBER))
    body.append(text(x + cw / 2, base + 26, 'an der Ecke gezogen:', 13, TEXT, 'middle', '600'))
    body.append(text(x + cw / 2, base + 43, 'größer, dieselbe Form', 13, GREY, 'middle'))
    two_ways('seitenverhaeltnis', 'pictures-in-text', (w, h), 'Ein Bild an der Seite und an der Ecke gezogen', panels)


def umbruch():
    """a picture in the line, against text that flows around it"""
    w, h = 560, 250
    panels = []
    for i, (title, around) in enumerate([('in der Zeile', False), ('Text fließt herum', True)]):
        x = 18 + i * 280
        body = []
        panels.append((body, (x, 4, 244, 232)))
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
    two_ways('umbruch', 'pictures-in-text', (w, h), 'Ein Bild in der Zeile gegen Text, der um das Bild fließt', panels)


# ---- Formatvorlagen

def vorlage(phone=False):
    """one style, many headings: change the style, and every heading changes; by hand wins"""
    w, h = (360, 470) if phone else (560, 268)
    body = []
    # the style
    sx, sy = (16, 14) if phone else (16, 88)
    body.append(rect(sx, sy, 170, 92, '#eef3fb', BLUE, 1.4, 6))
    body.append(text(sx + 12, sy + 24, 'Formatvorlage', 12, GREY))
    body.append(text(sx + 12, sy + 46, 'Überschrift 1', 16, TEXT, weight='600'))
    body.append(text(sx + 12, sy + 68, 'grün, 16 pt, fett', 13, TEXT))
    # the sheet with its headings
    px, py = (16, 150) if phone else (262, 14)
    body.append(paper(px, py, 282 if not phone else 326, 240))
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
    if phone:
        # from the style down along the right edge of the sheet to each heading
        for k, (tx, ty, by_hand) in enumerate(targets):
            colour = AMBER if by_hand else BLUE
            lane = 334 - k * 6
            dash = " stroke-dasharray='5 4'" if by_hand else ''
            hx = tx + 6 + measure(heads[k][0], 16, '700') + (96 if by_hand else 8)
            body.append(f"<path d='M {sx + 172} {sy + 46 + k * 8} L {lane} {sy + 46 + k * 8} L {lane} {ty} L {hx + 10} {ty}' fill='none' stroke='{colour}' stroke-width='1.5'{dash}/>")
            body.append(f"<polygon points='{hx},{ty} {hx + 9},{ty - 4} {hx + 9},{ty + 4}' fill='{colour}'/>")
        for k, t in enumerate(['Ändert sich die Vorlage, ändert sich jede', 'Überschrift mit ihr, außer dort, wo etwas', 'von Hand gesetzt ist.']):
            body.append(text(sx, py + 266 + k * 18, t, 13, GREY))
    else:
        for tx, ty, by_hand in targets:
            body.append(arrow(sx + 172, sy + 46, tx - 2, ty, AMBER if by_hand else BLUE, 1.5, dashed=by_hand))
        body.append(text(sx, sy + 120, 'Ändert sich die Vorlage,', 13, GREY))
        body.append(text(sx, sy + 137, 'ändert sich jede Überschrift', 13, GREY))
        body.append(text(sx, sy + 154, 'mit ihr, außer dort, wo etwas', 13, GREY))
        body.append(text(sx, sy + 171, 'von Hand gesetzt ist.', 13, GREY))
    svg(variant('vorlage', phone), 'paragraph-styles', w, h, body, 'Eine Formatvorlage für viele Überschriften, Formatierung von Hand gewinnt')


# ---- Seiten und Verzeichnisse

def verzeichnis(phone=False):
    """the table of contents comes from the headings and their pages"""
    w, h = (360, 530) if phone else (560, 262)
    body = []
    # the title page with the contents
    tx0, ty0 = (30, 10) if phone else (14, 14)
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
        px, py = (30, 270 + k * 84) if phone else (364, 10 + k * 84)
        body.append(paper(px, py, 182, 76))
        body.append(text(px + 12, py + 22, title, 11, PGREEN, weight='700'))
        for j in range(2):
            body.append(bar(px + 12, py + 38 + j * 11, 140 - j * 30, h=4))
        body.append(text(px + 91, py + 68, str(page), 10, INK, 'middle'))
        if phone:
            # from the page's heading along the left edge up to its line in the contents
            lane, ey = 8 + k * 6, lines_y[k] - 4
            body.append(f"<path d='M {px - 2} {py + 18} L {lane} {py + 18} L {lane} {ey} L {tx0 + 8} {ey}' fill='none' stroke='{BLUE}' stroke-width='1.4'/>")
            body.append(f"<polygon points='{tx0 + 16},{ey} {tx0 + 7},{ey - 4} {tx0 + 7},{ey + 4}' fill='{BLUE}'/>")
        else:
            body.append(arrow(px - 4, py + 18, tx0 + 240, lines_y[k] - 4, BLUE, 1.4))
    body.append(text(tx0 + 20, ty0 + 206, 'aus den Überschriften', 12, BLUE))
    body.append(text(tx0 + 20, ty0 + 222, 'und den Seiten, auf denen sie stehen', 12, BLUE))
    svg(variant('verzeichnis', phone), 'pages-and-contents', w, h, body, 'Das Inhaltsverzeichnis entsteht aus den Überschriften und ihren Seiten')


def fusszeile(phone=False):
    """a typed page number against a field in the footer"""
    w, h = (360, 336) if phone else (560, 290)
    body = []
    pw = 102 if phone else 132
    for row, (title, field) in enumerate([('getippt', False), ('ein Feld', True)]):
        if phone:
            y0 = 30 + row * 166
            body.append(text(16, y0 - 10, title, 15, TEXT, weight='600'))
        else:
            y0 = 12 + row * 142
            body.append(text(16, y0 + 44, title, 15, TEXT, weight='600'))
        for p in range(3):
            x = (16 + p * 114) if phone else (110 + p * 148)
            body.append(paper(x, y0, pw, 104))
            for j in range(4):
                body.append(bar(x + 12, y0 + 20 + j * 12, pw - 24 - (j + p) % 3 * 16, h=4))
            # the footer, the same on every page
            body.append(line(x + 8, y0 + 76, x + pw - 8, y0 + 76, EDGE, 1, " stroke-dasharray='3 3'"))
            label = 'Seite '
            lw = measure(label, 12)
            fx = x + pw / 2 - (lw + 12) / 2
            body.append(text(fx, y0 + 94, label, 12, INK))
            number = str(p + 1) if field else '1'
            if field:
                body.append(rect(fx + lw - 1, y0 + 82, 12, 16, FIELD, r=2))
            body.append(text(fx + lw + 5, y0 + 94, number, 12, PAMBER if not field and p else INK, 'middle',
                             weight='700' if not field and p else '400'))
        note = ('auf jeder Seite »Seite 1«' if not field else 'auf jeder Seite ihre eigene Zahl')
        body.append(text(16 if phone else 110, y0 + 124, note, 13, GREY))
    svg(variant('fusszeile', phone), 'headers-and-footnotes', w, h, body, 'Die Seitenzahl getippt und als Feld in der Fußzeile')



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



# ---- the unit and alignment (Was das Auge nicht sieht)

def punkt(phone=False):
    """the point: 72 to an inch, about 0,35 mm; a 12 pt letter measured from
    the top of its tallest to the bottom of its lowest letters"""
    w, h = (360, 372) if phone else (560, 280)
    body = []
    # a ruler: 1 cm, drawn larger than life, with 1 pt and 12 pt measured
    # along it from 0 (lengths along the ruler, so it is clear what is meant)
    x0, y0, mm = 30, 96, 22.0
    body.append(text(x0, 20, 'ein Zentimeter, vergrößert', 14, GREY))
    body.append(line(x0, y0, x0 + 10 * mm, y0, TEXT, 1.4))
    for k in range(11):
        body.append(line(x0 + k * mm, y0, x0 + k * mm, y0 + (12 if k in (0, 5, 10) else 7), TEXT, 1.2))
    body.append(text(x0, y0 + 30, '0', 12, TEXT, 'middle'))
    body.append(text(x0 + 10 * mm, y0 + 30, '1 cm', 12, TEXT, 'middle'))
    # 1 pt: 0,3528 mm, a blue piece of the ruler from 0
    pt_w = 0.3528 * mm
    body.append(rect(x0, y0 - 5, pt_w, 5, BLUE))
    body.append(line(x0 + pt_w / 2, y0 - 7, x0 + 18, y0 - 26, BLUE, 1))
    body.append(text(x0 + 21, y0 - 24, '1 pt ≈ 0,35 mm', 13, BLUE, weight='600'))
    # 12 pt: 4,23 mm, a bracket over the ruler from 0
    twelve = 12 * pt_w
    body.append(line(x0, y0 - 38, x0, y0 - 44, AMBER, 1.4) + line(x0, y0 - 44, x0 + twelve, y0 - 44, AMBER, 1.4) +
                line(x0 + twelve, y0 - 44, x0 + twelve, y0 - 38, AMBER, 1.4))
    body.append(text(x0 + twelve + 8, y0 - 40, '12 pt ≈ 4,2 mm', 13, AMBER, weight='600'))
    if phone:
        body.append(text(x0 - 14, y0 + 58, '72 pt = 2,54 cm (ein Zoll), 28 pt ≈ 1 cm', 13, GREY))
    else:
        body.append(text(x0 + 10 * mm + 30, y0 - 14, '72 pt = 2,54 cm (ein Zoll)', 13, GREY))
        body.append(text(x0 + 10 * mm + 30, y0 + 4, '28 pt ≈ 1 cm', 13, GREY))
    # 12 pt: the size of a letter, from the top of the tallest to the
    # bottom of the lowest, drawn at the same scale (12 pt = 4,23 mm)
    size = 12 * 0.3528 * mm   # the em at this scale
    base = 262 if phone else 222
    gx = x0 + 20
    top, bottom = base - size * 0.78, base + size * 0.22
    # the box of the size (the font's em: 0,78 above the line, 0,22 below)
    body.append(rect(gx - 4, top, measure('Hg', size) + 8, bottom - top, PBLUE_BG))
    body.append(line(gx - 4, base, gx + measure('Hg', size) + 4, base, EDGE, 1, " stroke-dasharray='3 3'"))
    body.append(text(gx, base, 'Hg', size, INK))
    bx = gx + measure('Hg', size) + 14
    body.append(line(bx, top, bx + 10, top, AMBER, 1.5) + line(bx + 10, top, bx + 10, bottom, AMBER, 1.5) +
                line(bx, bottom, bx + 10, bottom, AMBER, 1.5))
    body.append(text(bx + 18, (top + bottom) / 2 - 4, '12 pt ≈ 4,2 mm', 14, AMBER, weight='600'))
    body.append(text(bx + 18, (top + bottom) / 2 + 14, 'das Feld, in das die', 12, GREY))
    body.append(text(bx + 18, (top + bottom) / 2 + 30, 'Buchstaben passen; ein H', 12, GREY))
    body.append(text(bx + 18, (top + bottom) / 2 + 46, 'ist etwas kleiner', 12, GREY))
    if phone:
        body.append(text(x0 - 14, base + 72, 'Schrift und Abstände misst die Textverarbeitung', 13, GREY))
        body.append(text(x0 - 14, base + 90, 'in pt; Seitenränder und Bilder in cm.', 13, GREY))
    else:
        body.append(text(x0 + 330, 186, 'Schrift und Abstände misst', 13, GREY))
        body.append(text(x0 + 330, 203, 'die Textverarbeitung in pt;', 13, GREY))
        body.append(text(x0 + 330, 220, 'Seitenränder und Bilder in cm.', 13, GREY))
    svg(variant('punkt', phone), 'formatting-marks', w, h, body, 'Ein Punkt (pt) neben einem Zentimeter, und wie groß 12 pt sind')


def ausrichtung():
    """left, centred, right: the same paragraph three times"""
    w, h = 560, 150
    panels = []
    lines = [196, 160, 182, 92]
    for i, (title, mode) in enumerate([('linksbündig', 'left'), ('zentriert', 'center'), ('rechtsbündig', 'right')]):
        x = 10 + i * 186
        body = []
        panels.append((body, (x, 4, 168, 136)))
        body.append(text(x + 84, 22, title, 14, TEXT, 'middle', '600'))
        body.append(paper(x, 34, 168, 104))
        tx, tw = x + 14, 140
        body.append(line(tx, 44, tx, 128, EDGE, 1, " stroke-dasharray='2 3'") +
                    line(tx + tw, 44, tx + tw, 128, EDGE, 1, " stroke-dasharray='2 3'"))
        for k, length in enumerate(lines):
            length = length * tw / 200
            bx = tx if mode == 'left' else tx + tw - length if mode == 'right' else tx + (tw - length) / 2
            body.append(bar(bx, 62 + k * 18, length))
    two_ways('ausrichtung', 'formatting-marks', (w, h), 'Derselbe Absatz linksbündig, zentriert und rechtsbündig', panels)


# ---- levels (Formatvorlagen)

def ebenen():
    """headings in levels: the document beside its outline"""
    w, h = 560, 240
    body = []
    panels = [(body, (16, 14, 250, 212))]
    px, py = 16, 14
    body.append(paper(px, py, 250, 212))
    y = py + 32
    rows = [(1, 'Montag: Anreise'), (0, None), (1, 'Mittwoch: Wanderung'), (2, 'Am Vormittag: um den See'), (0, None),
            (2, 'Am Nachmittag: über den Moorpfad'), (0, None)]
    marks = []
    for lvl, title in rows:
        if lvl == 1:
            body.append(text(px + 16, y, title, 14, PGREEN, weight='700'))
            marks.append((lvl, title, y))
            y += 22
        elif lvl == 2:
            body.append(text(px + 16, y, title, 12, PGREEN, weight='700'))
            marks.append((lvl, title, y))
            y += 20
        else:
            body.append(bar(px + 16, y - 4, 210))
            body.append(bar(px + 16, y + 8, 160))
            y += 28
    # the outline: what the program shows beside the text
    ox, oy = 300, 30
    body = []
    panels.append((body, (ox - 12, oy - 14, 256, 210)))
    body.append(rect(ox - 12, oy - 14, 256, 150, '#f3f4f6', '#d1d5db', 1, 6))
    body.append(text(ox, oy + 6, 'Übersicht der Überschriften', 12, GREY))
    yy = oy + 34
    for lvl, title, _ in marks:
        indent = 0 if lvl == 1 else 18
        body.append(text(ox + indent, yy, ('▸ ' if lvl == 1 else '') + title, 13, TEXT, weight='600' if lvl == 1 else '400'))
        yy += 26
    body.append(text(ox, oy + 156, 'Ebene 1: die Tage', 13, BLUE))
    body.append(text(ox, oy + 174, 'Ebene 2: die Teile eines Tages,', 13, BLUE))
    body.append(text(ox, oy + 190, 'eingerückt unter ihrem Tag', 13, BLUE))
    two_ways('ebenen', 'paragraph-styles', (w, h), 'Überschriften in Ebenen und die Übersicht, die das Programm daraus macht', panels)


# ---- the newspaper page (Die Zeitungsseite)

def spalten():
    """a page in two columns: margins, the columns, the gap between them"""
    w, h = 450, 350
    body = []
    scale = 12.0     # px per cm
    px, py = 170, 14
    pw, ph = 21 * scale, 22 * scale
    body.append(paper(px, py, pw, ph))
    m, gap = 1.5 * scale, 0.8 * scale
    col = (pw - 2 * m - gap) / 2
    for c in range(2):
        cx = px + m + c * (col + gap)
        body.append(rect(cx, py + m, col, ph - 2 * m, PBLUE_BG))
        for k in range(14):
            body.append(bar(cx + 4, py + m + 14 + k * 14, col - 8 - (6 if k % 5 == 4 else 0), h=4))
    # the measures
    yy = py + ph + 0
    def span(x1, x2, y, label, colour):
        out = line(x1, y, x2, y, colour, 1.3) + line(x1, y - 5, x1, y + 5, colour, 1.3) + line(x2, y - 5, x2, y + 5, colour, 1.3)
        return out + text((x1 + x2) / 2, y - 8, label, 12, colour, 'middle', '600')
    # under the page: the margins and the gap, then the whole width
    body.append(span(px, px + m, py + ph + 22, '1,5', AMBER))
    body.append(span(px + pw - m, px + pw, py + ph + 22, '1,5', AMBER))
    body.append(span(px + m + col, px + m + col + gap, py + ph + 22, '0,8', AMBER))
    body.append(span(px, px + pw, py + ph + 56, '21 cm', TEXT))
    body.append(text(20, py + 40, 'Seitenrand', 14, TEXT, weight='600'))
    body.append(text(20, py + 58, 'links und rechts', 12, GREY))
    body.append(text(20, py + 74, 'je 1,5 cm', 12, GREY))
    body.append(text(20, py + 140, 'zwei Spalten,', 14, TEXT, weight='600'))
    body.append(text(20, py + 158, 'dazwischen 0,8 cm', 12, GREY))
    body.append(text(20, py + ph + 26, 'in cm:', 12, AMBER))
    svg('spalten', 'newspaper-page', w, h, body, 'Eine Seite in zwei Spalten mit ihren Rändern und dem Abstand dazwischen')


def satz():
    """an office document's look against a newspaper's: ragged right with
    spacing between paragraphs, justified with a first-line indent"""
    w, h = 560, 304
    panels = []
    paragraphs = [[200, 186, 194, 120], [192, 200, 176, 198, 88], [198, 184, 140]]
    for i, (title, news) in enumerate([('wie ein neues Dokument', False), ('wie in der Zeitung', True)]):
        x = 18 + i * 280
        body = []
        panels.append((body, (x, 4, 244, 292)))
        body.append(text(x + 122, 24, title, 15, TEXT, 'middle', '600'))
        body.append(paper(x, 36, 244, 256))
        tx, tw = x + 18, 208
        y = 58
        for k, para in enumerate(paragraphs):
            for j, length in enumerate(para):
                last = j == len(para) - 1
                if news:
                    start = tx + (18 if j == 0 and k > 0 else 0)
                    full = tx + tw - start
                    body.append(bar(start, y, full if not last else length * tw / 200 * 0.6))
                else:
                    body.append(bar(tx, y, length * tw / 200))
                y += 11 if news else 13
            y += 0 if news else 12
        if news:
            body.append(line(tx + tw + 4, 50, tx + tw + 4, y - 6, PAMBER, 1.2, " stroke-dasharray='3 3'"))
            body.append(text(tx + tw - 2, y + 18, 'jede Zeile bis zum Rand,', 12, PAMBER, 'end'))
            body.append(text(tx + tw - 2, y + 33, 'ein Einzug statt einer Lücke', 12, PAMBER, 'end'))
        else:
            body.append(text(tx, y + 12, 'rechts flatternd, Lücken', 12, GREY))
            body.append(text(tx, y + 27, 'zwischen den Absätzen', 12, GREY))
    two_ways('satz', 'newspaper-page', (w, h), 'Flattersatz mit Abstand gegen Blocksatz mit Einzug', panels)



# ---- the card of the series in the catalog

def karte():
    """three sheets of the series fanned out: the invitation with its
    formatting marks and a check's numbered mark, the report's contents,
    the newspaper page with a green tick"""
    w, h = 480, 270
    body = [rect(0, 0, w, h, '#eef3fb')]
    sw, sh = 150, 212

    def sheet(cx, cy, angle, inner):
        return (f"<g transform='translate({cx} {cy}) rotate({angle})'>" +
                rect(-sw / 2 + 2, -sh / 2 + 3, sw, sh, '#0b0f19', r=3, extra=" fill-opacity='0.12'") +
                rect(-sw / 2, -sh / 2, sw, sh, PAPER, EDGE, 1, 3) + inner + '</g>')

    x0, y0 = -sw / 2 + 14, -sh / 2 + 22
    # the invitation, its marks on
    inv = [text(0, y0 + 2, 'Einladung', 12, INK, 'middle', '700')]
    inv.append(text(-sw / 2 + 12, y0 + 2, '·' * 9, 12, PBLUE, weight='700'))
    inv.append(text(sw / 2 - 12, y0 + 22, '·' * 14 + ' 2. März', 8, INK, 'end'))
    for k, length in enumerate([110, 120, 64]):
        inv.append(bar(x0, y0 + 46 + k * 11, length, h=3))
        inv.append(text(x0 + length + 3, y0 + 48 + k * 11, '↵' if k < 2 else '¶', 8, PBLUE))
    inv.append(text(x0, y0 + 90, '¶', 9, PBLUE))
    for k, length in enumerate([116, 90]):
        inv.append(bar(x0, y0 + 104 + k * 11, length, h=3))
    inv.append(f"<circle cx='{-sw / 2 + 6}' cy='{y0 - 2}' r='8' fill='{PAMBER}'/>" + text(-sw / 2 + 6, y0 + 2, '1', 10, PAPER, 'middle', '700'))
    body.append(sheet(120, 142, -9, ''.join(inv)))
    # the report's contents
    toc = [text(x0, y0 + 4, 'Reisebericht', 13, PGREEN, weight='700'), text(x0, y0 + 30, 'Inhalt', 9, INK, weight='700')]
    for k, (title, page) in enumerate([('Montag', 2), ('Dienstag', 3), ('Mittwoch', 4), ('Donnerstag', 5), ('Freitag', 6)]):
        y = y0 + 46 + k * 13
        toc.append(text(x0, y, title, 8, INK))
        toc.append(text(x0 + 46, y, '.' * 22, 8, INK))
        toc.append(text(sw / 2 - 14, y, str(page), 8, INK, 'end'))
    body.append(sheet(240, 132, 3, ''.join(toc)))
    # the newspaper page, in front
    news = [text(x0, y0 + 4, 'Kraniche', 14, PGREEN, weight='700')]
    col = (sw - 28 - 8) / 2
    for c in range(2):
        cx = x0 + c * (col + 8)
        rows = 13 if c == 0 else 6
        for k in range(rows):
            news.append(bar(cx + (6 if k in (0, 6) and c == 0 else 0), y0 + 22 + k * 9, col - (6 if k in (0, 6) and c == 0 else 0) - (col * 0.4 if k in (5, 12) else 0), h=3))
    # a photo in the second column
    px, py = x0 + col + 8, y0 + 80
    news.append(rect(px, py, col, 40, PBLUE_BG) + f"<path d='M {px} {py + 40} L {px} {py + 28} Q {px + col * 0.4} {py + 14} {px + col} {py + 30} L {px + col} {py + 40} Z' fill='{PGREEN}'/>" +
                f"<circle cx='{px + col * 0.72}' cy='{py + 12}' r='5' fill='{PAMBER}'/>")
    for k in range(3):
        news.append(bar(px, py + 52 + k * 9, col - (col * 0.3 if k == 2 else 0), h=3))
    news.append(f"<circle cx='{sw / 2 - 12}' cy='{-sh / 2 + 14}' r='13' fill='#2e7d32'/>"
                f"<path d='M {sw / 2 - 18} {-sh / 2 + 14} l 4 5 l 9 -10' fill='none' stroke='{PAPER}' stroke-width='2.6' stroke-linecap='round' stroke-linejoin='round'/>")
    body.append(sheet(358, 140, 7, ''.join(news)))
    svg('karte', 'formatting-marks', w, h, body, 'Die Karte der Reihe: drei Blätter, Einladung, Inhaltsverzeichnis, Zeitungsseite')



# ---- spreadsheets (docs/plan-tabellenkalkulation.md)

HEAD_BG = '#e6e8eb'      # the letters and numbers at the edge, as on a sheet


def grid(x, y, cols, rows, cw, ch, cells=None, fills=None, head=True, size=12):
    """a piece of a spreadsheet: column letters over it, row numbers beside
    it (cols: letters, rows: numbers); cells: {(col, row): (text, align,
    weight)}; fills: {(col, row): colour}"""
    out = []
    hx, hy = (24, 18) if head else (0, 0)
    w, h = hx + cw * len(cols), hy + ch * len(rows)
    out.append(rect(x, y, w, h, PAPER, EDGE, 1, 2))
    if head:
        out.append(rect(x, y, w, hy, HEAD_BG) + rect(x, y, hx, h, HEAD_BG))
        for i, c in enumerate(cols):
            out.append(text(x + hx + cw * i + cw / 2, y + 13, c, 11, INK, 'middle'))
        for j, r in enumerate(rows):
            out.append(text(x + hx / 2, y + hy + ch * j + ch / 2 + 4, str(r), 11, INK, 'middle'))
    for (c, r), colour in (fills or {}).items():
        i, j = cols.index(c), rows.index(r)
        out.append(rect(x + hx + cw * i, y + hy + ch * j, cw, ch, colour))
    for i in range(len(cols) + 1):
        out.append(line(x + hx + cw * i, y, x + hx + cw * i, y + h, EDGE, 1))
    for j in range(len(rows) + 1):
        out.append(line(x, y + hy + ch * j, x + w, y + hy + ch * j, EDGE, 1))
    for (c, r), value in (cells or {}).items():
        label, align, weight = (value + ('left', '400'))[:3] if isinstance(value, tuple) else (value, 'left', '400')
        i, j = cols.index(c), rows.index(r)
        tx = x + hx + cw * i + (cw - 5 if align == 'right' else cw / 2 if align == 'middle' else 5)
        anchor = 'end' if align == 'right' else 'middle' if align == 'middle' else 'start'
        out.append(text(tx, y + hy + ch * j + ch / 2 + size * 0.36, label, size, INK, anchor, weight))
    return ''.join(out), (x + hx, y + hy)


def cell_box(x0, y0, cols, rows, cw, ch, c, r):
    return x0 + cw * cols.index(c), y0 + ch * rows.index(r), cw, ch


def variant(name, phone):
    """a drawing in two arrangements (docs/checkliste.md, »Auf dem Handy«):
    in a row for the desktop (name-wide.svg), stacked for a phone (name.svg)"""
    return name if phone else f'{name}-wide'


def adresse(phone=False):
    """a cell's address: its column's letter and its row's number"""
    w, h = (360, 268) if phone else (500, 220)
    cols, rows = ['A', 'B', 'C', 'D'], [1, 2, 3, 4, 5, 6]
    cw, ch = 74, 26
    fills = {('C', r): PBLUE_BG for r in rows}
    fills.update({(c, 4): PBLUE_BG for c in cols})
    fills[('C', 4)] = PAMBER_BG
    body = []
    g, (x0, y0) = grid(20, 20, cols, rows, cw, ch, {('C', 4): ('24,50', 'right')}, fills)
    body.append(g)
    bx, by, bw, bh = cell_box(x0, y0, cols, rows, cw, ch, 'C', 4)
    body.append(rect(bx, by, bw, bh, 'none', PAMBER, 2.4))
    if phone:
        ly = y0 + ch * len(rows) + 32
        body.append(text(20, ly, 'Zelle C4', 16, TEXT, weight='700'))
        body.append(text(20, ly + 19, 'Spalte C, Zeile 4', 13, GREY))
    else:
        lx = x0 + cw * 4 + 22
        body.append(arrow(lx - 4, by + bh / 2, bx + bw + 4, by + bh / 2, AMBER))
        body.append(text(lx, by + bh / 2 - 6, 'Zelle C4', 16, TEXT, weight='700'))
        body.append(text(lx, by + bh / 2 + 13, 'Spalte C, Zeile 4', 13, GREY))
    svg(variant('adresse', phone), 'spreadsheet-cells', w, h, body, 'Die Zelle C4: Spalte C und Zeile 4')


def formel(phone=False):
    """a result typed, against a formula that takes its numbers from cells"""
    w, h = (360, 312) if phone else (560, 270)
    cols, rows = ['B', 'C', 'D'], [5]
    cw, ch = 76, 28
    body = []
    for k, (title, formula) in enumerate([('getippt', False), ('eine Formel', True)]):
        if phone:
            y = 10 + k * 140
            body.append(text(20, y + 14, title, 15, TEXT, weight='600'))
            g, (x0, y0) = grid(20, y + 24, cols, rows, cw, ch, {('B', 5): ('29', 'right'), ('C', 5): ('148,00', 'right'), ('D', 5): ('4.292,00', 'right', '700')})
        else:
            y = 30 + k * 130
            body.append(text(20, y + 34, title, 15, TEXT, weight='600'))
            g, (x0, y0) = grid(130, y, cols, rows, cw, ch, {('B', 5): ('29', 'right'), ('C', 5): ('148,00', 'right'), ('D', 5): ('4.292,00', 'right', '700')})
        body.append(g)
        dx, dy, dw, dh = cell_box(x0, y0, cols, rows, cw, ch, 'D', 5)
        if formula:
            for c in ('B', 'C'):
                sx, sy, sw, sh = cell_box(x0, y0, cols, rows, cw, ch, c, 5)
                lift = 22 if c == 'C' else 40
                ex = dx + dw / 2 + (10 if c == 'B' else -10)
                body.append(f"<path d='M {sx + sw / 2:.1f} {sy + sh + 2:.1f} C {sx + sw / 2:.1f} {sy + sh + lift:.1f} {ex:.1f} {dy + dh + lift:.1f} {ex:.1f} {dy + dh + 11:.1f}' fill='none' stroke='{BLUE}' stroke-width='1.6'/>")
                body.append(f"<polygon points='{ex:.1f},{dy + dh + 2:.1f} {ex - 5:.1f},{dy + dh + 11:.1f} {ex + 5:.1f},{dy + dh + 11:.1f}' fill='{BLUE}'/>")
            label, note, colour = '=B5*C5', 'rechnet mit B5 und C5', BLUE
        else:
            label, note, colour = '4292', 'weiß nichts von B5 und C5', PAMBER
        if phone:
            tx, ty = 20, dy + dh + (66 if formula else 28)
        else:
            tx, ty = dx + dw + 16, dy + 12
        body.append(text(tx, ty, label, 15, colour, weight='700', family=MONO))
        body.append(text(tx, ty + 19, note, 12 if not phone else 13, GREY))
    svg(variant('formel', phone), 'spreadsheet-cells', w, h, body, 'Ein getipptes Ergebnis gegen eine Formel, die mit B5 und C5 rechnet')


def ausfuellen(phone=False):
    """filling down: the formula moves with its row"""
    w, h = (360, 234) if phone else (520, 180)
    cols, rows = ['B', 'C', 'D'], [4, 5, 6, 7]
    cw, ch = 96, 30
    cells = {('B', r): (str(n), 'right') for r, n in zip(rows, [1, 29, 27, 29])}
    for r in rows:
        cells[('D', r)] = (f'=B{r}*C{r}', 'left', '700' if r == 4 else '400')
    cells.update({('C', 4): ('1.250,00', 'right'), ('C', 5): ('148,00', 'right'), ('C', 6): ('24,50', 'right'), ('C', 7): ('9,00', 'right')})
    fills = {('D', r): PBLUE_BG for r in rows[1:]}
    body = []
    g, (x0, y0) = grid(10 if phone else 20, 20, cols, rows, cw, ch, cells, fills, size=13)
    body.append(g)
    dx, dy, dw, dh = cell_box(x0, y0, cols, rows, cw, ch, 'D', 4)
    body.append(rect(dx, dy, dw, dh, 'none', PAMBER, 2.4))
    body.append(rect(dx + dw - 4, dy + dh - 4, 8, 8, PAMBER))
    body.append(arrow(dx + dw + 14, dy + dh, dx + dw + 14, dy + dh * 4 - 4, AMBER, 2))
    if phone:
        ly = y0 + ch * 4 + 30
        body.append(text(10, ly, 'das kleine Quadrat nach unten ziehen:', 13, TEXT, weight='600'))
        body.append(text(10, ly + 19, 'jede Zeile rechnet mit ihrer eigenen Zeile', 13, GREY))
    else:
        lx = dx + dw + 30
        body.append(text(lx, dy + 20, 'das kleine Quadrat', 13, TEXT, weight='600'))
        body.append(text(lx, dy + 37, 'nach unten ziehen', 13, GREY))
        body.append(text(lx, dy + dh * 3 - 4, 'jede Zeile rechnet', 13, TEXT, weight='600'))
        body.append(text(lx, dy + dh * 3 + 13, 'mit ihrer eigenen Zeile', 13, GREY))
    svg(variant('ausfuellen', phone), 'spreadsheet-cells', w, h, body, 'Eine Formel nach unten ausgefüllt: B4*C4 wird B5*C5, B6*C6, B7*C7')


MONO = "'IBM Plex Mono', monospace"


def zahltext(phone=False):
    """a number stands right, text typed with its € stands left, and SUMME
    counts only the numbers"""
    w, h = (360, 282) if phone else (590, 196)
    cols, rows = ['B', 'C'], [4, 5, 6, 7, 8]
    cw, ch = 124, 28
    cells = {('B', 4): 'Familie Arslan', ('B', 5): 'Familie Becker', ('B', 6): 'Familie Çelik', ('B', 7): 'Familie Groß',
             ('B', 8): ('Zusammen', 'left', '700'), ('C', 4): ('245', 'right'), ('C', 5): ('245 €', 'left'),
             ('C', 6): ('245', 'right'), ('C', 7): ('EUR 245', 'left'), ('C', 8): ('490', 'right', '700')}
    fills = {('C', 5): PAMBER_BG, ('C', 7): PAMBER_BG}
    body = []
    g, (x0, y0) = grid(20, 20, cols, rows, cw, ch, cells, fills, size=13)
    body.append(g)
    right = x0 + cw * 2
    _, sy, _, _ = cell_box(x0, y0, cols, rows, cw, ch, 'C', 8)
    target = sy + ch / 2
    for r, lift in ((4, 34), (6, 22)):
        _, ry, _, _ = cell_box(x0, y0, cols, rows, cw, ch, 'C', r)
        cy = ry + ch / 2
        body.append(f"<path d='M {right + 3:.1f} {cy:.1f} C {right + lift:.1f} {cy:.1f} {right + lift:.1f} {target:.1f} {right + 12:.1f} {target:.1f}' fill='none' stroke='{BLUE}' stroke-width='1.6'/>")
    body.append(f"<polygon points='{right + 3:.1f},{target:.1f} {right + 12:.1f},{target - 5:.1f} {right + 12:.1f},{target + 5:.1f}' fill='{BLUE}'/>")
    if phone:
        ly = y0 + ch * 5 + 30
        body.append(text(20, ly, 'eine Zahl: steht rechts', 14, TEXT, weight='600'))
        body.append(text(20, ly + 21, 'Text: steht links, wie getippt', 14, AMBER, weight='600'))
        body.append(text(20, ly + 46, '=SUMME(C4:C7)', 14, BLUE, weight='700', family=MONO))
        body.append(text(20, ly + 64, 'zählt nur die Zahlen zusammen', 13, GREY))
    else:
        lx = right + 48
        _, y4, _, _ = cell_box(x0, y0, cols, rows, cw, ch, 'C', 4)
        _, y5, _, _ = cell_box(x0, y0, cols, rows, cw, ch, 'C', 5)
        body.append(text(lx, y4 + 19, 'eine Zahl: steht rechts', 14, TEXT, weight='600'))
        body.append(text(lx, y5 + 19, 'Text: steht links, wie getippt', 14, AMBER, weight='600'))
        body.append(text(lx, target - 4, '=SUMME(C4:C7)', 14, BLUE, weight='700', family=MONO))
        body.append(text(lx, target + 14, 'zählt nur die Zahlen zusammen', 13, GREY))
    svg(variant('zahltext', phone), 'number-formats', w, h, body, 'Zahlen stehen rechts, getippter Text mit Euro-Zeichen links; SUMME zählt nur die Zahlen')


def formate(phone=False):
    """one number in a cell, shown in three formats; formulas take the number"""
    shown = [('232,75', 'Zahl, 2 Nachkommastellen'), ('232,75 €', 'Währung'), ('233 €', 'Währung, ohne Nachkommastellen')]
    body = []
    if phone:
        w, h = 360, 330
        bx, by = 20, 44
        body.append(text(bx, by - 12, 'in der Zelle steht die Zahl', 13, GREY))
        body.append(rect(bx, by, 120, 40, PAPER, PBLUE, 2, 3))
        body.append(text(bx + 60, by + 27, '232,75', 19, INK, 'middle', '700'))
        trunk = bx + 14
        last = 0
        for i, (value, label) in enumerate(shown):
            cy = 118 + i * 52
            last = cy + 15
            body.append(arrow(trunk, cy + 15, 44, cy + 15, GREY, 1.4))
            body.append(rect(46, cy, 106, 30, PAPER, EDGE, 1))
            body.append(text(146, cy + 20, value, 15, INK, 'end'))
            body.append(text(164, cy + 20, label if i < 2 else 'Währung, ohne', 13, TEXT))
            if i == 2:
                body.append(text(164, cy + 37, 'Nachkommastellen', 13, TEXT))
        body.append(line(trunk, by + 40, trunk, last, GREY, 1.4))
        body.append(text(20, h - 32, 'Jede Formel rechnet mit 232,75,', 14, BLUE, weight='600'))
        body.append(text(20, h - 12, 'auch wenn die Zelle 233 € zeigt.', 14, BLUE, weight='600'))
    else:
        w, h = 600, 236
        bx, by = 20, 86
        body.append(text(bx, by - 30, 'in der Zelle', 13, GREY))
        body.append(text(bx, by - 13, 'steht die Zahl', 13, GREY))
        body.append(rect(bx, by, 120, 40, PAPER, PBLUE, 2, 3))
        body.append(text(bx + 60, by + 27, '232,75', 19, INK, 'middle', '700'))
        for i, (value, label) in enumerate(shown):
            cy = 30 + i * 62
            cx = 250
            body.append(arrow(bx + 124, by + 20, cx - 6, cy + 15, GREY, 1.4))
            body.append(rect(cx, cy, 110, 30, PAPER, EDGE, 1))
            body.append(text(cx + 104, cy + 20, value, 15, INK, 'end'))
            body.append(text(cx + 124, cy + 20, label, 13, TEXT))
        body.append(text(20, h - 14, 'Jede Formel rechnet mit 232,75, auch wenn die Zelle 233 € zeigt.', 14, BLUE, weight='600'))
    svg(variant('formate', phone), 'number-formats', w, h, body, 'Eine Zahl, drei Formate: 232,75, 232,75 € und 233 €; gerechnet wird mit 232,75')


def tage(phone=False):
    """a date is the number of days counted from the end of 1899"""
    import datetime
    days = [datetime.date(2027, 3, d) for d in range(2, 10)]
    names = ['Mo', 'Di', 'Mi', 'Do', 'Fr', 'Sa', 'So']
    body = []
    if phone:
        w, h = 360, 290
        bw, bh, x0, y0 = 62, 54, 52, 70
        body.append(text(20, 20, 'oben das Format Datum,', 13, GREY))
        body.append(text(20, 38, 'unten die Zahl, die in der Zelle steht', 13, GREY))
        body.append(text(20, y0 - 8, 'Tag 0: Ende 1899', 12, GREY))
        body.append(line(20, y0 + bh / 2, x0 - 6, y0 + bh / 2, GREY, 1.4, " stroke-dasharray='3 4'"))
        per_row = 4
    else:
        w, h = 640, 176
        bw, bh, x0, y0 = 62, 54, 122, 40
        body.append(text(20, y0 + 20, 'Tag 0:', 13, GREY))
        body.append(text(20, y0 + 38, 'Ende 1899', 13, GREY))
        body.append(line(92, y0 + bh / 2, x0 - 8, y0 + bh / 2, GREY, 1.4, " stroke-dasharray='3 4'"))
        body.append(text(x0, 24, 'oben das Format Datum, unten die Zahl, die in der Zelle steht', 13, GREY))
        per_row = len(days)
    for i, d in enumerate(days):
        x = x0 + (i % per_row) * bw
        y = y0 + (i // per_row) * (bh + 12)
        first_last = i in (0, len(days) - 1)
        body.append(rect(x, y, bw, bh, PAMBER_BG if first_last else PAPER, EDGE, 1))
        body.append(text(x + bw / 2, y + 20, f"{names[d.weekday()]} {d.day:02d}.03.", 12, INK, 'middle'))
        serial = (d - datetime.date(1899, 12, 30)).days
        body.append(text(x + bw / 2, y + 42, str(serial), 14, INK, 'middle', '700', MONO))
    if phone:
        yb = y0 + 2 * bh + 12 + 34
        body.append(text(20, yb, '=A13-A4', 15, BLUE, weight='700', family=MONO))
        body.append(text(20, yb + 22, '46455 − 46448 = 7 Tage', 14, TEXT))
    else:
        a, b = x0 + bw / 2, x0 + bw * (len(days) - 1) + bw / 2
        yb = y0 + bh + 14
        body.append(f"<path d='M {a:.1f} {yb - 6:.1f} L {a:.1f} {yb:.1f} L {b:.1f} {yb:.1f} L {b:.1f} {yb - 6:.1f}' fill='none' stroke='{BLUE}' stroke-width='1.6'/>")
        body.append(text((a + b) / 2, yb + 26, '=A13-A4', 15, BLUE, 'middle', '700', MONO))
        body.append(text((a + b) / 2, yb + 46, '46455 − 46448 = 7 Tage', 14, TEXT, 'middle'))
    svg(variant('tage', phone), 'number-formats', w, h, body, 'Die Tage vom 2. bis 9. März 2027 mit ihren Zahlen 46448 bis 46455; A13 minus A4 ergibt 7 Tage')


def _bezug(fixed, phone=False):
    """C6's share filled down to C8: without $ each row divides by the cell
    four rows up (B2, B3, B4), with $B$2 all by B2"""
    cols, rows = ['B', 'C'], [2, 3, 4, 5, 6, 7, 8]
    cw, ch = (84, 28) if phone else (104, 28)
    size = 12 if phone else 13
    cells = {('B', 2): ('27', 'right'), ('B', 3): ('5 %', 'right'), ('B', 5): ('Kosten', 'left', '700'),
             ('C', 5): ('pro Kind', 'left', '700'), ('B', 6): ('1.250,00 €', 'right'), ('B', 7): ('3.996,00 €', 'right'),
             ('B', 8): ('661,50 €', 'right')}
    if fixed:
        cells.update({('C', 6): ('46,30 €', 'right'), ('C', 7): ('148,00 €', 'right'), ('C', 8): ('24,50 €', 'right')})
        fills = {('B', 2): PBLUE_BG}
    else:
        cells.update({('C', 6): ('46,30 €', 'right'), ('C', 7): ('79.920,00 €', 'right'), ('C', 8): ('#DIV/0!', 'right', '700')})
        fills = {('B', 2): PBLUE_BG, ('B', 3): PAMBER_BG, ('B', 4): PAMBER_BG, ('C', 7): PAMBER_BG, ('C', 8): PAMBER_BG}
    body = []
    gx = 80 if phone else 96
    g, (x0, y0) = grid(gx, 20, cols, rows, cw, ch, cells, fills, size=size)
    body.append(g)
    left = x0 - 24
    notes = []
    for k, r in enumerate((6, 7, 8)):
        target = 2 if fixed else r - 4
        ok = fixed or r == 6
        colour = BLUE if ok else AMBER
        ys = y0 + ch * rows.index(r) + ch / 2
        yt = y0 + ch * rows.index(target) + ch / 2
        bulge = left - 18 - 16 * k
        body.append(f"<path d='M {left - 2:.1f} {ys:.1f} C {bulge:.1f} {ys:.1f} {bulge:.1f} {yt:.1f} {left - 11:.1f} {yt:.1f}' fill='none' stroke='{colour}' stroke-width='1.6'/>")
        body.append(f"<polygon points='{left - 2:.1f},{yt:.1f} {left - 11:.1f},{yt - 5:.1f} {left - 11:.1f},{yt + 5:.1f}' fill='{colour}'/>")
        lx = x0 + cw * 2 + (10 if phone else 16)
        formula = f'=B{r}/$B$2' if fixed else f'=B{r}/B{target}'
        body.append(text(lx, ys + 5, formula, 12 if phone else 14, colour, weight='700', family=MONO))
        note = {6: 'teilt durch 27', 7: 'teilt durch 27' if fixed else 'teilt durch 0,05', 8: 'teilt durch 27' if fixed else 'teilt durch eine leere Zelle'}[r]
        if phone:
            notes.append((f'C{r} {note}', colour))
        else:
            body.append(text(lx + 112, ys + 5, note, 13, GREY))
    if phone:
        w = 360
        y = y0 + ch * len(rows) + 28
        if fixed:
            body.append(text(20, y, '$B$2', 16, BLUE, weight='700', family=MONO))
            body.append(text(76, y - 1, '$ vor B: die Spalte bleibt,', 13, TEXT))
            body.append(text(76, y + 17, '$ vor 2: die Zeile bleibt', 13, TEXT))
            y += 44
            body.append(text(20, y, 'C6, C7 und C8 teilen alle durch 27', 13, GREY))
            h = y + 16
        else:
            body.append(text(20, y, 'nach unten ausgefüllt, ohne $:', 13, GREY))
            for i, (n, colour) in enumerate(notes):
                body.append(text(20, y + 20 + i * 19, n, 13, TEXT if colour == BLUE else AMBER))
            h = y + 20 + 3 * 19
    else:
        w, h = 640, 262
        if fixed:
            ax, ay = x0 + cw * 2 + 16, y0 + 8
            body.append(text(ax, ay + 6, '$B$2', 18, BLUE, weight='700', family=MONO))
            body.append(text(ax + 66, ay - 2, '$ vor B: die Spalte bleibt', 13, TEXT))
            body.append(text(ax + 66, ay + 15, '$ vor 2: die Zeile bleibt', 13, TEXT))
        else:
            body.append(text(x0 + cw * 2 + 16, y0 + 14, 'nach unten ausgefüllt, ohne $', 13, GREY))
    name = 'mit-dollar' if fixed else 'ohne-dollar'
    about = ('Mit $B$2 teilen C6, C7 und C8 alle durch B2' if fixed
             else 'Ohne $ teilt C7 durch B3 und C8 durch die leere Zelle B4')
    svg(variant(name, phone), 'absolute-references', w, h, body, about)


def ohne_dollar(phone=False):
    _bezug(False, phone)


def mit_dollar(phone=False):
    _bezug(True, phone)


def leer_null(phone=False):
    """an empty cell is left out of MITTELWERT, a 0 counts"""
    w, h = (360, 490) if phone else (560, 252)
    rows = [4, 5, 6, 7, 8, 9]
    cw, ch = 70, 28
    body = []
    for k, (title, third, mean, count) in enumerate([('leer gelassen', None, '4', '4 Zahlen'), ('0 getippt', '0', '3,2', '5 Zahlen')]):
        x, top = (20, k * 240) if phone else (20 + k * 280, 0)
        cells = {('B', 4): ('5', 'right'), ('B', 5): ('4', 'right'), ('B', 7): ('3', 'right'), ('B', 8): ('4', 'right'),
                 ('B', 9): (mean, 'right', '700')}
        if third:
            cells[('B', 6)] = (third, 'right')
        g, (x0, y0) = grid(x, top + 42, ['B'], rows, cw, ch, cells, {('B', 6): PAMBER_BG}, size=14)
        body.append(g)
        body.append(text(x, top + 26, title, 15, TEXT, weight='600'))
        lx = x0 + cw + 12
        body.append(text(lx, y0 + ch * 5 + 13, '=MITTELWERT(B4:B8)', 13, BLUE, weight='700', family=MONO))
        body.append(text(lx, y0 + ch * 5 + 31, f'zählt {count}', 13, GREY))
        body.append(text(lx, y0 + ch * 2 + 18, 'nicht gezählt' if not third else 'zählt mit', 13, AMBER, weight='600'))
    about = ('Links bleibt B6 leer, MITTELWERT zählt 4 Zahlen und zeigt 4; rechts steht 0 in B6, MITTELWERT zählt 5 Zahlen und zeigt 3,2' if not phone
             else 'Oben bleibt B6 leer, MITTELWERT zählt 4 Zahlen und zeigt 4; unten steht 0 in B6, MITTELWERT zählt 5 Zahlen und zeigt 3,2')
    svg(variant('leer-null', phone), 'spreadsheet-functions', w, h, body, about)


def wenn():
    """the three parts of WENN: the condition, then, else"""
    w, h = 400, 152
    body = []
    drop = {'die Bedingung': 0, 'wenn sie stimmt': 26, 'wenn nicht': 0}
    parts = [('=WENN(', None), ('E33>=4', 'die Bedingung'), (';', None), ('"ja"', 'wenn sie stimmt'), (';', None),
             ('"nein"', 'wenn nicht'), (')', None)]
    x, y = 24, 60
    size = 22
    for part, label in parts:
        width = len(part) * size * 0.6
        colour = BLUE if label else INK
        body.append(text(x, y, part, size, colour, weight='700', family=MONO))
        if label:
            mid = x + width / 2
            body.append(line(x, y + 10, x + width, y + 10, BLUE, 1.6))
            body.append(line(mid, y + 10, mid, y + 26 + drop[label], BLUE, 1.2))
            body.append(text(mid, y + 44 + drop[label], label, 14, TEXT, 'middle'))
        x += width
    svg('wenn', 'spreadsheet-functions', w, h, body, 'Die Teile von WENN: die Bedingung E33>=4, »ja«, wenn sie stimmt, »nein«, wenn nicht')


def sortieren(phone=False):
    """sorting whole rows keeps each family with its amount; sorting the
    column of names alone puts names beside other families' amounts"""
    rows = [4, 5, 6, 7]
    cw, ch = 92, 28
    before = [('Engel', '245,00 €'), ('Lehmann', '100,00 €'), ('Becker', '245,00 €'), ('Kaya', '')]
    whole = sorted(before)
    names = sorted(n for n, _ in before)
    alone = [(n, before[i][1]) for i, n in enumerate(names)]
    body = []
    for k, (title, data, wrong) in enumerate([('vorher', before, False), ('ganze Zeilen sortiert', whole, False),
                                              ('nur Spalte B sortiert', alone, True)]):
        x, top = (20, k * 168) if phone else (20 + k * 232, 0)
        cells, fills = {}, {}
        for r, (n, paid) in zip(rows, data):
            cells[('B', r)] = n
            cells[('D', r)] = (paid, 'right')
            if wrong and dict(before)[n] != paid:
                fills[('B', r)] = PAMBER_BG
                fills[('D', r)] = PAMBER_BG
        g, _ = grid(x, top + 46, ['B', 'D'], rows, cw, ch, cells, fills, size=13)
        body.append(g)
        body.append(text(x, top + 30, title, 14, AMBER if wrong else TEXT, weight='600'))
    if phone:
        w, h = 360, 3 * 168 + 86
        body.append(text(20, h - 32, 'Unten hat Kaya bezahlt und Lehmann nichts:', 13, GREY))
        body.append(text(20, h - 13, 'Die Namen stehen neben fremden Beträgen.', 13, GREY))
    else:
        w, h = 700, 236
        body.append(text(20, h - 16, 'Rechts hat Kaya bezahlt und Lehmann nichts: Die Namen stehen neben fremden Beträgen.', 13, GREY))
    svg(variant('sortieren', phone), 'sort-and-filter', w, h, body, 'Vier Familien mit ihren Zahlungen: ganze Zeilen sortiert bleiben richtig, nur die Namen sortiert stehen neben fremden Beträgen')


def _columns(x, y, w, h, values, lo, hi, ticks, colour=PBLUE, labels=None):
    """a small column chart on paper: values from lo to hi, ticks on the axis"""
    out = [rect(x, y, w, h, PAPER, EDGE, 1, 2)]
    left, bottom, top = x + 34, y + h - 26, y + 12
    for t in ticks:
        ty = bottom - (t - lo) / (hi - lo) * (bottom - top)
        out.append(line(left, ty, x + w - 10, ty, EDGE, 1))
        out.append(text(left - 6, ty + 4, f'{t:g}'.replace('.', ','), 11, INK, 'end'))
    slot = (x + w - 10 - left) / len(values)
    for i, v in enumerate(values):
        bh = (v - lo) / (hi - lo) * (bottom - top)
        out.append(rect(left + slot * i + slot * 0.2, bottom - bh, slot * 0.6, bh, colour))
        if labels:
            out.append(text(left + slot * i + slot / 2, bottom + 16, labels[i], 11, INK, 'middle'))
    out.append(line(left, bottom, x + w - 10, bottom, INK, 1.2))
    return ''.join(out)


def _panel(kind, x, y, pw, ph):
    """one small chart on paper: 'pie', 'columns' or 'line'"""
    import math
    out = []
    if kind == 'pie':
        out.append(rect(x, y, pw, ph, PAPER, EDGE, 1, 2))
        cx, cy, r = x + pw / 2, y + ph / 2, min(pw, ph) / 2 - 18
        shares = [46.3, 148, 24.5, 9, 4.5]
        a = -math.pi / 2
        for v, c in zip(shares, [PBLUE, PAMBER, PGREEN, BAR, EDGE]):
            b = a + v / sum(shares) * 2 * math.pi
            large = 1 if b - a > math.pi else 0
            out.append(f"<path d='M {cx:.1f} {cy:.1f} L {cx + r * math.cos(a):.1f} {cy + r * math.sin(a):.1f} "
                       f"A {r:.1f} {r:.1f} 0 {large} 1 {cx + r * math.cos(b):.1f} {cy + r * math.sin(b):.1f} Z' fill='{c}' stroke='{PAPER}' stroke-width='1.5'/>")
            a = b
    elif kind == 'columns':
        out.append(_columns(x, y, pw, ph, [4.4, 3.6, 2.6, 4.0], 0, 5, [0, 1, 2, 3, 4, 5]))
    else:
        out.append(rect(x, y, pw, ph, PAPER, EDGE, 1, 2))
        left, bottom, top = x + 20, y + ph - 20, y + 16
        pts = [0, 0.18, 0.4, 0.55, 0.62, 0.8, 0.86, 0.95, 1.0]
        xs = [left + i * (pw - 40) / (len(pts) - 1) for i in range(len(pts))]
        ys = [bottom - p * (bottom - top) for p in pts]
        out.append(line(left, bottom, x + pw - 14, bottom, INK, 1.2) + line(left, bottom, left, top - 4, INK, 1.2))
        out.append("<polyline points='" + ' '.join(f'{a:.1f},{b:.1f}' for a, b in zip(xs, ys)) + f"' fill='none' stroke='{PBLUE}' stroke-width='2.4'/>")
    return ''.join(out)


def diagrammarten(phone=False):
    """which chart answers which question"""
    questions = [('pie', 'Kreis', 'Welcher Teil des Ganzen', 'ist wie groß?'), ('columns', 'Säulen', 'Was ist größer,', 'was kleiner?'),
                 ('line', 'Linie', 'Wie ändert sich etwas', 'mit der Zeit?')]
    body = []
    if phone:
        w, h = 360, 3 * 146 + 10
        for k, (kind, name, q1, q2) in enumerate(questions):
            y = 10 + k * 146
            body.append(_panel(kind, 20, y, 160, 130))
            body.append(text(196, y + 52, name, 15, TEXT, weight='700'))
            body.append(text(196, y + 72, q1, 13, GREY))
            body.append(text(196, y + 89, q2, 13, GREY))
    else:
        w, h = 660, 250
        pw, ph, gap = 196, 160, 36
        for k, (kind, name, q1, q2) in enumerate(questions):
            x = 20 + k * (pw + gap - 10)
            body.append(_panel(kind, x, 20, pw, ph))
            body.append(text(x + pw / 2, 20 + ph + 26, name, 15, TEXT, 'middle', '700'))
            body.append(text(x + pw / 2, 20 + ph + 46, q1, 13, GREY, 'middle'))
            body.append(text(x + pw / 2, 20 + ph + 63, q2, 13, GREY, 'middle'))
    svg(variant('diagrammarten', phone), 'spreadsheet-charts', w, h, body, 'Drei Diagramme und ihre Fragen: Kreis, welcher Teil des Ganzen wie groß ist; Säulen, was größer und was kleiner ist; Linie, wie sich etwas mit der Zeit ändert')


def achse(phone=False):
    """the same two means, the axis from 0 and from 2,5"""
    w, h = (360, 500) if phone else (600, 244)
    body = []
    names = ['Kletterwald', 'Moormuseum']
    for k, (title, lo, ticks) in enumerate([('Achse ab 0', 0, [0, 1, 2, 3, 4, 5]), ('Achse ab 2,5', 2.5, [2.5, 3, 3.5, 4, 4.5])]):
        x, top = (20, k * 248) if phone else (20 + k * 300, 0)
        hi = 5 if lo == 0 else 4.5
        body.append(text(x, top + 22, title, 15, AMBER if lo else TEXT, weight='600'))
        body.append(_columns(x, top + 34, 300 if phone else 260, 200, [4.4, 2.6], lo, hi, ticks, PAMBER if lo else PBLUE, names))
    svg(variant('achse', phone), 'spreadsheet-charts', w, h, body, 'Dieselben Mittelwerte 4,4 und 2,6 zweimal als Säulen: mit der Achse ab 0 ist die eine knapp doppelt so hoch, mit der Achse ab 2,5 fast zwanzigmal')


def karte_tabellen():
    """the card of the series on spreadsheets: three sheets fanned out, a
    grid with a formula and a check's numbered mark, a pie chart, a column
    chart with a green tick"""
    import math
    w, h = 480, 270
    body = [rect(0, 0, w, h, '#eef3fb')]
    sw, sh = 150, 212

    def sheet(cx, cy, angle, inner):
        return (f"<g transform='translate({cx} {cy}) rotate({angle})'>" +
                rect(-sw / 2 + 2, -sh / 2 + 3, sw, sh, '#0b0f19', r=3, extra=" fill-opacity='0.12'") +
                rect(-sw / 2, -sh / 2, sw, sh, PAPER, EDGE, 1, 3) + inner + '</g>')

    x0, y0 = -sw / 2 + 20, -sh / 2 + 20
    # a grid: column letters, row numbers, a formula in the marked cell
    cw, ch, hx = 34, 15, 14
    g = [rect(x0, y0, hx + cw * 3, ch, HEAD_BG), rect(x0, y0, hx, ch * 10, HEAD_BG)]
    for i, c in enumerate('BCD'):
        g.append(text(x0 + hx + cw * i + cw / 2, y0 + 11, c, 8, INK, 'middle'))
    values = [('29', '148,00', '4.292,00'), ('27', '24,50', '661,50'), ('29', '9,00', '261,00'), ('29', '4,50', '130,50')]
    for j in range(9):
        y = y0 + ch * (j + 1)
        g.append(text(x0 + hx / 2, y + 11, str(j + 4), 7, INK, 'middle'))
        if j < len(values):
            for i, v in enumerate(values[j]):
                g.append(text(x0 + hx + cw * i + cw - 3, y + 11, v, 7, INK, 'end'))
    for i in range(4):
        g.append(line(x0 + hx + cw * i, y0, x0 + hx + cw * i, y0 + ch * 10, EDGE, 0.8))
    for j in range(11):
        g.append(line(x0, y0 + ch * j, x0 + hx + cw * 3, y0 + ch * j, EDGE, 0.8))
    mx, my = x0 + hx + cw * 2, y0 + ch * 2
    g.append(rect(mx, my, cw, ch, PAMBER_BG) + rect(mx, my, cw, ch, 'none', PAMBER, 1.6))
    g.append(text(mx + cw - 3, my + 11, '661,50', 7, INK, 'end'))
    g.append(text(x0, y0 + ch * 10 + 22, '=B5*C5', 13, PBLUE, weight='700', family="'IBM Plex Mono', monospace"))
    g.append(f"<circle cx='{x0 - 9}' cy='{my + ch / 2}' r='7' fill='{PAMBER}'/>" + text(x0 - 9, my + ch / 2 + 3.5, '1', 9, PAPER, 'middle', '700'))
    body.append(sheet(120, 142, -9, ''.join(g)))
    # a pie chart
    pie = [text(0, y0 + 4, 'Wofür ein Kind', 10, INK, 'middle', '700'), text(0, y0 + 17, 'bezahlt', 10, INK, 'middle', '700')]
    cx, cy, r = 0, 18, 52
    a = -math.pi / 2
    shares = [46.3, 148, 24.5, 9, 4.5]
    for v, c in zip(shares, [PBLUE, PAMBER, PGREEN, BAR, EDGE]):
        b = a + v / sum(shares) * 2 * math.pi
        large = 1 if b - a > math.pi else 0
        pie.append(f"<path d='M {cx} {cy} L {cx + r * math.cos(a):.1f} {cy + r * math.sin(a):.1f} A {r} {r} 0 {large} 1 "
                   f"{cx + r * math.cos(b):.1f} {cy + r * math.sin(b):.1f} Z' fill='{c}' stroke='{PAPER}' stroke-width='1.2'/>")
        a = b
    body.append(sheet(240, 132, 3, ''.join(pie)))
    # a column chart, in front, with the tick
    col = [text(0, y0 + 4, 'Wie hat es gefallen?', 10, INK, 'middle', '700')]
    left, bottom, top = -sw / 2 + 26, sh / 2 - 34, y0 + 26
    for t in range(6):
        ty = bottom - t / 5 * (bottom - top)
        col.append(line(left, ty, sw / 2 - 12, ty, EDGE, 0.8) + text(left - 5, ty + 3, str(t), 7, INK, 'end'))
    slot = (sw / 2 - 12 - left) / 4
    for i, v in enumerate([4.4, 3.6, 2.6, 4.0]):
        bh = v / 5 * (bottom - top)
        col.append(rect(left + slot * i + slot * 0.2, bottom - bh, slot * 0.6, bh, PBLUE))
    col.append(line(left, bottom, sw / 2 - 12, bottom, INK, 1))
    col.append(f"<circle cx='{sw / 2 - 12}' cy='{-sh / 2 + 14}' r='13' fill='#2e7d32'/>"
               f"<path d='M {sw / 2 - 18} {-sh / 2 + 14} l 4 5 l 9 -10' fill='none' stroke='{PAPER}' stroke-width='2.6' stroke-linecap='round' stroke-linejoin='round'/>")
    body.append(sheet(358, 140, 7, ''.join(col)))
    svg('karte', 'spreadsheet-cells', w, h, body, 'Die Karte der Reihe: drei Blätter, eine Tabelle mit Formel, ein Kreisdiagramm, ein Säulendiagramm')


DRAWINGS = {f.__name__: f for f in [abstand, serifen, hervorheben, liste, tabstopp, zelle, kopfzeile,
                                      seitenverhaeltnis, umbruch, vorlage, verzeichnis, fusszeile, seite, punkt, ausrichtung, ebenen, spalten, satz, karte, adresse, formel, ausfuellen,
                                      zahltext, formate, tage, ohne_dollar, mit_dollar, leer_null, wenn, sortieren, diagrammarten, achse, karte_tabellen]}
# the drawings with a phone arrangement too (docs/checkliste.md, »Auf dem Handy«)
TWO_WAYS = {adresse, formel, ausfuellen, zahltext, formate, tage, ohne_dollar, mit_dollar, leer_null, sortieren, diagrammarten, achse,
            hervorheben, tabstopp, zelle, vorlage, verzeichnis, fusszeile, punkt}

if __name__ == '__main__':
    for name in sys.argv[1:] or DRAWINGS:
        DRAWINGS[name]()
        if DRAWINGS[name] in TWO_WAYS:
            DRAWINGS[name](phone=True)
