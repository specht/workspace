"""The diagrams of »Eine Suchmaschine bauen« drawn in Python, as SVG beside
the page (the card picture is bilder/teaser.py).

    python3 src/content/suchmaschine/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/suchmaschine/NAME.svg WIDTH

- drei-teile: crawler, index and ranking as three steps one after the other,
  with what passes between them.
- tierlexikon-links: the pages of tierlexikon/ and their links, read from
  the HTML files: index.html in the middle, the others in a ring so that
  pages linking each other stand side by side; a line with two heads where
  two pages link each other. Under each page how many other pages the
  crawler finds that link to it (incoming), geheim.html apart, which no
  page links.
- crawl-queue: the crawler's queue on a small made-up site of five pages
  (not the Tierlexikon, whose pages and their number are tasks): the site
  on the left, on the right what each step takes out, the queue after it and
  everything seen so far, the new paths marked.
- eingehend: the same small site, /e.html with a link to itself, and under
  each page how many other pages link to it.
- index-umgedreht: the two pages of the page's example and the inverted
  index made of them, each page in its colour.
- schnittmenge: the pages of two words as two circles, the hits of a search
  for both where they overlap (made-up pages, not the Tierlexikon's:
  »nachts insekten« is a task).
"""

import math
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402
from diagram_labels import code  # noqa: E402

OUT = os.path.join(HERE, '..')
SITE = os.path.join(OUT, 'tierlexikon')
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
INK = '#1f2937'
GREY = '#6b7280'
BLUE = '#2b5fb4'
AMBER = '#d97706'
PALE_AMBER = '#fdecc8'
PALE = '#eef3fb'
WHITE = '#ffffff'


def save(name, w, h, comment, parts, scale=2):
    render = f'src/scripts/render-svg.rb src/content/suchmaschine/{name}.svg {scale * w}'
    head = ap.svg_head(w, h, comment + '; gemacht von bilder/diagramme.py, als Bild:', render)
    head = head.replace('<svg ', f"<svg font-family='{FONT}' ", 1)
    with open(os.path.join(OUT, name + '.svg'), 'w') as f:
        f.write(head + f"<rect width='{w}' height='{h}' fill='{WHITE}'/>" + ''.join(parts) + '</svg>\n')


def text(x, y, s, size=16, color=INK, anchor='middle', italic=False, halo=True):
    """halo: an edge in the background's colour, so that no line runs
    through the text; not inside a box, where it would show."""
    style = " font-style='italic'" if italic else ''
    edge = f" stroke='{WHITE}' stroke-width='5' stroke-linejoin='round' paint-order='stroke'" if halo else ''
    return f"<text x='{x:.1f}' y='{y:.1f}' font-size='{size}' fill='{color}' text-anchor='{anchor}'{style}{edge}>{s}</text>"


def drei_teile():
    w, h = 1000, 200
    steps = [('Crawler', 'folgt den Links'), ('Index', 'Wort → Seiten'), ('Ranking', 'ordnet die Treffer')]
    between = ['die Seiten', 'die Treffer']
    bw, bh, gap = 200, 80, 120
    x0 = (w - 3 * bw - 2 * gap) / 2
    parts = []
    for i, (name, what) in enumerate(steps):
        x = x0 + i * (bw + gap)
        parts.append(f"<rect x='{x:.1f}' y='50' width='{bw}' height='{bh}' rx='12' fill='{PALE}' stroke='{INK}' stroke-width='2'/>")
        parts.append(text(x + bw / 2, 86, name, 22, halo=False))
        parts.append(text(x + bw / 2, 112, what, 15, GREY, halo=False))
        if i < 2:
            a, b = x + bw, x + bw + gap
            parts.append(f"<line x1='{a:.1f}' y1='90' x2='{b - 11:.1f}' y2='90' stroke='{BLUE}' stroke-width='2.5'/>")
            parts.append(ap.arrowhead(b, 90, 1, 0, BLUE, 12, 6))
            parts.append(text((a + b) / 2, 76, between[i], 15, BLUE))
    parts.append(text(x0 - 14, 95, 'Web', 17, GREY, 'end', italic=True))
    parts.append(text(x0 + 3 * bw + 2 * gap + 14, 95, 'du', 17, GREY, 'start', italic=True))
    save('drei-teile', w, h, 'Crawler, Index und Ranking hintereinander', parts)


def site_links():
    links = {}
    for name in os.listdir(SITE):
        if name.endswith('.html'):
            html = open(os.path.join(SITE, name)).read()
            links[name] = [l.lstrip('/') for l in re.findall(r'href="([^"#]+)"', html) if l.endswith('.html')]
    return links


def tierlexikon_links():
    links = site_links()
    found, todo = [], ['index.html']
    while todo:
        page = todo.pop(0)
        if page in found or page not in links:
            continue
        found.append(page)
        todo += links[page]
    incoming = {p: sum(1 for q in found if q != p and p in links[q]) for p in found}

    w, h = 1100, 700
    cx, cy, r = 520, 350, 265
    ring = ['katzenfutter-billig.html', 'hund.html', 'katze.html', 'amsel.html', 'eule.html',
            'fledermaus.html', 'igel.html', 'pinguin.html', 'impressum.html']
    assert sorted(ring + ['index.html']) == sorted(found), found
    at = {'index.html': (cx, cy)}
    for i, page in enumerate(ring):
        angle = math.radians(-160 + i * 360 / len(ring))
        at[page] = (cx + r * math.cos(angle) * 1.55, cy + r * math.sin(angle))
    # hund and katze would almost touch at the top of the ring
    x, y = at['hund.html']
    at['hund.html'] = (x - 70, y + 40)
    at['geheim.html'] = (960, 650)
    bw, bh = 250, 54

    def border(x, y, dx, dy):
        tx = bw / 2 / abs(dx) if dx else math.inf
        ty = bh / 2 / abs(dy) if dy else math.inf
        t = min(tx, ty)
        return x + dx * t, y + dy * t

    parts = []
    done = set()
    for a in found:
        for b in set(links[a]):
            if b == a or b not in at or frozenset((a, b)) in done:
                continue
            done.add(frozenset((a, b)))
            both = a in links[b]
            (ax, ay), (bx, by) = at[a], at[b]
            n = math.hypot(bx - ax, by - ay)
            ux, uy = (bx - ax) / n, (by - ay) / n
            sx, sy = border(ax, ay, ux, uy)
            ex, ey = border(bx, by, -ux, -uy)
            head = 11
            x1, y1 = (sx + ux * (head - 1), sy + uy * (head - 1)) if both else (sx, sy)
            parts.append(f"<line x1='{x1:.1f}' y1='{y1:.1f}' x2='{ex - ux * (head - 1):.1f}' y2='{ey - uy * (head - 1):.1f}' "
                         f"stroke='{GREY}' stroke-width='1.8'/>")
            parts.append(ap.arrowhead(ex, ey, ux, uy, GREY, head, 5.5))
            if both:
                parts.append(ap.arrowhead(sx, sy, -ux, -uy, GREY, head, 5.5))
    # links to itself: a loop, which counts for nothing
    x, y = at['katzenfutter-billig.html']
    parts.append(ap.curve((x - 30, y - bh / 2), (x - 60, y - 75), (x + 60, y - 75), (x + 30, y - bh / 2 - 1),
                          color=GREY, width=1.8, length=11, half=5.5))
    parts.append(text(x, y - 66, 'auf sich selbst', 14, GREY, italic=True))
    for page, (x, y) in at.items():
        hidden = page not in found
        stroke, fill, dash = (GREY, WHITE, " stroke-dasharray='6 4'") if hidden else (BLUE if page == 'index.html' else INK, PALE, '')
        parts.append(f"<rect x='{x - bw / 2:.1f}' y='{y - bh / 2:.1f}' width='{bw}' height='{bh}' rx='8' fill='{fill}' "
                     f"stroke='{stroke}' stroke-width='{2.5 if page == 'index.html' else 1.8}'{dash}/>")
        parts.append(code(page, x, y - 2, size=15, anchor='middle', background=False))
        label = 'kein Link führt hin' if hidden else f"{incoming[page]} eingehend"
        parts.append(text(x, y + 18, label, 13, GREY, halo=False))
    save('tierlexikon-links', w, h, 'Die Seiten des Tierlexikons und ihre Links', parts)


def chip(x, y, label, new=False, color=None, w=None):
    """a path as a small box with its name in code; new: just put in"""
    w = w or 12 + 8.4 * len(label)
    stroke = color or (BLUE if new else GREY)
    fill = PALE if new else WHITE
    width = 2.2 if new or color else 1.4
    return (f"<rect x='{x:.1f}' y='{y - 15:.1f}' width='{w:.1f}' height='28' rx='6' fill='{fill}' stroke='{stroke}' stroke-width='{width}'/>"
            + code(label, x + w / 2, y + 4, size=14, anchor='middle', background=False)), w


SMALL_AT = {'a': (70, 200), 'b': (205, 115), 'c': (205, 285), 'd': (340, 200), 'e': (340, 330)}


def small_site(site, at, counts=None):
    """the boxes of a small site and its links; counts: under each box how
    many other pages link to it"""
    parts = []
    bw, bh = (110, 62) if counts is not None else (96, 40)

    def border(x, y, dx, dy):
        tx = bw / 2 / abs(dx) if dx else math.inf
        ty = bh / 2 / abs(dy) if dy else math.inf
        t = min(tx, ty)
        return x + dx * t, y + dy * t
    done = set()
    for a, targets in site.items():
        for b in targets:
            if b == a:
                # a link to itself: a loop at the right
                x, y = at[a]
                parts.append(ap.curve((x + bw / 2, y - 10), (x + bw / 2 + 50, y - 40), (x + bw / 2 + 50, y + 40), (x + bw / 2 + 1, y + 10),
                                      color=GREY, width=1.8, length=11, half=5.5))
                continue
            if frozenset((a, b)) in done:
                continue
            done.add(frozenset((a, b)))
            both = a in site[b]
            (ax, ay), (bx, by) = at[a], at[b]
            n = math.hypot(bx - ax, by - ay)
            ux, uy = (bx - ax) / n, (by - ay) / n
            sx, sy = border(ax, ay, ux, uy)
            ex, ey = border(bx, by, -ux, -uy)
            head = 11
            x1, y1 = (sx + ux * (head - 1), sy + uy * (head - 1)) if both else (sx, sy)
            parts.append(f"<line x1='{x1:.1f}' y1='{y1:.1f}' x2='{ex - ux * (head - 1):.1f}' y2='{ey - uy * (head - 1):.1f}' "
                         f"stroke='{GREY}' stroke-width='1.8'/>")
            parts.append(ap.arrowhead(ex, ey, ux, uy, GREY, head, 5.5))
            if both:
                parts.append(ap.arrowhead(sx, sy, -ux, -uy, GREY, head, 5.5))
    for page, (x, y) in at.items():
        start = page == 'a' and counts is None
        parts.append(f"<rect x='{x - bw / 2:.1f}' y='{y - bh / 2:.1f}' width='{bw}' height='{bh}' rx='8' fill='{PALE}' "
                     f"stroke='{BLUE if start else INK}' stroke-width='{2.5 if start else 1.8}'/>")
        if counts is None:
            parts.append(code(f'/{page}.html', x, y + 5, size=15, anchor='middle', background=False))
        else:
            parts.append(code(f'/{page}.html', x, y - 5, size=15, anchor='middle', background=False))
            parts.append(text(x, y + 19, f'{counts[page]} eingehend', 14, BLUE, halo=False))
    if counts is None:
        parts.append(text(at['a'][0], at['a'][1] + 45, 'Start', 14, BLUE, halo=False))
    return parts


def eingehend():
    site = {'a': ['b', 'c'], 'b': ['a', 'd'], 'c': ['d'], 'd': ['e'], 'e': ['e']}
    counts = {p: sum(1 for q, t in site.items() if q != p and p in t) for p in site}
    at = {k: (x * 1.25 + 20, y * 1.15 - 75) for k, (x, y) in SMALL_AT.items()}
    w, h = 740, 362
    parts = small_site(site, at, counts)
    x, y = at['e']
    parts.append(text(x + 125, y - 4, 'ein Link auf sich', 14, GREY, 'start', italic=True))
    parts.append(text(x + 125, y + 15, 'selbst: zählt nicht', 14, GREY, 'start', italic=True))
    save('eingehend', w, h, 'Eine kleine Website und wie viele andere Seiten auf jede verlinken', parts)


def crawl_queue():
    site = {'a': ['b', 'c'], 'b': ['a', 'd'], 'c': ['d'], 'd': ['e'], 'e': []}
    at = SMALL_AT
    w, h = 1290, 400
    parts = [text(205, 34, 'die Website', 18, INK, halo=False)] + small_site(site, at)
    # the steps: what is taken, the queue after it, all seen so far
    cols = [470, 590, 830]
    parts.append(text(cols[0], 34, 'holt', 16, GREY, 'start', halo=False))
    parts.append(text(cols[1], 34, 'Queue danach', 16, GREY, 'start', halo=False))
    parts.append(text(cols[2], 34, 'schon gesehen', 16, GREY, 'start', halo=False))
    queue, seen, rows = ['a'], ['a'], [(None, ['a'], ['a'], {'a'})]
    while queue:
        page = queue.pop(0)
        new = set()
        for link in site[page]:
            if link not in seen:
                seen.append(link)
                queue.append(link)
                new.add(link)
        rows.append((page, list(queue), list(seen), new))
    for i, (taken, q, sn, new) in enumerate(rows):
        y = 74 + i * 52
        if i:
            parts.append(f"<line x1='{cols[0] - 10}' y1='{y - 26}' x2='{w - 20}' y2='{y - 26}' stroke='#d1d5db' stroke-width='1'/>")
        if taken:
            parts.append(chip(cols[0], y, f'/{taken}.html')[0])
        else:
            parts.append(text(cols[0], y + 5, 'am Anfang', 15, GREY, 'start', italic=True, halo=False))
        x = cols[1]
        if not q:
            parts.append(text(x, y + 5, 'leer: fertig', 15, GREY, 'start', italic=True, halo=False))
        for p_ in q:
            c, cw = chip(x, y, f'/{p_}.html', p_ in new)
            parts.append(c)
            x += cw + 6
        x = cols[2]
        for p_ in sn:
            c, cw = chip(x, y, f'/{p_}.html', p_ in new)
            parts.append(c)
            x += cw + 6
    save('crawl-queue', w, h, 'Die Queue des Crawlers auf einer kleinen Website', parts)


def index_umgedreht():
    pages = [('/a.html', 'Der Igel schläft.', BLUE, PALE), ('/b.html', 'Der Hund schläft nicht.', AMBER, PALE_AMBER)]
    w, h = 1000, 330
    parts = [text(170, 34, 'jede Seite: ihre Wörter', 17, INK, halo=False),
             text(720, 34, 'der Index: zu jedem Wort die Seiten', 17, INK, halo=False)]
    for i, (path, sentence, color, pale) in enumerate(pages):
        y = 80 + i * 120
        parts.append(f"<rect x='30' y='{y}' width='280' height='92' rx='10' fill='{WHITE}' stroke='{color}' stroke-width='2.4'/>")
        parts.append(code(path, 48, y + 26, size=15, anchor='start', background=False))
        parts.append(text(48, y + 64, sentence, 17, INK, 'start', halo=False))
    parts.append(f"<line x1='340' y1='175' x2='478' y2='175' stroke='{GREY}' stroke-width='2'/>")
    parts.append(ap.arrowhead(490, 175, 1, 0, GREY, 12, 6))
    parts.append(text(415, 160, 'umgedreht', 15, GREY, halo=False))
    index = [('der', [0, 1]), ('igel', [0]), ('schläft', [0, 1]), ('hund', [1]), ('nicht', [1])]
    for i, (word, on) in enumerate(index):
        y = 82 + i * 50
        parts.append(code(word, 560, y + 4, size=16, anchor='start', background=False))
        x = 680
        for k in on:
            c, cw = chip(x, y, pages[k][0], color=pages[k][2])
            parts.append(c)
            x += cw + 8
    save('index-umgedreht', w, h, 'Zwei Seiten und der invertierte Index daraus', parts)


def schnittmenge():
    w, h = 760, 370
    cl, cr, cy, r = 295, 465, 185, 140
    parts = []
    parts.append(f"<circle cx='{cl}' cy='{cy}' r='{r}' fill='{BLUE}' fill-opacity='0.10' stroke='{BLUE}' stroke-width='2.4'/>")
    parts.append(f"<circle cx='{cr}' cy='{cy}' r='{r}' fill='{AMBER}' fill-opacity='0.12' stroke='{AMBER}' stroke-width='2.4'/>")
    parts.append(code('katze', cl - 80, 40, size=17, anchor='middle', background=False))
    parts.append(code('hund', cr + 80, 40, size=17, anchor='middle', background=False))
    for label, x, y in [('/a.html', cl - 75, cy), ('/c.html', cr + 75, cy), ('/b.html', 380, cy - 22), ('/d.html', 380, cy + 26)]:
        c, cw = chip(x - 38, y, label, new=label in ('/b.html', '/d.html'), w=76)
        parts.append(c)
    parts.append(text(380, 356, 'Treffer für »katze hund«: wo beide Kreise sich überlappen', 16, INK, halo=False))
    save('schnittmenge', w, h, 'Die Seiten zweier Wörter und ihre Schnittmenge', parts)


drei_teile()
tierlexikon_links()
crawl_queue()
eingehend()
index_umgedreht()
schnittmenge()
