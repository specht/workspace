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


drei_teile()
tierlexikon_links()
