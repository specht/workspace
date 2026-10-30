#!/usr/bin/env python3
"""The pictures of »Ein winziges Sprachmodell« drawn in Python, in the style
of the card picture (bilder/teaser.py): words in boxes, arrows as thick as
they were counted.

- folgen: the dictionary of »Der Wolf kam. Der Wolf fraß. Der Jäger kam.«
  as a graph, every word once, an arrow to each word that followed it with
  how often. Writing a tale is walking along the arrows.
- kontext: »Da sprach der Wolf« twice, once with one word as context and
  once with two, each time an arrow to the word that follows.
- wuerfeln: the random numbers 0 to 9 as a strip, six fields for »war«,
  three for »ist«, one for »trug«, the drawn 7 marked: it lands on »ist«.

    python3 src/content/sprachmodell/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/sprachmodell/folgen.svg 1440
    ruby src/scripts/render-svg.rb src/content/sprachmodell/wuerfeln.svg 1280
    ruby src/scripts/render-svg.rb src/content/sprachmodell/kontext.svg 880
"""

import math
from pathlib import Path

SANS = "'IBM Plex Sans', system-ui, sans-serif"
TEXT, BLUE, BOX, GREY, PURPLE = "#1f2937", "#2b5fb4", "#eef3fb", "#6b7280", "#7c3aed"
OUT = Path(__file__).resolve().parent.parent


def width_for(count):
    return 2 + 2.5 * count


def border(box, towards):
    """Where the line from the box's centre towards a point leaves the box."""
    cx, cy, w, h = box
    dx, dy = towards[0] - cx, towards[1] - cy
    t = min((w / 2) / abs(dx) if dx else math.inf, (h / 2) / abs(dy) if dy else math.inf)
    return cx + dx * t, cy + dy * t


def head(tip, angle, width):
    """An arrowhead with its tip on tip, pointing along angle."""
    length = 9 + 1.6 * width
    spread = length * 0.5 + width / 2
    base = (tip[0] - length * math.cos(angle), tip[1] - length * math.sin(angle))
    left = (base[0] + spread * math.sin(angle), base[1] - spread * math.cos(angle))
    right = (base[0] - spread * math.sin(angle), base[1] + spread * math.cos(angle))
    return base, (f"<polygon points='{tip[0]:.1f},{tip[1]:.1f} {left[0]:.1f},{left[1]:.1f} "
                  f"{right[0]:.1f},{right[1]:.1f}' fill='{BLUE}'/>")


def line(a, b, count):
    """A straight arrow from box a to box b, aimed at their centres."""
    start, end = border(a, b[:2]), border(b, a[:2])
    angle = math.atan2(end[1] - start[1], end[0] - start[0])
    w = width_for(count)
    base, tip = head(end, angle, w)
    return (f"<line x1='{start[0]:.1f}' y1='{start[1]:.1f}' x2='{base[0]:.1f}' y2='{base[1]:.1f}' "
            f"stroke='{BLUE}' stroke-width='{w:.1f}' stroke-linecap='round'/>" + tip)


def label(x, y, count):
    return (f"<text x='{x}' y='{y}' text-anchor='middle' font-family=\"{SANS}\" font-size='17' "
            f"font-weight='600' fill='{GREY}'>{count}</text>")


def word(box, text):
    cx, cy, w, h = box
    return (f"<rect x='{cx - w / 2}' y='{cy - h / 2}' width='{w}' height='{h}' rx='10' fill='{BOX}' "
            f"stroke='{BLUE}' stroke-width='2'/>"
            f"<text x='{cx}' y='{cy + 7.5}' text-anchor='middle' font-family=\"{SANS}\" font-size='21' "
            f"fill='{TEXT}'>{text}</text>")


def folgen():
    w, h = 720, 275
    boxes = {
        'Der': (80, 125, 84, 46),
        'Wolf': (260, 60, 84, 46),
        'Jäger': (260, 190, 92, 46),
        'fraß': (450, 60, 84, 46),
        'kam': (450, 190, 84, 46),
        '.': (630, 125, 54, 46),
    }
    edges = [('Der', 'Wolf', 2), ('Der', 'Jäger', 1), ('Wolf', 'fraß', 1), ('Wolf', 'kam', 1),
             ('Jäger', 'kam', 1), ('fraß', '.', 1), ('kam', '.', 2)]
    # where each count stands, beside the middle of its arrow
    places = {('Der', 'Wolf'): (158, 76), ('Der', 'Jäger'): (158, 182), ('Wolf', 'fraß'): (355, 48),
              ('Wolf', 'kam'): (372, 117), ('Jäger', 'kam'): (355, 178), ('fraß', '.'): (553, 77),
              ('kam', '.'): (555, 182)}
    parts = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {w} {h}' width='{w}' height='{h}'>",
             f"  <!-- Das Dictionary von »Der Wolf kam. Der Wolf fraß. Der Jäger kam.« als Graph; "
             f"gemacht von bilder/diagramme.py, als Bild:\n       "
             f"ruby src/scripts/render-svg.rb src/content/sprachmodell/folgen.svg 1440 -->",
             f"<rect width='{w}' height='{h}' fill='#ffffff'/>"]
    for a, b, count in edges:
        parts.append(line(boxes[a], boxes[b], count))
        parts.append(label(*places[(a, b)], count))
    # ».« back to »Der«: an arc underneath, its tip on the bottom of »Der«
    p0 = (boxes['.'][0], boxes['.'][1] + boxes['.'][3] / 2)
    p3 = (boxes['Der'][0], boxes['Der'][1] + boxes['Der'][3] / 2)
    p1, p2 = (p0[0], 290), (p3[0], 290)
    wb = width_for(2)
    base, tip = head(p3, -math.pi / 2, wb)
    parts.append(f"<path d='M{p0[0]} {p0[1]} C{p1[0]} {p1[1]} {p2[0]} {p2[1]} {base[0]:.1f} {base[1]:.1f}' "
                 f"fill='none' stroke='{BLUE}' stroke-width='{wb}' stroke-linecap='round'/>" + tip)
    parts.append(label(355, 238, 2))
    for name, box in boxes.items():
        parts.append(word(box, name))
    parts.append('</svg>')
    (OUT / 'folgen.svg').write_text('\n'.join(parts) + '\n')


def wuerfeln():
    w, h = 640, 190
    size, x0, y = 52, 60, 70
    words = [('war', 6), ('ist', 3), ('trug', 1)]
    drawn = 7
    parts = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {w} {h}' width='{w}' height='{h}'>",
             f"  <!-- Die Zufallszahlen 0 bis 9 für war, ist und trug; gemacht von bilder/diagramme.py, als Bild:\n       "
             f"ruby src/scripts/render-svg.rb src/content/sprachmodell/wuerfeln.svg 1280 -->",
             f"<rect width='{w}' height='{h}' fill='#ffffff'/>"]
    for k in range(10):
        x = x0 + k * size
        mark = k == drawn
        parts.append(f"<rect x='{x}' y='{y}' width='{size}' height='{size}' fill='{BOX if mark else '#ffffff'}' "
                     f"stroke='{PURPLE if mark else TEXT}' stroke-width='{3 if mark else 1.5}'/>")
        parts.append(f"<text x='{x + size / 2}' y='{y + size / 2 + 7}' text-anchor='middle' font-family=\"{SANS}\" "
                     f"font-size='20' fill='{TEXT}'>{k}</text>")
    # the marked field drawn on top, so its outline is whole
    xm = x0 + drawn * size
    parts.append(f"<rect x='{xm}' y='{y}' width='{size}' height='{size}' fill='none' stroke='{PURPLE}' stroke-width='3'/>")
    parts.append(f"<text x='{xm + size / 2}' y='{y - 34}' text-anchor='middle' font-family=\"{SANS}\" font-size='16' "
                 f"fill='{PURPLE}'>gezogen: {drawn}</text>")
    base, tip = head((xm + size / 2, y - 4), math.pi / 2, 2)
    parts.append(f"<line x1='{xm + size / 2}' y1='{y - 26}' x2='{base[0]:.1f}' y2='{base[1]:.1f}' stroke='{PURPLE}' "
                 f"stroke-width='2'/>" + tip.replace(BLUE, PURPLE))
    # under each word's fields a bracket and the word with its count
    first = 0
    for name, count in words:
        a, b = x0 + first * size + 4, x0 + (first + count) * size - 4
        yb = y + size + 12
        parts.append(f"<path d='M{a} {yb - 6} L{a} {yb} L{b} {yb} L{b} {yb - 6}' fill='none' stroke='{BLUE}' stroke-width='2'/>")
        parts.append(f"<text x='{(a + b) / 2}' y='{yb + 28}' text-anchor='middle' font-family=\"{SANS}\" font-size='20' "
                     f"fill='{TEXT}'>{name}</text>")
        parts.append(f"<text x='{(a + b) / 2}' y='{yb + 50}' text-anchor='middle' font-family=\"{SANS}\" font-size='15' "
                     f"font-weight='600' fill='{GREY}'>{count}-mal</text>")
        first += count
    parts.append('</svg>')
    (OUT / 'wuerfeln.svg').write_text('\n'.join(parts) + '\n')


def kontext():
    w, h = 440, 230
    words = ['Da', 'sprach', 'der', 'Wolf']
    widths = [62, 104, 66, 84]
    gap, x0 = 14, 40
    xs = []
    x = x0
    for wd in widths:
        xs.append(x)
        x += wd + gap
    parts = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {w} {h}' width='{w}' height='{h}'>",
             f"  <!-- Ein und zwei Wörter Kontext; gemacht von bilder/diagramme.py, als Bild:\n       "
             f"ruby src/scripts/render-svg.rb src/content/sprachmodell/kontext.svg 880 -->",
             f"<rect width='{w}' height='{h}' fill='#ffffff'/>"]
    for row, (y, context, title) in enumerate([(40, [2], 'Kontext'), (150, [1, 2], 'Kontext')]):
        for k, (wd, word_text) in enumerate(zip(widths, words)):
            inside = k in context
            nxt = k == 3
            fill = BOX if inside else '#ffffff'
            stroke = PURPLE if nxt else (BLUE if inside else TEXT)
            parts.append(f"<rect x='{xs[k]}' y='{y}' width='{wd}' height='40' rx='8' fill='{fill}' stroke='{stroke}' "
                         f"stroke-width='{2.5 if inside or nxt else 1.5}'/>")
            parts.append(f"<text x='{xs[k] + wd / 2}' y='{y + 27}' text-anchor='middle' font-family=\"{SANS}\" "
                         f"font-size='20' fill='{TEXT}'>{word_text}</text>")
        # a bracket under the context, an arrow from it to the next word
        a, b = xs[context[0]] + 4, xs[context[-1]] + widths[context[-1]] - 4
        yb = y + 50
        parts.append(f"<path d='M{a} {yb - 5} L{a} {yb} L{b} {yb} L{b} {yb - 5}' fill='none' stroke='{BLUE}' stroke-width='2'/>")
        parts.append(f"<text x='{(a + b) / 2}' y='{yb + 22}' text-anchor='middle' font-family=\"{SANS}\" font-size='15' "
                     f"fill='{BLUE}'>{title}</text>")
        tx = xs[3] + widths[3] / 2
        parts.append(f"<text x='{tx}' y='{yb + 22}' text-anchor='middle' font-family=\"{SANS}\" font-size='15' "
                     f"fill='{PURPLE}'>danach</text>")
        mid = (a + b) / 2
        p0, p3 = (mid, y - 4), (tx, y - 4)
        base, tip = head(p3, math.pi / 2 - 0.5, 2)
        parts.append(f"<path d='M{p0[0]} {p0[1]} C{p0[0]} {y - 30} {tx - 30} {y - 34} {base[0]:.1f} {base[1]:.1f}' "
                     f"fill='none' stroke='{PURPLE}' stroke-width='2'/>" + tip.replace(BLUE, PURPLE))
    parts.append('</svg>')
    (OUT / 'kontext.svg').write_text('\n'.join(parts) + '\n')


folgen()
wuerfeln()
kontext()
