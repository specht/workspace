"""The pictures of »Logisch programmieren mit Prolog« drawn in Python, as
SVG beside the page.

    python3 src/content/prolog/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/prolog/NAME.svg WIDTH

- stammbaum: the facts of familie.pl with the children of Anna Magdalena
  (the task »Die Kinder von Anna Magdalena«), every parent(P, C) an arrow
  from P to C, the generations from left to right; the facts the task adds
  in blue.
- bezirke: the twelve districts of Berlin roughly where they lie, a line
  between two neighbours, as in the table on the page.
- bezirke-farben: the same with the colouring Prolog finds first for
  loesung/bezirke.pl (berlin(Cw, Fk, Li, Mh, Mi, Nk, Pa, Re, Sp, Sz, Ts, Tk)),
  each district a ring in its colour with the colour's name.
"""

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
PALE = '#eef3fb'
WHITE = '#ffffff'
COLOURS = {'rot': '#dc2626', 'gelb': '#eab308', 'gruen': '#16a34a', 'blau': '#2b5fb4'}


def save(name, w, h, comment, parts, scale=2):
    render = f'src/scripts/render-svg.rb src/content/prolog/{name}.svg {scale * w}'
    head = ap.svg_head(w, h, comment + '; gemacht von bilder/diagramme.py, als Bild:', render)
    head = head.replace('<svg ', f"<svg font-family='{FONT}' ", 1)
    with open(os.path.join(OUT, name + '.svg'), 'w') as f:
        f.write(head + f"<rect width='{w}' height='{h}' fill='{WHITE}'/>" + ''.join(parts) + '</svg>\n')


def text(x, y, s, size=16, color=INK, anchor='middle'):
    # a halo in the background's colour, so that no line runs through a name
    return (f"<text x='{x:.1f}' y='{y:.1f}' font-size='{size}' fill='{color}' text-anchor='{anchor}' "
            f"stroke='{WHITE}' stroke-width='5' stroke-linejoin='round' paint-order='stroke'>{s}</text>")


def stammbaum():
    # columns are generations; (column, row y) per person
    cx = [75, 280, 505, 790, 1100]
    widths = [120, 180, 180, 270, 280]
    at = {
        'christoph': (0, 250),
        'johann_michael': (1, 90), 'johann_ambrosius': (1, 250), 'maria_elisabeth': (1, 330),
        'maria_barbara': (2, 90), 'johann_sebastian': (2, 228), 'anna_magdalena': (2, 340),
        'johann_christoph': (2, 420), 'johann_jacob': (2, 470),
        'catharina_dorothea': (3, 40), 'wilhelm_friedemann': (3, 92), 'carl_philipp_emanuel': (3, 144),
        'johann_gottfried_bernhard': (3, 196), 'gottfried_heinrich': (3, 248),
        'elisabeth_juliane_friederica': (3, 300), 'johann_christoph_friedrich': (3, 352), 'johann_christian': (3, 404),
        'johann_sebastian_der_juengere': (4, 144), 'wilhelm_friedrich_ernst': (4, 352),
    }
    new = ['gottfried_heinrich', 'elisabeth_juliane_friederica', 'johann_christoph_friedrich', 'johann_christian']
    facts = [('christoph', 'johann_ambrosius'), ('johann_michael', 'maria_barbara')]
    for p in ['johann_ambrosius', 'maria_elisabeth']:
        facts += [(p, c) for c in ['johann_christoph', 'johann_jacob', 'johann_sebastian']]
    first = ['catharina_dorothea', 'wilhelm_friedemann', 'carl_philipp_emanuel', 'johann_gottfried_bernhard']
    for p in ['johann_sebastian', 'maria_barbara']:
        facts += [(p, c) for c in first]
    for p in ['johann_sebastian', 'anna_magdalena']:
        facts += [(p, c) for c in new]
    facts += [('carl_philipp_emanuel', 'johann_sebastian_der_juengere'),
              ('johann_christoph_friedrich', 'wilhelm_friedrich_ernst')]
    h = 30
    parts = []
    for p, c in facts:
        (pc, py), (cc, cy) = at[p], at[c]
        x0, x3 = cx[pc] + widths[pc] / 2, cx[cc] - widths[cc] / 2
        length = x3 - x0
        # leaves to the right, arrives along the line to the child's centre
        color = BLUE if c in new else GREY
        parts.append(ap.curve((x0, py), (x0 + 0.45 * length, py), (x3 - 0.4 * length, cy), (x3, cy),
                              color=color, width=1.6, length=9, half=4.5))
    for name, (col, y) in at.items():
        w = widths[col]
        stroke, width = (BLUE, 2.5) if name in new else (INK, 1.5)
        parts.append(f"<rect x='{cx[col] - w / 2:.1f}' y='{y - h / 2:.1f}' width='{w}' height='{h}' rx='7' "
                     f"fill='{PALE}' stroke='{stroke}' stroke-width='{width}'/>")
        parts.append(code(name, cx[col], y + 5, size=14, anchor='middle', background=False))
    save('stammbaum', 1250, 500, 'Die Familie Bach aus familie.pl: jeder parent-Fakt ein Pfeil', parts)


DISTRICTS = {
    'Re': ('Reinickendorf', 300, 95), 'Pa': ('Pankow', 530, 85), 'Sp': ('Spandau', 95, 255),
    'Cw': ('Charlottenburg-Wilmersdorf', 290, 285), 'Mi': ('Mitte', 465, 225),
    'Fk': ('Friedrichshain-Kreuzberg', 600, 310), 'Li': ('Lichtenberg', 745, 215),
    'Mh': ('Marzahn-Hellersdorf', 880, 140), 'Sz': ('Steglitz-Zehlendorf', 215, 470),
    'Ts': ('Tempelhof-Schöneberg', 440, 430), 'Nk': ('Neukölln', 600, 480), 'Tk': ('Treptow-Köpenick', 800, 455),
}
NEIGHBOURS = [('Cw', 'Mi'), ('Cw', 'Re'), ('Cw', 'Sp'), ('Cw', 'Sz'), ('Cw', 'Ts'),
              ('Fk', 'Li'), ('Fk', 'Mi'), ('Fk', 'Nk'), ('Fk', 'Pa'), ('Fk', 'Ts'), ('Fk', 'Tk'),
              ('Li', 'Mh'), ('Li', 'Pa'), ('Li', 'Tk'), ('Mh', 'Tk'),
              ('Mi', 'Pa'), ('Mi', 'Re'), ('Mi', 'Ts'), ('Nk', 'Ts'), ('Nk', 'Tk'),
              ('Pa', 'Re'), ('Re', 'Sp'), ('Sp', 'Sz'), ('Sz', 'Ts')]
# the first answer of berlin(Cw, Fk, Li, Mh, Mi, Nk, Pa, Re, Sp, Sz, Ts, Tk) for loesung/bezirke.pl
FIRST = dict(zip(['Cw', 'Fk', 'Li', 'Mh', 'Mi', 'Nk', 'Pa', 'Re', 'Sp', 'Sz', 'Ts', 'Tk'],
                 ['gelb', 'gruen', 'rot', 'gruen', 'rot', 'rot', 'gelb', 'gruen', 'rot', 'gruen', 'blau', 'gelb']))


def bezirke(name, colours=None):
    w, h = 1000, 560
    r = 30
    parts = []
    # edges centre to centre, under the circles
    for a, b in NEIGHBOURS:
        _, ax, ay = DISTRICTS[a]
        _, bx, by = DISTRICTS[b]
        parts.append(f"<line x1='{ax}' y1='{ay}' x2='{bx}' y2='{by}' stroke='{GREY}' stroke-width='2'/>")
    for var, (full, x, y) in DISTRICTS.items():
        stroke, width = (COLOURS[colours[var]], 7) if colours else (INK, 2)
        parts.append(f"<circle cx='{x}' cy='{y}' r='{r}' fill='{WHITE}' stroke='{stroke}' stroke-width='{width}'/>")
        parts.append(code(var, x, y + 6, size=18, anchor='middle', background=False))
        parts.append(text(x, y + r + 22, full, 15))
        if colours:
            parts.append(text(x, y + r + 40, colours[var].replace('gruen', 'grün'), 14, COLOURS[colours[var]]))
    if colours:
        comment = 'Die Bezirke Berlins in der ersten Färbung, die Prolog findet'
    else:
        comment = 'Die Bezirke Berlins und ihre Nachbarn, ungefähr da, wo sie liegen'
    save(name, w, h, comment, parts)


stammbaum()
bezirke('bezirke')
bezirke('bezirke-farben', FIRST)
