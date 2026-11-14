# The tables of »Feste Bezüge«: what one child pays for the trip. The
# children's costs (the teachers are not the families' to pay) divided by
# the number of children in B2, then less the siblings' discount in B3. The
# starting file: one formula, =B6/B2, the parent's, in C6. The solution:
# C6:C10 divide by $B$2, D6:D10 take off $B$3, sums below, all as currency.
# Stages that must fail: the formula filled down without $ (#DIV/0! and
# nonsense), and the number of children typed into the formulas (=B6/27).
# Run in the office image:
#
#     src/scripts/office python3 src/content/absolute-references/dokumente.py

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from calc_build import Calc, put, fmt, bold, widths  # noqa: E402

STATIC = os.path.join(HERE, '..', '..', 'static', 'absolute-references')
STARTER = '03 Anteile.xlsx'
CHILDREN = 27
DISCOUNT = 0.05
# the children's part of each cost (sheet 1: the bus is one price for all)
ROWS = [
    ('Bus, hin und zurück', 1250),
    ('Jugendherberge, 4 Nächte', CHILDREN * 148),
    ('Kletterwald', CHILDREN * 24.5),
    ('Schifffahrt', CHILDREN * 9),
    ('Moormuseum', CHILDREN * 4.5),
]
CURRENCY = '#.##0,00 [$€-407]'


def table(calc, path, share=None, discount=None, title='Was ein Kind bezahlt', children=CHILDREN, rate=DISCOUNT, rows=ROWS,
          cells=None, plain=''):
    """share, discount: a formula for row r, a pattern ('=B{r}/$B$2') or a
    function of r; None: the share only in C6, as the parent wrote it, and no
    discount column; cells: a cell's own content instead; plain: cells left
    without the currency format"""
    def formula(pattern, r):
        return pattern(r) if callable(pattern) else pattern.format(r=r)
    doc = calc.new()
    put(doc, 'A1', [[title],
                    ['Kinder', children],
                    ['Ermäßigung für Geschwister', rate],
                    [],
                    ['Posten', 'Kosten', 'pro Kind', 'mit Ermäßigung']])
    last = 5 + len(rows)
    for i, (name, cost) in enumerate(rows):
        r = 6 + i
        row = [name, cost]
        if share:
            row.append(formula(share, r))
            if discount:
                row.append(formula(discount, r))
        elif r == 6:
            row.append('=B6/B2')
        put(doc, f'A{r}', [row])
    total = ['Zusammen', f'=SUM(B6:B{last})']
    if share:
        total.append(f'=SUM(C6:C{last})')
        if discount:
            total.append(f'=SUM(D6:D{last})')
    put(doc, f'A{last + 1}', [total])
    for name, value in (cells or {}).items():
        put(doc, name, [[value]])
    bold(doc, f'A1, A5:D5, A{last + 1}:D{last + 1}')
    fmt(doc, f'B6:B{last + 1}', CURRENCY)
    fmt(doc, 'B3', '0 %')
    if share and discount:
        fmt(doc, f'C6:D{last + 1}', CURRENCY)
    if plain:
        fmt(doc, plain, '0,########')
    widths(doc, [6.0, 2.6, 2.6, 3.9])
    calc.save(doc, path)


# »Noch eine Runde«: what a child pays for the sports day, 28 children,
# 10 % for siblings
SPORTS = [('Bus, hin und zurück', 420), ('Medaillen', 28 * 1.8), ('Urkunden', 28 * 0.5), ('Getränke', 28 * 1.25)]

# »Fünf Fallen«: what a child pays for the hiking day, 26 children; C7
# divides by B3 (C6's formula filled down without $), C8 by a typed 26, D9
# takes off a typed 5 %, D6 has lost its currency format (a trap of the
# second sheet), the total of C one row short (of the first)
HIKE = [('Bus, hin und zurück', 380), ('Eintritt Tierpark', 26 * 6.5), ('Führung', 90), ('Picknick', 26 * 3.2)]
HIKE_TRAPS = {'C7': '=B7/B3', 'C8': '=B8/26', 'D9': '=C9*0.95', 'C10': '=SUM(C6:C8)'}


def prices(calc, path, solved):
    """»Zwei Handgriffe«: what the trip costs a class of 25, 27 or 29
    children, one formula with mixed references for the whole grid
    (=$B4*C$3), and each item's share of the total cost per child
    (=B4/$B$8, as percent)"""
    doc = calc.new()
    put(doc, 'A1', [['Was die Fahrt für eine Klasse kostet'], [],
                    ['Posten', 'pro Kind', 25, 27, 29, 'Anteil']])
    items = [('Jugendherberge, 4 Nächte', 148), ('Kletterwald', 24.5), ('Schifffahrt', 9), ('Moormuseum', 4.5)]
    for i, (name, cost) in enumerate(items):
        r = 4 + i
        put(doc, f'A{r}', [[name, cost] + ([f'=$B{r}*C$3', f'=$B{r}*D$3', f'=$B{r}*E$3', f'=B{r}/$B$8'] if solved else [])])
    put(doc, 'A8', [['Zusammen', '=SUM(B4:B7)']])
    fmt(doc, 'B4:E8', CURRENCY)
    fmt(doc, 'C3:E3', '0" Kinder"')
    if solved:
        fmt(doc, 'F4:F7', '0,0 %')
    bold(doc, 'A1, A3:F3, A8:B8')
    widths(doc, [5.4, 2.4, 2.8, 2.8, 2.8, 2.2])
    calc.save(doc, path)


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    for folder in ('stufen', 'bilder'):
        os.makedirs(os.path.join(HERE, folder), exist_ok=True)
    stufen = os.path.join(HERE, 'stufen')
    with Calc() as calc:
        starter = os.path.join(STATIC, STARTER)
        table(calc, starter)
        table(calc, os.path.join(stufen, 'anteile.xlsx'), '=B{r}/$B$2', '=C{r}*(1-$B$3)')
        # only the row fixed: enough for filling down
        table(calc, os.path.join(stufen, 'zeile-fest.xlsx'), '=B{r}/B$2', '=C{r}*(1-B$3)')
        # C6's formula filled down as it stands: each row divides by the cell
        # four rows up, B3, B4 … instead of B2
        table(calc, os.path.join(stufen, 'ohne-dollar.xlsx'), lambda r: f'=B{r}/B{r - 4}', lambda r: f'=C{r}*(1-B{r - 3})')
        table(calc, os.path.join(stufen, 'getippt.xlsx'), '=B{r}/27', '=C{r}*0.95')
        # Üben und knobeln
        sports = dict(title='Was ein Kind fürs Sportfest bezahlt', children=28, rate=0.1, rows=SPORTS)
        table(calc, os.path.join(STATIC, '03 Sportfest.xlsx'), **sports)
        table(calc, os.path.join(stufen, 'sportfest.xlsx'), '=B{r}/$B$2', '=C{r}*(1-$B$3)', **sports)
        prices(calc, os.path.join(STATIC, '03 Preise.xlsx'), False)
        prices(calc, os.path.join(stufen, 'preise.xlsx'), True)
        hike = dict(title='Was ein Kind beim Wandertag bezahlt', children=26, rate=0.05, rows=HIKE)
        table(calc, os.path.join(STATIC, '03 Wandertag.xlsx'), '=B{r}/$B$2', '=C{r}*(1-$B$3)', cells=HIKE_TRAPS, plain='D6', **hike)
        table(calc, os.path.join(stufen, 'wandertag.xlsx'), '=B{r}/$B$2', '=C{r}*(1-$B$3)', **hike)
        # four traps fixed, the total of C still one row short
        table(calc, os.path.join(stufen, 'wandertag-vier.xlsx'), '=B{r}/$B$2', '=C{r}*(1-$B$3)', cells={'C10': '=SUM(C6:C8)'}, **hike)
        calc.picture(starter, os.path.join(HERE, 'bilder', 'anteile.svg'))
        calc.picture(os.path.join(stufen, 'anteile.xlsx'), os.path.join(HERE, 'bilder', 'anteile-fertig.svg'))
        calc.picture(os.path.join(stufen, 'ohne-dollar.xlsx'), os.path.join(HERE, 'bilder', 'ohne-dollar.svg'))
    print('written: static/absolute-references/03 *.xlsx, stufen/*.xlsx, bilder/*.svg')
