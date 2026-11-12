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


def table(calc, path, share=None, discount=None):
    """share, discount: a formula for row r, a pattern ('=B{r}/$B$2') or a
    function of r; None: the share only in C6, as the parent wrote it, and no
    discount column"""
    def formula(pattern, r):
        return pattern(r) if callable(pattern) else pattern.format(r=r)
    doc = calc.new()
    put(doc, 'A1', [['Was ein Kind bezahlt'],
                    ['Kinder', CHILDREN],
                    ['Ermäßigung für Geschwister', DISCOUNT],
                    [],
                    ['Posten', 'Kosten', 'pro Kind', 'mit Ermäßigung']])
    last = 5 + len(ROWS)
    for i, (name, cost) in enumerate(ROWS):
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
    bold(doc, f'A1, A5:D5, A{last + 1}:D{last + 1}')
    fmt(doc, f'B6:B{last + 1}', CURRENCY)
    fmt(doc, 'B3', '0 %')
    if share and discount:
        fmt(doc, f'C6:D{last + 1}', CURRENCY)
    widths(doc, [6.0, 2.6, 2.6, 3.9])
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
        calc.picture(starter, os.path.join(HERE, 'bilder', 'anteile.svg'))
        calc.picture(os.path.join(stufen, 'anteile.xlsx'), os.path.join(HERE, 'bilder', 'anteile-fertig.svg'))
        calc.picture(os.path.join(stufen, 'ohne-dollar.xlsx'), os.path.join(HERE, 'bilder', 'ohne-dollar.svg'))
    print('written: static/absolute-references/03 Anteile.xlsx, stufen/*.xlsx, bilder/*.svg')
