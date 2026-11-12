# The tables of »Zellen, die rechnen«: the trip's costs as a parent typed
# them, every result worked out on a calculator (the starting file), the
# same table calculating (the solution, task-examples.yaml) and one with
# formulas in the rows but the total typed (a stage that must fail). Run in
# the office image:
#
#     src/scripts/office python3 src/content/spreadsheet-cells/dokumente.py

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from calc_build import Calc, put, fmt, bold, widths  # noqa: E402

STATIC = os.path.join(HERE, '..', '..', 'static', 'spreadsheet-cells')
STARTER = '01 Kosten.xlsx'
# 27 children and 2 teachers; the bus is one price for all
ROWS = [
    ('Bus, hin und zurück', 1, 1250),
    ('Jugendherberge, 4 Nächte', 29, 148),
    ('Kletterwald', 27, 24.5),
    ('Schifffahrt', 29, 9),
    ('Moormuseum', 29, 4.5),
]


def table(calc, path, rows='typed', total='typed'):
    """rows, total: 'typed' (worked out and typed) or 'formula'"""
    doc = calc.new()
    put(doc, 'A1', [['Kosten der Klassenfahrt'], [], ['Posten', 'Anzahl', 'Preis', 'Summe']])
    for i, (name, count, price) in enumerate(ROWS):
        r = 4 + i
        put(doc, f'A{r}', [[name, count, price, f'=B{r}*C{r}' if rows == 'formula' else count * price]])
    last = 3 + len(ROWS)
    put(doc, f'A{last + 1}', [['Gesamt', None, None, f'=SUM(D4:D{last})' if total == 'formula' else sum(c * p for _, c, p in ROWS)]])
    bold(doc, f'A1, A3:D3, A{last + 1}:D{last + 1}')
    fmt(doc, f'C4:D{last + 1}', '#.##0,00')
    widths(doc, [5.4, 2.0, 2.2, 2.6])
    calc.save(doc, path)


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    for folder in ('stufen', 'bilder'):
        os.makedirs(os.path.join(HERE, folder), exist_ok=True)
    with Calc() as calc:
        starter = os.path.join(STATIC, STARTER)
        table(calc, starter)
        table(calc, os.path.join(HERE, 'stufen', 'kosten.xlsx'), rows='formula', total='formula')
        table(calc, os.path.join(HERE, 'stufen', 'gesamt-getippt.xlsx'), rows='formula', total='typed')
        calc.picture(starter, os.path.join(HERE, 'bilder', 'kosten.svg'))
    print('written: static/spreadsheet-cells/01 Kosten.xlsx, stufen/*.xlsx, bilder/kosten.svg')
