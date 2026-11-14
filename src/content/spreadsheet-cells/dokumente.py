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


def table(calc, path, rows='typed', total='typed', title='Kosten der Klassenfahrt', items=ROWS, cells=None, people=None):
    """rows, total: 'typed' (worked out and typed) or 'formula'; cells: a
    cell's own content instead ({'D6': '=2*45'}); people: a line »Personen«
    two rows below the total"""
    doc = calc.new()
    put(doc, 'A1', [[title], [], ['Posten', 'Anzahl', 'Preis', 'Summe']])
    for i, (name, count, price) in enumerate(items):
        r = 4 + i
        put(doc, f'A{r}', [[name, count, price, f'=B{r}*C{r}' if rows == 'formula' else round(count * price, 2)]])
    last = 3 + len(items)
    put(doc, f'A{last + 1}', [['Gesamt', None, None, f'=SUM(D4:D{last})' if total == 'formula' else round(sum(c * p for _, c, p in items), 2)]])
    for name, value in (cells or {}).items():
        put(doc, name, [[value]])
    bold(doc, f'A1, A3:D3, A{last + 1}:D{last + 1}')
    fmt(doc, f'C4:D{last + 1}', '#.##0,00')
    if people:
        put(doc, f'A{last + 3}', [['Personen', people]])
        # the cell for the cost per person (D13 once a row is inserted
        # above the total) formatted like the sums: with all its decimals
        # the result is wider than the column (#####)
        fmt(doc, f'D{last + 4}', '#.##0,00')
    widths(doc, [5.4, 2.0, 2.2, 2.6])
    calc.save(doc, path)


# »Noch eine Runde«: the shopping for the 7b's summer party, as a parent
# worked it out on a calculator
PARTY = [
    ('Würstchen', 60, 0.45),
    ('Brötchen', 60, 0.35),
    ('Apfelsaft, Flasche', 12, 1.29),
    ('Wasser, Kasten', 3, 5.49),
    ('Servietten, Packung', 2, 1.99),
    ('Pappteller, Packung', 3, 2.49),
]

# »Fünf Fallen«: the costs of the hiking day, finished at first sight;
# D5 typed, D6 with typed numbers, D7 with the price of the next row, D8
# adds instead of multiplying, the total without the last row
HIKE = [
    ('Bus, hin und zurück', 1, 380),
    ('Eintritt Tierpark', 27, 6.5),
    ('Führung', 2, 45),
    ('Picknick', 29, 3.2),
    ('Getränke', 29, 1.5),
]
HIKE_TRAPS = {'D5': 175.5, 'D6': '=2*45', 'D7': '=B7*C8', 'D8': '=B8+C8', 'D9': '=SUM(D4:D7)'}


def with_ice(calc, path, solved):
    """»Zwei Handgriffe«: the calculating cost table with the number of
    people below it (the starting file); solved: a row »Eis« above the
    total, the total over it and the cost per person"""
    if not solved:
        table(calc, path, rows='formula', total='formula', people=29)
        return
    items = ROWS + [('Eis', 29, 2)]
    table(calc, path, rows='formula', total=solved, items=items, people=29,
          cells={'A13': 'Pro Person', 'D13': '=D10/B12'} if solved == 'formula' else {'A13': 'Pro Person', 'D13': '=D10/B12', 'D10': '=SUM(D4:D8)'})


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    for folder in ('stufen', 'bilder'):
        os.makedirs(os.path.join(HERE, folder), exist_ok=True)
    stage = lambda name: os.path.join(HERE, 'stufen', name)  # noqa: E731
    with Calc() as calc:
        starter = os.path.join(STATIC, STARTER)
        table(calc, starter)
        table(calc, stage('kosten.xlsx'), rows='formula', total='formula')
        table(calc, stage('gesamt-getippt.xlsx'), rows='formula', total='typed')
        # formulas with the numbers typed in, and the total without the last row
        table(calc, stage('kosten-zahlen.xlsx'), rows='formula', total='formula', cells={'D5': '=29*148'})
        table(calc, stage('summe-kurz.xlsx'), rows='formula', total='formula', cells={'D9': '=SUM(D4:D7)'})
        calc.picture(starter, os.path.join(HERE, 'bilder', 'kosten.svg'))
        # Üben und knobeln
        table(calc, os.path.join(STATIC, '01 Sommerfest.xlsx'), title='Einkauf fürs Sommerfest', items=PARTY)
        table(calc, stage('sommerfest.xlsx'), rows='formula', total='formula', title='Einkauf fürs Sommerfest', items=PARTY)
        with_ice(calc, os.path.join(STATIC, '01 Eis.xlsx'), None)
        with_ice(calc, stage('eis.xlsx'), 'formula')
        with_ice(calc, stage('eis-bereich.xlsx'), 'short')
        table(calc, os.path.join(STATIC, '01 Wandertag.xlsx'), rows='formula', total='formula', title='Kosten des Wandertags',
              items=HIKE, cells=HIKE_TRAPS)
        table(calc, stage('wandertag.xlsx'), rows='formula', total='formula', title='Kosten des Wandertags', items=HIKE)
        # four traps fixed, the total still without the last row
        table(calc, stage('wandertag-vier.xlsx'), rows='formula', total='formula', title='Kosten des Wandertags', items=HIKE,
              cells={k: v for k, v in HIKE_TRAPS.items() if k == 'D9'})
    print('written: static/spreadsheet-cells/01 *.xlsx, stufen/*.xlsx, bilder/kosten.svg')
