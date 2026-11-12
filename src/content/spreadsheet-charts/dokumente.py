# The tables of »Diagramme«: what a child pays for each part of the trip
# (sheet 3) and how each part was liked (sheet 4). The solution: a pie chart
# of the costs, a column chart of the stars, each with a title, the column
# chart with axis titles. Stages that must fail: the starting file (no
# chart), and the stars as a pie without titles. Run in the office image:
#
#     src/scripts/office python3 src/content/spreadsheet-charts/dokumente.py

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from calc_build import Calc, put, fmt, bold, widths, chart  # noqa: E402

STATIC = os.path.join(HERE, '..', '..', 'static', 'spreadsheet-charts')
STARTER = '06 Diagramme.xlsx'
COSTS = [('Bus', 1250 / 27), ('Jugendherberge', 148), ('Kletterwald', 24.5), ('Schifffahrt', 9), ('Moormuseum', 4.5)]
STARS = [('Kletterwald', 115 / 26), ('Schifffahrt', 97 / 27), ('Moormuseum', 62 / 24), ('Jugendherberge', 99 / 25)]
CURRENCY = '#.##0,00 [$€-407]'


def table(calc, path, charts=None):
    """charts: None, 'right' (the solution) or 'wrong' (the stars as a pie,
    no titles)"""
    doc = calc.new()
    put(doc, 'A1', [['Was ein Kind bezahlt'], [], ['Posten', 'pro Kind']])
    for i, (name, value) in enumerate(COSTS):
        put(doc, f'A{4 + i}', [[name, round(value, 2)]])
    put(doc, 'A9', [['Zusammen', '=SUM(B4:B8)']])
    put(doc, 'A12', [['Wie hat es gefallen?'], [], ['Programmpunkt', 'Sterne im Mittel']])
    for i, (name, value) in enumerate(STARS):
        put(doc, f'A{15 + i}', [[name, value]])
    bold(doc, 'A1, A3:B3, A9:B9, A12, A14:B14')
    fmt(doc, 'B4:B9', CURRENCY)
    fmt(doc, 'B15:B18', '0,0')
    widths(doc, [4.2, 4.0])
    if charts == 'right':
        chart(doc, 'Kosten', 'A3:B8', 'pie', 'Wofür ein Kind bezahlt', where=(9000, 0, 11000, 7000))
        chart(doc, 'Sterne', 'A14:B18', 'column', 'Wie hat es gefallen?', 'Programmpunkt', 'Sterne im Mittel',
              where=(9000, 7500, 11000, 7000))
    elif charts == 'wrong':
        chart(doc, 'Sterne', 'A14:B18', 'pie', None, where=(9000, 0, 11000, 7000))
    calc.save(doc, path)


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    for folder in ('stufen', 'bilder'):
        os.makedirs(os.path.join(HERE, folder), exist_ok=True)
    stufen = os.path.join(HERE, 'stufen')
    with Calc() as calc:
        starter = os.path.join(STATIC, STARTER)
        table(calc, starter)
        table(calc, os.path.join(stufen, 'diagramme.xlsx'), 'right')
        table(calc, os.path.join(stufen, 'falsch.xlsx'), 'wrong')
        calc.picture(starter, os.path.join(HERE, 'bilder', 'daten.svg'))
        calc.picture(os.path.join(stufen, 'diagramme.xlsx'), os.path.join(HERE, 'bilder', 'diagramme-fertig.svg'))
        calc.picture(os.path.join(stufen, 'falsch.xlsx'), os.path.join(HERE, 'bilder', 'falsch.svg'))
    print('written: static/spreadsheet-charts/06 Diagramme.xlsx, stufen/*.xlsx, bilder/*.svg')
