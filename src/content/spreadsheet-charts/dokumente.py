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
    elif charts == 'total':
        # the pie with the total row along: a second slice as big as the rest
        chart(doc, 'Kosten', 'A3:B9', 'pie', 'Wofür ein Kind bezahlt', where=(9000, 0, 11000, 7000))
        chart(doc, 'Sterne', 'A14:B18', 'column', 'Wie hat es gefallen?', 'Programmpunkt', 'Sterne im Mittel',
              where=(9000, 7500, 11000, 7000))
    elif charts == 'wrong':
        chart(doc, 'Sterne', 'A14:B18', 'pie', None, where=(9000, 0, 11000, 7000))
    calc.save(doc, path)


def two_tables(calc, path, title1, head1, rows1, title2, head2, rows2, charts=(), cells=None, money=True, decimals='0,0'):
    """two small tables, the first with a total row (rows 3 to 3+n+1), the
    second from row 11 on (header in row 13); charts: (name, source, kind,
    title, x_title, y_title) each; cells: a cell's own content"""
    doc = calc.new()
    put(doc, 'A1', [[title1], [], list(head1)])
    for i, (name, value) in enumerate(rows1):
        put(doc, f'A{4 + i}', [[name, value]])
    total = 4 + len(rows1)
    put(doc, f'A{total}', [['Zusammen', f'=SUM(B4:B{total - 1})']])
    put(doc, 'A11', [[title2], [], list(head2)])
    for i, (name, value) in enumerate(rows2):
        put(doc, f'A{14 + i}', [[name, value]])
    for name, value in (cells or {}).items():
        put(doc, name, [[value]])
    bold(doc, f'A1, A3:B3, A{total}:B{total}, A11, A13:B13')
    fmt(doc, f'B4:B{total}', CURRENCY if money else '0')
    fmt(doc, f'B14:B{13 + len(rows2)}', decimals)
    widths(doc, [7.0, 3.6])
    for i, (name, source, kind, title, x_title, y_title) in enumerate(charts):
        chart(doc, name, source, kind, title, x_title, y_title, where=(11500, i * 7500, 11000, 7000))
    calc.save(doc, path)


# »Noch eine Runde«: what the class fund spent on what (a whole: a pie), and
# how many take part in each club (compared: columns)
FUND = [('Bastelmaterial', 48.2), ('Geschenke', 25), ('Ausflüge', 112.5), ('Druckkosten', 18.3)]
CLUBS = [('Robotik', 14), ('Chor', 22), ('Theater', 9), ('Schach', 11)]
# »Zwei Handgriffe«: sign-ups over the weeks before the deadline (a line),
# and the votes for long-named outings (bars, the names readable)
WEEKS = [('Woche 1', 3), ('Woche 2', 9), ('Woche 3', 15), ('Woche 4', 21), ('Woche 5', 25), ('Woche 6', 27)]
VOTES = [('Ein Tag im Kletterwald am See', 9), ('Besuch der Sternwarte mit Nachtführung', 7),
         ('Stadtrallye durch die Altstadt', 6), ('Kanutour auf der Havel', 5)]
# »Fünf Fallen«: what a child pays for the hiking day and how it was liked;
# the pie takes the total along, the columns lack a title and axis titles,
# B5 is text, the total typed
HIKE_COSTS = [('Bus', 14.62), ('Eintritt Tierpark', 6.5), ('Führung', 3.46), ('Picknick', 3.2)]
HIKE_STARS = [('Tierpark', 4.2), ('Picknick', 4.1), ('Führung', 2.8), ('Wanderung', 3.5)]


def votes(calc, path, solved):
    doc_charts = (('Anmeldungen', 'A3:B9', 'line', 'Anmeldungen bis zum Stichtag', 'Woche', 'Angemeldet'),
                  ('Stimmen', 'A13:B17', 'bar', 'Wohin soll der Ausflug gehen?', 'Ausflug', 'Stimmen')) if solved else ()
    doc = calc.new()
    put(doc, 'A1', [['Anmeldungen bis zum Stichtag'], [], ['Woche', 'Angemeldet']])
    for i, (name, value) in enumerate(WEEKS):
        put(doc, f'A{4 + i}', [[name, value]])
    put(doc, 'A11', [['Wohin soll der Ausflug gehen?'], [], ['Ausflug', 'Stimmen']])
    for i, (name, value) in enumerate(VOTES):
        put(doc, f'A{14 + i}', [[name, value]])
    bold(doc, 'A1, A3:B3, A11, A13:B13')
    widths(doc, [7.6, 2.8])
    for i, (name, source, kind, title, x_title, y_title) in enumerate(doc_charts):
        chart(doc, name, source, kind, title, x_title, y_title, where=(11500, i * 7500, 11000, 7000))
    calc.save(doc, path)


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    for folder in ('stufen', 'bilder'):
        os.makedirs(os.path.join(HERE, folder), exist_ok=True)
    stufen = os.path.join(HERE, 'stufen')
    stage = lambda name: os.path.join(stufen, name)  # noqa: E731
    with Calc() as calc:
        starter = os.path.join(STATIC, STARTER)
        table(calc, starter)
        table(calc, stage('diagramme.xlsx'), 'right')
        table(calc, stage('falsch.xlsx'), 'wrong')
        table(calc, stage('mit-summe.xlsx'), 'total')
        calc.picture(starter, os.path.join(HERE, 'bilder', 'daten.svg'))
        calc.picture(stage('diagramme.xlsx'), os.path.join(HERE, 'bilder', 'diagramme-fertig.svg'))
        calc.picture(stage('falsch.xlsx'), os.path.join(HERE, 'bilder', 'falsch.svg'))
        # Üben und knobeln
        fund = ('Wofür die Klassenkasse ausgegeben hat', ('Zweck', 'Betrag'), FUND, 'Wer in welcher AG ist', ('AG', 'Teilnehmende'), CLUBS)
        two_tables(calc, os.path.join(STATIC, '06 Klassenkasse.xlsx'), *fund, decimals='0')
        two_tables(calc, stage('klassenkasse.xlsx'), *fund, decimals='0',
                   charts=(('Kasse', 'A3:B7', 'pie', 'Wofür die Klassenkasse ausgegeben hat', None, None),
                           ('AGs', 'A13:B17', 'column', 'Wer in welcher AG ist', 'AG', 'Teilnehmende')))
        votes(calc, os.path.join(STATIC, '06 Anmeldungen.xlsx'), False)
        votes(calc, stage('anmeldungen.xlsx'), True)
        hike = ('Was ein Kind beim Wandertag bezahlt', ('Posten', 'pro Kind'), HIKE_COSTS, 'Wie hat es gefallen?', ('Programmpunkt', 'Sterne im Mittel'), HIKE_STARS)
        right = (('Kosten', 'A3:B7', 'pie', 'Wofür ein Kind bezahlt', None, None),
                 ('Sterne', 'A13:B17', 'column', 'Wie hat es gefallen?', 'Programmpunkt', 'Sterne im Mittel'))
        two_tables(calc, os.path.join(STATIC, '06 Wandertag.xlsx'), *hike, cells={'B5': '6,50 €', 'B8': 27.78},
                   charts=(('Kosten', 'A3:B8', 'pie', 'Wofür ein Kind bezahlt', None, None),
                           ('Sterne', 'A13:B17', 'column', None, None, None)))
        two_tables(calc, stage('wandertag.xlsx'), *hike, charts=right)
        # four traps fixed, the axes still without titles
        two_tables(calc, stage('wandertag-vier.xlsx'), *hike,
                   charts=(right[0], ('Sterne', 'A13:B17', 'column', 'Wie hat es gefallen?', None, None)))
    print('written: static/spreadsheet-charts/06 *.xlsx, stufen/*.xlsx, bilder/*.svg')
