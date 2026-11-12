# The tables of »Zahlen in Form«: the payments for the trip as the class's
# treasurer typed them (the starting file): some amounts typed with their €
# as text, which SUMME leaves out; the dates as numbers, since a date is a
# number of days; the discount as 0,05. The solution: every amount a number
# formatted as currency, the dates as dates, the discount as percent. Stages:
# the amounts numbers, the formats not yet (must fail); the formats as Excel
# writes them, its euro as text in the format (must pass). Run in the office
# image:
#
#     src/scripts/office python3 src/content/number-formats/dokumente.py

import datetime
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from calc_build import Calc, put, fmt, bold, widths, sheet_of  # noqa: E402
from com.sun.star.lang import Locale  # noqa: E402

STATIC = os.path.join(HERE, '..', '..', 'static', 'number-formats')
STARTER = '02 Einzahlungen.xlsx'
# (day of March, family, amount, typed as text?)
PAYMENTS = [
    (2, 'Familie Arslan', 245, None), (2, 'Familie Becker', 245, '245 €'), (3, 'Familie Çelik', 245, None),
    (3, 'Familie Dąbrowski', 245, '245,00 €'), (4, 'Familie Engel', 245, None), (5, 'Familie Fischer', 232.75, None),
    (5, 'Familie Groß', 245, 'EUR 245'), (6, 'Familie Hoang', 245, None), (9, 'Familie Ivanova', 245, '245 €'),
    (9, 'Familie Jansen', 232.75, None),
]
YEAR = 2027
DISCOUNT = 0.05


def serial(day):
    """a date as the days since 30 December 1899, as the programs count"""
    return (datetime.date(YEAR, 3, day) - datetime.date(1899, 12, 30)).days


def table(calc, path, amounts='mixed', formats=False):
    doc = calc.new()
    put(doc, 'A1', [['Einzahlungen für die Klassenfahrt'],
                    ['Ermäßigung für Geschwister', None, DISCOUNT],
                    ['Datum', 'Familie', 'Betrag']])
    for i, (day, family, amount, typed) in enumerate(PAYMENTS):
        r = 4 + i
        value = typed if amounts == 'mixed' and typed else amount
        put(doc, f'A{r}', [[serial(day), family, value]])
    last = 3 + len(PAYMENTS)
    put(doc, f'A{last + 1}', [[None, 'Zusammen', f'=SUM(C4:C{last})']])
    bold(doc, f'A1, A3:C3, B{last + 1}:C{last + 1}')
    if formats == 'excel':
        # as German Excel stores Währung, Datum kurz and Prozent in an .xlsx
        english, sh = Locale('en', 'US', ''), sheet_of(doc)
        for cells, code in ((f'C4:C{last + 1}', '#,##0.00\\ "€"'), (f'A4:A{last}', 'dd/mm/yyyy'), ('C2', '0%')):
            key = doc.NumberFormats.queryKey(code, english, False)
            sh.getCellRangeByName(cells).NumberFormat = key if key != -1 else doc.NumberFormats.addNew(code, english)
    elif formats:
        fmt(doc, f'C4:C{last + 1}', '#.##0,00 [$€-407]')
        fmt(doc, f'A4:A{last}', 'TT.MM.JJJJ')
        fmt(doc, 'C2', '0 %')
    widths(doc, [2.6, 4.4, 2.6])
    calc.save(doc, path)


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    for folder in ('stufen', 'bilder'):
        os.makedirs(os.path.join(HERE, folder), exist_ok=True)
    with Calc() as calc:
        starter = os.path.join(STATIC, STARTER)
        table(calc, starter)
        table(calc, os.path.join(HERE, 'stufen', 'einzahlungen.xlsx'), amounts='numbers', formats=True)
        table(calc, os.path.join(HERE, 'stufen', 'ohne-format.xlsx'), amounts='numbers', formats=False)
        table(calc, os.path.join(HERE, 'stufen', 'excel-format.xlsx'), amounts='numbers', formats='excel')
        calc.picture(starter, os.path.join(HERE, 'bilder', 'einzahlungen.svg'))
        calc.picture(os.path.join(HERE, 'stufen', 'einzahlungen.xlsx'), os.path.join(HERE, 'bilder', 'einzahlungen-fertig.svg'))
    print('written: static/number-formats/02 Einzahlungen.xlsx, stufen/*.xlsx, bilder/*.svg')
