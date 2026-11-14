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


def ledger(calc, path, title, rate_label, rate, rows, head='Familie', typed=True, formats=False, total=None, after=None):
    """a list like the payments: dates as numbers, amounts, some typed as
    text (typed), the rate in C2; formats: currency, date and percent;
    total: the total's formula; after(doc, last): more to set"""
    doc = calc.new()
    put(doc, 'A1', [[title], [rate_label, None, rate], ['Datum', head, 'Betrag']])
    for i, (date, name, amount, text) in enumerate(rows):
        put(doc, f'A{4 + i}', [[date, name, text if typed and text else amount]])
    last = 3 + len(rows)
    put(doc, f'A{last + 1}', [[None, 'Zusammen', total or f'=SUM(C4:C{last})']])
    bold(doc, f'A1, A3:C3, B{last + 1}:C{last + 1}')
    if formats:
        fmt(doc, f'C4:C{last + 1}', '#.##0,00 [$€-407]')
        fmt(doc, f'A4:A{last}', 'TT.MM.JJJJ')
        fmt(doc, 'C2', '0 %')
    if after:
        after(doc, last)
    widths(doc, [2.6, 5.0, 2.6])
    calc.save(doc, path)


def day(month, d):
    return (datetime.date(YEAR, month, d) - datetime.date(1899, 12, 30)).days


# »Noch eine Runde«: what the class fund paid in April, some amounts
# copied from receipts as text; the stationer's discount 10 %
FUND = [
    (day(4, 6), 'Bastelpapier', 12.4, '12,40 €'), (day(4, 8), 'Kleber', 6.9, None),
    (day(4, 12), 'Geschenk für den Hausmeister', 25, 'EUR 25'), (day(4, 15), 'Getränke für den Wandertag', 18.75, None),
    (day(4, 19), 'Laminierfolie', 9.99, '9,99 €'), (day(4, 22), 'Briefmarken', 8.5, None),
    (day(4, 26), 'Pinnwand', 34.9, None), (day(4, 29), 'Batterien', 7.2, '7,20 €'),
]

# »Fünf Fallen«: the payments for the hiking day, 18 € each, 17,10 € for
# siblings; C6 typed as text, A9 a date without its format, C2 a 5 where
# 0,05 belongs (500 %), C11 an amount formatted as a date, the total one
# row short (a trap of the first sheet)
HIKE = [
    (day(5, 3), 'Familie Arslan', 18, None), (day(5, 3), 'Familie Becker', 18, None), (day(5, 4), 'Familie Çelik', 18, '18 €'),
    (day(5, 5), 'Familie Dąbrowski', 18, None), (day(5, 6), 'Familie Engel', 18, None), (day(5, 7), 'Familie Fischer', 17.1, None),
    (day(5, 7), 'Familie Groß', 18, None), (day(5, 10), 'Familie Hoang', 17.1, None), (day(5, 11), 'Familie Ivanova', 18, None),
    (day(5, 12), 'Familie Jansen', 18, None),
]


def hike_traps(doc, last):
    fmt(doc, 'A9', 'Standard')
    fmt(doc, 'C11', 'TT.MM.JJJJ')
    put(doc, 'C2', [[5]])


def deposit(calc, path, solved):
    """»Zwei Handgriffe«: the deposit, 20 % of the price, and the day it is
    due, two weeks after a family signed up"""
    doc = calc.new()
    put(doc, 'A1', [['Anmeldung zur Klassenfahrt'], ['Preis', None, 245], ['Anzahlung', None, 0.2],
                    ['Anzahlung je Familie'], [], ['Familie', 'Angemeldet am', 'Anzahlung bis']])
    for i, (name, d) in enumerate([('Familie Arslan', 1), ('Familie Becker', 3), ('Familie Çelik', 4), ('Familie Dąbrowski', 8)]):
        put(doc, f'A{7 + i}', [[name, day(2, d)] + ([f'=B{7 + i}+14'] if solved else [])])
    if solved:
        put(doc, 'C4', [['=C2*C3']])
        fmt(doc, 'C4', '#.##0,00 [$€-407]')
        fmt(doc, 'C7:C10', 'TT.MM.JJJJ')
    fmt(doc, 'C2', '#.##0,00 [$€-407]')
    fmt(doc, 'C3', '0 %')
    fmt(doc, 'B7:B10', 'TT.MM.JJJJ')
    bold(doc, 'A1, A6:C6')
    widths(doc, [4.4, 3.4, 3.4])
    calc.save(doc, path)


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    for folder in ('stufen', 'bilder'):
        os.makedirs(os.path.join(HERE, folder), exist_ok=True)
    stage = lambda name: os.path.join(HERE, 'stufen', name)  # noqa: E731
    with Calc() as calc:
        starter = os.path.join(STATIC, STARTER)
        table(calc, starter)
        table(calc, stage('einzahlungen.xlsx'), amounts='numbers', formats=True)
        table(calc, stage('ohne-format.xlsx'), amounts='numbers', formats=False)
        table(calc, stage('excel-format.xlsx'), amounts='numbers', formats='excel')
        calc.picture(starter, os.path.join(HERE, 'bilder', 'einzahlungen.svg'))
        calc.picture(stage('einzahlungen.xlsx'), os.path.join(HERE, 'bilder', 'einzahlungen-fertig.svg'))
        # Üben und knobeln
        fund = dict(title='Klassenkasse der 7b: Ausgaben im April', rate_label='Rabatt im Schreibwarenladen', rate=0.1, rows=FUND, head='Wofür')
        ledger(calc, os.path.join(STATIC, '02 Klassenkasse.xlsx'), **fund)
        ledger(calc, stage('klassenkasse.xlsx'), typed=False, formats=True, **fund)
        deposit(calc, os.path.join(STATIC, '02 Anzahlung.xlsx'), False)
        deposit(calc, stage('anzahlung.xlsx'), True)
        hike = dict(title='Einzahlungen für den Wandertag', rate_label='Ermäßigung für Geschwister', rate=0.05, rows=HIKE)
        ledger(calc, os.path.join(STATIC, '02 Wandertag.xlsx'), formats=True, total='=SUM(C4:C12)', after=hike_traps, **hike)
        ledger(calc, stage('wandertag.xlsx'), typed=False, formats=True, **hike)
        # four traps fixed, the total still one row short
        ledger(calc, stage('wandertag-vier.xlsx'), typed=False, formats=True, total='=SUM(C4:C12)', **hike)
    print('written: static/number-formats/02 *.xlsx, stufen/*.xlsx, bilder/*.svg')
