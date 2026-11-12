# The tables of »Die Abrechnung der Klassenfahrt«, the hand-in of the
# series: every family's payment (all 27 have paid now, the dates copied
# from the bank statement as bare numbers, in the order they came) and the
# invoices of the trip. The parent's summary is the student's to build:
# income and expenses with SUMME, what is left, the children with ANZAHL,
# the refund per child with ABRUNDEN (rounded it would be 10 cents too
# much), a refund column with $, WENN for refund or extra payment, formats,
# the list sorted by family, a pie chart of the expenses. Stages that must
# fail: the starting file; the refund rounded and typed. Run in the office
# image:
#
#     src/scripts/office python3 src/content/trip-accounts/dokumente.py

import datetime
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
import uno  # noqa: E402
from calc_build import Calc, put, fmt, bold, widths, chart, prop  # noqa: E402

STATIC = os.path.join(HERE, '..', '..', 'static', 'trip-accounts')
STARTER = '07 Abrechnung.xlsx'
YEAR = 2027
FULL, SIBLING = 245, 232.75
SIBLINGS = {'Fischer', 'Jansen'}
# the order the payments came (sheet 5), the last four paid late
ORDER = ['Arslan', 'Becker', 'Çelik', 'Dąbrowski', 'Engel', 'Fischer', 'Groß', 'Hoang', 'Ivanova', 'Jansen', 'Schulz',
         'Müller', 'Lehmann', 'Nowak', 'Tran', 'Okafor', 'Yılmaz', 'Petrov', 'Vogel', 'Uysal', 'Xu', 'Schmidt', 'Quast',
         'Kaya', 'Richter', 'Wagner', 'Zimmermann']
DAYS = [2, 2, 3, 3, 4, 5, 5, 6, 9, 9, 10, 10, 11, 11, 12, 12, 15, 15, 16, 17, 18, 19, 22, 24, 25, 26, 29]
# the invoices: (day of the month, month, item, amount) for the children
INVOICES = [(1, 4, 'Bus', 1250), (9, 4, 'Jugendherberge', 3996), (6, 4, 'Kletterwald', 661.5),
            (7, 4, 'Schifffahrt', 243), (8, 4, 'Moormuseum', 121.5)]
FIRST, LAST = 5, 31
CURRENCY = '#.##0,00 [$€-407]'


def serial(day, month=3):
    return (datetime.date(YEAR, month, day) - datetime.date(1899, 12, 30)).days


def german(name):
    return name.replace('Ç', 'C').replace('ą', 'a').replace('ı', 'i').replace('ß', 'ss').lower()


def table(calc, path, solution=None):
    """solution: None, 'right', or 'typed' (the refund worked out, rounded,
    and typed: 11,80 €)"""
    doc = calc.new()
    rows = list(zip(DAYS, ORDER))
    if solution:
        rows.sort(key=lambda p: german(p[1]))
    put(doc, 'A1', [['Abrechnung der Klassenfahrt der 7b'], [], ['Einnahmen', None, None, None, None, 'Ausgaben'],
                    ['Datum', 'Familie', 'Betrag', 'zurück', None, 'Datum', 'Posten', 'Betrag']])
    for i, (day, family) in enumerate(rows):
        r = FIRST + i
        refund = None
        if solution == 'right':
            refund = '=$G$17'
        elif solution == 'typed':
            refund = 11.8
        put(doc, f'A{r}', [[serial(day), family, SIBLING if family in SIBLINGS else FULL, refund]])
    for i, (day, month, item, amount) in enumerate(INVOICES):
        put(doc, f'F{FIRST + i}', [[serial(day, month), item, amount]])
    labels = ['Einnahmen', 'Ausgaben', 'Rest', 'Kinder', 'Zurück pro Kind', 'Bleibt in der Klassenkasse', 'Ergebnis']
    put(doc, 'F12', [['Abrechnung']] + [[label] for label in labels])
    if solution:
        formulas = [f'=SUM(C{FIRST}:C{LAST})', '=SUM(H5:H9)', '=G13-G14', f'=COUNT(C{FIRST}:C{LAST})',
                    '=ROUNDDOWN(G15/G16;2)' if solution == 'right' else 11.8, '=G15-G16*G17',
                    '=IF(G15>=0;"Rückzahlung";"Nachzahlung")']
        put(doc, 'G13', [[f] for f in formulas])
        fmt(doc, f'C{FIRST}:D{LAST}, H5:H9, G13:G15, G17:G18', CURRENCY)
        fmt(doc, f'A{FIRST}:A{LAST}, F5:F9', 'TT.MM.JJJJ')
    bold(doc, 'A1, A3, F3, A4:H4, F12, F19:G19')
    widths(doc, [2.8, 3.4, 2.6, 2.4, 0.8, 5.2, 3.6, 2.6])
    if solution == 'right':
        chart(doc, 'Ausgaben', 'G4:H9', 'pie', 'Wofür das Geld ausgegeben wurde', where=(17500, 9500, 11000, 7500))
    calc.save(doc, path)


def pdf(calc, source, target):
    """the table as a PDF, landscape on one page, as the page explains"""
    doc = calc.open(source)
    style = doc.StyleFamilies.getByName('PageStyles').getByName(doc.Sheets.getByIndex(0).PageStyle)
    w, h = style.Width, style.Height
    style.IsLandscape = True
    style.Width, style.Height = max(w, h), min(w, h)
    style.ScaleToPagesX, style.ScaleToPagesY = 1, 1
    doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(target)), (prop('FilterName', 'calc_pdf_Export'),))
    doc.close(True)


def title_only(calc, path):
    """a table with nothing but its title: as a PDF, too few words"""
    doc = calc.new()
    put(doc, 'A1', [['Abrechnung der Klassenfahrt der 7b']])
    calc.save(doc, path)


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    for folder in ('stufen', 'bilder'):
        os.makedirs(os.path.join(HERE, folder), exist_ok=True)
    stufen = os.path.join(HERE, 'stufen')
    income = sum(SIBLING if f in SIBLINGS else FULL for f in ORDER)
    costs = sum(a for *_, a in INVOICES)
    print('income', income, 'costs', costs, 'rest', income - costs, 'per child', (income - costs) / 27)
    with Calc() as calc:
        starter = os.path.join(STATIC, STARTER)
        table(calc, starter)
        table(calc, os.path.join(stufen, 'abrechnung.xlsx'), 'right')
        table(calc, os.path.join(stufen, 'getippt.xlsx'), 'typed')
        pdf(calc, os.path.join(stufen, 'abrechnung.xlsx'), os.path.join(stufen, 'abrechnung.pdf'))
        title_only(calc, os.path.join(stufen, '_titel.xlsx'))
        pdf(calc, os.path.join(stufen, '_titel.xlsx'), os.path.join(stufen, 'nur-titel.pdf'))
        os.remove(os.path.join(stufen, '_titel.xlsx'))
        calc.picture(starter, os.path.join(HERE, 'bilder', 'vorlage.svg'))
        calc.picture(os.path.join(stufen, 'abrechnung.xlsx'), os.path.join(HERE, 'bilder', 'abrechnung.svg'))
    print('written: static/trip-accounts/07 Abrechnung.xlsx, stufen/*.xlsx, bilder/*.svg')
