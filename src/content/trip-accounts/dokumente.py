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
from calc_build import Calc, put, fmt, bold, widths, chart, prop, sheet_of  # noqa: E402

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


def account(calc, path, title, rows, invoices, solution=None, refund=None, total=None, typed=(), result=None,
            rest=None, chart_title='Wofür das Geld ausgegeben wurde', heads=('zurück', 'Zurück pro Kind'), red=False):
    """an account: rows (day serial, family, amount) of the income, invoices
    (serial, item, amount); solution: None (nothing calculates yet), a
    formula for G17 (the refund per child: »ROUNDDOWN(G15/G16;2)«) or a
    number typed there. refund: the formula of each cell in D (default
    $G$17, or a number typed); total: G13's formula; typed: rows whose amount is typed as text
    (»18 €«; in a German sheet it stays text); result: G19 typed instead of WENN; red: G19 red at
    »Nachzahlung«"""
    doc = calc.new()
    last = FIRST + len(rows) - 1
    put(doc, 'A1', [[title], [], ['Einnahmen', None, None, None, None, 'Ausgaben'],
                    ['Datum', 'Familie', 'Betrag', heads[0], None, 'Datum', 'Posten', 'Betrag']])
    for i, (day, family, amount) in enumerate(rows):
        r = FIRST + i
        cell = None
        if solution is not None:
            cell = refund if isinstance(refund, (int, float)) else (refund or '=$G$17')
        put(doc, f'A{r}', [[day, family, f'{amount:g} €'.replace('.', ',') if r in typed else amount, cell]])
    for i, (day, item, amount) in enumerate(invoices):
        put(doc, f'F{FIRST + i}', [[day, item, amount]])
    spent = FIRST + len(invoices) - 1
    labels = ['Einnahmen', 'Ausgaben', 'Rest', 'Kinder', heads[1], 'Bleibt in der Klassenkasse', 'Ergebnis']
    put(doc, 'F12', [['Abrechnung']] + [[label] for label in labels])
    if solution is not None:
        g17 = f'={solution}' if isinstance(solution, str) else solution
        formulas = [total or f'=SUM(C{FIRST}:C{last})', f'=SUM(H{FIRST}:H{spent})', '=G13-G14', f'=COUNT(C{FIRST}:C{last})',
                    g17, '=G15-G16*G17', result or '=IF(G15>=0;"Rückzahlung";"Nachzahlung")']
        put(doc, 'G13', [[f] for f in formulas])
        fmt(doc, f'C{FIRST}:D{last}, H{FIRST}:H{spent}, G13:G15, G17:G18', CURRENCY)
        fmt(doc, f'A{FIRST}:A{last}, F{FIRST}:F{spent}', 'TT.MM.JJJJ')
    if red:
        sh = sheet_of(doc)
        entries = sh.getCellRangeByName('G19').ConditionalFormat
        entries.addNew((prop('Operator', uno.Enum('com.sun.star.sheet.ConditionOperator', 'EQUAL')),
                        prop('Formula1', '"Nachzahlung"'), prop('StyleName', 'Bad')))
        sh.getCellRangeByName('G19').ConditionalFormat = entries
    bold(doc, 'A1, A3, F3, A4:H4, F12, F19:G19')
    widths(doc, [2.8, 3.4, 2.6, 2.4, 0.8, 5.2, 3.6, 2.6])
    if chart_title:
        chart(doc, 'Ausgaben', f'G4:H{spent}', 'pie', chart_title, where=(17500, 9500, 11000, 7500))
    calc.save(doc, path)


def table(calc, path, solution=None):
    """the hand-in: solution None, 'right', or 'typed' (the refund worked
    out, rounded, and typed: 11,80 €)"""
    rows = [(serial(day), family, SIBLING if family in SIBLINGS else FULL) for day, family in zip(DAYS, ORDER)]
    if solution:
        rows.sort(key=lambda p: german(p[1]))
    invoices = [(serial(d, m), item, amount) for d, m, item, amount in INVOICES]
    account(calc, path, 'Abrechnung der Klassenfahrt der 7b', rows, invoices,
            {None: None, 'right': 'ROUNDDOWN(G15/G16;2)', 'typed': 11.8}[solution], refund=11.8 if solution == 'typed' else None,
            chart_title='Wofür das Geld ausgegeben wurde' if solution == 'right' else None)


# »Noch eine Runde«: the theatre in December, 22 € for each child, the
# payments in the order they came; left 61 €, 2,25 € back (rounded 2,26 €
# would be 2 cents too much), 0,25 € stay
THEATRE_ORDER = ['Hoang', 'Arslan', 'Nowak', 'Becker', 'Xu', 'Çelik', 'Tran', 'Engel', 'Dąbrowski', 'Lehmann', 'Fischer',
                 'Okafor', 'Groß', 'Zimmermann', 'Ivanova', 'Jansen', 'Kaya', 'Müller', 'Petrov', 'Quast', 'Richter',
                 'Schmidt', 'Schulz', 'Uysal', 'Vogel', 'Wagner', 'Yılmaz']
THEATRE_DAYS = [1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 15, 15, 16, 16, 17, 18]
THEATRE = [(serial(d, 12), item, a) for d, item, a in [(8, 'Eintritt', 324), (9, 'Bus', 185), (11, 'Programmhefte', 24)]]

# »Zwei Handgriffe«: the hand-in, a week after the parents' evening a second
# invoice of the bus company (waiting time): 131,50 € missing; rounded
# down, 4,87 € each leave the class fund a cent short, rounded up 4,88 €
# leave 0,26 €
LATE = (serial(30, 4), 'Bus, Wartezeit', 450)

# »Fünf Fallen«: the hiking day's account in a hurry, the families as on the
# sheets before, 18 € each, 17,10 € for siblings; left 53,25 €, 5,32 €
# back, 0,05 € stay. With the traps it still looks finished (9 children,
# 1,92 € back) instead of going below 0. The traps: G13 one row short (sheet 1), C9 typed as
# text (sheet 2), D without $ (sheet 3), G19 typed (sheet 4), G17 rounded
# instead of rounded down (this sheet)
HIKE = [(serial(d, 5), n, a) for d, n, a in [
    (3, 'Arslan', 18), (3, 'Becker', 18), (4, 'Çelik', 18), (5, 'Dąbrowski', 18), (6, 'Engel', 18), (7, 'Fischer', 17.1),
    (7, 'Groß', 18), (10, 'Hoang', 17.1), (11, 'Ivanova', 18), (12, 'Jansen', 18)]]
HIKE_COSTS = [(serial(18, 5), 'Bus', 69.95), (serial(18, 5), 'Wildpark', 55)]


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
        # Üben und knobeln
        theatre = [(serial(d, 11), f, 22) for d, f in zip(THEATRE_DAYS, THEATRE_ORDER)]
        title = 'Abrechnung der Theaterfahrt der 7b'
        account(calc, os.path.join(STATIC, '07 Theaterfahrt.xlsx'), title, theatre, THEATRE, chart_title=None)
        account(calc, os.path.join(stufen, 'theaterfahrt.xlsx'), title, sorted(theatre, key=lambda p: german(p[1])), THEATRE,
                'ROUNDDOWN(G15/G16;2)', chart_title='Wofür das Geld ausgegeben wurde')
        rows = sorted([(serial(d), f, SIBLING if f in SIBLINGS else FULL) for d, f in zip(DAYS, ORDER)], key=lambda p: german(p[1]))
        invoices = [(serial(d, m), item, amount) for d, m, item, amount in INVOICES] + [LATE]
        late = dict(heads=('pro Kind', 'Pro Kind'))
        title = 'Abrechnung der Klassenfahrt der 7b'
        account(calc, os.path.join(STATIC, '07 Nachzahlung.xlsx'), title, rows, invoices, 'ROUNDDOWN(G15/G16;2)', **late)
        account(calc, os.path.join(stufen, 'nachzahlung.xlsx'), title, rows, invoices,
                'IF(G15<0;ROUNDUP(G15/G16;2);ROUNDDOWN(G15/G16;2))', red=True, **late)
        # only rounded up: right while money is missing, 1 cent too much when it is left
        account(calc, os.path.join(stufen, 'nur-aufgerundet.xlsx'), title, rows, invoices, 'ROUNDUP(G15/G16;2)', red=True, **late)
        title = 'Abrechnung des Wandertags der 7b'
        traps = dict(total='=SUM(C5:C13)', typed=(9,), refund='=G17', result='Rückzahlung')
        account(calc, os.path.join(STATIC, '07 Wandertag.xlsx'), title, HIKE, HIKE_COSTS, 'ROUND(G15/G16;2)', **traps)
        account(calc, os.path.join(stufen, 'wandertag.xlsx'), title, HIKE, HIKE_COSTS, 'ROUNDDOWN(G15/G16;2)')
        # four traps fixed, G17 still rounded
        account(calc, os.path.join(stufen, 'wandertag-vier.xlsx'), title, HIKE, HIKE_COSTS, 'ROUND(G15/G16;2)')
        calc.picture(starter, os.path.join(HERE, 'bilder', 'vorlage.svg'))
        calc.picture(os.path.join(stufen, 'abrechnung.xlsx'), os.path.join(HERE, 'bilder', 'abrechnung.svg'))
    print('written: static/trip-accounts/07 Abrechnung.xlsx, stufen/*.xlsx, bilder/*.svg')
