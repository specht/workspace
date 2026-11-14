# The tables of »Sortieren, Filtern, Hervorheben«: all 27 families' payments
# for the trip in the order the parent entered them (by date, those who
# have not paid at the end), with what each still owes (=C-D). The solution:
# sorted by family, rows kept together, an autofilter, what is still open
# coloured by a conditional format. Stages that must fail: only the column
# of names sorted (names beside other families' amounts); sorted but no
# filter and no colour. Run in the office image:
#
#     src/scripts/office python3 src/content/sort-and-filter/dokumente.py

import datetime
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from calc_build import Calc, put, fmt, bold, widths, sheet_of, prop  # noqa: E402

STATIC = os.path.join(HERE, '..', '..', 'static', 'sort-and-filter')
STARTER = '05 Zahlungen.xlsx'
YEAR = 2027
FULL, SIBLING = 245, 232.75
# (day of March or None, family, owed, paid or None), in the order entered
PAYMENTS = [
    (2, 'Arslan', FULL, FULL), (2, 'Becker', FULL, FULL), (3, 'Çelik', FULL, FULL), (3, 'Dąbrowski', FULL, FULL),
    (4, 'Engel', FULL, FULL), (5, 'Fischer', SIBLING, SIBLING), (5, 'Groß', FULL, FULL), (6, 'Hoang', FULL, FULL),
    (9, 'Ivanova', FULL, FULL), (9, 'Jansen', SIBLING, SIBLING), (10, 'Schulz', FULL, FULL), (10, 'Müller', FULL, FULL),
    (11, 'Lehmann', FULL, 100), (11, 'Nowak', FULL, FULL), (12, 'Tran', FULL, FULL), (12, 'Okafor', FULL, FULL),
    (15, 'Yılmaz', FULL, FULL), (15, 'Petrov', FULL, 150), (16, 'Vogel', FULL, FULL), (17, 'Uysal', FULL, FULL),
    (18, 'Xu', FULL, FULL), (19, 'Schmidt', FULL, FULL), (22, 'Quast', FULL, FULL),
    (None, 'Kaya', FULL, None), (None, 'Richter', FULL, None), (None, 'Wagner', FULL, None), (None, 'Zimmermann', FULL, None),
]
FIRST = 4
LAST = FIRST + len(PAYMENTS) - 1       # 30
CURRENCY = '#.##0,00 [$€-407]'


def german(name):
    """a name as the programs sort it: Ç with C, ą with a, ı with i"""
    return name.replace('Ç', 'C').replace('ą', 'a').replace('ı', 'i').replace('ß', 'ss').lower()


def serial(day, month=3):
    return (datetime.date(YEAR, month, day) - datetime.date(1899, 12, 30)).days


# »Noch eine Runde«: the library's books, lent in April (day, name, books
# lent, books back or None), in the order lent
BOOKS = [
    (6, 'Okafor', 2, 2), (6, 'Brandt', 1, 1), (7, 'Yıldız', 3, 1), (8, 'Fuchs', 2, 2), (8, 'Çakır', 1, 1),
    (12, 'Albrecht', 2, 2), (13, 'Nguyen', 1, None), (13, 'Krause', 2, 2), (14, 'Wolff', 1, 1), (15, 'Dąbek', 2, 1),
    (19, 'Petersen', 1, 1), (20, 'Hahn', 3, 3), (21, 'Seidel', 1, None), (22, 'Ivanov', 2, 2),
]

# »Fünf Fallen«: the payments for the hiking day, 18 € each, sorted by
# family but with one row out of place, no filter, the colour only above
# 100 (cents: the condition misses a family), one result typed, one amount
# without its currency format
HIKE = [
    (3, 'Arslan', 18, 18), (3, 'Becker', 18, 18), (4, 'Çelik', 18, 10), (4, 'Dąbrowski', 18, 18), (5, 'Engel', 18, 18),
    (6, 'Fischer', 18, 18), (6, 'Groß', 18, None), (7, 'Hoang', 18, 18), (7, 'Ivanova', 18, 18), (10, 'Jansen', 18, 18),
    (10, 'Kaya', 18, 18), (11, 'Lehmann', 18, 8), (11, 'Müller', 18, 18), (12, 'Nowak', 18, 18),
]


def table(calc, path, order='entered', names_only=False, filtered=False, coloured=False, probe=False,
          rows=PAYMENTS, title='Zahlungen für die Klassenfahrt', heads=('Datum', 'Familie', 'Soll', 'Gezahlt', 'Offen'),
          total='Noch offen', condition=('GREATER', '0'), swap=None, cells=None, plain=None, month=3, more=None, money=True):
    """order: 'entered', 'family' or 'open' (the most open first, then by
    family); swap: two indexes of rows to change places after sorting (one
    row out of order); cells: a cell's own content; plain: cells without the
    currency format; more: [label, formula] two rows below the total;
    money: amounts as currency (else plain numbers)"""
    doc = calc.new()
    first, last = FIRST, FIRST + len(rows) - 1
    rows = list(rows)
    if order == 'family':
        rows.sort(key=lambda p: german(p[1]))
    elif order == 'open':
        rows.sort(key=lambda p: (-(p[2] - (p[3] or 0)), german(p[1])))
    if swap:
        i, j = swap
        rows[i], rows[j] = rows[j], rows[i]
    put(doc, 'A1', [[title], [], list(heads)])
    names = sorted((p[1] for p in rows), key=german) if names_only else None
    for i, (day, family, owed, paid) in enumerate(rows):
        r = first + i
        put(doc, f'A{r}', [[serial(day, month) if day else None, names[i] if names else family, owed, paid, f'=C{r}-D{r}']])
    put(doc, f'A{last + 2}', [[None, total, None, None, f'=SUM(E{first}:E{last})']])
    if more:
        put(doc, f'A{last + 3}', [[None, more[0], None, None, more[1]]])
    for name, value in (cells or {}).items():
        put(doc, name, [[value]])
    bold(doc, f'A1, A3:E3, B{last + 2}:E{last + 3 if more else last + 2}')
    fmt(doc, f'C{first}:E{last + 3 if more else last + 2}', CURRENCY if money else '0')
    if plain:
        fmt(doc, plain, '0')
    fmt(doc, f'A{first}:A{last}', 'TT.MM.JJJJ')
    widths(doc, [2.8, 3.6, 2.6, 2.6, 2.6])
    sh = sheet_of(doc)
    LAST = last  # noqa: N806 (the rest below as before)
    if filtered:
        area = sh.getCellRangeByName(f'A3:E{LAST}')
        db = doc.DatabaseRanges
        db.addNewByName('Zahlungen', area.RangeAddress)
        db.getByName('Zahlungen').AutoFilter = True
    if coloured:
        entries = sh.getCellRangeByName(f'E{FIRST}:E{LAST}').ConditionalFormat
        entries.addNew((prop('Operator', uno_operator(condition[0])), prop('Formula1', condition[1]), prop('StyleName', 'Bad')))
        sh.getCellRangeByName(f'E{FIRST}:E{LAST}').ConditionalFormat = entries
    if probe:
        # does SUM leave out the rows a filter hides? (the page's prediction)
        area = sh.getCellRangeByName(f'A3:E{LAST}')
        descriptor = area.createFilterDescriptor(True)
        from com.sun.star.sheet import TableFilterField
        field = TableFilterField()
        field.Field = 4
        field.Operator = uno_filter('EQUAL')   # hide the six that still owe something
        field.IsNumeric = True
        field.NumericValue = 0
        descriptor.FilterFields = (field,)
        descriptor.ContainsHeader = True
        area.filter(descriptor)
        doc.calculateAll()
        hidden = sum(1 for r in range(FIRST, LAST + 1) if not sh.Rows.getByIndex(r - 1).IsVisible)
        print('probe: rows hidden by the filter', hidden, '; SUM below:', sh.getCellRangeByName(f'E{LAST + 2}').Value)
    calc.save(doc, path)


def uno_operator(name):
    import uno
    return uno.Enum('com.sun.star.sheet.ConditionOperator', name)


def uno_filter(name):
    import uno
    return uno.Enum('com.sun.star.sheet.FilterOperator', name)


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    for folder in ('stufen', 'bilder'):
        os.makedirs(os.path.join(HERE, folder), exist_ok=True)
    stufen = os.path.join(HERE, 'stufen')
    print('still open:', sum(o - (p or 0) for _, _, o, p in PAYMENTS), 'families:', sum(1 for _, _, o, p in PAYMENTS if o - (p or 0) > 0))
    with Calc() as calc:
        starter = os.path.join(STATIC, STARTER)
        table(calc, starter)
        table(calc, os.path.join(stufen, 'zahlungen.xlsx'), 'family', filtered=True, coloured=True)
        table(calc, os.path.join(stufen, 'nur-namen.xlsx'), names_only=True, filtered=True, coloured=True)
        table(calc, os.path.join(stufen, 'nur-sortiert.xlsx'), 'family')
        table(calc, os.path.join(stufen, '_probe.xlsx'), 'family', probe=True)
        os.remove(os.path.join(stufen, '_probe.xlsx'))
        # Üben und knobeln
        books = dict(rows=BOOKS, title='Ausleihe der Klassenbücherei', heads=('Ausgeliehen', 'Name', 'Bücher', 'Zurück', 'Fehlen'),
                     total='Fehlen noch', month=4, money=False)
        table(calc, os.path.join(STATIC, '05 Buecherei.xlsx'), **books)
        table(calc, os.path.join(stufen, 'buecherei.xlsx'), 'family', filtered=True, coloured=True, **books)
        more = ['Sichtbar offen', f'=SUBTOTAL(9;E{FIRST}:E{LAST})']
        table(calc, os.path.join(STATIC, '05 Offen.xlsx'), 'family', filtered=True, coloured=True, more=['Sichtbar offen', None])
        table(calc, os.path.join(stufen, 'offen.xlsx'), 'open', filtered=True, coloured=True, more=more)
        hike = dict(rows=HIKE, title='Zahlungen für den Wandertag', month=5)
        hike_last = FIRST + len(HIKE) - 1
        table(calc, os.path.join(STATIC, '05 Wandertag.xlsx'), 'family', coloured=True, condition=('GREATER', '9'), swap=(9, 10),
              cells={'E13': 0}, plain='C8', **hike)
        table(calc, os.path.join(stufen, 'wandertag.xlsx'), 'family', filtered=True, coloured=True, **hike)
        # four traps fixed, one row still out of place
        table(calc, os.path.join(stufen, 'wandertag-vier.xlsx'), 'family', filtered=True, coloured=True, swap=(9, 10), **hike)
        print('hike open:', sum(o - (p or 0) for _, _, o, p in HIKE), 'rows', FIRST, hike_last)
        calc.picture(starter, os.path.join(HERE, 'bilder', 'zahlungen.svg'))
        calc.picture(os.path.join(stufen, 'zahlungen.xlsx'), os.path.join(HERE, 'bilder', 'zahlungen-fertig.svg'))
        calc.picture(os.path.join(stufen, 'nur-namen.xlsx'), os.path.join(HERE, 'bilder', 'nur-namen.svg'))
    print('written: static/sort-and-filter/05 Zahlungen.xlsx, stufen/*.xlsx, bilder/*.svg')
