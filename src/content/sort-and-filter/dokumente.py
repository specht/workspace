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


def serial(day):
    return (datetime.date(YEAR, 3, day) - datetime.date(1899, 12, 30)).days


def table(calc, path, order='entered', names_only=False, filtered=False, coloured=False, probe=False):
    doc = calc.new()
    rows = list(PAYMENTS)
    if order == 'family':
        rows.sort(key=lambda p: german(p[1]))
    put(doc, 'A1', [['Zahlungen für die Klassenfahrt'], [], ['Datum', 'Familie', 'Soll', 'Gezahlt', 'Offen']])
    names = sorted((p[1] for p in PAYMENTS), key=german) if names_only else None
    for i, (day, family, owed, paid) in enumerate(rows):
        r = FIRST + i
        put(doc, f'A{r}', [[serial(day) if day else None, names[i] if names else family, owed, paid, f'=C{r}-D{r}']])
    put(doc, f'A{LAST + 2}', [[None, 'Noch offen', None, None, f'=SUM(E{FIRST}:E{LAST})']])
    bold(doc, f'A1, A3:E3, B{LAST + 2}:E{LAST + 2}')
    fmt(doc, f'C{FIRST}:E{LAST + 2}', CURRENCY)
    fmt(doc, f'A{FIRST}:A{LAST}', 'TT.MM.JJJJ')
    widths(doc, [2.8, 3.6, 2.6, 2.6, 2.6])
    sh = sheet_of(doc)
    if filtered:
        area = sh.getCellRangeByName(f'A3:E{LAST}')
        db = doc.DatabaseRanges
        db.addNewByName('Zahlungen', area.RangeAddress)
        db.getByName('Zahlungen').AutoFilter = True
    if coloured:
        entries = sh.getCellRangeByName(f'E{FIRST}:E{LAST}').ConditionalFormat
        entries.addNew((prop('Operator', uno_operator('GREATER')), prop('Formula1', '0'), prop('StyleName', 'Bad')))
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
        calc.picture(starter, os.path.join(HERE, 'bilder', 'zahlungen.svg'))
        calc.picture(os.path.join(stufen, 'zahlungen.xlsx'), os.path.join(HERE, 'bilder', 'zahlungen-fertig.svg'))
        calc.picture(os.path.join(stufen, 'nur-namen.xlsx'), os.path.join(HERE, 'bilder', 'nur-namen.svg'))
    print('written: static/sort-and-filter/05 Zahlungen.xlsx, stufen/*.xlsx, bilder/*.svg')
