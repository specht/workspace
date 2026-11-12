# The tables of »Auswerten«: the class's survey after the trip, 27 answers
# with one to five stars for four parts of it, some left out (an empty
# cell, not a 0). The starting file: the answers and the labels of the
# summary below them. The solution: per column ANZAHL, MITTELWERT (one
# decimal), MAX, MIN and WENN (»ja« from a mean of 4 on). The hostel's mean
# is 3,96: shown as 4,0, but WENN says »nein«. Stages that must fail: the
# means typed, worked out with the empty cells as 0; MITTELWERT over a
# range that misses the last rows. Run in the office image:
#
#     src/scripts/office python3 src/content/spreadsheet-functions/dokumente.py

import os
import random
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from calc_build import Calc, put, fmt, bold, widths  # noqa: E402

STATIC = os.path.join(HERE, '..', '..', 'static', 'spreadsheet-functions')
STARTER = '04 Umfrage.xlsx'
ANSWERS = 27
FIRST = 4                     # the first answer's row
LAST = FIRST + ANSWERS - 1    # 30
# column: (title, answers given, sum of stars, lowest, highest, empty rows)
PARTS = {
    'B': ('Kletterwald', 26, 115, 3, 5, [11]),
    'C': ('Schifffahrt', 27, 97, 1, 5, []),
    'D': ('Moormuseum', 24, 62, 1, 4, [7, 19, 26]),
    'E': ('Jugendherberge', 25, 99, 2, 5, [15, 28]),
}
SUMMARY = ['Antworten', 'Mittelwert', 'Bester Wert', 'Schlechtester Wert', 'Wieder hin?']
S0 = LAST + 2                 # 32: the first row of the summary
FUNCS = ['=COUNT({c}4:{c}30)', '=AVERAGE({c}4:{c}30)', '=MAX({c}4:{c}30)', '=MIN({c}4:{c}30)',
         '=IF({c}33>=4;"ja";"nein")']   # UNO's formulas separate arguments with ;


def stars(n, total, lo, hi, seed):
    """n answers from lo to hi stars that add up to total, both ends present"""
    rng = random.Random(seed)
    values = [lo, hi] + [round(total / n)] * (n - 2)
    while sum(values) != total:
        i = rng.randrange(2, n)
        step = 1 if sum(values) < total else -1
        if lo <= values[i] + step <= hi:
            values[i] += step
    # spread them a little, keeping the sum
    for _ in range(n):
        i, j = rng.randrange(2, n), rng.randrange(2, n)
        if lo <= values[i] + 1 <= hi and lo <= values[j] - 1 <= hi:
            values[i] += 1
            values[j] -= 1
    rng.shuffle(values)
    return values


def columns():
    out = {}
    for k, (col, (_, n, total, lo, hi, empty)) in enumerate(PARTS.items()):
        given = iter(stars(n, total, lo, hi, 7 + k))
        out[col] = {r: (None if r in empty else next(given)) for r in range(FIRST, LAST + 1)}
    return out


def table(calc, path, summary=None):
    """summary: None (labels only), 'functions', 'typed' (means worked out
    with the empty cells as 0) or 'short' (the ranges end at row 25)"""
    doc = calc.new()
    data = columns()
    put(doc, 'A1', [['Umfrage nach der Klassenfahrt (1 bis 5 Sterne)'], [],
                    ['Antwort'] + [PARTS[c][0] for c in PARTS]])
    for r in range(FIRST, LAST + 1):
        put(doc, f'A{r}', [[r - FIRST + 1] + [data[c][r] for c in PARTS]])
    for i, label in enumerate(SUMMARY):
        put(doc, f'A{S0 + i}', [[label]])
    if summary:
        for c in PARTS:
            for i, f in enumerate(FUNCS):
                formula = f.format(c=c)
                if summary == 'short':
                    formula = formula.replace('30)', '25)')
                if summary == 'typed' and i == 1:
                    given = [v for v in data[c].values() if v is not None]
                    formula = round(sum(given) / ANSWERS, 1)
                put(doc, f'{c}{S0 + i}', [[formula]])
        fmt(doc, f'B{S0 + 1}:E{S0 + 1}', '0,0')
    bold(doc, f'A1, A3:E3, A{S0}:A{S0 + len(SUMMARY) - 1}')
    widths(doc, [4.7, 3.0, 3.0, 3.2, 3.7])
    calc.save(doc, path)


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    for folder in ('stufen', 'bilder'):
        os.makedirs(os.path.join(HERE, folder), exist_ok=True)
    stufen = os.path.join(HERE, 'stufen')
    data = columns()
    for c, (title, n, total, lo, hi, empty) in PARTS.items():
        given = [v for v in data[c].values() if v is not None]
        print(c, title, len(given), sum(given), round(sum(given) / len(given), 4), min(given), max(given))
    with Calc() as calc:
        starter = os.path.join(STATIC, STARTER)
        table(calc, starter)
        table(calc, os.path.join(stufen, 'umfrage.xlsx'), 'functions')
        table(calc, os.path.join(stufen, 'getippt.xlsx'), 'typed')
        table(calc, os.path.join(stufen, 'zu-kurz.xlsx'), 'short')
        calc.picture(starter, os.path.join(HERE, 'bilder', 'umfrage.svg'))
        calc.picture(os.path.join(stufen, 'umfrage.xlsx'), os.path.join(HERE, 'bilder', 'umfrage-fertig.svg'))
    print('written: static/spreadsheet-functions/04 Umfrage.xlsx, stufen/*.xlsx, bilder/*.svg')
