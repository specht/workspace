# Ein Gitter aus Bits mit Paritätsbits: Jede Zeile und jede Spalte hat eine
# gerade Zahl von Einsen. Kippt ein Bit, findet find_error, wo.

def parity(bits):
    ones = 0
    for bit in bits:
        ones = ones + bit
    return ones % 2


def find_error(grid):
    # die Zeile mit einer ungeraden Zahl von Einsen
    row = None
    for r in range(len(grid)):
        if parity(grid[r]) == 1:
            row = r
    # die Spalte mit einer ungeraden Zahl von Einsen
    column = None
    for c in range(len(grid[0])):
        bits = [line[c] for line in grid]
        if parity(bits) == 1:
            column = c
    # alles gerade: kein Fehler
    if row is None or column is None:
        return None
    return [row, column]
