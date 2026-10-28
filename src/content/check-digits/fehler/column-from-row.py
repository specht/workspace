# Häufiger Fehler: für die Spalte c wird die Zeile c genommen

def parity(bits):
    return sum(bits) % 2


def find_error(grid):
    row = None
    for r in range(len(grid)):
        if parity(grid[r]) == 1:
            row = r
    column = None
    for c in range(len(grid[0])):
        bits = grid[c] if c < len(grid) else []
        if parity(bits) == 1:
            column = c
    if row is None or column is None:
        return None
    return [row, column]
