# Ist die N-te Ziffer von hinten eine 1? Der NEA rät, wo diese 1 steht, und
# zählt danach die Ziffern bis zum Ende ab.
N = 3
DIGITS = '01'
START = 0
ACCEPTING = {N}
TRANSITIONS = {
    (0, '0'): {0},
    (0, '1'): {0, 1},
}
for i in range(1, N):
    TRANSITIONS[(i, '0')] = {i + 1}
    TRANSITIONS[(i, '1')] = {i + 1}
