# Ist die N-te Ziffer von hinten eine 1? Der NEA rät, wo diese 1 steht, und
# zählt danach die Ziffern bis zum Ende ab: In q2 hat er zwei gezählt.
N = 3
DIGITS = '01'
START = 'q0'
ACCEPTING = {f'q{N}'}
TRANSITIONS = {
    ('q0', '0'): {'q0'},
    ('q0', '1'): {'q0', 'q1'},
}
for i in range(1, N):
    TRANSITIONS[(f'q{i}', '0')] = {f'q{i + 1}'}
    TRANSITIONS[(f'q{i}', '1')] = {f'q{i + 1}'}
