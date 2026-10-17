# Ist eine Binärzahl durch 5 teilbar? Lösung zu »Teilbar durch 5«: Der Zustand
# ist der Rest beim Teilen durch 5; eine Ziffer d macht aus q_r den Zustand
# mit dem Rest (2·r + d) mod 5.
START = 'q0'
ACCEPTING = {'q0'}
TRANSITIONS = {
    ('q0', '0'): 'q0', ('q0', '1'): 'q1',
    ('q1', '0'): 'q2', ('q1', '1'): 'q3',
    ('q2', '0'): 'q4', ('q2', '1'): 'q0',
    ('q3', '0'): 'q1', ('q3', '1'): 'q2',
    ('q4', '0'): 'q3', ('q4', '1'): 'q4',
}


def accepts(word):
    state = START
    for symbol in word:
        state = TRANSITIONS[(state, symbol)]
    return state in ACCEPTING


for word in ['11', '110', '111', '1001', '10000']:
    print(word, int(word, 2), accepts(word))
