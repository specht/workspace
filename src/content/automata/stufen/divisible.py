# Ist eine Binärzahl durch 3 teilbar? Der Automat liest sie Ziffer für Ziffer
# von links. Sein Zustand ist der Rest, der bis dahin beim Teilen durch 3
# bleibt: In q2 ist der Rest 2.
START = 'q0'
ACCEPTING = {'q0'}
TRANSITIONS = {
    ('q0', '0'): 'q0', ('q0', '1'): 'q1',
    ('q1', '0'): 'q2', ('q1', '1'): 'q0',
    ('q2', '0'): 'q1', ('q2', '1'): 'q2',
}


def accepts(word):
    state = START
    for symbol in word:
        state = TRANSITIONS[(state, symbol)]
    return state in ACCEPTING


for word in ['11', '110', '111', '1001', '10000']:
    print(word, int(word, 2), accepts(word))
