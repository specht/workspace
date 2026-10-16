# Ist eine Binärzahl durch 5 teilbar? Lösung zu »Teilbar durch 5«: Der Zustand
# ist der Rest beim Teilen durch 5; eine Ziffer d macht aus dem Rest r den
# Rest (2·r + d) mod 5.
START = 0
ACCEPTING = {0}
TRANSITIONS = {
    (0, '0'): 0, (0, '1'): 1,
    (1, '0'): 2, (1, '1'): 3,
    (2, '0'): 4, (2, '1'): 0,
    (3, '0'): 1, (3, '1'): 2,
    (4, '0'): 3, (4, '1'): 4,
}


def accepts(word):
    state = START
    for symbol in word:
        state = TRANSITIONS[(state, symbol)]
    return state in ACCEPTING


for word in ['11', '110', '111', '1001', '10000']:
    print(word, int(word, 2), accepts(word))
