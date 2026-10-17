# Ein Zahlenschloss mit dem Code 1213. Es öffnet, sobald die letzten vier
# getippten Ziffern der Code sind. Der Zustand merkt sich, wie viel vom Code
# schon stimmt: In q2 stimmen die ersten zwei Ziffern, 12.
START = 'q0'
ACCEPTING = {'q4'}
TRANSITIONS = {
    ('q0', '1'): 'q1',
    ('q1', '2'): 'q2',
    ('q2', '1'): 'q3',
    ('q3', '3'): 'q4',
    # Lösung: Nicht jede falsche Ziffer heißt »von vorn«. Eine 1 ist immer
    # der Anfang des Codes, und nach 121 stimmt bei einer 2 noch 12.
    ('q1', '1'): 'q1',
    ('q3', '1'): 'q1',
    ('q3', '2'): 'q2',
    ('q4', '1'): 'q1',
}


def opens(digits):
    state = START
    for digit in digits:
        # Fehlt ein Übergang, geht es zurück zum Start.
        state = TRANSITIONS.get((state, digit), START)
    return state in ACCEPTING


for digits in ['1213', '991213', '12', '121213']:
    print(digits, opens(digits))
