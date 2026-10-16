# Ein Zahlenschloss mit dem Code 1213. Es öffnet, sobald die letzten vier
# getippten Ziffern der Code sind. Der Zustand merkt sich, wie viel vom Code
# schon stimmt.
START = 'start'
ACCEPTING = {'1213'}
TRANSITIONS = {
    ('start', '1'): '1',
    ('1', '2'): '12',
    ('12', '1'): '121',
    ('121', '3'): '1213',
    # Lösung: Nicht jede falsche Ziffer heißt »von vorn«. Eine 1 ist immer
    # der Anfang des Codes, und nach 121 stimmt bei einer 2 noch 12.
    ('1', '1'): '1',
    ('121', '1'): '1',
    ('121', '2'): '12',
    ('1213', '1'): '1',
}


def opens(digits):
    state = START
    for digit in digits:
        # Fehlt ein Übergang, geht es zurück zum Start.
        state = TRANSITIONS.get((state, digit), START)
    return state in ACCEPTING


for digits in ['1213', '991213', '12', '121213']:
    print(digits, opens(digits))
