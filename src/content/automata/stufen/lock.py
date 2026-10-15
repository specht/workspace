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
}


def opens(digits):
    state = START
    for digit in digits:
        # Fehlt ein Übergang, geht es zurück zum Start.
        state = TRANSITIONS.get((state, digit), START)
    return state in ACCEPTING


for digits in ['1213', '991213', '12', '121213']:
    print(digits, opens(digits))
