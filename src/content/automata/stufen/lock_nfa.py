# Das Zahlenschloss als nichtdeterministischer endlicher Automat (NEA).
# Ein Übergang führt hier zu einer Menge von Zuständen: Der NEA darf raten.
DIGITS = '0123456789'
START = 'start'
ACCEPTING = {'1213'}
TRANSITIONS = {}

# In start wartet der NEA: Jede Ziffer führt wieder nach start …
for digit in DIGITS:
    TRANSITIONS[('start', digit)] = {'start'}
# … aber bei einer 1 darf er auch raten, dass hier der Code beginnt.
TRANSITIONS[('start', '1')] = {'start', '1'}
TRANSITIONS[('1', '2')] = {'12'}
TRANSITIONS[('12', '1')] = {'121'}
TRANSITIONS[('121', '3')] = {'1213'}


def opens(digits):
    states = {START}
    for digit in digits:
        new_states = set()
        for state in states:
            new_states |= TRANSITIONS.get((state, digit), set())
        states = new_states
        print(digit, sorted(states))
    return len(states & ACCEPTING) > 0


if __name__ == '__main__':
    print(opens('121213'))
