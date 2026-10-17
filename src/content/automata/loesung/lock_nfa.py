# Das Zahlenschloss als nichtdeterministischer endlicher Automat (NEA).
# Ein Übergang führt hier zu einer Menge von Zuständen: Der NEA darf raten.
# In q2 stimmen die ersten zwei Ziffern des Codes, wie beim Schloss vorher.
DIGITS = '0123456789'
START = 'q0'
ACCEPTING = {'q4', 'q8'}
TRANSITIONS = {}

# In q0 wartet der NEA: Jede Ziffer führt wieder nach q0 …
for digit in DIGITS:
    TRANSITIONS[('q0', digit)] = {'q0'}
# … aber bei einer 1 darf er auch raten, dass hier der Code beginnt.
TRANSITIONS[('q0', '1')] = {'q0', 'q1'}
TRANSITIONS[('q1', '2')] = {'q2'}
TRANSITIONS[('q2', '1')] = {'q3'}
TRANSITIONS[('q3', '3')] = {'q4'}
# Lösung: Der Generalschlüssel 4711 ist ein zweiter Weg, geraten ab einer 4.
TRANSITIONS[('q0', '4')] = {'q0', 'q5'}
TRANSITIONS[('q5', '7')] = {'q6'}
TRANSITIONS[('q6', '1')] = {'q7'}
TRANSITIONS[('q7', '1')] = {'q8'}


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
