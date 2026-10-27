# Eine Turingmaschine addiert zwei Zahlen in Strichen: 111+11 heißt 3 + 2,
# und am Ende steht 11111 auf dem Band.
START = 'q0'
HALT = 'qH'
# (Zustand, gelesenes Zeichen) -> (was sie schreibt, wohin der Kopf geht
# (L links, R rechts), neuer Zustand)
TRANSITIONS = {
    # q0: über die erste Zahl; das + wird zu einem Strich
    ('q0', '1'): ('1', 'R', 'q0'),
    ('q0', '+'): ('1', 'R', 'q1'),
    # q1: über die zweite Zahl bis ans Ende
    ('q1', '1'): ('1', 'R', 'q1'),
    ('q1', '_'): ('_', 'L', 'q2'),
    # q2: ein Strich zu viel, der letzte geht weg
    ('q2', '1'): ('_', 'L', HALT),
}


# Das Band ab dem ersten bis zum letzten beschriebenen Feld, der Kopf in [ ]
def show(tape, head):
    used = [i for i in tape if tape[i] != '_'] + [head]
    return ''.join(f'[{tape.get(i, "_")}]' if i == head else tape.get(i, '_') for i in range(min(used), max(used) + 1))


def run(word, trace=False, max_steps=1000):
    tape = dict(enumerate(word))
    head = 0
    state = START
    steps = 0
    if trace:
        print(f'{state:<3} {show(tape, head)}')
    while state != HALT:
        if steps == max_steps:
            raise RuntimeError(f'Nach {max_steps} Schritten hat die Maschine nicht angehalten.')
        write, move, state = TRANSITIONS[(state, tape.get(head, '_'))]
        tape[head] = write
        head += 1 if move == 'R' else -1
        steps += 1
        if trace:
            print(f'{state:<3} {show(tape, head)}')
    used = [i for i in tape if tape[i] != '_']
    return ''.join(tape.get(i, '_') for i in range(min(used), max(used) + 1)) if used else ''


run('111+11', trace=True)
for word in ['1+1', '111+11', '+11', '11+']:
    print(f'{word} → {run(word)}')
