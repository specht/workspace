# Eine Turingmaschine zieht 1 von einer Binärzahl ab, die größer als 0 ist.
# Eine 0 vorne darf stehen bleiben: Aus 1000 wird 0111.
START = 'q0'
HALT = 'qH'
# (Zustand, gelesenes Zeichen) -> (was sie schreibt, wohin der Kopf geht
# (L links, R rechts), neuer Zustand)
TRANSITIONS = {
    # q0: nach rechts bis hinter die letzte Ziffer
    ('q0', '0'): ('0', 'R', 'q0'),
    ('q0', '1'): ('1', 'R', 'q0'),
    ('q0', '_'): ('_', 'L', 'q1'),
    # q1: von hinten 1 ab, mit Übertrag
    ('q1', '0'): ('1', 'L', 'q1'),
    ('q1', '1'): ('0', 'L', HALT),
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


run('1100', trace=True)
for word in ['1', '10', '1100', '1000']:
    print(f'{word} → {run(word)}')
