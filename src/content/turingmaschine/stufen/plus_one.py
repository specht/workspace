# Eine Turingmaschine zählt 1 zu einer Binärzahl dazu. Sie hat ein Band aus
# Feldern, einen Kopf, der auf einem Feld steht, und einen Zustand. _ ist
# ein leeres Feld; das Band geht nach beiden Seiten ohne Ende weiter.
START = 'q0'
HALT = 'qH'
# (Zustand, gelesenes Zeichen) -> (was sie schreibt, wohin der Kopf geht
# (L links, R rechts), neuer Zustand)
TRANSITIONS = {
    # q0: nach rechts bis hinter die letzte Ziffer
    ('q0', '0'): ('0', 'R', 'q0'),
    ('q0', '1'): ('1', 'R', 'q0'),
    ('q0', '_'): ('_', 'L', 'q1'),
    # q1: von hinten 1 dazu, mit Übertrag
    ('q1', '1'): ('0', 'L', 'q1'),
    ('q1', '0'): ('1', 'L', HALT),
    ('q1', '_'): ('1', 'L', HALT),
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


run('1011', trace=True)
for word in ['0', '1', '1011', '111']:
    print(f'{word} → {run(word)}')
