# Eine Turingmaschine prüft a^n b^n c^n: erst n-mal a, dann n-mal b, dann
# n-mal c. In jeder Runde streicht sie ein a, ein b und ein c durch (X, Y,
# Z). Steht kein a mehr da, prüft sie, dass auch nichts anderes übrig ist.
START = 'q0'
ACCEPT = 'qJ'
TRANSITIONS = {
    # q0: das nächste a durchstreichen; keins mehr: zur Prüfung
    ('q0', 'a'): ('X', 'R', 'q1'),
    ('q0', 'Y'): ('Y', 'R', 'q4'),
    ('q0', '_'): ('_', 'R', ACCEPT),
    # q1: nach rechts zum ersten b, durchstreichen
    ('q1', 'a'): ('a', 'R', 'q1'),
    ('q1', 'Y'): ('Y', 'R', 'q1'),
    ('q1', 'b'): ('Y', 'R', 'q2'),
    # q2: weiter zum ersten c, durchstreichen
    ('q2', 'b'): ('b', 'R', 'q2'),
    ('q2', 'Z'): ('Z', 'R', 'q2'),
    ('q2', 'c'): ('Z', 'L', 'q3'),
    # q3: zurück bis hinter das letzte X
    ('q3', 'a'): ('a', 'L', 'q3'),
    ('q3', 'b'): ('b', 'L', 'q3'),
    ('q3', 'Y'): ('Y', 'L', 'q3'),
    ('q3', 'Z'): ('Z', 'L', 'q3'),
    ('q3', 'X'): ('X', 'R', 'q0'),
    # q4: hinter den a nur noch Y und Z
    ('q4', 'Y'): ('Y', 'R', 'q4'),
    ('q4', 'Z'): ('Z', 'R', 'q4'),
    ('q4', '_'): ('_', 'L', ACCEPT),
}


# Das Band ab dem ersten bis zum letzten beschriebenen Feld, der Kopf in [ ]
def show(tape, head):
    used = [i for i in tape if tape[i] != '_'] + [head]
    return ''.join(f'[{tape.get(i, "_")}]' if i == head else tape.get(i, '_') for i in range(min(used), max(used) + 1))


# Nimmt die Maschine das Wort an? Ohne passenden Übergang hält sie und
# lehnt ab. steps zählt die Schritte bis dahin.
def accepts(word, trace=False):
    tape = dict(enumerate(word))
    head = 0
    state = START
    steps = 0
    if trace:
        print(state, show(tape, head))
    while state != ACCEPT:
        move = TRANSITIONS.get((state, tape.get(head, '_')))
        if move is None:
            break
        write, direction, state = move
        tape[head] = write
        head += 1 if direction == 'R' else -1
        steps += 1
        if trace:
            print(state, show(tape, head))
    if trace:
        print(steps, 'Schritte')
    return state == ACCEPT


accepts('aabbcc', trace=True)
for word in ['abc', 'aabbcc', 'aabbc', 'abcabc', '']:
    print(repr(word), accepts(word))
