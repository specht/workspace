# Ein Kellerautomat prüft runde und eckige Klammern. Er liest das Wort Zeichen für
# Zeichen und hat einen Stack; ganz unten liegt #, das Zeichen für »leer«.
START = 'q0'
ACCEPTING = {'q0'}
# (Zustand, gelesenes Zeichen, oberstes Zeichen des Stacks)
#   -> (neuer Zustand, was an dessen Stelle auf den Stack kommt)
# Der Stack wird von unten nach oben geschrieben: '#(' heißt, # bleibt
# liegen und ( kommt oben drauf; '' heißt, das oberste Zeichen ist weg.
TRANSITIONS = {
    ('q0', '(', '#'): ('q0', '#('),
    ('q0', '(', '('): ('q0', '(('),
    ('q0', '(', '['): ('q0', '[('),
    ('q0', '[', '#'): ('q0', '#['),
    ('q0', '[', '('): ('q0', '(['),
    ('q0', '[', '['): ('q0', '[['),
    ('q0', ')', '('): ('q0', ''),
    ('q0', ']', '['): ('q0', ''),
}


def accepts(word, trace=False):
    state = START
    stack = ['#']
    if trace:
        print(f'      {state}  {"".join(stack)}')
    for symbol in word:
        top = stack.pop()
        move = TRANSITIONS.get((state, symbol, top))
        # kein Übergang: der Fehlerzustand, das Wort ist abgelehnt
        if move is None:
            return False
        state, push = move
        stack.extend(push)
        if trace:
            print(f'{symbol:<5} {state}  {"".join(stack)}')
    return state in ACCEPTING and stack == ['#']


accepts('([]())', trace=True)
for word in ['([]())', '([)]', '[[', '']:
    print(repr(word), accepts(word))
