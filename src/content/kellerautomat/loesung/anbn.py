# Ein Kellerautomat für a^n b^n: erst n-mal a, dann genauso oft b, also
# '', 'ab', 'aabb', 'aaabbb' und so weiter. Ganz unten im Stack liegt #.
START = 'q0'
ACCEPTING = {'q0', 'q1'}
# (Zustand, gelesenes Zeichen, oberstes Zeichen des Stacks)
#   -> (neuer Zustand, was an dessen Stelle auf den Stack kommt)
# Der Stack wird von unten nach oben geschrieben: '#(' heißt, # bleibt
# liegen und ( kommt oben drauf; '' heißt, das oberste Zeichen ist weg.
TRANSITIONS = {
    ('q0', 'a', '#'): ('q0', '#A'),
    ('q0', 'a', 'A'): ('q0', 'AA'),
    ('q0', 'b', 'A'): ('q1', ''),
    ('q1', 'b', 'A'): ('q1', ''),
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


accepts('aabb', trace=True)
for word in ['ab', 'aabb', 'aab', 'abab', 'ba', '']:
    print(repr(word), accepts(word))
