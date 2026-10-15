# Die Potenzmengenkonstruktion: Sie baut aus einem NEA einen DEA. Jeder
# Zustand des DEA ist eine Menge von Zuständen des NEA, nämlich die, in denen
# der NEA gerade sein könnte.
from lock_nfa import DIGITS, START, ACCEPTING, TRANSITIONS


# Alle Zustände, in die der NEA aus states mit symbol kommt.
def step(states, symbol):
    new_states = set()
    for state in states:
        new_states |= TRANSITIONS.get((state, symbol), set())
    return frozenset(new_states)


def to_dfa():
    start = frozenset({START})
    found = {start}
    todo = [start]
    transitions = {}
    while todo:
        states = todo.pop()
        for symbol in DIGITS:
            target = step(states, symbol)
            transitions[(states, symbol)] = target
            if target not in found:
                found.add(target)
                todo.append(target)
    return found, transitions


found, transitions = to_dfa()
print(len(found), 'Zustände')
for (states, symbol), target in transitions.items():
    if target != {START}:
        print(sorted(states), symbol, '→', sorted(target))
