# Stehen runde, eckige und geschweifte Klammern richtig? Der Stack merkt
# sich, welche Klammern offen sind, die zuletzt geöffnete oben.
PAIRS = {')': '(', ']': '[', '}': '{'}


def balanced(word):
    stack = []
    for symbol in word:
        if symbol in PAIRS.values():
            stack.append(symbol)
        elif symbol in PAIRS:
            if not stack or stack.pop() != PAIRS[symbol]:
                return False
    return not stack


for word in ['([]{})', '([)]', '((', '']:
    print(repr(word), balanced(word))
