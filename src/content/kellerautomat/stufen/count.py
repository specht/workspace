# Stehen die runden Klammern richtig? Ein Zähler merkt sich, wie viele
# Klammern gerade offen sind.
def balanced(word):
    depth = 0
    for symbol in word:
        if symbol == '(':
            depth += 1
        elif symbol == ')':
            depth -= 1
            if depth < 0:
                return False
    return depth == 0


for word in ['(()())', '(()', '())(', '']:
    print(repr(word), balanced(word))
