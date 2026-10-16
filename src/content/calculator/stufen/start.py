# Ein Rechner für Ausdrücke


def tokenize(text):
    """Zerlegt einen Ausdruck in Tokens: Zahlen, Rechenzeichen und Klammern."""
    return []


def parse(tokens):
    """Macht aus Tokens einen Baum."""
    return tokens[0] if tokens else None


def evaluate(tree, variables=None):
    """Rechnet einen Baum aus."""
    return tree


def calculate(text, variables=None):
    return evaluate(parse(tokenize(text)), variables)


while True:
    try:
        line = input('> ').strip()
    except EOFError:
        print()
        break
    if not line:
        continue
    try:
        print(calculate(line))
    except ValueError as error:
        print(f'Fehler: {error}')
