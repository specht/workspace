# Ein Rechner für Ausdrücke: Musterlösung zu allen Abschnitten.


# --- Zerlegen ----------------------------------------------------------------

def tokenize(text):
    """Zerlegt einen Ausdruck in Tokens: Zahlen als int oder float, Namen,
    Rechenzeichen und Klammern als Strings mit einem Zeichen."""
    tokens = []
    i = 0
    while i < len(text):
        c = text[i]
        if c == ' ':
            i += 1
        elif c.isdigit():
            start = i
            while i < len(text) and text[i].isdigit():
                i += 1
            if i + 1 < len(text) and text[i] == '.' and text[i + 1].isdigit():
                i += 1
                while i < len(text) and text[i].isdigit():
                    i += 1
                tokens.append(float(text[start:i]))
            else:
                tokens.append(int(text[start:i]))
        elif c.isalpha() or c == '_':
            start = i
            while i < len(text) and (text[i].isalnum() or text[i] == '_'):
                i += 1
            tokens.append(text[start:i])
        elif c in '+-*/^()':
            tokens.append(c)
            i += 1
        else:
            raise ValueError(f'Unbekanntes Zeichen »{c}« an Stelle {i + 1}')
    return tokens


# --- Ein Baum aus Tokens -----------------------------------------------------
#
#   expression = term { ("+" | "-") term }
#   term       = factor { ("*" | "/") factor }
#   factor     = "-" factor | power
#   power      = atom [ "^" factor ]
#   atom       = Zahl | Name | "(" expression ")"
#
# Jede Funktion nimmt vorn aus tokens, was zu ihr gehört, und gibt den Baum
# dafür zurück. first(tokens) ist das nächste Token, tokens.pop(0) nimmt es.

def first(tokens):
    return tokens[0] if tokens else None


def parse(tokens):
    tokens = list(tokens)
    tree = expression(tokens)
    if tokens:
        raise ValueError(f'Unerwartet: {tokens[0]!r}')
    return tree


def expression(tokens):
    tree = term(tokens)
    while first(tokens) in ('+', '-'):
        tree = [tokens.pop(0), tree, term(tokens)]
    return tree


def term(tokens):
    tree = factor(tokens)
    while first(tokens) in ('*', '/'):
        tree = [tokens.pop(0), tree, factor(tokens)]
    return tree


def factor(tokens):
    if first(tokens) == '-':
        tokens.pop(0)
        return ['neg', factor(tokens)]
    return power(tokens)


def power(tokens):
    base = atom(tokens)
    if first(tokens) != '^':
        return base
    tokens.pop(0)
    return ['^', base, factor(tokens)]


def atom(tokens):
    if not tokens:
        raise ValueError('Der Ausdruck hört zu früh auf')
    token = tokens.pop(0)
    if isinstance(token, (int, float)) or is_name(token):
        return token
    if token == '(':
        tree = expression(tokens)
        if first(tokens) != ')':
            raise ValueError('Es fehlt eine schließende Klammer')
        tokens.pop(0)
        return tree
    raise ValueError(f'Unerwartet: {token!r}')


def is_name(token):
    return isinstance(token, str) and (token[0].isalpha() or token[0] == '_')


# --- Ausrechnen ---------------------------------------------------------------

def evaluate(tree, variables=None):
    variables = variables or {}
    if isinstance(tree, (int, float)):
        return tree
    if is_name(tree):
        if tree not in variables:
            raise ValueError(f'Unbekannter Name: {tree}')
        return variables[tree]
    op, *args = tree
    values = [evaluate(arg, variables) for arg in args]
    if op == '+':
        return values[0] + values[1]
    if op == '-':
        return values[0] - values[1]
    if op == '*':
        return values[0] * values[1]
    if op == '/':
        return values[0] / values[1]
    if op == '^':
        return values[0] ** values[1]
    if op == 'neg':
        return -values[0]


def calculate(text, variables=None):
    return evaluate(parse(tokenize(text)), variables)


import re
variables = {}
while True:
    try:
        line = input('> ').strip()
    except EOFError:
        print()
        break
    if not line:
        continue
    try:
        m = re.fullmatch(r'([A-Za-z_]\w*)\s*=(.*)', line)
        if m:
            variables[m[1]] = calculate(m[2], variables)
            print(f'{m[1]} = {variables[m[1]]}')
        else:
            print(calculate(line, variables))
    except (ValueError, ZeroDivisionError) as error:
        print(f'Fehler: {error}')
