# Ausdrücke mit + und * und Klammern, nach der Grammatik
#   expression = term { "+" term }
#   term       = factor { "*" factor }
#   factor     = number | "(" expression ")"
#   number     = digit { digit }
#   digit      = "0" | "1" | … | "9"

# Das Zeichen am Index i, am Ende des Worts ein leerer String
def peek(word, i):
    return word[i] if i < len(word) else ''


# digit = "0" | "1" | … | "9"
def digit(word, i):
    if not '0' <= peek(word, i) <= '9':
        return None
    return i + 1


# number = digit { digit }
def number(word, i):
    i = digit(word, i)
    if i is None:
        return None
    while '0' <= peek(word, i) <= '9':
        i = digit(word, i)
    return i


# expression = term { "+" term }
def expression(word, i):
    i = term(word, i)
    while i is not None and peek(word, i) == '+':
        i = term(word, i + 1)
    return i


# term = factor { "*" factor }
def term(word, i):
    i = factor(word, i)
    while i is not None and peek(word, i) == '*':
        i = factor(word, i + 1)
    return i


# factor = number | "(" expression ")"
def factor(word, i):
    if peek(word, i) != '(':
        return number(word, i)
    i = expression(word, i + 1)
    if i is None or peek(word, i) != ')':
        return None
    return i + 1


# Das Wort gehört dazu, wenn expression es bis zum Ende liest
def accepts(word):
    return expression(word, 0) == len(word)


for word in ['2+3*4', '(1+2)*3', '2+', '(1+2', '2*+3']:
    print(repr(word), accepts(word))
