# Wörter aus Klammerpaaren, nach der Grammatik
#   brackets = { "(" brackets ")" }
# Jede Funktion liest ab dem Index i, was ihre Regel erlaubt, und gibt den
# Index dahinter zurück, oder None, wenn das Wort dort nicht passt.

# Das Zeichen am Index i, am Ende des Worts ein leerer String
def peek(word, i):
    return word[i] if i < len(word) else ''


# brackets = { "(" brackets ")" }
def brackets(word, i):
    while peek(word, i) == '(':
        i = brackets(word, i + 1)
        if i is None or peek(word, i) != ')':
            return None
        i += 1
    return i


# Das Wort gehört dazu, wenn brackets es bis zum Ende liest
def accepts(word):
    return brackets(word, 0) == len(word)


for word in ['(())()', '(()', '())', '']:
    print(repr(word), accepts(word))
