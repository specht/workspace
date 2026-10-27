# Zahlen aus Ziffern, nach der Grammatik
#   number = digit { digit }
#   digit  = "0" | "1" | … | "9"

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


print(number('123', 0))
print(number('12a', 0))
print(number('a1', 0))
