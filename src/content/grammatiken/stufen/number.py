# Zahlen aus Ziffern, nach der Grammatik
#   number = digit { digit }
#   digit  = "0" | "1" | … | "9"

# Das Zeichen am Index i, am Ende des Worts ein leerer String
def peek(word, i):
    return word[i] if i < len(word) else ''


# hier: digit und number

print(number('123', 0))
print(number('12a', 0))
print(number('a1', 0))
