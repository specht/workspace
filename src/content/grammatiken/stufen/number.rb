# Zahlen aus Ziffern, nach der Grammatik
#   number = digit { digit }
#   digit  = "0" | "1" | … | "9"

# Das Zeichen am Index i, am Ende des Worts ein leerer String
def peek(word, i)
    word[i] || ''
end

# hier: digit und number

p number('123', 0)
p number('12a', 0)
p number('a1', 0)
