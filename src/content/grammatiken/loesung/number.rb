# Zahlen aus Ziffern, nach der Grammatik
#   number = digit { digit }
#   digit  = "0" | "1" | … | "9"

# Das Zeichen am Index i, am Ende des Worts ein leerer String
def peek(word, i)
    word[i] || ''
end

# digit = "0" | "1" | … | "9"
def digit(word, i)
    return nil unless peek(word, i).between?('0', '9')
    i + 1
end

# number = digit { digit }
def number(word, i)
    i = digit(word, i)
    return nil if i.nil?
    i = digit(word, i) while peek(word, i).between?('0', '9')
    i
end

p number('123', 0)
p number('12a', 0)
p number('a1', 0)
