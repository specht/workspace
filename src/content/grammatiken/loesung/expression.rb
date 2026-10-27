# Ausdrücke mit + und * und Klammern, nach der Grammatik
#   expression = term { "+" term }
#   term       = factor { "*" factor }
#   factor     = number | "(" expression ")"
#   number     = digit { digit }
#   digit      = "0" | "1" | … | "9"

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

# expression = term { "+" term }
def expression(word, i)
    i = term(word, i)
    while !i.nil? && peek(word, i) == '+'
        i = term(word, i + 1)
    end
    i
end

# term = factor { "*" factor }
def term(word, i)
    i = factor(word, i)
    while !i.nil? && peek(word, i) == '*'
        i = factor(word, i + 1)
    end
    i
end

# factor = number | "(" expression ")"
def factor(word, i)
    return number(word, i) unless peek(word, i) == '('
    i = expression(word, i + 1)
    return nil if i.nil? || peek(word, i) != ')'
    i + 1
end

# Das Wort gehört dazu, wenn expression es bis zum Ende liest
def accepts(word)
    expression(word, 0) == word.length
end

['2+3*4', '(1+2)*3', '2+', '(1+2', '2*+3'].each do |word|
    puts "#{word.inspect} #{accepts(word)}"
end
