# Wörter aus Klammerpaaren, nach der Grammatik
#   brackets = { "(" brackets ")" }
# Jede Funktion liest ab dem Index i, was ihre Regel erlaubt, und gibt den
# Index dahinter zurück, oder nil, wenn das Wort dort nicht passt.

# Das Zeichen am Index i, am Ende des Worts ein leerer String
def peek(word, i)
    word[i] || ''
end

# brackets = { "(" brackets ")" }
def brackets(word, i)
    while peek(word, i) == '('
        i = brackets(word, i + 1)
        return nil if i.nil? || peek(word, i) != ')'
        i += 1
    end
    i
end

# Das Wort gehört dazu, wenn brackets es bis zum Ende liest
def accepts(word)
    brackets(word, 0) == word.length
end

['(())()', '(()', '())', ''].each do |word|
    puts "#{word.inspect} #{accepts(word)}"
end
