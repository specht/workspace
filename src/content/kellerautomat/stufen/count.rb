# Stehen die runden Klammern richtig? Ein Zähler merkt sich, wie viele
# Klammern gerade offen sind.
def balanced(word)
    depth = 0
    word.each_char do |symbol|
        if symbol == '('
            depth += 1
        elsif symbol == ')'
            depth -= 1
            return false if depth < 0
        end
    end
    depth == 0
end

['(()())', '(()', '())(', ''].each do |word|
    puts "#{word.inspect} #{balanced(word)}"
end
