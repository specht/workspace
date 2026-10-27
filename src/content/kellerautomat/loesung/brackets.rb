# Stehen runde, eckige und geschweifte Klammern richtig? Der Stack merkt
# sich, welche Klammern offen sind, die zuletzt geöffnete oben.
PAIRS = { ')' => '(', ']' => '[', '}' => '{' }

def balanced(word)
    stack = []
    word.each_char do |symbol|
        if PAIRS.value?(symbol)
            stack.push(symbol)
        elsif PAIRS.key?(symbol)
            return false if stack.empty? || stack.pop != PAIRS[symbol]
        end
    end
    stack.empty?
end

['([]{})', '([)]', '((', ''].each do |word|
    puts "#{word.inspect} #{balanced(word)}"
end
