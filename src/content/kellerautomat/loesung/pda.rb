# Ein Kellerautomat prüft runde und eckige Klammern. Er liest das Wort Zeichen für
# Zeichen und hat einen Stack; ganz unten liegt #, das Zeichen für »leer«.
START = 'q0'
ACCEPTING = ['q0']
# [Zustand, gelesenes Zeichen, oberstes Zeichen des Stacks]
#   => [neuer Zustand, was an dessen Stelle auf den Stack kommt]
# Der Stack wird von unten nach oben geschrieben: '#(' heißt, # bleibt
# liegen und ( kommt oben drauf; '' heißt, das oberste Zeichen ist weg.
TRANSITIONS = {
    ['q0', '(', '#'] => ['q0', '#('],
    ['q0', '(', '('] => ['q0', '(('],
    ['q0', '(', '['] => ['q0', '[('],
    ['q0', '[', '#'] => ['q0', '#['],
    ['q0', '[', '('] => ['q0', '(['],
    ['q0', '[', '['] => ['q0', '[['],
    ['q0', ')', '('] => ['q0', ''],
    ['q0', ']', '['] => ['q0', ''],
}

def accepts(word, trace: false)
    state = START
    stack = ['#']
    puts "      #{state}  #{stack.join}" if trace
    word.each_char do |symbol|
        top = stack.pop
        move = TRANSITIONS[[state, symbol, top]]
        # kein Übergang: der Fehlerzustand, das Wort ist abgelehnt
        return false if move.nil?
        state, push = move
        stack.concat(push.chars)
        puts "#{symbol.ljust(5)} #{state}  #{stack.join}" if trace
    end
    ACCEPTING.include?(state) && stack == ['#']
end

accepts('([]())', trace: true)
['([]())', '([)]', '[[', ''].each do |word|
    puts "#{word.inspect} #{accepts(word)}"
end
