# Ist eine Binärzahl durch 3 teilbar? Der Automat liest sie Ziffer für Ziffer
# von links. Sein Zustand ist der Rest, der bis dahin beim Teilen durch 3
# bleibt: In q2 ist der Rest 2.
START = 'q0'
ACCEPTING = ['q0']
TRANSITIONS = {
    ['q0', '0'] => 'q0', ['q0', '1'] => 'q1',
    ['q1', '0'] => 'q2', ['q1', '1'] => 'q0',
    ['q2', '0'] => 'q1', ['q2', '1'] => 'q2',
}

def accepts(word)
    state = START
    word.each_char do |symbol|
        state = TRANSITIONS.fetch([state, symbol])
    end
    ACCEPTING.include?(state)
end

['11', '110', '111', '1001', '10000'].each do |word|
    puts "#{word} #{word.to_i(2)} #{accepts(word)}"
end
