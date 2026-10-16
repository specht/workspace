# Ist eine Binärzahl durch 3 teilbar? Der Automat liest sie Ziffer für Ziffer
# von links. Sein Zustand ist der Rest, der bis dahin beim Teilen durch 3 bleibt.
START = 0
ACCEPTING = [0]
TRANSITIONS = {
    [0, '0'] => 0, [0, '1'] => 1,
    [1, '0'] => 2, [1, '1'] => 0,
    [2, '0'] => 1, [2, '1'] => 2,
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
