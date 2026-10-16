# Ist eine Binärzahl durch 5 teilbar? Lösung zu »Teilbar durch 5«: Der Zustand
# ist der Rest beim Teilen durch 5; eine Ziffer d macht aus dem Rest r den
# Rest (2·r + d) mod 5.
START = 0
ACCEPTING = [0]
TRANSITIONS = {
    [0, '0'] => 0, [0, '1'] => 1,
    [1, '0'] => 2, [1, '1'] => 3,
    [2, '0'] => 4, [2, '1'] => 0,
    [3, '0'] => 1, [3, '1'] => 2,
    [4, '0'] => 3, [4, '1'] => 4,
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
