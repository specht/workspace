# Ein Zahlenschloss mit dem Code 1213. Es öffnet, sobald die letzten vier
# getippten Ziffern der Code sind. Der Zustand merkt sich, wie viel vom Code
# schon stimmt.
START = 'start'
ACCEPTING = ['1213']
TRANSITIONS = {
    ['start', '1'] => '1',
    ['1', '2'] => '12',
    ['12', '1'] => '121',
    ['121', '3'] => '1213',
}

def opens(digits)
    state = START
    digits.each_char do |digit|
        # Fehlt ein Übergang, geht es zurück zum Start.
        state = TRANSITIONS.fetch([state, digit], START)
    end
    ACCEPTING.include?(state)
end

['1213', '991213', '12', '121213'].each do |digits|
    puts "#{digits} #{opens(digits)}"
end
