# Ein Zahlenschloss mit dem Code 1213. Es öffnet, sobald die letzten vier
# getippten Ziffern der Code sind. Der Zustand merkt sich, wie viel vom Code
# schon stimmt: In q2 stimmen die ersten zwei Ziffern, 12.
START = 'q0'
ACCEPTING = ['q4']
TRANSITIONS = {
    ['q0', '1'] => 'q1',
    ['q1', '2'] => 'q2',
    ['q2', '1'] => 'q3',
    ['q3', '3'] => 'q4',
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
