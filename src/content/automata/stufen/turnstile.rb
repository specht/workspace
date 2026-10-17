# Ein Drehkreuz wie am Eingang zum Schwimmbad. Für jeden Zustand und jede
# Eingabe steht hier, in welchen Zustand das Drehkreuz wechselt.
# q0: Das Drehkreuz ist zu. q1: Es ist offen.
TRANSITIONS = {
    ['q0', 'münze'] => 'q1',
    ['q0', 'drücken'] => 'q0',
    ['q1', 'münze'] => 'q1',
    ['q1', 'drücken'] => 'q0',
}

def run(inputs)
    state = 'q0'
    inputs.each do |symbol|
        new_state = TRANSITIONS.fetch([state, symbol])
        puts "#{state}  #{symbol.ljust(8)} → #{new_state}"
        state = new_state
    end
    state
end

run(['drücken', 'münze', 'drücken', 'drücken'])
