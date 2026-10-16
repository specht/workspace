# Ein Drehkreuz wie am Eingang zum Schwimmbad. Für jeden Zustand und jede
# Eingabe steht hier, in welchen Zustand das Drehkreuz wechselt.
TRANSITIONS = {
    ['zu', 'münze'] => 'offen',
    ['zu', 'drücken'] => 'zu',
    ['offen', 'münze'] => 'offen',
    ['offen', 'drücken'] => 'zu',
}

def run(inputs)
    state = 'zu'
    inputs.each do |symbol|
        new_state = TRANSITIONS.fetch([state, symbol])
        puts "#{state.rjust(5)}  #{symbol.ljust(8)} → #{new_state}"
        state = new_state
    end
    state
end

run(['drücken', 'münze', 'drücken', 'drücken'])
