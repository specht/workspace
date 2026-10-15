# Ein Drehkreuz wie am Eingang zum Schwimmbad. Für jeden Zustand und jede
# Eingabe steht hier, in welchen Zustand das Drehkreuz wechselt.
TRANSITIONS = {
    ('zu', 'münze'): 'offen',
    ('zu', 'drücken'): 'zu',
    ('offen', 'münze'): 'offen',
    ('offen', 'drücken'): 'zu',
}


def run(inputs):
    state = 'zu'
    for symbol in inputs:
        new_state = TRANSITIONS[(state, symbol)]
        print(f'{state:>5}  {symbol:<8} → {new_state}')
        state = new_state
    return state


run(['drücken', 'münze', 'drücken', 'drücken'])
