# Ein Drehkreuz wie am Eingang zum Schwimmbad. Für jeden Zustand und jede
# Eingabe steht hier, in welchen Zustand das Drehkreuz wechselt.
# q0: Das Drehkreuz ist zu. q1: Es ist offen.
TRANSITIONS = {
    ('q0', 'münze'): 'q1',
    ('q0', 'drücken'): 'q0',
    ('q1', 'münze'): 'q1',
    ('q1', 'drücken'): 'q0',
}


def run(inputs):
    state = 'q0'
    for symbol in inputs:
        new_state = TRANSITIONS[(state, symbol)]
        print(f'{state}  {symbol:<8} → {new_state}')
        state = new_state
    return state


run(['drücken', 'münze', 'drücken', 'drücken'])
