# Eine Turingmaschine addiert zwei Zahlen in Strichen: 111+11 heißt 3 + 2,
# und am Ende steht 11111 auf dem Band.
START = 'q0'
HALT = 'qH'
# [Zustand, gelesenes Zeichen] => [was sie schreibt, wohin der Kopf geht
# (L links, R rechts), neuer Zustand]
TRANSITIONS = {
    # q0: über die erste Zahl; das + wird zu einem Strich
    ['q0', '1'] => ['1', 'R', 'q0'],
    ['q0', '+'] => ['1', 'R', 'q1'],
    # q1: über die zweite Zahl bis ans Ende
    ['q1', '1'] => ['1', 'R', 'q1'],
    ['q1', '_'] => ['_', 'L', 'q2'],
    # q2: ein Strich zu viel, der letzte geht weg
    ['q2', '1'] => ['_', 'L', HALT],
}

# Das Band ab dem ersten bis zum letzten beschriebenen Feld, der Kopf in [ ]
def show(tape, head)
    used = tape.keys.reject { |i| tape[i] == '_' } + [head]
    (used.min..used.max).map { |i| i == head ? "[#{tape[i]}]" : tape[i] }.join
end

def run(input, trace: false, max_steps: 1000)
    tape = Hash.new('_')
    input.each_char.with_index { |symbol, i| tape[i] = symbol }
    head = 0
    state = START
    steps = 0
    puts "#{state.ljust(3)} #{show(tape, head)}" if trace
    until state == HALT
        raise "Nach #{max_steps} Schritten hat die Maschine nicht angehalten." if steps == max_steps
        write, move, state = TRANSITIONS.fetch([state, tape[head]])
        tape[head] = write
        head += move == 'R' ? 1 : -1
        steps += 1
        puts "#{state.ljust(3)} #{show(tape, head)}" if trace
    end
    used = tape.keys.reject { |i| tape[i] == '_' }
    used.empty? ? '' : (used.min..used.max).map { |i| tape[i] }.join
end

run('111+11', trace: true)
['1+1', '111+11', '+11', '11+'].each do |word|
    puts "#{word} → #{run(word)}"
end
