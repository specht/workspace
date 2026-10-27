# Eine Turingmaschine zieht 1 von einer Binärzahl ab, die größer als 0 ist.
# Eine 0 vorne darf stehen bleiben: Aus 1000 wird 0111.
START = 'q0'
HALT = 'qH'
# [Zustand, gelesenes Zeichen] => [was sie schreibt, wohin der Kopf geht
# (L links, R rechts), neuer Zustand]
TRANSITIONS = {
    # q0: nach rechts bis hinter die letzte Ziffer
    ['q0', '0'] => ['0', 'R', 'q0'],
    ['q0', '1'] => ['1', 'R', 'q0'],
    ['q0', '_'] => ['_', 'L', 'q1'],
    # q1: von hinten 1 ab, mit Übertrag
    ['q1', '0'] => ['1', 'L', 'q1'],
    ['q1', '1'] => ['0', 'L', HALT],
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

run('1100', trace: true)
['1', '10', '1100', '1000'].each do |word|
    puts "#{word} → #{run(word)}"
end
