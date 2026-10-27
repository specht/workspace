# Eine Turingmaschine prüft a^n b^n c^n: erst n-mal a, dann n-mal b, dann
# n-mal c. In jeder Runde streicht sie ein a, ein b und ein c durch (X, Y,
# Z). Steht kein a mehr da, prüft sie, dass auch nichts anderes übrig ist.
START = 'q0'
ACCEPT = 'qJ'
TRANSITIONS = {
    # q0: das nächste a durchstreichen; keins mehr: zur Prüfung
    ['q0', 'a'] => ['X', 'R', 'q1'],
    ['q0', 'Y'] => ['Y', 'R', 'q4'],
    ['q0', '_'] => ['_', 'R', ACCEPT],
    # q1: nach rechts zum ersten b, durchstreichen
    ['q1', 'a'] => ['a', 'R', 'q1'],
    ['q1', 'Y'] => ['Y', 'R', 'q1'],
    ['q1', 'b'] => ['Y', 'R', 'q2'],
    # q2: weiter zum ersten c, durchstreichen
    ['q2', 'b'] => ['b', 'R', 'q2'],
    ['q2', 'Z'] => ['Z', 'R', 'q2'],
    ['q2', 'c'] => ['Z', 'L', 'q3'],
    # q3: zurück bis hinter das letzte X
    ['q3', 'a'] => ['a', 'L', 'q3'],
    ['q3', 'b'] => ['b', 'L', 'q3'],
    ['q3', 'Y'] => ['Y', 'L', 'q3'],
    ['q3', 'Z'] => ['Z', 'L', 'q3'],
    ['q3', 'X'] => ['X', 'R', 'q0'],
    # q4: hinter den a nur noch Y und Z
    ['q4', 'Y'] => ['Y', 'R', 'q4'],
    ['q4', 'Z'] => ['Z', 'R', 'q4'],
    ['q4', '_'] => ['_', 'L', ACCEPT],
}

# Das Band ab dem ersten bis zum letzten beschriebenen Feld, der Kopf in [ ]
def show(tape, head)
    used = tape.keys.reject { |i| tape[i] == '_' } + [head]
    (used.min..used.max).map { |i| i == head ? "[#{tape[i]}]" : tape[i] }.join
end

# Nimmt die Maschine das Wort an? Ohne passenden Übergang hält sie und
# lehnt ab. steps zählt die Schritte bis dahin.
def accepts(word, trace: false)
    tape = Hash.new('_')
    word.each_char.with_index { |symbol, i| tape[i] = symbol }
    head = 0
    state = START
    steps = 0
    puts "#{state} #{show(tape, head)}" if trace
    while state != ACCEPT
        move = TRANSITIONS[[state, tape[head]]]
        break if move.nil?
        write, direction, state = move
        tape[head] = write
        head += direction == 'R' ? 1 : -1
        steps += 1
        puts "#{state} #{show(tape, head)}" if trace
    end
    puts "#{steps} Schritte" if trace
    state == ACCEPT
end

accepts('aabbcc', trace: true)
['abc', 'aabbcc', 'aabbc', 'abcabc', ''].each do |word|
    puts "#{word.inspect} #{accepts(word)}"
end
