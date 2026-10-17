# Das Zahlenschloss als nichtdeterministischer endlicher Automat (NEA).
# Ein Übergang führt hier zu einer Menge von Zuständen: Der NEA darf raten.
# In q2 stimmen die ersten zwei Ziffern des Codes, wie beim Schloss vorher.
DIGITS = '0123456789'
START = 'q0'
ACCEPTING = Set['q4', 'q8']
TRANSITIONS = {}

# In q0 wartet der NEA: Jede Ziffer führt wieder nach q0 …
DIGITS.each_char do |digit|
    TRANSITIONS[['q0', digit]] = Set['q0']
end
# … aber bei einer 1 darf er auch raten, dass hier der Code beginnt.
TRANSITIONS[['q0', '1']] = Set['q0', 'q1']
TRANSITIONS[['q1', '2']] = Set['q2']
TRANSITIONS[['q2', '1']] = Set['q3']
TRANSITIONS[['q3', '3']] = Set['q4']
# Lösung: Der Generalschlüssel 4711 ist ein zweiter Weg, geraten ab einer 4.
TRANSITIONS[['q0', '4']] = Set['q0', 'q5']
TRANSITIONS[['q5', '7']] = Set['q6']
TRANSITIONS[['q6', '1']] = Set['q7']
TRANSITIONS[['q7', '1']] = Set['q8']

def opens(digits)
    states = Set[START]
    digits.each_char do |digit|
        new_states = Set.new
        states.each do |state|
            new_states |= TRANSITIONS.fetch([state, digit], Set.new)
        end
        states = new_states
        puts "#{digit} #{states.sort.inspect}"
    end
    !(states & ACCEPTING).empty?
end

if __FILE__ == $0
    puts opens('121213')
end
