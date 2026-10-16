# Das Zahlenschloss als nichtdeterministischer endlicher Automat (NEA).
# Ein Übergang führt hier zu einer Menge von Zuständen: Der NEA darf raten.
DIGITS = '0123456789'
START = 'start'
ACCEPTING = Set['1213', '4711']
TRANSITIONS = {}

# In start wartet der NEA: Jede Ziffer führt wieder nach start …
DIGITS.each_char do |digit|
    TRANSITIONS[['start', digit]] = Set['start']
end
# … aber bei einer 1 darf er auch raten, dass hier der Code beginnt.
TRANSITIONS[['start', '1']] = Set['start', '1']
TRANSITIONS[['1', '2']] = Set['12']
TRANSITIONS[['12', '1']] = Set['121']
TRANSITIONS[['121', '3']] = Set['1213']
# Lösung: Der Generalschlüssel 4711 ist ein zweiter Weg, geraten ab einer 4.
TRANSITIONS[['start', '4']] = Set['start', '4']
TRANSITIONS[['4', '7']] = Set['47']
TRANSITIONS[['47', '1']] = Set['471']
TRANSITIONS[['471', '1']] = Set['4711']

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
