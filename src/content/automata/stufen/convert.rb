# Die Potenzmengenkonstruktion: Sie baut aus einem NEA einen DEA. Jeder
# Zustand des DEA ist eine Menge von Zuständen des NEA, nämlich die, in denen
# der NEA gerade sein könnte.
require_relative 'lock_nfa'

# Alle Zustände, in die der NEA aus states mit symbol kommt.
def step(states, symbol)
    new_states = Set.new
    states.each do |state|
        new_states |= TRANSITIONS.fetch([state, symbol], Set.new)
    end
    new_states
end

def to_dfa
    start = Set[START]
    found = Set[start]
    todo = [start]
    transitions = {}
    until todo.empty?
        states = todo.pop
        DIGITS.each_char do |symbol|
            target = step(states, symbol)
            transitions[[states, symbol]] = target
            unless found.include?(target)
                found << target
                todo << target
            end
        end
    end
    [found, transitions]
end

found, transitions = to_dfa
puts "#{found.size} Zustände"
transitions.each do |(states, symbol), target|
    next if target == Set[START]

    puts "#{states.sort.inspect} #{symbol} → #{target.sort.inspect}"
end
