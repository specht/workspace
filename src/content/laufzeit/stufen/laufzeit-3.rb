# Wie lange dauert das? Dieselbe Frage auf drei Arten, schnelle Hilfen und
# eine Rundfahrt durch Berlin.
require 'set'

# Wie viele verschiedene Namen? Mit einer Liste dessen, was man schon kennt.
def distinct_list(names)
  seen = []
  names.each do |name|
    seen << name unless seen.include?(name)
  end
  seen.size
end

# Sortiert stehen gleiche Namen nebeneinander.
def distinct_sorted(names)
  sorted = names.sort
  count = 0
  sorted.each_with_index do |name, i|
    count += 1 if i == 0 || name != sorted[i - 1]
  end
  count
end

# Eine Menge findet ohne Suchen, ob sie einen Namen schon hat.
def distinct_set(names)
  seen = Set.new
  names.each { |name| seen << name }
  seen.size
end
