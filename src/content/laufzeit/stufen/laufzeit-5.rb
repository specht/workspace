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

# Fibonacci mit einem Gedächtnis: Jede Zahl wird nur einmal ausgerechnet.
def fib_memo(n, memo = {})
  return n if n < 2
  memo[n] ||= fib_memo(n - 1, memo) + fib_memo(n - 2, memo)
end

# Gibt es zwei Zahlen an verschiedenen Stellen, die zusammen target ergeben?
def pair_with_sum?(numbers, target)
  seen = Set.new
  numbers.each do |x|
    return true if seen.include?(target - x)
    seen << x
  end
  false
end
