# Misst, wie lange deine drei Funktionen für immer mehr Namen brauchen:
# die ersten 1000 Straßennamen, 2000, 4000 … aus strassen.txt. Eine Art
# fällt heraus, wenn sie länger als 10 Sekunden braucht. Eine Zahl dahinter
# lässt erst bei so vielen Namen anfangen.
#   ruby messen.rb
#   ruby messen.rb 8000
require_relative 'laufzeit'

names = File.readlines('strassen.txt', chomp: true)
WAYS = %i[distinct_list distinct_sorted distinct_set].freeze
LIMIT = 10.0

def seconds
  start = Process.clock_gettime(Process::CLOCK_MONOTONIC)
  yield
  Process.clock_gettime(Process::CLOCK_MONOTONIC) - start
end

puts format('%8s  %-16s %10s  %8s', 'n', 'Art', 'Sekunden', 'Faktor')
active = WAYS.dup
before = {}
n = (ARGV[0] || 1000).to_i
while n <= names.size && !active.empty?
  part = names.first(n)
  active.dup.each do |way|
    time = seconds { send(way, part) }
    factor = before[way] && before[way] > 0.0005 ? format('%.1f', time / before[way]) : ''
    puts format('%8d  %-16s %10.4f  %8s', n, way, time, factor)
    before[way] = time
    active.delete(way) if time > LIMIT
  end
  n *= 2
end
