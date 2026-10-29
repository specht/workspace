# Eine Rundfahrt zu den ersten n Sehenswürdigkeiten aus berlin-places.tsv,
# mit deinen Funktionen aus laufzeit.rb: alle Reihenfolgen ausprobiert und
# immer zum nächsten Ort, mit der Zeit, die jede Art braucht.
#   ruby rundfahrt.rb 8
require_relative 'laufzeit'

n = (ARGV[0] || 8).to_i
sights = File.readlines('berlin-places.tsv', chomp: true).filter_map do |line|
  kind, name, at = line.split("\t")
  next unless kind == 'sight'
  lon, lat = at.split(',').map(&:to_f)
  [name, lon, lat]
end
places = sights.first(n)

[[:best_tour, 'alle Reihenfolgen'], [:nearest_tour, 'nächster Ort']].each do |way, label|
  start = Process.clock_gettime(Process::CLOCK_MONOTONIC)
  tour = send(way, places)
  seconds = Process.clock_gettime(Process::CLOCK_MONOTONIC) - start
  puts format('%-18s %7.1f km  %9.3f s', label, tour_length(tour, places), seconds)
  puts '  ' + (tour + [tour.first]).map { |i| places[i][0] }.join(' → ')
end
