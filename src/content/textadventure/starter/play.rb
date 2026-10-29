# Die Spielschleife: liest Befehle und gibt sie an das Spiel weiter.
#   ruby play.rb          spielt deine Welt (build_world in adventure.rb)
#   ruby play.rb schule   spielt die Nacht in der Schule (schule.rb)
require_relative 'adventure'

if ARGV[0] == 'schule'
  require_relative 'schule'
  game = Game.new(build_school)
else
  game = Game.new(build_world)
end

puts game.start
until game.over?
  print "> "
  line = $stdin.gets
  break if line.nil?
  puts game.handle(line)
end
