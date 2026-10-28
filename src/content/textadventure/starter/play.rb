# Die Spielschleife: liest Befehle und gibt sie an die Spielerin weiter.
# Die Welt baut build_world in adventure.rb.
require_relative 'adventure'

player = build_world
puts player.room.describe

loop do
  print "> "
  line = gets
  break if line.nil?
  verb, word = line.strip.downcase.split(" ", 2)
  case verb
  when "gehe"
    puts player.go(word.to_s)
  when "nimm"
    puts player.take(word.to_s)
  when "benutze"
    puts player.use(word.to_s)
  when "schau"
    puts player.room.describe
  when "tasche"
    names = player.inventory.map(&:name)
    puts names.empty? ? "Deine Tasche ist leer." : "In deiner Tasche: #{names.join(', ')}"
  when "ende"
    break
  when nil
    next
  else
    puts "Das verstehe ich nicht. Befehle: gehe, nimm, benutze, schau, tasche, ende"
  end
end
