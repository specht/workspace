# Farbe im Terminal (textadventure.md, »Farbe im Terminal«): Text in einer
# ANSI-Escape-Sequenz, und eine Antwort des Spiels, bemalt. Das Spiel selbst
# bleibt, wie es ist; nur die Spielschleife bemalt, was sie ausgibt.
BOLD = 1
GREEN = 32
BLUE = 34

LISTS = { 'Hier liegt: ' => GREEN, 'In deiner Tasche: ' => GREEN, 'Ausgänge: ' => BLUE }

def color(text, code)
  "\e[#{code}m#{text}\e[0m"
end

def paint(answer)
  lines = answer.split("\n")
  room = lines.any? { |line| line.start_with?('Ausgänge: ') }
  lines.each_with_index.map do |line, i|
    label, code = LISTS.find { |start, _| line.start_with?(start) }
    if i == 0 && room
      color(line, BOLD)
    elsif label && line != 'Ausgänge: keine'
      label + line.delete_prefix(label).split(', ').map { |name| color(name, code) }.join(', ')
    else
      line
    end
  end.join("\n")
end
