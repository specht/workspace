# Ein Textadventure aus Objekten: Nachts in der Schule.

class Room
  attr_reader :name, :description, :items

  def initialize(name, description)
    @name = name
    @description = description
    @exits = {}
    @locks = {}
    @items = []
  end

  def connect(direction, room)
    @exits[direction] = room
  end

  def exit_to(direction)
    @exits[direction]
  end

  def directions
    @exits.keys
  end

  def add_item(item)
    @items << item
  end

  def take(name)
    item = @items.find { |i| i.name == name }
    @items.delete(item) if item
    item
  end

  def lock(direction, key_name)
    @locks[direction] = key_name
  end

  def locked?(direction)
    @locks.key?(direction)
  end

  def unlock(key_name)
    doors = @locks.select { |_, key| key == key_name }.keys
    doors.each { |direction| @locks.delete(direction) }
    !doors.empty?
  end

  def dark?
    false
  end

  def goal?
    false
  end

  def describe
    lines = [@name, @description]
    lines << "Hier liegt: #{@items.map(&:name).join(', ')}" unless @items.empty?
    exits = directions.map { |d| locked?(d) ? "#{d} (verschlossen)" : d }
    lines << "Ausgänge: #{exits.empty? ? 'keine' : exits.join(', ')}"
    lines.join("\n")
  end
end

class DarkRoom < Room
  def dark?
    true
  end
end

class GoalRoom < Room
  def goal?
    true
  end
end

class Item
  attr_reader :name, :description

  def initialize(name, description)
    @name = name
    @description = description
  end

  def use(player)
    "Damit kannst du hier nichts anfangen."
  end

  def shines?
    false
  end
end

class Key < Item
  def use(player)
    if player.room.unlock(name)
      "Du schließt die Tür auf."
    else
      "Hier passt #{name} in kein Schloss."
    end
  end
end

class Food < Item
  def use(player)
    "Du isst #{name}. #{description}"
  end
end

class Lamp < Item
  def use(player)
    "#{name} leuchtet schon."
  end

  def shines?
    true
  end
end

class Chest < Item
  def initialize(name, description)
    super(name, description)
    @contents = []
  end

  def put(item)
    @contents << item
  end

  def use(player)
    return "#{name} ist leer." if @contents.empty?
    names = @contents.map(&:name).join(", ")
    @contents.each { |item| player.room.add_item(item) }
    @contents = []
    "Du öffnest #{name}. Darin: #{names}"
  end
end

class Player
  attr_reader :room

  def initialize(room)
    @room = room
    @bag = []
  end

  def look
    return "Es ist stockdunkel. Du siehst nichts." if @room.dark? && !has_light?
    @room.describe
  end

  def go(direction)
    target = @room.exit_to(direction)
    return "Da geht es nicht weiter." if target.nil?
    return "Die Tür nach #{direction} ist verschlossen." if @room.locked?(direction)
    @room = target
    look
  end

  def take(name)
    return "Im Dunkeln findest du nichts." if @room.dark? && !has_light?
    item = @room.take(name)
    return "Hier liegt nichts mit dem Namen #{name}." if item.nil?
    @bag << item
    "Du nimmst #{item.name}."
  end

  def has?(name)
    @bag.any? { |item| item.name == name }
  end

  def bag
    return "Deine Tasche ist leer." if @bag.empty?
    "In deiner Tasche: #{@bag.map(&:name).join(', ')}"
  end

  def remove(item)
    @bag.delete(item)
  end

  def use(name)
    item = @bag.find { |i| i.name == name }
    return "Du hast nichts mit dem Namen #{name}." if item.nil?
    item.use(self)
  end

  def has_light?
    @bag.any?(&:shines?)
  end

  def examine(name)
    item = @bag.find { |i| i.name == name } || @room.items.find { |i| i.name == name }
    return "Hier gibt es nichts mit dem Namen #{name}." if item.nil?
    item.description
  end
end

class Game
  def initialize(player)
    @player = player
    @over = false
  end

  def start
    @player.look
  end

  def over?
    @over
  end

  def won?
    @player.room.goal?
  end

  def handle(line)
    verb, word = line.strip.downcase.split(" ", 2)
    word = word.to_s
    text = case verb
           when "gehe" then @player.go(word)
           when "nimm" then @player.take(word)
           when "benutze" then @player.use(word)
           when "untersuche" then @player.examine(word)
           when "schau" then @player.look
           when "tasche" then @player.bag
           when "ende"
             @over = true
             "Bis bald."
           else
             "Das verstehe ich nicht. Befehle: gehe, nimm, benutze, untersuche, schau, tasche, ende"
           end
    @over = true if won?
    text
  end
end

# Alle Räume, die man von start aus erreicht, start zuerst (gegeben).
def all_rooms(start)
  seen = [start]
  queue = [start]
  until queue.empty?
    room = queue.shift
    room.directions.each do |direction|
      other = room.exit_to(direction)
      next if seen.include?(other)
      seen << other
      queue << other
    end
  end
  seen
end

# Deine Welt: ein Wohnheim, in dem das Licht ausgefallen ist.
def build_world
  room = Room.new("Zimmer 3", "Dein Zimmer im Wohnheim. Der Strom ist weg.")
  corridor = DarkRoom.new("Gang", "Ein langer Gang mit Türen links und rechts.")
  kitchen = Room.new("Küche", "Im Kühlschrank brummt nichts mehr.")
  fuse = GoalRoom.new("Sicherungskasten", "Du legst den Schalter um, und überall geht das Licht an. Geschafft!")
  room.connect("flur", corridor)
  corridor.connect("zimmer", room)
  corridor.connect("küche", kitchen)
  kitchen.connect("flur", corridor)
  corridor.connect("kasten", fuse)
  corridor.lock("kasten", "kastenschlüssel")
  room.add_item(Lamp.new("kerze", "Eine Kerze, die schon brennt."))
  kitchen.add_item(Key.new("kastenschlüssel", "Ein kleiner Schlüssel mit einem Blitz darauf."))
  kitchen.add_item(Food.new("keks", "Ein bisschen weich, aber gut."))
  Player.new(room)
end

# Der Weg zum Ziel, Befehl für Befehl: Game spielt ihn durch.
WALKTHROUGH = [
  "nimm kerze", "gehe flur", "gehe küche", "nimm kastenschlüssel", "gehe flur",
  "benutze kastenschlüssel", "gehe kasten",
].freeze
