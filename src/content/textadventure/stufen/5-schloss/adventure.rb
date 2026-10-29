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

  def describe
    lines = [@name, @description]
    lines << "Hier liegt: #{@items.map(&:name).join(', ')}" unless @items.empty?
    exits = directions.map { |d| locked?(d) ? "#{d} (verschlossen)" : d }
    lines << "Ausgänge: #{exits.empty? ? 'keine' : exits.join(', ')}"
    lines.join("\n")
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
    player.remove(self)
    "Du isst #{name}. #{description}"
  end
end

class Player
  attr_reader :room

  def initialize(room)
    @room = room
    @bag = []
  end

  def look
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
end
