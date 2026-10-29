# Ein Textadventure aus Objekten: Nachts in der Schule.

class Room
  attr_reader :name, :description, :items

  def initialize(name, description)
    @name = name
    @description = description
    @exits = {}
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

  def describe
    lines = [@name, @description]
    lines << "Hier liegt: #{@items.map(&:name).join(', ')}" unless @items.empty?
    exits = directions
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
end
