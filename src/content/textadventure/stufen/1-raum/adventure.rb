# Ein Textadventure aus Objekten: Räume, Gegenstände, eine Spielerin.

class Room
  attr_reader :name, :description

  def initialize(name, description)
    @name = name
    @description = description
    @exits = {}
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
end
