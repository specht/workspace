# Ein Textadventure aus Objekten: Räume, Gegenstände, eine Spielerin.

class Room:
    def __init__(self, name, description):
        self.name = name
        self.description = description
        self.exits = {}

    def connect(self, direction, room):
        self.exits[direction] = room

    def exit_to(self, direction):
        return self.exits.get(direction)

    def directions(self):
        return list(self.exits.keys())
