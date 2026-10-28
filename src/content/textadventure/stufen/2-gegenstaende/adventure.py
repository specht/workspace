# Ein Textadventure aus Objekten: Räume, Gegenstände, eine Spielerin.

class Room:
    def __init__(self, name, description):
        self.name = name
        self.description = description
        self.exits = {}
        self.items = []

    def connect(self, direction, room):
        self.exits[direction] = room

    def exit_to(self, direction):
        return self.exits.get(direction)

    def directions(self):
        return list(self.exits.keys())

    def add_item(self, item):
        self.items.append(item)

    def take(self, name):
        for item in self.items:
            if item.name == name:
                self.items.remove(item)
                return item
        return None


class Item:
    def __init__(self, name, description):
        self.name = name
        self.description = description
