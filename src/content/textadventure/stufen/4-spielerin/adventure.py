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

    def describe(self):
        lines = [self.name, self.description]
        if self.items:
            lines.append("Hier liegt: " + ", ".join(item.name for item in self.items))
        exits = self.directions()
        lines.append("Ausgänge: " + (", ".join(exits) if exits else "keine"))
        return "\n".join(lines)


class Item:
    def __init__(self, name, description):
        self.name = name
        self.description = description


class Player:
    def __init__(self, room):
        self.room = room
        self.inventory = []

    def go(self, direction):
        target = self.room.exit_to(direction)
        if target is None:
            return "Da geht es nicht weiter."
        self.room = target
        return self.room.describe()

    def take(self, name):
        item = self.room.take(name)
        if item is None:
            return f"Hier liegt nichts mit dem Namen {name}."
        self.inventory.append(item)
        return f"Du nimmst {item.name}."
