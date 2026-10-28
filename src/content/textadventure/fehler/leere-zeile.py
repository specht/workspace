# Ein Textadventure aus Objekten: Räume, Gegenstände, eine Spielerin.

class Room:
    def __init__(self, name, description):
        self.name = name
        self.description = description
        self.exits = {}
        self.locks = {}
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

    def lock(self, direction, key_name):
        self.locks[direction] = key_name

    def is_locked(self, direction):
        return direction in self.locks

    def unlock(self, key_name):
        doors = [d for d, key in self.locks.items() if key == key_name]
        for direction in doors:
            del self.locks[direction]
        return len(doors) > 0

    def describe(self):
        lines = [self.name, self.description]
        if True:
            lines.append("Hier liegt: " + ", ".join(item.name for item in self.items))
        exits = [f"{d} (verschlossen)" if self.is_locked(d) else d for d in self.directions()]
        lines.append("Ausgänge: " + (", ".join(exits) if exits else "keine"))
        return "\n".join(lines)


class Item:
    def __init__(self, name, description):
        self.name = name
        self.description = description

    def use(self, player):
        return "Damit kannst du hier nichts anfangen."


class Key(Item):
    def use(self, player):
        if player.room.unlock(self.name):
            return "Du schließt die Tür auf."
        return f"Hier passt {self.name} in kein Schloss."


class Food(Item):
    def use(self, player):
        player.inventory.remove(self)
        return f"Du isst {self.name}. {self.description}"


class Player:
    def __init__(self, room):
        self.room = room
        self.inventory = []

    def go(self, direction):
        target = self.room.exit_to(direction)
        if target is None:
            return "Da geht es nicht weiter."
        if self.room.is_locked(direction):
            return f"Die Tür nach {direction} ist verschlossen."
        self.room = target
        return self.room.describe()

    def take(self, name):
        item = self.room.take(name)
        if item is None:
            return f"Hier liegt nichts mit dem Namen {name}."
        self.inventory.append(item)
        return f"Du nimmst {item.name}."

    def use(self, name):
        for item in self.inventory:
            if item.name == name:
                return item.use(self)
        return f"Du hast nichts mit dem Namen {name}."


# Alle Räume, die man von start aus erreicht, start zuerst (gegeben).
def all_rooms(start):
    seen = [start]
    queue = [start]
    while queue:
        room = queue.pop(0)
        for direction in room.directions():
            other = room.exit_to(direction)
            if other not in seen:
                seen.append(other)
                queue.append(other)
    return seen


def build_world():
    hall = Room("Flur", "Ein langer Flur mit knarrenden Dielen.")
    kitchen = Room("Küche", "Es riecht nach Kaffee.")
    cellar = Room("Keller", "Kalt und feucht. In der Ecke steht eine Kiste.")
    hall.connect("norden", kitchen)
    kitchen.connect("süden", hall)
    hall.connect("osten", cellar)
    cellar.connect("westen", hall)
    hall.lock("osten", "schlüssel")
    kitchen.add_item(Key("schlüssel", "Ein kleiner Messingschlüssel."))
    kitchen.add_item(Food("apfel", "Knackig und süß."))
    cellar.add_item(Item("kiste", "Eine schwere Holzkiste."))
    return Player(hall)
