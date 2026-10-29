# Ein Textadventure aus Objekten: Nachts in der Schule.


class Room:
    def __init__(self, name, description):
        self.name = name
        self.description = description
        self._exits = {}
        self._locks = {}
        self.items = []

    def connect(self, direction, room):
        self._exits[direction] = room

    def exit_to(self, direction):
        return self._exits.get(direction)

    def directions(self):
        return list(self._exits.keys())

    def add_item(self, item):
        self.items.append(item)

    def take(self, name):
        for item in self.items:
            if item.name == name:
                self.items.remove(item)
                return item
        return None

    def lock(self, direction, key_name):
        self._locks[direction] = key_name

    def is_locked(self, direction):
        return direction in self._locks

    def unlock(self, key_name):
        doors = [d for d, key in self._locks.items() if key == key_name]
        for direction in doors:
            del self._locks[direction]
        return len(doors) > 0

    def is_dark(self):
        return False

    def is_goal(self):
        return False

    def describe(self):
        lines = [self.name, self.description]
        if self.items:
            lines.append("Hier liegt: " + ", ".join(item.name for item in self.items))
        exits = [d + " (verschlossen)" if self.is_locked(d) else d for d in self.directions()]
        lines.append("Ausgänge: " + (", ".join(exits) if exits else "keine"))
        return "\n".join(lines)


class DarkRoom(Room):
    def is_dark(self):
        return True


class GoalRoom(Room):
    def is_goal(self):
        return True


class Item:
    def __init__(self, name, description):
        self.name = name
        self.description = description

    def use(self, player):
        return "Damit kannst du hier nichts anfangen."

    def shines(self):
        return False


class Key(Item):
    def use(self, player):
        if player.room.unlock(self.name):
            return "Du schließt die Tür auf."
        return f"Hier passt {self.name} in kein Schloss."


class Food(Item):
    def use(self, player):
        return f"Du isst {self.name}. {self.description}"


class Lamp(Item):
    def use(self, player):
        return f"{self.name} leuchtet schon."

    def shines(self):
        return True


class Chest(Item):
    def __init__(self, name, description):
        super().__init__(name, description)
        self._contents = []

    def put(self, item):
        self._contents.append(item)

    def use(self, player):
        if not self._contents:
            return f"{self.name} ist leer."
        names = ", ".join(item.name for item in self._contents)
        for item in self._contents:
            player.room.add_item(item)
        self._contents = []
        return f"Du öffnest {self.name}. Darin: {names}"


class Player:
    def __init__(self, room):
        self.room = room
        self._bag = []

    def look(self):
        if self.room.is_dark() and not self.has_light():
            return "Es ist stockdunkel. Du siehst nichts."
        return self.room.describe()

    def go(self, direction):
        target = self.room.exit_to(direction)
        if target is None:
            return "Da geht es nicht weiter."
        if self.room.is_locked(direction):
            return f"Die Tür nach {direction} ist verschlossen."
        self.room = target
        return self.look()

    def take(self, name):
        if self.room.is_dark() and not self.has_light():
            return "Im Dunkeln findest du nichts."
        item = self.room.take(name)
        if item is None:
            return f"Hier liegt nichts mit dem Namen {name}."
        self._bag.append(item)
        return f"Du nimmst {item.name}."

    def has(self, name):
        return any(item.name == name for item in self._bag)

    def bag(self):
        if not self._bag:
            return "Deine Tasche ist leer."
        return "In deiner Tasche: " + ", ".join(item.name for item in self._bag)

    def remove(self, item):
        self._bag.remove(item)

    def use(self, name):
        for item in self._bag:
            if item.name == name:
                return item.use(self)
        return f"Du hast nichts mit dem Namen {name}."

    def has_light(self):
        return any(item.shines() for item in self._bag)

    def examine(self, name):
        for item in self._bag + self.room.items:
            if item.name == name:
                return item.description
        return f"Hier gibt es nichts mit dem Namen {name}."


class Game:
    def __init__(self, player):
        self._player = player
        self._over = False

    def start(self):
        return self._player.look()

    def is_over(self):
        return self._over

    def is_won(self):
        return self._player.room.is_goal()

    def handle(self, line):
        parts = line.strip().lower().split(" ", 1)
        verb = parts[0]
        word = parts[1] if len(parts) > 1 else ""
        if verb == "gehe":
            text = self._player.go(word)
        elif verb == "nimm":
            text = self._player.take(word)
        elif verb == "benutze":
            text = self._player.use(word)
        elif verb == "untersuche":
            text = self._player.examine(word)
        elif verb == "schau":
            text = self._player.look()
        elif verb == "tasche":
            text = self._player.bag()
        elif verb == "ende":
            self._over = True
            text = "Bis bald."
        else:
            text = "Das verstehe ich nicht. Befehle: gehe, nimm, benutze, untersuche, schau, tasche, ende"
        if self.is_won():
            self._over = True
        return text


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


# Deine Welt: ein Wohnheim, in dem das Licht ausgefallen ist.
def build_world():
    room = Room("Zimmer 3", "Dein Zimmer im Wohnheim. Der Strom ist weg.")
    corridor = DarkRoom("Gang", "Ein langer Gang mit Türen links und rechts.")
    kitchen = Room("Küche", "Im Kühlschrank brummt nichts mehr.")
    fuse = GoalRoom("Sicherungskasten", "Du legst den Schalter um, und überall geht das Licht an. Geschafft!")
    room.connect("flur", corridor)
    corridor.connect("zimmer", room)
    corridor.connect("küche", kitchen)
    kitchen.connect("flur", corridor)
    corridor.connect("kasten", fuse)
    corridor.lock("kasten", "kastenschlüssel")
    room.add_item(Lamp("kerze", "Eine Kerze, die schon brennt."))
    kitchen.add_item(Key("kastenschlüssel", "Ein kleiner Schlüssel mit einem Blitz darauf."))
    kitchen.add_item(Food("keks", "Ein bisschen weich, aber gut."))
    return Player(room)


# Der Weg zum Ziel, Befehl für Befehl: Game spielt ihn durch.
WALKTHROUGH = [
    "nimm kerze", "gehe flur", "gehe küche", "nimm kastenschlüssel", "gehe flur",
    "benutze kastenschlüssel", "gehe kasten",
]
