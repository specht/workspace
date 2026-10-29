# Ein Textadventure aus Objekten: Nachts in der Schule.


class Room:
    def __init__(self, name, description):
        self.name = name
        self.description = description
        self._exits = {}

    def connect(self, direction, room):
        self._exits[direction] = room

    def exit_to(self, direction):
        return self._exits.get(direction)

    def directions(self):
        return list(self._exits.keys())
