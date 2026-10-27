class Customer:
    def __init__(self, items, arrival):
        self._items = items
        self._arrival = arrival

    @property
    def items(self):
        return self._items

    @property
    def arrival(self):
        return self._arrival
