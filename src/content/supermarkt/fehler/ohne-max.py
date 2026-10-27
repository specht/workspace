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


class Checkout:
    def __init__(self):
        self._free_at = 0

    @property
    def free_at(self):
        return self._free_at

    def service_time(self, customer):
        return 60 + 3 * customer.items

    def serve(self, customer):
        start = self._free_at
        self._free_at = start + self.service_time(customer)
        return start - customer.arrival
