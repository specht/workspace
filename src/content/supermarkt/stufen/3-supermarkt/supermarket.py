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
        start = max(customer.arrival, self._free_at)
        self._free_at = start + self.service_time(customer)
        return start - customer.arrival


class Supermarket:
    def __init__(self, checkouts):
        self._checkouts = checkouts
        self._total_wait = 0
        self._customers = 0

    def arrive(self, customer):
        best = None
        for checkout in self._checkouts:
            if best is None or checkout.free_at < best.free_at:
                best = checkout
        wait = best.serve(customer)
        self._total_wait += wait
        self._customers += 1
        return wait

    def average_wait(self):
        return self._total_wait / self._customers
