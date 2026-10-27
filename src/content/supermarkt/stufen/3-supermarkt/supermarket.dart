class Customer {
  final int _items;
  final int _arrival;

  Customer(this._items, this._arrival);

  int get items => _items;
  int get arrival => _arrival;
}

class Checkout {
  int _freeAt = 0;

  int get freeAt => _freeAt;

  int serviceTime(Customer customer) => 60 + 3 * customer.items;

  int serve(Customer customer) {
    final start = customer.arrival > _freeAt ? customer.arrival : _freeAt;
    _freeAt = start + serviceTime(customer);
    return start - customer.arrival;
  }
}

class Supermarket {
  final List<Checkout> _checkouts;
  int _totalWait = 0;
  int _customers = 0;

  Supermarket(this._checkouts);

  int arrive(Customer customer) {
    Checkout? best;
    for (final checkout in _checkouts) {
      if (best == null || checkout.freeAt < best.freeAt) {
        best = checkout;
      }
    }
    final wait = best!.serve(customer);
    _totalWait += wait;
    _customers += 1;
    return wait;
  }

  double averageWait() => _totalWait / _customers;
}
