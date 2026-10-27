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
