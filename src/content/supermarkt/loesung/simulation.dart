import 'dart:io';

import 'supermarket.dart';

double simulate(List<Checkout> checkouts) {
  final supermarket = Supermarket(checkouts);
  for (final line in File('customers.csv').readAsLinesSync().skip(1)) {
    final parts = line.split(',').map(int.parse).toList();
    supermarket.arrive(Customer(parts[1], parts[0]));
  }
  return supermarket.averageWait();
}

void main() {
  print(simulate([Checkout(), Checkout(), Checkout()]));
  print(simulate([Checkout(), Checkout(), ExpressCheckout()]));
  print(simulate([Checkout(), Checkout(), SelfCheckout(), SelfCheckout()]));
}
