var balance = 0;

Future<void> save() => Future.delayed(const Duration(milliseconds: 10));

Future<void> deposit(int amount) async {
  final before = balance;
  await save();
  balance = before + amount;
}

Future<int> depositAll(List<int> amounts) async {
  balance = 0;
  await Future.wait([for (final amount in amounts) deposit(amount)]);
  return balance;
}
