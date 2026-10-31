var balance = 0;

Future<void> save() => Future.delayed(const Duration(milliseconds: 10));

Future<void> deposit(int amount) async {
  final before = balance;
  await save();
  balance = before + amount;
}

class Lock {
  Future<void> _last = Future.value();

  Future<T> synchronized<T>(Future<T> Function() action) {
    final result = _last.then((_) => action());
    _last = result.then((_) {}, onError: (_) {});
    return result;
  }
}

Future<int> depositAllAtOnce(List<int> amounts) async {
  balance = 0;
  final lock = Lock();
  await Future.wait([for (final amount in amounts) lock.synchronized(() => deposit(amount))]);
  return balance;
}
