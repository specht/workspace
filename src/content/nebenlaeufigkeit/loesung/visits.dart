import 'dart:io';

class Lock {
  Future<void> _last = Future.value();

  Future<T> synchronized<T>(Future<T> Function() action) {
    final result = _last.then((_) => action());
    _last = result.then((_) {}, onError: (_) {});
    return result;
  }
}

Future<void> visit(String path) async {
  final file = File(path);
  final count = int.parse(await file.readAsString());
  await file.writeAsString('${count + 1}');
}

Future<int> visitAll(String path, int n) async {
  await File(path).writeAsString('0');
  final lock = Lock();
  await Future.wait([for (var i = 0; i < n; i++) lock.synchronized(() => visit(path))]);
  return int.parse(await File(path).readAsString());
}
