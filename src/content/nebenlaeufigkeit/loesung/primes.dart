import 'dart:isolate';

int countPrimes(int from, int to) {
  var count = 0;
  for (var n = from; n < to; n++) {
    if (n < 2) continue;
    var prime = true;
    for (var d = 2; d * d <= n; d++) {
      if (n % d == 0) {
        prime = false;
        break;
      }
    }
    if (prime) count++;
  }
  return count;
}

Future<int> countPrimesParallel(int limit, int parts) async {
  final size = (limit + parts - 1) ~/ parts;
  final counts = await Future.wait([
    for (var p = 0; p < parts; p++)
      Isolate.run(() => countPrimes(p * size, (p + 1) * size < limit ? (p + 1) * size : limit)),
  ]);
  return counts.fold<int>(0, (sum, c) => sum + c);
}
