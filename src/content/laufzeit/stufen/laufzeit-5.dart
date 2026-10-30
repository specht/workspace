// Wie lange dauert das? Dieselbe Frage auf drei Arten, schnelle Hilfen und
// eine Rundfahrt durch Berlin.

// Wie viele verschiedene Namen? Mit einer Liste dessen, was man schon kennt.
int distinctList(List<String> names) {
  final seen = <String>[];
  for (final name in names) {
    if (!seen.contains(name)) {
      seen.add(name);
    }
  }
  return seen.length;
}

// Sortiert stehen gleiche Namen nebeneinander.
int distinctSorted(List<String> names) {
  final sorted = [...names]..sort();
  var count = 0;
  for (var i = 0; i < sorted.length; i++) {
    if (i == 0 || sorted[i] != sorted[i - 1]) {
      count++;
    }
  }
  return count;
}

// Eine Menge findet ohne Suchen, ob sie einen Namen schon hat.
int distinctSet(List<String> names) {
  final seen = <String>{};
  for (final name in names) {
    seen.add(name);
  }
  return seen.length;
}

// Fibonacci mit einem Gedächtnis: Jede Zahl wird nur einmal ausgerechnet.
int fibMemo(int n, [Map<int, int>? memo]) {
  memo ??= {};
  if (n < 2) {
    return n;
  }
  return memo[n] ??= fibMemo(n - 1, memo) + fibMemo(n - 2, memo);
}

// Gibt es zwei Zahlen an verschiedenen Stellen, die zusammen target ergeben?
bool pairWithSum(List<int> numbers, int target) {
  final seen = <int>{};
  for (final x in numbers) {
    if (seen.contains(target - x)) {
      return true;
    }
    seen.add(x);
  }
  return false;
}
