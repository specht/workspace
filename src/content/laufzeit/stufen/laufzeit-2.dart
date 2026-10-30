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
