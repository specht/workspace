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
