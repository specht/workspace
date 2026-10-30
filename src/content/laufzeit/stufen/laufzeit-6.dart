// Wie lange dauert das? Dieselbe Frage auf drei Arten, schnelle Hilfen und
// eine Rundfahrt durch Berlin.
import 'dart:math';

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

// Ein Ort: Name, Länge und Breite (gegeben).
typedef Place = ({String name, double lon, double lat});

// Wie weit es von a nach b ist, in Kilometern Luftlinie (gegeben).
double distance(Place a, Place b) {
  final dx = (b.lon - a.lon) * 111.32 * cos((a.lat + b.lat) / 2 * pi / 180);
  final dy = (b.lat - a.lat) * 110.57;
  return sqrt(dx * dx + dy * dy);
}

// Die Länge einer Rundfahrt: von Ort zu Ort in der Reihenfolge der Nummern
// und am Ende zurück zum ersten.
double tourLength(List<int> tour, List<Place> places) {
  var total = 0.0;
  for (var i = 0; i < tour.length; i++) {
    final from = tour[i];
    final to = tour[(i + 1) % tour.length];
    total += distance(places[from], places[to]);
  }
  return total;
}
