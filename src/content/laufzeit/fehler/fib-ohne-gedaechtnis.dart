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
  return fibMemo(n - 1) + fibMemo(n - 2);
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

// Alle Reihenfolgen einer Liste, wie in den Katas zur Rekursion.
Iterable<List<int>> permutations(List<int> items) sync* {
  if (items.isEmpty) {
    yield [];
    return;
  }
  for (var i = 0; i < items.length; i++) {
    final rest = [...items.sublist(0, i), ...items.sublist(i + 1)];
    for (final order in permutations(rest)) {
      yield [items[i], ...order];
    }
  }
}

// Alle Reihenfolgen ausprobieren; der erste Ort bleibt vorn.
List<int> bestTour(List<Place> places) {
  final rest = [for (var i = 1; i < places.length; i++) i];
  var best = <int>[];
  var shortest = double.infinity;
  for (final order in permutations(rest)) {
    final tour = [0, ...order];
    final length = tourLength(tour, places);
    if (length < shortest) {
      best = tour;
      shortest = length;
    }
  }
  return best;
}

// Immer zum nächsten Ort, an dem man noch nicht war.
List<int> nearestTour(List<Place> places) {
  final tour = [0];
  final left = [for (var i = 1; i < places.length; i++) i];
  while (left.isNotEmpty) {
    final here = places[tour.last];
    var nearest = left.first;
    for (final i in left) {
      if (distance(here, places[i]) < distance(here, places[nearest])) {
        nearest = i;
      }
    }
    tour.add(nearest);
    left.remove(nearest);
  }
  return tour;
}
