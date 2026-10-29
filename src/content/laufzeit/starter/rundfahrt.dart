// Eine Rundfahrt zu den ersten n Sehenswürdigkeiten aus berlin-places.tsv,
// mit deinen Funktionen aus laufzeit.dart: alle Reihenfolgen ausprobiert und
// immer zum nächsten Ort, mit der Zeit, die jede Art braucht.
//   dart run rundfahrt.dart 8
import 'dart:io';
import 'laufzeit.dart';

void main(List<String> args) {
  final n = args.isEmpty ? 8 : int.parse(args[0]);
  final sights = <Place>[];
  for (final line in File('berlin-places.tsv').readAsLinesSync()) {
    final parts = line.split('\t');
    if (parts.length < 3 || parts[0] != 'sight') {
      continue;
    }
    final at = parts[2].split(',');
    sights.add((name: parts[1], lon: double.parse(at[0]), lat: double.parse(at[1])));
  }
  final places = sights.take(n).toList();
  final ways = <String, List<int> Function(List<Place>)>{
    'alle Reihenfolgen': bestTour,
    'nächster Ort': nearestTour,
  };
  ways.forEach((label, way) {
    final watch = Stopwatch()..start();
    final tour = way(places);
    final seconds = watch.elapsedMicroseconds / 1e6;
    final km = tourLength(tour, places).toStringAsFixed(1).padLeft(7);
    print('${label.padRight(18)} $km km  ${seconds.toStringAsFixed(3).padLeft(9)} s');
    print('  ' + [...tour, tour.first].map((i) => places[i].name).join(' → '));
  });
}
