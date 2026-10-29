// Misst, wie lange deine drei Funktionen für immer mehr Namen brauchen:
// die ersten 1000 Straßennamen, 2000, 4000 … aus strassen.txt. Eine Art
// fällt heraus, wenn sie länger als 10 Sekunden braucht. Eine Zahl dahinter
// lässt erst bei so vielen Namen anfangen.
//   dart run messen.dart
//   dart run messen.dart 8000
import 'dart:io';
import 'laufzeit.dart';

const limit = 10.0;

void main(List<String> args) {
  final names = File('strassen.txt').readAsLinesSync();
  final ways = <String, int Function(List<String>)>{
    'distinctList': distinctList,
    'distinctSorted': distinctSorted,
    'distinctSet': distinctSet,
  };
  print('${'n'.padLeft(8)}  ${'Art'.padRight(16)} ${'Sekunden'.padLeft(10)}  ${'Faktor'.padLeft(8)}');
  final active = ways.keys.toList();
  final before = <String, double>{};
  var n = args.isEmpty ? 1000 : int.parse(args[0]);
  while (n <= names.length && active.isNotEmpty) {
    final part = names.sublist(0, n);
    for (final way in [...active]) {
      final watch = Stopwatch()..start();
      ways[way]!(part);
      final time = watch.elapsedMicroseconds / 1e6;
      final last = before[way];
      final factor = last != null && last > 0.0005 ? (time / last).toStringAsFixed(1) : '';
      print('${'$n'.padLeft(8)}  ${way.padRight(16)} ${time.toStringAsFixed(4).padLeft(10)}  ${factor.padLeft(8)}');
      before[way] = time;
      if (time > limit) {
        active.remove(way);
      }
    }
    n *= 2;
  }
}
