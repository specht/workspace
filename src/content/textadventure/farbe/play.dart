// Die Spielschleife für deine Welt (buildWorld in adventure.dart): liest
// Befehle und gibt sie an das Spiel weiter.
//   dart run play.dart
import 'dart:io';

import 'adventure.dart';
import 'colors.dart';

void main() {
  final game = Game(buildWorld());
  print(paint(game.start()));
  while (!game.isOver) {
    stdout.write('> ');
    final line = stdin.readLineSync();
    if (line == null) break;
    print(paint(game.handle(line)));
  }
}
