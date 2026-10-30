// Farbe im Terminal (textadventure.md, »Farbe im Terminal«): Text in einer
// ANSI-Escape-Sequenz, und eine Antwort des Spiels, bemalt. Das Spiel selbst
// bleibt, wie es ist; nur die Spielschleife bemalt, was sie ausgibt.
const bold = 1;
const green = 32;
const blue = 34;

const lists = {'Hier liegt: ': green, 'In deiner Tasche: ': green, 'Ausgänge: ': blue};

String color(String text, int code) => '\x1B[${code}m$text\x1B[0m';

String paint(String answer) {
  final lines = answer.split('\n');
  final room = lines.any((line) => line.startsWith('Ausgänge: '));
  final painted = <String>[];
  for (var i = 0; i < lines.length; i++) {
    final line = lines[i];
    final label = lists.keys.where((start) => line.startsWith(start)).firstOrNull;
    if (i == 0 && room) {
      painted.add(color(line, bold));
    } else if (label != null && line != 'Ausgänge: keine') {
      final names = line.substring(label.length).split(', ');
      painted.add(label + names.map((name) => color(name, lists[label]!)).join(', '));
    } else {
      painted.add(line);
    }
  }
  return painted.join('\n');
}
