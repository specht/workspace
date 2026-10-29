// Nachts in der Schule: eine Welt aus deinen Klassen. Spielen mit
// dart run schule.dart.
import 'dart:io';

import 'adventure.dart';

Player buildSchool() {
  final classroom = Room('Raum 204', 'Du bist über deinem Heft eingeschlafen. Draußen ist es dunkel, die Schule ist still.');
  final hall = Room('Flur', 'Nur die grünen Notausgangsschilder leuchten.');
  final staff = Room('Lehrerzimmer', 'Stapel von Heften, eine kalte Kaffeemaschine.');
  final stairs = Room('Treppenhaus', 'Deine Schritte hallen.');
  final entrance = Room('Eingangshalle', 'Hinter der großen Glastür liegt der Schulhof.');
  final caretaker = Room('Hausmeisterraum', 'Werkzeug an der Wand, ein Radio, das leise rauscht.');
  final cellar = DarkRoom('Keller', 'Heizungsrohre, alte Stühle und ein Schlüsselbrett.');
  final outside = GoalRoom('Schulhof', 'Kühle Nachtluft. Du bist draußen. Geschafft!');

  classroom.connect('westen', hall);
  hall.connect('osten', classroom);
  hall.connect('norden', staff);
  staff.connect('süden', hall);
  hall.connect('unten', stairs);
  stairs.connect('oben', hall);
  stairs.connect('unten', entrance);
  entrance.connect('oben', stairs);
  entrance.connect('westen', caretaker);
  caretaker.connect('osten', entrance);
  entrance.connect('unten', cellar);
  cellar.connect('oben', entrance);
  entrance.connect('süden', outside);

  hall.lock('norden', 'lehrerschlüssel');
  entrance.lock('süden', 'haustürschlüssel');

  classroom.addItem(Item('zettel', '»Der Hausmeister lässt seine Taschenlampe nie aus dem Raum.«'));
  classroom.addItem(Food('apfel', 'Knackig und süß.'));
  caretaker.addItem(Lamp('taschenlampe', 'Eine schwere Taschenlampe. Sie leuchtet.'));
  cellar.addItem(Key('lehrerschlüssel', 'Am Anhänger steht: Lehrerzimmer.'));
  staff.addItem(Key('haustürschlüssel', 'Ein großer Schlüssel mit einem roten Anhänger.'));

  return Player(classroom);
}

// Die Spielschleife: liest Befehle und gibt sie an das Spiel weiter.
void main() {
  final game = Game(buildSchool());
  print(game.start());
  while (!game.isOver) {
    stdout.write('> ');
    final line = stdin.readLineSync();
    if (line == null) break;
    print(game.handle(line));
  }
}
