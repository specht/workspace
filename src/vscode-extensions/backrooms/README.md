# Backrooms-Räume

Farben für die Räume der Backrooms (`rooms/NAME.room` in der Vorlage
[specht/backrooms-starter](https://github.com/specht/backrooms-starter)):

- im Grundriss (`[map]`) Wände `#`, Boden `.`, der Start `@`, Türen (Ziffern)
  und Dinge (Buchstaben: Leuchten, Blöcke, Statuen), jedes in seiner Farbe;
  ein `#` ist dort eine Wand, kein Kommentar
- Abschnitte in eckigen Klammern, Einträge `name = wert` mit Farben, Zahlen
  und Wörtern
- Kommentare: eine Zeile, die mit `#` anfängt und Wörter enthält, und am
  Ende eines Eintrags alles ab einem `#` mit Leerzeichen davor und danach
  (`0.08, #5a4626` ist eine Höhe und eine Farbe)

Dieselben Regeln wie die Code-Blöcke ` ```room ` auf der Website
(`src/ruby/include/backrooms_room_lexer.rb`).
