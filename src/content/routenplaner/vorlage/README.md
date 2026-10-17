# Routenplaner

Die Vorlage zu »Ein Routenplaner für Berlin« im Hackschule Workspace: eine
Karte von Steglitz im Browser, und ein Programm, das den Weg findet.

```bash
gem install sinatra rackup puma
ruby server.rb
```

- `routes.rb`: deine Funktionen, Schritt für Schritt
- `server.rb`: der Server, er zeigt die Karte und fragt deine Funktionen
- `public/`: die Seite mit der Karte
- `data/`: die Gebiete, ein Weg pro Zeile, und die Orte darin

Kartendaten © OpenStreetMap-Mitwirkende, ODbL 1.0.
