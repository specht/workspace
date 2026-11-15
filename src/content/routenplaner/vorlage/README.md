# Routenplaner

Die Vorlage zu »Ein Routenplaner für Berlin« im Hackschule Workspace: eine
Karte von Steglitz-Zehlendorf im Browser, und ein Programm, das den Weg findet.
Die Funktionen schreibst du in einer von fünf Sprachen; der Server zeigt im
Browser, was sie schon können, und spielt jede Suche auf der Karte ab.

```bash
./run                      # die Sprache, deren Ordner du zuletzt geändert hast
./run dart                 # ruby, python, dart, javascript oder java
AREA=mitte ./run           # ein anderer Bezirk, beim ersten Mal heruntergeladen
AREA=mitte,pankow ./run    # mehrere Bezirke zusammen
```

Beim ersten Start installiert `./run` Sinatra für den Server (Ruby). Änderst
du deine Funktionen, startet der Server sie von selbst neu, sobald du
speicherst; ein Fehler steht im Terminal und auf der Karte.

- `ruby/routes.rb`, `python/routes.py`, `dart/routes.dart`,
  `javascript/routes.js`, `java/Routes.java`: deine Funktionen, Schritt für
  Schritt; in Dart und Java dazu die Typen (`graph.dart`, `Types.java`)
- `bridge.*` in jedem Ordner: die Brücke, über die der Server deine
  Funktionen fragt; hier änderst du nichts
- `server.rb`: der Server, er zeigt die Karte und fragt deine Funktionen
- `public/`: die Seite mit der Karte
- `data/`: die Gebiete, ein Weg pro Zeile, und die Orte darin; `test.tsv` und
  `detour.tsv` sind kleine Testkarten

Kartendaten © OpenStreetMap-Mitwirkende, ODbL 1.0.
