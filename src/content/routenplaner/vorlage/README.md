# Routenplaner

Die Vorlage zu »Ein Routenplaner für Berlin« im Hackschule Workspace: eine
Karte von Steglitz-Zehlendorf im Browser, und ein Programm, das den Weg findet.

In Ruby:

```bash
gem install sinatra rackup puma
ruby server.rb                       # Steglitz-Zehlendorf
AREA=mitte ruby server.rb            # ein anderer Bezirk, beim ersten Mal heruntergeladen
AREA=mitte,pankow ruby server.rb     # mehrere Bezirke zusammen
```

In Python:

```bash
pip install flask
python3 server.py                    # und genauso mit AREA=…
```

- `routes.rb` oder `routes.py`: deine Funktionen, Schritt für Schritt
- `server.rb` oder `server.py`: der Server, er zeigt die Karte und fragt deine Funktionen
- `public/`: die Seite mit der Karte
- `data/`: die Gebiete, ein Weg pro Zeile, und die Orte darin; `test.tsv` und
  `detour.tsv` sind kleine Testkarten

Kartendaten © OpenStreetMap-Mitwirkende, ODbL 1.0.
