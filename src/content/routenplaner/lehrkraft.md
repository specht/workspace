Noch in Arbeit: Die Schritte 1 bis 5 stehen, bis Dijkstra, in Steglitz-Zehlendorf. Es fehlen die Prioritätswarteschlange, ganz Berlin mit A*, die Arten zu reisen und die Stadtrundfahrt.

**Ablauf:** Klonen, Sinatra installieren (`gem install sinatra rackup puma`, einmal pro Workspace), den Server starten und die Karte im Browser über den Reiter »Ports« öffnen. Dann liest das Programm die Wege, rechnet Abstände aus Koordinaten und baut den Graphen; »Graph zeigen« macht ihn auf der Karte sichtbar. Breitensuche und Dijkstra schreiben die Lernenden selbst, die Seite zeigt bei jeder Suche, in welcher Reihenfolge sie die Knoten besucht hat. Daran sieht man den Unterschied: Die Breitensuche breitet sich nach Kanten aus, Dijkstra in Metern, wie Ringe um den Start.

**Zum Vergleichen:** Vom Fichtenberg-Gymnasium zum Titania-Palast findet die Breitensuche 2,0 km über 92 Knoten (25 565 besucht, ein paar Hundertstel Sekunden), Dijkstra 1,9 km über 128 Knoten (19 462 besucht, gut eine halbe Sekunde mit `min_by`). Zur Pfaueninsel, nur mit der Fähre zu erreichen: Breitensuche 22,9 km, Dijkstra 15,8 km; beide besuchen fast alle 141 106 Knoten des Bezirks, Dijkstra braucht dafür etwa 5 Sekunden. Die Testkarte `data/detour.tsv` ist so gebaut, dass beide verschiedene Wege finden.

**Gebiete:** Die Vorlage hat Steglitz-Zehlendorf (45 181 Wege). Jeden anderen der zwölf Bezirke lädt `server.rb` mit `AREA=mitte` (und so weiter) beim ersten Start aus `specht/workspace-files`; ganz Berlin (`AREA=berlin`, 464 000 Wege) braucht etwa 25 Sekunden zum Start und gut 1,5 GB Speicher und ist erst mit schnellen Suchen sinnvoll.

**Stolpersteine:** Nach jeder Änderung an `routes.rb` muss der Server neu starten (<kbd>Strg</kbd><kbd>C</kbd>, dann `ruby server.rb`). Ein Fehler in einer Suche steht im Terminal, die Seite sagt nur, dass etwas schiefging. `Math.cos` rechnet im Bogenmaß; wer Grad hineingibt, bekommt nach Osten falsche Abstände, und die Prüfung sagt das.

**Datenschutz:** Start und Ziel sind öffentliche Orte, Sehenswürdigkeiten, Plätze und Schulen; die Karte hat kein Feld für Adressen. Bitte keine Aufgabe »dein Weg nach Hause« stellen.

**Daten:** aus OpenStreetMap (© OpenStreetMap-Mitwirkende, ODbL 1.0), gebaut von `src/scripts/collect-streets/`. Die Bezirke schneidet das Skript an ihren Grenzen aus, andere Gebiete per Rechteck.
