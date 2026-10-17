Noch in Arbeit: Die Schritte 1 bis 5 stehen, bis Dijkstra. Es fehlen die Prioritätswarteschlange, ganz Berlin mit A*, die Arten zu reisen und die Stadtrundfahrt.

**Ablauf:** Klonen, Sinatra installieren (`gem install sinatra rackup puma`, einmal pro Workspace), den Server starten und die Karte im Browser über den Reiter »Ports« öffnen. Dann liest das Programm die Wege, rechnet Abstände aus Koordinaten und baut den Graphen; »Graph zeigen« macht ihn auf der Karte sichtbar. Breitensuche und Dijkstra schreiben die Lernenden selbst, die Seite zeigt bei jeder Suche, in welcher Reihenfolge sie die Knoten besucht hat. Daran sieht man den Unterschied: Die Breitensuche breitet sich nach Kanten aus, Dijkstra in Metern, wie Ringe um den Start.

**Zum Vergleichen:** Vom Fichtenberg-Gymnasium zum Titania-Palast findet die Breitensuche 2,0 km über 92 Knoten, Dijkstra 1,9 km über 128; beide besuchen gut 17 000 der 38 478 Knoten, Dijkstra mit `min_by` braucht dafür etwa eine halbe Sekunde, die Breitensuche ein paar Hundertstel. Die Testkarte `data/detour.tsv` ist so gebaut, dass beide verschiedene Wege finden.

**Stolpersteine:** Nach jeder Änderung an `routes.rb` muss der Server neu starten (<kbd>Strg</kbd><kbd>C</kbd>, dann `ruby server.rb`). Ein Fehler in einer Suche steht im Terminal, die Seite sagt nur, dass etwas schiefging. `Math.cos` rechnet im Bogenmaß; wer Grad hineingibt, bekommt nach Osten falsche Abstände, und die Prüfung sagt das.

**Datenschutz:** Start und Ziel sind öffentliche Orte, Sehenswürdigkeiten und Schulen; die Karte hat kein Feld für Adressen. Bitte keine Aufgabe »dein Weg nach Hause« stellen.

**Daten:** aus OpenStreetMap (© OpenStreetMap-Mitwirkende, ODbL 1.0), gebaut von `src/scripts/collect-streets/`. Andere Gebiete schneidet das Skript per Rechteck aus.
