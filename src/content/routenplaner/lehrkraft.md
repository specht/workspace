Noch in Arbeit: Die Schritte 1 bis 3 stehen, die Suche nach dem Weg kommt noch.

**Ablauf:** Klonen, Sinatra installieren (`gem install sinatra rackup puma`, einmal pro Workspace), den Server starten und die Karte im Browser über den Reiter »Ports« öffnen. Dann liest das Programm die Wege, rechnet Abstände aus Koordinaten und baut den Graphen.

**Stolpersteine:** Nach jeder Änderung an `routes.rb` muss der Server neu starten (<kbd>Strg</kbd><kbd>C</kbd>, dann `ruby server.rb`). `Math.cos` rechnet im Bogenmaß; wer Grad hineingibt, bekommt nach Osten falsche Abstände, und die Prüfung sagt das.

**Datenschutz:** Start und Ziel sind öffentliche Orte, Sehenswürdigkeiten und Schulen; die Karte hat kein Feld für Adressen. Bitte keine Aufgabe »dein Weg nach Hause« stellen.

**Daten:** aus OpenStreetMap (© OpenStreetMap-Mitwirkende, ODbL 1.0), gebaut von `src/scripts/collect-streets/`. Andere Gebiete schneidet das Skript per Rechteck aus.
