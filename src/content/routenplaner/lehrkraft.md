Lernende bauen einen Routenplaner für Berlin mit echten Karten von OpenStreetMap, in Ruby (Sinatra) oder Python (Flask), je nach Einstellung der Klasse: Sie bauen aus den Straßen einen Graphen, schreiben die Suchen selbst, Breitensuche, Dijkstra, einen Heap und A*, und planen zu Fuß, mit dem Rad, mit dem Auto und eine Rundfahrt durch mehrere Orte. Eine Seite im Browser zeigt bei jeder Suche, welche Knoten ihr Code in welcher Reihenfolge besucht hat.

<table class='table'>
<tr><th>Dauer</th><td>Etwa drei Doppelstunden für die ersten fünf Schritte und drei für den Rest; die Schritte 6 bis 9 eignen sich auch für schnelle Gruppen oder ein Projekt.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>Einrichten:</b> Klonen, Sinatra (<code>gem install sinatra rackup puma</code>) oder Flask (<code>pip install flask</code>) installieren, einmal pro Workspace, den Server starten (<code>ruby server.rb</code> oder <code>python3 server.py</code>) und die Karte im Browser öffnen: Der Server schreibt ihre Adresse ins Terminal, ein Klick mit Strg öffnet sie. Die Vorlage hat beide Sprachen nebeneinander; die Seite im Browser ist dieselbe.</li>
<li><b>Der Graph:</b> Das Programm liest die Wege, rechnet Abstände aus Koordinaten und baut den Graphen; die Ansicht »Graph« macht ihn auf der Karte sichtbar.</li>
<li><b>Die Suchen:</b> Breitensuche und Dijkstra schreiben die Lernenden selbst, die Seite zeigt bei jeder Suche, in welcher Reihenfolge sie die Knoten besucht hat. Daran sieht man den Unterschied: Die Breitensuche breitet sich nach Kanten aus, Dijkstra in Metern, wie Ringe um den Start. Danach ein Heap; Dijkstra bauen die Lernenden selbst auf ihn um (ein Kasten »Jetzt bist du dran« mit Tipps in Stufen). »Dijkstra mit dem Heap« ruft <code>dijkstra</code> mit den Wegen von <code>detour.tsv</code> und einem kleinen Graphen auf, in dem ein Knoten zweimal in den Heap kommt, und sieht nach, dass der zweite übersprungen wird und dass <code>dijkstra</code> überhaupt <code>MinHeap#pop</code> aufruft; die Fassung mit <code>frontier</code> besteht sie nicht. Dann A*.</li>
<li><b>Unterwegs:</b> zu Fuß, mit dem Rad und mit dem Auto (Einbahnstraßen, Minuten statt Meter).</li>
<li><b>Die Rundfahrt:</b> alle Reihenfolgen, nächster Nachbar, 2-opt.</li>
</ul></td></tr>
<tr><th>Gebiete</th><td><ul>
<li>Die Vorlage hat Steglitz-Zehlendorf (45 181 Wege).</li>
<li>Jeden anderen der zwölf Bezirke lädt <code>server.rb</code> mit <code>AREA=mitte</code> (und so weiter) beim ersten Start aus <code>specht/workspace-files</code>.</li>
<li>Ganz Berlin (<code>AREA=berlin</code>, 464 000 Wege) braucht etwa 25 Sekunden zum Start und gut 1,5 GB Speicher und ist erst mit schnellen Suchen sinnvoll.</li>
</ul></td></tr>
<tr><th>Datenschutz</th><td>Start und Ziel sind öffentliche Orte, Sehenswürdigkeiten, Plätze und Schulen (die Schulen aus dem Schulverzeichnis Berlins, nur Name und Lage); die Karte hat kein Feld für Adressen. Bitte keine Aufgabe »dein Weg nach Hause« stellen.</td></tr>
<tr><th>Daten</th><td>Aus OpenStreetMap (© OpenStreetMap-Mitwirkende, ODbL 1.0), gebaut von <code>src/scripts/collect-streets/</code>. Die Bezirke schneidet das Skript an ihren Grenzen aus, andere Gebiete per Rechteck.</td></tr>
</table>

#### Zum Vergleichen

- **Zur Pfaueninsel** (nur mit der Fähre zu erreichen) vom Gymnasium Steglitz besuchen Breitensuche und Dijkstra fast alle 141 148 Knoten des Bezirks; die Breitensuche findet 23,9 km, Dijkstra 16,7 km. Dijkstra mit `min_by` braucht etwa 5 Sekunden, mit dem Heap knapp eine.
- **Vom Gymnasium Steglitz zum Brandenburger Tor** (drei Bezirke, 356 615 Knoten): 8,0 km, Dijkstra besucht 225 556 Knoten in 2 s, A* 23 996 in 0,2 s. Mit dem Auto 9,8 km in 12 min; A* besucht dort noch fast die Hälfte, weil die Schätzung mit 90 km/h rechnet.
- **Die Rundfahrt durch acht Orte in Mitte** zu Fuß: nächster Nachbar 11,2 km, alle Reihenfolgen 9,8 km, 2-opt 9,86 km.
- Die Testkarte `data/detour.tsv` ist so gebaut, dass Breitensuche und Dijkstra verschiedene Wege finden.

#### Tipps in Stufen

Jede Aufgabe, in der eine Funktion entsteht, hat zwei oder drei Tipps: ein Beispiel von Hand auf den Testkarten (oft mit dem Hinweis auf das Widget im Handbuch), den Weg in Worten, ein Gerüst mit Lücken. Die nächste Stufe erscheint eine Minute nach der vorigen, die ganze Lösung steht in keiner. Wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst; hängen viele an derselben Stelle, ist das ein Moment für die ganze Gruppe.

#### Stolpersteine

- Nach jeder Änderung an `routes.rb` muss der Server neu starten (<kbd>Strg</kbd><kbd>C</kbd>, dann `ruby server.rb`).
- Ein Fehler in einer Suche steht im Terminal, die Seite sagt nur, dass etwas schiefging.
- `Math.cos` rechnet im Bogenmaß; wer Grad hineingibt, bekommt nach Osten falsche Abstände, und die Prüfung sagt das.

#### Eigene Projekte

Nach der Seite haben die Lernenden drei Suchen auf einer echten Karte. Die Ideen stellen eine eigene Frage an diese Karte; bewertet wird beim Vorführen auf der Karte und daran, dass die Lernenden erklären, was sie an Gewichten oder Suche geändert haben.

- **Der schönste Schulweg** (eine Doppelstunde): nicht der kürzeste, sondern der ruhigste oder der mit den wenigsten großen Straßen. Dafür bekommt jede Kante ein anderes Gewicht als ihre Länge, wie in [»Zu Fuß, mit dem Rad, mit dem Auto«](#zu-fuß-mit-dem-rad-mit-dem-auto); die Lernenden begründen ihre Gewichte und zeigen, wie sich der Weg ändert.
- **Wie weit komme ich in zehn Minuten?** (eine Doppelstunde): [Dijkstra](#dijkstra) ohne Ziel, der aufhört, wenn die Zeit um ist, und alle erreichten Kreuzungen einfärbt. Zu Fuß, mit dem Rad und mit dem Auto entstehen drei verschiedene Flecken um die Schule.
- **Suchen im Vergleich** (eine Doppelstunde): Breitensuche, Dijkstra und [A*](#a-mit-dem-ziel-vor-augen) auf zwanzig zufälligen Paaren von Orten, gezählt wird, wie viele Kreuzungen jede besucht. Abgegeben wird eine Tabelle mit einer Erklärung, wann A* viel spart und wann kaum.
