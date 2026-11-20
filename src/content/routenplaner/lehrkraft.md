Lernende bauen einen Routenplaner für Berlin mit echten Karten von OpenStreetMap, in Ruby, Python, Dart, JavaScript oder Java, je nach Einstellung der Klasse: Sie bauen aus den Straßen einen Graphen, schreiben die Suchen selbst, Breitensuche, Dijkstra, einen Heap und A*, und planen zu Fuß, mit dem Rad, mit dem Auto und eine Rundfahrt durch mehrere Orte. Eine Seite im Browser zeigt bei jeder Suche, welche Knoten ihr Code in welcher Reihenfolge besucht hat.

<table class='table'>
<tr><th>Ziele</th><td>Die Lernenden können danach<ul>
<li>aus Wegen mit Koordinaten einen Graphen bauen und Abstände auf der Erde berechnen,</li>
<li>Breitensuche, Dijkstra und A* von Hand an einem kleinen Graphen ausführen und schreiben,</li>
<li>begründen, warum die Breitensuche einen Weg mit wenigen Kanten findet, der lang sein kann, und Dijkstra den kürzesten,</li>
<li>erklären, warum ein Heap Dijkstra schneller macht und warum für eine Rundfahrt durch viele Orte nur noch Näherungen wie der nächste Nachbar und 2-opt helfen.</li>
</ul></td></tr>
<tr><th>Sprachen</th><td><ul>
<li>Die Funktionen schreiben die Lernenden in Ruby, Python, Dart, JavaScript oder Java, den Sprachen der Data Structures Sandbox: Eine Gruppe, die dort ihre Datenstrukturen in Dart gebaut hat, bleibt bei Dart. Die Klassen aus der Sandbox laufen hier nicht unverändert (sie speichern nur ganze Zahlen über die Speicher-Objekte der Sandbox), aber die Sprache und ihre Eigenheiten kommen mit.</li>
<li>Der Server ist immer derselbe, in Ruby; die Lernenden ändern ihn nicht. Er startet die Funktionen der gewählten Sprache als eigenes Programm (eine Brücke je Sprache in der Vorlage) und startet sie neu, sobald eine Datei im Ordner der Sprache gespeichert wird.</li>
<li>Alle Schritte gibt es in allen fünf Sprachen, vom Lesen der Karte bis zu 2-opt. Den Heap schreiben die Lernenden in jeder Sprache selbst, auch in Java, das eine <code>PriorityQueue</code> hätte. Ob <code>dijkstra</code> den Heap benutzt, sieht die Prüfung in Ruby, Python und JavaScript beim Aufruf, in Dart und Java am Code der Funktion: Steht dort kein <code>MinHeap</code> mit <code>pop</code>, besteht die Fassung mit <code>frontier</code> die Aufgabe nicht.</li>
</ul></td></tr>
<tr><th>Dauer</th><td>Etwa drei Doppelstunden für die ersten fünf Schritte und drei für den Rest; die Schritte 6 bis 9 eignen sich auch für schnelle Gruppen oder ein Projekt.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>Einrichten:</b> Klonen, den Server mit <code>./run</code> und der Sprache starten (<code>./run dart</code>; beim ersten Mal installiert <code>./run</code> Sinatra, einmal pro Workspace) und die Karte im Browser öffnen: Der Server schreibt ihre Adresse ins Terminal, ein Klick mit Strg öffnet sie. Die Vorlage hat alle fünf Sprachen nebeneinander, jede in ihrem Ordner; die Seite im Browser ist dieselbe.</li>
<li><b>Der Graph:</b> Das Programm liest die Wege, rechnet Abstände aus Koordinaten und baut den Graphen; die Ansicht »Graph« macht ihn auf der Karte sichtbar.</li>
<li><b>Die Suchen:</b> Breitensuche und Dijkstra schreiben die Lernenden selbst, die Seite zeigt bei jeder Suche, in welcher Reihenfolge sie die Knoten besucht hat. Daran sieht man den Unterschied: Die Breitensuche breitet sich nach Kanten aus, Dijkstra in Metern, wie Ringe um den Start. Danach ein Heap; Dijkstra bauen die Lernenden selbst auf ihn um (ein Kasten »Jetzt bist du dran« mit Tipps in Stufen). »Dijkstra mit dem Heap« ruft <code>dijkstra</code> mit den Wegen von <code>detour.tsv</code> und einem kleinen Graphen auf, in dem ein Knoten zweimal in den Heap kommt, und sieht nach, dass der zweite übersprungen wird und dass <code>dijkstra</code> überhaupt <code>pop</code> eines <code>MinHeap</code> aufruft; die Fassung mit <code>frontier</code> besteht sie nicht. Dann A*.</li>
<li><b>Unterwegs:</b> zu Fuß, mit dem Rad und mit dem Auto (Einbahnstraßen, Minuten statt Meter).</li>
<li><b>Die Rundfahrt:</b> alle Reihenfolgen, nächster Nachbar, 2-opt.</li>
</ul></td></tr>
<tr><th>Ohne Rechner</th><td><ul>
<li><b>Vor »Kürzeste Wege«, 20 Minuten, zu viert:</b> ein Blatt mit einem kleinen Straßennetz, zehn bis zwölf Kreuzungen mit Buchstaben, ohne Längen, Start und Ziel markiert, dazu Haftzettel. Am Tischrand klebt eine Reihe Haftzettel, die Queue: Vorn nimmt die Gruppe einen weg, hinten klebt sie neue an; zu Beginn steht dort nur der Start. Für den vordersten Zettel bekommt jeder Nachbar, der noch keinen Zettel auf der Karte hat, einen (»gefunden von …«) und kommt hinten in die Queue. Ist das Ziel vorn, folgt die Gruppe den Zetteln auf der Karte vom Ziel zurück zum Start.</li>
<li><b>Was sichtbar wird:</b> Die Suche breitet sich in Wellen um den Start aus, ohne die Karte als Ganzes zu sehen; den Weg kennt sie erst am Ende, rückwärts über die Vorgänger.</li>
<li><b>Zurück zur Seite:</b> <a href='#breitensuche'>»Breitensuche«</a> hat dieselben vier Schritte: Die Zettel auf der Karte sind <code>previous</code>, der Weg zurück ist <code>path_to</code>, und die Seite im Browser zeigt die besuchten Knoten in der Reihenfolge, in der die Gruppe ihre Zettel vorn weggenommen hat. Längen kommen erst nach der Frage »Wenige Kanten, langer Weg« an die Straßen, für <a href='#dijkstra'>»Dijkstra«</a>.</li>
</ul></td></tr>
<tr><th>Gebiete</th><td><ul>
<li>Die Vorlage hat Steglitz-Zehlendorf (45 181 Wege).</li>
<li>Jeden anderen der zwölf Bezirke lädt der Server mit <code>AREA=mitte ./run dart</code> (und so weiter) beim ersten Start aus <code>specht/workspace-files</code>.</li>
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
- Die Testkarte `data/shortcut.tsv` ist so gebaut, dass ein Dijkstra ohne das Verbessern (ein Nachbar kommt nur dazu, wenn er neu ist, wie in der Breitensuche) einen falschen Weg findet: Den Knoten 44 erreicht die Suche zuerst über 42 nach 600 Metern, über 43 sind es nur 500. Die Aufgabe »Dijkstra« prüft darum beide Karten.

#### Tipps in Stufen

Jede Aufgabe, in der eine Funktion entsteht, hat zwei oder drei Tipps: ein Beispiel von Hand auf den Testkarten (oft mit dem Hinweis auf das Widget im Handbuch), den Weg in Worten, ein Gerüst mit Lücken. Die nächste Stufe erscheint eine Minute nach der vorigen, die ganze Lösung steht in keiner. Wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst; hängen viele an derselben Stelle, ist das ein Moment für die ganze Gruppe.

#### Stolpersteine

- Neu starten muss niemand: Der Server startet die Funktionen nach dem Speichern selbst neu. Ein Fehler (ein Tippfehler, der sich nicht übersetzen lässt, eine Exception in einer Suche) steht im Terminal mit der Zeile und auf der Karte mit seiner ersten Zeile.
- Einen Heap aus der Data Structures Sandbox (Dart) bringen Lernende mit: `my_array_heap.dart` in den Ordner `dart`, die erste Zeile auf `import 'heap_sandbox.dart';` geändert, dazu ein `MinHeap`, der Paare aus Priorität und Knoten in eine ganze Zahl packt (er steht auf der Seite unter dem Heap). Das geht auch mit einem Heap aus dem gemeinsamen Repository der Klasse. Die Aufgabe »Eine Priority Queue« prüft den Heap darunter wie jeden anderen: Ein geliehener Heap, der falsch ordnet, fällt dort auf, bevor Dijkstra auf der Karte falsche Wege findet.
- In Dart, JavaScript und Java braucht die Breitensuche eine schnelle Queue: `Queue` aus `dart:collection`, `ArrayDeque` in Java, in JavaScript ein Index statt `shift()`. Mit `removeAt(0)` oder `shift()` rückt bei jedem Knoten das ganze Array nach vorne, und die Suche über 141 000 Knoten dauert Minuten. Ein guter Anlass, an die Queue als Ring Buffer aus der Sandbox zu erinnern.
- `Math.cos` rechnet im Bogenmaß; wer Grad hineingibt, bekommt nach Osten falsche Abstände, und die Prüfung sagt das.

#### Eigene Projekte

Wie man ein Projekt mit Meilensteinen und Kriterien begleitet: [Eigene Projekte begleiten](/lehrmaterial-projekte).

Nach der Seite haben die Lernenden drei Suchen auf einer echten Karte. Die Ideen stellen eine eigene Frage an diese Karte; bewertet wird beim Vorführen auf der Karte und daran, dass die Lernenden erklären, was sie an Gewichten oder Suche geändert haben.

- **Der schönste Schulweg** (eine Doppelstunde): nicht der kürzeste, sondern der ruhigste oder der mit den wenigsten großen Straßen. Dafür bekommt jede Kante ein anderes Gewicht als ihre Länge, wie in [»Zu Fuß, mit dem Rad, mit dem Auto«](#zu-fuß-mit-dem-rad-mit-dem-auto); die Lernenden begründen ihre Gewichte und zeigen, wie sich der Weg ändert. *Meilensteine:* die Gewichte mit ihrer Begründung; ein Weg mit den neuen Gewichten; der Vergleich mit dem kürzesten Weg auf der Karte. *Kriterien:* Der Weg ändert sich so, wie die Gewichte es erwarten lassen, und die Lernenden begründen jedes Gewicht.
- **Wie weit komme ich in zehn Minuten?** (eine Doppelstunde): [Dijkstra](#dijkstra) ohne Ziel, der aufhört, wenn die Zeit um ist, und alle erreichten Kreuzungen einfärbt. Zu Fuß, mit dem Rad und mit dem Auto entstehen drei verschiedene Flecken um die Schule. *Meilensteine:* auf Papier, wann Dijkstra aufhört; der Fleck für den Weg zu Fuß; drei Flecken nebeneinander. *Kriterien:* Jede eingefärbte Kreuzung ist in der Zeit erreichbar, und keine erreichbare Kreuzung fehlt; die Lernenden erklären, warum Dijkstra dafür kein Ziel braucht.
- **Suchen im Vergleich** (eine Doppelstunde): Breitensuche, Dijkstra und [A*](#a-mit-dem-ziel-vor-augen) auf zwanzig zufälligen Paaren von Orten, gezählt wird, wie viele Kreuzungen jede besucht. Abgegeben wird eine Tabelle mit einer Erklärung, wann A* viel spart und wann kaum. *Meilensteine:* eine Vermutung, welche Suche wie viel spart; die Zählung für ein Paar; die Tabelle für zwanzig Paare. *Kriterien:* Alle drei Suchen laufen auf denselben Paaren, und die Erklärung nennt Paare, bei denen A* viel spart, und solche, bei denen kaum, mit dem Grund.
