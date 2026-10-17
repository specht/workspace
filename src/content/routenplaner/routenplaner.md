<div class='meta'>
image: karte.webp
kind: projekt
language: Ruby
topic: computing
duration: 3 Doppelstunden
template: https://github.com/specht/routenplaner-starter.git
template_name: der Vorlage des Routenplaners
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Ein Routenplaner für Berlin

<p class='abstract'>
Du baust einen Routenplaner wie den in deinem Handy, mit echten Karten von OpenStreetMap: Dein Programm macht aus den Straßen und Wegen von Berlin einen Graphen und findet darin den kürzesten Weg, von deiner Schule zum Botanischen Garten oder quer durch die Stadt zum Brandenburger Tor. Erst in Steglitz, dann in ganz Berlin, wo der schnelle Weg auch eine schnelle Suche braucht.
</p>

<!--
Noch in Arbeit (nur in der Entwicklung im Katalog): die Schritte 1 bis 5 stehen,
bis Dijkstra. Es fehlen die Prioritätswarteschlange, ganz Berlin mit A*, die
Arten zu reisen und die Stadtrundfahrt. Die Vorlage entsteht in vorlage/;
specht/routenplaner-starter bekommt sie als Patch.
-->

## Einrichten

### Repository klonen

Für diese Anleitung brauchst du die Vorlage des Routenplaners:

_include(workspace#repository-klonen)

### Den Server starten

Der Routenplaner läuft im Browser. Die Seite mit der Karte bekommt er von einem kleinen Server in Ruby, der mit [Sinatra](https://sinatrarb.com/) gebaut ist. Sinatra installierst du einmal, es bleibt in deinem Workspace:

```bash
gem install sinatra rackup puma
```

Dann startest du den Server:

```bash
ruby server.rb
```

<!-- tutorial-screenshot
# Die Vorlage so, wie das Klonen sie anlegt (Dateien aus vorlage/, also
# ohne das Repository auf GitHub), den Ordner öffnen, Sinatra installieren
# und den Server starten
show-left-sidebar
write-file: server.rb <- file:vorlage/server.rb
write-file: routes.rb <- file:vorlage/routes.rb
write-file: README.md <- file:vorlage/README.md
write-file: public/index.html <- file:vorlage/public/index.html
write-file: public/map.css <- file:vorlage/public/map.css
write-file: public/map.js <- file:vorlage/public/map.js
write-file: data/steglitz.tsv <- file:vorlage/data/steglitz.tsv
write-file: data/steglitz-places.tsv <- file:vorlage/data/steglitz-places.tsv
write-file: data/test.tsv <- file:vorlage/data/test.tsv
press: Control+K
press: Control+O
wait-for-input-value: /workspace/
type: routenplaner-starter
press: Enter
wait-for-input-value: /workspace/routenplaner-starter/
press: Enter
wait-for-text: routes.rb
open-file: routes.rb
close-tab: Welcome
hide-left-sidebar
terminal-open
terminal-maximize
terminal-run: gem install sinatra rackup puma
terminal-wait-for-prompt: 180
terminal-run: clear
terminal-run: ruby server.rb
wait-for-text: Beenden mit Strg+C
check-task-fails: lesen
check-task-fails: abstand
check-task-fails: graph
crop-terminal-lines: auto
-->
<img class='full' src='server.webp' alt='Im Terminal läuft server.rb: steglitz: 13493 Wege, 34 Orte, »Noch nicht fertig: read_ways«, die Karte auf Port 4567, und Sinatra hat auf Port 4567 begonnen.'>

Der Server sagt, was er gefunden hat: 13 493 Wege in Steglitz und 34 Orte. Dass `read_ways` noch nicht fertig ist, stimmt: Das schreibst du gleich selbst. Der Server läuft so lange, bis du ihn mit <kbd>Strg</kbd><kbd>C</kbd> anhältst.

Visual Studio Code leitet seinen Port 4567 an eine Adresse weiter, die dein Browser erreicht. Du findest sie im Reiter »Ports« neben »Terminal«, in der Spalte »Forwarded Address«. Klicke mit gedrückter <kbd>Strg</kbd>-Taste darauf:

<!-- tutorial-screenshot
preview-port: 4567
tab: preview
-->
<img class='full' src='karte.webp' alt='Die Karte von Steglitz im Browser, gezeichnet aus den Daten: Hauptstraßen gelb, die Autobahn orange, Wohnstraßen weiß; der Botanische Garten, der Bierpinsel, das Titania-Palast und das Rathaus Steglitz sind beschriftet. Links oben die Felder für Start und Ziel, unten links der Maßstab.'>

Ziehen verschiebt die Karte, das Mausrad und die Knöpfe rechts oben zoomen, ein Doppelklick holt eine Stelle heran. Oben links wählst du Start und Ziel: Tipp den Anfang eines Namens, und die Seite schlägt Sehenswürdigkeiten und Schulen in Steglitz vor. Die »Legende« darunter sagt, welche Farbe welche Art von Weg ist. Einen Weg findet die Seite noch nicht; das bringst du ihr bei.

## Die Karte

Die Karte kommt von [OpenStreetMap](https://www.openstreetmap.org/), einer Karte der ganzen Welt, an der alle mitzeichnen dürfen. Sie besteht aus **Wegen**: Jede Straße, jeder Fußweg, jede Treppe ist eine Folge von Punkten. Wo sich zwei Wege kreuzen, teilen sie sich einen Punkt.

Öffne `data/steglitz.tsv`. Jede Zeile ist ein Weg, die vier Teile trennt ein Tabulator:

```text
residential		Schloßstraße	26735834 13.326700,52.456700 26735901 13.325300,52.457700 …
```

| Teil | Bedeutung |
| --- | --- |
| Art | was für ein Weg: `residential` (Wohnstraße), `primary` (Hauptstraße), `footway` (Fußweg), `cycleway` (Radweg), `steps` (Treppe), … |
| Einbahnstraße | `yes`, wenn man nur in Richtung der Punkte fahren darf, `-1` nur dagegen, sonst leer |
| Name | der Name der Straße, oft leer |
| Punkte | je eine ID und die Lage als Länge und Breite in Grad, mit Komma dazwischen |

Die ID macht den Punkt eindeutig: Zwei Wege mit derselben ID an einem Punkt sind an dieser Stelle verbunden. Länge und Breite sind die Koordinaten, wie in jeder Karten-App: Steglitz liegt bei 13,3 Grad östlicher Länge und 52,45 Grad nördlicher Breite.

_task(fusswege)

<!-- tutorial-step
check-task: fusswege <- 6589
-->

Jetzt liest dein Programm die Datei. `read_ways` in `routes.rb` soll für jeden Weg einen Hash zurückgeben, mit der Art, der Einbahnstraße, dem Namen und den Punkten. Ein Punkt ist ein Array aus ID, Länge und Breite:

<!-- page-code: own -->
```ruby
{ kind: 'residential', oneway: '', name: 'Schloßstraße',
  points: [['26735834', 13.3267, 52.4567], ['26735901', 13.3253, 52.4577]] }
```

`File.readlines(path, chomp: true)` gibt die Zeilen ohne Zeilenumbruch, `split("\t")` teilt eine Zeile an den Tabulatoren. Die Punkte teilst du an den Leerzeichen und nimmst je zwei Teile zusammen: `each_slice(2)`. `to_f` macht aus `'13.326700'` die Zahl 13.3267.

Ausprobieren kannst du `read_ways` mit der kleinen Testkarte `data/test.tsv`: drei Wege, fünf Punkte.

_task(lesen)

## Wie weit ist es?

Für den kürzesten Weg braucht dein Programm Abstände in Metern. Die Koordinaten sind aber in Grad. Die Erde hat einen Radius von etwa 6 371 km; ein Grad ist ein 360stel ihres Umfangs:

$$1° = \frac{2\pi \cdot 6\,371\,000\ \text{m}}{360} \approx 111\,195\ \text{m}$$

Das gilt für die Breite, nach Norden. Nach Osten stimmt es nur am Äquator: Die Längengrade laufen an den Polen zusammen, und je weiter nördlich man ist, desto näher liegen sie beieinander, um den Faktor \\(\cos(\text{Breite})\\). In Berlin ist ein Grad Länge nur gut 67 km lang.

_task(grad)

Auf den paar Kilometern einer Stadt ist die Erde fast flach. Du rechnest beide Unterschiede in Meter um und nimmst den Satz des Pythagoras:

$$d = \sqrt{(\Delta\text{Länge} \cdot \cos(\text{Breite}))^2 + (\Delta\text{Breite})^2} \cdot \frac{\pi}{180} \cdot 6\,371\,000\ \text{m}$$

Die Winkel sind dabei im Bogenmaß: `Math.cos` und die Umrechnung mit \\(\pi / 180\\) erwarten keine Grad. Als Breite nimmst du die Mitte zwischen den beiden Punkten.

_task(abstand)

## Vom Weg zum Graphen

Ein Routenplaner sucht nicht in Wegen, sondern in einem **Graphen**: Punkte, die **Knoten**, und Verbindungen zwischen ihnen, die **Kanten**. Jede Kante hat eine Länge in Metern. Aus jedem Weg werden so viele Kanten, wie er Abschnitte hat: zwischen dem ersten und dem zweiten Punkt, dem zweiten und dem dritten und so weiter. Ein Fußweg, der zwei Straßen verbindet, wird zu Kanten, die über die gemeinsamen Punkte an beide anschließen.

`build_graph` bekommt die Wege und gibt zwei Hashes zurück: `nodes` mit der Lage jedes Punktes, `edges` mit seinen Nachbarn und dem Abstand zu jedem. Kanten gelten erst einmal in beide Richtungen; um Einbahnstraßen kümmerst du dich später:

<!-- page-code: own -->
```ruby
{ nodes: { '2' => [13.301, 52.45], … },
  edges: { '2' => [['1', 67.8], ['3', 67.8], ['4', 111.2]], … } }
```

`each_cons(2)` gibt immer zwei aufeinanderfolgende Punkte eines Weges. `Hash.new { |hash, id| hash[id] = [] }` ist ein Hash, der für einen neuen Schlüssel von selbst ein leeres Array anlegt.

_task(graph)

Halte den Server mit <kbd>Strg</kbd><kbd>C</kbd> an und starte ihn neu. Er baut jetzt mit deinen Funktionen den Graphen von Steglitz. Schalte auf der Seite »Graph zeigen« ein: Über der blassen Karte siehst du, was `build_graph` gebaut hat, jeden Knoten als Punkt und jede Kante als Linie. Klicke auf eine Kreuzung: Die Seite zeigt den Knoten dort und seine Nachbarn.

<!-- tutorial-step
# Die Lösung der ersten drei Schritte, der Server neu gestartet (im
# Workspace; das Bild danach ist die Vorschau)
write-file: routes.rb <- file:loesung/routes-3.rb
check-task: lesen
check-task: abstand
check-task: graph
terminal-open
press: Control+C
terminal-wait-for-prompt
terminal-run: clear
terminal-run: ruby server.rb
# »Graph:« schreibt erst der neue Server, mit dem Graphen aus routes.rb
wait-for-text: Graph:
wait-for-text: Beenden mit Strg+C
sleep: 2
-->

<!-- tutorial-screenshot
# Auf der Karte nah heran und auf eine Kreuzung geklickt
# (tutorial-screenshot-hooks.mjs)
preview-reload
tab: preview
# die Kreuzung Kuhligkshofstraße / Albrechtstraße, nicht weit von der Schule
hook: show 13.322572 52.456434 220000 graph
wait-for-text: 4 Nachbarn
-->
<img class='full' src='nachbarn.webp' alt='Der Graph über der blassen Karte, nah herangezoomt an der Kreuzung Kuhligkshofstraße und Albrechtstraße: die Knoten als blaue Punkte, die Kanten als blaue Linien. Der angeklickte Knoten ist pink, seine vier Nachbarn sind mit pinken Linien verbunden; oben links steht, dass er 4 Nachbarn hat und wie weit sie entfernt sind.'>

Eine Straße ist im Graphen mehr als ihre Kreuzungen: Wo sie eine Kurve macht, hat sie Punkte, die nur ihre Form zeichnen. Auch das sind Knoten, mit genau zwei Nachbarn.

## Breitensuche

Jetzt sucht dein Programm einen Weg. Die einfachste Suche geht vom Start aus wie eine Welle: erst zu allen Nachbarn des Starts, dann zu deren Nachbarn, dann zu den Nachbarn der Nachbarn. Das heißt **Breitensuche**. Die Knoten, die sie gefunden, aber noch nicht weiterverfolgt hat, stehen in einer **Warteschlange**: Wer zuerst hineinkommt, ist zuerst wieder dran. `queue.shift` nimmt den ersten Knoten heraus, `queue << node` stellt einen hinten an.

Damit die Suche am Ende den Weg kennt, merkt sie sich für jeden gefundenen Knoten, von welchem aus sie ihn gefunden hat: seinen **Vorgänger**. In `previous` steht für den Start `nil`, für jeden anderen gefundenen Knoten sein Vorgänger:

<!-- page-code: own -->
```ruby
previous = { '11' => nil, '12' => '11', '21' => '11', '13' => '12', … }
```

So weiß die Suche auch, welche Knoten sie schon kennt: Ein Nachbar, der schon in `previous` steht, kommt nicht noch einmal in die Warteschlange.

Ist die Suche am Ziel, geht der Weg vom Ziel aus rückwärts: zum Vorgänger des Ziels, zu dessen Vorgänger und so weiter, bis zum Start, dessen Vorgänger `nil` ist. Das macht `path_to`. Weil du den Weg rückwärts findest, kommt jeder Knoten vorne dazu: `path.unshift(node)`.

_task(weg)

Die Breitensuche selbst, Schritt für Schritt:

1. In der Warteschlange steht nur der Start, in `previous` der Start mit `nil`.
2. Nimm den ersten Knoten aus der Warteschlange und schreib ihn in `visited`. Ist es das Ziel, bist du fertig.
3. Jeder Nachbar, der noch nicht in `previous` steht, bekommt den Knoten als Vorgänger und kommt hinten in die Warteschlange.
4. Weiter mit 2, solange die Warteschlange nicht leer ist. Wird sie leer, ohne dass das Ziel dran war, gibt es keinen Weg.

`visited` braucht die Suche selbst nicht: Die Seite zeigt damit, in welcher Reihenfolge die Suche die Knoten besucht hat. Zum Ausprobieren gibt es die Testkarte `data/detour.tsv`: Die Lindenstraße geht in fünf Kanten geradeaus vom Knoten 11 zum Knoten 16, der Eichenweg macht einen Bogen und braucht nur drei.

_task(breitensuche)

<!-- tutorial-step
# Die Lösung bis zur Breitensuche und zu Dijkstra, der Server neu gestartet
write-file: data/detour.tsv <- file:vorlage/data/detour.tsv
write-file: routes.rb <- file:loesung/routes-5.rb
check-task: weg
check-task: breitensuche
check-task: dijkstra
terminal-open
press: Control+C
terminal-wait-for-prompt
terminal-run: clear
terminal-run: ruby server.rb
wait-for-text: Graph:
wait-for-text: Beenden mit Strg+C
sleep: 2
-->

Starte den Server neu, wähle einen Start und ein Ziel und lass die Breitensuche suchen. Die Seite zeigt erst die Suche, jeden besuchten Knoten als Punkt, von Gelb (früh besucht) bis Rot (spät), und dann den Weg, den sie gefunden hat:

<!-- tutorial-screenshot
# Vom Fichtenberg-Gymnasium zum Titania-Palast, mit der Breitensuche
preview-reload
tab: preview
hook: route Fichtenberg | Titania | bfs
-->
<img class='full' src='breitensuche.webp' alt='Die Breitensuche vom Fichtenberg-Gymnasium zum Titania-Palast: Über der blassen Karte liegen die besuchten Knoten als Punkte, gelb um die Schule, nach außen orange und rot; der gefundene Weg ist eine lila Linie mit Zacken und Umwegen. Oben links steht, wie lang er ist, über wie viele Knoten er geht und wie viele Knoten die Suche besucht hat.'>

Die Welle breitet sich nach Kanten aus, nicht nach Metern: Im Botanischen Garten, wo die Wege viele kurze Kanten haben, kommt sie langsam voran, auf langen geraden Straßen schnell. Darum macht ihr Weg Zacken und Umwege.

_task(kanten)

## Dijkstra

Den kürzesten Weg in Metern findet die Suche von [Edsger W. Dijkstra](https://de.wikipedia.org/wiki/Edsger_W._Dijkstra) aus dem Jahr 1959. Sie nimmt als Nächstes nicht den Knoten, der am längsten wartet, sondern den, der vom Start aus am nächsten liegt. Dafür merkt sie sich für jeden gefundenen Knoten, wie weit es bis zu ihm ist, auf dem besten Weg, den sie bis jetzt kennt:

1. `metres` sagt, wie weit jeder gefundene Knoten vom Start weg ist, `previous` wieder seinen Vorgänger, `frontier` die Knoten, die gefunden, aber noch nicht dran waren, mit ihrem Abstand. Am Anfang steht in allen dreien nur der Start, mit 0 Metern und ohne Vorgänger.
2. Nimm aus `frontier` den Knoten mit dem kleinsten Abstand und schreib ihn in `visited`. Ist es das Ziel, bist du fertig.
3. Für jeden Nachbarn: Wie weit wäre er über diesen Knoten? Sein Abstand plus die Länge der Kante. Kennst du den Nachbarn noch nicht, oder ist der Weg über diesen Knoten kürzer als der bisher beste, bekommt er den neuen Abstand und diesen Knoten als Vorgänger und kommt (wieder) in `frontier`.
4. Weiter mit 2, solange `frontier` nicht leer ist.

Warum stimmt der Abstand, wenn ein Knoten dran ist? Alle anderen in `frontier` sind weiter weg, und jeder Umweg über sie macht den Weg nur länger: Kanten sind nie kürzer als 0 Meter.

`frontier.min_by { |_id, m| m }` gibt den Knoten mit dem kleinsten Abstand, als Paar aus ID und Metern. `frontier.delete(node)` nimmt ihn heraus. Am Ende baut `path_to` den Weg, genau wie bei der Breitensuche.

_task(dijkstra)

Starte den Server neu und suche denselben Weg noch einmal, jetzt mit »Dijkstra«:

<!-- tutorial-screenshot
# Derselbe Weg mit Dijkstra
preview-reload
tab: preview
hook: route Fichtenberg | Titania | dijkstra
-->
<img class='full' src='dijkstra.webp' alt='Dijkstra vom Fichtenberg-Gymnasium zum Titania-Palast: Die besuchten Knoten liegen gelb nah an der Schule und werden in alle Richtungen gleichmäßig orange und rot, wie Ringe um den Start. Der Weg ist eine lila Linie, die gerade die Schloßstraße entlang führt. Oben links stehen Länge, Knoten und wie viele die Suche besucht hat.'>

Jetzt breitet sich die Welle in Metern aus, wie Ringe um den Start, und der Weg folgt der Schloßstraße statt Zacken zu machen. Er hat mehr Knoten, ist aber kürzer. Schau auf die Zeit: Dijkstra hat etwa so viele Knoten besucht wie die Breitensuche, braucht aber viel länger. `min_by` geht bei jedem Schritt durch die ganze `frontier`, und die wird groß. Für Steglitz geht das noch; wie es für größere Gebiete schneller wird, kommt als Nächstes.
