<div class='meta'>
icon: /images/pip/icon-mampfer.png
kind: uebung
duration: 2 Doppelstunden
languages: [ruby, python, javascript]
idea: "Dem Mampfer entkommen: Schritte zählen, das Licht nutzen und den kürzesten Weg mit einer Breitensuche finden"
curriculum: "Sek I: Algorithmisches Problemlösen (Miniwelten); Oberstufe: Algorithmen und Datenstrukturen (Graphen, Breitensuche)"
requires: [pip-2, kata-dicts]
</div>

# Pip unter Funkelbach: Der Mampfer

<p class='abstract'>
Ganz unten in den Stollen wohnt der Mampfer, und er nimmt immer den kürzesten
Weg zu Pip. Du zählst Schritte, nutzt das Licht der alten Lampen und schreibst
am Ende selbst die Suche, mit der er Pip findet.
</p>

<script src='/include/algo_stepper.js'></script>

Ein paar Tage später summt der große Funkenstein, den Bello aus der Tiefe
mitgebracht hat. Nachts glüht er rot. Der Wichtel dreht ihn in den Händen und
wird blass.

<div class='pip-wichtel'>
»Ein Herzstein«, sagt er. »Der gehört dem Mampfer. Er wohnt ganz unten, hinter
der letzten alten Lampe, die noch nie jemand angezündet hat. Der Stein summt,
weil er nach Hause will, und der Mampfer sucht ihn, seit Bello ihn mitgenommen
hat. Nach oben ist er ihm nicht gefolgt: Auf dem Weg brennen jetzt die Lampen,
und seine Augen sind fürs Dunkle gemacht, eine Lampe blendet ihn. Aber wer den
Stein trägt, den findet er. Er hört jeden Schritt, kennt jeden Gang und nimmt
immer den kürzesten Weg. Bring ihm seinen Stein zurück.«
</div>

## Der Mampfer

Nach jedem Schritt von Pip geht der Mampfer ein Feld weiter, auf dem kürzesten
Weg zu Pip. Ein Schritt ist jeder Befehl außer `say`: auch eine Drehung und das
Aufheben eines Steins. Bleibt Pip stehen, holt der Mampfer Pip also ein, auch
wenn dein Plan zu Ende ist, bevor Pip an der Lampe ist. Steht er auf Pips Feld
oder Pip auf seinem, schnappt er zu.

Bevor Pip losläuft: Auf dieser Karte macht Pip einen Schritt nach rechts.

<div class='pip-world pip-static' role='img' aria-label='Ein Raum mit einem langen Felsen in der Mitte, sieben Felder breit. Der Mampfer steht oben rechts in der ersten Reihe, Feld 1 liegt links neben ihm, Feld 2 rechts. Pip steht unten in der dritten Reihe, zwei Felder vom linken Rand, und schaut nach rechts.'>
<pre class='no-copy'>
########
#...1M2#
#.####.#
#..P...#
########
</pre>
</div>

_task(predict)

<!-- after: predict -->
Der Mampfer geht auf Feld 2, obwohl Pip links unter ihm zu stehen scheint. Er
geht nicht dorthin, wo Pip näher aussieht, sondern den Weg, der kürzer ist:
Links um den Felsen herum wären es neun Felder bis zu Pip, rechts herum fünf.
<!-- /after -->

_task(run)

<div class='hint try'>
<strong>Ausprobieren:</strong> Schreib vor den ersten Schritt zwei Zeilen mit
einer Drehung nach links und dahinter noch zwei, sodass Pip wieder nach rechts
schaut. Schau unter der Karte zu, wie viele Felder der Mampfer dabei aufholt.
</div>

## Im Licht

Die Lampen, die Pip unterwegs angezündet hat, brennen noch. In ihr Licht, die
neun Felder um eine brennende Lampe, geht der Mampfer nicht, auch nicht, wenn
Pip darin steht. Pip darf hinein. Auf der Karte ist das Licht ein heller Schein auf
dem Boden.

_task(stone-in-the-dark)

_task(both-sides)

## Was der Mampfer weiß

Woher kennt der Mampfer den kürzesten Weg? Er sucht von Pip aus, wie eine
Welle: erst die Felder einen Schritt von Pip entfernt, dann die zwei Schritte
entfernten und so weiter, bis er bei seinem eigenen Feld ankommt. Zu jedem
Feld merkt er sich, von welchem Feld aus er es gefunden hat, seinen
Vorgänger. Der Vorgänger seines eigenen Feldes liegt einen Schritt näher an
Pip: Dorthin geht er.

Das ist die Breitensuche aus dem Handbuch [Graphen](/reference/graphen), nur
auf der Karte: Jedes freie Feld ist ein Knoten, verbunden mit den freien
Feldern rechts, oben, links und unten. Die Nachbarn eines Feldes nimmt der
Mampfer immer in der Reihenfolge Osten, Norden, Westen, Süden. Ein Feld heißt
nach seiner Spalte und seiner Zeile, `5,3` ist Spalte 5 in Zeile 3, gezählt ab
0 und nach unten.

```pseudocode
breadth_first_search(graph, start, goal):
    predecessor ← { start: null }
    queue ← [start]
    while queue not empty:
        node ← take the front node from queue
        if node == goal: return path_back(predecessor, goal)
        for each neighbor of node:
            if neighbor not in predecessor:
                predecessor[neighbor] ← node
                add neighbor at the back of queue
    no path
```

Hier steht Pip auf `5,3`, der Mampfer auf `1,1`, und um den Felsen in der
Mitte führen zwei gleich lange Wege. Gelb sind die Felder in der Queue, lila
die schon herausgenommenen; jeder Pfeil zeigt von einem Feld auf seinen
Vorgänger. Am Ende steht der Weg von Pip zum Mampfer da, und der Pfeil, der
vom Feld des Mampfers ausgeht, ist sein nächster Schritt.

<div class='algo-stepper' data-title='Was der Mampfer sieht' data-algo='bfs' data-grid='#######|#M....#|#.##..#|#....P#|#######'></div>

Welcher der beiden gleich langen Wege es wird, entscheidet die Reihenfolge der
Nachbarn: Von Pip aus kommt Norden vor Westen, also findet die Suche den Weg
oben herum zuerst.

_person(moore)

Jetzt schreibst du diese Suche selbst. Die Karte des Wichtels ist eine Liste
von Zeilen, jede Zeile ein Text, mit denselben Zeichen wie oben: `#` Fels,
`.` Boden, `P` Pip, `M` der Mampfer, `L` die Lampe, `o` ein Funkenstein.
Ein Feld schreibst du als seine Position:

<!-- language: ruby -->
```ruby
cave = ['#######',
        '#P..#L#',
        '#.#...#',
        '#######']
puts cave[1][5]   # L: Zeile 1, Spalte 5
pip = [1, 1]      # eine Position: [x, y]
```
<!-- /language -->
<!-- language: python -->
```python
cave = ['#######',
        '#P..#L#',
        '#.#...#',
        '#######']
print(cave[1][5])   # L: Zeile 1, Spalte 5
pip = (1, 1)        # eine Position als Tupel: (x, y)
```

Positionen sind hier Tupel, keine Listen: Nur ein Tupel kann Schlüssel eines
Dictionarys sein, und die Suche braucht Dictionarys mit Positionen.
<!-- /language -->
<!-- language: javascript -->
```javascript
const cave = ['#######',
              '#P..#L#',
              '#.#...#',
              '#######'];
console.log(cave[1][5]);   // L: Zeile 1, Spalte 5
const pip = [1, 1];        // eine Position: [x, y]
```
<!-- /language -->

Erst kommt die Zeile, `cave[y]`, dann in ihr die Spalte, `cave[y][x]`.

_task(find)

_task(neighbors)

_task(distance)

_task(way)

## Der kürzeste Weg

Zwei neue Fragen geben Pip die Karte, die der Wichtel auf der Suche nach Bello
gezeichnet hat, und sagen, wohin Pip schaut.
Wie die anderen Fragen kosten sie keinen Schritt.

<!-- language: ruby -->
<table class='table'>
<tr><th>Befehl</th><th>Was Pip bekommt</th></tr>
<tr><td><code>cave_map</code></td><td>die Karte, wie sie gerade ist, als Liste von Zeilen, mit Pip und dem Mampfer, wo sie jetzt stehen</td></tr>
<tr><td><code>facing</code></td><td>wohin Pip schaut: <code>"east"</code>, <code>"north"</code>, <code>"west"</code> oder <code>"south"</code></td></tr>
</table>
<!-- /language -->
<!-- language: python -->
<table class='table'>
<tr><th>Befehl</th><th>Was Pip bekommt</th></tr>
<tr><td><code>cave_map()</code></td><td>die Karte, wie sie gerade ist, als Liste von Zeilen, mit Pip und dem Mampfer, wo sie jetzt stehen</td></tr>
<tr><td><code>facing()</code></td><td>wohin Pip schaut: <code>"east"</code>, <code>"north"</code>, <code>"west"</code> oder <code>"south"</code></td></tr>
</table>
<!-- /language -->
<!-- language: javascript -->
<table class='table'>
<tr><th>Befehl</th><th>Was Pip bekommt</th></tr>
<tr><td><code>caveMap()</code></td><td>die Karte, wie sie gerade ist, als Liste von Zeilen, mit Pip und dem Mampfer, wo sie jetzt stehen</td></tr>
<tr><td><code>facing()</code></td><td>wohin Pip schaut: <code>"east"</code>, <code>"north"</code>, <code>"west"</code> oder <code>"south"</code></td></tr>
</table>
<!-- /language -->

_task(face)

<div class='pip-wichtel'>
»Die letzte Lampe«, sagt der Wichtel und zeigt auf seine Karte. »Dahinter
gehört der Herzstein hin. Der Mampfer ist so schnell wie du. Ein Umweg, und er
hat dich.«
</div>

_task(escape)

<!-- after: escape -->
<div class='pip-wichtel'>
Die letzte Lampe flammt auf, und der Mampfer bleibt am Rand ihres Lichts
stehen. Pip legt den Herzstein auf den Boden, dort, wo das Licht aufhört, und
schiebt ihn ins Dunkle. Er hört auf zu summen. Der Mampfer schnuppert daran,
schiebt ihn mit der Nase in seine Höhle und rollt sich darum zusammen wie Bello
um einen Knochen. Von Pip will er nichts mehr wissen. »Er wollte nur seinen
Stein zurück«, sagt der Wichtel. »Und jetzt leise nach Hause.«
</div>
<!-- /after -->

<!-- extra -->
## Extras

Zwei Katas, die nicht mitzählen.

_task(next-step)

_task(steps)

<p class='text-secondary small'>Den Mampfer hat 0x72 gezeichnet, als
»big demon« im <a href='https://0x72.itch.io/dungeontileset-ii'>16x16
DungeonTileset II</a> (CC0); er war schon das Monster der alten Hackschule.</p>
