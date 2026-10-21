<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Listen, Stack und Queue

<p class='abstract'>
Eine Liste speichert Werte in einer Reihenfolge. Wie sie das tut, entscheidet, was schnell geht und was nicht: Eine Array List findet jeden Index sofort und muss beim Einfügen rücken, eine Linked List biegt nur zwei Referenzen um und muss dafür laufen. Stack und Queue sind Listen, die nur an den Enden arbeiten, und eine Queue passt mit einem Ring Buffer in ein festes Array. Hier steht jede Speicherform als Pseudocode und zum Durchklicken.
</p>

<script src='/include/algo_stepper.js'></script>

<!-- part: liste -->
<!-- short: Eine Liste: get, insert und remove an einem Index; dieselbe Schnittstelle, verschiedene Speicherformen. -->
## Was eine Liste kann

Eine **Liste** hält Werte in einer festen Reihenfolge, jeder an einem **Index** von 0 bis size − 1. Die wichtigsten Operationen:

- `get(i)` gibt den Wert an Index \\(i\\) zurück,
- `insert(i, value)` fügt einen Wert an Index \\(i\\) ein, alles ab \\(i\\) rückt einen Index weiter,
- `remove(i)` nimmt den Wert an Index \\(i\\) heraus, alles dahinter rückt nach vorn,
- `size` sagt, wie viele Werte darin sind.

Ein Index außerhalb von 0 bis size − 1 ist ein Fehler, bei `insert` ist zusätzlich size erlaubt (hinten anhängen). Diese Schnittstelle lässt sich auf zwei grundverschiedene Arten bauen, mit einem **Array** oder mit verketteten **Knoten**. Von außen verhalten sich beide gleich, innen kosten dieselben Aufrufe ganz verschieden viel.
<!-- /part -->

<!-- part: array-list -->
<!-- short: Array List: Werte in den ersten size Zellen eines Arrays; get sofort, insert und remove rücken, ein volles Array wird verdoppelt. -->
## Die Array List

Eine **Array List** legt ihre Werte in die ersten size Zellen eines Arrays. Das Array hat eine **Kapazität**, meist mehr Zellen als gerade gebraucht.

### get

Die Zelle \\(i\\) liegt an einer festen Stelle im Speicher: Anfang des Arrays plus \\(i\\)-mal die Größe einer Zelle. `get(i)` ist deshalb sofort fertig, egal wie lang die Liste ist.

### insert und remove

Soll ein Wert an Index \\(i\\) hinein, muss dort erst Platz werden: Jeder Wert ab \\(i\\) rückt eine Zelle nach rechts, und zwar **von hinten** angefangen, sonst überschreibt der erste Schritt den nächsten Wert. `remove` macht es umgekehrt und rückt von vorn nach links.

```pseudocode
insert(i, value):
    if i < 0 or i > size: error
    if size = capacity: grow
    for j from size - 1 down to i:
        cells[j + 1] ← cells[j]
    cells[i] ← value
    size ← size + 1

remove(i):
    if i < 0 or i ≥ size: error
    value ← cells[i]
    for j from i to size - 2:
        cells[j] ← cells[j + 1]
    size ← size - 1
    return value
```

### Vergrößern

Ist das Array voll, kommt ein neues, **doppelt** so großes, und alle Werte werden hinüberkopiert. Das ist teuer, passiert aber selten: Nach dem Kopieren von 4 Werten ist Platz für 4 weitere, nach dem Kopieren von 8 für 8 weitere. Auf jeden angehängten Wert kommt so im Mittel weniger als eine Kopie. Wer das Array stattdessen jedes Mal nur um eine Zelle vergrößert, kopiert bei jedem Anhängen alles.

Probier es mit einem Array mit vier Zellen und den Aufrufen der [Data Structures Sandbox](/sandbox-datenstrukturen) aus, dann noch einem fünften Wert vorn und einem `remove`:

<div class='algo-stepper' data-title='Array: einfügen und entfernen, das Array wächst' data-algo='array' data-capacity='4' data-ops='insert(0, 7), insert(1, 3), insert(1, 5), insert(3, 9), get(2), insert(0, 4), remove(1)'></div>

**Typische Fehler**

- Beim Rücken von vorn anfangen: Dann steht am Ende überall derselbe Wert.
- `size` nicht ändern oder an der falschen Stelle: Der Wert ist im Array, aber nicht in der Liste.
- Nach `remove` die letzte Zelle nicht leeren: Bei Zahlen ist das egal, bei Objekten hält die Liste so einen Wert fest, den es nicht mehr gibt.
<!-- /part -->

<!-- part: linked-list -->
<!-- short: Linked List: Knoten mit Wert und next; insert und remove biegen Referenzen um, get muss vom Anfang laufen. -->
## Die Linked List

Eine **Linked List** braucht kein großes Array. Jeder Wert steckt in einem eigenen **Knoten**, und jeder Knoten kennt seinen Nachfolger: Er hat ein Feld `value` und ein Feld `next`, das auf den nächsten Knoten zeigt oder auf nichts. Die Liste selbst merkt sich nur den ersten Knoten, `head`, und meist size.

### Laufen

Wo der Knoten mit Index \\(i\\) liegt, weiß niemand. Man findet ihn nur, indem man bei `head` anfängt und \\(i\\)-mal `next` folgt. Darum braucht `get(i)` bei einer Linked List bis zu size Schritte.

```pseudocode
node_at(i):
    k ← head
    repeat i times:
        k ← k.next
    return k
```

### insert und remove

Zum Einfügen läuft man zum Knoten **vor** dem Index, dem Vorgänger. Dann zeigt der neue Knoten auf den bisherigen Nachfolger, und der Vorgänger zeigt auf den neuen Knoten. Die Reihenfolge ist wichtig: Erst den neuen Knoten verbinden, dann den Vorgänger umbiegen, sonst ist der Rest der Liste verloren. Am Anfang der Liste gibt es keinen Vorgänger, dort wird `head` umgebogen.

```pseudocode
insert(i, value):
    if i < 0 or i > size: error
    new ← a node with value
    if i = 0:
        new.next ← head
        head ← new
    else:
        before ← node_at(i - 1)
        new.next ← before.next
        before.next ← new
    size ← size + 1

remove(i):
    if i < 0 or i ≥ size: error
    if i = 0:
        value ← head.value
        head ← head.next
    else:
        before ← node_at(i - 1)
        value ← before.next.value
        before.next ← before.next.next
    size ← size - 1
    return value
```

Dieselben Aufrufe wie oben, jetzt auf einer Linked List. Die Knoten liegen irgendwo im Speicher; gezeichnet sind sie in der Reihenfolge der Liste:

<div class='algo-stepper' data-title='Linked List: einfügen und entfernen über die Zeiger' data-algo='linked' data-ops='insert(0, 7), insert(1, 3), insert(1, 5), insert(3, 9), get(2), insert(0, 4), remove(1)'></div>

**Typische Fehler**

- Erst `before.next ← new`, dann `new.next ← before.next`: Der neue Knoten zeigt auf sich selbst, die Liste wird zum Kreis.
- Den Fall `i = 0` vergessen: Es gibt keinen Vorgänger, `node_at(-1)` geht schief.
- Einen Schritt zu weit laufen: Für `insert(i, …)` und `remove(i)` braucht man den Knoten an \\(i - 1\\), nicht an \\(i\\).
- `size` vergessen. Die Sandbox prüft, dass size zur Zahl der Knoten passt.

### Doppelt verkettet

Eine **doppelt verkettete** Linked List gibt jedem Knoten zusätzlich `prev`, den Vorgänger, und merkt sich neben `head` auch `tail`, den letzten Knoten. Dann geht Anhängen und Entfernen an beiden Enden ohne Laufen, und man kann rückwärts laufen. Dafür muss jedes `insert` und `remove` vier Referenzen richtig setzen statt zwei.
<!-- /part -->

<!-- part: sortierte-liste -->
<!-- short: Sortierte Liste: insert sucht die Stelle selbst; im Array mit binärer Suche, in einer Linked List durch Laufen. -->
## Sortierte Listen

Eine **sortierte Liste** hat kein `insert(i, value)`, sondern nur `insert(value)`: Sie sucht die Stelle selbst, sodass die Werte immer aufsteigend bleiben.

- **Im Array** findet man die Stelle mit **binärer Suche**: den mittleren Wert ansehen, dann in der linken oder rechten Hälfte weitersuchen. Bei einer Million Werten sind das 20 Vergleiche. Danach muss trotzdem alles dahinter rücken.
- **In einer Linked List** hilft binäre Suche nicht, denn die Mitte findet man nur durch Laufen. Man läuft vom Anfang, bis der nächste Wert größer ist, und hängt den neuen Knoten davor ein.

```pseudocode
find_position(cells, size, value):
    left ← 0
    right ← size
    while left < right:
        middle ← (left + right) / 2, rounded down
        if cells[middle] < value: left ← middle + 1
        else: right ← middle
    return left
```

`find_position` gibt den ersten Index zurück, an dem ein Wert steht, der nicht kleiner ist als `value`. Genau dort wird eingefügt, und genau dort steht der Wert, wenn man ihn sucht. Ob doppelte Werte erlaubt sind, legt die Aufgabe fest; die Sandbox prüft es.
<!-- /part -->

<!-- part: stack -->
<!-- short: Stack: push legt oben drauf, pop nimmt oben weg (LIFO); mit einem Array hinten, mit Knoten vorn. -->
## Der Stack

Ein **Stack** kann nur an einem Ende arbeiten, oben:

- `push(value)` legt einen Wert oben drauf,
- `pop()` nimmt den obersten Wert weg und gibt ihn zurück,
- `peek()` zeigt den obersten, ohne ihn wegzunehmen.

Was zuletzt hineinkam, kommt zuerst heraus: **LIFO**, last in, first out. Ein Stack steckt in jedem Programm, das Funktionen aufruft: Jeder Aufruf legt fest, wohin es danach zurückgeht, und `return` nimmt das wieder vom Stack. Die [Tiefensuche](/reference/graphen#tiefensuche) benutzt einen, und jedes »Rückgängig« in einem Editor auch.

Mit einem Array ist das obere Ende das **hintere**: `push` hängt an Index size an, `pop` nimmt den an size − 1. Nichts muss rücken. Mit Knoten ist das obere Ende der **Anfang**: `push` ist `insert(0, …)`, `pop` ist `remove(0)`, und auch da muss nichts laufen.

```pseudocode
push(value):
    if size = capacity: full (oder vergrößern)
    cells[size] ← value
    size ← size + 1

pop():
    if size = 0: error, the stack is empty
    size ← size - 1
    return cells[size]
```
<!-- /part -->

<!-- part: queue -->
<!-- short: Queue: enqueue hinten, dequeue vorn (FIFO); mit Knoten head und tail, im Array als Ring Buffer. -->
## Die Queue

Eine **Queue** arbeitet an beiden Enden: `enqueue(value)` stellt hinten an, `dequeue()` nimmt vorn weg. Was zuerst kam, kommt zuerst dran: **FIFO**, first in, first out. Die [Breitensuche](/reference/graphen#breitensuche) braucht eine, ebenso ein Drucker, der Aufträge der Reihe nach abarbeitet.

Mit Knoten braucht die Queue `head` und `tail`: `enqueue` hängt hinter `tail` an, `dequeue` nimmt `head` weg. Mit einem einfachen Array ginge `dequeue` als `remove(0)`, aber dann rückt bei jedem `dequeue` die ganze Queue.

### Der Ring Buffer

Ein **Ring Buffer** lässt die Werte stehen und bewegt stattdessen den Anfang. Er merkt sich `head`, den Index des vordersten Werts, und `size`. Der nächste freie Platz ist size Zellen hinter head. Läuft die Queue hinten aus dem Array, geht es vorn weiter, als wären die Zellen im Kreis angeordnet: Dafür sorgt der Rest der Division, **mod**.

```pseudocode
enqueue(value):
    if size = capacity: error, the queue is full
    cells[(head + size) mod capacity] ← value
    size ← size + 1

dequeue():
    if size = 0: error, the queue is empty
    value ← cells[head]
    head ← (head + 1) mod capacity
    size ← size - 1
    return value
```

Sechs Zellen, und so viel hinein und heraus, dass die Queue einmal ganz herumläuft und zum Schluss voll ist:

<div class='algo-stepper' data-title='Queue als Ringpuffer: enqueue und dequeue' data-algo='ring' data-capacity='6' data-ops='enqueue 4, enqueue 8, enqueue 15, dequeue, enqueue 16, dequeue, enqueue 23, enqueue 42, enqueue 7, dequeue, enqueue 1, enqueue 2, enqueue 3'></div>

**Typische Fehler**

- Statt size ein zweites Ende `tail` speichern und »voll« mit »leer« verwechseln: Bei beiden ist `head = tail`. Mit size gibt es das Problem nicht.
- `mod` in Sprachen, in denen der Rest negativ werden kann (Java, JavaScript, C): `head - 1` mod 6 ist dort −1. Beim Rückwärtsgehen vorher die Kapazität addieren.
- `dequeue` setzt head weiter, vergisst aber size: Dann ist die Queue scheinbar nie leer.
<!-- /part -->

<!-- part: listen-vergleich -->
<!-- short: Was jede Speicherform kostet: get, insert vorn, hinten und in der Mitte. -->
## Was was kostet

Die Zahl der Schritte für eine Liste mit \\(n\\) Werten. »sofort« heißt: gleich viele Schritte, egal wie lang die Liste ist.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th></th><th>get(i)</th><th>insert vorn</th><th>insert hinten</th><th>insert in der Mitte</th><th>Platz</th></tr></thead>
<tbody>
<tr><td>Array List</td><td>sofort</td><td>\(n\) rücken</td><td>sofort, selten kopieren</td><td>\(n/2\) rücken</td><td>Array, oft halb leer</td></tr>
<tr><td>Linked List</td><td>\(i\) laufen</td><td>sofort</td><td>\(n\) laufen, mit <code>tail</code> sofort</td><td>\(n/2\) laufen</td><td>ein <code>next</code> je Wert</td></tr>
<tr><td>Ring Buffer</td><td>sofort</td><td>sofort</td><td>sofort</td><td>–</td><td>fest, kann voll werden</td></tr>
</tbody>
</table>
</div>

In der Praxis gewinnt die Array List fast immer, auch wenn die Tabelle anders aussieht: Ihre Werte liegen nebeneinander im Speicher, und Prozessoren lesen Nachbarzellen viel schneller als Knoten, die irgendwo verstreut sind. Eine Linked List lohnt sich, wenn man oft an einer Stelle einfügt, an der man ohnehin schon ist.
<!-- /part -->

## Zum Nachschlagen

Die Array List bringen Ruby und Python fertig mit, als `Array` und als `list`; mit `push` (Python: `append`) und `pop` ist sie auch ein Stack. Eine Queue, die vorn schnell herausnimmt, ist in Python `collections.deque`, in Ruby ein `Array` mit `push` und `shift`.

- [Ruby: `Array`](https://docs.ruby-lang.org/en/3.4/Array.html): alle Methoden von Rubys Array List, darunter `insert`, `delete_at`, `push`, `pop` und `shift`. Für sortierte Listen findet [`bsearch_index`](https://docs.ruby-lang.org/en/3.4/Array.html#method-i-bsearch_index) die Stelle mit binärer Suche.
- [Python: Listen als Stack und als Queue](https://docs.python.org/3/tutorial/datastructures.html#using-lists-as-stacks) im Tutorial: was `list` kann, und warum eine Queue besser eine `deque` ist.
- [Python: `collections.deque`](https://docs.python.org/3/library/collections.html#collections.deque): eine Queue, die an beiden Enden schnell einfügt und herausnimmt.
- [Python: `bisect`](https://docs.python.org/3/library/bisect.html): binäre Suche in einer sortierten Liste; `bisect_left` tut dasselbe wie `find_position` oben.
- [VisuAlgo: Linked List, Stack, Queue](https://visualgo.net/en/list): keine offizielle Referenz, aber jede Operation als Animation, auf Englisch.
