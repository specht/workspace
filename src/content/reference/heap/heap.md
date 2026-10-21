<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Heap und Priority Queue

<p class='abstract'>
Eine Priority Queue gibt immer den Eintrag mit der kleinsten Priorität zurück: den nächsten Knoten für Dijkstra, die zwei seltensten Zeichen für Huffman, den nächsten Termin. Ein binärer Heap macht das schnell, mit einem einfachen Array. Hier steht, wie er aufgebaut ist, wie <code>push</code> und <code>pop</code> ihn in Ordnung halten, zum Durchklicken und als Pseudocode, und wie man mit ihm sortiert.
</p>

<script src='/include/algo_stepper.js'></script>

<!-- part: priority-queue -->
<!-- short: Priority Queue: push legt einen Eintrag mit Priorität hinein, pop gibt den mit der kleinsten heraus. -->
## Die Priority Queue

Eine **Priority Queue** kann zwei Dinge:

- **push** legt einen Eintrag mit einer Priorität hinein,
- **pop** nimmt den Eintrag mit der **kleinsten** Priorität heraus und gibt ihn zurück.

Wie sie das macht, ist erst einmal egal. Die einfachste Art, eine Liste, in der `pop` jedes Mal den kleinsten sucht, ist richtig, aber `pop` muss dafür alle Einträge ansehen. Eine sortierte Liste macht `pop` schnell, dafür muss `push` die richtige Stelle finden und alles dahinter verschieben. Ein **Heap** macht beides schnell:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th></th><th>push</th><th>pop</th><th>bei einer Million Einträgen</th></tr></thead>
<tbody>
<tr><td>unsortierte Liste</td><td>schnell</td><td>alle ansehen</td><td>eine Million Vergleiche je <code>pop</code></td></tr>
<tr><td>sortierte Liste</td><td>verschieben</td><td>schnell</td><td>bis zu einer Million Verschiebungen je <code>push</code></td></tr>
<tr><td>binärer Heap</td><td>\(\log_2 n\) Schritte</td><td>\(\log_2 n\) Schritte</td><td>etwa 20 Schritte</td></tr>
</tbody>
</table>
</div>

Priority Queues stecken in Dijkstra und A* ([Graphen und Wege](/reference/graphen)), in Huffman-Codes (die zwei seltensten Bäume zuerst), in jedem Terminkalender und in jedem Betriebssystem, das entscheidet, welches Programm als Nächstes rechnen darf.
<!-- /part -->

## Der binäre Heap

<!-- part: heap -->
<!-- short: Ein Heap ist ein Array, gedacht als Baum: die Kinder von Index i stehen bei 2i + 1 und 2i + 2, kein Kind ist kleiner als sein Elternteil. -->
### Ein Array als Baum

Ein **binärer Heap** ist ein Array, das man sich als Baum denkt. Ganz oben steht der Eintrag an Index 0. Die Kinder des Eintrags an Index \\(i\\) stehen an den Indizes \\(2i + 1\\) und \\(2i + 2\\), sein Elternteil an \\(\lfloor (i - 1) / 2 \rfloor\\):

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Index</th><th>0</th><th>1</th><th>2</th><th>3</th><th>4</th><th>5</th><th>6</th></tr></thead>
<tbody>
<tr><td>Kinder</td><td>1, 2</td><td>3, 4</td><td>5, 6</td><td>7, 8</td><td>9, 10</td><td>11, 12</td><td>13, 14</td></tr>
<tr><td>Elternteil</td><td>–</td><td>0</td><td>0</td><td>1</td><td>1</td><td>2</td><td>2</td></tr>
</tbody>
</table>
</div>

Der Baum ist immer von oben nach unten und in jeder Zeile von links nach rechts gefüllt, ohne Lücken. Deshalb braucht er keine Zeiger: Das Array ist der Baum.

### Die Heap-Eigenschaft

Für jeden Eintrag gilt: **Kein Kind ist kleiner als sein Elternteil.** Daraus folgt, dass ganz oben, an Index 0, der kleinste Eintrag steht. Geschwister sind dabei nicht geordnet, und eine Zeile weiter unten kann kleinere Zahlen haben als eine weiter oben, nur eben nicht unter demselben Elternteil. Ein Heap ist also **nicht** sortiert, er ist gerade so geordnet, dass man den kleinsten Eintrag sofort findet.
<!-- /part -->

<!-- part: heap-push -->
<!-- short: push: hinten anhängen, dann mit dem Elternteil tauschen, solange es größer ist (nach oben sickern). -->
## push

Ein neuer Eintrag kommt **hinten** an das Array, an die erste freie Stelle des Baums. Dort verletzt er vielleicht die Heap-Eigenschaft: Er ist kleiner als sein Elternteil. Dann tauscht er mit ihm den Platz, und das so lange, bis sein Elternteil nicht mehr größer ist oder er oben angekommen ist. Er **sickert nach oben**.

```pseudocode
push(heap, entry):
    append entry to the back of heap
    i ← the last index of heap
    while i > 0:
        parent ← (i - 1) / 2, rounded down
        if heap[parent] ≤ heap[i]: done
        swap heap[parent] and heap[i]
        i ← parent
```

Probier es aus mit 5, 3, 8, 1, 4 und 2. Oben steht der Heap als Baum, darunter dasselbe als Array, mit den Indizes klein daneben:

<div class='algo-stepper' data-title='Heap: push' data-algo='heap' data-ops='push 5, push 3, push 8, push 1, push 4, push 2'></div>

Ein Eintrag steigt höchstens so viele Stufen auf, wie der Baum hoch ist: bei \\(n\\) Einträgen etwa \\(\log_2 n\\), bei einer Million also 20.
<!-- /part -->

<!-- part: heap-pop -->
<!-- short: pop: den Eintrag an Index 0 herausnehmen, den letzten an seine Stelle setzen, dann mit dem kleineren Kind tauschen, solange eins kleiner ist (nach unten sickern). -->
## pop

Der kleinste Eintrag steht an Index 0. Ihn herauszunehmen, reißt ein Loch an der Spitze. Also kommt der **letzte** Eintrag des Arrays dorthin, und das Array wird um eins kürzer. Der neue an der Spitze ist meist zu groß. Er tauscht mit dem **kleineren** seiner beiden Kinder, solange dieses kleiner ist als er. Er **sickert nach unten**.

```pseudocode
pop(heap):
    if heap empty: nothing to pop
    top ← heap[0]
    last ← take the last entry from heap
    if heap not empty:
        heap[0] ← last
        i ← 0
        repeat:
            min ← i
            for each child in (2i + 1, 2i + 2):
                if child < length of heap and heap[child] < heap[min]:
                    min ← child
            if min == i: done
            swap heap[min] and heap[i]
            i ← min
    return top
```

Derselbe Heap aus 5, 3, 8, 1, 4 und 2, und `pop` holt dreimal den kleinsten heraus:

<div class='algo-stepper' data-title='Heap: pop' data-algo='heap' data-setup='push 5, push 3, push 8, push 1, push 4, push 2' data-ops='pop, pop, pop'></div>

**Typische Fehler**

- Mit dem **größeren** Kind tauschen oder mit dem ersten, das kleiner ist. Dann steht das kleinere Geschwister über dem größeren, und die Heap-Eigenschaft ist kaputt.
- Den letzten Eintrag herausnehmen und an Index 0 schreiben, ohne zu prüfen, ob der Heap danach leer ist. Bei einem Heap mit nur einem Eintrag steht der gerade herausgenommene dann wieder darin.
- Prüfen, ob ein Kind existiert, ohne die Länge des Arrays: `2i + 1` kann hinter dem Ende liegen.
- Prioritäten und Werte trennen: Liegen Paare aus Priorität und Wert im Heap, wird nur die Priorität verglichen, sonst entscheidet bei gleicher Priorität der Wert, und der lässt sich vielleicht gar nicht vergleichen.
<!-- /part -->

<!-- part: heapsort -->
<!-- short: Heapsort: alles in einen Heap, dann immer wieder pop; n log n Schritte. -->
## Sortieren mit einem Heap

Legt man alle Zahlen mit `push` in einen Heap und holt sie mit `pop` wieder heraus, kommen sie sortiert heraus, die kleinste zuerst. Das ist **Heapsort**. Es braucht für \\(n\\) Zahlen \\(n\\)-mal `push` und \\(n\\)-mal `pop`, jedes höchstens \\(\log_2 n\\) Schritte: zusammen \\(O(n \log n)\\), so schnell wie die besten Sortierverfahren, auch im schlechtesten Fall.

Ohne zweites Array geht es auch: Man baut den Heap im Array selbst auf und legt den jeweils größten Eintrag (mit einem Heap, in dem kein Kind *größer* ist als sein Elternteil) ans Ende, in den Teil, der schon sortiert ist. In der [Sorting Sandbox](/sandbox-sortieren) kannst du Heapsort selbst programmieren und mit den anderen Verfahren vergleichen.
<!-- /part -->

## Zum Nachschlagen

Python bringt genau diesen Heap mit: `heapq` arbeitet auf einer gewöhnlichen `list`, die Kinder von Index \\(k\\) stehen bei \\(2k + 1\\) und \\(2k + 2\\), der kleinste Eintrag an Index 0. Ruby hat weder einen Heap noch eine Priority Queue; dort baust du ihn wie oben auf einem `Array`.

- [Python: `heapq`](https://docs.python.org/3/library/heapq.html): `heappush`, `heappop` und `heapify` auf einer Liste. Der Abschnitt [Priority Queue Implementation Notes](https://docs.python.org/3/library/heapq.html#priority-queue-implementation-notes) zeigt, wie man Priorität und Wert trennt.
- [Ruby: `Array`](https://docs.ruby-lang.org/en/3.4/Array.html): das Array, auf dem du den Heap baust, mit `push`, `pop` und dem Zugriff über den Index.
- [VisuAlgo: Binary Heap](https://visualgo.net/en/heap): `push`, `pop` und Heapsort als Animation, auf Englisch. Dort steht der *größte* Eintrag oben, und das Array beginnt bei Index 1: Die Kinder von \\(i\\) stehen bei \\(2i\\) und \\(2i + 1\\).
