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
- `insert(i, wert)` fügt einen Wert an Index \\(i\\) ein, alles ab \\(i\\) rückt einen Index weiter,
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

```text
insert(i, wert):
    wenn i < 0 oder i > size: Fehler
    wenn size = Kapazität: vergrößern
    für j von size - 1 abwärts bis i:
        zellen[j + 1] ← zellen[j]
    zellen[i] ← wert
    size ← size + 1

remove(i):
    wenn i < 0 oder i ≥ size: Fehler
    wert ← zellen[i]
    für j von i bis size - 2:
        zellen[j] ← zellen[j + 1]
    size ← size - 1
    gib wert zurück
```

### Vergrößern

Ist das Array voll, kommt ein neues, **doppelt** so großes, und alle Werte werden hinüberkopiert. Das ist teuer, passiert aber selten: Nach dem Kopieren von 4 Werten ist Platz für 4 weitere, nach dem Kopieren von 8 für 8 weitere. Auf jeden angehängten Wert kommt so im Mittel weniger als eine Kopie. Wer das Array stattdessen jedes Mal nur um eine Zelle vergrößert, kopiert bei jedem Anhängen alles.

Probier es mit einem Array mit vier Zellen und den Aufrufen der [Data Structures Sandbox](/sandbox-datenstrukturen) aus, dann noch einem fünften Wert vorn und einem `remove`:

<div class='algo-stepper' data-algo='array' data-capacity='4' data-ops='insert(0, 7), insert(1, 3), insert(1, 5), insert(3, 9), get(2), insert(0, 4), remove(1)'></div>

**Typische Fehler**

- Beim Rücken von vorn anfangen: Dann steht am Ende überall derselbe Wert.
- `size` nicht ändern oder an der falschen Stelle: Der Wert ist im Array, aber nicht in der Liste.
- Nach `remove` die letzte Zelle nicht leeren: Bei Zahlen ist das egal, bei Objekten hält die Liste so einen Wert fest, den es nicht mehr gibt.
<!-- /part -->

<!-- part: linked-list -->
<!-- short: Linked List: Knoten mit Wert und next; insert und remove biegen Referenzen um, get muss vom Anfang laufen. -->
## Die Linked List

Eine **Linked List** braucht kein großes Array. Jeder Wert steckt in einem eigenen **Knoten**, und jeder Knoten kennt seinen Nachfolger: Er hat ein Feld `wert` und ein Feld `next`, das auf den nächsten Knoten zeigt oder auf nichts. Die Liste selbst merkt sich nur den ersten Knoten, `head`, und meist size.

### Laufen

Wo der Knoten mit Index \\(i\\) liegt, weiß niemand. Man findet ihn nur, indem man bei `head` anfängt und \\(i\\)-mal `next` folgt. Darum braucht `get(i)` bei einer Linked List bis zu size Schritte.

```text
knoten_an(i):
    k ← head
    i-mal wiederholen:
        k ← k.next
    gib k zurück
```

### insert und remove

Zum Einfügen läuft man zum Knoten **vor** dem Index, dem Vorgänger. Dann zeigt der neue Knoten auf den bisherigen Nachfolger, und der Vorgänger zeigt auf den neuen Knoten. Die Reihenfolge ist wichtig: Erst den neuen Knoten verbinden, dann den Vorgänger umbiegen, sonst ist der Rest der Liste verloren. Am Anfang der Liste gibt es keinen Vorgänger, dort wird `head` umgebogen.

```text
insert(i, wert):
    wenn i < 0 oder i > size: Fehler
    neu ← Knoten mit wert
    wenn i = 0:
        neu.next ← head
        head ← neu
    sonst:
        vor ← knoten_an(i - 1)
        neu.next ← vor.next
        vor.next ← neu
    size ← size + 1

remove(i):
    wenn i < 0 oder i ≥ size: Fehler
    wenn i = 0:
        wert ← head.wert
        head ← head.next
    sonst:
        vor ← knoten_an(i - 1)
        wert ← vor.next.wert
        vor.next ← vor.next.next
    size ← size - 1
    gib wert zurück
```

Dieselben Aufrufe wie oben, jetzt auf einer Linked List. Die Knoten liegen irgendwo im Speicher; gezeichnet sind sie in der Reihenfolge der Liste:

<div class='algo-stepper' data-algo='linked' data-ops='insert(0, 7), insert(1, 3), insert(1, 5), insert(3, 9), get(2), insert(0, 4), remove(1)'></div>

**Typische Fehler**

- Erst `vor.next ← neu`, dann `neu.next ← vor.next`: Der neue Knoten zeigt auf sich selbst, die Liste wird zum Kreis.
- Den Fall `i = 0` vergessen: Es gibt keinen Vorgänger, `knoten_an(-1)` geht schief.
- Einen Schritt zu weit laufen: Für `insert(i, …)` und `remove(i)` braucht man den Knoten an \\(i - 1\\), nicht an \\(i\\).
- `size` vergessen. Die Sandbox prüft, dass size zur Zahl der Knoten passt.

### Doppelt verkettet

Eine **doppelt verkettete** Linked List gibt jedem Knoten zusätzlich `prev`, den Vorgänger, und merkt sich neben `head` auch `tail`, den letzten Knoten. Dann geht Anhängen und Entfernen an beiden Enden ohne Laufen, und man kann rückwärts laufen. Dafür muss jedes `insert` und `remove` vier Referenzen richtig setzen statt zwei.
<!-- /part -->

<!-- part: sortierte-liste -->
<!-- short: Sortierte Liste: insert sucht die Stelle selbst; im Array mit binärer Suche, in einer Linked List durch Laufen. -->
## Sortierte Listen

Eine **sortierte Liste** hat kein `insert(i, wert)`, sondern nur `insert(wert)`: Sie sucht die Stelle selbst, sodass die Werte immer aufsteigend bleiben.

- **Im Array** findet man die Stelle mit **binärer Suche**: den mittleren Wert ansehen, dann in der linken oder rechten Hälfte weitersuchen. Bei einer Million Werten sind das 20 Vergleiche. Danach muss trotzdem alles dahinter rücken.
- **In einer Linked List** hilft binäre Suche nicht, denn die Mitte findet man nur durch Laufen. Man läuft vom Anfang, bis der nächste Wert größer ist, und hängt den neuen Knoten davor ein.

```text
stelle_finden(zellen, size, wert):
    links ← 0
    rechts ← size
    solange links < rechts:
        mitte ← (links + rechts) / 2, abgerundet
        wenn zellen[mitte] < wert: links ← mitte + 1
        sonst: rechts ← mitte
    gib links zurück
```

`stelle_finden` gibt den ersten Index zurück, an dem ein Wert steht, der nicht kleiner ist als `wert`. Genau dort wird eingefügt, und genau dort steht der Wert, wenn man ihn sucht. Ob doppelte Werte erlaubt sind, legt die Aufgabe fest; die Sandbox prüft es.
<!-- /part -->

<!-- part: stack -->
<!-- short: Stack: push legt oben drauf, pop nimmt oben weg (LIFO); mit einem Array hinten, mit Knoten vorn. -->
## Der Stack

Ein **Stack** kann nur an einem Ende arbeiten, oben:

- `push(wert)` legt einen Wert oben drauf,
- `pop()` nimmt den obersten Wert weg und gibt ihn zurück,
- `peek()` zeigt den obersten, ohne ihn wegzunehmen.

Was zuletzt hineinkam, kommt zuerst heraus: **LIFO**, last in, first out. Ein Stack steckt in jedem Programm, das Funktionen aufruft: Jeder Aufruf legt fest, wohin es danach zurückgeht, und `return` nimmt das wieder vom Stack. Die [Tiefensuche](/reference/graphen#tiefensuche) benutzt einen, und jedes »Rückgängig« in einem Editor auch.

Mit einem Array ist das obere Ende das **hintere**: `push` hängt an Index size an, `pop` nimmt den an size − 1. Nichts muss rücken. Mit Knoten ist das obere Ende der **Anfang**: `push` ist `insert(0, …)`, `pop` ist `remove(0)`, und auch da muss nichts laufen.

```text
push(wert):
    wenn size = Kapazität: voll (oder vergrößern)
    zellen[size] ← wert
    size ← size + 1

pop():
    wenn size = 0: Fehler, der Stack ist leer
    size ← size - 1
    gib zellen[size] zurück
```
<!-- /part -->

<!-- part: queue -->
<!-- short: Queue: enqueue hinten, dequeue vorn (FIFO); mit Knoten head und tail, im Array als Ring Buffer. -->
## Die Queue

Eine **Queue** arbeitet an beiden Enden: `enqueue(wert)` stellt hinten an, `dequeue()` nimmt vorn weg. Was zuerst kam, kommt zuerst dran: **FIFO**, first in, first out. Die [Breitensuche](/reference/graphen#breitensuche) braucht eine, ebenso ein Drucker, der Aufträge der Reihe nach abarbeitet.

Mit Knoten braucht die Queue `head` und `tail`: `enqueue` hängt hinter `tail` an, `dequeue` nimmt `head` weg. Mit einem einfachen Array ginge `dequeue` als `remove(0)`, aber dann rückt bei jedem `dequeue` die ganze Queue.

### Der Ring Buffer

Ein **Ring Buffer** lässt die Werte stehen und bewegt stattdessen den Anfang. Er merkt sich `head`, den Index des vordersten Werts, und `size`. Der nächste freie Platz ist size Zellen hinter head. Läuft die Queue hinten aus dem Array, geht es vorn weiter, als wären die Zellen im Kreis angeordnet: Dafür sorgt der Rest der Division, **mod**.

```text
enqueue(wert):
    wenn size = Kapazität: Fehler, die Queue ist voll
    zellen[(head + size) mod Kapazität] ← wert
    size ← size + 1

dequeue():
    wenn size = 0: Fehler, die Queue ist leer
    wert ← zellen[head]
    head ← (head + 1) mod Kapazität
    size ← size - 1
    gib wert zurück
```

Sechs Zellen, und so viel hinein und heraus, dass die Queue einmal ganz herumläuft und zum Schluss voll ist:

<div class='algo-stepper' data-algo='ring' data-capacity='6' data-ops='enqueue 4, enqueue 8, enqueue 15, dequeue, enqueue 16, dequeue, enqueue 23, enqueue 42, enqueue 7, dequeue, enqueue 1, enqueue 2, enqueue 3'></div>

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
