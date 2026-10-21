<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Suchbäume

<p class='abstract'>
Ein binärer Suchbaum hält Werte so geordnet, dass Suchen, Einfügen und Entfernen nur einen Weg von oben nach unten brauchen: links das Kleinere, rechts das Größere. Damit das schnell bleibt, darf der Baum nicht zu einer langen Kette werden; ein AVL-Baum verhindert das mit Rotationen. Hier steht jeder Schritt als Pseudocode und zum Durchklicken, dazu die drei Arten, einen Baum zu durchlaufen.
</p>

<script src='/include/algo_stepper.js'></script>

<!-- part: suchbaum -->
<!-- short: Binärer Suchbaum: jeder Knoten hat höchstens zwei Kinder, links nur kleinere, rechts nur größere Werte. -->
## Der binäre Suchbaum

Ein **Baum** besteht aus Knoten. Ganz oben steht die **Wurzel**, jeder Knoten hat höchstens zwei **Kinder**, ein linkes und ein rechtes, und jeder Knoten außer der Wurzel genau ein Elternteil. Ein Knoten ohne Kinder heißt **Blatt**. Jedes Kind ist selbst wieder die Wurzel eines kleineren Baums, seines **Teilbaums**: Darum lässt sich fast alles an Bäumen rekursiv schreiben.

Ein **binärer Suchbaum** hat eine Regel: Für jeden Knoten stehen in seinem linken Teilbaum **nur kleinere** Werte, in seinem rechten **nur größere**. Nicht nur das direkte Kind, der ganze Teilbaum.

### Suchen

Die Regel sagt bei jedem Knoten, wohin es weitergeht: Ist der gesuchte Wert kleiner, nach links, ist er größer, nach rechts. Endet der Weg an einer leeren Stelle, ist der Wert nicht im Baum.

```pseudocode
find(node, value):
    if node == null: not found
    if value == node.value: found
    if value < node.value: return find(node.left, value)
    else: return find(node.right, value)
```

### Einfügen

Einfügen ist Suchen: Man sucht den Wert, und an der leeren Stelle, an der die Suche endet, kommt der neue Knoten hin. Der Baum wächst immer unten.

```pseudocode
insert(node, value):
    if node == null: return a new node with value
    if value < node.value: node.left ← insert(node.left, value)
    if value > node.value: node.right ← insert(node.right, value)
    return node
```

Dass `insert` den Knoten zurückgibt und der Aufrufer ihn wieder einträgt, ist der Trick: So braucht kein Knoten sein Elternteil zu kennen, und die leere Wurzel ist kein Sonderfall (`root ← insert(root, value)`).

Acht Werte nacheinander:

<div class='algo-stepper' data-title='Suchbaum: einfügen' data-algo='bst' data-ops='insert 50, insert 30, insert 70, insert 20, insert 40, insert 60, insert 80, insert 35'></div>

Und im fertigen Baum zwei Suchen, eine erfolgreich und eine nicht:

<div class='algo-stepper' data-title='Suchbaum: suchen' data-algo='bst' data-setup='insert 50, insert 30, insert 70, insert 20, insert 40, insert 60, insert 80, insert 35' data-ops='find 35, find 65'></div>
<!-- /part -->

<!-- part: suchbaum-entfernen -->
<!-- short: Entfernen: ein Blatt fällt weg, ein Knoten mit einem Kind wird durch das Kind ersetzt, einer mit zwei Kindern durch den nächstgrößeren Wert. -->
## Aus dem Suchbaum entfernen

Ein Knoten wird zuerst gesucht. Dann gibt es drei Fälle:

1. **Keine Kinder:** Er fällt einfach weg.
2. **Ein Kind:** Das Kind rückt mit seinem ganzen Teilbaum an seine Stelle. Die Regel bleibt erfüllt, denn der ganze Teilbaum lag schon auf der richtigen Seite.
3. **Zwei Kinder:** Hier passt kein Kind an seine Stelle. Stattdessen kommt der **nächstgrößere** Wert dorthin: der kleinste im rechten Teilbaum, einmal nach rechts und dann links, so weit es geht. Er ist größer als alles links und kleiner als alles andere rechts. Sein alter Knoten hat kein linkes Kind (sonst wäre er nicht der kleinste) und ist damit Fall 1 oder 2.

```pseudocode
remove(node, value):
    if node == null: return null
    if value < node.value: node.left ← remove(node.left, value)
    else if value > node.value: node.right ← remove(node.right, value)
    else:
        if node.left == null: return node.right
        if node.right == null: return node.left
        successor ← node.right
        while successor.left != null: successor ← successor.left
        node.value ← successor.value
        node.right ← remove(node.right, successor.value)
    return node
```

Derselbe Baum, dann alle drei Fälle nacheinander: 20 ist ein Blatt, 40 hat ein Kind, 50 hat zwei:

<div class='algo-stepper' data-title='Suchbaum: entfernen' data-algo='bst' data-setup='insert 50, insert 30, insert 70, insert 20, insert 40, insert 60, insert 80, insert 35' data-ops='remove 20, remove 40, remove 50'></div>

**Typische Fehler**

- Im Fall 3 den nächstgrößeren Wert kopieren, aber seinen alten Knoten nicht entfernen: Der Wert steht dann zweimal im Baum.
- Den Rückgabewert von `remove` nicht eintragen: Der Knoten ist »entfernt«, hängt aber noch am Elternteil.
- Statt des nächstgrößeren das rechte Kind nach oben holen: Dann hat es plötzlich drei Teilbäume, und einer geht verloren.
<!-- /part -->

<!-- part: traversierung -->
<!-- short: Einen Baum durchlaufen: inorder (sortiert), preorder (Knoten zuerst), postorder (Knoten zuletzt). -->
## Einen Baum durchlaufen

Um jeden Knoten einmal zu besuchen, etwa zum Ausgeben oder Zählen, geht man rekursiv in beide Teilbäume. Die drei Arten unterscheiden sich nur darin, wann der Knoten selbst drankommt:

```pseudocode
inorder(node):
    if node == null: done
    inorder(node.left)
    visit node
    inorder(node.right)
```

- **Inorder** (links, Knoten, rechts) besucht die Werte eines Suchbaums **sortiert**. Für den Baum oben: 20, 30, 35, 40, 50, 60, 70, 80.
- **Preorder** (Knoten, links, rechts) besucht jeden Knoten vor seinen Kindern: 50, 30, 20, 40, 35, 70, 60, 80. Wer die Werte in dieser Reihenfolge in einen leeren Suchbaum einfügt, bekommt genau denselben Baum zurück, deshalb speichert man Bäume so.
- **Postorder** (links, rechts, Knoten) besucht jeden Knoten nach seinen Kindern: 20, 35, 40, 30, 60, 80, 70, 50. So berechnet man, was von den Teilbäumen abhängt, etwa die Höhe, oder gibt einen Baum frei.

Die **Höhe** eines Baums ist die Zahl der Knoten auf dem längsten Weg von der Wurzel nach unten, ein leerer Baum hat die Höhe 0:

```pseudocode
height(node):
    if node == null: return 0
    return 1 + the larger of height(node.left) and height(node.right)
```
<!-- /part -->

<!-- part: avl -->
<!-- short: AVL-Baum: die Höhen der Teilbäume unterscheiden sich höchstens um 1; nach dem Einfügen stellen Rotationen (LL, RR, LR, RL) das wieder her. -->
## Ausgeglichene Bäume: AVL

Suchen, Einfügen und Entfernen laufen einen Weg von der Wurzel nach unten, sie brauchen also so viele Schritte, wie der Baum hoch ist. Ein gut gemischter Baum mit einer Million Werten ist etwa 20 Ebenen hoch. Kommen die Werte aber sortiert an, hängt jeder rechts am vorigen, und der Baum wird eine Kette:

<div class='algo-stepper' data-title='Suchbaum: sortiert einfügen' data-algo='bst' data-ops='insert 10, insert 20, insert 30, insert 40, insert 50, insert 60'></div>

Das ist eine Linked List mit Umwegen: Bei einer Million Werten braucht die Suche eine Million Schritte.

### Die AVL-Regel

Ein **AVL-Baum** (nach Adelson-Velski und Landis) hält sich selbst ausgeglichen. Jeder Knoten speichert seine Höhe, und für jeden Knoten gilt: Die Höhen seines linken und rechten Teilbaums unterscheiden sich **höchstens um 1**. Ein AVL-Baum mit \\(n\\) Werten ist höchstens etwa \\(1{,}44 \log_2 n\\) hoch.

Nach jedem Einfügen geht man den Weg zurück nach oben, berechnet die Höhen neu und prüft die Regel. Der erste Knoten, bei dem sie verletzt ist, wird mit einer **Rotation** repariert.

### Rotationen

Eine **Rotation nach rechts** um einen Knoten \\(y\\) mit linkem Kind \\(x\\) hebt \\(x\\) nach oben: \\(y\\) wird das rechte Kind von \\(x\\), und der rechte Teilbaum von \\(x\\) wechselt als linker Teilbaum zu \\(y\\). Die Suchregel bleibt erfüllt, denn dieser Teilbaum liegt zwischen \\(x\\) und \\(y\\). Die Rotation nach links ist dasselbe spiegelverkehrt.

```pseudocode
rotate_right(y):
    x ← y.left
    y.left ← x.right
    x.right ← y
    update the height of y, then of x
    return x
```

Welche Rotation nötig ist, hängt davon ab, wo der neue Wert gelandet ist, von dem zu hohen Knoten aus gesehen:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Fall</th><th>der neue Wert liegt</th><th>Reparatur</th></tr></thead>
<tbody>
<tr><td>LL</td><td>links vom linken Kind</td><td>rechts rotieren</td></tr>
<tr><td>RR</td><td>rechts vom rechten Kind</td><td>links rotieren</td></tr>
<tr><td>LR</td><td>rechts vom linken Kind</td><td>erst das linke Kind links rotieren, dann rechts rotieren</td></tr>
<tr><td>RL</td><td>links vom rechten Kind</td><td>erst das rechte Kind rechts rotieren, dann links rotieren</td></tr>
</tbody>
</table>
</div>

Diese acht Werte lösen alle vier Fälle aus, LL, RR, RL und LR in dieser Reihenfolge. Klein neben jedem Knoten steht seine Höhe:

<div class='algo-stepper' data-title='AVL-Baum: einfügen' data-algo='avl' data-ops='insert 50, insert 40, insert 30, insert 60, insert 70, insert 45, insert 20, insert 25'></div>

### Entfernen

Entfernt wird wie im Suchbaum. Danach kann ein Teilbaum um eins zu niedrig sein, und das zeigt sich auf dem Weg zurück nach oben: Jeder Aufruf berechnet die Höhe seines Knotens neu und repariert ihn, wenn die Regel verletzt ist. Welche Rotation nötig ist, sagt hier nicht der entfernte Wert, sondern das **schwerere Kind**: Ist sein innerer Teilbaum höher als sein äußerer, braucht es die Doppelrotation.

Anders als beim Einfügen ist mit einer Rotation nicht unbedingt Schluss. Die Rotation macht den Teilbaum um eins niedriger, und dann kann der Knoten darüber schief sein, bis hinauf zur Wurzel.

```pseudocode
remove(node, value):
    (wie im Suchbaum)
    update the height of node
    return rebalance(node)

rebalance(node):
    if height(node.left) - height(node.right) > 1:
        if height(node.left.left) < height(node.left.right):
            node.left ← rotate_left(node.left)
        return rotate_right(node)
    if height(node.right) - height(node.left) > 1:
        if height(node.right.right) < height(node.right.left):
            node.right ← rotate_right(node.right)
        return rotate_left(node)
    return node
```

Hier löst schon das Entfernen der 50 zwei Reparaturen aus: erst eine Doppelrotation bei 60, dann eine Rotation bei der Wurzel:

<div class='algo-stepper' data-title='AVL-Baum: entfernen' data-algo='avl' data-setup='insert 40, insert 20, insert 60, insert 10, insert 30, insert 50, insert 70, insert 5, insert 15, insert 25, insert 35, insert 65, insert 3' data-ops='remove 50'></div>

**Typische Fehler**

- Die Höhen in der falschen Reihenfolge neu berechnen: Erst der Knoten, der nach unten gerutscht ist, dann der, der jetzt oben ist.
- Den neuen obersten Knoten nicht zurückgeben und eintragen: Die Rotation passiert, aber der Elternteil zeigt noch auf den alten.
- LR und RL mit einer einzigen Rotation lösen: Dann ist der Baum danach genauso schief, nur zur anderen Seite.
- Die Höhe eines leeren Teilbaums nicht als 0 behandeln: Ohne eine kleine Funktion `height(node)`, die bei leer 0 liefert, stürzt jede Rechnung an einem Blatt ab.
- Nach dem Entfernen beim ersten reparierten Knoten aufhören: Anders als beim Einfügen kann auch darüber noch eine Rotation nötig sein.
<!-- /part -->

## Zum Nachschlagen

Weder Ruby noch Python bringen einen Suchbaum mit. Sollen Schlüssel sortiert bleiben, nimmt man dort meist eine [sortierte Liste](/reference/listen#sortierte-liste) mit binärer Suche; ist die Reihenfolge egal, eine [Hash Table](/reference/hash-tables) (`Hash`, `dict`).

- [Python: `bisect`](https://docs.python.org/3/library/bisect.html): binäre Suche und Einfügen in einer sortierten Liste.
- [Ruby: `Array#bsearch`](https://docs.ruby-lang.org/en/3.4/Array.html#method-i-bsearch): binäre Suche in einem sortierten Array.
- [VisuAlgo: Binary Search Tree, AVL Tree](https://visualgo.net/en/bst): keine offizielle Referenz; Suchen, Einfügen, Entfernen und die Rotationen als Animation, auf Englisch.
- [Wikipedia: AVL-Baum](https://de.wikipedia.org/wiki/AVL-Baum): keine offizielle Referenz; mehr über die Höhe, das Entfernen mit Rotationen und die Doppelrotationen.
