<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Hash Tables

<p class='abstract'>
Eine Hash Table findet einen Schlüssel, ohne zu suchen: Eine Hashfunktion rechnet aus, in welchem Bucket er liegen muss. Weil sich zwei Schlüssel einen Bucket teilen können, braucht jede Hash Table einen Plan für Kollisionen, eine Liste je Bucket oder den nächsten freien Platz. Hier steht, wie beide arbeiten, was der Füllgrad damit zu tun hat und wann die Tabelle wächst.
</p>

<script src='/include/algo_stepper.js'></script>

<!-- part: hash-funktion -->
<!-- short: Hashfunktion: macht aus einem Schlüssel einen Bucket-Index; gleiche Schlüssel, gleicher Index. -->
## Hashfunktion und Buckets

Ein Array findet den Wert an Index 5 sofort, eine Liste von Namen muss man durchsuchen. Eine **Hash Table** verbindet beides: Sie hat ein Array von \\(m\\) **Buckets**, und eine **Hashfunktion** rechnet aus jedem Schlüssel einen Index von 0 bis \\(m - 1\\) aus. Dort wird gespeichert, und dort wird später gesucht.

Für ganze Zahlen ist die einfachste Hashfunktion der Rest der Division:

$$ h(k) = k \bmod m $$

Bei 8 Buckets landet 13 in Bucket 5, 21 auch in Bucket 5, 6 in Bucket 6.

### Was eine gute Hashfunktion kann

- **Gleiche Schlüssel, gleicher Bucket**, jedes Mal. Sonst findet man nichts wieder.
- **Verschiedene Schlüssel möglichst gleichmäßig verteilt.** Bei \\(k \bmod 8\\) landen alle geraden Zahlen in geraden Buckets, die Hälfte der Tabelle bleibt leer, wenn nur gerade Schlüssel kommen. Eine Primzahl als Bucketzahl, etwa 7 oder 11, verteilt solche Muster besser.
- **Schnell** zu berechnen.

Für Strings rechnet man aus allen Zeichen eine Zahl, zum Beispiel so, dass jedes Zeichen den bisherigen Wert mal 31 plus seinen Code ergibt, und nimmt davon den Rest. Wer nur das erste Zeichen nimmt, bekommt für »Anna«, »Alex« und »Ali« denselben Bucket.

**Negative Schlüssel.** In Java, JavaScript und C ist der Rest einer negativen Zahl negativ: `-7 % 8` ergibt −7, kein gültiger Index. Python und Ruby rechnen 1. Wer sicher gehen will, rechnet `((k % m) + m) % m`.
<!-- /part -->

<!-- part: hash-chaining -->
<!-- short: Chaining: jeder Bucket hält eine Liste; eine Kollision hängt den Schlüssel an die Liste seines Buckets. -->
## Kollisionen: Chaining

Zwei Schlüssel im selben Bucket sind eine **Kollision**. Sie lassen sich nicht vermeiden: Es gibt mehr mögliche Schlüssel als Buckets. Beim **Chaining** (separate chaining) hält jeder Bucket eine kleine Liste, und alle Schlüssel, die dort landen, kommen in diese Liste.

```text
insert(k):
    b ← h(k)
    wenn k schon in buckets[b]: fertig (oder Fehler, je nach Aufgabe)
    k hinten an buckets[b] anhängen
    anzahl ← anzahl + 1

find(k):
    gib zurück, ob k in buckets[b] steht, mit b ← h(k)

remove(k):
    k aus buckets[h(k)] entfernen, wenn es da ist
```

Gesucht wird nur in **einem** Bucket. Solange die Listen kurz sind, ist das fast so schnell wie ein Array. Sechs Schlüssel in acht Buckets, darunter drei, die alle in Bucket 5 landen:

<div class='algo-stepper' data-algo='hash' data-mode='chain' data-size='8' data-ops='insert 13, insert 21, insert 6, insert 29, insert 14, insert 40, find 29, find 37'></div>
<!-- /part -->

<!-- part: hash-probing -->
<!-- short: Linear Probing: ohne Listen; ist der Bucket belegt, der nächste, bis einer frei ist. Suchen läuft genauso, bis zum ersten leeren Bucket. -->
## Kollisionen: Linear Probing

Ohne Listen geht es auch: Jeder Bucket hält höchstens einen Schlüssel. Ist der Bucket \\(h(k)\\) belegt, probiert man den nächsten, dann den übernächsten, und am Ende des Arrays geht es vorn weiter. Das heißt **Linear Probing** (open addressing).

```text
insert(k):
    b ← h(k)
    solange buckets[b] belegt:
        wenn buckets[b] = k: fertig
        b ← (b + 1) mod m
    buckets[b] ← k

find(k):
    b ← h(k)
    solange buckets[b] belegt:
        wenn buckets[b] = k: gefunden
        b ← (b + 1) mod m
    nicht gefunden
```

Die Suche hört erst an einem **leeren** Bucket auf, denn bis dahin könnte \\(k\\) beim Einfügen weitergerutscht sein. Dieselben sechs Schlüssel wie oben:

<div class='algo-stepper' data-algo='hash' data-mode='probe' data-size='8' data-ops='insert 13, insert 21, insert 6, insert 29, insert 14, insert 40, find 29, find 37'></div>

Die Kollisionen in Bucket 5 schieben andere Schlüssel weiter, die selbst gar nicht kollidiert hätten: 6 gehört in Bucket 6, doch dort steht schon 21. So wachsen lange belegte Strecken, **Cluster**, und die Suche nach 37 muss am Ende fast die ganze Tabelle ansehen.

**Entfernen** ist bei Linear Probing heikel: Wird ein Bucket einfach geleert, endet die Suche nach einem Schlüssel, der hinter ihm liegt, zu früh. Man markiert ihn deshalb als **gelöscht** (einen »Grabstein«): Beim Suchen läuft man darüber hinweg, beim Einfügen darf er wieder belegt werden.
<!-- /part -->

<!-- part: hash-fuellgrad -->
<!-- short: Füllgrad: Schlüssel durch Buckets; über etwa 0,75 wächst die Tabelle, und alle Schlüssel werden neu verteilt (Rehashing). -->
## Füllgrad und Rehashing

Der **Füllgrad** (load factor) ist die Zahl der Schlüssel geteilt durch die Zahl der Buckets, \\(\alpha = n / m\\). Er sagt, wie schnell die Tabelle ist:

- Beim Chaining ist eine Liste im Mittel \\(\alpha\\) lang. Bei \\(\alpha = 1\\) steht in jedem Bucket im Mittel ein Schlüssel, bei \\(\alpha = 10\\) sucht man in Listen mit zehn.
- Bei Linear Probing muss \\(\alpha\\) unter 1 bleiben, sonst passt nichts mehr hinein, und schon ab etwa 0,7 werden die Cluster lang.

Deshalb wächst eine Hash Table, wenn der Füllgrad eine Grenze überschreitet, meist 0,75: Sie legt ein Array mit doppelt so vielen Buckets an und fügt **jeden** Schlüssel neu ein. Kopieren genügt nicht, denn mit \\(m\\) ändert sich \\(h(k) = k \bmod m\\), und jeder Schlüssel kommt in einen anderen Bucket. Das heißt **Rehashing**. Wie beim Vergrößern einer [Array List](/reference/listen#array-list) ist es teuer, aber selten.

```text
insert(k):
    … wie oben …
    wenn anzahl / m > 0,75:
        alt ← buckets
        m ← 2 · m
        buckets ← m leere Buckets
        anzahl ← 0
        für jeden Schlüssel s in alt: insert(s)
```

### Wofür

Hash Tables stecken hinter jedem `Hash` in Ruby, jedem `dict` und `set` in Python, jedem Objekt in JavaScript und jeder `HashMap` in Java: überall, wo man zu einem Schlüssel schnell einen Wert finden will. Ein Suchbaum kann dasselbe, langsamer, aber er hält die Schlüssel dafür sortiert. Eine Hash Table weiß nichts über die Reihenfolge, nach dem kleinsten Schlüssel muss sie alle Buckets durchsuchen.
<!-- /part -->
