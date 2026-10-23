<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Sortieren

<p class='abstract'>
Fünf Verfahren, die eine Liste durch Vergleichen sortieren: Selection Sort, Insertion Sort, Bubble Sort, Merge Sort und Quick Sort. Zu jedem die Idee, was nach jedem Schritt gilt, der Pseudocode zum Übersetzen in deine Sprache, ein Beispiel zum Durchklicken und typische Fehler. Am Ende ein Vergleich, wie viele Vergleiche jedes Verfahren braucht, wenn die Liste länger wird.
</p>

<script src='/include/algo_stepper.js'></script>

## Worauf es ankommt

<!-- part: sortieren-grundlagen -->
<!-- short: Sortieren durch Vergleichen: Was zählt, sind Vergleiche und Schreibzugriffe; eine Invariante sagt, was nach jedem Schritt gilt. -->
### Vergleichen und Schreiben

Alle Verfahren auf dieser Seite sortieren nur, indem sie zwei Elemente **vergleichen** und Elemente **schreiben** oder **vertauschen**. Wie schnell ein Verfahren ist, misst man deshalb nicht in Sekunden, sondern zählt Vergleiche und Schreibzugriffe; die [Sorting Sandbox](/sandbox-sortieren) zählt genau so. Wichtig ist, wie die Zahl wächst, wenn die Liste länger wird: Bei \\(n^2\\) Vergleichen braucht eine zehnmal so lange Liste hundertmal so viele, bei \\(n \log_2 n\\) nur gut dreizehnmal so viele.

### Die Invariante

Jedes Verfahren hat einen Satz, der nach jedem Schritt stimmt, die **Invariante**: »Links von Index i ist alles sortiert«, »Rechts von end stehen die größten, an ihrem Platz«. Wer seinen Code so schreibt, dass dieser Satz nie falsch wird, findet die meisten Fehler, bevor sie passieren. Im Beispiel zum Durchklicken ist der Teil lila, für den die Invariante schon gilt.

Alle Beispiele sortieren dieselben acht Zahlen: 5, 2, 8, 1, 9, 3, 7, 4. Über jedem Beispiel kannst du andere acht wählen: schon sortiert, fast sortiert, umgekehrt sortiert oder mit vielen gleichen. Sieh am Ende auf die Zahl der Vergleiche: Bei welchen Verfahren ändert sie sich, bei welchen nicht?
<!-- /part -->

<!-- part: selection-sort -->
<!-- short: Selection Sort: die kleinste der übrigen suchen und nach vorne tauschen; immer n²/2 Vergleiche. -->
## Selection Sort

Suche die kleinste Zahl und tausche sie an Index 0. Suche unter den übrigen die kleinste und tausche sie an Index 1. Und so weiter. **Invariante:** Links von Index i stehen die i kleinsten Zahlen, sortiert.

```pseudocode
selection_sort(list):
    for i from 0 to length - 2:
        min ← i
        for j from i + 1 to length - 1:
            if list[j] < list[min]: min ← j
        swap list[i] and list[min]
```

<div class='algo-stepper' data-title='Selection Sort' data-algo='sort' data-sort='selection' data-values='5, 2, 8, 1, 9, 3, 7, 4' data-presets='gemischt: 5, 2, 8, 1, 9, 3, 7, 4 | schon sortiert: 1, 2, 3, 4, 5, 7, 8, 9 | fast sortiert: 1, 2, 4, 3, 5, 7, 9, 8 | umgekehrt sortiert: 9, 8, 7, 5, 4, 3, 2, 1 | viele gleiche: 3, 1, 3, 2, 1, 3, 2, 1'></div>

Selection Sort braucht immer gleich viele Vergleiche, \\(\frac{n(n-1)}{2}\\), ob die Liste schon sortiert ist oder nicht: Bei 8 Zahlen 28, bei 100 schon 4 950. Dafür tauscht es höchstens \\(n - 1\\)-mal, das wenigste von allen.
<!-- /part -->

<!-- part: insertion-sort -->
<!-- short: Insertion Sort: das nächste Element herausnehmen und nach links schieben, bis es passt; schnell, wenn die Liste fast sortiert ist. -->
## Insertion Sort

So sortiert man Spielkarten in der Hand: Nimm die nächste Karte und schieb sie nach links, an allen größeren vorbei, bis sie passt. **Invariante:** Links von Index i ist alles sortiert, aber noch nicht unbedingt an seinem endgültigen Platz.

```pseudocode
insertion_sort(list):
    for i from 1 to length - 1:
        current ← list[i]
        j ← i
        while j > 0 and list[j - 1] > current:
            list[j] ← list[j - 1]        (eins nach rechts rücken)
            j ← j - 1
        list[j] ← current
```

Das angehobene Element ist das, das gerade einsortiert wird:

<div class='algo-stepper' data-title='Insertion Sort' data-algo='sort' data-sort='insertion' data-values='5, 2, 8, 1, 9, 3, 7, 4' data-presets='gemischt: 5, 2, 8, 1, 9, 3, 7, 4 | schon sortiert: 1, 2, 3, 4, 5, 7, 8, 9 | fast sortiert: 1, 2, 4, 3, 5, 7, 9, 8 | umgekehrt sortiert: 9, 8, 7, 5, 4, 3, 2, 1 | viele gleiche: 3, 1, 3, 2, 1, 3, 2, 1'></div>

Ist die Liste schon sortiert, braucht Insertion Sort nur \\(n - 1\\) Vergleiche, ist sie umgekehrt sortiert, \\(\frac{n(n-1)}{2}\\). Für fast sortierte Listen ist es deshalb sehr gut, und für kurze Listen auch: Viele schnelle Verfahren sortieren die kleinen Stücke am Ende mit Insertion Sort.

**Typische Fehler:** `j` bis unter 0 laufen lassen (erst `j > 0` prüfen, dann `list[j - 1]` lesen); `current` erst nach dem Verschieben aus der Liste lesen, dann ist es schon überschrieben.
<!-- /part -->

<!-- part: bubble-sort -->
<!-- short: Bubble Sort: Nachbarn vertauschen, wenn sie falsch herum stehen; nach jedem Durchgang steht die größte ganz rechts. -->
## Bubble Sort

Geh durch die Liste und vertausche zwei Nachbarn, wenn der linke größer ist. Nach einem Durchgang ist die größte Zahl ganz rechts angekommen, wie eine Blase, die aufsteigt. Der nächste Durchgang muss sie nicht mehr ansehen. **Invariante:** Rechts von `end` stehen die größten, an ihrem Platz.

```pseudocode
bubble_sort(list):
    for end from length - 1 down to 1:
        swapped ← false
        for i from 0 to end - 1:
            if list[i] > list[i + 1]:
                swap list[i] and list[i + 1]
                swapped ← true
        if not swapped: done
```

<div class='algo-stepper' data-title='Bubble Sort' data-algo='sort' data-sort='bubble' data-values='5, 2, 8, 1, 9, 3, 7, 4' data-presets='gemischt: 5, 2, 8, 1, 9, 3, 7, 4 | schon sortiert: 1, 2, 3, 4, 5, 7, 8, 9 | fast sortiert: 1, 2, 4, 3, 5, 7, 9, 8 | umgekehrt sortiert: 9, 8, 7, 5, 4, 3, 2, 1 | viele gleiche: 3, 1, 3, 2, 1, 3, 2, 1'></div>

Bubble Sort ist leicht zu verstehen und leicht richtig zu schreiben, aber langsam: Es tauscht viel öfter als die anderen. `swapped` sorgt dafür, dass es bei einer sortierten Liste nach einem Durchgang aufhört.
<!-- /part -->

<!-- part: merge-sort -->
<!-- short: Merge Sort: halbieren, beide Hälften sortieren, zusammenführen; immer n log n Vergleiche, braucht einen Zwischenspeicher. -->
## Merge Sort

Teile die Liste in zwei Hälften, sortiere jede Hälfte (mit Merge Sort, also rekursiv), und führe die beiden sortierten Hälften zusammen. Zum **Zusammenführen** schaut man nur auf die vordersten beiden Zahlen der Hälften und nimmt die kleinere. Eine Liste mit einer Zahl ist schon sortiert, dort hört die Rekursion auf.

```pseudocode
merge_sort(list, start, stop):          (stop gehört nicht mehr dazu)
    if stop - start < 2: done
    middle ← (start + stop) / 2, rounded down
    merge_sort(list, start, middle)
    merge_sort(list, middle, stop)
    i ← start, j ← middle
    for k from start to stop - 1:
        if i < middle and (j ≥ stop or list[i] ≤ list[j]):
            scratch[k] ← list[i], i ← i + 1
        else:
            scratch[k] ← list[j], j ← j + 1
    copy scratch[start … stop - 1] back to list
```

Unter den Balken steht der Zwischenspeicher; die gestrichelte Linie trennt die beiden Hälften, die gerade zusammengeführt werden:

<div class='algo-stepper' data-title='Merge Sort' data-algo='sort' data-sort='merge' data-values='5, 2, 8, 1, 9, 3, 7, 4' data-presets='gemischt: 5, 2, 8, 1, 9, 3, 7, 4 | schon sortiert: 1, 2, 3, 4, 5, 7, 8, 9 | fast sortiert: 1, 2, 4, 3, 5, 7, 9, 8 | umgekehrt sortiert: 9, 8, 7, 5, 4, 3, 2, 1 | viele gleiche: 3, 1, 3, 2, 1, 3, 2, 1'></div>

Merge Sort braucht immer etwa \\(n \log_2 n\\) Vergleiche, auch im schlechtesten Fall, und lässt gleiche Zahlen in ihrer Reihenfolge (es ist **stabil**). Dafür braucht es einen Zwischenspeicher so groß wie die Liste; in der Sandbox ist das `scratch`.

**Typische Fehler:** `≤` statt `<` beim Zusammenführen vergessen (dann ist es nicht mehr stabil); eine Hälfte ist leer, und man liest trotzdem `list[j]` hinter dem Ende.
<!-- /part -->

_person(neumann)

<!-- part: quick-sort -->
<!-- short: Quick Sort: ein Pivot wählen, kleinere nach links, größere nach rechts, beide Seiten rekursiv; meist am schnellsten, bei ungünstigem Pivot n². -->
## Quick Sort

Wähle eine Zahl, das **Pivot**, hier die letzte des Bereichs. Bringe alle kleineren nach links und alle anderen nach rechts, und setze das Pivot dazwischen: Jetzt steht es an seinem endgültigen Platz. Sortiere dann links und rechts davon auf dieselbe Weise.

```pseudocode
quick_sort(list, first, last):          (last gehört dazu)
    if last ≤ first: done
    pivot ← list[last]
    boundary ← first
    for i from first to last - 1:
        if list[i] < pivot:
            swap list[i] and list[boundary]
            boundary ← boundary + 1
    swap list[boundary] and list[last]     (das Pivot an seinen Platz)
    quick_sort(list, first, boundary - 1)
    quick_sort(list, boundary + 1, last)
```

Rot ist das Pivot, die rote gestrichelte Linie die Grenze: Links von ihr stehen die Zahlen, die kleiner sind als das Pivot.

<div class='algo-stepper' data-title='Quick Sort' data-algo='sort' data-sort='quick' data-values='5, 2, 8, 1, 9, 3, 7, 4' data-presets='gemischt: 5, 2, 8, 1, 9, 3, 7, 4 | schon sortiert: 1, 2, 3, 4, 5, 7, 8, 9 | fast sortiert: 1, 2, 4, 3, 5, 7, 9, 8 | umgekehrt sortiert: 9, 8, 7, 5, 4, 3, 2, 1 | viele gleiche: 3, 1, 3, 2, 1, 3, 2, 1'></div>

Quick Sort ist in der Praxis meist das schnellste der fünf: Es braucht keinen Zwischenspeicher, und bei zufälligen Zahlen teilt das Pivot den Bereich ungefähr in der Mitte, das gibt etwa \\(n \log_2 n\\) Vergleiche. Ist die Liste aber schon sortiert, ist das letzte Element immer das größte, die eine Seite bleibt leer, und es werden \\(\frac{n(n-1)}{2}\\). Darum wählt man das Pivot in der Praxis zufällig oder nimmt den mittleren von drei Werten.
<!-- /part -->

_person(hoare)

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Quicksort</strong>, C. A. R. Hoare, The Computer Journal, 1962. Hoare beschreibt sein Verfahren ausführlich und untersucht, wie schnell es ist; 1961 hatte er es schon als kurzes Programm in den Communications of the ACM gedruckt. Er teilt anders als der Pseudocode oben: Zwei Zeiger laufen von beiden Enden aufeinander zu und tauschen, was auf der falschen Seite steht. Die Fassung oben, mit dem letzten Element als Pivot, geht auf Nico Lomuto zurück. Der Aufsatz steht frei <a href='https://doi.org/10.1093/comjnl/5.1.10'>bei Oxford University Press</a>.</p>
</div>

<!-- part: sortieren-vergleich -->
<!-- short: Vergleiche bei 100 Zahlen: Selection, Insertion, Bubble etwa 2500 bis 5000, Merge und Quick etwa 550; sortiert und umgekehrt sortiert sieht es anders aus. -->
## Wie viele Vergleiche?

Die Zahl der Vergleiche für 100 Zahlen, gezählt mit genau den Verfahren oben:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Verfahren</th><th>zufällig</th><th>schon sortiert</th><th>umgekehrt sortiert</th><th>wächst mit</th></tr></thead>
<tbody>
<tr><td>Selection Sort</td><td>4 950</td><td>4 950</td><td>4 950</td><td>\(n^2\)</td></tr>
<tr><td>Insertion Sort</td><td>2 374</td><td>99</td><td>4 950</td><td>\(n^2\), sortiert \(n\)</td></tr>
<tr><td>Bubble Sort</td><td>4 779</td><td>99</td><td>4 950</td><td>\(n^2\), sortiert \(n\)</td></tr>
<tr><td>Merge Sort</td><td>542</td><td>316</td><td>356</td><td>\(n \log n\)</td></tr>
<tr><td>Quick Sort (letztes als Pivot)</td><td>584</td><td>4 950</td><td>4 950</td><td>\(n \log n\), schlimmstenfalls \(n^2\)</td></tr>
<tr><td><a href='/reference/heap#heapsort'>Heapsort</a></td><td colspan='3'>etwa \(2 n \log_2 n\), in jedem Fall</td><td>\(n \log n\)</td></tr>
</tbody>
</table>
</div>

Bei 100 Zahlen ist der Unterschied zwischen 550 und 5 000 Vergleichen noch klein. Bei einer Million Zahlen sind es etwa 20 Millionen gegen 500 Milliarden: Sekunden gegen Tage. Schneller als \\(n \log_2 n\\) Vergleiche geht es übrigens nicht, wenn man nur vergleichen darf. Wer mehr über die Zahlen weiß, etwa dass es nur ganze Zahlen von 0 bis 100 sind, kann sie auch zählen statt vergleichen und ist dann noch schneller.
<!-- /part -->

## Zum Nachschlagen

Im eigenen Programm sortierst du mit dem, was die Sprache mitbringt: `sort` und `sort_by` in Ruby, `sorted` und `list.sort` in Python. Die Verfahren auf dieser Seite programmierst du selbst, um zu verstehen, wie Sortieren geht und was es kostet.

- [Ruby: `Array#sort`](https://docs.ruby-lang.org/en/3.4/Array.html#method-i-sort) und [`sort_by`](https://docs.ruby-lang.org/en/3.4/Array.html#method-i-sort_by-21): sortieren mit `<=>` oder nach einem Schlüssel. Rubys Sortieren ist nicht garantiert stabil: Gleiche Werte können ihre Reihenfolge tauschen.
- [Python: Sorting HOW TO](https://docs.python.org/3/howto/sorting.html): `sorted` und `list.sort`, Sortieren nach einem Schlüssel (`key=`) und absteigend, und warum Pythons Sortieren **stabil** ist. Python sortiert mit Timsort, einer Mischung aus Merge Sort und Insertion Sort.
- [VisuAlgo: Sorting](https://visualgo.net/en/sorting): alle fünf Verfahren dieser Seite als Animation mit eigenen Zahlen, dazu Counting Sort und Radix Sort, die zählen statt vergleichen. Auf Englisch.
- [Wikipedia: Sortierverfahren](https://de.wikipedia.org/wiki/Sortierverfahren): ein Überblick über viele weitere Verfahren und was sie kosten.
