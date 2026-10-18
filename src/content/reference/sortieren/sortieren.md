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

Alle Beispiele sortieren dieselben acht Zahlen: 5, 2, 8, 1, 9, 3, 7, 4.
<!-- /part -->

<!-- part: selection-sort -->
<!-- short: Selection Sort: die kleinste der übrigen suchen und nach vorne tauschen; immer n²/2 Vergleiche. -->
## Selection Sort

Suche die kleinste Zahl und tausche sie an Index 0. Suche unter den übrigen die kleinste und tausche sie an Index 1. Und so weiter. **Invariante:** Links von Index i stehen die i kleinsten Zahlen, sortiert.

```text
selection_sort(liste):
    für i von 0 bis länge - 2:
        kleinste ← i
        für j von i + 1 bis länge - 1:
            wenn liste[j] < liste[kleinste]: kleinste ← j
        liste[i] und liste[kleinste] tauschen
```

<div class='algo-stepper' data-algo='sort' data-sort='selection' data-values='5, 2, 8, 1, 9, 3, 7, 4'></div>

Selection Sort braucht immer gleich viele Vergleiche, \\(\frac{n(n-1)}{2}\\), ob die Liste schon sortiert ist oder nicht: Bei 8 Zahlen 28, bei 100 schon 4 950. Dafür tauscht es höchstens \\(n - 1\\)-mal, das wenigste von allen.
<!-- /part -->

<!-- part: insertion-sort -->
<!-- short: Insertion Sort: das nächste Element herausnehmen und nach links schieben, bis es passt; schnell, wenn die Liste fast sortiert ist. -->
## Insertion Sort

So sortiert man Spielkarten in der Hand: Nimm die nächste Karte und schieb sie nach links, an allen größeren vorbei, bis sie passt. **Invariante:** Links von Index i ist alles sortiert, aber noch nicht unbedingt an seinem endgültigen Platz.

```text
insertion_sort(liste):
    für i von 1 bis länge - 1:
        aktuell ← liste[i]
        j ← i
        solange j > 0 und liste[j - 1] > aktuell:
            liste[j] ← liste[j - 1]        (eins nach rechts rücken)
            j ← j - 1
        liste[j] ← aktuell
```

Das angehobene Element ist das, das gerade einsortiert wird:

<div class='algo-stepper' data-algo='sort' data-sort='insertion' data-values='5, 2, 8, 1, 9, 3, 7, 4'></div>

Ist die Liste schon sortiert, braucht Insertion Sort nur \\(n - 1\\) Vergleiche, ist sie umgekehrt sortiert, \\(\frac{n(n-1)}{2}\\). Für fast sortierte Listen ist es deshalb sehr gut, und für kurze Listen auch: Viele schnelle Verfahren sortieren die kleinen Stücke am Ende mit Insertion Sort.

**Typische Fehler:** `j` bis unter 0 laufen lassen (erst `j > 0` prüfen, dann `liste[j - 1]` lesen); `aktuell` erst nach dem Verschieben aus der Liste lesen, dann ist es schon überschrieben.
<!-- /part -->

<!-- part: bubble-sort -->
<!-- short: Bubble Sort: Nachbarn vertauschen, wenn sie falsch herum stehen; nach jedem Durchgang steht die größte ganz rechts. -->
## Bubble Sort

Geh durch die Liste und vertausche zwei Nachbarn, wenn der linke größer ist. Nach einem Durchgang ist die größte Zahl ganz rechts angekommen, wie eine Blase, die aufsteigt. Der nächste Durchgang muss sie nicht mehr ansehen. **Invariante:** Rechts von `ende` stehen die größten, an ihrem Platz.

```text
bubble_sort(liste):
    für ende von länge - 1 bis 1, rückwärts:
        getauscht ← falsch
        für i von 0 bis ende - 1:
            wenn liste[i] > liste[i + 1]:
                liste[i] und liste[i + 1] tauschen
                getauscht ← wahr
        wenn nicht getauscht: fertig
```

<div class='algo-stepper' data-algo='sort' data-sort='bubble' data-values='5, 2, 8, 1, 9, 3, 7, 4'></div>

Bubble Sort ist leicht zu verstehen und leicht richtig zu schreiben, aber langsam: Es tauscht viel öfter als die anderen. `getauscht` sorgt dafür, dass es bei einer sortierten Liste nach einem Durchgang aufhört.
<!-- /part -->

<!-- part: merge-sort -->
<!-- short: Merge Sort: halbieren, beide Hälften sortieren, zusammenführen; immer n log n Vergleiche, braucht einen Zwischenspeicher. -->
## Merge Sort

Teile die Liste in zwei Hälften, sortiere jede Hälfte (mit Merge Sort, also rekursiv), und führe die beiden sortierten Hälften zusammen. Zum **Zusammenführen** schaut man nur auf die vordersten beiden Zahlen der Hälften und nimmt die kleinere. Eine Liste mit einer Zahl ist schon sortiert, dort hört die Rekursion auf.

```text
merge_sort(liste, von, bis):            (bis gehört nicht mehr dazu)
    wenn bis - von < 2: fertig
    mitte ← (von + bis) / 2, abgerundet
    merge_sort(liste, von, mitte)
    merge_sort(liste, mitte, bis)
    i ← von, j ← mitte
    für k von von bis bis - 1:
        wenn i < mitte und (j ≥ bis oder liste[i] ≤ liste[j]):
            zwischen[k] ← liste[i], i ← i + 1
        sonst:
            zwischen[k] ← liste[j], j ← j + 1
    zwischen[von … bis - 1] zurück nach liste kopieren
```

Unter den Balken steht der Zwischenspeicher; die gestrichelte Linie trennt die beiden Hälften, die gerade zusammengeführt werden:

<div class='algo-stepper' data-algo='sort' data-sort='merge' data-values='5, 2, 8, 1, 9, 3, 7, 4'></div>

Merge Sort braucht immer etwa \\(n \log_2 n\\) Vergleiche, auch im schlechtesten Fall, und lässt gleiche Zahlen in ihrer Reihenfolge (es ist **stabil**). Dafür braucht es einen Zwischenspeicher so groß wie die Liste; in der Sandbox ist das `scratch`.

**Typische Fehler:** `≤` statt `<` beim Zusammenführen vergessen (dann ist es nicht mehr stabil); eine Hälfte ist leer, und man liest trotzdem `liste[j]` hinter dem Ende.
<!-- /part -->

<!-- part: quick-sort -->
<!-- short: Quick Sort: ein Pivot wählen, kleinere nach links, größere nach rechts, beide Seiten rekursiv; meist am schnellsten, bei ungünstigem Pivot n². -->
## Quick Sort

Wähle eine Zahl, das **Pivot**, hier die letzte des Bereichs. Bringe alle kleineren nach links und alle anderen nach rechts, und setze das Pivot dazwischen: Jetzt steht es an seinem endgültigen Platz. Sortiere dann links und rechts davon auf dieselbe Weise.

```text
quick_sort(liste, von, bis):            (bis gehört dazu)
    wenn bis ≤ von: fertig
    pivot ← liste[bis]
    grenze ← von
    für i von von bis bis - 1:
        wenn liste[i] < pivot:
            liste[i] und liste[grenze] tauschen
            grenze ← grenze + 1
    liste[grenze] und liste[bis] tauschen     (das Pivot an seinen Platz)
    quick_sort(liste, von, grenze - 1)
    quick_sort(liste, grenze + 1, bis)
```

Rot ist das Pivot, die rote gestrichelte Linie die Grenze: Links von ihr stehen die Zahlen, die kleiner sind als das Pivot.

<div class='algo-stepper' data-algo='sort' data-sort='quick' data-values='5, 2, 8, 1, 9, 3, 7, 4'></div>

Quick Sort ist in der Praxis meist das schnellste der fünf: Es braucht keinen Zwischenspeicher, und bei zufälligen Zahlen teilt das Pivot den Bereich ungefähr in der Mitte, das gibt etwa \\(n \log_2 n\\) Vergleiche. Ist die Liste aber schon sortiert, ist das letzte Element immer das größte, die eine Seite bleibt leer, und es werden \\(\frac{n(n-1)}{2}\\). Darum wählt man das Pivot in der Praxis zufällig oder nimmt den mittleren von drei Werten.
<!-- /part -->

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
<tr><td>Quick Sort (letztes als Pivot)</td><td>584</td><td>4 898</td><td>4 945</td><td>\(n \log n\), schlimmstenfalls \(n^2\)</td></tr>
<tr><td><a href='/reference/heap#heapsort'>Heapsort</a></td><td colspan='3'>etwa \(2 n \log_2 n\), in jedem Fall</td><td>\(n \log n\)</td></tr>
</tbody>
</table>
</div>

Bei 100 Zahlen ist der Unterschied zwischen 550 und 5 000 Vergleichen noch klein. Bei einer Million Zahlen sind es etwa 20 Millionen gegen 500 Milliarden: Sekunden gegen Tage. Schneller als \\(n \log_2 n\\) Vergleiche geht es übrigens nicht, wenn man nur vergleichen darf. Wer mehr über die Zahlen weiß, etwa dass es nur ganze Zahlen von 0 bis 100 sind, kann sie auch zählen statt vergleichen und ist dann noch schneller.
<!-- /part -->
