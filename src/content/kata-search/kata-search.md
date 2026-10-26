<div class='meta'>
icon: bi-search
kind: uebung
duration: 1 Doppelstunde
languages: [ruby, python, javascript]
idea: Suchen, Schritt für Schritt und durch Halbieren
curriculum: "Oberstufe: Algorithmen und Datenstrukturen (lineare und binäre Suche, Effizienz)"
requires: [kata-recursion]
</div>

# Katas: Suchen

<p class='abstract'>
Du schreibst Funktionen, die in Listen suchen: von vorn, von hinten, nach der
kleinsten Zahl, und in sortierten Listen durch Halbieren, so schnell, dass
hunderttausend Suchen in hunderttausend Zahlen nur einen Augenblick dauern.
</p>

## Von vorn bis hinten, oder halbieren

Die einfachste Suche schaut sich ein Element nach dem anderen an, bis sie es
gefunden hat. Bei einer Liste mit einer Million Zahlen sind das im
schlimmsten Fall eine Million Vergleiche. Ist die Liste sortiert, geht es
anders: Schau in die Mitte, und du weißt, in welcher Hälfte die Zahl stehen
muss. Nach dem nächsten Blick ist es ein Viertel, dann ein Achtel. Nach 20
Blicken ist von einer Million nur noch eine Zahl übrig, denn 2²⁰ ist mehr als
eine Million.

<!-- language: ruby -->
```ruby
numbers = [2, 5, 8, 12, 16, 23]
low = 0
high = numbers.length - 1
middle = (low + high) / 2    # 2: numbers[2] ist 8
```

Bei ganzen Zahlen rundet `/` ab: Die Mitte zwischen den Indizes 0 und 5 ist 2.
<!-- /language -->
<!-- language: python -->
```python
numbers = [2, 5, 8, 12, 16, 23]
low = 0
high = len(numbers) - 1
middle = (low + high) // 2   # 2: numbers[2] ist 8
```

`//` teilt und rundet ab: Die Mitte zwischen den Indizes 0 und 5 ist 2.
<!-- /language -->
<!-- language: javascript -->
```javascript
const numbers = [2, 5, 8, 12, 16, 23];
let low = 0;
let high = numbers.length - 1;
const middle = Math.floor((low + high) / 2);   // 2: numbers[2] ist 8
```

`Math.floor` rundet ab: Die Mitte zwischen den Indizes 0 und 5 ist 2.
<!-- /language -->

Wer findet, gibt den Index zurück; wer nichts findet, sagt das mit
<!-- language: ruby -->
`nil`.
<!-- /language -->
<!-- language: python -->
`None`.
<!-- /language -->
<!-- language: javascript -->
`null`.
<!-- /language -->

## Einstieg

_task(linear-search)

_task(min-index)

_task(last-index)

_task(first-greater)

## Übung

_task(binary-search)

_task(guesses)

<div class='hint try'>
<strong>Ausprobieren:</strong> Wie viele Tipps brauchst du höchstens bei
Zahlen von 1 bis 100, von 1 bis 1000, von 1 bis einer Million? Ruf deine
Funktion mit der schwierigsten Zahl auf, die dir einfällt, und schau, wie
lang die Liste wird.
</div>

_task(first-at-least)

_task(int-sqrt)

## Knobelei

_task(insert-sorted)

_task(is-permutation)

_task(rotation-point)

Die kleinste Zahl finden und eine Zahl in eine sortierte Liste einsetzen sind
die Bausteine zweier Sortierverfahren: Selection Sort sucht immer wieder die
kleinste der übrigen Zahlen, Insertion Sort setzt eine Zahl nach der anderen
an ihre Stelle. Ganze Sortierverfahren schreibst, beobachtest und vergleichst
du in der [Sorting Sandbox](/sandbox-sortieren).

<!-- extra -->
## Extras

Zwei Katas, die nicht mitzählen. Bei der zweiten zählt auch, wie schnell dein
Code ist.

_task(search-steps)

_task(count-found)
