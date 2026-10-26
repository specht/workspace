<div class='meta'>
icon: bi-check2-square
kind: uebung
duration: 1 Doppelstunde
languages: [ruby, python, javascript]
idea: Tests schreiben, die jeden Fehler finden
curriculum: "Oberstufe: Softwareentwicklung (Testen)"
requires: [kata-search]
</div>

# Katas: Tests schreiben

<p class='abstract'>
Diesmal schreibst du keine Funktionen, sondern Tests für sie. Die Prüfung
hat jeweils eine richtige Fassung und einige fehlerhafte: Deine Tests müssen
mit der richtigen bestehen und jede fehlerhafte entlarven.
</p>

## Ein Test ist eine Behauptung

Ein Test sagt, was eine Funktion für einen bestimmten Wert zurückgeben soll,
und meldet sich, wenn sie etwas anderes tut:

<!-- language: ruby -->
```ruby
assert_equal(12, gcd(60, 48))    # erwartet 12
assert_equal(7, gcd(7, 7))
```
<!-- /language -->
<!-- language: python -->
```python
assert_equal(12, gcd(60, 48))    # erwartet 12
assert_equal(7, gcd(7, 7))
```
<!-- /language -->
<!-- language: javascript -->
```javascript
assertEqual(12, gcd(60, 48));    // erwartet 12
assertEqual(7, gcd(7, 7));
```
<!-- /language -->

Zuerst steht, was herauskommen soll, dann der Aufruf. Ein guter Test sucht
die Stellen, an denen Fehler wohnen: die Ränder (0, eine leere Liste, das
erste und das letzte Element), die Fälle, in denen eine Regel eine Ausnahme
hat, und Werte, die nicht ganz gewöhnlich sind (negative Zahlen, zwei
gleiche). Ein Test, der nur das Erwartbare prüft, besteht auch mit vielen
fehlerhaften Fassungen.

Die Prüfung bringt die Funktion selbst mit; schreib sie nicht hinein, nur die
Tests. Meldet die Prüfung, dass eine fehlerhafte Fassung besteht, sagt sie
auch, was diese Fassung anders macht.

## Einstieg

_task(test-even)

_task(test-max)

_task(test-absolute)

## Übung

_task(test-leap-year)

_task(test-slug)

_task(test-fizzbuzz)

## Knobelei

_task(test-binary-search)

_task(test-merge)

<div class='hint try'>
<strong>Ausprobieren:</strong> Nimm einen deiner Tests heraus und führe
wieder aus. Welche fehlerhafte Fassung kommt jetzt durch? So findest du
heraus, welcher Test wofür da ist.
</div>

<!-- extra -->
## Extras

Zwei Katas, die nicht mitzählen.

_task(find-the-bug)

_task(test-caesar)
