<div class='meta'>
icon: bi-list-ol
kind: uebung
duration: 1 Doppelstunde
languages: [ruby, python, javascript]
idea: Listen durchgehen, zusammenrechnen, umbauen und mischen
curriculum: "Sek I: Algorithmisches Problemlösen (Implementieren von Algorithmen auf Listen); Oberstufe: Algorithmen und Datenstrukturen (Listen)"
age: Sek I
requires: [kata-texts]
</div>

# Katas: Listen

<p class='abstract'>
Du schreibst Funktionen, die Listen bekommen: zusammenzählen, das Größte
finden, eine neue Liste in anderer Reihenfolge bauen, Doppelte entfernen und
zwei sortierte Listen zu einer mischen.
</p>

## Element für Element

Eine Liste hält mehrere Werte in einer Reihe. Jedes Element hat einen Index:
Das erste hat den Index 0, das zweite den Index 1. Diese Funktion baut eine
neue Liste, in der jede Zahl doppelt so groß ist:

<!-- language: ruby -->
```ruby
def doubled(numbers)
  result = []
  numbers.each do |number|
    result << number * 2
  end
  result
end

puts doubled([3, 4, 5]).inspect   # [6, 8, 10]
numbers = [3, 4, 5]
puts numbers.length               # 3
puts numbers[0]                   # 3, das Element mit dem Index 0
puts numbers[-1]                  # 5, das letzte
```

`numbers.each do |number| … end` geht die Elemente der Reihe nach durch,
`result << …` hängt hinten an. Wer den Index braucht, zählt selbst:
`(0...numbers.length).each do |i| … end` läuft mit `i` von 0 bis zum letzten
Index.
<!-- /language -->
<!-- language: python -->
```python
def doubled(numbers):
    result = []
    for number in numbers:
        result.append(number * 2)
    return result

print(doubled([3, 4, 5]))   # [6, 8, 10]
numbers = [3, 4, 5]
print(len(numbers))         # 3
print(numbers[0])           # 3, das Element mit dem Index 0
print(numbers[-1])          # 5, das letzte
```

`for number in numbers:` geht die Elemente der Reihe nach durch,
`result.append(…)` hängt hinten an. Wer den Index braucht, zählt selbst:
`for i in range(len(numbers)):` läuft mit `i` von 0 bis zum letzten Index.
<!-- /language -->
<!-- language: javascript -->
```javascript
function doubled(numbers) {
  const result = [];
  for (const number of numbers) {
    result.push(number * 2);
  }
  return result;
}

console.log(doubled([3, 4, 5]));          // [ 6, 8, 10 ]
const numbers = [3, 4, 5];
console.log(numbers.length);              // 3
console.log(numbers[0]);                  // 3, das Element mit dem Index 0
console.log(numbers[numbers.length - 1]); // 5, das letzte
```

`for (const number of numbers) { … }` geht die Elemente der Reihe nach
durch, `result.push(…)` hängt hinten an. Wer den Index braucht, zählt
selbst: `for (let i = 0; i < numbers.length; i++)` läuft mit `i` von 0 bis
zum letzten Index.
<!-- /language -->

Die Katas geben eine neue Liste zurück und lassen die, die sie bekommen,
wie sie ist.

## Einstieg

_task(list-sum)

_task(list-max)

_task(count-even)

_task(contains)

## Übung

_task(reverse-list)

_task(swap-ends)

<div class='hint try'>
<strong>Ausprobieren:</strong> Ändere in <code>swap_ends</code> die
Liste, die du bekommst, statt einer Kopie, und gib sie aus, nachdem du die
Funktion aufgerufen hast. Was ist mit der Liste draußen passiert?
</div>

_task(rotate-left)

_task(ascending)

## Knobelei

_task(remove-duplicates)

_task(missing-number)

_task(merge-sorted)

Das Mischen zweier sortierter Listen ist der Kern von Merge Sort: Wer eine
Liste halbiert, beide Hälften sortiert und sie dann so mischt, hat sie
sortiert. Ganze Sortierverfahren schreibst, beobachtest und vergleichst du in
der [Sorting Sandbox](/sandbox-sortieren); wie Listen im Speicher aussehen,
zeigt die [Data Structures Sandbox](/sandbox-datenstrukturen).

<!-- extra -->
## Extras

Zwei Katas, die nicht mitzählen.

_task(two-sum)

_task(chunks)
