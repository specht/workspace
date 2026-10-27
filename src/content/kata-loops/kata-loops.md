<div class='meta'>
icon: bi-arrow-repeat
kind: uebung
duration: 1 Doppelstunde
languages: [ruby, python, javascript]
idea: Zählen, sammeln und suchen mit Schleifen
curriculum: "Sek I: Algorithmisches Problemlösen (algorithmische Grundstrukturen: Wiederholung); Oberstufe: Algorithmen und Softwareentwicklung (Wiederholung)"
topics: [grundstrukturen]
age: Sek I
requires: [kata-decisions]
</div>

# Katas: Schleifen

<p class='abstract'>
Du schreibst Funktionen mit Schleifen: Sie zählen, summieren, füllen Listen
und suchen, bis sie fertig sind. Von der Summe bis eins über Fakultät und
Fibonacci bis zu den Primzahlen.
</p>


## Immer wieder

Eine Schleife wiederholt, was in ihr steht. Diese Funktion zählt, wie viele
Zahlen von 1 bis `n` durch 7 teilbar sind:

<!-- language: ruby -->
```ruby
def sevens(n)
  count = 0
  (1..n).each do |i|
    count += 1 if i % 7 == 0
  end
  count
end

puts sevens(50)
```

`(1..n).each do |i|` läuft mit `i` von 1 bis `n`: Alles bis `end` geschieht
einmal für jede dieser Zahlen. `count` fängt vor der Schleife bei 0 an, und
`count += 1 if …` macht es um 1 größer, wenn die Bedingung gilt. So eine
Variable sammelt das Ergebnis.
<!-- /language -->
<!-- language: python -->
```python
def sevens(n):
    count = 0
    for i in range(1, n + 1):
        if i % 7 == 0:
            count += 1
    return count

print(sevens(50))
```

`for i in range(1, n + 1):` läuft mit `i` von 1 bis `n`: Die zweite Zahl ist
die erste, die nicht mehr dabei ist. Was eingerückt darunter steht, geschieht
einmal für jede dieser Zahlen. `count` fängt vor der Schleife bei 0 an, und
`count += 1` macht es um 1 größer, wenn die Bedingung gilt. So eine Variable
sammelt das Ergebnis.
<!-- /language -->
<!-- language: javascript -->
```javascript
function sevens(n) {
  let count = 0;
  for (let i = 1; i <= n; i++) {
    if (i % 7 === 0) {
      count++;
    }
  }
  return count;
}

console.log(sevens(50));
```

`for (let i = 1; i <= n; i++)` fängt mit `i` bei 1 an, läuft, solange
`i <= n` gilt, und zählt nach jedem Durchlauf eins weiter. `count` fängt vor
der Schleife bei 0 an, und `count++` macht es um 1 größer, wenn die Bedingung
gilt. So eine Variable sammelt das Ergebnis; `let` erlaubt, sie zu ändern,
`const` nicht.
<!-- /language -->

_task(predict-sevens)

<!-- after: predict-sevens -->
Es erscheint `7`: 7, 14, 21, 28, 35, 42 und 49. Bei diesen Zahlen ist der Rest
beim Teilen durch 7 gleich 0, und nur dann wird `count` größer.
<!-- /after -->

### Herunterzählen

Eine Schleife kann auch rückwärts zählen:

<!-- language: ruby -->
```ruby
3.downto(1) do |i|
  puts i
end
puts 'Los!'
```
<!-- /language -->
<!-- language: python -->
```python
for i in range(3, 0, -1):
    print(i)
print('Los!')
```
<!-- /language -->
<!-- language: javascript -->
```javascript
for (let i = 3; i >= 1; i--) {
  console.log(i);
}
console.log('Los!');
```
<!-- /language -->

_task(predict-countdown)

<!-- after: predict-countdown -->
<!-- language: ruby -->
Es erscheinen 3, 2, 1 und dann `Los!`: `3.downto(1)` zählt von 3 herunter bis
einschließlich 1. Die letzte Zeile steht nach `end`, außerhalb der Schleife,
und kommt deshalb nur einmal.
<!-- /language -->
<!-- language: python -->
Es erscheinen 3, 2, 1 und dann `Los!`: `range(3, 0, -1)` zählt von 3 in
Schritten von −1 herunter; die 0 ist die erste Zahl, die nicht mehr dabei ist.
Die letzte Zeile ist nicht eingerückt, steht also außerhalb der Schleife und
kommt nur einmal.
<!-- /language -->
<!-- language: javascript -->
Es erscheinen 3, 2, 1 und dann `Los!`: `i--` macht `i` nach jedem Durchlauf
um 1 kleiner, und bei 0 gilt `i >= 1` nicht mehr. Die letzte Zeile steht nach
der schließenden Klammer, außerhalb der Schleife, und kommt nur einmal.
<!-- /language -->
<!-- /after -->

### Solange etwas gilt

Manchmal weiß man vorher nicht, wie oft sich etwas wiederholt. `while`
wiederholt, solange eine Bedingung gilt. Wie oft kann man 100 halbieren, bis 1
übrig bleibt, wenn man jedes Mal den Rest weglässt?

<!-- language: ruby -->
```ruby
n = 100
steps = 0
while n > 1
  n = n / 2
  steps += 1
end
puts steps
```
<!-- /language -->
<!-- language: python -->
```python
n = 100
steps = 0
while n > 1:
    n = n // 2
    steps += 1
print(steps)
```
<!-- /language -->
<!-- language: javascript -->
```javascript
let n = 100;
let steps = 0;
while (n > 1) {
  n = Math.floor(n / 2);
  steps++;
}
console.log(steps);
```
<!-- /language -->

_task(predict-halving)

<!-- after: predict-halving -->
Es erscheint `6`: 100, 50, 25, 12, 6, 3, 1. Nach jedem Halbieren prüft die
Schleife, ob `n` noch größer als 1 ist; bei 1 hört sie auf. Ändert sich in
der Schleife nichts an der Bedingung, hört sie nie auf: Dann bricht
**Ausführen** nach einer Weile ab.
<!-- /after -->

### Eine Liste füllen

Eine Schleife kann auch eine Liste füllen. Die Liste fängt leer an, und jeder
Durchlauf hängt einen Wert hinten an:

<!-- language: ruby -->
```ruby
result = []
(1..3).each do |i|
  result << i * 10
end
p result
```

`[]` ist eine leere Liste, und `<<` hängt einen Wert hinten an. `p` zeigt die
Liste so, wie man sie in Ruby schreibt: `[10, 20, 30]`.
<!-- /language -->
<!-- language: python -->
```python
result = []
for i in range(1, 4):
    result.append(i * 10)
print(result)
```

`[]` ist eine leere Liste, und `append` hängt einen Wert hinten an. Das
Programm zeigt `[10, 20, 30]`.
<!-- /language -->
<!-- language: javascript -->
```javascript
const result = [];
for (let i = 1; i <= 3; i++) {
  result.push(i * 10);
}
console.log(result);
```

`[]` ist eine leere Liste, und `push` hängt einen Wert hinten an. Das
Programm zeigt `[ 10, 20, 30 ]`. `const` passt hier: `result` bleibt dieselbe
Liste, nur ihr Inhalt wächst.
<!-- /language -->

Mehr über Listen kommt in den [Katas zu Listen](/kata-lists); für die Katas hier reicht das.

Schon das erste veröffentlichte Programm, 1843 für eine Rechenmaschine, die
nie fertig wurde, hatte eine Schleife.

_person(lovelace)

## Einstieg

_task(sum-to)

_task(numbers)

_task(squares)

_task(countdown)

## Übung

_task(factorial)

_task(digit-sum)

_task(fibonacci)

_task(divisors)

<div class='hint try'>
<strong>Ausprobieren:</strong> Lass deine Fibonacci-Funktion mit
<em>Ausführen</em> die Zahlen bis 90 ausgeben. Wie groß werden sie, und wie
lange braucht dein Code dafür?
</div>

## Knobelei

Jetzt hört eine Schleife nicht nach einer festen Zahl von Durchläufen auf,
sondern wenn etwas gefunden ist.

_task(prime)

_task(gcd)

_task(collatz)

<!-- extra -->
## Extras

Zwei Katas, die nicht mitzählen. Bei der zweiten zählt auch, wie schnell
dein Code ist.

_task(perfect)

_task(primes-up-to)
