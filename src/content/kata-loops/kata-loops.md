<div class='meta'>
icon: bi-arrow-repeat
kind: uebung
duration: 1 Doppelstunde
languages: [ruby, python, javascript]
idea: Zählen, sammeln und suchen mit Schleifen
curriculum: "Sek I: Kontrollstrukturen (Wiederholung); Oberstufe: Algorithmen"
requires: [kata-decisions]
</div>

# Katas: Wiederholen

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

`(1..n).each do |i|` läuft mit `i` von 1 bis `n`; mit drei Punkten, `1...n`,
hört es vor `n` auf. `n.downto(0)` zählt herunter, und `while` wiederholt,
solange eine Bedingung gilt. Eine Variable wie `count`, die vor der Schleife
anfängt und in ihr wächst, sammelt das Ergebnis.
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
die erste, die nicht mehr dabei ist. `range(n, -1, -1)` zählt herunter, und
`while` wiederholt, solange eine Bedingung gilt. Eine Variable wie `count`,
die vor der Schleife anfängt und in ihr wächst, sammelt das Ergebnis.
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
`i <= n` gilt, und zählt nach jedem Durchlauf eins weiter. `i--` zählt
herunter, und `while` wiederholt, solange eine Bedingung gilt. Eine Variable
wie `count`, die vor der Schleife anfängt und in ihr wächst, sammelt das
Ergebnis; `let` erlaubt, sie zu ändern, `const` nicht.
<!-- /language -->

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
