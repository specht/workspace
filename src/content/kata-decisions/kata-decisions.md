<div class='meta'>
icon: bi-signpost-split
accent: 195
kind: uebung
duration: 1 Doppelstunde
languages: [ruby, python, javascript]
idea: Je nach Wert etwas anderes tun
curriculum: "Sek I: Kontrollstrukturen (Verzweigung)"
requires: [kata-functions]
</div>

# Katas: Entscheidungen

<p class='abstract'>
Du schreibst Funktionen, die je nach Wert etwas anderes tun: mit
<code>if</code> und <code>else</code>, mit Vergleichen und mit
<code>and</code> und <code>or</code>. Bis zu FizzBuzz, einer berühmten
Aufgabe aus Vorstellungsgesprächen.
</p>


## Wenn, dann

Eine Funktion kann je nach Wert etwas anderes tun. Diese hier sagt, ob eine
Temperatur zum Baden reicht:

<!-- language: ruby -->
```ruby
def weather(degrees)
  if degrees >= 25
    'Baden!'
  elsif degrees >= 15
    'Spazieren'
  else
    'Lesen'
  end
end

puts weather(28)
```

Ruby prüft die Bedingungen der Reihe nach und nimmt die erste, die stimmt.
Verglichen wird mit `==`, `!=`, `<`, `<=`, `>` und `>=`; zwei Bedingungen
verbinden `&&` (beide) und `||` (mindestens eine), `!` dreht eine um. Ein
Vergleich ist selbst ein Wert, `true` oder `false`: `degrees >= 25` kann
eine Funktion auch direkt zurückgeben.
<!-- /language -->
<!-- language: python -->
```python
def weather(degrees):
    if degrees >= 25:
        return 'Baden!'
    elif degrees >= 15:
        return 'Spazieren'
    else:
        return 'Lesen'

print(weather(28))
```

Python prüft die Bedingungen der Reihe nach und nimmt die erste, die stimmt.
Verglichen wird mit `==`, `!=`, `<`, `<=`, `>` und `>=`; zwei Bedingungen
verbinden `and` (beide) und `or` (mindestens eine), `not` dreht eine um.
Ein Vergleich ist selbst ein Wert, `True` oder `False`: `degrees >= 25` kann
eine Funktion auch direkt zurückgeben.
<!-- /language -->
<!-- language: javascript -->
```javascript
function weather(degrees) {
  if (degrees >= 25) {
    return 'Baden!';
  } else if (degrees >= 15) {
    return 'Spazieren';
  } else {
    return 'Lesen';
  }
}

console.log(weather(28));
```

JavaScript prüft die Bedingungen der Reihe nach und nimmt die erste, die
stimmt. Verglichen wird mit `===`, `!==`, `<`, `<=`, `>` und `>=`; das
kürzere `==` gibt es auch, aber es hält `'5' == 5` für wahr. Zwei Bedingungen
verbinden `&&` (beide) und `||` (mindestens eine), `!` dreht eine um. Ein
Vergleich ist selbst ein Wert, `true` oder `false`: `degrees >= 25` kann eine
Funktion auch direkt zurückgeben.
<!-- /language -->

## Einstieg

_task(even)

_task(max-of-two)

_task(absolute)

_task(adult)

## Übung

Jetzt gibt es mehr als zwei Möglichkeiten. Schreib dir die Grenzen auf, bevor
du programmierst, und prüf genau die Werte, die auf einer Grenze liegen.

_task(leap-year)

_task(grade)

_task(ticket-price)

_task(max-of-three)

<div class='hint try'>
<strong>Ausprobieren:</strong> Vertausch in einer deiner Lösungen zwei
Zweige mit <code>elsif</code>, <code>elif</code> oder <code>else if</code> und
drück <em>Ausführen</em>.
Bei welchen Werten macht die Reihenfolge einen Unterschied, und bei welchen
nicht?
</div>

## Knobelei

_task(fizzbuzz)

_task(triangle)

_task(pythagorean)

<!-- extra -->
## Extras

_task(rock-paper-scissors)

_task(bread)

Weiter geht es mit Wiederholungen: Funktionen, die etwas viele Male tun.
