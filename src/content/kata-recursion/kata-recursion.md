<div class='meta'>
icon: bi-diagram-3
kind: uebung
duration: 1 Doppelstunde
languages: [ruby, python, javascript]
idea: Funktionen, die sich selbst aufrufen, mit einem Fall, der aufhört
curriculum: "Oberstufe: Algorithmen (rekursive Verfahren)"
requires: [kata-lists]
</div>

# Katas: Rekursion

<p class='abstract'>
Du schreibst Funktionen, die sich selbst aufrufen: die Fakultät, die Summe
einer Liste, Fibonacci, die Türme von Hanoi und alle Reihenfolgen eines
Textes. Die Prüfung sieht im Code nach, ob deine Funktion sich wirklich
selbst aufruft.
</p>

## Ein kleineres Problem derselben Art

Eine rekursive Funktion löst ein Problem, indem sie dasselbe Problem eine
Nummer kleiner löst, mit einem Aufruf von sich selbst, und daraus ihr
Ergebnis baut. Damit das aufhört, gibt es einen Fall, in dem sie sofort
fertig ist. Diese Funktion zählt, wie oft man eine Zahl halbieren kann, bis 1
übrig ist:

<!-- language: ruby -->
```ruby
def halvings(n)
  return 0 if n <= 1        # sofort fertig
  1 + halvings(n / 2)       # dasselbe, eine Nummer kleiner
end

puts halvings(20)           # 4: 20, 10, 5, 2, 1
```
<!-- /language -->
<!-- language: python -->
```python
def halvings(n):
    if n <= 1:                  # sofort fertig
        return 0
    return 1 + halvings(n // 2) # dasselbe, eine Nummer kleiner

print(halvings(20))             # 4: 20, 10, 5, 2, 1
```
<!-- /language -->
<!-- language: javascript -->
```javascript
function halvings(n) {
  if (n <= 1) {                            // sofort fertig
    return 0;
  }
  return 1 + halvings(Math.floor(n / 2));  // dasselbe, eine Nummer kleiner
}

console.log(halvings(20));                 // 4: 20, 10, 5, 2, 1
```
<!-- /language -->

`halvings(20)` wartet auf `halvings(10)`, das auf `halvings(5)`, bis
`halvings(1)` sofort 0 zurückgibt; dann rechnet jeder Aufruf seins dazu,
von innen nach außen. Fehlt der Fall, der aufhört, ruft sich die Funktion
ohne Ende auf, und das Programm bricht ab, weil der Speicher für die
wartenden Aufrufe voll ist.

## Einstieg

_task(factorial)

_task(total)

_task(countdown)

_task(text-length)

## Übung

_task(power)

_task(fibonacci)

<div class='hint try'>
<strong>Ausprobieren:</strong> Ruf deine Funktion <code>fibonacci</code>
mit 25, 30 und 35 auf und schau, wie lange es jeweils dauert. Warum wird es
so schnell langsamer? Zähl einmal, wie oft <code>fibonacci(2)</code>
aufgerufen wird, wenn du <code>fibonacci(6)</code> ausrechnest.
</div>

_task(palindrome)

_task(count-char)

## Knobelei

_task(hanoi)

_task(flatten)

_task(permutations)

<!-- extra -->
## Extras

Zwei Katas, die nicht mitzählen. Bei der ersten zählt auch, wie schnell dein
Code ist.

_task(fast-fibonacci)

_task(subset-sum)
