<div class='meta'>
icon: bi-braces
kind: uebung
duration: 1 Doppelstunde
languages: [ruby, python, javascript]
idea: Werte bekommen, rechnen, ein Ergebnis zurückgeben
curriculum: "Sek I: Algorithmisches Problemlösen (Variablenkonzept und Prozeduren)"
topics: [grundstrukturen, unterprogramme]
</div>

# Katas: Erste Funktionen

<p class='abstract'>
Du schreibst kleine Funktionen direkt hier auf der Seite: Sie bekommen Zahlen
oder Texte und geben ein Ergebnis zurück. Ohne Terminal und ohne Dateien, mit
einem Knopf, der deinen Code ausführt und prüft.
</p>


## Eine Funktion

Eine Funktion bekommt Werte, rechnet damit und gibt ein Ergebnis zurück. Diese
hier bekommt eine Zahl und gibt ihr Dreifaches zurück:

<!-- language: ruby -->
```ruby
def triple(n)
  3 * n
end

puts triple(14)
```

In Ruby ist das Ergebnis der Wert der letzten Zeile; `return 3 * n` schreibt
es ausdrücklich hin. Die Zeile `puts triple(14)` steht außerhalb der Funktion:
Sie ruft die Funktion zum Ausprobieren auf und schreibt ihr Ergebnis ins
Terminal.
<!-- /language -->
<!-- language: python -->
```python
def triple(n):
    return 3 * n

print(triple(14))
```

`return` gibt das Ergebnis zurück. Die Zeile `print(triple(14))` steht
außerhalb der Funktion: Sie ruft die Funktion zum Ausprobieren auf und
schreibt ihr Ergebnis ins Terminal.
<!-- /language -->
<!-- language: javascript -->
```javascript
function triple(n) {
  return 3 * n;
}

console.log(triple(14));
```

`return` gibt das Ergebnis zurück. Die Zeile `console.log(triple(14));` steht
außerhalb der Funktion: Sie ruft die Funktion zum Ausprobieren auf und
schreibt ihr Ergebnis in die Ausgabe. Namen aus mehreren Wörtern schreibt man
in JavaScript zusammen, jedes weitere Wort groß: `rectangleArea`.
<!-- /language -->

_task(predict-triple)

<!-- after: predict-triple -->
Es erscheint `42`. Beim Aufruf bekommt `n` den Wert `14`, die Funktion gibt
`3 * 14` zurück, und erst die Zeile außerhalb schreibt diesen Wert hin. Ohne
sie gäbe die Funktion ihr Ergebnis zurück, aber niemand schriebe es auf.
<!-- /after -->

### Texte bauen

Eine Funktion kann auch einen Text zurückgeben. Oft soll ein Wert mitten in
diesem Text stehen:

<!-- language: ruby -->
```ruby
def badge(name, age)
  "#{name} ist #{age} Jahre alt"
end

puts badge('Bello', 3)
```

Zwischen `#{` und `}` steht Code, und Ruby setzt seinen Wert in den Text ein.
Das geht nur in doppelten Anführungszeichen.
<!-- /language -->
<!-- language: python -->
```python
def badge(name, age):
    return f'{name} ist {age} Jahre alt'

print(badge('Bello', 3))
```

Das `f` vor dem Text macht ihn zu einem f-String: Zwischen `{` und `}` steht
Code, und Python setzt seinen Wert in den Text ein.
<!-- /language -->
<!-- language: javascript -->
```javascript
function badge(name, age) {
  return `${name} ist ${age} Jahre alt`;
}

console.log(badge('Bello', 3));
```

Der Text steht zwischen Backticks (`` ` ``). Zwischen `${` und `}` steht Code,
und JavaScript setzt seinen Wert in den Text ein.
<!-- /language -->

_task(predict-badge)

<!-- after: predict-badge -->
<!-- language: ruby -->
Es erscheint `Bello ist 3 Jahre alt`: `name` ist `'Bello'`, `age` ist `3`.
In einfachen Anführungszeichen setzt Ruby nichts ein; dort stünde
`#{name}` wörtlich im Text. Zwei Texte kannst du auch mit `+` aneinanderhängen:
`'Hallo, ' + name`.
<!-- /language -->
<!-- language: python -->
Es erscheint `Bello ist 3 Jahre alt`: `name` ist `'Bello'`, `age` ist `3`.
Ohne das `f` setzt Python nichts ein; dort stünde `{name}` wörtlich im Text.
Zwei Texte kannst du auch mit `+` aneinanderhängen: `'Hallo, ' + name`.
<!-- /language -->
<!-- language: javascript -->
Es erscheint `Bello ist 3 Jahre alt`: `name` ist `'Bello'`, `age` ist `3`.
In normalen Anführungszeichen setzt JavaScript nichts ein; dort stünde
`${name}` wörtlich im Text. Zwei Texte kannst du auch mit `+`
aneinanderhängen: `'Hallo, ' + name`.
<!-- /language -->
<!-- /after -->

In jeder Karte unten schreibst du eine solche Funktion. **Ausführen** startet
deinen Code einmal und zeigt, was er ausgibt; dann ruft es deine Funktion mit
verschiedenen Werten auf und vergleicht, was herauskommt. Neben dem Titel
steht, wie schwer eine Kata ist: Einstieg, Übung oder Knobelei. Du darfst
springen.

## Einstieg

_task(add)

_task(double)

_task(greet)

_task(rectangle)

<div class='hint try'>
<strong>Ausprobieren:</strong> Ruf eine deiner Funktionen in der letzten Zeile
mit anderen Werten auf, etwa mit einem Text statt einer Zahl, und drück
<em>Ausführen</em>. Was meldet dein Programm?
</div>

## Übung

Jetzt rechnen die Funktionen ein wenig mehr. Wie bei Rechnungen auf Papier
gilt Punkt vor Strich, und Klammern rechnen zuerst. Neu ist das Teilen: Was
kommt bei ganzen Zahlen heraus, und was bleibt übrig?

<!-- language: ruby -->
```ruby
puts 17 / 5
puts 17 % 5
puts 17 / 5.0
```
<!-- /language -->
<!-- language: python -->
```python
print(17 // 5)
print(17 % 5)
print(17 / 5)
```
<!-- /language -->
<!-- language: javascript -->
```javascript
console.log(Math.floor(17 / 5));
console.log(17 % 5);
console.log(17 / 5);
```
<!-- /language -->

_task(predict-rest)

<!-- after: predict-rest -->
<!-- language: ruby -->
Die Zeilen ergeben `3`, `2` und `3.4`. 17 durch 5 ist 3, Rest 2. Teilst du
in Ruby zwei ganze Zahlen, ist das Ergebnis eine ganze Zahl, und der Rest
fällt weg. `%` gibt genau diesen Rest. Steht eine Kommazahl in der Rechnung,
etwa `5.0`, kommt eine Kommazahl heraus.
<!-- /language -->
<!-- language: python -->
Die Zeilen ergeben `3`, `2` und `3.4`. 17 durch 5 ist 3, Rest 2. `//` teilt
ganz und lässt den Rest weg, `%` gibt genau diesen Rest, und `/` teilt immer
mit Komma.
<!-- /language -->
<!-- language: javascript -->
Die Zeilen ergeben `3`, `2` und `3.4`. 17 durch 5 ist 3, Rest 2. `/` teilt
in JavaScript immer mit Komma; `Math.floor` rundet ab und lässt so den Rest
weg. `%` gibt genau diesen Rest.
<!-- /language -->

Soll eine Zahl immer zwei Ziffern haben, wie die Sekunden einer Uhr,
schreibst du vorne eine Null dazu:

<!-- language: ruby -->
`format('%02d', 5)` ergibt den Text `'05'`.
<!-- /language -->
<!-- language: python -->
`f'{5:02d}'` ergibt den Text `'05'`.
<!-- /language -->
<!-- language: javascript -->
`String(5).padStart(2, '0')` ergibt den Text `'05'`.
<!-- /language -->
<!-- /after -->

_task(fahrenheit)

_task(clock)

_task(average)

_task(price)

## Knobelei

Hier brauchst du einen Plan, bevor du schreibst. Rechne ein Beispiel von
Hand, und schreib dir auf, welche Schritte du dabei gemacht hast.

_task(clock-after)

_task(last-digit)

<!-- extra -->
## Extras

Für alle, die schon fertig sind: zwei Katas, die nicht mitzählen.

_task(reverse-digits)

_task(duration)

Weiter geht es mit Entscheidungen: Funktionen, die je nach Wert etwas
anderes tun.
