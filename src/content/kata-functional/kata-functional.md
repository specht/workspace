<div class='meta'>
icon: bi-funnel
kind: uebung
duration: 2 Doppelstunden
languages: [ruby, python, javascript]
idea: Funktionen weitergeben, Listen umwandeln, sieben und zusammenfalten
curriculum: "Oberstufe: Vertiefung Applikative Programmierung (funktional: Funktionen als Werte, map, filter, reduce)"
topics: [funktional, listen]
requires: [kata-dicts]
</div>

# Katas: Funktionen als Werte

<p class='abstract'>
Du gibst Funktionen weiter wie Zahlen und baust Funktionen, die Funktionen
zurückgeben. Damit wandelst du jedes Element einer Liste um (<code>map</code>),
behältst, was passt (<code>filter</code>), und faltest alles zu einem Wert
zusammen (<code>reduce</code>). Zum Schluss schreibst du eine Schleife als
Kette um und triffst eine Aufgabe, bei der die Schleife klarer bleibt.
</p>

## Eine Funktion als Wert

Eine Zahl kann man in eine Variable legen und einer Funktion mitgeben. Mit
einer Funktion geht das auch. Diese Funktion bekommt eine Funktion `f` und
einen Wert `x` und wendet `f` zweimal an:

<!-- language: ruby -->
```ruby
def twice(f, x)
  f.call(f.call(x))
end

add_three = ->(x) { x + 3 }
puts add_three.call(4)        # 7
puts twice(add_three, 10)     # 16
```

`->(x) { x + 3 }` ist ein **Lambda**: eine Funktion ohne Namen, die man in
eine Variable legen und weitergeben kann wie eine Zahl. `f.call(…)` ruft sie
auf. `twice` weiß nicht, was `f` tut; es ruft sie nur auf.
<!-- /language -->
<!-- language: python -->
```python
def twice(f, x):
    return f(f(x))

add_three = lambda x: x + 3
print(add_three(4))           # 7
print(twice(add_three, 10))   # 16
```

`lambda x: x + 3` ist ein **Lambda**: eine Funktion ohne Namen, die man in
eine Variable legen und weitergeben kann wie eine Zahl. Hinter dem
Doppelpunkt steht, was sie zurückgibt. `f(…)` ruft sie auf, wie jede andere
Funktion. `twice` weiß nicht, was `f` tut; es ruft sie nur auf.
<!-- /language -->
<!-- language: javascript -->
```javascript
function twice(f, x) {
  return f(f(x));
}

const addThree = x => x + 3;
console.log(addThree(4));          // 7
console.log(twice(addThree, 10));  // 16
```

`x => x + 3` ist eine **Arrow Function**, oft auch Lambda genannt: eine
Funktion ohne Namen, die man in eine Variable legen und weitergeben kann wie
eine Zahl. Rechts vom Pfeil steht, was sie zurückgibt; mit zwei Parametern
schreibt man `(a, b) => a + b`. `f(…)` ruft sie auf. `twice` weiß nicht, was
`f` tut; es ruft sie nur auf.
<!-- /language -->

_task(predict-twice)

<!-- after: predict-twice -->
Es kommt `81` heraus: Der erste Aufruf macht aus 3 die 9, der zweite aus 9
die 81. Die Funktion, die `twice` bekommt, ist ein Wert wie die 3, und erst
`twice` entscheidet, wann und wie oft sie läuft.
<!-- /after -->

Das Wort Lambda kommt aus der Mathematik, vom griechischen Buchstaben λ,
mit dem Alonzo Church solche Funktionen schrieb.

_person(church)

_task(apply-n)

### Eine Funktion, die eine Funktion baut

Eine Funktion kann auch eine Funktion zurückgeben:

<!-- language: ruby -->
```ruby
def make_adder(n)
  ->(x) { x + n }
end

add_five = make_adder(5)
puts add_five.call(10)          # 15
puts make_adder(100).call(1)    # 101
```
<!-- /language -->
<!-- language: python -->
```python
def make_adder(n):
    return lambda x: x + n

add_five = make_adder(5)
print(add_five(10))             # 15
print(make_adder(100)(1))       # 101
```
<!-- /language -->
<!-- language: javascript -->
```javascript
function makeAdder(n) {
  return x => x + n;
}

const addFive = makeAdder(5);
console.log(addFive(10));        // 15
console.log(makeAdder(100)(1));  // 101
```
<!-- /language -->

Das Lambda merkt sich das `n`, mit dem es gebaut wurde, auch wenn
`make_adder` längst fertig ist. Eine Funktion, die sich so Variablen von
draußen merkt, heißt **Closure**.

_task(make-multiplier)

## map: jedes Element umwandeln

Die meisten Schleifen über Listen folgen einem von drei Mustern: Sie wandeln
jedes Element um, behalten einige oder rechnen alles zu einem Wert zusammen.
Für jedes Muster gibt es eine Funktion, der man nur noch sagt, was mit einem
Element geschehen soll. Die erste ist `map`:

<!-- language: ruby -->
```ruby
numbers = [1, 2, 3]
p numbers.map { |x| x * 10 }         # [10, 20, 30]
p numbers                            # [1, 2, 3], unverändert
words = ['Igel', 'Ameise', 'Wal']
p words.map { |word| word.upcase }   # ["IGEL", "AMEISE", "WAL"]
```

`map` ruft den Block für jedes Element auf und baut aus den Ergebnissen eine
neue Liste, so lang wie die alte; die alte bleibt, wie sie ist. Ein Block
`{ |x| … }` ist wie ein Lambda eine Funktion ohne Namen, nur schreibt man ihn
direkt hinter den Aufruf. Ein Lambda aus einer Variablen geht auch, mit `&`
davor: `numbers.map(&add_three)`.
<!-- /language -->
<!-- language: python -->
```python
numbers = [1, 2, 3]
print(list(map(lambda x: x * 10, numbers)))   # [10, 20, 30]
print(numbers)                                # [1, 2, 3], unverändert
words = ['Igel', 'Ameise', 'Wal']
print(list(map(str.upper, words)))            # ['IGEL', 'AMEISE', 'WAL']
```

`map(f, numbers)` ruft `f` für jedes Element auf; aus den Ergebnissen
entsteht eine neue Liste, so lang wie die alte, und die alte bleibt, wie sie
ist. `map` gibt dabei noch keine Liste zurück, sondern ein `map`-Objekt, das
die Werte erst ausrechnet, wenn jemand sie haben will; `list(…)` macht eine
Liste daraus. Statt eines Lambdas geht jede Funktion mit Namen, ohne
Klammern dahinter: `str.upper` ist die Methode `upper` der Strings.

In Python schreibt man dasselbe oft als **List Comprehension**:
`[x * 10 for x in numbers]`. Sie tut dasselbe wie `map`, und die Prüfungen
nehmen beides an.
<!-- /language -->
<!-- language: javascript -->
```javascript
const numbers = [1, 2, 3];
console.log(numbers.map(x => x * 10));              // [ 10, 20, 30 ]
console.log(numbers);                               // [ 1, 2, 3 ], unverändert
const words = ['Igel', 'Ameise', 'Wal'];
console.log(words.map(word => word.toUpperCase())); // [ 'IGEL', 'AMEISE', 'WAL' ]
```

`map` ruft die Funktion für jedes Element auf und baut aus den Ergebnissen
eine neue Liste, so lang wie die alte; die alte bleibt, wie sie ist.
<!-- /language -->

_task(predict-map)

<!-- after: predict-map -->
<!-- language: ruby -->
Es erscheint `[true, false, true]`: `map` gibt für jedes Element das
Ergebnis des Blocks, und `x > 1` ist `true` oder `false`. Die Liste ist so
lang wie vorher. Elemente weglassen kann `map` nicht; das tut die nächste
Funktion.
<!-- /language -->
<!-- language: python -->
Es erscheint `[True, False, True]`: `map` gibt für jedes Element das
Ergebnis des Lambdas, und `x > 1` ist `True` oder `False`. Die Liste ist so
lang wie vorher. Elemente weglassen kann `map` nicht; das tut die nächste
Funktion.
<!-- /language -->
<!-- language: javascript -->
Es erscheint `[ true, false, true ]`: `map` gibt für jedes Element das
Ergebnis der Funktion, und `x > 1` ist `true` oder `false`. Die Liste ist
so lang wie vorher. Elemente weglassen kann `map` nicht; das tut die nächste
Funktion.
<!-- /language -->
<!-- /after -->

<!-- language: ruby -->
In den Katas dieses Kapitels und des nächsten tut `map` oder `select` die
Arbeit; eine eigene Schleife brauchst du nicht. `each`, `for` und `times`
sind dafür abgeschaltet.
<!-- /language -->
<!-- language: python -->
In den Katas dieses Kapitels und des nächsten tut `map` oder `filter` die
Arbeit, oder eine List Comprehension; eine eigene Schleife brauchst du
nicht. Abschalten kann die Prüfung eine Schleife mit `for` nicht, schreib
sie trotzdem nicht.
<!-- /language -->
<!-- language: javascript -->
In den Katas dieses Kapitels und des nächsten tut `map` oder `filter` die
Arbeit; eine eigene Schleife brauchst du nicht. `forEach` ist dafür
abgeschaltet; eine Schleife mit `for` kann die Prüfung nicht abschalten,
schreib sie trotzdem nicht.
<!-- /language -->

_task(with-tax)

_task(lengths)

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I</strong>, John McCarthy, Communications of the ACM, 1960. Der Aufsatz, mit dem Lisp beginnt, schreibt Funktionen mit Churchs λ und hat einen Abschnitt »Functions with Functions as Arguments«. Die erste Funktion darin ist <code>maplist</code>: Sie wendet eine Funktion auf eine Liste an, dann auf den Rest ohne das erste Element und so weiter, und sammelt die Ergebnisse in einer neuen Liste. McCarthy schreibt sie in einer Zeile, rekursiv: <a href='https://doi.org/10.1145/367177.367199'>frei bei der ACM</a>.</p>
</div>

## filter: behalten, was passt

Die zweite Funktion bekommt eine Funktion, die für jedes Element sagt, ob es
bleiben soll:

<!-- language: ruby -->
```ruby
numbers = [4, 7, 10, 3]
p numbers.select { |x| x > 5 }    # [7, 10]
p numbers.reject { |x| x > 5 }    # [4, 3]
```

`select` behält die Elemente, für die der Block `true` ergibt, in ihrer
Reihenfolge; `reject` behält die anderen. `filter` ist in Ruby ein zweiter
Name für `select`.
<!-- /language -->
<!-- language: python -->
```python
numbers = [4, 7, 10, 3]
print(list(filter(lambda x: x > 5, numbers)))   # [7, 10]
print([x for x in numbers if x > 5])            # [7, 10]
```

`filter` behält die Elemente, für die das Lambda `True` ergibt, in ihrer
Reihenfolge; wie bei `map` macht `list(…)` daraus eine Liste. Die zweite
Zeile ist dasselbe als List Comprehension, mit `if` am Ende.
<!-- /language -->
<!-- language: javascript -->
```javascript
const numbers = [4, 7, 10, 3];
console.log(numbers.filter(x => x > 5));    // [ 7, 10 ]
console.log(numbers.filter(x => x <= 5));   // [ 4, 3 ]
```

`filter` behält die Elemente, für die die Funktion `true` ergibt, in ihrer
Reihenfolge.
<!-- /language -->

_task(evens)

_task(with-letter)

_task(passed)

## reduce: alles zu einem Wert

Die dritte Funktion rechnet eine Liste zu einem einzigen Wert zusammen. Sie
bekommt einen Startwert und eine Funktion mit zwei Parametern: dem Ergebnis
bis hierher und dem nächsten Element.

<!-- language: ruby -->
```ruby
numbers = [3, 4, 5]
p numbers.reduce(0) { |sum, x| sum + x }   # 12
p numbers.reduce { |sum, x| sum + x }      # 12
```

`reduce(0)` fängt mit 0 an. Was der Block zurückgibt, ist beim nächsten
Element das Ergebnis bis hierher, und nach dem letzten Element das Ergebnis.
Ohne Startwert fängt `reduce` mit dem ersten Element an. `inject` ist in
Ruby ein zweiter Name für `reduce`.
<!-- /language -->
<!-- language: python -->
```python
from functools import reduce

numbers = [3, 4, 5]
print(reduce(lambda total, x: total + x, numbers, 0))   # 12
print(reduce(lambda total, x: total + x, numbers))      # 12
```

`reduce(f, numbers, 0)` fängt mit 0 an. Was `f` zurückgibt, ist beim
nächsten Element das Ergebnis bis hierher, und nach dem letzten Element das
Ergebnis. Ohne Startwert fängt `reduce` mit dem ersten Element an. Seit
Python 3 steht `reduce` im Modul `functools` und muss importiert werden.
<!-- /language -->
<!-- language: javascript -->
```javascript
const numbers = [3, 4, 5];
console.log(numbers.reduce((total, x) => total + x, 0));   // 12
console.log(numbers.reduce((total, x) => total + x));      // 12
```

Der Startwert ist das zweite Argument, hier 0. Was die Funktion
zurückgibt, ist beim nächsten Element das Ergebnis bis hierher, und nach dem
letzten Element das Ergebnis. Ohne Startwert fängt `reduce` mit dem ersten
Element an.
<!-- /language -->

Mit dem Startwert 0 läuft es so:

```text
Startwert          0
Element 3     0 + 3 = 3
Element 4     3 + 4 = 7
Element 5     7 + 5 = 12
```

_task(predict-empty)

<!-- after: predict-empty -->
<!-- language: ruby -->
Es erscheint `nil`: Ohne Startwert nimmt `reduce` das erste Element, und
eine leere Liste hat keins. Mit `reduce(0)` kommt 0 heraus, die Summe von
nichts.
<!-- /language -->
<!-- language: python -->
Das Programm bricht ab: `TypeError: reduce() of empty iterable with no
initial value`. Ohne Startwert nimmt `reduce` das erste Element, und eine
leere Liste hat keins. Mit dem Startwert 0 kommt 0 heraus, die Summe von
nichts.
<!-- /language -->
<!-- language: javascript -->
Das Programm bricht ab: `TypeError: Reduce of empty array with no initial
value`. Ohne Startwert nimmt `reduce` das erste Element, und eine leere
Liste hat keins. Mit dem Startwert 0 kommt 0 heraus, die Summe von nichts.
<!-- /language -->

Gib darum einen Startwert an, wo die Liste leer sein kann: den Wert, der
herauskommen soll, wenn sie leer ist. Wer die [Katas zur
Rekursion](/kata-recursion) kennt: Dort rechnet `total` eine Summe genauso,
und der Fall, in dem sie sofort fertig ist, gibt diesen Startwert zurück.
<!-- /after -->

_task(product)

_task(longest)

<!-- language: ruby -->
Der Startwert muss keine Zahl sein. Ist er ein leerer Hash, `{}`, trägt
jedes Element etwas darin ein, und das Ergebnis bis hierher ist der Hash mit
allem, was schon eingetragen ist.
<!-- /language -->
<!-- language: python -->
Der Startwert muss keine Zahl sein. Ist er ein leeres Dictionary, `{}`,
trägt jedes Element etwas darin ein, und das Ergebnis bis hierher ist das
Dictionary mit allem, was schon eingetragen ist.
<!-- /language -->
<!-- language: javascript -->
Der Startwert muss keine Zahl sein. Ist er ein leeres Objekt, `{}`, trägt
jedes Element etwas darin ein, und das Ergebnis bis hierher ist das Objekt
mit allem, was schon eingetragen ist.
<!-- /language -->

_task(totals)

## Ketten

`map`, `filter` und `reduce` geben neue Werte zurück, und auf einer neuen
Liste kann gleich die nächste Funktion laufen. So entsteht eine Kette: Was
kosten alle Bestellungen zusammen, von denen jemand mindestens fünf Stück
bestellt hat? Die Preise sind in Cent.

<!-- language: ruby -->
```ruby
orders = [
  { 'item' => 'Heft', 'price' => 120, 'count' => 5 },
  { 'item' => 'Stift', 'price' => 80, 'count' => 10 },
  { 'item' => 'Lineal', 'price' => 250, 'count' => 1 },
]

total = orders
  .select { |order| order['count'] >= 5 }
  .map { |order| order['price'] * order['count'] }
  .sum
puts total   # 1400
```

Von oben nach unten liest sich die Kette wie die Frage: behalte die
Bestellungen mit mindestens fünf Stück, rechne für jede den Preis aus, zähl
zusammen. `sum` ist dabei ein `reduce`, das es fertig gibt: dasselbe wie
`reduce(0) { |sum, x| sum + x }`.
<!-- /language -->
<!-- language: python -->
```python
orders = [
    {'item': 'Heft', 'price': 120, 'count': 5},
    {'item': 'Stift', 'price': 80, 'count': 10},
    {'item': 'Lineal', 'price': 250, 'count': 1},
]

big = filter(lambda order: order['count'] >= 5, orders)
prices = map(lambda order: order['price'] * order['count'], big)
total = sum(prices)
print(total)   # 1400
```

Von oben nach unten liest sich das wie die Frage: behalte die Bestellungen
mit mindestens fünf Stück, rechne für jede den Preis aus, zähl zusammen.
`sum` ist dabei ein `reduce`, das es fertig gibt. In eine Zeile geschrieben
liest man die Kette in Python von innen nach außen,
`sum(map(…, filter(…, orders)))`; darum schreibt man sie oft als
Comprehension:

```python
total = sum(order['price'] * order['count'] for order in orders if order['count'] >= 5)
```
<!-- /language -->
<!-- language: javascript -->
```javascript
const orders = [
  { item: 'Heft', price: 120, count: 5 },
  { item: 'Stift', price: 80, count: 10 },
  { item: 'Lineal', price: 250, count: 1 },
];

const total = orders
  .filter(order => order.count >= 5)
  .map(order => order.price * order.count)
  .reduce((sum, x) => sum + x, 0);
console.log(total);   // 1400
```

Von oben nach unten liest sich die Kette wie die Frage: behalte die
Bestellungen mit mindestens fünf Stück, rechne für jede den Preis aus, zähl
zusammen.
<!-- /language -->

<div class='hint try'>
<strong>Ausprobieren:</strong> Vertausch in der Kette die ersten beiden
Schritte, so dass zuerst umgewandelt und dann behalten wird. Lies die
Fehlermeldung: Was ist eine Bestellung nach dem Umwandeln
geworden, und warum kann der zweite Schritt nicht mehr nach der Stückzahl
fragen?
</div>

_task(category-total)

### Eine Schleife umschreiben

_task(rewrite-loop)

### Wo die Schleife klarer bleibt

Nicht jede Schleife wird als Kette besser. `map`, `filter` und `reduce`
gehen immer die ganze Liste durch, und jedes Element wird für sich
behandelt; eine Schleife kann aufhören, sobald sie gefunden hat, was sie
sucht.

_task(first-overdraft)

<!-- after: first-overdraft -->
Mit `reduce` geht es auch, aber das Ergebnis bis hierher muss zwei Werte
tragen, den Kontostand und den Index, an dem er zum ersten Mal unter 0 fiel,
und es läuft bis zum Ende der Liste, auch wenn längst alles klar ist:

<!-- language: ruby -->
```ruby
def first_overdraft(balance, bookings)
  bookings.each_with_index.reduce([balance, -1]) do |(total, found), (amount, i)|
    total += amount
    found = i if found == -1 && total < 0
    [total, found]
  end[1]
end
```
<!-- /language -->
<!-- language: python -->
```python
from functools import reduce

def first_overdraft(balance, bookings):
    def step(state, pair):
        total, found = state
        i, amount = pair
        total += amount
        if found == -1 and total < 0:
            found = i
        return (total, found)
    return reduce(step, enumerate(bookings), (balance, -1))[1]
```
<!-- /language -->
<!-- language: javascript -->
```javascript
function firstOverdraft(balance, bookings) {
  return bookings.reduce(([total, found], amount, i) => {
    total += amount;
    if (found === -1 && total < 0) {
      found = i;
    }
    return [total, found];
  }, [balance, -1])[1];
}
```
<!-- /language -->

Die Schleife mit einem `return` mittendrin sagt dasselbe in weniger
Zeilen, und man sieht, wann sie aufhört. Eine Kette passt, wo jedes Element
für sich umgewandelt, behalten oder dazugerechnet wird; wo die Elemente
voneinander abhängen und es auf den Augenblick ankommt, ist eine Schleife
oft klarer.
<!-- /after -->

<!-- extra -->
## Extras

Zwei Katas, die nicht mitzählen: zwei Funktionen zu einer verbinden, und
`map` selbst schreiben.

_task(compose)

_task(my-map)
