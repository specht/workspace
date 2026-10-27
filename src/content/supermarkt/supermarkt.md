<div class='meta'>
image: teaser.webp
image_alt: "Drei Kassen als Zeitleisten von 9:00 bis 9:15: farbige Blöcke nebeneinander, je eine Kundin oder ein Kunde, die Zahl darin die Artikel, ein langer Block für 59 Artikel, ein kurzer für einen."
kind: projekt
duration: 3 Doppelstunden
languages: [ruby, python, javascript, dart, java]
curriculum: "Sek I: Algorithmisches Problemlösen (Grundlagen der Objektorientierung); Oberstufe: objektorientierte Modellierung (UML-Klassendiagramme), objektorientierte Programmierung"
topics: [objektorientierung]
requires: [kata-lists]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Klassen entwerfen: die Expresskasse

<p class='abstract'>
Ein Supermarkt überlegt, eine seiner drei Kassen zur Expresskasse zu machen.
Du entwirfst dafür Klassen, zeichnest sie als Klassendiagramm und programmierst
eine Simulation, die einen Samstagvormittag nachspielt: mit Kundinnen und
Kunden, Kassen, die sich merken, wann sie frei sind, und Kassen, die erben und
doch anders arbeiten. Am Ende weißt du, ob sich die Expresskasse lohnt.
</p>

## Die Geschichte

Die Filialleitung eines Supermarkts erzählt:

<div class='hint'>
»Samstags zwischen neun und elf ist bei uns am meisten los. Wir haben drei
Kassen, und die Schlangen reichen manchmal bis zu den Kühlregalen. Viele kaufen
nur ein paar Sachen und stehen trotzdem hinter vollen Einkaufswagen. Ich
überlege, eine der drei Kassen zur Expresskasse zu machen, für höchstens zehn
Artikel. Oder ich stelle statt der dritten Kasse zwei Selbstbedienungskassen
auf: Da scannen die Leute selbst, das dauert pro Artikel länger, aber Bezahlen
geht schneller. Ausprobieren kann ich das an einem Samstag nicht, ohne dass sich
alle ärgern.«
</div>

Ein Programm kann es ausprobieren: Es spielt den Vormittag nach, einmal mit
jeder Aufteilung der Kassen, und misst, wie lange die Leute warten. So ein
Programm heißt **Simulation**. Die ersten objektorientierten Programme waren
solche Simulationen: Die Sprache Simula entstand in den 1960er-Jahren am
Norwegischen Rechenzentrum in Oslo, um Schlangen an Schaltern, Häfen und Fabriken
nachzuspielen, und Simula 67 brachte Klassen, Objekte und Vererbung mit, die
heute fast jede Sprache hat.

Eine Simulation kann nicht alles nachspielen, was in der Geschichte vorkommt.
Sie braucht die Gegenstände, mit denen sie arbeitet, und von denen gibt es meist
viele: viele Kundinnen und Kunden, mehrere Kassen. Ein guter Anfang sind die
Nomen der Geschichte, und die Verben dazu, denn sie sagen, was mit den
Gegenständen geschieht.

_task(klasse-oder-attribut)

<!-- after: klasse-oder-attribut -->
Kundin und Kasse werden **Klassen**, die Expresskasse auch. Eine Klasse ist der
Bauplan für viele gleichartige Objekte: Jede Kundin ist ein **Objekt** der
Klasse Kundin, mit ihren eigenen Werten. Die Zahl der Artikel ist so ein Wert,
ein **Attribut**: Jede Kundin hat ihre eigene. Die Verben werden **Methoden**,
das, was ein Objekt tun kann: Eine Kasse bedient eine Kundin. Die Kühlregale
kommen im Programm nicht vor; die Simulation tut nichts mit ihnen.
<!-- /after -->

## Einrichten

Lege im Workspace einen Ordner `supermarkt` an und darin eine Datei
`supermarkt.uml` für das Klassendiagramm. Die UML-Werkstatt zeigt das Diagramm
neben der Datei, sobald du oben rechts auf das Symbol mit dem Diagramm klickst,
und unterstreicht, was sie nicht versteht. Alles, was du in eine `.uml`-Datei
schreiben kannst, steht im Handbuch unter
[Klassendiagramme als Text](/reference/uml).

<!-- language: ruby -->
Für den Code kommt `supermarket.rb` dazu:

```bash
mkdir -p ~/supermarkt
touch ~/supermarkt/supermarkt.uml ~/supermarkt/supermarket.rb
```
<!-- /language -->
<!-- language: python -->
Für den Code kommt `supermarket.py` dazu:

```bash
mkdir -p ~/supermarkt
touch ~/supermarkt/supermarkt.uml ~/supermarkt/supermarket.py
```
<!-- /language -->
<!-- language: javascript -->
Für den Code kommt `supermarket.js` dazu:

```bash
mkdir -p ~/supermarkt
touch ~/supermarkt/supermarkt.uml ~/supermarkt/supermarket.js
```
<!-- /language -->
<!-- language: dart -->
Für den Code kommt `supermarket.dart` dazu:

```bash
mkdir -p ~/supermarkt
touch ~/supermarkt/supermarkt.uml ~/supermarkt/supermarket.dart
```
<!-- /language -->
<!-- language: java -->
In Java steht jede Klasse in einer eigenen Datei, die heißt wie sie. Für die
Kundinnen und Kunden kommt `Customer.java` dazu:

```bash
mkdir -p ~/supermarkt
touch ~/supermarkt/supermarkt.uml ~/supermarkt/Customer.java
```
<!-- /language -->

## Die erste Klasse

### Im Diagramm

Ein **Klassendiagramm** (UML, Unified Modeling Language) zeigt jede Klasse als
Kasten mit drei Fächern: oben ihr Name, in der Mitte die Attribute, unten die
Methoden. In der UML-Werkstatt schreibst du es als Text; so sieht eine Klasse
für eine Lampe aus:

```uml
# Eine Lampe, als Beispiel
class Lamp
  - color: String
  - brightness: int
  + Lamp(color: String)
  + toggle(): void
```

Jedes Attribut steht in einer eigenen Zeile, `name: Typ`, die Methoden mit
ihren Parametern in Klammern und dem Typ dessen, was sie zurückgeben;
`void` heißt, sie geben nichts zurück. Eine Methode, die heißt wie die
Klasse und keinen Typ hat, ist der **Konstruktor**: Er legt ein neues
Objekt an und bekommt die Werte, die es von Anfang an braucht.

Das Zeichen davor sagt, wer es sehen darf: `-` ist **privat**, nur die Klasse
selbst kommt heran, `+` ist **öffentlich**. Attribute sind fast immer privat.
Von außen fragt man ein Objekt über seine Methoden; so kann es selbst
aufpassen, dass seine Werte stimmen. Das heißt **Kapselung**.

Die Typen schreibst du wie in Java, `int`, `double`, `boolean`, `String`,
egal in welcher Sprache du nachher programmierst: Ein Klassendiagramm
beschreibt einen Entwurf, keinen Code. Die Namen schreibst du so wie
nachher im Code, dann findest du jede Methode aus dem Diagramm dort wieder.

Eine Kundin oder ein Kunde bringt in die Simulation zwei Werte mit: wie viele
Artikel im Wagen liegen, und wann sie an den Kassen ankommt, in Sekunden nach
neun Uhr.

_task(diagramm-kunde)

### Klassendiagramm oder ER-Modell?

Wer ein ER-Modell kennt, etwa aus der [Filmdatenbank](/filmdatenbank), sieht
Ähnliches: Kästen mit Namen und Werten, Linien mit Zahlen daran. Die beiden
beschreiben aber Verschiedenes:

<table class='table'>
<tr><th></th><th>ER-Modell</th><th>Klassendiagramm</th></tr>
<tr><th>beschreibt</th><td>Daten, die gespeichert werden</td><td>Objekte in einem laufenden Programm</td></tr>
<tr><th>enthält</th><td>Entitäten, Attribute, Beziehungen</td><td>Klassen, Attribute, Methoden, Assoziationen, Vererbung</td></tr>
<tr><th>am Ende</th><td>bleibt alles in der Datenbank</td><td>sind die Objekte weg, sobald das Programm endet</td></tr>
<tr><th>wird zu</th><td>Tabellen</td><td>Klassen im Code</td></tr>
</table>

Ein ER-Modell sagt, was man sich merken muss, ein Klassendiagramm, wer was tut.
Die Kundinnen und Kunden dieser Simulation muss sich niemand merken: Es zählt,
was beim Nachspielen passiert, und danach nur eine Zahl, die mittlere
Wartezeit. Darum braucht die Simulation keine Datenbank. Ein Kassensystem im
echten Supermarkt hat beides: Artikel und Preise stehen in einer Datenbank, und
das Programm an der Kasse arbeitet mit Objekten. Zwischen beiden vermittelt oft
ein **ORM** (Object-Relational Mapping): Es liest Zeilen einer Tabelle als
Objekte und schreibt Objekte als Zeilen zurück.

### Im Code

<!-- language: ruby -->
In Ruby beginnt eine Klasse mit `class` und endet mit `end`. Der Konstruktor
heißt `initialize`; `Customer.new` ruft ihn auf. Was mit `@` beginnt, ist ein
Attribut des Objekts, eine **Instanzvariable**, und von außen nicht zu sehen.
`attr_reader` schreibt eine Methode, die es zurückgibt. Eine Kundin mit ihren
Artikeln:

```ruby
class Customer
  attr_reader :items

  def initialize(items)
    @items = items
  end
end

anna = Customer.new(12)
puts anna.items
```
<!-- /language -->
<!-- language: python -->
In Python beginnt eine Klasse mit `class`, eingerückt steht alles, was dazu
gehört. Der Konstruktor heißt `__init__`; `Customer(…)` ruft ihn auf. Jede
Methode bekommt als Erstes `self`, das Objekt selbst, und was in `self._items`
steht, gehört diesem Objekt. Der Unterstrich sagt: nur für die Klasse selbst.
`@property` macht aus einer Methode etwas, das man liest wie ein Attribut, aber
nicht ändern kann. Eine Kundin mit ihren Artikeln:

```python
class Customer:
    def __init__(self, items):
        self._items = items

    @property
    def items(self):
        return self._items


anna = Customer(12)
print(anna.items)
```
<!-- /language -->
<!-- language: javascript -->
In JavaScript beginnt eine Klasse mit `class`. Der Konstruktor heißt
`constructor`; `new Customer(…)` ruft ihn auf. Felder mit `#` vor dem Namen
sind privat, von außen kommt niemand heran. Ein `get` liest man wie ein Feld,
ändern kann man es so nicht. Eine Kundin mit ihren Artikeln:

```javascript
class Customer {
  #items;

  constructor(items) {
    this.#items = items;
  }

  get items() {
    return this.#items;
  }
}

const anna = new Customer(12);
console.log(anna.items);
```
<!-- /language -->
<!-- language: dart -->
In Dart beginnt eine Klasse mit `class`. Der Konstruktor heißt wie die Klasse;
`Customer(this._items)` setzt das Feld gleich mit dem Wert, den er bekommt. Ein
Name, der mit `_` beginnt, ist privat. Ein `get` liest man wie ein Feld, ändern
kann man es so nicht. Eine Kundin mit ihren Artikeln:

```dart
class Customer {
  final int _items;

  Customer(this._items);

  int get items => _items;
}

void main() {
  final anna = Customer(12);
  print(anna.items);
}
```
<!-- /language -->
<!-- language: java -->
In Java steht die Klasse `Customer` in `Customer.java`. Attribute stehen oben,
mit Typ und `private`. Der Konstruktor heißt wie die Klasse und hat keinen
Rückgabetyp; `new Customer(…)` ruft ihn auf. `this.items` ist das Attribut,
`items` allein der Parameter. Eine öffentliche Methode, die nur einen Wert
zurückgibt, heißt **Getter**. Eine Kundin mit ihren Artikeln:

```java
public class Customer {
    private int items;

    public Customer(int items) {
        this.items = items;
    }

    public int getItems() {
        return items;
    }
}
```
<!-- /language -->

Im Diagramm stehen solche Getter meist nicht: Dass man ein Attribut lesen kann,
gehört zum Code, nicht zum Entwurf.

<div class='hint own' data-check='kunde'>
<strong>Jetzt bist du dran:</strong> Eine Kundin hat auch eine Ankunftszeit.
Gib dem Konstruktor einen zweiten Parameter, merk dir die Zeit in einem zweiten
privaten Attribut und schreib die Methode, die sie zurückgibt, wie bei den
Artikeln.
</div>

<!-- language: ruby -->
<!-- hint -->
Alles, was die Artikel haben, braucht die Ankunftszeit auch: einen Parameter in
`initialize`, eine Zeile `@arrival = arrival` und einen Namen hinter
`attr_reader`.
<!-- /hint -->

<!-- solution -->
```ruby
class Customer
  attr_reader :items, :arrival

  def initialize(items, arrival)
    @items = items
    @arrival = arrival
  end
end
```
<!-- /solution -->
<!-- /language -->
<!-- language: python -->
<!-- hint -->
Alles, was die Artikel haben, braucht die Ankunftszeit auch: einen Parameter in
`__init__`, eine Zeile `self._arrival = arrival` und eine `@property`.
<!-- /hint -->

<!-- solution -->
```python
class Customer:
    def __init__(self, items, arrival):
        self._items = items
        self._arrival = arrival

    @property
    def items(self):
        return self._items

    @property
    def arrival(self):
        return self._arrival
```
<!-- /solution -->
<!-- /language -->
<!-- language: javascript -->
<!-- hint -->
Alles, was die Artikel haben, braucht die Ankunftszeit auch: ein Feld
`#arrival;`, einen Parameter im `constructor`, eine Zeile
`this.#arrival = arrival;` und ein `get`.
<!-- /hint -->

<!-- solution -->
```javascript
class Customer {
  #items;
  #arrival;

  constructor(items, arrival) {
    this.#items = items;
    this.#arrival = arrival;
  }

  get items() {
    return this.#items;
  }

  get arrival() {
    return this.#arrival;
  }
}
```
<!-- /solution -->
<!-- /language -->
<!-- language: dart -->
<!-- hint -->
Alles, was die Artikel haben, braucht die Ankunftszeit auch: ein Feld
`_arrival`, einen zweiten Parameter `this._arrival` im Konstruktor und ein
`get`.
<!-- /hint -->

<!-- solution -->
```dart
class Customer {
  final int _items;
  final int _arrival;

  Customer(this._items, this._arrival);

  int get items => _items;
  int get arrival => _arrival;
}
```
<!-- /solution -->
<!-- /language -->
<!-- language: java -->
<!-- hint -->
Alles, was die Artikel haben, braucht die Ankunftszeit auch: ein Attribut
`private int arrival;`, einen zweiten Parameter im Konstruktor, eine Zeile
`this.arrival = arrival;` und einen Getter `getArrival()`.
<!-- /hint -->

<!-- solution -->
```java
public class Customer {
    private int items;
    private int arrival;

    public Customer(int items, int arrival) {
        this.items = items;
        this.arrival = arrival;
    }

    public int getItems() {
        return items;
    }

    public int getArrival() {
        return arrival;
    }
}
```
<!-- /solution -->
<!-- /language -->

_task(kunde)

<!-- language: ruby -->
<div class='hint try'>
<strong>Ausprobieren:</strong> Leg unten in <code>supermarket.rb</code> zwei
Kundinnen an und gib ihre Artikel aus, <code>ruby supermarket.rb</code> im
Ordner <code>supermarkt</code>. Versuch danach, die Artikel zu ändern,
<code>anna.items = 3</code>: Was sagt Ruby dazu?
</div>
<!-- /language -->
<!-- language: python -->
<div class='hint try'>
<strong>Ausprobieren:</strong> Leg unten in <code>supermarket.py</code> zwei
Kundinnen an und gib ihre Artikel aus, <code>python3 supermarket.py</code> im
Ordner <code>supermarkt</code>. Versuch danach, die Artikel zu ändern,
<code>anna.items = 3</code>: Was sagt Python dazu?
</div>
<!-- /language -->
<!-- language: javascript -->
<div class='hint try'>
<strong>Ausprobieren:</strong> Leg unten in <code>supermarket.js</code> zwei
Kundinnen an und gib ihre Artikel aus, <code>node supermarket.js</code> im
Ordner <code>supermarkt</code>. Versuch danach, das private Feld zu lesen,
<code>console.log(anna.#items)</code>: Was sagt JavaScript dazu?
</div>
<!-- /language -->
<!-- language: dart -->
<div class='hint try'>
<strong>Ausprobieren:</strong> Leg in <code>main</code> zwei Kundinnen an und
gib ihre Artikel aus, <code>dart supermarket.dart</code> im Ordner
<code>supermarkt</code>. Versuch danach, die Artikel zu ändern,
<code>anna.items = 3;</code>: Was sagt Dart dazu?
</div>
<!-- /language -->
<!-- language: java -->
<div class='hint try'>
<strong>Ausprobieren:</strong> Schreib eine Klasse <code>Main</code> in
<code>Main.java</code> mit <code>public static void main(String[] args)</code>,
leg darin zwei Kundinnen an und gib ihre Artikel aus. Starte sie im Terminal im
Ordner <code>supermarkt</code> mit <code>java Main.java</code>. Ändere danach
in <code>Main</code> die Artikel direkt, <code>anna.items = 3;</code>: Was sagt
Java dazu?
</div>
<!-- /language -->

## Zustand und Verhalten

Eine Kasse bedient eine Kundin nach der anderen. Wie lange das dauert, hängt an
den Artikeln: Die Kassiererin scannt jeden in 3 Sekunden, und Bezahlen dauert
eine Minute. Eine Kundin mit 10 Artikeln braucht also 60 + 3 · 10 = 90 Sekunden.

Ein Objekt merkt sich nicht nur, was es von Anfang an bekommt. Eine Kasse ist
mal frei, mal beschäftigt, und das ändert sich mit jeder Kundin: Das ist ihr
**Zustand**, und er ändert sich durch ihre Methoden, ihr **Verhalten**.

_task(kasse-vorhersage)

<!-- after: kasse-vorhersage -->
Sie wartet 60 Sekunden. Die erste Kundin ist von Sekunde 0 bis 90 an der Reihe;
die zweite kommt in Sekunde 30 und kommt in Sekunde 90 dran. Mehr muss die
Kasse dafür nicht wissen als eine Zahl: **ab wann sie wieder frei ist**. Kommt
eine Kundin, fängt die Kasse mit ihr an, sobald sie frei ist, aber nicht, bevor
die Kundin da ist: Ist die Kasse schon länger frei, wartet niemand, und die
Kasse stand eine Weile leer. Danach ist sie frei, wenn diese Kundin fertig ist.
<!-- /after -->

Im Diagramm bekommt die Kasse dafür ein Attribut und zwei Methoden: eine, die
sagt, wie lange eine Kundin braucht, und eine, die sie bedient und zurückgibt,
wie lange sie gewartet hat.

<!-- language: ruby -->
```uml
class Checkout
  - free_at: int
  + Checkout()
  + service_time(customer: Customer): int
  + serve(customer: Customer): int
```

Ruby schreibt Namen aus zwei Wörtern klein mit Unterstrich: `free_at`,
`service_time`.
<!-- /language -->
<!-- language: python -->
```uml
class Checkout
  - free_at: int
  + Checkout()
  + service_time(customer: Customer): int
  + serve(customer: Customer): int
```

Python schreibt Namen aus zwei Wörtern klein mit Unterstrich: `free_at`,
`service_time`.
<!-- /language -->
<!-- language: javascript dart java -->
```uml
class Checkout
  - freeAt: int
  + Checkout()
  + serviceTime(customer: Customer): int
  + serve(customer: Customer): int
```
<!-- /language -->

Eine neue Kasse ist sofort frei, ab Sekunde 0. Die Methode, die bedient,
bekommt eine Kundin als **Parameter** und fragt sie nach ihren Werten: Die
Kasse kennt die Kundin nur für diesen einen Aufruf.

<div class='hint own' data-check='kasse'>
<strong>Jetzt bist du dran:</strong> Schreib die Klasse für die Kasse. Sie
merkt sich, ab wann sie frei ist, und man kann es abfragen; sie sagt, wie lange
eine Kundin braucht; und sie bedient eine Kundin: Sie fängt an, sobald sie frei
und die Kundin da ist, merkt sich, wann sie danach frei ist, und gibt zurück,
wie viele Sekunden die Kundin gewartet hat.
</div>

<!-- language: ruby -->
<!-- hint -->
`@free_at = 0` in `initialize` und `attr_reader :free_at`. `service_time`
rechnet `60 + 3 * customer.items`.
<!-- /hint -->

<!-- hint -->
In `serve` ist der Anfang der spätere von zwei Zeitpunkten:
`start = [customer.arrival, @free_at].max`. Danach ist die Kasse ab
`start + service_time(customer)` frei, und gewartet hat die Kundin
`start - customer.arrival`.
<!-- /hint -->

<!-- solution -->
```ruby
class Checkout
  attr_reader :free_at

  def initialize
    @free_at = 0
  end

  def service_time(customer)
    60 + 3 * customer.items
  end

  def serve(customer)
    start = [customer.arrival, @free_at].max
    @free_at = start + service_time(customer)
    start - customer.arrival
  end
end
```
<!-- /solution -->
<!-- /language -->
<!-- language: python -->
<!-- hint -->
`self._free_at = 0` in `__init__` und eine `@property` `free_at`.
`service_time` rechnet `60 + 3 * customer.items`.
<!-- /hint -->

<!-- hint -->
In `serve` ist der Anfang der spätere von zwei Zeitpunkten:
`start = max(customer.arrival, self._free_at)`. Danach ist die Kasse ab
`start + self.service_time(customer)` frei, und gewartet hat die Kundin
`start - customer.arrival`.
<!-- /hint -->

<!-- solution -->
```python
class Checkout:
    def __init__(self):
        self._free_at = 0

    @property
    def free_at(self):
        return self._free_at

    def service_time(self, customer):
        return 60 + 3 * customer.items

    def serve(self, customer):
        start = max(customer.arrival, self._free_at)
        self._free_at = start + self.service_time(customer)
        return start - customer.arrival
```
<!-- /solution -->
<!-- /language -->
<!-- language: javascript -->
<!-- hint -->
Ein Feld `#freeAt = 0;` und ein `get freeAt()`. `serviceTime` rechnet
`60 + 3 * customer.items`.
<!-- /hint -->

<!-- hint -->
In `serve` ist der Anfang der spätere von zwei Zeitpunkten:
`const start = Math.max(customer.arrival, this.#freeAt);`. Danach ist die Kasse
ab `start + this.serviceTime(customer)` frei, und gewartet hat die Kundin
`start - customer.arrival`.
<!-- /hint -->

<!-- solution -->
```javascript
class Checkout {
  #freeAt = 0;

  get freeAt() {
    return this.#freeAt;
  }

  serviceTime(customer) {
    return 60 + 3 * customer.items;
  }

  serve(customer) {
    const start = Math.max(customer.arrival, this.#freeAt);
    this.#freeAt = start + this.serviceTime(customer);
    return start - customer.arrival;
  }
}
```
<!-- /solution -->
<!-- /language -->
<!-- language: dart -->
<!-- hint -->
Ein Feld `int _freeAt = 0;` und ein `get freeAt`. `serviceTime` rechnet
`60 + 3 * customer.items`.
<!-- /hint -->

<!-- hint -->
In `serve` ist der Anfang der spätere von zwei Zeitpunkten: die Ankunft oder
`_freeAt`, je nachdem, was größer ist. Danach ist die Kasse ab
`start + serviceTime(customer)` frei, und gewartet hat die Kundin
`start - customer.arrival`.
<!-- /hint -->

<!-- solution -->
```dart
class Checkout {
  int _freeAt = 0;

  int get freeAt => _freeAt;

  int serviceTime(Customer customer) => 60 + 3 * customer.items;

  int serve(Customer customer) {
    final start = customer.arrival > _freeAt ? customer.arrival : _freeAt;
    _freeAt = start + serviceTime(customer);
    return start - customer.arrival;
  }
}
```
<!-- /solution -->
<!-- /language -->
<!-- language: java -->
<!-- hint -->
In `Checkout.java` ein Attribut `private int freeAt = 0;` und ein Getter
`getFreeAt()`. `serviceTime` rechnet `60 + 3 * customer.getItems()`.
<!-- /hint -->

<!-- hint -->
In `serve` ist der Anfang der spätere von zwei Zeitpunkten:
`int start = Math.max(customer.getArrival(), freeAt);`. Danach ist die Kasse ab
`start + serviceTime(customer)` frei, und gewartet hat die Kundin
`start - customer.getArrival()`.
<!-- /hint -->

<!-- solution -->
```java
public class Checkout {
    private int freeAt = 0;

    public int getFreeAt() {
        return freeAt;
    }

    public int serviceTime(Customer customer) {
        return 60 + 3 * customer.getItems();
    }

    public int serve(Customer customer) {
        int start = Math.max(customer.getArrival(), freeAt);
        freeAt = start + serviceTime(customer);
        return start - customer.getArrival();
    }
}
```
<!-- /solution -->
<!-- /language -->

_task(kasse)

## Der Supermarkt

Der Supermarkt hat mehrere Kassen, und er kennt sie die ganze Zeit, nicht nur
für einen Aufruf. So eine dauerhafte Verbindung zwischen Objekten zweier
Klassen heißt **Assoziation**. Im Diagramm ist sie eine Linie zwischen den
Kästen, mit einem Namen und an jedem Ende einer **Multiplizität**: wie viele
Objekte der Klasse an diesem Ende zu einem Objekt am anderen gehören. Ein
Beispiel aus der Schule:

```uml
class School
  - name: String
  + School(name: String)

class Room
  - number: String
  + Room(number: String)

association rooms: School 1 -- 1..* Room
```

Eine Schule hat einen oder mehrere Räume, `1..*` an `Room`; jeder Raum gehört
zu genau einer Schule, `1` an `School`. Die Zahl steht also an der Klasse,
deren Anzahl sie nennt. Weitere Multiplizitäten sind `0..1` (keins oder eins),
`*` (beliebig viele) und Bereiche wie `2..4`. Die Räume stehen nicht noch
einmal als Attribut im Kasten der Schule: Die Linie sagt es schon. Im Code wird
daraus trotzdem ein Attribut, eine Liste.

Eine Kundin dagegen bekommt die Kasse nur als Parameter, für einen Aufruf: Das
ist keine Assoziation, die Kasse kennt sie danach nicht mehr.

Der Supermarkt nimmt die Kundinnen in der Reihenfolge an, in der sie kommen,
und schickt jede an eine seiner Kassen. Dabei zählt er mit, wie lange alle
zusammen gewartet haben und wie viele es waren, damit er am Ende die **mittlere
Wartezeit** sagen kann, und die ist eine Kommazahl, `double`.

_task(diagramm-supermarkt)

<div class='hint own' data-check='supermarkt'>
<strong>Jetzt bist du dran:</strong> Schreib die Klasse für den Supermarkt. Der
Konstruktor bekommt eine Liste von Kassen. Kommt eine Kundin, geht sie an die
Kasse, die am frühesten frei ist; sind zwei gleich früh frei, an die, die in
der Liste zuerst steht. Die Kasse bedient sie, und der Supermarkt gibt die
Wartezeit zurück. Dazu eine Methode für die mittlere Wartezeit aller, die bisher
da waren.
</div>

<!-- language: ruby -->
<!-- hint -->
Drei Attribute: die Liste `@checkouts`, `@total_wait = 0` und
`@customers = 0`. Die mittlere Wartezeit ist `@total_wait.to_f / @customers`;
ohne `to_f` rechnet Ruby mit ganzen Zahlen und lässt die Nachkommastellen weg.
<!-- /hint -->

<!-- hint -->
In `arrive` merkt sich `best = nil` die beste Kasse bisher. Eine Schleife über
`@checkouts` nimmt eine Kasse, wenn es noch keine gibt oder sie früher frei ist
als `best`: `best = checkout if best.nil? || checkout.free_at < best.free_at`.
Mit `<` statt `<=` bleibt bei Gleichstand die erste.
<!-- /hint -->

<!-- solution -->
```ruby
class Supermarket
  def initialize(checkouts)
    @checkouts = checkouts
    @total_wait = 0
    @customers = 0
  end

  def arrive(customer)
    best = nil
    @checkouts.each do |checkout|
      best = checkout if best.nil? || checkout.free_at < best.free_at
    end
    wait = best.serve(customer)
    @total_wait += wait
    @customers += 1
    wait
  end

  def average_wait
    @total_wait.to_f / @customers
  end
end
```
<!-- /solution -->
<!-- /language -->
<!-- language: python -->
<!-- hint -->
Drei Attribute: die Liste `self._checkouts`, `self._total_wait = 0` und
`self._customers = 0`. Die mittlere Wartezeit ist
`self._total_wait / self._customers`; `/` gibt in Python immer eine Kommazahl.
<!-- /hint -->

<!-- hint -->
In `arrive` merkt sich `best = None` die beste Kasse bisher. Eine Schleife über
`self._checkouts` nimmt eine Kasse, wenn es noch keine gibt oder sie früher frei
ist als `best`: `if best is None or checkout.free_at < best.free_at:`. Mit `<`
statt `<=` bleibt bei Gleichstand die erste.
<!-- /hint -->

<!-- solution -->
```python
class Supermarket:
    def __init__(self, checkouts):
        self._checkouts = checkouts
        self._total_wait = 0
        self._customers = 0

    def arrive(self, customer):
        best = None
        for checkout in self._checkouts:
            if best is None or checkout.free_at < best.free_at:
                best = checkout
        wait = best.serve(customer)
        self._total_wait += wait
        self._customers += 1
        return wait

    def average_wait(self):
        return self._total_wait / self._customers
```
<!-- /solution -->
<!-- /language -->
<!-- language: javascript -->
<!-- hint -->
Drei Felder: die Liste `#checkouts`, `#totalWait = 0` und `#customers = 0`. Die
mittlere Wartezeit ist `this.#totalWait / this.#customers`.
<!-- /hint -->

<!-- hint -->
In `arrive` merkt sich `let best = null;` die beste Kasse bisher. Eine Schleife
`for (const checkout of this.#checkouts)` nimmt eine Kasse, wenn es noch keine
gibt oder sie früher frei ist als `best`:
`if (best === null || checkout.freeAt < best.freeAt)`. Mit `<` statt `<=`
bleibt bei Gleichstand die erste.
<!-- /hint -->

<!-- solution -->
```javascript
class Supermarket {
  #checkouts;
  #totalWait = 0;
  #customers = 0;

  constructor(checkouts) {
    this.#checkouts = checkouts;
  }

  arrive(customer) {
    let best = null;
    for (const checkout of this.#checkouts) {
      if (best === null || checkout.freeAt < best.freeAt) {
        best = checkout;
      }
    }
    const wait = best.serve(customer);
    this.#totalWait += wait;
    this.#customers += 1;
    return wait;
  }

  averageWait() {
    return this.#totalWait / this.#customers;
  }
}
```
<!-- /solution -->
<!-- /language -->
<!-- language: dart -->
<!-- hint -->
Drei Felder: die Liste `final List<Checkout> _checkouts;`, `int _totalWait = 0;`
und `int _customers = 0;`. Die mittlere Wartezeit ist
`_totalWait / _customers`; `/` gibt in Dart immer ein `double`.
<!-- /hint -->

<!-- hint -->
In `arrive` merkt sich `Checkout? best;` die beste Kasse bisher, am Anfang
`null`. Eine Schleife `for (final checkout in _checkouts)` nimmt eine Kasse,
wenn es noch keine gibt oder sie früher frei ist als `best`:
`if (best == null || checkout.freeAt < best.freeAt)`. Mit `<` statt `<=` bleibt
bei Gleichstand die erste. Danach ist `best!` sicher eine Kasse.
<!-- /hint -->

<!-- solution -->
```dart
class Supermarket {
  final List<Checkout> _checkouts;
  int _totalWait = 0;
  int _customers = 0;

  Supermarket(this._checkouts);

  int arrive(Customer customer) {
    Checkout? best;
    for (final checkout in _checkouts) {
      if (best == null || checkout.freeAt < best.freeAt) {
        best = checkout;
      }
    }
    final wait = best!.serve(customer);
    _totalWait += wait;
    _customers += 1;
    return wait;
  }

  double averageWait() => _totalWait / _customers;
}
```
<!-- /solution -->
<!-- /language -->
<!-- language: java -->
<!-- hint -->
In `Supermarket.java`, mit `import java.util.List;` oben: drei Attribute, die
Liste `private List<Checkout> checkouts;`, `private int totalWait = 0;` und
`private int customers = 0;`. Die mittlere Wartezeit ist
`(double) totalWait / customers`; ohne `(double)` teilt Java ganze Zahlen und
lässt die Nachkommastellen weg.
<!-- /hint -->

<!-- hint -->
In `arrive` merkt sich `Checkout best = null;` die beste Kasse bisher. Eine
Schleife `for (Checkout checkout : checkouts)` nimmt eine Kasse, wenn es noch
keine gibt oder sie früher frei ist als `best`:
`if (best == null || checkout.getFreeAt() < best.getFreeAt())`. Mit `<` statt
`<=` bleibt bei Gleichstand die erste.
<!-- /hint -->

<!-- solution -->
```java
import java.util.List;

public class Supermarket {
    private List<Checkout> checkouts;
    private int totalWait = 0;
    private int customers = 0;

    public Supermarket(List<Checkout> checkouts) {
        this.checkouts = checkouts;
    }

    public int arrive(Customer customer) {
        Checkout best = null;
        for (Checkout checkout : checkouts) {
            if (best == null || checkout.getFreeAt() < best.getFreeAt()) {
                best = checkout;
            }
        }
        int wait = best.serve(customer);
        totalWait += wait;
        customers += 1;
        return wait;
    }

    public double averageWait() {
        return (double) totalWait / customers;
    }
}
```
<!-- /solution -->
<!-- /language -->

_task(supermarkt)

## Expresskasse und Selbstbedienung

### Vererbung

Eine Expresskasse ist eine Kasse wie jede andere: Sie merkt sich, wann sie frei
ist, und bedient genauso schnell. Nur nimmt sie niemanden mit mehr als zehn
Artikeln. Eine Selbstbedienungskasse nimmt alle, aber die Leute scannen selbst,
6 Sekunden je Artikel, und bezahlen in 30 Sekunden, weil dort nur Karten gehen.

Was beide mit der Kasse gemeinsam haben, muss niemand noch einmal schreiben:
Sie **erben** von ihr. Alles, was die Kasse hat, haben sie auch; was bei ihnen
anders ist, schreiben sie selbst. Ein Beispiel von der Straße:

```uml
class Vehicle
  - wheels: int
  + Vehicle(wheels: int)
  + seats(): int

class Bicycle extends Vehicle
  + Bicycle()
  + seats(): int
```

Im Diagramm zeigt ein Pfeil mit hohler Spitze von der erbenden Klasse zu der,
von der sie erbt. Steht eine Methode in beiden, gilt die der erbenden Klasse:
Sie **überschreibt** die andere. Ein Fahrrad hat Räder wie jedes Fahrzeug, aber
seine eigene Zahl von Sitzen. Gibt es von einer Klasse selbst keine Objekte, nur
von denen, die von ihr erben, ist sie **abstrakt**: `class Vehicle (abstract)`.
Eine normale Kasse ist selbst eine Kasse, sie bleibt also, wie sie ist.

Damit der Supermarkt weiß, wen er wohin schicken darf, fragt er jede Kasse, ob
sie eine Kundin nimmt. Die normale Kasse sagt immer ja; die Expresskasse
überschreibt das.

_task(diagramm-kassen)

_task(kassen-vorhersage)

<!-- after: kassen-vorhersage -->
Es gibt 120, 120 und 150 aus. Die Schleife weiß nur, dass Kassen in der Liste
stehen; welche Rechnung läuft, entscheidet jedes Objekt selbst, nach seiner
Klasse. Die Expresskasse hat keine eigene, also gilt die geerbte; die
Selbstbedienungskasse hat ihre eigene. Das heißt **Polymorphie**: derselbe
Aufruf, verschiedenes Verhalten. Auch die Methode, die bedient, steht nur in der
Kasse und rechnet in der Selbstbedienungskasse doch mit deren Zeit, weil sie
dort die überschriebene Methode aufruft.
<!-- /after -->

<div class='hint own' data-check='kassen'>
<strong>Jetzt bist du dran:</strong> Gib der Kasse die Methode, die sagt, ob
sie eine Kundin nimmt, und schreib die Expresskasse und die
Selbstbedienungskasse; jede überschreibt eine Methode. Der Supermarkt schickt
eine Kundin dann nur an Kassen, die sie nehmen.
</div>

<!-- language: ruby -->
<!-- hint -->
`class ExpressCheckout < Checkout` erbt von `Checkout`. In `Checkout` gibt
`accepts?(customer)` `true` zurück; die Expresskasse schreibt ihre eigene, die
`customer.items <= 10` zurückgibt. Eine Methode, die mit Ja oder Nein antwortet,
endet in Ruby mit `?`.
<!-- /hint -->

<!-- hint -->
`SelfCheckout` überschreibt nur `service_time`. In der Schleife von `arrive`
überspringt `next unless checkout.accepts?(customer)` eine Kasse, die die
Kundin nicht nimmt.
<!-- /hint -->

<!-- solution -->
```ruby
class Checkout
  ~ …
  def accepts?(customer)
    true
  end
  ~ …
end

class ExpressCheckout < Checkout
  def accepts?(customer)
    customer.items <= 10
  end
end

class SelfCheckout < Checkout
  def service_time(customer)
    30 + 6 * customer.items
  end
end
```

Und in `arrive`, als erste Zeile der Schleife:

```ruby
next unless checkout.accepts?(customer)
```
<!-- /solution -->
<!-- /language -->
<!-- language: python -->
<!-- hint -->
`class ExpressCheckout(Checkout):` erbt von `Checkout`. In `Checkout` gibt
`accepts(self, customer)` `True` zurück; die Expresskasse schreibt ihre eigene,
die `customer.items <= 10` zurückgibt.
<!-- /hint -->

<!-- hint -->
`SelfCheckout` überschreibt nur `service_time`. In der Schleife von `arrive`
kommt eine Kasse nur in Frage, wenn `checkout.accepts(customer)` gilt:
`if checkout.accepts(customer) and (best is None or …):`.
<!-- /hint -->

<!-- solution -->
```python
class Checkout:
    ~ …
    def accepts(self, customer):
        return True
    ~ …


class ExpressCheckout(Checkout):
    def accepts(self, customer):
        return customer.items <= 10


class SelfCheckout(Checkout):
    def service_time(self, customer):
        return 30 + 6 * customer.items
```

Und in `arrive`, in der Schleife:

```python
if checkout.accepts(customer) and (best is None or checkout.free_at < best.free_at):
```
<!-- /solution -->
<!-- /language -->
<!-- language: javascript -->
<!-- hint -->
`class ExpressCheckout extends Checkout` erbt von `Checkout`. In `Checkout`
gibt `accepts(customer)` `true` zurück; die Expresskasse schreibt ihre eigene,
die `customer.items <= 10` zurückgibt.
<!-- /hint -->

<!-- hint -->
`SelfCheckout` überschreibt nur `serviceTime`. In der Schleife von `arrive`
kommt eine Kasse nur in Frage, wenn `checkout.accepts(customer)` gilt:
`if (checkout.accepts(customer) && (best === null || …))`.
<!-- /hint -->

<!-- solution -->
```javascript
class Checkout {
  ~ …
  accepts(customer) {
    return true;
  }
  ~ …
}

class ExpressCheckout extends Checkout {
  accepts(customer) {
    return customer.items <= 10;
  }
}

class SelfCheckout extends Checkout {
  serviceTime(customer) {
    return 30 + 6 * customer.items;
  }
}
```

Und in `arrive`, in der Schleife:

```javascript
if (checkout.accepts(customer) && (best === null || checkout.freeAt < best.freeAt)) {
```
<!-- /solution -->
<!-- /language -->
<!-- language: dart -->
<!-- hint -->
`class ExpressCheckout extends Checkout` erbt von `Checkout`. In `Checkout`
gibt `accepts(Customer customer)` `true` zurück; die Expresskasse schreibt ihre
eigene, mit `@override` davor, die `customer.items <= 10` zurückgibt.
<!-- /hint -->

<!-- hint -->
`SelfCheckout` überschreibt nur `serviceTime`. In der Schleife von `arrive`
kommt eine Kasse nur in Frage, wenn `checkout.accepts(customer)` gilt:
`if (checkout.accepts(customer) && (best == null || …))`.
<!-- /hint -->

<!-- solution -->
```dart
class Checkout {
  ~ …
  bool accepts(Customer customer) => true;
  ~ …
}

class ExpressCheckout extends Checkout {
  @override
  bool accepts(Customer customer) => customer.items <= 10;
}

class SelfCheckout extends Checkout {
  @override
  int serviceTime(Customer customer) => 30 + 6 * customer.items;
}
```

Und in `arrive`, in der Schleife:

```dart
if (checkout.accepts(customer) && (best == null || checkout.freeAt < best.freeAt)) {
```
<!-- /solution -->
<!-- /language -->
<!-- language: java -->
<!-- hint -->
`public class ExpressCheckout extends Checkout` in `ExpressCheckout.java` erbt
von `Checkout`. In `Checkout` gibt `accepts(Customer customer)` `true` zurück;
die Expresskasse schreibt ihre eigene, mit `@Override` davor, die
`customer.getItems() <= 10` zurückgibt.
<!-- /hint -->

<!-- hint -->
`SelfCheckout` in `SelfCheckout.java` überschreibt nur `serviceTime`. In der
Schleife von `arrive` kommt eine Kasse nur in Frage, wenn
`checkout.accepts(customer)` gilt:
`if (checkout.accepts(customer) && (best == null || …))`.
<!-- /hint -->

<!-- solution -->
In `Checkout.java`:

```java
public boolean accepts(Customer customer) {
    return true;
}
```

`ExpressCheckout.java` und `SelfCheckout.java`:

```java
public class ExpressCheckout extends Checkout {
    @Override
    public boolean accepts(Customer customer) {
        return customer.getItems() <= 10;
    }
}
```

```java
public class SelfCheckout extends Checkout {
    @Override
    public int serviceTime(Customer customer) {
        return 30 + 6 * customer.getItems();
    }
}
```

Und in `arrive`, in der Schleife:

```java
if (checkout.accepts(customer) && (best == null || checkout.getFreeAt() < best.getFreeAt())) {
```
<!-- /solution -->
<!-- /language -->

_task(kassen)

<!-- language: dart java -->
In Dart und Java hat jede Variable einen Typ. Die Liste des Supermarkts ist
eine Liste von `Checkout`, und doch darf eine Expresskasse darin stehen: Sie
ist eine Kasse, weil sie erbt. Aufgerufen wird trotzdem ihre eigene Methode.
<!-- /language -->

## Der Samstag

Jetzt spielt das Programm den Vormittag nach. Lade die Kundinnen und Kunden in
den Ordner: ein ausgedachter, aber typischer Samstag von neun bis elf, in
`customers.csv`. In jeder Zeile steht, wann jemand an den Kassen ankommt, in
Sekunden nach neun, und wie viele Artikel im Wagen liegen.

```bash_copy
cd ~/supermarkt
wget ${web_root}/supermarkt/customers.csv
```

```text
arrival,items
35,19
66,41
67,6
```

Die Simulation kommt in eine eigene Datei. Sie liest die Zeilen, legt für jede
eine Kundin an und schickt sie in den Supermarkt; am Ende gibt sie die mittlere
Wartezeit aus.

<!-- language: ruby -->
Lösch vorher die Zeilen zum Ausprobieren unten in `supermarket.rb`, sonst
laufen sie bei jedem Start mit. `require_relative` lädt die Klassen aus
`supermarket.rb`. Die Simulation, in `simulation.rb`:

```ruby_copy
require_relative 'supermarket'

def simulate(checkouts)
  supermarket = Supermarket.new(checkouts)
  File.readlines('customers.csv').drop(1).each do |line|
    arrival, items = line.split(',').map(&:to_i)
    supermarket.arrive(Customer.new(items, arrival))
  end
  supermarket.average_wait
end

puts simulate([Checkout.new, Checkout.new, Checkout.new])
```

Starte sie im Ordner `supermarkt` mit `ruby simulation.rb`.
<!-- /language -->
<!-- language: python -->
Lösch vorher die Zeilen zum Ausprobieren unten in `supermarket.py`, sonst
laufen sie bei jedem Start mit. `from supermarket import …` lädt die Klassen aus
`supermarket.py`. Die Simulation, in `simulation.py`:

```python_copy
from supermarket import Customer, Checkout, ExpressCheckout, SelfCheckout, Supermarket


def simulate(checkouts):
    supermarket = Supermarket(checkouts)
    with open('customers.csv') as f:
        for line in f.readlines()[1:]:
            arrival, items = [int(x) for x in line.split(',')]
            supermarket.arrive(Customer(items, arrival))
    return supermarket.average_wait()


print(simulate([Checkout(), Checkout(), Checkout()]))
```

Starte sie im Ordner `supermarkt` mit `python3 simulation.py`.
<!-- /language -->
<!-- language: javascript -->
Lösch vorher die Zeilen zum Ausprobieren unten in `supermarket.js` und schreib
stattdessen eine Zeile, die die Klassen für andere Dateien freigibt:

```javascript
module.exports = { Customer, Checkout, ExpressCheckout, SelfCheckout, Supermarket };
```

`require` lädt sie in der Simulation, in `simulation.js`:

```javascript_copy
const fs = require('fs');
const { Customer, Checkout, ExpressCheckout, SelfCheckout, Supermarket } = require('./supermarket');

function simulate(checkouts) {
  const supermarket = new Supermarket(checkouts);
  for (const line of fs.readFileSync('customers.csv', 'utf8').trim().split('\n').slice(1)) {
    const [arrival, items] = line.split(',').map(Number);
    supermarket.arrive(new Customer(items, arrival));
  }
  return supermarket.averageWait();
}

console.log(simulate([new Checkout(), new Checkout(), new Checkout()]));
```

Starte sie im Ordner `supermarkt` mit `node simulation.js`.
<!-- /language -->
<!-- language: dart -->
Lösch vorher `main` in `supermarket.dart`; die Simulation bekommt ihr eigenes.
`import` lädt die Klassen aus `supermarket.dart`. Die Simulation, in
`simulation.dart`:

```dart_copy
import 'dart:io';

import 'supermarket.dart';

double simulate(List<Checkout> checkouts) {
  final supermarket = Supermarket(checkouts);
  for (final line in File('customers.csv').readAsLinesSync().skip(1)) {
    final parts = line.split(',').map(int.parse).toList();
    supermarket.arrive(Customer(parts[1], parts[0]));
  }
  return supermarket.averageWait();
}

void main() {
  print(simulate([Checkout(), Checkout(), Checkout()]));
}
```

Starte sie im Ordner `supermarkt` mit `dart simulation.dart`.
<!-- /language -->
<!-- language: java -->
Die Simulation kommt in `Simulation.java`; `Main.java` zum Ausprobieren kannst
du löschen.

```java_copy
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class Simulation {
    static double simulate(List<Checkout> checkouts) throws Exception {
        Supermarket supermarket = new Supermarket(checkouts);
        List<String> lines = Files.readAllLines(Path.of("customers.csv"));
        for (String line : lines.subList(1, lines.size())) {
            String[] parts = line.split(",");
            supermarket.arrive(new Customer(Integer.parseInt(parts[1]), Integer.parseInt(parts[0])));
        }
        return supermarket.averageWait();
    }

    public static void main(String[] args) throws Exception {
        System.out.println(simulate(List.of(new Checkout(), new Checkout(), new Checkout())));
    }
}
```

Starte sie im Ordner `supermarkt` mit `java Simulation.java`.
<!-- /language -->

_task(drei-kassen)

_task(expresskasse-vorhersage)

Ändere die Liste der Kassen in der letzten Zeile: zwei normale Kassen und eine
Expresskasse.

_task(expresskasse)

<!-- after: expresskasse -->
Die Expresskasse lohnt sich nicht: Im Mittel warten die Leute 118 statt 70
Sekunden. Wer höchstens zehn Artikel hat, wartet zwar kürzer, 40 statt 58
Sekunden. Wer mehr hat, wartet aber 197 statt 82 Sekunden, denn für die vollen
Wagen bleiben nur zwei Kassen, während die Expresskasse immer wieder leer steht,
wenn gerade niemand mit wenigen Artikeln kommt. Drei Kassen für alle bedienen
mehr als zwei für alle und eine nur für manche.
<!-- /after -->

Und die Selbstbedienung? Probier die dritte Aufteilung: zwei normale Kassen und
zwei Selbstbedienungskassen.

_task(selbstbedienung)

<!-- after: selbstbedienung -->
Mit zwei Selbstbedienungskassen warten die Leute im Mittel nur 12 Sekunden,
obwohl jede bei vollen Wagen langsamer ist als eine Kasse mit Kassiererin: Es
sind vier Kassen statt drei, und bei wenigen Artikeln ist die
Selbstbedienungskasse sogar schneller, weil das Bezahlen kürzer dauert.
<!-- /after -->

### Was die Simulation nicht weiß

Die Zahlen sind so gut wie die Annahmen dahinter. In der Simulation sieht jede
Kundin, welche Kasse am frühesten frei ist; echte Leute sehen nur die Schlange
und schätzen. Niemand wechselt die Schlange, niemand braucht an der
Selbstbedienungskasse Hilfe, und die Kundinnen und Kunden sind ausgedacht. Ein
Modell lässt immer etwas weg; wichtig ist, dass man weiß, was, und ob es die
Antwort ändern könnte.

## Wie geht es weiter?

- Gib die mittlere Wartezeit getrennt aus, für Wagen mit höchstens zehn
  Artikeln und für die anderen. Der Supermarkt braucht dafür zwei Zähler mehr.
- Was ändert sich, wenn die Expresskasse nur bis fünf Artikel nimmt, oder bis
  fünfzehn?
- Die längste Wartezeit sagt oft mehr als die mittlere: Wer fünf Minuten
  steht, kommt vielleicht nicht wieder.
- Eine echte Schlange: Jede Kasse hält ihre Kundinnen in einer
  [Queue](/reference/listen#queue), und die Kundinnen stellen sich dort an, wo die
  wenigsten stehen. Kommt dasselbe heraus?
- Jede neue Idee beginnt wieder im Diagramm: Welche Klasse ändert sich, welche
  kommt dazu?
