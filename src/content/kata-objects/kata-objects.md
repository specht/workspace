<div class='meta'>
icon: bi-boxes
kind: uebung
duration: 2 Doppelstunden
languages: [ruby, python, javascript, dart, java]
idea: Klassen schreiben, Objekte bauen, Zustand hüten und vererben
curriculum: "Sek I: Algorithmisches Problemlösen (Grundlagen der Objektorientierung); Oberstufe: objektorientierte Programmierung (Klassen, Objekte, Attribute, Methoden, Vererbung)"
topics: [objektorientierung]
requires: [kata-dicts]
</div>

# Katas: Objekte

<p class='abstract'>
Du schreibst Klassen und baust daraus Objekte: jedes mit seinen eigenen
Daten und mit Methoden, die damit arbeiten. Ein Objekt passt auf seinen
Zustand auf, kennt andere Objekte, zeigt sich als Text und erbt von einer
Klasse, was es nicht selbst anders macht.
</p>

## Klassen und Objekte

### Eine Klasse, viele Objekte

Eine **Klasse** ist ein Bauplan. Aus ihm baut man **Objekte**, so viele man
will, und jedes hat seine eigenen Daten, die **Attribute**. Was ein Objekt
kann, steht in seinen **Methoden**. Dieser Zähler fängt bei 0 an und zählt
bei jedem `increment` eins weiter:

<!-- language: ruby -->
```ruby
class Counter
  def initialize
    @value = 0
  end

  def increment
    @value += 1
  end

  def value
    @value
  end
end

clicks = Counter.new
clicks.increment
puts clicks.value    # 1
```

`initialize` läuft, wenn `Counter.new` ein neues Objekt baut. Ein Name mit
`@` davor ist ein Attribut: Er gehört dem Objekt, nicht der Methode, und
bleibt, wenn die Methode fertig ist.
<!-- /language -->
<!-- language: python -->
```python
class Counter:
    def __init__(self):
        self.value = 0

    def increment(self):
        self.value += 1

clicks = Counter()
clicks.increment()
print(clicks.value)    # 1
```

`__init__` läuft, wenn `Counter()` ein neues Objekt baut. `self` ist das
Objekt selbst; `self.value` ist ein Attribut: Es gehört dem Objekt, nicht
der Methode, und bleibt, wenn die Methode fertig ist. Jede Methode bekommt
`self` als ersten Parameter, beim Aufruf schreibt man es nicht hin.
<!-- /language -->
<!-- language: javascript -->
```javascript
class Counter {
  constructor() {
    this.value = 0;
  }

  increment() {
    this.value += 1;
  }
}

const clicks = new Counter();
clicks.increment();
console.log(clicks.value);    // 1
```

`constructor` läuft, wenn `new Counter()` ein neues Objekt baut. `this` ist
das Objekt selbst; `this.value` ist ein Attribut: Es gehört dem Objekt,
nicht der Methode, und bleibt, wenn die Methode fertig ist.
<!-- /language -->
<!-- language: dart -->
```dart
class Counter {
  int value = 0;

  void increment() {
    value += 1;
  }
}

void main() {
  final clicks = Counter();
  clicks.increment();
  print(clicks.value);    // 1
}
```

`Counter()` baut ein neues Objekt. `value` ist ein Attribut: Es gehört dem
Objekt, nicht der Methode, und bleibt, wenn die Methode fertig ist. In den
Methoden schreibt man es einfach `value`; wer es genauer sagen will,
schreibt `this.value`, »das `value` dieses Objekts«.
<!-- /language -->
<!-- language: java -->
```java
class Counter {
    int value = 0;

    void increment() {
        value += 1;
    }
}

void main() {
    Counter clicks = new Counter();
    clicks.increment();
    IO.println(clicks.value);    // 1
}
```

`new Counter()` baut ein neues Objekt. `value` ist ein Attribut: Es gehört
dem Objekt, nicht der Methode, und bleibt, wenn die Methode fertig ist. In
den Methoden schreibt man es einfach `value`; wer es genauer sagen will,
schreibt `this.value`, »das `value` dieses Objekts«.
<!-- /language -->

_task(predict-two-counters)

<!-- after: predict-two-counters -->
`a` zählt bis 2, `b` bis 1: Jedes Objekt hat sein eigenes Attribut `value`.
Die Klasse gibt nur vor, *dass* es eins gibt; welchen Wert es hat, steht in
jedem Objekt für sich.
<!-- /after -->

Methoden können auch Parameter haben, wie Funktionen. Das Objekt bekommt
seine Daten beim Bauen mit:

<!-- language: ruby -->
```ruby
class Circle
  def initialize(radius)
    @radius = radius
  end

  def diameter
    2 * @radius
  end
end

puts Circle.new(5).diameter    # 10
```
<!-- /language -->
<!-- language: python -->
```python
class Circle:
    def __init__(self, radius):
        self.radius = radius

    def diameter(self):
        return 2 * self.radius

print(Circle(5).diameter())    # 10
```
<!-- /language -->
<!-- language: javascript -->
```javascript
class Circle {
  constructor(radius) {
    this.radius = radius;
  }

  diameter() {
    return 2 * this.radius;
  }
}

console.log(new Circle(5).diameter());    // 10
```
<!-- /language -->
<!-- language: dart -->
```dart
class Circle {
  final int radius;

  Circle(this.radius);

  int diameter() {
    return 2 * radius;
  }
}

void main() {
  print(Circle(5).diameter());    // 10
}
```

`Circle(this.radius);` ist der Konstruktor: Er läuft, wenn `Circle(5)` ein
Objekt baut, und setzt das Attribut `radius` gleich auf den Wert des
Parameters.
<!-- /language -->
<!-- language: java -->
```java
class Circle {
    private int radius;

    Circle(int radius) {
        this.radius = radius;
    }

    int diameter() {
        return 2 * radius;
    }
}

void main() {
    IO.println(new Circle(5).diameter());    // 10
}
```

`Circle(int radius) { … }` ist der Konstruktor: Er läuft, wenn
`new Circle(5)` ein Objekt baut. `this.radius = radius;` setzt das Attribut
auf den Wert des Parameters, der genauso heißt. `private` heißt: Nur die
Methoden der Klasse kommen an das Attribut.
<!-- /language -->

_task(rectangle)

### Ein Objekt passt auf sich auf

Ein Objekt ändert seine Attribute nur in seinen Methoden. Darum kann es
Regeln durchsetzen: Ein Konto lässt sich nicht ins Minus bringen, wenn die
Methode, die abhebt, vorher nachsieht. Wer das Konto benutzt, muss die Regel
nicht kennen; er ruft die Methode auf, und das Objekt entscheidet. Das heißt
**Kapselung**.

_task(account)

### Objekte, die Objekte kennen

Ein Attribut kann auch ein anderes Objekt sein, oder eine Liste von
Objekten. Eine Playlist kennt ihre Songs, und jeder Song kennt seinen Titel
und seine Länge:

<!-- language: ruby -->
```ruby
class Song
  attr_reader :title, :seconds

  def initialize(title, seconds)
    @title = title
    @seconds = seconds
  end
end

class Playlist
  def initialize
    @songs = []
  end

  def add(song)
    @songs << song
  end

  def count
    @songs.length
  end
end

mix = Playlist.new
mix.add(Song.new("Intro", 95))
mix.add(Song.new("Outro", 210))
puts mix.count    # 2
```

`attr_reader :title, :seconds` schreibt die Methoden `title` und `seconds`,
die nur den Wert des Attributs zurückgeben, damit man von außen
`song.title` fragen kann.
<!-- /language -->
<!-- language: python -->
```python
class Song:
    def __init__(self, title, seconds):
        self.title = title
        self.seconds = seconds

class Playlist:
    def __init__(self):
        self.songs = []

    def add(self, song):
        self.songs.append(song)

    def count(self):
        return len(self.songs)

mix = Playlist()
mix.add(Song("Intro", 95))
mix.add(Song("Outro", 210))
print(mix.count())    # 2
```
<!-- /language -->
<!-- language: javascript -->
```javascript
class Song {
  constructor(title, seconds) {
    this.title = title;
    this.seconds = seconds;
  }
}

class Playlist {
  constructor() {
    this.songs = [];
  }

  add(song) {
    this.songs.push(song);
  }

  count() {
    return this.songs.length;
  }
}

const mix = new Playlist();
mix.add(new Song("Intro", 95));
mix.add(new Song("Outro", 210));
console.log(mix.count());    // 2
```
<!-- /language -->
<!-- language: dart -->
```dart
class Song {
  final String title;
  final int seconds;

  Song(this.title, this.seconds);
}

class Playlist {
  final songs = <Song>[];

  void add(Song song) {
    songs.add(song);
  }

  int count() {
    return songs.length;
  }
}

void main() {
  final mix = Playlist();
  mix.add(Song('Intro', 95));
  mix.add(Song('Outro', 210));
  print(mix.count());    // 2
}
```
<!-- /language -->
<!-- language: java -->
```java
class Song {
    String title;
    int seconds;

    Song(String title, int seconds) {
        this.title = title;
        this.seconds = seconds;
    }
}

class Playlist {
    List<Song> songs = new ArrayList<>();

    void add(Song song) {
        songs.add(song);
    }

    int count() {
        return songs.size();
    }
}

void main() {
    Playlist mix = new Playlist();
    mix.add(new Song("Intro", 95));
    mix.add(new Song("Outro", 210));
    IO.println(mix.count());    // 2
}
```
<!-- /language -->

_task(playlist)

### Wie ein Objekt sich zeigt

Macht man aus einem Objekt einfach Text, steht da wenig: in Ruby und Python
die Klasse und wo das Objekt im Speicher liegt, in Java die Klasse und eine
Kennzahl, in Dart `Instance of 'Point'`, in JavaScript nur
`[object Object]`. Was ein Mensch lesen soll, bestimmt eine Methode mit
einem festen Namen:

<!-- language: ruby -->
```ruby
class Point
  def initialize(x, y)
    @x = x
    @y = y
  end

  def to_s
    "(#{@x}, #{@y})"
  end
end

puts Point.new(3, 4)    # (3, 4)
```

`puts` ruft `to_s` auf, und `"Punkt #{p}"` auch.
<!-- /language -->
<!-- language: python -->
```python
class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __str__(self):
        return f"({self.x}, {self.y})"

print(Point(3, 4))    # (3, 4)
```

`print` und `str(p)` rufen `__str__` auf, und `f"Punkt {p}"` auch.
<!-- /language -->
<!-- language: javascript -->
```javascript
class Point {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  toString() {
    return `(${this.x}, ${this.y})`;
  }
}

console.log(`${new Point(3, 4)}`);    // (3, 4)
```

`String(p)` und `` `Punkt ${p}` `` rufen `toString` auf. `console.log(p)`
allein zeigt dagegen die Attribute, nicht `toString`.
<!-- /language -->
<!-- language: dart -->
```dart
class Point {
  final int x;
  final int y;

  Point(this.x, this.y);

  @override
  String toString() {
    return '($x, $y)';
  }
}

void main() {
  print(Point(3, 4));    // (3, 4)
}
```

`print(p)` und `'Punkt $p'` rufen `toString` auf. Jedes Objekt hat schon
eins, das `Instance of 'Point'` zurückgibt; `@override` sagt, dass deins es
ersetzt.
<!-- /language -->
<!-- language: java -->
```java
class Point {
    private int x;
    private int y;

    Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public String toString() {
        return "(" + x + ", " + y + ")";
    }
}

void main() {
    IO.println(new Point(3, 4));    // (3, 4)
}
```

`IO.println(p)` und `"Punkt " + p` rufen `toString` auf. Jedes Objekt hat
schon eins, das die Klasse und eine Kennzahl zurückgibt, `Point@1b6d3586`;
`@Override` sagt, dass deins es ersetzt. Es muss `public` sein wie das, das
es ersetzt.
<!-- /language -->

_task(money)

## Vererbung

Manche Klassen sind fast gleich. Statt alles zweimal zu schreiben, **erbt**
eine Klasse von einer anderen: Sie bekommt deren Attribute und Methoden und
schreibt nur, was bei ihr anders ist. Diese Methode heißt dann
**überschrieben**:

<!-- language: ruby -->
```ruby
class Animal
  def initialize(name)
    @name = name
  end

  def sound
    "..."
  end

  def describe
    "#{@name} sagt #{sound}"
  end
end

class Dog < Animal
  def sound
    "Wuff"
  end
end

class Cat < Animal
  def sound
    "Miau"
  end
end
```

`class Dog < Animal` heißt: `Dog` erbt von `Animal`.
<!-- /language -->
<!-- language: python -->
```python
class Animal:
    def __init__(self, name):
        self.name = name

    def sound(self):
        return "..."

    def describe(self):
        return f"{self.name} sagt {self.sound()}"

class Dog(Animal):
    def sound(self):
        return "Wuff"

class Cat(Animal):
    def sound(self):
        return "Miau"
```

`class Dog(Animal)` heißt: `Dog` erbt von `Animal`.
<!-- /language -->
<!-- language: javascript -->
```javascript
class Animal {
  constructor(name) {
    this.name = name;
  }

  sound() {
    return "...";
  }

  describe() {
    return `${this.name} sagt ${this.sound()}`;
  }
}

class Dog extends Animal {
  sound() {
    return "Wuff";
  }
}

class Cat extends Animal {
  sound() {
    return "Miau";
  }
}
```

`class Dog extends Animal` heißt: `Dog` erbt von `Animal`.
<!-- /language -->
<!-- language: dart -->
```dart
class Animal {
  final String name;

  Animal(this.name);

  String sound() {
    return '...';
  }

  String describe() {
    return '$name sagt ${sound()}';
  }
}

class Dog extends Animal {
  Dog(super.name);

  @override
  String sound() {
    return 'Wuff';
  }
}

class Cat extends Animal {
  Cat(super.name);

  @override
  String sound() {
    return 'Miau';
  }
}
```

`class Dog extends Animal` heißt: `Dog` erbt von `Animal`. Konstruktoren
erbt eine Klasse nicht; `Dog(super.name);` gibt den Namen an den
Konstruktor von `Animal` weiter.
<!-- /language -->
<!-- language: java -->
```java
class Animal {
    String name;

    Animal(String name) {
        this.name = name;
    }

    String sound() {
        return "...";
    }

    String describe() {
        return name + " sagt " + sound();
    }
}

class Dog extends Animal {
    Dog(String name) {
        super(name);
    }

    @Override
    String sound() {
        return "Wuff";
    }
}

class Cat extends Animal {
    Cat(String name) {
        super(name);
    }

    @Override
    String sound() {
        return "Miau";
    }
}
```

`class Dog extends Animal` heißt: `Dog` erbt von `Animal`. Konstruktoren
erbt eine Klasse nicht; `super(name);` gibt den Namen an den Konstruktor
von `Animal` weiter.
<!-- /language -->

_task(predict-describe)

<!-- after: predict-describe -->
`describe` steht nur in `Animal`, und trotzdem kommt `Bello sagt Wuff`
heraus: Ruft `describe` die Methode `sound` auf, nimmt die Sprache die des
Objekts, und das ist ein `Dog`. Dieselbe Zeile Code tut also je nach Objekt
etwas anderes. Das heißt **Polymorphie**: Wer `describe` aufruft, muss nicht
wissen, ob er einen Hund oder eine Katze vor sich hat.
<!-- /after -->

_task(total-area)

Soll eine überschriebene Methode die der Oberklasse nicht ersetzen, sondern
ergänzen, ruft sie sie mit `super` auf:

<!-- language: ruby -->
```ruby
class Puppy < Dog
  def sound
    super + " (leise)"
  end
end

puts Puppy.new("Fips").describe    # Fips sagt Wuff (leise)
```

In `initialize` gibt `super(name)` den Namen an das `initialize` der
Oberklasse weiter.
<!-- /language -->
<!-- language: python -->
```python
class Puppy(Dog):
    def sound(self):
        return super().sound() + " (leise)"

print(Puppy("Fips").describe())    # Fips sagt Wuff (leise)
```

In `__init__` gibt `super().__init__(name)` den Namen an das `__init__` der
Oberklasse weiter.
<!-- /language -->
<!-- language: javascript -->
```javascript
class Puppy extends Dog {
  sound() {
    return super.sound() + " (leise)";
  }
}

console.log(new Puppy("Fips").describe());    // Fips sagt Wuff (leise)
```

Im `constructor` gibt `super(name)` den Namen an den `constructor` der
Oberklasse weiter; erst danach darf man `this` benutzen.
<!-- /language -->
<!-- language: dart -->
```dart
class Puppy extends Dog {
  Puppy(super.name);

  @override
  String sound() {
    return '${super.sound()} (leise)';
  }
}

void main() {
  print(Puppy('Fips').describe());    // Fips sagt Wuff (leise)
}
```

Im Konstruktor gibt `: super(name)` den Namen an den Konstruktor der
Oberklasse weiter; `Puppy(super.name);` ist die kurze Form davon.
<!-- /language -->
<!-- language: java -->
```java
class Puppy extends Dog {
    Puppy(String name) {
        super(name);
    }

    @Override
    String sound() {
        return super.sound() + " (leise)";
    }
}

void main() {
    IO.println(new Puppy("Fips").describe());    // Fips sagt Wuff (leise)
}
```

Im Konstruktor gibt `super(name);` den Namen an den Konstruktor der
Oberklasse weiter; es steht in seiner ersten Zeile.
<!-- /language -->

_task(manager)

Klassen, Objekte und Vererbung gibt es seit 1967, aus einer Sprache für
Simulationen.

_person(nygaard-dahl)

## Zum Nachschlagen

- Was eine Klasse im Großen ausmacht, wie man sie aus einer Geschichte
  findet und als Klassendiagramm zeichnet, zeigt die Aktivität
  [Klassen entwerfen: die Expresskasse](/supermarkt).

<!-- extra -->
## Extras

Zwei Katas, die nicht mitzählen: ein Stack als Klasse, und ein Bruch, der
rechnen kann.

_task(stack)

_task(fraction)
