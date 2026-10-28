<div class='meta'>
icon: bi-pencil
kind: uebung
duration: 2 Doppelstunden
languages: [ruby, python, javascript]
idea: Eine Schildkröte zeichnet, was du ihr sagst
curriculum: "Sek I: Algorithmisches Problemlösen (algorithmische Grundstrukturen, Implementieren von Algorithmen), Digitale Bilder und Visualisierung (Vektorgrafik, rekursive Algorithmen für Grafiken), Geschichte der Informatik (Lebensläufe und Leistungen bedeutender Persönlichkeiten)"
topics: [grundstrukturen, unterprogramme, rekursion, computergrafik, geschichte]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2">
</div>

# Turtle: Bilder aus Befehlen

<p class='abstract'>
Eine Schildkröte läuft über ein Blatt Papier und zieht mit einem Stift eine
Linie hinter sich her. Du sagst ihr, was sie tun soll: vorwärts gehen, sich
nach links drehen, sich nach rechts drehen. Aus ein paar Befehlen wird ein
Quadrat, aus einer Schleife ein Stern, aus einer eigenen Funktion eine
Blume und am Ende ein Bild, das du dir selbst ausdenkst.
</p>

## Die Schildkröte

In der Mitte des Papiers sitzt die Schildkröte und schaut nach rechts. Mit
`forward` geht sie so viele Schritte geradeaus, wie du ihr sagst, und
zeichnet dabei eine Linie. Mit `left` dreht sie sich auf der Stelle nach
links, um so viele Grad, wie du ihr sagst, mit `right` nach rechts; `back`
geht rückwärts. Ein Kästchen auf dem Papier ist 50 Schritte breit;
zeichnest du etwas Großes, zoomt das Papier heraus.

Dieses Programm zeichnet ein L, das auf dem Rücken liegt: 100 Schritte nach
rechts, eine Vierteldrehung nach links, 50 Schritte nach oben.

<!-- language: ruby -->
```ruby
forward 100
left 90
forward 50
```
<!-- /language -->
<!-- language: python -->
```python
forward(100)
left(90)
forward(50)
```
<!-- /language -->
<!-- language: javascript -->
```javascript
forward(100);
left(90);
forward(50);
```
<!-- /language -->

Dein Programm schreibst du in eine Karte wie die folgenden; *Ausführen*
startet es. Die Schildkröte zeichnet, während es läuft, und die Zeile, an
der sie gerade ist, leuchtet im Programm. Wie schnell sie läuft, stellst du
unter dem Papier ein. Dein Programm läuft in deinem Browser; ist es zu Ende,
vergleicht der Workspace dein Bild mit dem grauen Bild auf dem Papier.

_task(steps)

_task(square)

_task(triangle-angle)

<!-- after: triangle-angle -->
Die Schildkröte dreht sich nicht um den Winkel, der innen in der Ecke
liegt, sondern um so viel, wie sich ihre Richtung ändert. Ginge sie
geradeaus weiter, wären es 0 Grad; kehrte sie ganz um, 180. Bei einer Ecke
von 60 Grad dreht sie sich um 180 − 60 = 120 Grad.

Zusammen sind das dreimal 120 = 360 Grad, einmal ganz herum, genau wie beim
Quadrat mit viermal 90. Das gilt für jede Figur, die die Schildkröte einmal
umrundet: Am Ende schaut sie wieder in die Richtung vom Anfang, und alle
Drehungen ergeben zusammen 360 Grad.
<!-- /after -->

_task(triangle)

## Wiederholen

Beim Quadrat hast du zweimal dieselben zwei Zeilen geschrieben, und noch
zweimal. Was sich wiederholt, macht eine **Schleife**. Diese zeichnet eine
Mauer mit fünf Zinnen:

<!-- language: ruby -->
```ruby
5.times do
  forward 20
  left 90
  forward 20
  right 90
  forward 20
  right 90
  forward 20
  left 90
end
```

`5.times do` heißt: fünfmal. Was eingerückt bis zum `end` steht, wiederholt
Ruby.
<!-- /language -->
<!-- language: python -->
```python
for i in range(5):
    forward(20)
    left(90)
    forward(20)
    right(90)
    forward(20)
    right(90)
    forward(20)
    left(90)
```

`for i in range(5):` heißt: fünfmal. Was darunter eingerückt steht,
wiederholt Python; `i` zählt dabei mit, von 0 bis 4.
<!-- /language -->
<!-- language: javascript -->
```javascript
for (let i = 0; i < 5; i++) {
  forward(20);
  left(90);
  forward(20);
  right(90);
  forward(20);
  right(90);
  forward(20);
  left(90);
}
```

`for (let i = 0; i < 5; i++)` heißt: fünfmal. Was zwischen den geschweiften
Klammern steht, wiederholt JavaScript; `i` zählt dabei mit, von 0 bis 4.
<!-- /language -->

_task(square-loop)

_task(hexagon)

Bisher hat die Schildkröte nachgezeichnet. Jetzt bist du dran, mit einem
Muster, das sich niemand vorher ausgedacht hat:

_task(pattern)

<div class='hint try'>
<strong>Ausprobieren:</strong> Ein Kreis ist für die Schildkröte ein Vieleck
mit sehr vielen Ecken. Lass sie 360-mal 2 Schritte gehen und sich jedes Mal
um 1 Grad drehen. Achte darauf, ob du die Ecken noch siehst, und wie groß
der Kreis wird, wenn sie 3 Schritte geht.
</div>

## Eigene Befehle

Die Schildkröte kennt nur ein paar Befehle, aber du kannst ihr neue
beibringen: Eine **Funktion** bekommt einen Namen und die Befehle, die sie
ausführen soll. Was in Klammern hinter dem Namen steht, ist ein
**Parameter**: ein Wert, den man ihr beim Aufruf mitgibt. Diese Funktion
zeichnet eine Treppenstufe in der Größe, die sie bekommt, und drei Aufrufe
zeichnen drei Stufen, die immer größer werden:

<!-- language: ruby -->
```ruby
def stair(size)
  forward size
  left 90
  forward size
  right 90
end

stair(20)
stair(40)
stair(60)
```

`def` beginnt die Funktion, `end` beendet sie. Erst die Aufrufe darunter
lassen die Schildkröte zeichnen; beim ersten ist `size` 20, beim zweiten 40.
<!-- /language -->
<!-- language: python -->
```python
def stair(size):
    forward(size)
    left(90)
    forward(size)
    right(90)

stair(20)
stair(40)
stair(60)
```

`def` beginnt die Funktion; was darunter eingerückt steht, gehört zu ihr.
Erst die Aufrufe darunter lassen die Schildkröte zeichnen; beim ersten ist
`size` 20, beim zweiten 40.
<!-- /language -->
<!-- language: javascript -->
```javascript
function stair(size) {
  forward(size);
  left(90);
  forward(size);
  right(90);
}

stair(20);
stair(40);
stair(60);
```

`function` beginnt die Funktion; was zwischen den geschweiften Klammern
steht, gehört zu ihr. Erst die Aufrufe darunter lassen die Schildkröte
zeichnen; beim ersten ist `size` 20, beim zweiten 40.
<!-- /language -->

Bei den folgenden Aufgaben ruft der Workspace deine Funktion selbst auf,
mehrmals und mit anderen Werten, jedes Mal auf einem frischen Blatt. Was
dein Programm sonst noch zeichnet, zählt dabei nicht.

_task(square-function)

_task(polygon)

_task(flower)

_task(own-flower)

## Stift und Farbe

<!-- language: ruby -->
Mit `pen_up` hebt die Schildkröte ihren Stift: Sie geht weiter, ohne zu
zeichnen, bis `pen_down` ihn wieder aufsetzt. `color` gibt dem Stift eine
Farbe, mit einem englischen Namen wie `"red"`, `"orange"` oder
`"darkgreen"`; `width` macht die Linie dicker. Diese Linie ist erst dick und
grün, dann kommt eine Lücke, dann geht sie dünn und rot weiter:

```ruby
width 6
color "green"
forward 80
pen_up
forward 40
pen_down
width 2
color "red"
forward 80
```
<!-- /language -->
<!-- language: python -->
Mit `pen_up()` hebt die Schildkröte ihren Stift: Sie geht weiter, ohne zu
zeichnen, bis `pen_down()` ihn wieder aufsetzt. `color` gibt dem Stift eine
Farbe, mit einem englischen Namen wie `"red"`, `"orange"` oder
`"darkgreen"`; `width` macht die Linie dicker. Diese Linie ist erst dick und
grün, dann kommt eine Lücke, dann geht sie dünn und rot weiter:

```python
width(6)
color("green")
forward(80)
pen_up()
forward(40)
pen_down()
width(2)
color("red")
forward(80)
```
<!-- /language -->
<!-- language: javascript -->
Mit `penUp()` hebt die Schildkröte ihren Stift: Sie geht weiter, ohne zu
zeichnen, bis `penDown()` ihn wieder aufsetzt. `color` gibt dem Stift eine
Farbe, mit einem englischen Namen wie `"red"`, `"orange"` oder
`"darkgreen"`; `width` macht die Linie dicker. Diese Linie ist erst dick und
grün, dann kommt eine Lücke, dann geht sie dünn und rot weiter:

```javascript
width(6);
color("green");
forward(80);
penUp();
forward(40);
penDown();
width(2);
color("red");
forward(80);
```
<!-- /language -->

Welche Namen es gibt, steht in der [Liste der Farbnamen](https://developer.mozilla.org/de/docs/Web/CSS/named-color)
von MDN, mit einem Kästchen in jeder Farbe; statt eines Namens geht auch
eine Farbe wie `"#ff8800"`.

_task(dashes)

_task(colors)

## Spiralen

Eine **Variable** merkt sich einen Wert unter einem Namen, und der Wert
darf sich ändern. Hier wird jede Stufe 10 Schritte größer als die davor:

<!-- language: ruby -->
```ruby
size = 10
6.times do
  forward size
  left 90
  forward size
  right 90
  size = size + 10
end
```
<!-- /language -->
<!-- language: python -->
```python
size = 10
for i in range(6):
    forward(size)
    left(90)
    forward(size)
    right(90)
    size = size + 10
```
<!-- /language -->
<!-- language: javascript -->
```javascript
let size = 10;
for (let i = 0; i < 6; i++) {
  forward(size);
  left(90);
  forward(size);
  right(90);
  size = size + 10;
}
```
<!-- /language -->

`size = size + 10` rechnet mit dem alten Wert und merkt sich den neuen:
Aus 10 wird 20, aus 20 wird 30.

_task(spiral)

_task(spiral-91)

<!-- after: spiral-91 -->
Bei 90 Grad liegt jede Ecke genau über einer Ecke von vorher. Bei 91 Grad
ist jede Ecke um ein Grad weiter gedreht als die davor; nach vielen Ecken
werden daraus viele Grad, und die Arme der Spirale biegen sich zu einem
Wirbel. Kleine Änderungen, die sich wiederholen, werden groß.
<!-- /after -->

<div class='hint try'>
<strong>Ausprobieren:</strong> Lass deine Spirale mit 100 statt 20 Linien
laufen und probier als Drehung 89, 91, 120, 121 und 144. Achte darauf, in
welche Richtung sich die Arme drehen und wie viele Arme es sind.
</div>

## Sterne

_task(star)

Die Schildkröte, die über den Bildschirm läuft, ist über 50 Jahre alt.
Erfunden wurde sie für Logo, eine Programmiersprache, mit der Kinder
programmieren lernen sollten; die erste Schildkröte war ein Roboter auf dem
Fußboden.

<!-- Die Seite ist noch in Entwicklung: Die Personenbox für Seymour Papert (part papert, papert.webp, ihre Quellen in sources.yaml) steht in Commit ff1b480f und kommt mit _person(papert) zurück, wenn die Seite in den Katalog geht. -->

<div class='book'>
<i class='bi bi-book'></i>
<p><strong>Mindstorms</strong>, Seymour Papert, 1980. Das Buch, in dem Papert
beschreibt, wie Kinder mit der Schildkröte lernen: Sie spielen selbst
Schildkröte, laufen ein Quadrat ab und schreiben dann auf, was sie getan
haben.</p>
</div>

## Kreise und Federn

Einen echten Kreis kann die Schildkröte nicht zeichnen, nur gerade Linien.
Aber ein Vieleck mit vielen Ecken sieht aus wie ein Kreis, wenn seine
Seiten kurz genug sind. Wie lang, verrät der Umfang: Ein Kreis mit dem
Radius r ist 2 · π · r lang, und π ist ungefähr 3,14.

_task(circle)

_task(slinky)

<div class='hint try'>
<strong>Ausprobieren:</strong> Dreh die Schildkröte nach jedem Kreis ein
wenig, um 1,5 Grad zum Beispiel, und gib jedem Kreis eine andere Farbe. Achte
darauf, wie sich die Feder biegt, und wie viele Kreise sie braucht, bis
sie einmal herum ist.
</div>

## Dein eigenes Bild

Jetzt du: Was du in den Aufgaben gebaut hast, darfst du alles
wiederverwenden. Gefällt dir dein Bild, kannst du es unter der Aufgabe
einreichen: Deine Lehrkraft kann es dann in der Stunde zeigen, ohne deinen
Namen.

_task(own)

<!-- extra -->
## Bäume und Kurven

Aufgaben, die nicht mitzählen, für alle, die weiter wollen. Bis auf die
erste sind sie **rekursiv**: Eine Funktion ruft sich selbst auf, mit einer
kleineren Aufgabe, bis die so klein ist, dass nichts mehr zu zeichnen
bleibt. Wie das geht, üben auch die [Katas: Rekursion](/kata-recursion).

_task(stars)

### Ein Baum

Ein Baum ist ein Stamm, an dem links und rechts ein kleinerer Baum wächst.
Jeder davon ist wieder ein Stamm mit zwei kleineren Bäumen, und so weiter,
bis die Bäume so klein sind, dass die Funktion aufhört. Klick dich durch
die Stufen: Jede Stufe besteht aus einem Stamm und zwei Bäumen der Stufe
davor, in Farbe.

<div class='turtle-levels' data-figure='tree'></div>

Wer die Funktion schreibt, muss also nur zwei Fragen beantworten: Was
zeichnet die kleinste Stufe? Und wie setzt sich eine Stufe aus der davor
zusammen? Den Rest erledigen die Aufrufe von selbst.

_task(tree)

_task(beautiful-tree)

Ein Baum, in dem links und rechts dasselbe passiert, sieht aus wie gezeichnet.
Andere Winkel und andere Längen für jede Seite, und er wächst wie ein
echter:

_task(asymmetric-tree)

_task(weeping-tree)

_task(own-tree)

<div class='hint try'>
<strong>Ausprobieren:</strong> Lass die Winkel und die Längen bei jedem
Ast ein wenig vom Zufall abhängen
<!-- language: ruby -->
(`rand(15..35)` ist eine Zahl von 15 bis 35)
<!-- /language -->
<!-- language: python -->
(mit `import random` am Anfang ist `random.randint(15, 35)` eine Zahl von 15 bis 35)
<!-- /language -->
<!-- language: javascript -->
(`15 + Math.random() * 20` ist eine Zahl von 15 bis 35)
<!-- /language -->
und lass den Baum mehrmals wachsen. Achte darauf, ob er dann eher wie ein
echter Baum aussieht und welche Tiefe dafür reicht.
</div>

### Ein Farnblatt

Ein Farnblatt wächst wie ein Baum, nur anders: Sein Stiel biegt sich bei
jedem Schritt ein wenig, und an jedem Schritt wächst links und rechts ein
kleines Farnblatt. Was nach diesem Schritt kommt, ist wieder ein Farnblatt,
nur ein bisschen kleiner.

<div class='turtle-levels' data-figure='fern'></div>

_task(fern)

### Das Sierpiński-Dreieck

Wacław Sierpiński hat ein Dreieck beschrieben, das aus drei halb so großen
Dreiecken besteht, von denen jedes wieder aus dreien besteht:

<div class='turtle-levels' data-figure='sierpinski'></div>

_person(sierpinski)

_task(sierpinski)

### Die Koch-Kurve

Helge von Koch hat 1904 eine Kurve beschrieben, die aus vier Kurven mit
einem Drittel der Länge besteht. Je höher die Stufe, desto feiner die
Zacken; ein Stück von ihr sieht aus wie die ganze Kurve.

<div class='turtle-levels' data-figure='koch'></div>

_task(koch)

### Die Hilbert-Kurve

David Hilbert hat 1891 eine Kurve beschrieben, die durch ein Quadrat läuft,
ohne sich je zu kreuzen, und mit jeder Stufe jeder Stelle des Quadrats
näherkommt. Ihre Stufe 1 ist ein U aus drei Linien. Jede höhere Stufe setzt
vier Kurven der Stufe davor an die Ecken dieses U, verbunden durch seine
drei Linien. Damit sie sich zu einer Linie verbinden, sind die erste und
die letzte gespiegelt.

<div class='turtle-levels' data-figure='hilbert'></div>

_task(hilbert)
