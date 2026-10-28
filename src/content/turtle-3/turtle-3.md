<div class='meta'>
icon: bi-flower1
kind: uebung
duration: 2 Doppelstunden
languages: [ruby, python, javascript]
idea: Farben aus Zahlen, Merken und Zurückkehren, Zufall und Pflanzen, die aus Wörtern wachsen
curriculum: "Sek I: Algorithmisches Problemlösen (Zeichenkettenbearbeitung), Digitale Bilder und Visualisierung (Turtle-Grafik, Farbmodelle, rekursive Algorithmen für Grafiken)"
topics: [computergrafik, zeichenketten, grammatiken]
requires: [turtle-2]
age: Oberstufe
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2">
</div>

# Turtle: Pflanzen, Zufall und Farbe

<p class='abstract'>
Die Schildkröte bekommt drei neue Werkzeuge: Farben aus Zahlen, ein
Gedächtnis für Stellen, zu denen sie zurückkehren kann, und den Zufall.
Damit wachsen Bäume, die sich im Wind biegen, Wiesen, in denen kein Halm dem
anderen gleicht, und Pflanzen, die aus einem einzigen Wort entstehen. Am
Ende zeichnest du, was dir gefällt.
</p>

Die Befehle der Schildkröte kennst du aus [Turtle: Bilder aus
Befehlen](/turtle-1), Bäume, die sich selbst enthalten, aus [Turtle: Bäume
und Kurven](/turtle-2).

## Farben aus Zahlen

Bisher hatten Farben Namen. Gibst du `color` stattdessen eine Zahl, sucht
sich die Schildkröte die Farbe auf dem **Farbkreis** aus: 0 ist Rot, 60
Gelb, 120 Grün, 180 Türkis, 240 Blau, 300 Lila, und bei 360 ist sie wieder
bei Rot. Eine Zahl, die wächst, lässt die Farbe darum einmal durch den
Regenbogen wandern.

<!-- language: ruby -->
```ruby
color 0      # rot
color 120    # grün
color 200    # ein helles Blau
```
<!-- /language -->
<!-- language: python -->
```python
color(0)      # rot
color(120)    # grün
color(200)    # ein helles Blau
```
<!-- /language -->
<!-- language: javascript -->
```javascript
color(0);      // rot
color(120);    // grün
color(200);    // ein helles Blau
```
<!-- /language -->

_task(sun)

_task(rainbow-spiral)

Zwischen zwei Farben liegen viele andere. Mit zwei Namen und einem Anteil
dazwischen mischt `color` sie: Beim Anteil 0 ist es ganz die erste, bei 1
ganz die zweite, bei 0,5 genau die Mitte.

<!-- language: ruby -->
```ruby
color "gold", "crimson", 0.25    # ein Viertel des Wegs von Gold nach Rot
```
<!-- /language -->
<!-- language: python -->
```python
color("gold", "crimson", 0.25)    # ein Viertel des Wegs von Gold nach Rot
```
<!-- /language -->
<!-- language: javascript -->
```javascript
color("gold", "crimson", 0.25);    // ein Viertel des Wegs von Gold nach Rot
```
<!-- /language -->

_task(fan)

Auch das Papier bekommt eine Farbe: `background` nimmt dieselben Farben wie
`color`, einen Namen, eine Zahl aus dem Farbkreis oder eine Mischung. Ein
Klick auf `color` oder `background` über dem Papier öffnet Farben zum
Aussuchen, darunter die Palette Cling.

## Merken und zurückkehren

Beim Baum in »Bäume und Kurven« ist die Schildkröte nach jedem Ast den
ganzen Weg zurückgelaufen und hat sich zurückgedreht. Einfacher geht es,
wenn sie sich merkt, wo sie war: `push` merkt sich, wo die Schildkröte
steht und wohin sie schaut, dazu ihren Stift, ihre Farbe und ihre Breite.
`pop` bringt sie dorthin zurück, ohne zu zeichnen.

Merkt sie sich mehrere Stellen, kommt bei `pop` immer die zuletzt gemerkte
zuerst zurück, wie bei einem Stapel Zettel, von dem man den obersten nimmt.
In der Informatik heißt so ein Stapel **Stack**. Dasselbe `push` und `pop`
kennen auch Zeichenprogramme: Ein Canvas im Browser merkt sich seinen
Zustand mit `save()` und holt ihn mit `restore()` zurück.

_task(pop-predict)

<!-- after: pop-predict -->
Nach `pop` steht die Schildkröte wieder dort, wo sie bei `push` stand, 100
Schritte rechts vom Anfang, und schaut wieder nach rechts. Die kurze Linie
nach oben bleibt stehen; der letzte Strich verlängert die erste Linie auf
150 Schritte.
<!-- /after -->

_task(tree-push)

## Zufall

`random(1, 6)` ist eine zufällige ganze Zahl von 1 bis 6, wie ein Würfel.
Mit Kommazahlen, `random(0.5, 2.0)`, ist es eine zufällige Kommazahl
dazwischen. Jeder Aufruf gibt eine neue Zahl, und jeder Lauf ein neues Bild.

Ein Bild aus dem Zufall sieht bei jedem Lauf anders aus, darum vergleicht
hier niemand mit einer Vorlage: Jedes Bild ist richtig.

_task(walk)

_task(wind-tree)

<div class='hint try'>
<strong>Ausprobieren:</strong> Lass an manchen Ästen einen dritten wachsen:
Ist <code>random(1, 3)</code> gleich 1, kommt noch ein Ast geradeaus dazu. Achte
darauf, wie viel dichter die Krone wird.
</div>

_task(meadow)

## Pflanzen aus Wörtern

Der Biologe Aristid Lindenmayer hat 1968 beschrieben, wie Algen wachsen:
mit Wörtern, in denen jeder Buchstabe nach einer Regel durch ein ganzes Wort
ersetzt wird, immer wieder. Solche Regeln heißen heute
**Lindenmayer-Systeme** oder kurz L-Systeme. Liest die Schildkröte das Wort
als Befehle, wächst eine Pflanze:

<table class='table'>
<tr><th>Buchstabe</th><th>Die Schildkröte</th></tr>
<tr><td><code>F</code></td><td>geht ein Stück vorwärts und zeichnet</td></tr>
<tr><td><code>+</code></td><td>dreht sich nach links</td></tr>
<tr><td><code>-</code></td><td>dreht sich nach rechts</td></tr>
<tr><td><code>[</code></td><td>merkt sich die Stelle (<code>push</code>)</td></tr>
<tr><td><code>]</code></td><td>kehrt zur Stelle zurück (<code>pop</code>)</td></tr>
</table>

Das Wort `F[+F]F[-F]F` ist ein Stiel aus drei Stücken mit einem Zweig nach
links und einem nach rechts.

_task(draw-word)

Jetzt wächst das Wort. Die Regel heißt: Jedes `F` wird zu `F[+F]F[-F]F`. Aus
`F` wird so `F[+F]F[-F]F`, und daraus ein Wort, in dem jedes dieser fünf `F`
wieder ein ganzer Zweig ist. Klick dich durch die Stufen: Jedes `F` der Regel
wird zur Pflanze der Stufe davor, jede in ihrer Farbe.

<div class='turtle-levels' data-figure='plant'></div>

_task(grow)

<div class='hint try'>
<strong>Ausprobieren:</strong> Andere Regeln, andere Pflanzen. Probier
<code>F[+F]F[-F][F]</code> mit 20 Grad und fünf Stufen, oder <code>FF-[-F+F+F]+[+F-F-F]</code> mit
22,5 Grad und vier Stufen. Mit zwei Regeln wächst eine Pflanze aus dem Buch
unten: <code>X</code> wird zu <code>F+[[X]-X]-F[-FX]+X</code> und <code>F</code> zu <code>FF</code>, mit 22,5 Grad, fünf
Stufen und <code>X</code> als erstem Wort. <code>X</code> zeichnet nichts, es sagt nur,
wo die Pflanze weiterwächst. Achte darauf, welche Regel einen Busch und
welche einen Halm wachsen lässt.
</div>

<div class='book'>
<i class='bi bi-book'></i>
<p><strong>The Algorithmic Beauty of Plants</strong>, Przemysław Prusinkiewicz
und Aristid Lindenmayer, 1990. Das Buch zu den Pflanzen aus Wörtern, mit
Regeln für Büsche, Bäume, Blüten und Farne und mit vielen Bildern. Eine
elektronische Fassung steht frei auf
<a href='https://algorithmicbotany.org/papers/#abop'>algorithmicbotany.org</a>.</p>
</div>

_task(own-plant)

## Dein Bild

Zum Schluss alles zusammen: Farben aus Zahlen, `push` und `pop`, Zufall und
Pflanzen aus Wörtern. Geprüft wird hier nichts.

_task(finale)
