<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Kartografie

<p class='abstract'>
Wie man einen Raum festhält, den man gefunden hat, oder einen, den man bauen will: als Textdatei, Feld für Feld, mit seinen Wänden, Türen, Leuchten und allem, was darin steht. Hier stehen alle Einträge zum Nachschlagen.
</p>

<div class='found-note'>
<p>Schreib alles auf. Auch das, was sich nicht wiederholen lässt. Vor allem das.</p>
<cite>Erste Regel der Kartografen</cite>
</div>

## Die Dateien

Ein Level besteht aus einer Datei `level.ini` und einer Datei für jeden Raum im Ordner `rooms`, etwa `rooms/zimmer.room`. Der Dateiname ist der Name des Raums: nur Kleinbuchstaben, Ziffern, `-` und `_`.

Beide Arten von Dateien bestehen aus Abschnitten in eckigen Klammern, darunter Einträge `name = wert`, wie in einer [INI-Datei](https://de.wikipedia.org/wiki/Initialisierungsdatei). Eine Zeile, die mit `#` beginnt, ist ein Kommentar, ebenso alles ab einem `#` mit Leerzeichen davor und danach; eine Farbe wie `#86763c` hat kein Leerzeichen dahinter. Längen sind in Metern, Winkel in Grad, Farben hexadezimal wie in CSS.

Das Programm liest zuerst den Raum aus `start =`, dann jeden Raum, zu dem eine Tür eines schon gelesenen Raums führt. Eine Datei, zu der keine Tür führt, liest es nie.

```room
# Ein kleines Zimmer.

[room]
name = Zimmer
height = 2.6
look = east

[map]
#######
#.....#
#.@.L.1
#.....#
#######

[L]
light = #ffe7b8
power = 0.9

[1]
door = gang 1
```

## `level.ini`

Im Abschnitt `[level]`:

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>name</code></td><td>der Name des Levels</td></tr>
<tr><td><code>start</code></td><td>der Raum, in dem man anfängt: <code>start = zimmer</code> liest <code>rooms/zimmer.room</code></td></tr>
<tr><td><code>bounces</code></td><td>wie oft Licht von Flächen zurückgeworfen wird (3); 0: nur direktes Licht</td></tr>
<tr><td><code>detail</code></td><td>Punkte der Lightmap je Meter (16): mehr gibt schärfere Schatten und dauert länger</td></tr>
<tr><td><code>ambient</code></td><td>Licht von überall (0)</td></tr>
<tr><td><code>vhs</code></td><td><code>on</code>: das Bild wie von einer alten Videokassette</td></tr>
<tr><td><code>aberration</code></td><td><code>on</code>: farbige Säume zum Rand hin, wie durch eine einfache Linse</td></tr>
</table>

## Der Grundriss

In `[map]` steht der Grundriss, von oben gesehen, oben ist Norden. Jedes Zeichen ist ein Feld, ein Meter im Quadrat. Hier zählt jedes Zeichen, Kommentare gibt es darin nicht.

<table class='table'>
<tr><th>Zeichen</th><th>Bedeutung</th></tr>
<tr><td><code>#</code></td><td>Wand</td></tr>
<tr><td><code>.</code></td><td>Boden</td></tr>
<tr><td><code>@</code></td><td>hier fängt man an, nur einmal im ganzen Level</td></tr>
<tr><td>Leerzeichen</td><td>nichts, außerhalb des Raums</td></tr>
<tr><td>ein Buchstabe</td><td>eine Leuchte, ein Block, ein Modell, ein Becken, eine Treppe oder ein Podest, beschrieben im Abschnitt mit diesem Buchstaben; statt eines <code>#</code>: eine Wand mit runden Ecken</td></tr>
<tr><td>eine Ziffer</td><td>eine Tür, beschrieben im Abschnitt mit dieser Ziffer; dieselbe Ziffer mehrmals in einer Reihe, nebeneinander oder untereinander, ist eine breite Tür</td></tr>
<tr><td><code>_</code></td><td>ein Loch im Boden; darunter liegt ein anderer Raum, beschrieben in <code>[_]</code></td></tr>
<tr><td><code>^</code></td><td>ein Loch in der Decke: Der Raum darüber hat dort sein <code>_</code></td></tr>
</table>

Ein Feld mit Boden darf nicht an »nichts« grenzen: Um jeden Raum steht eine Wand. Die Wände zweier Räume dürfen sich überdecken, ihre Böden nicht.

## `[room]`

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>name</code></td><td>der Name, den die Automap zeigt</td></tr>
<tr><td><code>height</code></td><td>Deckenhöhe über dem Boden (2,7); zwei Zahlen für eine schräge Decke, etwa <code>height = 3, 7</code></td></tr>
<tr><td><code>slope</code></td><td><code>flat</code>, die Richtung, in die die Decke ansteigt (<code>north</code>, <code>east</code>, <code>south</code>, <code>west</code>), oder <code>gable</code>, ein Satteldach, sein First an der längeren Seite</td></tr>
<tr><td><code>ramp</code></td><td>ein schräger Boden: Richtung und um wie viele Meter er ansteigt, etwa <code>ramp = east, 4</code>; negativ: Er fällt. Decke und Leuchten steigen mit</td></tr>
<tr><td><code>walls</code></td><td><code>wallpaper</code>, <code>marble</code>, <code>wood</code>, <code>tiles</code>, <code>velvet</code>, <code>plain</code>, <code>concrete</code>, <code>brick</code> oder <code>terrazzo</code>, in der Farbe von <code>wallpaper</code></td></tr>
<tr><td><code>floor</code></td><td><code>carpet</code>, <code>marble</code>, <code>wood</code>, <code>tiles</code>, <code>plain</code>, <code>concrete</code>, <code>brick</code>, <code>terrazzo</code>, <code>carpet_tiles</code>, <code>checker</code> oder <code>parquet</code>, in der Farbe von <code>carpet</code></td></tr>
<tr><td><code>wallpaper</code>, <code>carpet</code>, <code>ceiling</code></td><td>die Farben von Wänden, Boden und Decke</td></tr>
<tr><td><code>baseboard</code></td><td>die Sockelleiste: Höhe und Farbe; ohne Angabe 8 cm und dunkler als die Tapete; <code>none</code>: keine. An einem schrägen Boden läuft sie schräg mit</td></tr>
<tr><td><code>dado</code></td><td>die Wände unten anders: Höhe und Farbe, etwa <code>dado = 1, #553a27</code></td></tr>
<tr><td><code>dado_material</code></td><td>woraus dieser untere Teil ist, wie bei <code>walls</code>; ohne: wie die Wand</td></tr>
<tr><td><code>dado_rail</code></td><td>eine Leiste auf seiner Kante: Höhe und Farbe</td></tr>
<tr><td><code>cornice</code></td><td>eine schräge Leiste, wo Wand und Decke sich treffen: Höhe und Farbe</td></tr>
<tr><td><code>look</code></td><td>wohin man am <code>@</code> schaut: eine Himmelsrichtung oder Grad, im Uhrzeigersinn von Norden aus</td></tr>
<tr><td><code>ambient</code></td><td>Licht von überall in diesem Raum (0 bis 1)</td></tr>
</table>

## Was in einem Feld steht

Jeder Buchstabe im Grundriss hat einen Abschnitt mit diesem Buchstaben. Was darin steht, sagt, was er ist.

### Eine Leuchte

Sie hängt knapp unter der Decke, mitten über ihrem Feld, unter einer schrägen Decke so schräg wie sie.

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>light</code></td><td>ihre Farbe, etwa <code>#fff3cf</code></td></tr>
<tr><td><code>power</code></td><td>wie hell (1)</td></tr>
<tr><td><code>size</code></td><td>Breite und Länge, eine Zahl für eine quadratische (0,85); größere werfen weichere Schatten</td></tr>
<tr><td><code>flicker</code></td><td><code>1</code>: Sie flackert ab und zu</td></tr>
</table>

### Ein Block

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>block</code></td><td>Breite, Höhe, Tiefe, mitten auf seinem Feld; eine Zahl: das ganze Feld so hoch. Höher als die Decke: Er verschwindet in ihr</td></tr>
<tr><td><code>base</code></td><td>wie hoch er über dem Boden schwebt (0)</td></tr>
<tr><td><code>color</code></td><td>seine Farbe; ohne: wie die Wände seines Raums</td></tr>
<tr><td><code>material</code></td><td>woraus, wie bei <code>walls</code>, dazu <code>cardboard</code>: ein Karton mit Klebeband, Etikett und Pfeilen</td></tr>
<tr><td><code>shape</code></td><td><code>round</code>: eine runde Säule</td></tr>
<tr><td><code>rotation</code></td><td>in Grad um die senkrechte Achse gedreht, um seine Mitte; nur um diese Achse. Gedreht bleibt er am besten in seinem Feld</td></tr>
<tr><td><code>offset</code></td><td>aus der Mitte des Felds geschoben: x, y, z (x nach Osten, y nach oben, z nach Süden)</td></tr>
</table>

### Eine runde Ecke

Ein Buchstabe statt eines `#`, dessen Abschnitt `round` nennt, ist eine Wand mit runden Ecken.

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>round</code></td><td>der Radius in Metern. Treffen an dem Feld zwei Wände des Raums aufeinander (ein <code>#</code> in einer Ecke des Raums), wird die Ecke ein Viertelkreis, Boden und Decke folgen ihm. Ragt die Wand dort in den Raum (das Ende einer Wand, die Ecke eines Pfeilers), wird ihre Kante rund; an einer Wand, die ein Feld dick ist, höchstens 0,5, dann ist ihr Ende ein Halbkreis</td></tr>
</table>

Sockelleiste, Lambris, Stuhlleiste und Gesims laufen um die Rundung herum. Der Radius muss passen: auf beiden Wänden bis zur nächsten Tür oder Ecke, innerhalb des Raums. Passt er nicht, steht oben links, an welchem Feld.

### Ein Modell

Eine Statue, ein Möbelstück, eine Lampe: eine Datei im Format [glTF](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html), als `.gltf` mit ihren Dateien daneben oder als eine `.glb`. Modelle unter CC0 gibt es etwa bei [Poly Haven](https://polyhaven.com/models), in der Auflösung 1K.

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>model</code></td><td>der Pfad zur Datei, etwa <code>models/athena.glb</code></td></tr>
<tr><td><code>height</code></td><td>wie hoch es ist (2)</td></tr>
<tr><td><code>rotation</code></td><td>gedreht, in Grad: eine Zahl um die senkrechte Achse; drei, <code>x, y, z</code>, um alle drei Achsen, erst um y, dann um x, dann um z</td></tr>
<tr><td><code>offset</code></td><td>aus der Mitte des Felds geschoben: x, y, z</td></tr>
<tr><td><code>material</code></td><td><code>marble</code>: aus Marmor statt in seinen eigenen Farben</td></tr>
<tr><td><code>pedestal</code></td><td>ein Sockel darunter: Breite, Höhe, Tiefe; <code>pedestal_color</code> und <code>pedestal_material</code> wie bei einem Block</td></tr>
<tr><td><code>light</code></td><td>eine Farbe: Das Modell ist eine Lampe. Ihr Licht kommt aus seinen leuchtenden Teilen, nach allen Seiten; <code>power</code> und <code>flicker</code> wie bei einer Leuchte</td></tr>
<tr><td><code>beam</code></td><td>bei einer Lampe mit Schirm: Das Licht fällt nur in einen Kegel nach unten, so viel Grad von der Senkrechten, etwa <code>beam = 75</code>; <code>beam = up, 60</code>: nach oben</td></tr>
</table>

Erst wird ein Modell gedreht, um seine eigene Mitte. Dann steht seine Mitte über der Mitte des Felds und sein tiefster Punkt auf dem Boden oder dem Sockel. Erst danach schiebt `offset` es weiter, in den Richtungen der Welt, nicht in denen des gedrehten Modells.

### Ein Becken

Jedes Feld mit dem Buchstaben gehört zum Becken; Felder nebeneinander, gleich tief, mit gleichem Wasser und gleicher Farbe, sind eins. In ein Becken voll Wasser kann man nicht hineingehen, in ein leeres fällt man. Ein Raum mit einem Becken hat keine Rampe.

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>pool</code></td><td>wie tief es unter den Boden reicht, 0,2 bis 20 m</td></tr>
<tr><td><code>water</code></td><td>wie tief unter dem Boden das Wasser steht (0,15); <code>off</code>: leer</td></tr>
<tr><td><code>tint</code></td><td>die Farbe des Wassers: so, wie Weiß hinter 2 m davon aussieht (<code>#86b8b5</code>, blaugrün)</td></tr>
<tr><td><code>waves</code></td><td>wie groß die Wellen sind: 1 ruhig, 2 größer, 0 glatt</td></tr>
<tr><td><code>color</code></td><td>die Fliesen des Beckens</td></tr>
<tr><td><code>rim</code></td><td>ein Rand aus Fliesen: Höhe und Breite, etwa <code>rim = 0.15, 0.3</code></td></tr>
<tr><td><code>railing</code></td><td><code>on</code>: ein Geländer, wo man am Rand stehen kann</td></tr>
</table>

### Eine Treppe, ein Podest

Unter beiden ist kein Platz: Sie sind massiv. Eine Treppe braucht ein Rechteck aus Feldern und davor Platz, um auf die erste Stufe zu treten.

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>stairs</code></td><td>eine Treppe: Richtung und Höhe oben, etwa <code>stairs = north, 3</code>, in Stufen von etwa 18 cm. Liegt an ihrem unteren Ende ein Podest, beginnt sie auf dessen Höhe</td></tr>
<tr><td><code>raise</code></td><td>ein Podest: Diese Felder liegen so viel höher; nebeneinander sind sie eins</td></tr>
<tr><td><code>railing</code></td><td><code>on</code>: ein Geländer, wo es an der Seite mehr als einen halben Meter hinuntergeht, für alle Treppen und Podeste eines Raums in einem Stück</td></tr>
<tr><td><code>material</code>, <code>color</code></td><td>woraus und in welcher Farbe; ohne: wie Boden und Wände des Raums</td></tr>
</table>

### Im Becken, auf dem Podest

Eine Leuchte, ein Block oder ein Modell kann zugleich zu einem Becken oder einem Podest gehören:

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>in = W</code></td><td>das Feld gehört zum Becken der <code>W</code>: eine Säule im Wasser, eine Leuchte darüber</td></tr>
<tr><td><code>on = P</code></td><td>das Feld gehört zum Podest der <code>P</code>: ein Stuhl darauf</td></tr>
</table>

## Türen und Löcher

Eine Tür ist ein Feld, das zwei Räumen gehört. Wie hoch und wo ein Raum liegt, folgt aus seinen Türen; es genügt, wenn einer der beiden Räume die Tür beschreibt.

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>door = gang 2</code></td><td>diese Ziffer ist dasselbe Feld wie die <code>2</code> in <code>rooms/gang.room</code></td></tr>
<tr><td><code>door = halle 1, -4</code></td><td>eine Tür hoch in der Wand: Der Boden auf der anderen Seite liegt 4 m tiefer; ohne Minus höher</td></tr>
<tr><td><code>arch = 2.4</code></td><td>oben rund, ein Halbkreis über die ganze Breite, in der Mitte 2,4 m hoch; <code>arch = 2.4, 0.8</code>: nur 0,8 m breit, daneben Wand</td></tr>
<tr><td><code>casing = on</code></td><td>ein Rahmen: innen heller Putz, unten eine Schwelle aus Stein, auf jeder Seite der Wand eine Leiste um die Öffnung, in der Farbe der Sockelleiste des Raums oder, ohne Sockelleiste, hell gestrichen</td></tr>
</table>

Eine Tür ist so tief wie die Wand, ein Feld, und sieht aus wie der Raum, dem dieses Feld gehört; mit `casing = on` hat sie einen Rahmen.

Ein Loch verbindet zwei Räume übereinander. Im oberen Raum stehen `_` im Grundriss und ein Abschnitt `[_]`, im unteren an denselben Feldern `^`:

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>below = keller</code></td><td>der Raum darunter</td></tr>
<tr><td><code>slab</code></td><td>wie dick der Boden zwischen beiden ist (0,3)</td></tr>
</table>

Räume dürfen übereinander liegen, solange der obere Boden nirgends unter der Decke des unteren liegt.

## Tasten

<table class='table'>
<tr><th>Taste</th><th>Was passiert</th></tr>
<tr><td><kbd>W</kbd> <kbd>A</kbd> <kbd>S</kbd> <kbd>D</kbd>, Pfeile</td><td>laufen (mit <kbd>Shift</kbd> rennen), drehen</td></tr>
<tr><td><kbd>M</kbd> oder <kbd>Tab</kbd></td><td>die Automap; darin <kbd>V</kbd>: die Räume von oben, <kbd>U</kbd>: was von hier aus gezeichnet wird</td></tr>
<tr><td><kbd>T</kbd> <kbd>L</kbd> <kbd>B</kbd> <kbd>O</kbd></td><td>Texturen, direktes Licht, zurückgeworfenes Licht, Kontaktschatten</td></tr>
<tr><td><kbd>G</kbd> <kbd>E</kbd> <kbd>K</kbd></td><td>die Punkte der Lightmap, die Kanten, was beim Laufen im Weg ist</td></tr>
<tr><td><kbd>F</kbd> <kbd>H</kbd> <kbd>C</kbd> <kbd>X</kbd></td><td>Flackern, Bloom, Videokassette, farbige Säume</td></tr>
<tr><td><kbd>3</kbd></td><td>ein Bild für eine Rot-Cyan-Brille</td></tr>
<tr><td><kbd>Shift</kbd> <kbd>L</kbd>, <kbd>Shift</kbd> <kbd>R</kbd>, <kbd>P</kbd></td><td>das Licht neu berechnen, zurück zum Anfang, den Kasten oben links aus- und einblenden</td></tr>
</table>

## Zum Nachschlagen

- [glTF 2.0](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html): die Beschreibung des Formats der Modelle, von der Khronos Group
- [three.js](https://threejs.org/docs/): die Dokumentation der 3D-Bibliothek, mit der gezeichnet wird
- [Poly Haven](https://polyhaven.com/models): Modelle unter CC0
- [GLSL](/reference/glsl): die Sprache der Shader, die Licht, Holz, Fliesen und Marmor berechnen
