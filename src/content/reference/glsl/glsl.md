<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# GLSL

<p class='abstract'>
GLSL ist die Sprache, in der Shader geschrieben werden: kleine Programme, die auf der Grafikkarte laufen, eins für jede Ecke und eins für jeden Pixel. Hier stehen die Grundlagen und das Wichtigste zum Nachschlagen, in der Fassung <code>#version 100</code>, die Browser verstehen. Die vollständige Beschreibung steht in den offiziellen Unterlagen, die am Ende verlinkt sind.
</p>

## Grundlagen

<!-- part: shader -->
<!-- short: Der Vertex-Shader läuft für jede Ecke und setzt `gl_Position`, der Fragment-Shader für jeden Pixel und setzt `gl_FragColor`; `varying` reicht Werte weiter. -->
### Was ein Shader tut

Die Grafikkarte zeichnet alles aus Dreiecken, und an zwei Stellen fragt sie ein Programm von dir:

1. Der **Vertex-Shader** läuft für jede Ecke eines Dreiecks. Er rechnet aus, wo die Ecke auf dem Bildschirm landet, und schreibt das nach `gl_Position`.
2. Die Grafikkarte füllt das Dreieck mit Pixeln. Was der Vertex-Shader in einer `varying`-Variable an die drei Ecken geschrieben hat, mischt sie für jeden Pixel dazwischen: In der Mitte eines Dreiecks mit einer roten und zwei blauen Ecken kommt ein Lila an.
3. Der **Fragment-Shader** läuft für jeden dieser Pixel und rechnet seine Farbe aus. Die schreibt er nach `gl_FragColor`.

Beide laufen tausendfach gleichzeitig, jeder für sich: Ein Pixel weiß nichts von seinen Nachbarn und kann sich nichts für den nächsten merken. Was für alle gleich ist, etwa die Richtung der Sonne, schickt das Programm auf dem Computer als `uniform` hinein.
<!-- /part -->

<!-- part: version -->
<!-- short: Erste Zeile `#version 100`; im Fragment-Shader danach `precision mediump float;`. -->
### Version und Genauigkeit

Jeder Shader beginnt mit der Version. Browser (WebGL) verstehen GLSL ES 1.00:

```glsl
#version 100
precision mediump float;
```

Die zweite Zeile braucht nur der Fragment-Shader: Er muss sagen, wie genau er mit Kommazahlen rechnet. `mediump` reicht für Farben und Licht, `highp` ist genauer, aber nicht auf jedem Handy da.

Auf dem Computer ohne Browser verwendet raylib meist `#version 330`. Dort heißen einige Dinge anders: `in` und `out` statt `attribute` und `varying`, `texture` statt `texture2D` und eine eigene `out vec4`-Variable statt `gl_FragColor`. Die Beispiele hier sind alle `#version 100`.
<!-- /part -->

## Die Sprache

<!-- part: typen -->
<!-- short: `float`, `int`, `bool`, `vec2`–`vec4`, `mat4`, `sampler2D`; Kommazahlen immer mit Punkt (`1.0`), keine automatische Umwandlung von `int` nach `float`. -->
### Typen und Zahlen

GLSL sieht aus wie C, hat aber eigene Typen für Vektoren und Matrizen:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Typ</th><th>Was</th></tr></thead>
<tbody>
<tr><td><code>float</code>, <code>int</code>, <code>bool</code></td><td>Kommazahl, ganze Zahl, wahr oder falsch</td></tr>
<tr><td><code>vec2</code>, <code>vec3</code>, <code>vec4</code></td><td>Zwei, drei oder vier Kommazahlen: Punkte, Richtungen, Farben</td></tr>
<tr><td><code>ivec2</code> … <code>ivec4</code>, <code>bvec2</code> … <code>bvec4</code></td><td>Dasselbe mit ganzen Zahlen oder Wahrheitswerten</td></tr>
<tr><td><code>mat2</code>, <code>mat3</code>, <code>mat4</code></td><td>Matrizen; <code>mat4 * vec4</code> dreht, verschiebt und projiziert einen Punkt</td></tr>
<tr><td><code>sampler2D</code></td><td>Eine Textur, gelesen mit <code>texture2D</code></td></tr>
</tbody>
</table>
</div>

Ein Vektor entsteht aus Zahlen oder anderen Vektoren, solange es insgesamt genug Zahlen sind:

```glsl
vec3 up = vec3(0.0, 1.0, 0.0);
vec3 grey = vec3(0.5);                 // alle drei Zahlen 0.5
vec4 color = vec4(grey, 1.0);          // drei aus grey, dazu 1.0
```

Rechnen geht Zahl für Zahl: `a + b`, `a * b` und `a * 2.0` rechnen mit jeder Komponente einzeln.

Zwei Stolperfallen: Kommazahlen schreibst du immer mit Punkt und Nachkommastelle, `1.0` statt `1`. Und GLSL wandelt `int` nicht von selbst in `float` um. `float x = 1;` ist ein Fehler, `float(i)` macht aus der ganzen Zahl `i` eine Kommazahl.
<!-- /part -->

<!-- part: komponenten -->
<!-- short: `v.x`, `v.xz`, `color.rgb`, `uv.st`: Komponenten lesen und neu zusammenstellen. -->
### Komponenten

Die Zahlen eines Vektors heißen `x`, `y`, `z`, `w`. Für Farben gibt es dieselben noch einmal als `r`, `g`, `b`, `a`, für Texturkoordinaten als `s`, `t`. Mehrere Buchstaben hintereinander ergeben einen neuen Vektor:

```glsl
vec4 color = vec4(1.0, 0.5, 0.2, 1.0);
vec3 rgb = color.rgb;        // (1.0, 0.5, 0.2)
float red = color.r;         // 1.0
vec2 ground = position.xz;   // nur x und z, ohne die Höhe
vec3 swapped = rgb.bgr;      // umgedreht: (0.2, 0.5, 1.0)
```

Innerhalb eines Ausdrucks bleibt man bei einer Sorte: `color.rgb` und `color.xyz` gehen, `color.rgz` nicht.
<!-- /part -->

<!-- part: ein-und-ausgaben -->
<!-- short: `attribute` (je Ecke, nur Vertex-Shader), `uniform` (für alle gleich), `varying` (vom Vertex- zum Fragment-Shader); `gl_Position`, `gl_FragColor`, `gl_FragCoord`, `gl_FrontFacing`. -->
### Ein- und Ausgaben

Woher ein Wert kommt, sagt ein Wort vor dem Typ:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Wort</th><th>Woher</th><th>Beispiel</th></tr></thead>
<tbody>
<tr><td><code>attribute</code></td><td>Für jede Ecke anders, vom Programm. Nur im Vertex-Shader.</td><td><code>attribute vec3 vertexPosition;</code></td></tr>
<tr><td><code>uniform</code></td><td>Für alle Ecken und Pixel gleich, vom Programm gesetzt</td><td><code>uniform vec3 sun;</code></td></tr>
<tr><td><code>varying</code></td><td>Vom Vertex-Shader geschrieben, im Fragment-Shader für jeden Pixel gemischt gelesen; in beiden gleich deklariert</td><td><code>varying vec3 normal;</code></td></tr>
<tr><td><code>const</code></td><td>Steht fest</td><td><code>const float FOG_START = 16.0;</code></td></tr>
</tbody>
</table>
</div>

Dazu kommen Variablen, die GLSL schon kennt:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Variable</th><th>Wo</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>gl_Position</code></td><td>Vertex-Shader, schreiben</td><td>Wo die Ecke auf dem Bildschirm landet (<code>vec4</code>)</td></tr>
<tr><td><code>gl_FragColor</code></td><td>Fragment-Shader, schreiben</td><td>Die Farbe des Pixels, Rot, Grün, Blau und Deckkraft von 0.0 bis 1.0</td></tr>
<tr><td><code>gl_FragCoord</code></td><td>Fragment-Shader, lesen</td><td>Wo der Pixel im Fenster liegt, in Pixeln (<code>.xy</code>)</td></tr>
<tr><td><code>gl_FrontFacing</code></td><td>Fragment-Shader, lesen</td><td><code>true</code>, wenn die Vorderseite zu sehen ist (Ecken gegen den Uhrzeigersinn)</td></tr>
</tbody>
</table>
</div>

`discard;` im Fragment-Shader lässt einen Pixel ganz weg, etwa wo eine Textur durchsichtig ist.
<!-- /part -->

<!-- part: kontrolle -->
<!-- short: `if`, `for` mit fester Obergrenze und `break`, Funktionen vor ihrer Verwendung, Arrays mit fester Größe, `#define`. -->
### Bedingungen, Schleifen, Funktionen

`if`, `else`, Vergleiche und `&&`, `||` schreibst du wie in C. Eine Schleife braucht in `#version 100` eine feste Obergrenze. Wie viele Durchläufe es wirklich sind, regelt ein `break`:

```glsl
#define MAX_TORCHES 16
uniform vec3 torches[MAX_TORCHES];
uniform int torchCount;

for (int i = 0; i < MAX_TORCHES; i++) {
    if (i >= torchCount) break;
    // … torches[i] …
}
```

`while` gibt es nicht überall. Eigene Funktionen gehen, aber ohne Rekursion, und sie müssen über der Stelle stehen, an der du sie aufrufst:

```glsl
float brightness(vec3 c)
{
    return dot(c, vec3(0.299, 0.587, 0.114));
}
```
<!-- /part -->

## Rechnen

<!-- part: funktionen -->
<!-- short: `dot`, `cross`, `normalize`, `length`, `distance`, `reflect`, `mix`, `clamp`, `step`, `smoothstep`, `fract`, `mod`, `pow`, `sin`, `texture2D`. -->
### Eingebaute Funktionen

Fast alle Funktionen nehmen auch Vektoren und rechnen dann mit jeder Komponente einzeln: `max(v, 0.0)` setzt jede negative Zahl in `v` auf 0.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Funktion</th><th>Ergebnis</th></tr></thead>
<tbody>
<tr><td><code>dot(a, b)</code></td><td>Skalarprodukt: für zwei Richtungen der Länge 1 der Kosinus des Winkels dazwischen, 1 bei gleicher Richtung, 0 im rechten Winkel</td></tr>
<tr><td><code>cross(a, b)</code></td><td>Kreuzprodukt zweier <code>vec3</code>: senkrecht auf beiden</td></tr>
<tr><td><code>normalize(v)</code>, <code>length(v)</code></td><td>Dieselbe Richtung mit Länge 1; die Länge</td></tr>
<tr><td><code>distance(a, b)</code></td><td>Abstand zweier Punkte</td></tr>
<tr><td><code>reflect(d, n)</code></td><td>Richtung <code>d</code>, gespiegelt an einer Fläche mit der Normale <code>n</code></td></tr>
<tr><td><code>min(a, b)</code>, <code>max(a, b)</code>, <code>clamp(x, a, b)</code></td><td>Kleinere, größere; <code>x</code> zwischen <code>a</code> und <code>b</code> gehalten</td></tr>
<tr><td><code>mix(a, b, t)</code></td><td>Mischung: <code>a</code> bei <code>t = 0.0</code>, <code>b</code> bei <code>t = 1.0</code></td></tr>
<tr><td><code>step(edge, x)</code></td><td>0.0, solange <code>x</code> kleiner als <code>edge</code> ist, sonst 1.0</td></tr>
<tr><td><code>smoothstep(a, b, x)</code></td><td>Weicher Übergang von 0.0 bei <code>a</code> zu 1.0 bei <code>b</code></td></tr>
<tr><td><code>abs</code>, <code>sign</code>, <code>floor</code>, <code>ceil</code>, <code>fract</code></td><td>Betrag, Vorzeichen, ab- und aufrunden, Nachkommaanteil</td></tr>
<tr><td><code>mod(x, y)</code></td><td>Rest beim Teilen, auch für Kommazahlen (statt <code>%</code>)</td></tr>
<tr><td><code>pow(x, y)</code>, <code>sqrt(x)</code>, <code>exp</code>, <code>log</code></td><td>Potenz, Wurzel, Exponential- und Logarithmusfunktion</td></tr>
<tr><td><code>sin</code>, <code>cos</code>, <code>tan</code>, <code>atan(y, x)</code></td><td>Winkelfunktionen im Bogenmaß; <code>radians(deg)</code> rechnet Grad um</td></tr>
<tr><td><code>texture2D(t, uv)</code></td><td>Farbe der Textur <code>t</code> an der Stelle <code>uv</code>, von (0.0, 0.0) bis (1.0, 1.0)</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

## Mit raylib

<!-- part: raylib -->
<!-- short: raylib liefert `vertexPosition`, `vertexNormal`, `vertexColor`, `vertexTexCoord`, `mvp` und `texture0` unter diesen Namen. -->
### Was raylib mitschickt

Wie du einen Shader in C lädst und Werte hineinschickst, steht bei [raylib](/reference/raylib#shader). Einige Werte setzt raylib selbst, wenn sie im Shader unter diesen Namen vorkommen:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Name</th><th>Was</th></tr></thead>
<tbody>
<tr><td><code>attribute vec3 vertexPosition</code></td><td>Die Ecke in der Welt</td></tr>
<tr><td><code>attribute vec3 vertexNormal</code></td><td>Die Normale aus <code>rlNormal3f</code></td></tr>
<tr><td><code>attribute vec4 vertexColor</code></td><td>Die Farbe aus <code>rlColor4ub</code>, von 0.0 bis 1.0</td></tr>
<tr><td><code>attribute vec2 vertexTexCoord</code></td><td>Die Texturkoordinate aus <code>rlTexCoord2f</code></td></tr>
<tr><td><code>uniform mat4 mvp</code></td><td>Kamera und Projektion zusammen: <code>mvp * vec4(vertexPosition, 1.0)</code> ist der Punkt auf dem Bildschirm</td></tr>
<tr><td><code>uniform sampler2D texture0</code></td><td>Die Textur, mit der gerade gezeichnet wird</td></tr>
<tr><td><code>uniform vec4 colDiffuse</code></td><td>Die Farbe, die <code>DrawModel</code> und Co. mitgeben</td></tr>
</tbody>
</table>
</div>

Der kleinste Vertex-Shader reicht Farbe und Normale weiter und setzt die Ecke an ihren Platz:

```glsl
#version 100

attribute vec3 vertexPosition;
attribute vec3 vertexNormal;
attribute vec4 vertexColor;
uniform mat4 mvp;

varying vec3 normal;
varying vec4 color;

void main()
{
    normal = vertexNormal;
    color = vertexColor;
    gl_Position = mvp * vec4(vertexPosition, 1.0);
}
```
<!-- /part -->

## Fehler finden

<!-- part: fehler -->
<!-- short: Fehler stehen in der Konsole der Seite; ein unbenutztes `uniform` verschwindet; Werte als Farbe ausgeben. -->
### Wenn nichts oder Schwarz zu sehen ist

Einen Fehler im Shader findet der Compiler erst, wenn das Programm läuft. raylib meldet ihn in der Konsole der Seite, mit Zeilennummer im Shader; die Welt bleibt dann schwarz oder wird ohne Shader gezeichnet. Die häufigsten Ursachen:

- `1` statt `1.0`, oder `int` und `float` gemischt.
- `precision mediump float;` fehlt im Fragment-Shader.
- Ein `varying` heißt im Vertex- und im Fragment-Shader verschieden oder hat einen anderen Typ.
- Ein `uniform`, das der Shader nirgends verwendet, entfernt der Compiler. `GetShaderLocation` liefert dann −1, und der Wert kommt nie an, ohne Fehlermeldung.

Einen Wert, den man nicht sieht, macht man zur Farbe. Eine Normale hat Zahlen von −1 bis 1, Farben gehen von 0 bis 1:

```glsl
gl_FragColor = vec4(normalize(normal) * 0.5 + 0.5, 1.0);
```

Jetzt hat jede Richtung ihre Farbe: Oberseiten (0, 1, 0) werden hellgrün, Seiten nach +x rosa. Genauso lässt sich ein Abstand oder ein Lichtwert als Grauwert anzeigen, `vec4(vec3(sunAmount), 1.0)`.
<!-- /part -->

## Zum Nachschlagen

- [OpenGL ES 2.0 Reference Card](https://www.khronos.org/files/opengles20-reference-card.pdf) der Khronos Group: alle Typen, Variablen und Funktionen von GLSL ES 1.00 auf zwei Seiten (die letzten beiden der PDF).
- [GLSL ES 1.00 Specification](https://registry.khronos.org/OpenGL/specs/es/2.0/GLSL_ES_Specification_1.00.pdf): die vollständige Beschreibung der Sprache, für `#version 100`. Die Einschränkungen für Schleifen stehen im Anhang A.
- [GLSL 3.30 Specification](https://registry.khronos.org/OpenGL/specs/gl/GLSLangSpec.3.30.pdf): die Fassung `#version 330` für raylib ohne Browser.
- [raylib: Shader-Beispiele](https://www.raylib.com/examples.html) (Abschnitt »shaders«) und ihre [Shader in `#version 100`](https://github.com/raysan5/raylib/tree/master/examples/shaders/resources/shaders/glsl100) zum Lesen und Abwandeln.
- [The Book of Shaders](https://thebookofshaders.com/?lan=de) von Patricio Gonzalez Vivo und Jen Lowe: keine offizielle Referenz, aber eine Einführung in Fragment-Shader zum Ausprobieren im Browser, auch auf Deutsch.
