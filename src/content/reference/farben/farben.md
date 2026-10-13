<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Farben angeben

<p class='abstract'>
Wie du dem Computer eine Farbe nennst: mit einem Namen wie <code>gold</code>, mit einem Farbcode wie <code>#f9c80e</code> oder mit Anteilen von Rot, Grün und Blau. Das gilt für CSS in Webseiten und HTML-Folien und genauso für <code>fill</code>, <code>stroke</code> und <code>background</code> im Anaglyph-Viewer.
</p>

## Eine Farbe angeben

<!-- part: farbnamen -->
### Farbnamen

Der einfachste Weg: Du schreibst den englischen Namen der Farbe. Browser kennen 148 Namen, von `red` bis `rebeccapurple`. Groß- und Kleinschreibung ist egal, Leerzeichen gibt es nicht (`lightblue`, nicht `light blue`). Ein paar, die du oft brauchst:

<table class='table table-sm reference-table'>
<tr><th></th><th>Name</th><th>Farbcode</th><th></th><th>Name</th><th>Farbcode</th></tr>
<tr><td><span class='color-swatch' style='background: white;'></span></td><td><code>white</code></td><td><code>#ffffff</code></td><td><span class='color-swatch' style='background: black;'></span></td><td><code>black</code></td><td><code>#000000</code></td></tr>
<tr><td><span class='color-swatch' style='background: silver;'></span></td><td><code>silver</code></td><td><code>#c0c0c0</code></td><td><span class='color-swatch' style='background: gray;'></span></td><td><code>gray</code> / <code>grey</code></td><td><code>#808080</code></td></tr>
<tr><td><span class='color-swatch' style='background: red;'></span></td><td><code>red</code></td><td><code>#ff0000</code></td><td><span class='color-swatch' style='background: crimson;'></span></td><td><code>crimson</code></td><td><code>#dc143c</code></td></tr>
<tr><td><span class='color-swatch' style='background: orange;'></span></td><td><code>orange</code></td><td><code>#ffa500</code></td><td><span class='color-swatch' style='background: gold;'></span></td><td><code>gold</code></td><td><code>#ffd700</code></td></tr>
<tr><td><span class='color-swatch' style='background: yellow;'></span></td><td><code>yellow</code></td><td><code>#ffff00</code></td><td><span class='color-swatch' style='background: saddlebrown;'></span></td><td><code>saddlebrown</code></td><td><code>#8b4513</code></td></tr>
<tr><td><span class='color-swatch' style='background: green;'></span></td><td><code>green</code></td><td><code>#008000</code></td><td><span class='color-swatch' style='background: lime;'></span></td><td><code>lime</code></td><td><code>#00ff00</code></td></tr>
<tr><td><span class='color-swatch' style='background: teal;'></span></td><td><code>teal</code></td><td><code>#008080</code></td><td><span class='color-swatch' style='background: skyblue;'></span></td><td><code>skyblue</code></td><td><code>#87ceeb</code></td></tr>
<tr><td><span class='color-swatch' style='background: blue;'></span></td><td><code>blue</code></td><td><code>#0000ff</code></td><td><span class='color-swatch' style='background: navy;'></span></td><td><code>navy</code></td><td><code>#000080</code></td></tr>
<tr><td><span class='color-swatch' style='background: purple;'></span></td><td><code>purple</code></td><td><code>#800080</code></td><td><span class='color-swatch' style='background: hotpink;'></span></td><td><code>hotpink</code></td><td><code>#ff69b4</code></td></tr>
</table>

Vorsicht bei Namen, die anders aussehen, als sie klingen: `green` ist ein dunkles Grün, das helle heißt `lime`. `gray` ist dunkler als `darkgray`.

```css
h2 { color: crimson; }
```

```ini
fill = gold
shape = sphere
```

Alle 148 Namen mit ihren Farben findest du in der [Liste von MDN](https://developer.mozilla.org/de/docs/Web/CSS/named-color).
<!-- /part -->

<!-- part: farbcodes -->
### Farbcodes mit `#`

Ein Farbcode wie `#f9c80e` besteht aus drei Paaren: Rot (`f9`), Grün (`c8`) und Blau (`0e`). Jedes Paar ist eine Hexadezimalzahl von `00` (nichts von dieser Farbe) bis `ff` (so viel wie möglich, 255). Licht mischt sich anders als Malfarbe: Rot und Grün ergeben Gelb, alle drei ganz ergeben Weiß.

<table class='table table-sm reference-table'>
<tr><th></th><th>Code</th><th>Rot</th><th>Grün</th><th>Blau</th><th>Farbe</th></tr>
<tr><td><span class='color-swatch' style='background: #ff0000;'></span></td><td><code>#ff0000</code></td><td>255</td><td>0</td><td>0</td><td>Rot</td></tr>
<tr><td><span class='color-swatch' style='background: #ffff00;'></span></td><td><code>#ffff00</code></td><td>255</td><td>255</td><td>0</td><td>Gelb</td></tr>
<tr><td><span class='color-swatch' style='background: #f9c80e;'></span></td><td><code>#f9c80e</code></td><td>249</td><td>200</td><td>14</td><td>Sonnengelb</td></tr>
<tr><td><span class='color-swatch' style='background: #3a6ee8;'></span></td><td><code>#3a6ee8</code></td><td>58</td><td>110</td><td>232</td><td>ein kräftiges Blau</td></tr>
<tr><td><span class='color-swatch' style='background: #808080;'></span></td><td><code>#808080</code></td><td>128</td><td>128</td><td>128</td><td>Grau: alle drei gleich</td></tr>
</table>

Die Kurzform `#fa0` steht für `#ffaa00`: Jede Ziffer wird verdoppelt. Einen Farbcode suchst du am bequemsten mit einem Farbwähler aus. VS Code zeigt in CSS-Dateien vor jedem Farbcode ein kleines Kästchen; ein Klick darauf öffnet einen.

```css
background: #f3e5f5;
```

```ini
fill = #3a6ee8
```
<!-- /part -->

<!-- part: rgb -->
### Rot, Grün und Blau einzeln

In CSS schreibst du die drei Anteile als Zahlen von 0 bis 255:

```css
color: rgb(249, 200, 14);
```

Im Anaglyph-Viewer stehen sie ohne `rgb(…)` und gehen von `0` bis `1`. Eine einzelne Zahl ist ein Grauton von `0` (schwarz) bis `1` (weiß):

```ini
fill = 1, 0.8, 0
background = 0
```

Weil das Zahlen sind, darfst du im Viewer auch rechnen, zum Beispiel mit der Zeit `t`: `fill = sin(t) * 0.5 + 0.5, 0, 0` lässt ein Objekt rot pulsieren.
<!-- /part -->

<!-- part: kontrast -->
## Kontrast

Text muss sich deutlich vom Hintergrund abheben. Schwarz auf Weiß und Weiß auf Dunkelblau sind gut lesbar, Hellgrau auf Weiß und Rot auf Grün nicht: Etwa jeder zwölfte Junge kann Rot und Grün schlecht unterscheiden. Prüfe im Zweifel, ob du den Text auch noch lesen kannst, wenn du ein Stück vom Bildschirm zurücktrittst.
<!-- /part -->
