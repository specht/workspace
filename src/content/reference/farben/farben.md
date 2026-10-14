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
Wie du dem Computer eine Farbe nennst: mit einem Namen wie <code>gold</code>, mit einem Farbcode wie <code>#f9c80e</code> oder mit Anteilen von Rot, Grün und Blau. Das gilt für CSS in Webseiten und HTML-Folien und genauso für <code>fill</code>, <code>stroke</code> und <code>background</code> im Anaglyph-Viewer. Dazu eine fertige Palette mit 44 Farben und ein Farbmischer, der jede Farbe in alle Schreibweisen übersetzt.
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

## Farben finden

<!-- part: cling -->
### Die Cling-Palette

Eine Palette ist eine Auswahl von Farben, die gut zusammenpassen. Die Cling-Palette hat 44 Farben, von hellem Gelb bis Schwarz. In [Pixelflow Canvas](/drawing) wählst du sie mit `set_predefined_palette(:cling)`, in [PixelRAM](/pixelram) mit `use_palette("cling")`; dort sind es die Farben 0 bis 43. In CSS und im Anaglyph-Viewer nimmst du den Farbcode. Klicke auf eine Farbe, dann steht sie unten im Farbmischer mit allen ihren Schreibweisen.

<div class='color-palette' aria-label='Die 44 Farben der Cling-Palette'>
<div class='color-palette-row'><span class='color-palette-family'>Gelb, Orange und Rot</span><div class='color-palette-swatches'>
<button type='button' class='color-palette-swatch dark' style='background:#ffe617' data-color='#ffe617' data-index='0' title='0 · #ffe617'>0</button>
<button type='button' class='color-palette-swatch dark' style='background:#fad31c' data-color='#fad31c' data-index='1' title='1 · #fad31c'>1</button>
<button type='button' class='color-palette-swatch dark' style='background:#fdb717' data-color='#fdb717' data-index='2' title='2 · #fdb717'>2</button>
<button type='button' class='color-palette-swatch dark' style='background:#faaa21' data-color='#faaa21' data-index='3' title='3 · #faaa21'>3</button>
<button type='button' class='color-palette-swatch dark' style='background:#f1753f' data-color='#f1753f' data-index='4' title='4 · #f1753f'>4</button>
<button type='button' class='color-palette-swatch dark' style='background:#ed5724' data-color='#ed5724' data-index='5' title='5 · #ed5724'>5</button>
<button type='button' class='color-palette-swatch dark' style='background:#ef4538' data-color='#ef4538' data-index='6' title='6 · #ef4538'>6</button>
<button type='button' class='color-palette-swatch dark' style='background:#ea2830' data-color='#ea2830' data-index='7' title='7 · #ea2830'>7</button>
<button type='button' class='color-palette-swatch light' style='background:#bc2326' data-color='#bc2326' data-index='8' title='8 · #bc2326'>8</button>
<button type='button' class='color-palette-swatch light' style='background:#8c0c03' data-color='#8c0c03' data-index='9' title='9 · #8c0c03'>9</button>
</div></div>
<div class='color-palette-row'><span class='color-palette-family'>Rosa und Violett</span><div class='color-palette-swatches'>
<button type='button' class='color-palette-swatch dark' style='background:#e5185d' data-color='#e5185d' data-index='10' title='10 · #e5185d'>10</button>
<button type='button' class='color-palette-swatch dark' style='background:#f384ae' data-color='#f384ae' data-index='11' title='11 · #f384ae'>11</button>
<button type='button' class='color-palette-swatch dark' style='background:#fac6d2' data-color='#fac6d2' data-index='12' title='12 · #fac6d2'>12</button>
<button type='button' class='color-palette-swatch dark' style='background:#b296c7' data-color='#b296c7' data-index='13' title='13 · #b296c7'>13</button>
<button type='button' class='color-palette-swatch light' style='background:#7b67ae' data-color='#7b67ae' data-index='14' title='14 · #7b67ae'>14</button>
<button type='button' class='color-palette-swatch light' style='background:#5f3577' data-color='#5f3577' data-index='15' title='15 · #5f3577'>15</button>
</div></div>
<div class='color-palette-row'><span class='color-palette-family'>Grün</span><div class='color-palette-swatches'>
<button type='button' class='color-palette-swatch dark' style='background:#c1d18a' data-color='#c1d18a' data-index='16' title='16 · #c1d18a'>16</button>
<button type='button' class='color-palette-swatch dark' style='background:#799155' data-color='#799155' data-index='17' title='17 · #799155'>17</button>
<button type='button' class='color-palette-swatch dark' style='background:#80bc42' data-color='#80bc42' data-index='18' title='18 · #80bc42'>18</button>
<button type='button' class='color-palette-swatch dark' style='background:#4aa03f' data-color='#4aa03f' data-index='19' title='19 · #4aa03f'>19</button>
<button type='button' class='color-palette-swatch dark' style='background:#16884a' data-color='#16884a' data-index='20' title='20 · #16884a'>20</button>
<button type='button' class='color-palette-swatch light' style='background:#003f2e' data-color='#003f2e' data-index='21' title='21 · #003f2e'>21</button>
</div></div>
<div class='color-palette-row'><span class='color-palette-family'>Blau</span><div class='color-palette-swatches'>
<button type='button' class='color-palette-swatch dark' style='background:#c3def1' data-color='#c3def1' data-index='22' title='22 · #c3def1'>22</button>
<button type='button' class='color-palette-swatch dark' style='background:#55beed' data-color='#55beed' data-index='23' title='23 · #55beed'>23</button>
<button type='button' class='color-palette-swatch dark' style='background:#31a8e0' data-color='#31a8e0' data-index='24' title='24 · #31a8e0'>24</button>
<button type='button' class='color-palette-swatch dark' style='background:#238acc' data-color='#238acc' data-index='25' title='25 · #238acc'>25</button>
<button type='button' class='color-palette-swatch light' style='background:#0d60ae' data-color='#0d60ae' data-index='26' title='26 · #0d60ae'>26</button>
<button type='button' class='color-palette-swatch light' style='background:#143b86' data-color='#143b86' data-index='27' title='27 · #143b86'>27</button>
<button type='button' class='color-palette-swatch light' style='background:#001b4a' data-color='#001b4a' data-index='28' title='28 · #001b4a'>28</button>
</div></div>
<div class='color-palette-row'><span class='color-palette-family'>Türkis</span><div class='color-palette-swatches'>
<button type='button' class='color-palette-swatch dark' style='background:#7dcdc2' data-color='#7dcdc2' data-index='29' title='29 · #7dcdc2'>29</button>
<button type='button' class='color-palette-swatch dark' style='background:#00a8a8' data-color='#00a8a8' data-index='30' title='30 · #00a8a8'>30</button>
<button type='button' class='color-palette-swatch dark' style='background:#12959f' data-color='#12959f' data-index='31' title='31 · #12959f'>31</button>
<button type='button' class='color-palette-swatch light' style='background:#094e54' data-color='#094e54' data-index='32' title='32 · #094e54'>32</button>
</div></div>
<div class='color-palette-row'><span class='color-palette-family'>Braun und Sand</span><div class='color-palette-swatches'>
<button type='button' class='color-palette-swatch light' style='background:#381e11' data-color='#381e11' data-index='33' title='33 · #381e11'>33</button>
<button type='button' class='color-palette-swatch dark' style='background:#c05c20' data-color='#c05c20' data-index='34' title='34 · #c05c20'>34</button>
<button type='button' class='color-palette-swatch dark' style='background:#bf9b6b' data-color='#bf9b6b' data-index='35' title='35 · #bf9b6b'>35</button>
<button type='button' class='color-palette-swatch dark' style='background:#e9d4a7' data-color='#e9d4a7' data-index='36' title='36 · #e9d4a7'>36</button>
</div></div>
<div class='color-palette-row'><span class='color-palette-family'>Grau, Schwarz und Weiß</span><div class='color-palette-swatches'>
<button type='button' class='color-palette-swatch dark' style='background:#e7e6e1' data-color='#e7e6e1' data-index='37' title='37 · #e7e6e1'>37</button>
<button type='button' class='color-palette-swatch dark' style='background:#cfd0d2' data-color='#cfd0d2' data-index='38' title='38 · #cfd0d2'>38</button>
<button type='button' class='color-palette-swatch dark' style='background:#8a8b8f' data-color='#8a8b8f' data-index='39' title='39 · #8a8b8f'>39</button>
<button type='button' class='color-palette-swatch dark' style='background:#778590' data-color='#778590' data-index='40' title='40 · #778590'>40</button>
<button type='button' class='color-palette-swatch light' style='background:#474d4d' data-color='#474d4d' data-index='41' title='41 · #474d4d'>41</button>
<button type='button' class='color-palette-swatch light' style='background:#050608' data-color='#050608' data-index='42' title='42 · #050608'>42</button>
<button type='button' class='color-palette-swatch dark' style='background:#ffffff' data-color='#ffffff' data-index='43' title='43 · #ffffff'>43</button>
</div></div>
</div>

<!-- /part -->

<!-- part: mischen -->
### Farben mischen

Mit den Reglern mischst du eine Farbe auf zwei Arten. **Rot, Grün und Blau** ist die Art, wie der Bildschirm sie leuchten lässt. **OKLCH** beschreibt dieselbe Farbe so, wie wir sie sehen: als Helligkeit (L), Buntheit (C, englisch chroma) und Farbton (H, englisch hue, ein Winkel auf dem Farbkreis von 0 bis 360). Drehst du nur am Farbton, bleibt die Farbe gleich hell. So findest du zum Beispiel mehrere Farben, die nebeneinander gleich kräftig wirken.

<div class='color-mixer' data-start='#31a8e0'>
<div class='color-mixer-preview'>
<div class='color-mixer-sample'><span class='on-light'>Text</span><span class='on-dark'>Text</span></div>
<label class='color-mixer-hex'>Farbcode <input type='text' spellcheck='false' autocomplete='off' maxlength='7' aria-label='Farbcode'></label>
</div>
<div class='color-mixer-sliders'>
<fieldset><legend>Rot, Grün, Blau</legend>
<label><span>R</span><input type='range' min='0' max='255' step='1' data-channel='r' aria-label='Rot'><output></output></label>
<label><span>G</span><input type='range' min='0' max='255' step='1' data-channel='g' aria-label='Grün'><output></output></label>
<label><span>B</span><input type='range' min='0' max='255' step='1' data-channel='b' aria-label='Blau'><output></output></label>
</fieldset>
<fieldset><legend>OKLCH</legend>
<label><span>L</span><input type='range' min='0' max='100' step='0.5' data-channel='l' aria-label='Helligkeit'><output></output></label>
<label><span>C</span><input type='range' min='0' max='0.37' step='0.005' data-channel='c' aria-label='Buntheit'><output></output></label>
<label><span>H</span><input type='range' min='0' max='360' step='1' data-channel='h' aria-label='Farbton'><output></output></label>
</fieldset>
</div>
<p class='color-mixer-gamut' hidden>Diese Farbe kann ein Bildschirm nicht zeigen. Angezeigt und unten ausgegeben ist die nächste, die er zeigen kann.</p>
<table class='table table-sm reference-table color-mixer-codes'>
<tr><th>CSS: Farbcode</th><td><code data-code='hex'></code></td></tr>
<tr><th>CSS: Rot, Grün, Blau</th><td><code data-code='rgb'></code></td></tr>
<tr><th>CSS: OKLCH</th><td><code data-code='oklch'></code></td></tr>
<tr><th>Anaglyph-Viewer</th><td><code data-code='anaglyph'></code></td></tr>
<tr><th>Nächste Cling-Farbe</th><td><span class='color-swatch' data-code='nearest-swatch'></span> <code data-code='nearest'></code></td></tr>
</table>
</div>

`oklch(…)` verstehen alle aktuellen Browser. Der Anaglyph-Viewer kennt es nicht; dort nimmst du den Farbcode oder die drei Anteile von 0 bis 1.
<!-- /part -->

<!-- part: kontrast -->
## Kontrast

Text muss sich deutlich vom Hintergrund abheben. Schwarz auf Weiß und Weiß auf Dunkelblau sind gut lesbar, Hellgrau auf Weiß und Rot auf Grün nicht: Etwa jeder zwölfte Junge kann Rot und Grün schlecht unterscheiden. Prüfe im Zweifel, ob du den Text auch noch lesen kannst, wenn du ein Stück vom Bildschirm zurücktrittst.
<!-- /part -->

<script src='/include/color_mixer.js'></script>
