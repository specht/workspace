<div class='meta'>
kind: reference
search: TIC-80, Fantasy-Konsole, Lua, TIC, BOOT, cls, spr, map, mget, btn, btnp, print, trace, sfx, music, rect, circ, line, pix, Sprite, Kachel, Konsole, Cartridge, ipairs, Tabelle
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# TIC-80

<p class='abstract'>
TIC-80 zum Nachschlagen, so wie in <a href='/tic80'>Star Catcher</a>: die Konsole und die Editoren, der Bildschirm, die Befehle zum Zeichnen, für Eingaben und Töne, die Karte, und Lua, die Sprache, in der man TIC-80 programmiert. TIC-80 selbst öffnest du unter <a href='/tic80/' target='_blank'>/tic80/</a>.
</p>

## Konsole und Editoren

<!-- part: tic-konsole -->
<!-- short: new lua, save name.lua, load, run; Esc zwischen Konsole und Editor, Strg+R startet, F1 bis F5 die Editoren. -->
<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>In der Konsole</th><th>Was es tut</th></tr></thead>
<tbody>
<tr><td><code>new lua</code></td><td>ein neues Spiel in Lua, mit einem kleinen Beispielprogramm</td></tr>
<tr><td><code>save stars.lua</code></td><td>speichert als Textdatei in <code>/workspace/TIC-80</code>; danach reicht <kbd>Strg</kbd><kbd>S</kbd></td></tr>
<tr><td><code>load stars.lua</code></td><td>lädt ein gespeichertes Spiel</td></tr>
<tr><td><code>ls</code>, <code>cd ordner</code></td><td>zeigt die Dateien, wechselt den Ordner</td></tr>
<tr><td><code>run</code></td><td>startet das Spiel, wie <kbd>Strg</kbd><kbd>R</kbd></td></tr>
<tr><td><code>help api</code>, <code>help spr</code></td><td>alle Befehle; was ein Befehl erwartet</td></tr>
</tbody>
</table>
</div>

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Taste</th><th>Was sie tut</th></tr></thead>
<tbody>
<tr><td><kbd>Esc</kbd></td><td>zwischen Konsole und Editor wechseln; ein laufendes Spiel beenden</td></tr>
<tr><td><kbd>Strg</kbd><kbd>R</kbd>, <kbd>Strg</kbd><kbd>S</kbd></td><td>starten, speichern</td></tr>
<tr><td><kbd>F1</kbd> bis <kbd>F5</kbd></td><td>Code, Sprites, Karte, Töne (SFX), Musik</td></tr>
<tr><td><kbd>Strg</kbd><kbd>F</kbd>, <kbd>Strg</kbd><kbd>G</kbd>, <kbd>Strg</kbd><kbd>/</kbd></td><td>im Code-Editor: suchen, zu einer Zeile springen, eine Zeile zum Kommentar machen und zurück</td></tr>
</tbody>
</table>
</div>

Eine Spieldatei (»Cartridge«) enthält alles: oben den Code, darunter als Kommentare die Sprites, die Karte und die Töne. Bearbeite sie entweder in TIC-80 oder in Visual Studio Code, nicht in beiden zugleich.
<!-- /part -->

## Bildschirm und Farben

<!-- part: tic-bildschirm -->
<!-- short: 240 × 136 Pixel, (0, 0) oben links, y nach unten; 16 Farben 0 bis 15; TIC() 60-mal in der Sekunde. -->
Der Bildschirm ist 240 Pixel breit und 136 hoch. (0, 0) ist oben links, `x` wächst nach rechts, `y` nach **unten**. Eine Farbe ist eine Nummer von 0 bis 15:

<div class='swatches'>
<span style='background-color: #1a1c2c;'><span>0</span></span>
<span style='background-color: #5d275d;'><span>1</span></span>
<span style='background-color: #b13e53;'><span>2</span></span>
<span style='background-color: #ef7d57;'><span>3</span></span>
<span style='background-color: #ffcd75;'><span>4</span></span>
<span style='background-color: #a7f070;'><span>5</span></span>
<span style='background-color: #38b764;'><span>6</span></span>
<span style='background-color: #257179;'><span>7</span></span>
<span style='background-color: #29366f;'><span>8</span></span>
<span style='background-color: #3b5dc9;'><span>9</span></span>
<span style='background-color: #41a6f6;'><span>10</span></span>
<span style='background-color: #73eff7;'><span>11</span></span>
<span style='background-color: #f4f4f4;'><span>12</span></span>
<span style='background-color: #94b0c2;'><span>13</span></span>
<span style='background-color: #566c86;'><span>14</span></span>
<span style='background-color: #333c57;'><span>15</span></span>
</div>

Ein Spiel besteht aus Funktionen, die TIC-80 selbst aufruft:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Funktion</th><th>Wann TIC-80 sie aufruft</th></tr></thead>
<tbody>
<tr><td><code>TIC()</code></td><td>60-mal in der Sekunde, jedes Mal für ein ganzes Bild; muss es geben</td></tr>
<tr><td><code>BOOT()</code></td><td>einmal beim Start, vor dem ersten <code>TIC()</code>: zum Vorbereiten</td></tr>
</tbody>
</table>
</div>

Weil `TIC()` 60-mal in der Sekunde läuft, ist eine Bewegung um 1 Pixel pro Aufruf 60 Pixel in der Sekunde. Wer die Bilder zählt (`t = t + 1` in `TIC()`), hat eine Uhr: `t // 60` sind die Sekunden. `time()` gibt die Millisekunden seit dem Start.
<!-- /part -->

## Zeichnen

<!-- part: tic-zeichnen -->
<!-- short: cls(farbe), pix, line, rect und rectb, circ und circb, tri, print(text, x, y, farbe); b am Ende heißt nur der Rand. -->
<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Was er zeichnet</th></tr></thead>
<tbody>
<tr><td><code>cls(farbe)</code></td><td>füllt den ganzen Bildschirm</td></tr>
<tr><td><code>pix(x, y, farbe)</code></td><td>einen Pixel; <code>pix(x, y)</code> ohne Farbe liest seine Farbe</td></tr>
<tr><td><code>line(x0, y0, x1, y1, farbe)</code></td><td>eine Linie</td></tr>
<tr><td><code>rect(x, y, breite, hoehe, farbe)</code></td><td>ein gefülltes Rechteck, (x, y) ist die Ecke oben links; <code>rectb</code> nur den Rand</td></tr>
<tr><td><code>circ(x, y, radius, farbe)</code></td><td>einen gefüllten Kreis um (x, y); <code>circb</code> nur den Rand</td></tr>
<tr><td><code>elli(x, y, a, b, farbe)</code></td><td>eine Ellipse mit den Halbachsen <code>a</code> und <code>b</code>; <code>ellib</code> nur den Rand</td></tr>
<tr><td><code>tri(x1, y1, x2, y2, x3, y3, farbe)</code></td><td>ein gefülltes Dreieck; <code>trib</code> nur den Rand</td></tr>
<tr><td><code>print(text, x, y, farbe)</code></td><td>einen Text; dahinter <code>fixed</code> (alle Zeichen gleich breit, <code>true</code>/<code>false</code>), <code>scale</code> (Vergrößerung), <code>smallfont</code> (kleine Schrift). Gibt die Breite in Pixeln zurück</td></tr>
</tbody>
</table>
</div>

Was später gezeichnet wird, liegt oben. Deshalb steht `cls` meist am Anfang des Zeichnens, dann der Hintergrund, zuletzt Figuren und Text. Umlaute kennt die Schrift nicht: `ae`, `oe`, `ue`, `ss`.

Einen Text in die Mitte setzen: erst unsichtbar messen, dann zeichnen.

```lua
local w = print("GAME OVER", 0, -6)   -- über dem Bildschirm: nur messen
print("GAME OVER", (240 - w) // 2, 64, 12)
```
<!-- /part -->

## Sprites und Karte

<!-- part: tic-sprites -->
<!-- short: spr(id, x, y, durchsichtig, vergroesserung, spiegeln, drehen, breite, hoehe); ein Sprite sind 8 × 8 Pixel. -->
Ein **Sprite** sind 8 × 8 Pixel aus dem Sprite-Editor (<kbd>F2</kbd>). Rechts im Editor stehen alle, nummeriert von links oben nach rechts, 16 in einer Zeile: In der zweiten Zeile beginnt Nummer 16.

```lua
spr(1, x, 112, 14, 1, 0, 0, 2, 2)
```

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Stelle</th><th>Bedeutung</th><th>hier</th></tr></thead>
<tbody>
<tr><td><code>id</code></td><td>die Nummer des Sprites</td><td>1</td></tr>
<tr><td><code>x</code>, <code>y</code></td><td>wo seine Ecke oben links hinkommt</td><td><code>x</code>, 112</td></tr>
<tr><td><code>colorkey</code></td><td>diese Farbe bleibt durchsichtig; −1 heißt keine</td><td>14</td></tr>
<tr><td><code>scale</code></td><td>Vergrößerung: 2 zeichnet jeden Pixel als 2 × 2</td><td>1</td></tr>
<tr><td><code>flip</code></td><td>spiegeln: 0 nicht, 1 waagerecht, 2 senkrecht, 3 beides</td><td>0</td></tr>
<tr><td><code>rotate</code></td><td>drehen im Uhrzeigersinn: 0, 1 (90°), 2 (180°), 3 (270°)</td><td>0</td></tr>
<tr><td><code>w</code>, <code>h</code></td><td>so viele Sprites nebeneinander und untereinander, als ein Bild</td><td>2, 2: 16 × 16 Pixel</td></tr>
</tbody>
</table>
</div>

Mehrere Sprites als ein Bild sind die im Editor nebeneinander und untereinander: Bei Sprite 1 mit `w = 2`, `h = 2` sind das 1, 2, 17 und 18, denn eine Zeile im Editor hat 16 Sprites. Eine Figur, die nach links läuft, ist dieselbe mit `flip = 1`. Für eine Animation wechselt die Nummer, etwa `spr(1 + (t // 10) % 2 * 2, …)` für zwei Bilder, die sich alle 10 Bilder abwechseln.
<!-- /part -->

<!-- part: tic-karte -->
<!-- short: map() zeichnet die Karte aus F3, in Zellen zu 8 × 8 Pixeln; mget(zx, zy) sagt, welches Sprite in einer Zelle steht. -->
### Die Karte

Im Map-Editor (<kbd>F3</kbd>) setzt du Sprites in ein großes Raster aus **Zellen**, je 8 × 8 Pixel; ein Bildschirm voll sind 30 × 17 Zellen.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Was er tut</th></tr></thead>
<tbody>
<tr><td><code>map()</code></td><td>zeichnet den ersten Bildschirm der Karte, ab Zelle (0, 0)</td></tr>
<tr><td><code>map(zx, zy, w, h, sx, sy, colorkey)</code></td><td>zeichnet <code>w</code> × <code>h</code> Zellen ab Zelle (<code>zx</code>, <code>zy</code>) an die Bildschirmstelle (<code>sx</code>, <code>sy</code>) in Pixeln; mit <code>colorkey</code> wird eine Farbe durchsichtig</td></tr>
<tr><td><code>mget(zx, zy)</code></td><td>welches Sprite in Zelle (<code>zx</code>, <code>zy</code>) steht</td></tr>
<tr><td><code>mset(zx, zy, id)</code></td><td>setzt eins hinein, etwa um eine eingesammelte Münze zu entfernen</td></tr>
</tbody>
</table>
</div>

Von Pixeln zu Zellen geht es mit `// 8`: Eine Figur bei (`x`, `y`) steht in Zelle (`x // 8`, `y // 8`). Ob sie in eine Wand liefe, fragt `mget` an der Stelle, an der sie nach dem Schritt stünde, bevor sie ihn macht.
<!-- /part -->

## Eingaben und Töne

<!-- part: tic-eingaben -->
<!-- short: btn(0) bis btn(3) die Pfeiltasten, 4 Z, 5 X, 6 A, 7 S; btnp nur im ersten Bild; key und mouse. -->
<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Nummer</th><th>0</th><th>1</th><th>2</th><th>3</th><th>4</th><th>5</th><th>6</th><th>7</th></tr></thead>
<tbody>
<tr><td>Knopf</td><td>oben</td><td>unten</td><td>links</td><td>rechts</td><td>A</td><td>B</td><td>X</td><td>Y</td></tr>
<tr><td>Tastatur</td><td><kbd>↑</kbd></td><td><kbd>↓</kbd></td><td><kbd>←</kbd></td><td><kbd>→</kbd></td><td><kbd>Z</kbd></td><td><kbd>X</kbd></td><td><kbd>A</kbd></td><td><kbd>S</kbd></td></tr>
</tbody>
</table>
</div>

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>wahr, …</th></tr></thead>
<tbody>
<tr><td><code>btn(4)</code></td><td>solange der Knopf gedrückt ist: zum Laufen</td></tr>
<tr><td><code>btnp(4)</code></td><td>nur im ersten Bild, in dem er gedrückt ist: zum Springen, Schießen, Neustarten</td></tr>
<tr><td><code>key(nummer)</code>, <code>keyp(nummer)</code></td><td>dasselbe für jede Taste der Tastatur; die Nummern zeigt <code>help keys</code></td></tr>
<tr><td><code>mx, my, links = mouse()</code></td><td>wo die Maus ist und ob die linke Taste gedrückt ist</td></tr>
</tbody>
</table>
</div>

Spieler 2 hat die Knöpfe 8 bis 15, in derselben Reihenfolge.
<!-- /part -->

<!-- part: tic-toene -->
<!-- short: sfx(nummer, note, dauer) spielt einen Ton aus F4, etwa sfx(0, "C-6", 8); music(nummer) ein Musikstück aus F5. -->
<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Was er tut</th></tr></thead>
<tbody>
<tr><td><code>sfx(0, "C-6", 8)</code></td><td>spielt Ton 0 aus dem SFX-Editor (<kbd>F4</kbd>) als C in der 6. Oktave, 8 Bilder lang; ohne Dauer so lang, bis ein anderer Ton kommt. Erhöhte Töne mit <code>#</code>: <code>"F#4"</code></td></tr>
<tr><td><code>sfx(1, "E-4", 30, 1)</code></td><td>auf Kanal 1 statt 0: Zwei Töne auf verschiedenen Kanälen klingen zugleich, auf demselben ersetzt der neue den alten</td></tr>
<tr><td><code>sfx(-1)</code></td><td>beendet den Ton auf Kanal 0</td></tr>
<tr><td><code>music(0)</code>, <code>music()</code></td><td>spielt Musikstück 0 aus dem Musik-Editor (<kbd>F5</kbd>) in einer Schleife; ohne Nummer hört die Musik auf</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

## Lua

<!-- part: tic-lua -->
<!-- short: if … then … end, for i = 1, 10 do … end, Tabellen ab 1, #t, ipairs, ~= für ungleich, .. hängt Text an, // ganzzahlig teilen. -->
Lua ist klein: ein paar Wörter für Verzweigungen und Schleifen, und für alles Zusammengesetzte eine einzige Sorte Wert, die **Tabelle**.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Was</th><th>In Lua</th></tr></thead>
<tbody>
<tr><td>Kommentar</td><td><code>-- bis zum Zeilenende</code>, über mehrere Zeilen <code>--[[ … ]]</code></td></tr>
<tr><td>Variable</td><td><code>x = 112</code> gilt überall; <code>local x = 112</code> nur in dieser Funktion oder diesem Block</td></tr>
<tr><td>nichts, wahr, falsch</td><td><code>nil</code>, <code>true</code>, <code>false</code>; nur <code>nil</code> und <code>false</code> sind falsch, auch <code>0</code> ist wahr</td></tr>
<tr><td>vergleichen</td><td><code>==</code>, <code>~=</code> (ungleich), <code>&lt;</code>, <code>&lt;=</code>; verknüpfen mit <code>and</code>, <code>or</code>, <code>not</code></td></tr>
<tr><td>rechnen</td><td><code>+ - * /</code>; <code>/</code> gibt immer eine Kommazahl, <code>7 // 2</code> ist 3, <code>7 % 2</code> ist 1</td></tr>
<tr><td>Text</td><td><code>"Score: " .. score</code> hängt an; <code>#text</code> ist die Länge</td></tr>
<tr><td>verzweigen</td><td><code>if x &lt; 0 then</code> … <code>elseif</code> … <code>else</code> … <code>end</code></td></tr>
<tr><td>zählen</td><td><code>for i = 1, 10 do</code> … <code>end</code>, mit 10; rückwärts <code>for i = 10, 1, -1 do</code></td></tr>
<tr><td>solange</td><td><code>while x &lt; 100 do</code> … <code>end</code></td></tr>
<tr><td>Funktion</td><td><code>function newStar(i)</code> … <code>return</code> … <code>end</code></td></tr>
<tr><td>Zufall</td><td><code>math.random(0, 232)</code>: eine ganze Zahl von 0 bis 232, beide mitgezählt</td></tr>
<tr><td>runden, Betrag, kleiner</td><td><code>math.floor(x)</code>, <code>math.abs(x)</code>, <code>math.min(a, b)</code>, <code>math.max(a, b)</code></td></tr>
</tbody>
</table>
</div>

_person(ierusalimschy-celes-figueiredo)
<!-- /part -->

<!-- part: tic-tabellen -->
<!-- short: {} als Liste ab 1 oder mit Namen wie { x = 3, y = 5 }; #t, table.insert, table.remove, ipairs; rückwärts löschen. -->
### Tabellen

Eine Tabelle ist eine Liste, ein Ding mit Namen für seine Teile, oder beides:

```lua
stars = {}                          -- eine leere Liste
stars[1] = { x = 40, y = 0 }        -- ein Stern: Teile mit Namen
table.insert(stars, { x = 90, y = -20 })   -- hinten anhängen
trace(#stars)                       -- wie viele: 2, in der Konsole
for i, s in ipairs(stars) do        -- der Reihe nach, i ist die Nummer
	s.y = s.y + 1                   -- s.y ist dasselbe wie s["y"]
end
```

Eine Liste zählt in Lua **ab 1**, nicht ab 0: `stars[1]` ist der erste, `stars[#stars]` der letzte. `table.remove(stars, i)` nimmt den `i`-ten heraus, und alle dahinter rücken auf. Wer beim Durchgehen löscht, geht deshalb rückwärts, sonst wird der Nachfolger eines gelöschten übersprungen:

```lua
for i = #stars, 1, -1 do
	if stars[i].y > 136 then
		table.remove(stars, i)
	end
end
```
<!-- /part -->

## Wenn etwas nicht klappt

<!-- part: tic-fehler -->
<!-- short: trace(x) schreibt in die Konsole; attempt to index a nil value, attempt to perform arithmetic on a nil value, 'end' expected. -->
`trace(wert)` schreibt einen Wert in die Konsole, ohne das Bild zu stören; mit <kbd>Esc</kbd> siehst du ihn. Bricht das Spiel mit einer Meldung ab, steht darin die Zeile:

- `attempt to index a nil value (global 'player')` – Die Tabelle `player` gibt es noch nicht: vertippt, oder sie wird erst später angelegt.
- `attempt to perform arithmetic on a nil value` – Mit einer Variablen gerechnet, die nie einen Wert bekommen hat, oft wegen eines Tippfehlers wie `scroe`.
- `attempt to call a nil value (global 'newstar')` – Die Funktion heißt anders (`newStar`, Groß und klein zählt), oder sie steht weiter unten als ihr erster Aufruf außerhalb einer Funktion.
- `'end' expected` – Ein `end` fehlt. Die Zeilennummer in Klammern nennt, wo der Block begann.
- `'then' expected`, `'do' expected` – Nach `if` fehlt `then`, nach `for` oder `while` fehlt `do`.
<!-- /part -->

## Zum Nachschlagen

- [TIC-80 Cheatsheet](https://github.com/nesbox/TIC-80/wiki/Cheatsheet): alle Befehle auf einer Seite, auf Englisch.
- [TIC-80 Wiki: API](https://github.com/nesbox/TIC-80/wiki/API): jeder Befehl mit einer eigenen Seite und Beispielen, auf Englisch.
- [Programming in Lua](https://www.lua.org/pil/contents.html): das Buch von Roberto Ierusalimschy, die erste Ausgabe frei im Netz, auf Englisch.
