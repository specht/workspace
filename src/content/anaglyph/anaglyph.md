<div class='meta'>
image: rotate-boxes.webp
</div>

# 3D-Grafik mit Anaglyphen

<p class='abstract'>
Anaglyphen sind Bilder, bei denen zwei Bilder in verschiedenen Farben übereinandergelegt werden, um einen 3D-Effekt zu erzeugen. Farbfilter in der 3D-Brille trennen die beiden Bilder, sodass jedes Auge nur eines der Bilder sieht. Dadurch entsteht der Eindruck von Tiefe und räumlicher Wahrnehmung. In diesem Tutorial lernst du, wie du verschiedene Körper in 3D mit Anaglyphen darstellen kannst.
</p>

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <span class='key'>Strg</span><span class='key'>K</span> und dann <span class='key'>F</span>. Dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='fresh-start.webp'>

## Repository klonen

Für diese Anleitung brauchst du ein Repository, das du klonen kannst, indem du auf den blauen Button »Clone Repository« klickst. Gib die folgende URL ein und bestätige mit <span class='key'>Enter</span>:

```bash
https://github.com/specht/anaglyph.git
```

<img class='full' src='git-clone.webp'>

Als nächstes musst du angeben, in welches Verzeichnis du das Repository klonen möchtest. Bestätige den Standardpfad `/workspace/` mit <span class='key'>Enter</span>.

<img class='full' src='confirm-clone-path.webp'>

Beantworte die Frage »Would you like to open the cloned repository?« mit »Open«.

<img class='full' src='open-yes-no.webp'>

Wenn du auf der linken Seite im Explorer nun die Datei `scene.ini` siehst, hast du alles richtig gemacht. Diese Datei enthält die Anweisungen für die 3D-Szene, die du erstellen wirst.

<img class='full' src='lets-start.webp'>

## Erweiterung installieren

Bevor du loslegen kannst, musst du noch eine Erweiterung installieren.
Öffne dazu links die Extensions, suche die Erweiterung »Live Server« und klicke auf »Install«, um die Erweiterung zu installieren:

<img class='full' src='live-server.webp'>

<img src='go-live.webp' class='r' style='width: 21em;'>

Wenn alles geklappt hat, solltest du unten rechts den Eintrag »Go Live« sehen. Wenn du darauf klickst, öffnet sich ein neues Tab in deinem Browser mit deiner 3D-Szene, die anfangs nur aus einem einzigen Würfel besteht.

<div style='clear: both;'></div>

<div class='hint'>
Tipp: Ziehe deinen Workspace und die Vorschau nebeneinander, damit du deine Änderungen live sehen kannst.
</div>

## Geometrische Objekte

Du solltest einen Würfel sehen, der in der Mitte des Bildschirms schwebt. Benutze die Maus, um die Szene zu drehen und zu zoomen.

<div style='text-align: center; margin: 1em 0;'>
<img src='box.webp' style='max-width: 100%;'>
</div>


Öffne die Datei `scene.ini`, indem du drauf klickst. Du solltest nun den Inhalt der Datei sehen, der ungefähr so aussieht:

```ini
shape = box
```

Ändere den Wert `box` in `sphere`, um den Würfel durch eine Kugel zu ersetzen:

```ini
shape = sphere
```

Speichere die Datei mit <span class='key'>Strg</span><span class='key'>S</span> und schaue dir die Vorschau an. Du solltest nun eine Kugel sehen:

<div style='text-align: center; margin: 1em 0;'>
<img src='sphere.webp' style='max-width: 100%;'>
</div>

<div class='hint'>
Falls sich die Vorschau nicht automatisch aktualisiert, klicke auf den »Refresh«-Button in deinem Browser oder drücke <span class='key'>F5</span>.
</div>

Probiere auch andere Formen aus, indem du den Wert für `shape` änderst. Hier sind die Formen, die du verwenden kannst:

- `box` für einen Würfel
- `sphere` für eine Kugel
- `cylinder` für einen Zylinder
- `cone` für einen Kegel
- `torus` für einen Torus (Ring)
- `plane` für ein Quadrat

Zusätzlich zu den geometrischen Körpern kannst auch ein Gitter anzeigen, indem du `shape = grid` einfügst:

```ini
shape = grid
shape = sphere
```

<div style='text-align: center; margin: 1em 0;'>
<img src='sphere-with-grid.webp' style='max-width: 100%;'>
</div>


Du kannst auch mehrere Formen kombinieren, indem du sie untereinander schreibst. Zum Beispiel:

```ini
shape = torus
shape = sphere
```

<div style='text-align: center; margin: 1em 0;'>
<img src='sphere-with-torus.webp' style='max-width: 100%;'>
</div>

## Objekte verschieben

Um Objekte zu verschieben, kannst du den Befehl `move` verwenden:

```ini
shape = grid

shape = box
move = 100, 0, 0
```

<div style='text-align: center; margin: 1em 0;'>
<img src='box-move-right.webp' style='max-width: 100%;'>
</div>

Dabei gibt `move = 100, 0, 0` an, dass das Objekt um 100 Einheiten nach rechts verschoben wird. Die drei Werte stehen für die Verschiebung in den X-, Y- und Z-Richtungen.

Die Richtungen sind wie folgt:

- die X-Achse verläuft von links nach rechts
- die Y-Achse verläuft von unten nach oben
- die Z-Achse verläuft von hinten nach vorn

Ein positiver Wert verschiebt das Objekt also nach rechts (X), nach oben (Y) oder nach vorne (Z), während ein negativer Wert es nach links, nach unten oder nach hinten verschiebt.

**Aufgabe:** Versuche, die folgende Szene zu erstellen, indem du vier Würfel erstellst:

- ein Würfel in der Mitte
- ein Würfel rechts daneben
- ein Würfel darüber
- ein Würfel davor

<div class='hint'>
Tipp: Du musst die Szene mit der Maus ein bisschen drehen, um alle Würfel gut zu sehen.
</div>

<div style='text-align: center; margin: 1em 0;'>
<img src='three-cubes.webp' style='max-width: 100%;'>
</div>

<div style='display: none;'>

```ini
shape = grid

shape = box

shape = box
move = 100, 0, 0

shape = box
move = 0, 100, 0

shape = box
move = 0, 0, 100
```

</div>

## Objekte rotieren

Um Objekte zu drehen, kannst du den Befehl `rotate` verwenden:

```ini
shape = grid

shape = box
rotate = 0, 30, 0
move = -100, 0, 0

shape = box

shape = box
rotate = 30, 0, 0
move = 100, 0, 0
```

<div style='text-align: center; margin: 1em 0;'>
<img src='rotate-boxes.webp' style='max-width: 100%;'>
</div>

Du siehst hier ein Gitter und drei Würfel:

- der erste Würfel ist um 30 Grad um die Y-Achse gedreht und nach links verschoben
- der zweite Würfel ist ganz normal in der Mitte
- der dritte Würfel ist um 30 Grad um die X-Achse gedreht und nach rechts verschoben

**Aufgabe:** Versuche, die folgende Szene zu erstellen, indem du drei Würfel erstellst und rotierst:

<div style='text-align: center; margin: 1em 0;'>
<img src='cube-star.webp' style='max-width: 100%;'>
</div>

<div style='display: none;'>

```ini
shape = box
rotate = 0, 0, -30

shape = box
rotate = 0, 0, 0

shape = box
rotate = 0, 0, 30
```
</div>

## Objekte skalieren

Um die Größe von Objekten mit einem Faktor zu multiplizieren, kannst du den Befehl `scale` verwenden:

```ini
shape = grid

shape = sphere
scale = 0.5
move = -100, 0, 0

shape = sphere
move = 0, 0, 0

shape = sphere
scale = 1.8
move = 100, 0, 0
```

In diesem Beispiel siehst du ein Gitter und drei Kugeln:

- die erste Kugel ist halb so groß wie die normale Kugel und nach links verschoben
- die zweite Kugel ist normal groß und in der Mitte
- die dritte Kugel ist 1,8-mal so groß wie die normale Kugel und nach rechts verschoben

<div class='hint books'>
Achtung: Beim Programmieren musst du meistens Kommazahlen mit einem Punkt schreiben, also <code>1.8</code> und nicht <code>1,8</code>. Wenn du ein Komma eingibst, werden die Nachkommastellen ignoriert und die Zahl wird als ganze Zahl interpretiert.
</div>

<div style='text-align: center; margin: 1em 0;'>
<img src='scaled-spheres.webp' style='max-width: 100%;'>
</div>

Du kannst ein Objekt auch in alle drei Richtungen unterschiedlich skalieren, indem du drei Werte angibst:

```ini
shape = grid

shape = sphere
scale = 1, 0.2, 1
```

Hier wurde eine Kugel nur in der Y-Richtung verkleinert (auf 20%), sodass sie wie eine Scheibe aussieht:

<div style='text-align: center; margin: 1em 0;'>
<img src='pancake.webp' style='max-width: 100%;'>
</div>

**Aufgabe:** Versuche, die folgende Szene zu erstellen, indem du eine Box erstellst und sie auf die zehnfache Breite skalierst. Verschiebe die Box anschließend um 25 Einheiten nach oben, so dass sie auf dem Gitter aufliegt:

<div style='text-align: center; margin: 1em 0;'>
<img src='slab.webp' style='max-width: 100%;'>
</div>

## Transformationen kombinieren

Verschiebungen, Drehungen und Skalierungen nennt man auch Transformationen. Du kannst mehrere Transformationen kombinieren, indem du sie hintereinander schreibst. Zum Beispiel:

```ini
shape = grid

shape = box
rotate = 0, 0, 45
move = 100, 0, 0
```

Hier wurde ein Würfel zuerst um 45 Grad um die Z-Achse gedreht und dann um 100 Einheiten nach rechts verschoben. Das Ergebnis sieht so aus:

<div style='text-align: center; margin: 1em 0;'>
<img src='rotate-then-move.webp' style='max-width: 100%;'>
</div>

Dabei ist es wichtig, in welcher Reihenfolge du die Transformationen angibst. Wenn du zuerst verschiebst und dann drehst, sieht das Ergebnis anders aus:

```ini
shape = grid

shape = box
move = 100, 0, 0
rotate = 0, 0, 45
```
<div style='text-align: center; margin: 1em 0;'>
<img src='move-then-rotate.webp' style='max-width: 100%;'>
</div>

Dieser Würfel schwebt jetzt über dem Gitter, weil er zuerst nach rechts verschoben wurde und danach erst um 45 Grad gedreht um die Z-Achse gedreht wurde, denn: jede Drehung findet immer um den Ursprung statt (und jede Skalierung auch).

## Farben und Schattierung

Du kannst die Farben der Objekte kontrollieren, um verschiedene Effekte zu erzielen:

<div style='text-align: center; margin: 1em 0; width: 100%;'>
<img src='cone-white-black-white.webp' style='max-width: 23%;'>
<img src='cone-white-black-off.webp' style='max-width: 23%;'>
<img src='cone-white-black-shade.webp' style='max-width: 23%;'>
<img src='cone-white-off-shade.webp' style='max-width: 23%;'>
</div>
<div style='text-align: center; margin: 1em 0; width: 100%; background-color: #000;'>
<img src='cone-black-white-black.webp' style='max-width: 23%;'>
<img src='cone-black-white-off.webp' style='max-width: 23%;'>
<img src='cone-black-black-shade.webp' style='max-width: 23%;'>
<img src='cone-black-off-shade.webp' style='max-width: 23%;'>
</div>

Dafür stehen dir vier Befehle zur Verfügung:

- `background` für die Hintergrundfarbe: `0.0` bis `1.0` für schwarz bis weiß
- `stroke` für die Strichfarbe:  `0.0` bis `1.0` für schwarz bis weiß / `off` für kein Strich
- `fill` für die Füllfarbe: `0.0` bis `1.0` für schwarz bis weiß / `off` für keine Füllung
- `shade` für 3D-Schattierung: `on` für 3D-Schattierung / `off` für keine 3D-Schattierung

Im folgenden Beispiel schalten wir die Strichfarbe aus und die 3D-Schattierung ein:

```ini
shape= grid

shape = torus
shade = on
stroke = off
```

<div style='text-align: center; margin: 1em 0;'>
<img src='3d-torus.webp' style='max-width: 100%;'>
</div>

### Anaglyph-Effekt ausschalten

Falls du gar keine 3D-Brille hast oder die 3D-Effekte nicht sehen möchtest, kannst du den Anaglyph-Effekt ausschalten, indem du die Zeile `anaglyph = off` hinzufügst:

<div style='text-align: center; margin: 1em 0;'>
<img src='anaglyph-off.webp' style='max-width: 100%;'>
</div>

## Code auskommentieren

Manchmal möchte man Teile eines Programms vorübergehend deaktivieren, ohne sie zu löschen. Dafür kannst du Zeilen auskommentieren, indem du ein Semikolon `;` am Anfang der Zeile setzt. Zum Beispiel:

```ini
;shape = grid
shape = box
```

<div class='hint'>
Tipp: Statt eines Semikolons (<code>;</code>) kannst du auch ein Hashtag (<code>#</code>) verwenden, um eine Zeile auszukommentieren.
</div>

Um schnell ganze Blöcke auszukommentieren, kannst du mehrere Zeilen markieren und dann <span class='key'>Strg</span><span class='key'>K</span> und <span class='key'>Strg</span><span class='key'>C</span> (für »comment«) drücken. Um die Kommentare wieder zu entfernen, markiere die Zeilen und drücke <span class='key'>Strg</span><span class='key'>K</span> und <span class='key'>Strg</span><span class='key'>U</span> (für »uncomment«). Es gibt auch weitere Shortcuts dafür, die du im Menü finden kannst.

## Animationen

Während deine Szene angezeigt wird, läuft im Hintergrund eine Uhr mit. Die Anzahl der Sekunden seit dem Start der Szene wird in der Variablen `t` gespeichert. Du kannst diese Variable verwenden, um Animationen zu erstellen:

```ini
shape = torus
rotate = 0, t * 30, 0
```

In diesem Beispiel wird der Torus um 30 Grad pro Sekunde um die Y-Achse gedreht. Das bedeutet, dass er nach 3 Sekunden 90 Grad geschafft hat und nach 12 Sekunden eine volle Umdrehung gemacht hat.

**Profitipp:** Mit Hilfe der Sinus- und Cosinusfunktionen kannst du auch sanfte Bewegungen erzeugen:

```ini
shape = box
rotate = cos(t * 2) * 10, 0, sin(t * 3) * 10
```

**Aufgabe:** Erstelle eine Animation mit verschiedenen Objekten!