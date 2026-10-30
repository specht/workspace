Lernende schreiben Fragment-Shader in GLSL, im Stil von Shadertoy (`mainImage`, `iResolution`, `iTime`, `iMouse`), und sehen sie im Browser über Live Server: von einem Farbverlauf über leuchtende Kreise, ein Kaleidoskop und Wolken bis zu einer 3D-Szene mit Raymarching und zu Fraktalen.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Keine; GLSL sieht aus wie C, und wer <a href='/pixelram'>Echtzeitgrafik mit C</a> oder den <a href='/raytracing'>Raytracer</a> kennt, erkennt vieles wieder. Aus der Mathematik helfen Vektoren, Sinus und Kosinus und der Satz des Pythagoras.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Vorlage klonen, Vorschau, was ein Shader bekommt, ein Kreis mit glattem Rand, Fehlermeldungen, leuchtende Kreise in Bewegung mit einer Kosinus-Palette.</li>
<li><b>2. Doppelstunde:</b> ein Kaleidoskop aus Polarkoordinaten und Kacheln, Wolken aus Value Noise und fbm, dann Raymarching mit der ersten Kugel, mit Schatten unter einer Lampe in einem dunklen Studio.</li>
<li><b>3. Doppelstunde:</b> sechs Tropfen, die mit einem weichen Minimum ineinanderfließen (Metaballs), weiche Schatten, Umgebungsverdeckung, Glanz nach Blinn und Schlick, ein schwarzer, spiegelnder Boden, eine Kamera, die man mit der Maus steuert.</li>
<li><b>4. Doppelstunde:</b> der Sierpinski-Teppich in 2D, der Menger-Schwamm in 3D und ein Flug durch seine endlosen Gänge, was Rechenzeit kostet, ein eigener Shader.</li>
<li>Die Stufen bis zu den Wolken sind kurz und unabhängig voneinander; von der Kugel bis zur Maus baut jede Stufe auf der vorigen auf; Teppich, Schwamm und Flug sind jeweils ein ganzes Programm.</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>Alles in <code>/workspace/glsl-starter</code>, geklont aus <code>specht/glsl-starter</code>. Geschrieben wird nur <code>shader.frag</code>; <code>index.html</code> setzt den Kopf davor (GLSL ES 3.00 auf WebGL 2, ohne WebGL 2 GLSL ES 1.00 auf WebGL 1) und zeigt Fehler mit der Zeile in <code>shader.frag</code>.</li>
<li>Jede Stufe liegt vollständig unter <a href='https://github.com/specht/workspace/tree/main/src/content/glsl/stufen'><code>src/content/glsl/stufen</code></a>, von <code>01-verlauf.frag</code> bis <code>12-im-schwamm.frag</code>. Wer den Anschluss verliert, kopiert die passende Stufe nach <code>shader.frag</code>.</li>
<li><code>index.html?shader=NAME.frag</code> zeigt eine andere Datei aus dem Ordner; so lassen sich mehrere Shader nebeneinander halten.</li>
</ul></td></tr>
<tr><th>Jetzt bist du dran</th><td><ul>
<li>An fünf Stellen schreiben die Lernenden den Code zuerst selbst, nachdem die Seite erklärt hat, was er tun soll: <code>d</code> und <code>inside</code> des Kreises, die Abstände <code>ball</code> und <code>map</code> und die Schleife in <code>march</code> für die erste Kugel, die Runde der Schleife in <code>carpet</code> und die Runde der Schleife in <code>sponge</code>, das Gegenstück zum Teppich in 3D. Das Programm darum steht mit einer Lücke auf der Seite, und bis sie gefüllt ist, zeigt die Vorschau nur den Hintergrund, ein graues Quadrat oder einen glatten Würfel.</li>
<li>Vor dem Code der Seite stehen Tipps, ein Beispiel von Hand und ein Gerüst mit Lücken. Jede Stufe erscheint erst eine Minute nach der vorigen, ohne Uhr; der Code der Seite ist die letzte Stufe. Wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst.</li>
<li>Unter jedem Kasten liest eine Aufgabe <code>shader.frag</code> als Text und sucht die Zeilen, die er verlangt, in verschiedenen Schreibweisen (Namen, Leerzeichen, <code>length(p - c)</code> oder <code>distance</code>); die Rückmeldung nennt, was fehlt, auch ein <code>3</code> ohne Punkt. Ausführen kann der Workspace einen Shader nicht; ob er wirklich so aussieht, zeigt die Vorschau. Wer eine Aufgabe löst, kann die Lösung des Kastens sofort öffnen.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Fünf Aufgaben lesen <code>glsl-starter/shader.frag</code> (oben, »Jetzt bist du dran«): der Kreis, <code>ball</code> und <code>map</code>, <code>march</code>, die Runde in <code>carpet</code> und die in <code>sponge</code>. Jede Stufe ersetzt <code>shader.frag</code>; wer eine solche Aufgabe erst nach der nächsten Stufe prüft, bekommt eine Ablehnung.</li>
<li>Die übrigen prüfen Antworten: eine Vorhersage zur Farbe einer Ecke, Rechnungen mit den Formeln der Seite (<code>p.x</code> am Rand, der höchste Wert von <code>fbm</code>, der erste Schritt eines Strahls, das weiche Minimum zweier gleicher Abstände, was nach drei Stufen vom Teppich und nach zwei vom Schwamm bleibt) und eine Frage, warum sich der Schein der Kreise addiert.</li>
<li>Am Ende geben die Lernenden einen eigenen Shader ab (eine <code>.frag</code>-Datei aus <code>glsl-starter</code>). Ob er läuft, prüft die Abgabe nicht; man sieht ihn, indem man ihn als <code>shader.frag</code> in eine eigene Vorlage legt.</li>
</ul></td></tr>
<tr><th>Tempo</th><td><ul>
<li>Gezeichnet wird auf dem Grafikchip, auch auf einem, der im Prozessor sitzt. Oben rechts zeigt die Seite Bilder pro Sekunde und die Zahl der Pixel.</li>
<li>Schafft der Rechner weniger als 40 Bilder pro Sekunde, rechnet die Seite weniger Pixel, bis zur halben Breite und Höhe, und streckt das Bild; <code>?scale=0.5</code> oder <code>?scale=1</code> legt das fest.</li>
<li>Gemessen auf einer GeForce GTX 1060 bei 1920 × 1080: die Stufen bis zu den Wolken um 1 ms je Bild, die Kugel gut 1 ms, die Tropfen knapp 4 ms, mit Glanz und Spiegelung gut 5 ms, der Teppich 0,5 ms, der Schwamm knapp 3 ms, der Flug durch ihn knapp 4 ms.</li>
<li>Ein Grafikchip im Prozessor (etwa Intel UHD 620) ist grob zehnmal langsamer; die aufwendigen Stufen laufen dort im großen Fenster mit weniger Pixeln, bleiben aber flüssig.</li>
</ul></td></tr>
</table>

#### Was schiefgeht

- **`1` statt `1.0`.** GLSL rechnet `int` und `float` nicht von selbst um; `2 / iResolution.y` ist ein Fehler. Die Seite zeigt diese Meldung absichtlich. Dasselbe in Schleifen: `float(i)`.
- **Ein Name verdeckt eine Funktion.** Eine Variable `floor`, `length` oder `mix` macht die gleichnamige Funktion im selben Block unbrauchbar; die Meldung lautet dann etwa »function name expected«.
- **Schwarzes Bild ohne Fehlermeldung.** Meist ist die vierte Zahl in `fragColor` 0 statt 1, oder eine Rechnung ergibt `NaN` (eine Wurzel aus einer negativen Zahl, eine Division durch 0). Werte als Farbe ausgeben hilft, wie im Handbuch unter »Fehler finden«.
- **Statt der Tropfen steht eine Halbkugel in der Mitte, halb im Boden.** `move_blobs();` fehlt in `mainImage`; ohne den Aufruf sind die sechs Mittelpunkte nicht gesetzt, in Chrome alle `(0, 0, 0)`. Stufe 7 hat sechs Änderungen an verschiedenen Stellen, die leicht eine vergessen lassen; die Stufe zum Vergleichen hilft.
- **Die Vorschau zeigt den alten Stand.** Live Server lädt nach dem Speichern neu; ist die Datei nicht gespeichert (Punkt im Tab des Editors), bleibt das alte Bild.
- **Ein Shader von Shadertoy läuft nicht.** Der Runner kennt nur `iResolution`, `iTime` und `iMouse`. Shader mit `iChannel0` (Bilder, Töne, Puffer), `iFrame` oder `iDate` brauchen Änderungen. Auf einem Rechner ohne WebGL 2 laufen außerdem nur Shader, die nichts aus GLSL ES 3.00 verwenden (Arrays mit Anfangswerten, `uint`, `texture`); alle Stufen der Seite tun das nicht.
- **Es ruckelt.** Die Anzeige oben rechts sagt, wie viel der Rechner schafft. Die Seite nennt, was Rechenzeit kostet: Schritte, Strahlen, Fragen an `map`.

#### Mathematik

Vektoren mit Komponenten (`p.xy`, `p.xz`), der Abstand als Länge eines Vektors, Polarkoordinaten mit `atan`, Sinus und Kosinus für Bahnen und Paletten, eine geometrische Reihe (`fbm`), die Normale als Richtung des stärksten Anstiegs (ein Gradient, mit Differenzenquotienten genähert). Raymarching ist eine Gelegenheit für die Frage, warum ein Strahl nie zu weit springt: Der Abstand zur nächsten Fläche ist ein Radius, in dem nichts liegt.

#### Eigene Projekte

Die Seite endet mit einem eigenen Shader. Die Ideen hier geben ihm ein Ziel; bewertet wird beim Vorführen und daran, dass die Lernenden eine Zeile ihres Shaders erklären, die sie selbst verändert haben.

- **Eine Landschaft aus Formeln** (eine Doppelstunde): Himmel, Sonne, Berge und Wolken, ohne ein einziges Bild, mit dem Rauschen aus [»Wolken aus Rauschen«](#wolken-aus-rauschen) für die Berge und einer Tageszeit, die mit `iTime` vergeht.
- **Eine eigene Szene aus Abständen** (ein Projekt): ein Schneemann, eine Tasse, ein Roboter, aus Kugeln, Zylindern und Quadern, die mit dem weichen Verschmelzen aus [»Tropfen, die ineinanderfließen«](#tropfen-die-ineinanderfließen) zusammenwachsen. Wer will, vergleicht mit dem [Raytracer](/raytracing): dieselben Strahlen, dort mit einer Formel je Form auf dem Prozessor, hier mit Abstandsfunktionen auf dem Grafikchip.
- **Eine eigene Symmetrie** (eine Stunde): Das [Kaleidoskop](#ein-kaleidoskop) mit einer anderen Zahl von Spiegeln, ein Kachelmuster, das sich drehend wiederholt, ein Muster nach einem Fliesenboden aus dem Kunstunterricht. Zum Vorführen erklärt jemand, an welcher Stelle der Raum gefaltet wird.

Weiter führen das Handbuch ([GLSL](/reference/glsl), [Prozedurale Texturen](/reference/prozedurale-texturen)), die [Klötzchenwelt](/raylib) mit Vertex- und Fragment-Shader für Dreiecke, The Book of Shaders, Shadertoy und die Artikel von Íñigo Quílez.
