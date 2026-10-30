Lernende programmieren in Ruby einen Feuer-Effekt, wie ihn die Demoszene der 90er-Jahre zeigte: erst eine Palette von Schwarz über Rot und Gelb bis Weiß, dann eine Animationsschleife, dann einen Filterkernel, der jedes Pixel aus seinen Nachbarn mittelt, und damit ein Feuer, das nach oben lodert und abkühlt. Gezeichnet wird mit Pixelflow Canvas wie in <a href='/drawing'>»Zeichnen mit Pixelflow Canvas«</a>, das vorher eingerichtet sein sollte.

<table class='table'>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> der Farbverlauf in vier Abschnitten, die Animationsschleife, der Filterkernel an neun Pixeln und am Ring.</li>
<li><b>2. Doppelstunde:</b> das Feuer: den Kernel anwenden, Zufall, die Grenzen 0 bis 63, kalte Luft; wer früher fertig ist, macht mit den Extras weiter.</li>
</ul></td></tr>
<tr><th>Jetzt bist du dran</th><td><ul>
<li>Nach dem zweiten Abschnitt des Farbverlaufs als Beispiel schreiben die Lernenden die Zeilen für den dritten und den vierten selbst.</li>
<li>Wind von links: In einer Kopie <code>wind.rb</code> nehmen sie das doppelt gezählte Pixel links unten, <code>get_pixel(x - 1, y + 1)</code>; die Prüfung lehnt Wind von rechts mit einem eigenen Hinweis ab.</li>
<li>Den Feuer-Kernel wenden sie selbst an: zwei Schleifen über das Bild, <code>get_pixel</code> für die drei Nachbarn, die Summe durch 4.</li>
<li>Davor stehen Tipps, eine Stufe nach der anderen mit einer Minute Pause und ohne Uhr; der Code der Seite ist die letzte Stufe.</li>
<li>Unter jedem Kasten liest eine Aufgabe <code>/workspace/fire.rb</code> als Text und sucht, was er verlangt, in verschiedenen Schreibweisen (<code>i * 8 + 128</code> oder <code>128 + 8 * i</code>, <code>(0...128)</code>, <code>(0..127)</code> oder <code>128.times</code>): »Orange bis Gelb« und »Gelb bis Weiß« die Zeile des Abschnitts mit allen drei Farbwerten, »Der Kernel in zwei Schleifen« die beiden Schleifen, die drei Nachbarn mit dem doppelten unteren, das Teilen durch 4 und <code>set_pixel(x, y, c)</code>, »Ein Quadrat, das sich dreht« (Extra) <code>draw_line</code>, Kosinus und Sinus um (128, 80) mit Radius 30, die Vierteldrehung <code>Math::PI / 2</code> (Grad werden abgelehnt), Farbe 63 und <code>angle += 0.03</code>. Die Rückmeldung nennt, was fehlt. Wer eine Aufgabe löst, kann die Lösung des Kastens sofort öffnen.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Zwölf Aufgaben. Fünf lesen den Code aus den Kästen »Jetzt bist du dran« (oben). Eine weitere liest <code>/workspace/fire.rb</code> als Text, ohne es auszuführen (das Feuer läuft in einer Endlosschleife), und sucht den Kernel mit dem doppelt gezählten unteren Pixel (<code>get_pixel(x, y + 1) * 2</code>, <code>2 * …</code> oder zweimal in einer Zeile), <code>clamp(0, 63)</code> und den Zufall mit <code>if c &gt; 0</code>.</li>
<li>Die übrigen fragen Zahlen und Auswahl: eine Farbe des Verlaufs (Nummer 20 ist <code>160, 32, 0</code>), den Blauwert der letzten Farbe (248, nicht 255, weil <code>(0...16)</code> bei 15 aufhört), einen Wert des Kernels mit ganzzahliger Division (32), warum falsche Farben auftauchen, eine Vorhersage zu <code>rand(7) - 4</code> und warum das Feuer mit vertauschtem Rot und Blau blau brennt, aber genauso lodert (die Animation rechnet nur mit Nummern, die Farben stehen in der Palette).</li>
</ul></td></tr>
<tr><th>Voraussetzungen</th><td>Schleifen und Arrays in Ruby. Zweidimensionale Arrays und das Rechnen mit Nachbarn eines Pixels erklärt die Seite.</td></tr>
<tr><th>Filterkernel</th><td>Die Seite rechnet einen Kernel an neun Pixeln vor und zeigt dann Identity, Weichzeichnen, Kanten finden und den Feuer-Kernel an einem weißglühenden Ring in den Farben des Feuers, den Feuer-Kernel auch nach 4, 12 und 40 Schritten. Ein guter Anlass, über Bildbearbeitung zu sprechen. Die Bilder macht <code>bilder/kernel.py</code>.</td></tr>
<tr><th>Für Schnelle</th><td><ul>
<li>Drei Extras: »Eine Form, die brennt« (ein Quadrat, das sich dreht, mit Sinus und Kosinus; ein Kasten »Jetzt bist du dran« mit Tipps in Stufen und der Extra-Aufgabe »Ein Quadrat, das sich dreht«), »Glühen statt Brennen« (der Kernel zählt alle vier Nachbarn gleich) und »Geheimnisvolles Leuchten« (der Kernel als Tabelle, acht Nachbarn, eine violette Palette), diese beiden ohne Aufgaben.</li>
<li>Die Programme dazu liegen als <code>stufen/form.rb</code>, <code>stufen/gluehen.rb</code> und <code>stufen/leuchten.rb</code> bei.</li>
</ul></td></tr>
</table>

#### Eigene Projekte

Nach der Seite haben die Lernenden eine Palette, eine Animationsschleife und einen Filterkernel. Daraus entsteht ein eigener Effekt; bewertet wird beim Vorführen und daran, welchen Teil des Feuers die Lernenden wiederverwendet haben und warum.

- **Ein anderer Effekt aus der Demoszene** (eine Doppelstunde): ein Plasma aus Sinuswellen, die sich überlagern, ein Sternenfeld, das auf die Zuschauer zufliegt, oder ein Tunnel. Die Palette aus [»Die Palette anzeigen«](#die-palette-anzeigen) bleibt; ein Plasma wird lebendig, wenn die Palette weiterrückt statt der Pixel.
- **Wasser statt Feuer** (eine Doppelstunde): Wellen, die sich von einem Tropfen aus ausbreiten und an den Rändern zurückkommen. Wie beim Feuer rechnet jedes Pixel aus seinen Nachbarn, nur mit zwei Bildern statt einem, dem jetzigen und dem davor; der Unterschied zu [»Der Filterkernel für Feuer«](#der-filterkernel-für-feuer) ist das Stück zum Erklären.
- **Feuer, das man anfasst** (eine Stunde): Die Maus legt Glut, wo sie ist, oder bläst das Feuer zur Seite. Mausereignisse liefert `fetch_events` aus der Dokumentation des Gems; der Rest ist das Feuer der Seite.
