Lernende programmieren in Ruby einen Feuer-Effekt, wie ihn die Demoszene der 90er-Jahre zeigte: erst eine Palette von Schwarz über Rot und Gelb bis Weiß, dann eine Animationsschleife, dann einen Filterkernel, der jedes Pixel aus seinen Nachbarn mittelt, und damit ein Feuer, das nach oben lodert und abkühlt. Gezeichnet wird mit Pixelflow Canvas wie in <a href='/drawing'>»Zeichnen mit Pixelflow Canvas«</a>, das vorher eingerichtet sein sollte.

<table class='table'>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> der Farbverlauf in vier Abschnitten, die Animationsschleife, der Filterkernel an neun Pixeln und am Ring.</li>
<li><b>2. Doppelstunde:</b> das Feuer: den Kernel anwenden, Zufall, die Grenzen 0 bis 63, kalte Luft; wer früher fertig ist, macht mit den Extras weiter.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Sechs Aufgaben. Eine liest <code>/workspace/fire.rb</code> als Text, ohne es auszuführen (das Feuer läuft in einer Endlosschleife), und sucht den Kernel mit dem doppelt gezählten unteren Pixel, <code>clamp(0, 63)</code> und den Zufall mit <code>if c &gt; 0</code>.</li>
<li>Die übrigen fragen Zahlen und Auswahl: eine Farbe des Verlaufs (Nummer 20 ist <code>160, 32, 0</code>), den Blauwert der letzten Farbe (248, nicht 255, weil <code>(0...16)</code> bei 15 aufhört), einen Wert des Kernels mit ganzzahliger Division (32), warum falsche Farben auftauchen, und eine Vorhersage zu <code>rand(7) - 4</code>.</li>
</ul></td></tr>
<tr><th>Voraussetzungen</th><td>Schleifen und Arrays in Ruby. Zweidimensionale Arrays und das Rechnen mit Nachbarn eines Pixels erklärt die Seite.</td></tr>
<tr><th>Filterkernel</th><td>Die Seite rechnet einen Kernel an neun Pixeln vor und zeigt dann Identity, Weichzeichnen, Kanten finden und den Feuer-Kernel an einem weißglühenden Ring in den Farben des Feuers, den Feuer-Kernel auch nach 4, 12 und 40 Schritten. Ein guter Anlass, über Bildbearbeitung zu sprechen. Die Bilder macht <code>bilder/kernel.py</code>.</td></tr>
<tr><th>Für Schnelle</th><td><ul>
<li>Drei Extras ohne Aufgaben: »Eine Form, die brennt« (ein Quadrat, das sich dreht, mit Sinus und Kosinus; ein Kasten »Jetzt bist du dran« mit Tipps in Stufen), »Glühen statt Brennen« (der Kernel zählt alle vier Nachbarn gleich) und »Geheimnisvolles Leuchten« (der Kernel als Tabelle, acht Nachbarn, eine violette Palette).</li>
<li>Die Programme dazu liegen als <code>stufen/form.rb</code>, <code>stufen/gluehen.rb</code> und <code>stufen/leuchten.rb</code> bei.</li>
</ul></td></tr>
</table>
