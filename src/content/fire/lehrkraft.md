Lernende programmieren in Ruby einen Feuer-Effekt, wie ihn die Demoszene der 90er-Jahre zeigte: erst eine Palette von Schwarz über Rot und Gelb bis Weiß, dann eine Animationsschleife, dann einen Filterkernel, der jedes Pixel aus seinen Nachbarn mittelt, und damit ein Feuer, das nach oben lodert und abkühlt. Gezeichnet wird mit Pixelflow Canvas wie in <a href='/drawing'>»Zeichnen mit Pixelflow Canvas«</a>, das vorher eingerichtet sein sollte.

<table class='table'>
<tr><th>Dauer</th><td>Eine bis zwei Doppelstunden.</td></tr>
<tr><th>Prüfung</th><td>Sechs Aufgaben. Eine liest <code>/workspace/fire.rb</code> als Text, ohne es auszuführen (das Feuer läuft in einer Endlosschleife), und sucht den Kernel mit dem doppelt gezählten unteren Pixel, <code>clamp(0, 63)</code> und den Zufall mit <code>if c &gt; 0</code>. Die übrigen fragen Zahlen und Auswahl: eine Farbe des Verlaufs (Nummer 20 ist <code>160, 32, 0</code>), den Blauwert der letzten Farbe (248, nicht 255, weil <code>(0...16)</code> bei 15 aufhört), einen Wert des Kernels mit ganzzahliger Division (32), warum falsche Farben auftauchen, und eine Vorhersage zu <code>rand(7) - 4</code>.</td></tr>
<tr><th>Voraussetzungen</th><td>Schleifen und Arrays in Ruby. Zweidimensionale Arrays und das Rechnen mit Nachbarn eines Pixels erklärt die Seite.</td></tr>
<tr><th>Filterkernel</th><td>Die Seite zeigt neben dem Kernel für das Feuer auch Weichzeichnen und Kanten finden, als Bilder; das ist ein guter Anlass, über Bildbearbeitung zu sprechen.</td></tr>
</table>
