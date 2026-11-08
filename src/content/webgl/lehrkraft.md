Lernende programmieren 3D-Grafik mit WebGL2 ohne Bibliothek, in JavaScript und GLSL: ein Dreieck mit Farben je Ecke, einen Würfel mit eigenen Matrizen, den Z-Buffer, Licht aus Normalen und am Ende ein Piratenschiff aus 3.163 Dreiecken mit Textur, das sie mit der Maus drehen und als Drahtgitter zeigen.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen, Schleifen und Arrays (die Katas bis <a href='/kata-loops'>Schleifen</a>, in einer beliebigen Sprache; die Seite erklärt das JavaScript, das sie braucht). Aus der Mathematik Sinus und Kosinus, Vektoren mit Skalar- und Kreuzprodukt (Handbuch <a href='/reference/vektoren'>Vektoren</a>); Matrizen erklärt die Seite, so weit sie gebraucht werden. <a href='/glsl'>Shader in GLSL</a> hilft, ist aber nicht nötig.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Einrichten (Ordner, Schiff mit <code>wget</code>, Vorschau mit »Go Live«), die Pipeline, das erste Dreieck mit zwei Shadern, wie die Farben zwischen den Ecken gemischt werden, Matrizen mit eigenem <code>multiply</code>.</li>
<li><b>2. Doppelstunde:</b> Drehen (eigenes <code>rotateY</code>), die Perspektive, ein Würfel, der sich dreht, ohne und mit Z-Buffer, die Normale (eigenes <code>faceNormal</code>) und Licht nach Lambert.</li>
<li><b>3. Doppelstunde:</b> das OBJ-Format (eigenes <code>parseObj</code>), eine Textur, die Kamera an der Maus, das Drahtgitter (eigenes <code>edges</code>), weitere Modelle des Kits.</li>
<li><b>Für Schnelle:</b> die Kästen »Ausprobieren«: Rückseiten weglassen (<code>CULL_FACE</code>), Licht von unten, andere Modelle, die Dreiecke zählen.</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>Alles in <code>/workspace/webgl</code>, als eigener Ordner in VS Code geöffnet, damit »Go Live« ihn ausliefert: <code>index.html</code> (bleibt, wie sie ist), <code>main.js</code> (malt), <code>math.js</code> (Matrizen und Normale), <code>model.js</code> (OBJ und Kanten).</li>
<li><code>math.js</code> und <code>model.js</code> enden mit <code>module.exports</code>: Der Browser lädt sie als gewöhnliche Skripte, die Prüfungen mit <code>node</code>.</li>
<li>Heruntergeladen mit <code>wget</code> aus <a href='https://github.com/specht/workspace-files/tree/main/webgl'><code>specht/workspace-files</code></a>: <code>ship-pirate-large.obj</code> und <code>colormap.png</code> aus dem <i>Pirate Kit</i> von Kenney (CC0); dort liegen fünf weitere Modelle, die zur selben Textur passen.</li>
<li>Jeder Stand liegt unter <a href='https://github.com/specht/workspace/tree/main/src/content/webgl/stufen'><code>src/content/webgl/stufen</code></a>: <code>main-1.js</code> (Dreieck) bis <code>main-5.js</code> (Schiff) und <code>main.js</code> (mit Drahtgitter), <code>math-1.js</code> und <code>math.js</code> (mit <code>faceNormal</code>), <code>model-1.js</code> und <code>model.js</code> (mit <code>edges</code>).</li>
</ul></td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Fünf Aufgaben rufen die Funktionen der Lernenden mit <code>node</code> auf (Prüfart <code>javascript</code>): <code>multiply</code>, <code>rotateY</code>, <code>faceNormal</code>, <code>parseObj</code> mit kleinen OBJ-Dateien, <code>edges</code>.</li>
<li>Zwei Vorhersagen (die Farbe in der Mitte des Dreiecks, der Würfel ohne Z-Buffer); was sie verrät, erscheint erst nach der Antwort.</li>
<li>Zwei Rechnungen (wo ein Punkt nach der Perspektive landet, wie hell eine Fläche nach Lambert wird) und zum Schluss eine Erklärung des Z-Buffers in eigenen Worten (Klassenboard).</li>
</ul></td></tr>
<tr><th>Jetzt bist du dran</th><td>An fünf Stellen schreiben die Lernenden die Funktion zuerst selbst, mit Tipps in Stufen; der Code der Seite ist die letzte Stufe, die Aufgabe darunter prüft die eigene Fassung. Die Shader, die Perspektivmatrix, das Laden der Textur und die Kamera sind ausgearbeitete Beispiele, die die Seite Zeile für Zeile erklärt.</td></tr>
</table>

#### Was schiefgeht

- **Die Vorschau bleibt dunkel.** Die Konsole des Browsers (in der Vorschau <kbd>F12</kbd>) zeigt den Fehler: ein Tippfehler in einem Shader (die Meldung kommt aus <code>compile</code>, mit der Zeile im Shader) oder eine Funktion aus <code>math.js</code>, die fehlt.
- **»Go Live« zeigt eine Liste von Ordnern.** Der Ordner <code>webgl</code> ist nicht als eigener Ordner geöffnet; <kbd>Strg</kbd>+<kbd>K</kbd>, <kbd>Strg</kbd>+<kbd>O</kbd>, <code>/workspace/webgl/</code>.
- **Der Würfel ist verzerrt oder fehlt.** Meist <code>multiply</code> mit vertauschten Indizes oder <code>perspective</code> mit einem Tippfehler; die Aufgabe zu <code>multiply</code> fängt das Erste.
- **Das Schiff ist schwarz oder hat falsche Farben.** <code>parseObj</code> zählt die Nummern ab 1 statt ab 0, oder die Textur ist nicht umgedreht (<code>UNPACK_FLIP_Y_WEBGL</code>).
- **Das Drahtgitter ist unvollständig.** <code>gl.drawElements</code> braucht die Zahl der Indizes, zwei je Kante und sechs je Dreieck, also <code>count * 2</code> bei <code>count</code> Ecken; mit <code>count</code> fehlt die Hälfte.

#### Die Aufgaben

- **Matrizen multiplizieren:** die Rückmeldungen erkennen vertauschte Indizes und das Produkt in falscher Reihenfolge (\(B \cdot A\) statt \(A \cdot B\)).
- **Um die y-Achse drehen:** eine Drehung in die falsche Richtung (Vorzeichen des Sinus vertauscht).
- **Wo landet der Punkt?:** 0,25; 1 ist \(x\) vor der Division, 4 multipliziert statt zu teilen.
- **Die Normale eines Dreiecks:** ohne Teilen durch die Länge, Kanten vertauscht (die Normale zeigt nach innen).
- **Wie hell?:** 0,86; 0,8 ist nur das Skalarprodukt, 0,56 vergisst das Licht von überall.
- **Eine OBJ-Datei lesen:** Nummern ab 1, eine Ecke ohne die Stelle in der Textur.
- **Die Kanten der Dreiecke:** die dritte Kante fehlt, oder die Ecken werden nicht mit 3 multipliziert.

#### Eigene Projekte

- **Eine eigene Szene** (eine Doppelstunde): mehrere Modelle des Kits in einer Szene, jedes mit eigener <code>model</code>-Matrix an seinen Platz geschoben und gedreht, eine Insel mit Palmen, Turm und Schiff. Wiederkommen Matrizen und Produkt; bewertet wird am Vorführen und am Erklären, welche Matrix was tut.
- **Glanzlichter** (eine Doppelstunde): zum Licht nach Lambert ein Glanzpunkt nach Blinn-Phong, mit der Richtung zur Kamera als neuem <code>uniform</code>. Wiederkommen Normale und Skalarprodukt; bewertet wird am Bild und an der Formel, die die Lernenden erklären.
- **Fliegen statt drehen** (ein Projekt): eine Kamera, die sich mit den Tasten durch eine Szene bewegt, wie in einem Spiel, mit einer eigenen Funktion <code>lookAt</code>. Wiederkommen Kreuzprodukt und Matrizen; bewertet wird am Spieltest.
- **Ein eigenes Modell** (ein Projekt): ein Modell in Blender bauen oder ein anderes Kit von Kenney nehmen, als OBJ speichern und laden; Modelle mit Vierecken brauchen in <code>parseObj</code> einen Fächer aus Dreiecken. Bewertet wird am Vorführen.
