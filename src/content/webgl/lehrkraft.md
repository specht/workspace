Lernende programmieren 3D-Grafik mit WebGL2 ohne Bibliothek, in JavaScript und GLSL: ein Dreieck mit Farben je Ecke, einen Würfel mit eigenen Matrizen, den Z-Buffer, Licht aus Normalen und am Ende ein Pferd aus Porzellan aus 22.388 Dreiecken (Poly Haven, CC0), das sie mit der Maus drehen, als Drahtgitter und mit Flat Shading zeigen, mit Glanzlichtern nach Blinn-Phong und vier Texturen: Farbe, Normal Map, Roughness und Ambient Occlusion.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen, Schleifen und Arrays (die Katas bis <a href='/kata-lists'>Listen</a>, in einer beliebigen Sprache; die Seite erklärt das JavaScript, das sie braucht). <a href='/glsl'>Shader in GLSL</a> davor, denn dort lernen sie den Fragment-Shader kennen, und <a href='/raytracing'>Raytracing</a>, denn dort kommen Vektoren, Normalen und das Licht nach Lambert vor. Aus der Mathematik Sinus und Kosinus, Vektoren mit Skalar- und Kreuzprodukt (Handbuch <a href='/reference/vektoren'>Vektoren</a>); Matrizen erklärt die Seite, so weit sie gebraucht werden.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Einrichten (Ordner, Pferd und Texturen mit <code>wget</code>, Vorschau mit »Go Live«), die Pipeline, das erste Dreieck mit zwei Shadern in eigenen Dateien, wie die Farben zwischen den Ecken gemischt werden, Matrizen mit eigenem <code>multiply</code>.</li>
<li><b>2. Doppelstunde:</b> Drehen (eigenes <code>rotateY</code>), die Perspektive, ein Würfel, der sich dreht, ohne und mit Z-Buffer, der Z-Buffer als Bild, die Normale (eigenes <code>faceNormal</code>) und Licht nach Lambert.</li>
<li><b>3. Doppelstunde:</b> das OBJ-Format (eigenes <code>parseObj</code>), das Pferd mit der Kamera an der Maus, das Drahtgitter (eigenes <code>edges</code>), Flat und Smooth Shading (eigenes <code>flatNormals</code>).</li>
<li><b>4. Doppelstunde:</b> Glanzlicht nach Blinn-Phong, die Textur mit Mipmaps, dann Normal Map, Roughness Map und Ambient Occlusion Map, jede mit einer Taste ein- und auszuschalten.</li>
<li><b>Für Schnelle:</b> die Kästen »Ausprobieren«: den Z-Buffer selbst als Grau malen, Rückseiten weglassen (<code>CULL_FACE</code>), Licht von unten, andere Exponenten, die Maps einzeln abschalten.</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>Alles in <code>/workspace/webgl</code>, als eigener Ordner in VS Code geöffnet, damit »Go Live« ihn ausliefert: <code>index.html</code> (bleibt, wie sie ist), <code>main.js</code> (malt), <code>shader.vert</code> und <code>shader.frag</code> (die Shader, mit <code>fetch</code> geladen), <code>math.js</code> (Matrizen und Normalen), <code>model.js</code> (OBJ und Kanten).</li>
<li>VS Code färbt <code>.vert</code> und <code>.frag</code> wie C; das stellen die Einstellungen ein, mit denen jeder Workspace startet.</li>
<li><code>math.js</code> und <code>model.js</code> enden mit <code>module.exports</code>: Der Browser lädt sie als gewöhnliche Skripte, die Prüfungen mit <code>node</code>.</li>
<li>Heruntergeladen mit <code>wget</code> aus <a href='https://github.com/specht/workspace-files/tree/main/webgl'><code>specht/workspace-files</code></a>: <code>horse.obj</code> und vier Texturen zu je 2048 × 2048 Pixeln, zusammen etwa 4 MB, vom Modell <i>Horse Statue 01</i> von Rico Cilliers aus Poly Haven (CC0).</li>
<li>Jeder Stand liegt unter <a href='https://github.com/specht/workspace/tree/main/src/content/webgl/stufen'><code>src/content/webgl/stufen</code></a>: <code>main-1.js</code> (Dreieck) bis <code>main-9.js</code> (Farbtextur) und <code>main.js</code> (alle Maps), die Shader von <code>shader-1</code> bis <code>shader.vert</code> und <code>shader.frag</code>, <code>math-1.js</code>, <code>math-2.js</code> (mit <code>faceNormal</code>) und <code>math.js</code> (mit <code>flatNormals</code>), <code>model-1.js</code> und <code>model.js</code> (mit <code>edges</code>).</li>
</ul></td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Sechs Aufgaben rufen die Funktionen der Lernenden mit <code>node</code> auf (Prüfart <code>javascript</code>): <code>multiply</code>, <code>rotateY</code>, <code>faceNormal</code>, <code>parseObj</code> mit kleinen OBJ-Dateien, <code>edges</code>, <code>flatNormals</code>.</li>
<li>Drei Vorhersagen (die Farbe in der Mitte des Dreiecks, der Würfel ohne Z-Buffer, ein größerer Exponent beim Glanz); was sie verrät, erscheint erst nach der Antwort.</li>
<li>Drei Rechnungen (wo ein Punkt nach der Perspektive landet, wie hell eine Fläche nach Lambert wird, wie hell ein Glanzpunkt ist), eine Frage zur Roughness Map und zum Schluss eine Erklärung des Z-Buffers in eigenen Worten (Klassenboard).</li>
</ul></td></tr>
<tr><th>Jetzt bist du dran</th><td>An sechs Stellen schreiben die Lernenden die Funktion zuerst selbst, mit Tipps in Stufen; der Code der Seite ist die letzte Stufe, die Aufgabe darunter prüft die eigene Fassung. Die Shader, die Perspektivmatrix, das Laden der Texturen, die Kamera und die Normal Map sind ausgearbeitete Beispiele, die die Seite Zeile für Zeile erklärt.</td></tr>
</table>

#### Was schiefgeht

- **Die Vorschau bleibt dunkel.** Die Konsole des Browsers (in der Vorschau <kbd>F12</kbd>) zeigt den Fehler: ein Tippfehler in einem Shader (die Meldung kommt aus <code>compile</code>, mit der Zeile im Shader), ein Kommentar vor <code>#version 300 es</code>, oder eine Funktion aus <code>math.js</code>, die fehlt.
- **»Go Live« zeigt eine Liste von Ordnern.** Der Ordner <code>webgl</code> ist nicht als eigener Ordner geöffnet; <kbd>Strg</kbd>+<kbd>K</kbd>, <kbd>Strg</kbd>+<kbd>O</kbd>, <code>/workspace/webgl/</code>.
- **Der Würfel ist verzerrt oder fehlt.** Meist <code>multiply</code> mit vertauschten Indizes oder <code>perspective</code> mit einem Tippfehler; die Aufgabe zu <code>multiply</code> fängt das Erste.
- **Das Pferd ist zerrissen oder fehlt.** <code>parseObj</code> zählt die Nummern ab 1 statt ab 0, oder die Ecken haben nicht acht Zahlen; die Aufgabe zu <code>parseObj</code> fängt beides.
- **Nach <kbd>F</kbd> bleibt Flat Shading, auch nach dem zweiten Drücken.** <code>flatNormals</code> ändert das übergebene Array, statt eine Kopie zu machen; dann sind auch die Normalen für Smooth Shading weg. Die Aufgabe zu <code>flatNormals</code> prüft das.
- **Das Drahtgitter ist unvollständig.** <code>gl.drawElements</code> braucht die Zahl der Indizes, zwei je Kante und sechs je Dreieck, also <code>count * 2</code> bei <code>count</code> Ecken; mit <code>count</code> fehlt die Hälfte.
- **Die Textur sitzt falsch oder steht auf dem Kopf.** <code>UNPACK_FLIP_Y_WEBGL</code> fehlt, oder der <code>sampler2D</code> liest aus einer anderen Einheit, als <code>gl.uniform1i</code> sagt.

#### Die Aufgaben

- **Matrizen multiplizieren:** die Rückmeldungen erkennen vertauschte Indizes und das Produkt in falscher Reihenfolge (\(B \cdot A\) statt \(A \cdot B\)).
- **Um die y-Achse drehen:** eine Drehung in die falsche Richtung (Vorzeichen des Sinus vertauscht).
- **Wo landet der Punkt?:** 0,25; 1 ist \(x\) vor der Division, 4 multipliziert statt zu teilen.
- **Die Normale eines Dreiecks:** ohne Teilen durch die Länge, Kanten vertauscht (die Normale zeigt nach innen).
- **Wie hell?:** 0,86; 0,8 ist nur das Skalarprodukt, 0,56 vergisst das Licht von überall.
- **Eine OBJ-Datei lesen:** Nummern ab 1, eine Ecke ohne die Stelle in der Textur.
- **Die Kanten der Dreiecke:** die dritte Kante fehlt, oder die Ecken werden nicht mit 3 multipliziert.
- **Flache Normalen:** nur die erste Ecke jedes Dreiecks bekommt die Normale, oder das übergebene Array wird geändert.
- **Wie hell ist der Glanz?:** 0,18; 0,45 vergisst den Faktor 0,4, 0,39 die Potenz, 15,68 multipliziert mit 40.

#### Eigene Projekte

- **Eine eigene Szene** (eine Doppelstunde): mehrere Pferde oder andere Modelle in einer Szene, jedes mit eigener <code>model</code>-Matrix an seinen Platz geschoben und gedreht. Wiederkommen Matrizen und Produkt; bewertet wird am Vorführen und am Erklären, welche Matrix was tut.
- **Ein anderes Modell von Poly Haven** (eine Doppelstunde): ein Modell mit eigenen Maps herunterladen, mit <code>werkzeug/gltf_to_obj.py</code> der Seite in OBJ umwandeln (oder in Blender als OBJ speichern) und laden; bei einem Modell aus Metall die Metallic Map dazunehmen. Bewertet wird am Vorführen und an der Erklärung der Maps.
- **Fliegen statt drehen** (ein Projekt): eine Kamera, die sich mit den Tasten durch eine Szene bewegt, wie in einem Spiel, mit einer eigenen Funktion <code>lookAt</code>. Wiederkommen Kreuzprodukt und Matrizen; bewertet wird am Spieltest.
- **Schatten** (ein Projekt): die Szene einmal von der Lampe aus in einen eigenen Z-Buffer malen (eine Shadow Map, nach Lance Williams) und beim zweiten Malen nachsehen, ob ein Pixel von der Lampe aus verdeckt ist. Wiederkommen Z-Buffer und Matrizen; bewertet wird am Bild.
