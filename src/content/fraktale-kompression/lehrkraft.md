Lernende schreiben in Python mit numpy einen Encoder und einen Decoder für fraktale Bildkompression: Jeder Block eines Fotos wird durch einen größeren, ähnlichen Block desselben Fotos beschrieben, und der Decoder macht aus jedem beliebigen Startbild in wenigen Schritten wieder das Foto, erst in Graustufen, dann in Farbe wie bei JPEG. Am Ende messen sie in Dezibel, vergleichen mit JPEG und sehen, wo das Verfahren gut ist und warum es sich nicht durchgesetzt hat.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen, Schleifen und Listen in Python (die Katas bis <a href='/kata-lists'>Listen</a>). numpy ist neu und wird auf der Seite erklärt, so weit sie es braucht (Ausschnitte <code>a[y:y + 8, x:x + 8]</code>, <code>reshape</code> und <code>mean</code>, Rechnen mit ganzen Arrays, <code>@</code>, <code>argmin</code>). Hilfreich, aber nicht nötig: <a href='/jpeg'>JPEG</a> oder <a href='/gif'>GIF</a> davor, damit der Unterschied zu einem Format, das Farben speichert, greifbar ist; aus der Mathematik Mittelwert und Summenzeichen.</td></tr>
<tr><th>Sprache</th><td>Nur Python: Die Suche über 18 408 Domain-Blöcke für jeden der 2400 Range-Blöcke braucht numpy. numpy und Pillow installieren die Lernenden selbst mit <code>pip install numpy pillow</code>.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> die Kopiermaschine (ein Sierpinski-Dreieck aus jedem Startbild, Vorhersage mit Rauschen, der Fixpunktsatz von Banach in einem Satz), die Idee für ein Foto, das Foto als Array, <code>shrink</code> selbst, die acht Lagen, Kontrast und Helligkeit mit der Methode der kleinsten Quadrate, <code>fit</code> selbst, der beste Partner eines Blocks.</li>
<li><b>2. Doppelstunde:</b> der Encoder für alle Blöcke auf einmal (ausgearbeitet und erklärt), die Größe des Codes, <code>decode_step</code> selbst, die Schritte aus Grau, warum <code>s</code> höchstens 1 ist, dann Farbe wie bei JPEG (YCbCr, Cb und Cr halb so groß, jeder Kanal mit eigenem Code), die Vorhersage zum ersten Schritt aus einem anderen Foto und die Galerie mit Verlauf und Rauschen.</li>
<li><b>3. Doppelstunde:</b> doppelt so groß decodieren, MSE und PSNR in Dezibel (mit einer Reihe derselben Ecke bei 25 bis 40 dB und einer Rechnung), die Vorhersage gegen JPEG, die Fotos mit dem Skytree und dem Kirschbaum (wo es gut geht und wo nicht), die Geschichte und warum heute kaum jemand fraktal komprimiert.</li>
<li><b>Für Schnelle</b> (auf der Seite als »Extra« markiert): große und kleine Blöcke mit einem Quadtree (ausgearbeitet, mit einem Bild, wo der Encoder geteilt hat), dazu die Ideen in »Wie geht es weiter?« (wirklich in Bits speichern, Farbe, schnellere Suche, andere Formen).</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>Alles in <code>/workspace/fraktal</code>: <code>kopiermaschine.py</code> und <code>fraktal.py</code> (Encoder und Decoder, aufgerufen mit <code>encode</code>, <code>decode</code> und <code>vergleich</code>); bis zum Decoder in Graustufen schreibt <code>fraktal.py encode</code> den Code nach <code>code.npy</code>, ab dem Kapitel zur Farbe die drei Codes nach <code>code.npz</code>; <code>decode</code> schreibt die Bilder <code>schritt-0.png</code> bis <code>schritt-8.png</code>.</li>
<li>Die Fotos laden die Lernenden mit <code>wget</code> von der Website (<code>/fraktal/</code>): <code>strandhaeuser.png</code>, <code>moewen.png</code>, <code>skytree.png</code>, <code>kirschbluete.png</code>, alle 480 × 320 Pixel, von Ian Kobylanski mit seiner Genehmigung; die Originale in der größten Fassung liegen in <code>fotos/</code>.</li>
<li>Jeder Stand liegt unter <code>src/content/fraktale-kompression/stufen</code>: <code>kopiermaschine.py</code>, <code>fraktal-1.py</code> (<code>shrink</code>) bis <code>fraktal-4.py</code> (Decoder in Graustufen), <code>fraktal-5.py</code> (Farbe), <code>fraktal-6.py</code> (Startbilder), <code>fraktal-7.py</code> (vergrößern, vergleichen), <code>fraktal-8.py</code> mit dem Quadtree aus dem Extra; <code>loesung/fraktal.py</code> ist der letzte Stand.</li>
</ul></td></tr>
<tr><th>Laufzeiten</th><td>Im Workspace gemessen: der Encoder 3 bis 12 Sekunden für ein Foto, je nachdem, wie viele Workspaces auf dem Server gerade rechnen, der Decoder wenige Sekunden für acht Schritte, doppelt so groß etwa viermal so lange. Die Suche nach dem Partner eines Blocks mit <code>fit</code> in Schleifen (2. Abschnitt) braucht etwa eine Sekunde; für alle Blöcke wären es mit Schleifen viele Minuten, darum der Encoder mit numpy.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Drei Aufgaben rufen die Funktionen der Lernenden auf (Prüfart <code>python</code>): <code>shrink</code>, <code>fit</code> (auch mit begrenztem <code>s</code> und einem Block ohne Kontrast) und <code>decode_step</code> (auch: liest es aus dem alten Bild, sind x und y richtig herum).</li>
<li>Drei Vorhersagen (die Kopiermaschine mit Rauschen, ein Schritt des Decoders aus dem Möwenfoto, fraktal gegen JPEG); was sie verrät, erscheint erst nach der Antwort.</li>
<li>Zwei Rechnungen: wie viele Bytes der Code der Helligkeit braucht, und das PSNR eines Bildes, das überall um 5 danebenliegt.</li>
</ul></td></tr>
<tr><th>Jetzt bist du dran</th><td>An drei Stellen schreiben die Lernenden die Funktion zuerst selbst, mit Tipps in Stufen: <code>shrink</code>, <code>fit</code>, <code>decode_step</code>. Der schnelle Encoder mit allen Domain-Blöcken auf einmal ist ein ausgearbeitetes Beispiel, das die Seite Zeile für Zeile erklärt.</td></tr>
</table>

#### Was schiefgeht

- **`ModuleNotFoundError: No module named 'numpy'`.** `pip install numpy pillow` fehlt oder lief in einem anderen Workspace.
- **`cannot reshape array`.** `shrink` bekommt ein Array mit einer ungeraden Zahl von Zeilen oder Spalten; meist ist der Ausschnitt des Domain-Blocks falsch (`2 * R` vergessen).
- **Das Bild bleibt grau oder flackert.** `decode_step` schreibt in dasselbe Bild, aus dem es liest, oder `s` ist nicht begrenzt; beides fängt die Aufgabe ab.
- **`decode` findet `code.npy` nicht.** Erst `python3 fraktal.py encode`, dann `decode`.
- **Die Zahlen weichen ein wenig ab.** Andere Versionen von numpy runden in der letzten Stelle anders; PSNR um ein Zehntel Dezibel daneben ist kein Fehler.

#### Die Aufgaben

- **kopiermaschine:** Wer »Dreieck aus Rauschen« wählt, denkt, die Kopien behielten ihren Inhalt; die Rückmeldung rechnet vor, dass ein Stück Rauschen nach acht Runden kleiner als ein Pixel ist.
- **passen:** Ein Fehler, der mit dem unbegrenzten `s` gerechnet wird, fällt beim Block mit dreifachem Kontrast auf; ein Nenner ohne Untergrenze beim Block ohne Kontrast (`nan`).
- **decodieren:** Der Fall mit zwei Blöcken, die sich überlappen, erkennt das Schreiben ins alte Bild; der Block bei x = 8, y = 0 vertauschte Koordinaten.
- **erster-schritt:** Wer »die Möwen, kaum verändert« wählt, denkt, ein Schritt korrigiere das Bild nur ein wenig; tatsächlich wird jeder Block neu geschrieben. Wer »übereinander« wählt, stellt sich ein Mischen vor. Die Galerie danach zeigt Schritt 1 und 8 für drei Startbilder.

#### Eigene Projekte

- **Der eigene Code als Datei** (eine Doppelstunde): `s` und `o` runden, alles mit 27 Bits je Block in eine Datei packen und wieder lesen; vergleichen, wie groß sie ist und wie viel das Runden kostet. Bewertet wird das Vorführen: Datei, Größe, decodiertes Bild.
- **Farbe sparsamer** (eine Doppelstunde): Cb und Cr mit denselben Partnern wie Y und nur eigenem `s` und `o`, oder noch gröber gespeichert; messen, was das an Bytes spart und an Dezibel kostet. Bewertet wird die Tabelle und die Erklärung, wo die Bytes hingehen.
- **Rechtecke statt Quadrate** (ein Projekt): Blöcke, die an Kanten des Bildes in zwei Rechtecke geteilt werden (HV-Partition), statt in vier Quadrate wie im Quadtree des Extras, mit einer Tabelle aus eigenen Messungen: Grenze, Bytes, PSNR. Bewertet werden die Tabelle und das Erklären des eigenen rekursiven Codes.
- **Ein eigenes Foto** (eine Stunde): ein Foto suchen, das sich gut codieren lässt, und eines, das schlecht geht, und begründen, warum. Bewertet wird die Begründung.
