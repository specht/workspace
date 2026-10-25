Lernende schreiben in C einen Decoder für Baseline-JPEG, der das Bild mit PixelRAM zeichnet: Segmente lesen, kanonische Huffman-Tabellen bauen, Bits mit Byte Stuffing lesen, DC- und AC-Koeffizienten decodieren, dequantisieren, iDCT, MCUs zusammensetzen, YCbCr in RGB umrechnen. Am Ende erscheint ein echtes Foto, Block für Block.

<table class='table'>
<tr><th>Voraussetzungen</th><td><ul>
<li>Grundlagen in C und die ersten Abschnitte von <a href='/pixelram'>Echtzeitgrafik mit C</a>; leichter wird es nach dem <a href='/gif'>GIF-Labor</a>, das denselben Aufbau hat und dieselbe Vorlage benutzt.</li>
<li>Die drei Grundlagenseiten (<a href='/rgb' target='_blank' rel='noopener'>Farbräume</a>, <a href='/colorspace' target='_blank' rel='noopener'>Trennung von Helligkeit und Farbe</a>, <a href='/dct' target='_blank' rel='noopener'>DCT</a>) sollten vorher besprochen sein.</li>
</ul></td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Vorbereitung und alle Segmente bis SOS, mit der Liste der Marker als Zwischenstand.</li>
<li><b>2. Doppelstunde:</b> Huffman-Tabellen, Bitleser mit Byte Stuffing, der erste DC-Koeffizient (−120).</li>
<li><b>3. Doppelstunde:</b> AC-Koeffizienten mit Zick-Zack, Dequantisierung, iDCT, ein erster Block.</li>
<li><b>4. Doppelstunde:</b> MCUs, Farben, das ganze Bild, Fehlersuche mit den beiden Prüfaufgaben.</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>Alles in <code>/workspace/jpeg</code> (ein eigener Klon des PixelRAM-Starters): <code>jpeg.c</code> und die beiden Fotos, die die Lernenden mit <code>wget</code> von <code>${web_root}/jpeg/</code> laden.</li>
<li>Als Code stehen auf der Seite nur das Gerüst (Datei lesen, <code>read_byte</code>, <code>read_word</code> im Big Endian), die Tabelle <code>ZIGZAG</code> und <code>print_average</code>, alle mit Kopierknopf.</li>
<li>Jeder weitere Teil hat einen Kasten »Für deinen Decoder« mit Funktion, Signatur, was sie tun soll und was das Programm zur Probe ausgeben sollte.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Die Aufgaben prüfen Antworten, nichts im Workspace.</li>
<li>Die ersten fragen nach Werten aus der Datei (MCU, Huffman-Codes, DC-Koeffizient, Zahl der MCUs), meist zu <code>91-baseline.jpg</code>, damit sich die Antwort nicht von der Seite ablesen lässt.</li>
<li>Für jede Stufe danach gibt es eine eigene: nach der Dequantisierung, nach der iDCT und für die Lage der Blöcke in der MCU; so findet sich ein Fehler, bevor das ganze Bild falsch ist.</li>
<li>Zwei Decoder liefern nicht Pixel für Pixel dasselbe (Rundung der iDCT, weiches Hochrechnen von Cb und Cr im Browser); die letzten beiden fragen deshalb nach einem Mittelwert und einem Pixel, mit Toleranz.</li>
</ul></td></tr>
<tr><th>Lösung</th><td>Ein vollständiger Decoder, knapp 400 Zeilen, liegt im Repository unter <a href='https://github.com/specht/workspace/blob/main/src/content/jpeg/loesung/jpeg.c'><code>src/content/jpeg/loesung/jpeg.c</code></a>; das Bild auf der Seite ist mit ihm gemacht. Er stimmt mit Pillow bis auf 3 je Farbwert überein (bei Subsampling an Kanten mehr, weil er Cb und Cr nicht weich hochrechnet). Die Lernenden sehen ihn auf der Seite nicht; das Repository ist allerdings öffentlich.</td></tr>
</table>

#### Die Stufen

- »Dequantisiert« (−48 für den ersten Y-Block von `91-baseline.jpg`) erkennt −16, den Wert aus der Datei ohne Multiplizieren.
- »Nach der iDCT« (110 ± 1, oben links im selben Block) erkennt −18 (ohne die Verschiebung um 128) und 122 (nur der DC-Koeffizient).
- »Die Blöcke in der MCU« (Y = 70 bei x = 0, y = 8 in `44-baseline.jpg`) erkennt 37: den zweiten Block unter den ersten gelegt statt daneben.
- Die Zeitlupe zur iDCT setzt den Beispielblock Koeffizient für Koeffizient aus seinen Basisbildern zusammen; nach dem letzten stimmen die Werte genau mit dem Referenzdecoder überein (`test/static/idct_stepper.test.cjs`). Sie eignet sich auch, um über die Quantisierung zu sprechen: Nach wenigen Schritten sieht der Block schon fast fertig aus.

#### Die Aufgaben

- »Ohne Dequantisierung« ist eine Vorhersage nach »Dequantisiert«: Ohne die Multiplikation ist jeder Koeffizient um seinen Tabellenwert zu klein, das Bild wird fast einheitlich grau, auch die Farben verschwinden fast. Die Erklärung erscheint erst nach der Antwort; wer mag, nimmt die Multiplikation im eigenen Decoder kurz heraus.
- »Eigene Huffman-Tabellen« (58) hat Rückmeldungen für 162 (die Standardtabelle aus `44-baseline.jpg`), 77 (die Segmentlänge) und 75 (ohne die 17 Bytes davor).
- »Der zweite Block« (−138) erkennt −18, die Differenz ohne den vorigen Wert.
- »Wie viele MCUs?« (10500) erkennt 10375 (abgerundet) und 10421 (Pixel durch 64).
- »Die Farben im Mittel« (Blau 108,9 ± 1) erkennt vertauschte Cb und Cr. Die Mittelwerte hängen fast nur von den DC-Koeffizienten ab: Ein Decoder ohne Zick-Zack-Reihenfolge liegt nur 0,1 daneben.
- Das fängt »Ein Pixel mit Details« (Rot 80 ± 2 bei 800, 350 in `91-baseline.jpg`), wo derselbe Fehler etwa 159 ergibt; auch dafür gibt es eine eigene Rückmeldung.

#### Was schiefgeht

- **Little statt Big Endian.** Wer aus dem GIF-Labor kommt, liest Längen mit dem niedrigen Byte zuerst. Dann ist die erste Segmentlänge 4096 statt 16, und die Liste der Marker zeigt es sofort.
- **Byte Stuffing fehlt.** Das erste `FF 00` steht schon bei Offset 0x387, in der fünften MCU. Danach liest der Bitleser Unsinn und bricht meist bald mit »Kein gültiger Huffman-Code« ab; vom Bild sind nur die ersten vier MCUs oben links richtig.
- **Der vorige DC-Koeffizient für alle Komponenten gemeinsam.** Das Bild hat dann falsche Helligkeit und Farbe, die von Block zu Block springt.
- **Die Zick-Zack-Reihenfolge vergessen**, bei den Koeffizienten oder bei den Quantisierungstabellen: Die Blöcke bestehen aus Streifen, die Farben stimmen ungefähr.
- **ZRL falsch gezählt.** ZRL bedeutet 16 Nullen; wer 15 überspringt und dann noch den Koeffizienten einträgt, verschiebt den Rest des Blocks.
- **Abgerundet statt aufgerundet.** Weder 1000 noch 667 sind Vielfache von 16. Wer die Zahl der MCUs in einer Reihe abrundet (62 statt 63), setzt jede Reihe um eine MCU zu früh fort, und das Bild wird schräg verzerrt. Wer die Zahl der Reihen abrundet, verliert unten einen Streifen. Über den Rand zu zeichnen schadet dagegen nicht, `set_pixel_rgb` ignoriert Pixel außerhalb.
- **`M_PI` unbekannt.** Das Makefile übersetzt mit `-std=c11`, und dort gibt es `M_PI` nicht; die Seite sagt das im Kasten zur iDCT.

#### Weiter

Progressive JPEGs (die übrigen Fotos sind welche), Restart Marker, weiches Hochrechnen von Cb und Cr, eine schnelle iDCT. Ein Vergleich mit GIF lohnt sich: verlustbehaftet gegen verlustfrei, Huffman gegen LZW, Big gegen Little Endian, Bits vom höchsten gegen vom niedrigsten an.
