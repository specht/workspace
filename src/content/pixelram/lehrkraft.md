Lernende programmieren in C Grafik direkt in den Bildschirmspeicher (PixelRAM), in zwei Teilen mit demselben Gedanken: Ein Bild ist eine Rechenvorschrift für jedes Pixel (Formeln, Wellen), und der Bildschirm ist Speicher, den man auch lesen kann: Im Spiel »Lichtspur« erkennen sie Zusammenstöße allein mit `get_pixel`. Wer die Wellen überspringen will, kann nach »Alle Pixel« direkt mit dem Spiel weitermachen.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Variablen, Schleifen, Bedingungen in irgendeiner Sprache; C wird unterwegs erklärt. Beim Spiel kommen <code>struct</code> und Zeiger dazu, nur so weit wie nötig.</td></tr>
<tr><th>Arbeitsweise</th><td>Datei ändern, <code>make</code> (oder <code>make PROGRAM=lichtspur</code>), Vorschau neu laden. Die meisten Fragen in der ersten Stunde sind übersehene Compilerfehler: Die erste Meldung lesen lassen.</td></tr>
<tr><th>Vorlage</th><td><a href='https://github.com/specht/pixelram-starter'>pixelram-starter</a>: <code>main.c</code>, fünf Beispiele in <code>beispiele/</code>, <code>make crystal</code> für Crystal Caves (ein, zwei Minuten, lädt den Quelltext von GitHub)</td></tr>
</table>

**Im Unterricht.** Die Lichtspur ist zu zweit an einer Tastatur gespielt ein guter Stundeneinstieg; die Ausbau-Ideen am Ende (Tempo, Hindernisse, Lücken, Computergegner) lassen sich verteilen und vergleichen. Der Computergegner ist der anspruchsvollste: Er braucht dieselbe Idee wie die Kollision, nur vorausschauend.

**Glatte Kanten.** Nach »Kreiswellen« glätten die Lernenden die Treppenstufen mit adaptivem Supersampling: Die Farben an den Ecken eines Pixels entscheiden, ob eine Kante hindurchläuft, und nur dann teilt die rekursive Funktion `square` das Pixel in vier kleinere Quadrate, höchstens `LEVELS`-mal. Ab da zeichnet `draw()` jedes Bild, `color` bleibt eine reine Formel. Dazu gehört, warum Röhrenbildschirme so etwas früher von selbst weicher machten. Der Aufwand ist gemessen (WebAssembly, Interferenz): Ein Bild braucht mit `LEVELS 1` etwa 2,5-mal so lange wie ohne Glätten, mit 2 gut 5-mal, mit 3 elfmal. Für Bewegung ist 1 empfohlen; ruckelt es auf schwachen Rechnern, ist das der Grund.

**Größere Projekte.** PixelRAM ist schnell genug für Vorhaben, die über mehrere Wochen tragen und an denen sich Mathematik und Informatik treffen:

- **Raycaster:** Eine Gitterkarte aus Wänden, für jede Bildschirmspalte ein Strahl, die Wandhöhe aus dem Abstand. Verbindet beide Teile der Seite: Pixelrechnung und Tastatursteuerung.
- **Raytracer:** Für jedes Pixel ein Strahl in eine Szene aus Kugeln und Ebenen, Schnittpunkte über quadratische Gleichungen, Licht, Schatten, Spiegelungen. Mit `pixel_rgb24` sind beliebige Farben möglich, ein einzelnes Bild darf Sekunden dauern.
- **Bilddecoder:** GIF (Palette mit bis zu 256 Farben, passt genau zu `pixel_indexed8`; LZW-Kompression) oder JPEG (Huffman-Codierung, diskrete Kosinustransformation, YCbCr nach RGB). Die Bilddatei kommt mit `--embed-file` beim `emcc`-Aufruf im Makefile ins Programm und lässt sich dann mit `fopen` lesen.
