Die einfachste Kompression zuerst: Lauflängen, in Ruby oder Python. Lernende malen im Pixeleditor auf der Seite und sehen, wie Läufe entstehen und verschwinden, schreiben `rle` und `unrle` als Text, stoßen an `2021` auf das Problem mit Ziffern und schreiben RLE dann als Paare von Bytes. Gemessen wird an einer Pixeldatei, einem Text und einem Foto.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Grundlagen in Ruby oder Python: Schleifen, Strings, Arrays oder Listen.</td></tr>
<tr><th>Sprache</th><td>Welche Sprache eine Klasse sieht, stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein, ohne Einstellung ist es Ruby. Lernende können auf der Seite selbst umschalten; eine Aufgabe, die sie in einer Sprache gelöst haben, gilt auch in der anderen.</td></tr>
<tr><th>Einordnung</th><td>Der Einstieg in den Abschnitt »Bilder und Kompression«; <a href='/huffman'>Huffman</a>, <a href='/lzw'>LZW</a>, <a href='/gif'>GIF</a> und <a href='/jpeg'>JPEG</a> bauen darauf auf.</td></tr>
<tr><th>Ablauf</th><td>Eine Doppelstunde:<ul>
<li>Pixeleditor und »Von Hand« (10 Minuten)</li>
<li><code>rle</code> und <code>unrle</code> (30 Minuten)</li>
<li>die Ziffern als Vorhersage, <code>rle_bytes</code> und <code>unrle_bytes</code> (30 Minuten)</li>
<li>Messen (15 Minuten)</li>
<li><b>Für Schnelle:</b> PackBits.</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>Alles in <code>/workspace/rle</code>: <code>rle.rb</code>, dazu <code>diskette.raw</code>, <code>spec-gif89a.txt</code> und <code>44-baseline.jpg</code>, die die Lernenden mit <code>wget</code> laden.</li>
<li><code>diskette.raw</code> ist die Diskette aus dem GIF-Labor, achtfach vergrößert, ein Byte pro Pixel (128 × 128).</li>
<li>Auf der Seite steht nur das Gerüst mit leeren Methoden.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Drei Aufgaben laden <code>rle/rle.rb</code> im Workspace der Lernenden, als Gast, der nichts verändern kann, und rufen die Methoden mit festen Eingaben auf. Die Rückmeldung nennt den ersten Aufruf, der nicht passt, und was herauskam. Der Workspace muss laufen.</td></tr>
<tr><th>Lösung</th><td>Im Repository unter <a href='https://github.com/specht/workspace/blob/main/src/content/rle/loesung/rle.rb'><code>src/content/rle/loesung/rle.rb</code></a> und <a href='https://github.com/specht/workspace/blob/main/src/content/rle/loesung/rle.py'><code>rle.py</code></a>; <code>ruby rle.rb diskette.raw</code> gibt die Zahlen der Aufgaben aus. Die Lösung verwendet <code>chunk_while</code>; mit einer Schleife und zwei Variablen geht es genauso, und das ist für die meisten der bessere Weg.</td></tr>
</table>

#### Die Aufgaben

- »Von Hand« (`4A3B2C1D`) erkennt das fehlende `1` vor dem D und die Zahl hinter dem Zeichen.
- »Lauflängen als Text« prüft unter anderem einen Lauf von 12 (zwei Ziffern) und den letzten Lauf (`AAB`).
- »Ziffern im Text« ist eine Vorhersage: `unrle(rle('2021'))` ergibt mit dem regulären Ausdruck der Seite einen leeren String, mit einer selbst geschriebenen Schleife meist Unsinn.
- »Lauflängen als Bytes« prüft die Grenze von 255 (300-mal A sind `[255, 65, 45, 65]`).
- »Die Diskette« (690) erkennt 345, die Zahl der Läufe.

#### Was schiefgeht

- **Der letzte Lauf fehlt.** Wer nur beim Wechsel des Zeichens schreibt, verliert den letzten Lauf: `rle('AAB')` ergibt `2A`. Die Rückmeldung sagt es.
- **Ein leerer Text** ergibt bei vielen Lösungen `'0'`, wenn der erste Lauf vor der Schleife mit `text[0]` beginnt: Das ist bei einem leeren Text `nil`, und `"#{nil}"` ist leer.
- **Läufe über 255.** In `rle_bytes` passt die Länge 300 nicht in ein Byte; als Ruby-Array fällt das nicht auf, beim Speichern in eine Datei schon. Die Aufgabe prüft es mit 300 und 510.
- **`'6'` statt 6.** Ohne `to_i` ergibt `'W' * '6'` einen `TypeError`.

#### Tipps in Stufen

»Lauflängen als Text«, »Zurück zum Text« und »Lauflängen als Bytes« haben zwei oder drei Tipps: ein Beispiel von Hand, den Weg in Worten, meist ein Gerüst mit Lücken, nie die ganze Lösung. Die nächste Stufe erscheint eine Minute nach der vorigen, ohne Uhr, und wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst. Hängen viele an derselben Stelle, ist das ein Thema für die ganze Gruppe.

#### Weiter

PackBits (in TIFF) vermeidet das Verdoppeln bei Dateien ohne Läufe. Die Frage, ob `diskette.raw` spaltenweise kleiner wird, führt auf die Idee, die PNG mit seinen Filtern verfolgt: Erst die Daten so umformen, dass sie sich besser packen lassen.
