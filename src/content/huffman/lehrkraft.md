Huffman-Codes zum Selberbauen, in Ruby oder Python: Lernende kommen vom Morsecode über präfixfreie Codes zum Baum, schreiben `frequencies`, `build_tree`, `code_table`, `encode` und `decode`, vergleichen mit LZW und `gzip` und bauen zum Schluss kanonische Codes, wie JPEG sie im DHT-Segment speichert.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Grundlagen in Ruby oder Python: Funktionen, Schleifen, Listen und Hashes oder <code>dict</code>. Eine Methode, die sich selbst aufruft, kommt in <code>code_table</code> vor; die Seite erklärt sie in zwei Sätzen, mehr Rekursion braucht es nicht.</td></tr>
<tr><th>Sprache</th><td>Welche Sprache eine Klasse sieht, stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein, ohne Einstellung ist es Ruby. Lernende können auf der Seite selbst umschalten; eine Aufgabe, die sie in einer Sprache gelöst haben, gilt auch in der anderen.</td></tr>
<tr><th>Einordnung</th><td>Huffman passt vor <a href='/lzw'>LZW</a> oder danach; das <a href='/jpeg'>JPEG-Labor</a> baut darauf auf.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Morsecode, präfixfrei, der Baum mit der Zeitlupe und von Hand, dann <code>frequencies</code> und <code>build_tree</code>.</li>
<li><b>2. Doppelstunde:</b> <code>code_table</code>, <code>encode</code>, <code>decode</code>, Messen an der Spezifikation von GIF und dem Foto, kanonische Codes.</li>
<li><b>Für Schnelle:</b> die Bits in echte Bytes packen.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/huffman</code>: <code>huffman.rb</code>, dazu <code>spec-gif89a.txt</code> und <code>44-baseline.jpg</code>, die die Lernenden mit <code>wget</code> laden. Auf der Seite steht nur das Gerüst mit leeren Methoden; die Regeln und Hinweise zu Ruby stehen im Text.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Vier Aufgaben laden <code>huffman/huffman.rb</code> im Workspace der Lernenden, als Gast, der nichts verändern kann, und rufen die Methoden auf. Der Workspace muss laufen.</li>
<li>Weil es bei Gleichstand mehrere richtige Bäume gibt, prüft »Der Baum und die Codes« nicht die Codes, sondern was für jeden richtigen Baum gilt: Alle Bytes haben einen Code, kein Code ist der Anfang eines anderen, und die Zahl der Bits ist die kleinstmögliche (23 für <code>ABRACADABRA</code>, 91 für die Rhabarberbarbara, 34.920 für 5000 Bytes mit vielen Werten).</li>
<li>Die Rückmeldung nennt den ersten Aufruf, der nicht passt.</li>
</ul></td></tr>
<tr><th>Lösung</th><td>Im Repository unter <a href='https://github.com/specht/workspace/blob/main/src/content/huffman/loesung/huffman.rb'><code>src/content/huffman/loesung/huffman.rb</code></a> und <a href='https://github.com/specht/workspace/blob/main/src/content/huffman/loesung/huffman.py'><code>huffman.py</code></a>; <code>ruby huffman.rb spec-gif89a.txt</code> gibt die Zahlen der Aufgaben aus. Die Lösungen der Programmieraufgaben sehen Lernende nach einer richtigen Antwort auch auf der Karte.</td></tr>
</table>

#### Die Aufgaben

- »Von Hand« (23) erkennt 88 (8 Bits pro Zeichen) und 33 (3 Bits für jedes).
- Dass verschiedene Bäume dieselbe Zahl ergeben, ist eine gute Frage an die Klasse: Die Zahl der Bits ist die Summe der Gewichte aller Knoten (2 + 4 + 6 + 11), und die hängt nicht davon ab, welchen von zwei gleich schweren Bäumen man nimmt.
- »Die Spezifikation von GIF« (45.240 Bytes) erkennt Bits statt Bytes.
- »Kanonische Codes« prüft fünf feste Fälle, darunter die DC-Tabelle aus `44-baseline.jpg`, die auf der JPEG-Seite steht.

#### Tipps in Stufen

»Der Baum und die Codes«, »Codieren und decodieren« und »Kanonische Codes« haben je drei Tipps: ein Beispiel von Hand, den Weg in Worten, ein Gerüst mit Lücken. Die nächste Stufe erscheint eine Minute nach der vorigen, die ganze Lösung steht in keiner. Wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst; hängen viele an derselben Stelle, ist das ein Moment für die ganze Gruppe.

#### Was schiefgeht

- **Nur einmal sortiert.** Wer die Liste nur am Anfang sortiert und neue Bäume hinten anhängt, nimmt später nicht immer die beiden leichtesten. Bei `ABRACADABRA` kommen dann 24 statt 23 Bits heraus; die Rückmeldung fragt genau danach.
- **Der Baum aus einem Blatt.** Für `AAAA` liefert `code_table` ohne Sonderfall den leeren Code, und `decode` findet nie ein Byte. Die Rückmeldung zeigt `{65 => ""}`.
- **Zeichen statt Bytes.** Wer mit `each_char` zählt, bekommt Strings als Schlüssel statt Zahlen; bei Umlauten wird außerdem ein Zeichen zu zwei Bytes. Die Prüfung von `frequencies` enthält »Grüße«.
- **Die Tabelle vergessen.** Beim Vergleich mit LZW zählt Huffman hier nur die Bits des Texts, nicht die Tabelle, die eine echte Datei mitschicken muss. Das ist Absicht und der Übergang zu den kanonischen Codes.

#### Weiter

Bits in Bytes packen (`pack('B*')`), die Grenze von 16 Bits bei JPEG, arithmetische Codierung. Historisch: Fano stellte die Frage 1951 als Hausarbeit, Huffman veröffentlichte die Lösung 1952.
