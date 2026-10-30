Steganographie in Ruby oder Python: Lernende verstecken Text in den untersten Bits eines Fotos (LSB), entdecken ihn mit Bitebenen und dem Chi-Quadrat-Test von Westfeld und Pfitzmann wieder und schreiben zum Schluss ein Verfahren mit Farbklassen, das die Botschaft als eine große Zahl mit wechselnder Basis schreibt, mit Würfeln im RGB-Raum.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Grundlagen in Ruby oder Python und Zahlen im Zweiersystem; »Kryptographie im Terminal« ist ein guter Einstieg in das Gespräch über Verschlüsseln und Verstecken.</td></tr>
<tr><th>Sprache</th><td>Welche Sprache eine Klasse sieht, stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein, ohne Einstellung ist es Ruby.</td></tr>
<tr><th>Bibliotheken</th><td>PNG lesen und schreiben ChunkyPNG (<code>gem install chunky_png</code>) und Pillow (<code>pip install pillow</code>); alles andere schreiben die Lernenden selbst, vom Bit bis zur Farbklasse.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Bitebenen, das Fundstück mit seinem Hinweis in Bit 1, LSB mit <code>hide</code> und <code>reveal</code>, das Codewort.</li>
<li><b>2. Doppelstunde:</b> wie viel hineinpasst und ab wann man es sieht, die Bitebene von Text und von komprimierten Daten, der Chi-Quadrat-Test.</li>
<li><b>3. Doppelstunde:</b> Farbklassen, die Zahl mit wechselnder Basis (der Vergleich mit Stunden, Minuten und Sekunden trägt), dann am Test vorbei: Würfel der Breite 3 entgehen dem Chi-Quadrat-Test, ein Test mit Gruppen findet sie, und das zweite Fundstück. Zum Schluss die eigene Abgabe.</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>In <code>/workspace/stego</code>: <code>stego.rb</code> oder <code>stego.py</code>, die Bilder <code>fundstueck.png</code>, <code>fundstueck-2.png</code>, <code>strandhaeuser.png</code> und <code>moewen.png</code> (Fotos von Ian Kobylanski, mit seiner Genehmigung) und <code>spec-gif89a.txt</code> aus der LZW-Aktivität.</li>
<li>Das Gerüst steht in diesem Repository unter <code>src/content/steganography/stufen/</code>, die Musterlösung beider Sprachen unter <code>loesung/</code>.</li>
<li><code>beispiele/make_examples.py</code> erzeugt das Fundstück und die Vergleichsbilder der Seite neu.</li>
</ul></td></tr>
<tr><th>Laufzeit</th><td>Mit Farbklassen ist die Botschaft eine einzige große Zahl, und jede Division dauert umso länger, je größer sie ist: 20.000 Zeichen brauchen etwa eine Sekunde, die ganze GIF-Spezifikation (83 KB) in Python schon 15. Darum nimmt die Seite nur den Anfang.</td></tr>
</table>

#### Was schiefgeht

- Ein JPEG als Träger zerstört die Botschaft beim Speichern; nur PNG bewahrt jedes Bit.
- Wer die Bits eines Bytes andersherum schreibt (niedrigstes zuerst) oder die Länge in Zeichen statt Bytes zählt, liest das Fundstück nicht: Umlaute sind in UTF-8 zwei Bytes.
- Fehlt die 1 vor der Botschaft in `embed`, gehen Nullbytes am Anfang verloren.
- Liest `extract` vorwärts statt vom letzten Pixel zurück, kommt Unsinn heraus.

#### Entdecken

- **Klartext** im untersten Bit zeigt in der Bitebene Streifen (in UTF-8 ist das oberste Bit der meisten Bytes 0).
- **Komprimierte oder verschlüsselte Daten** sehen aus wie Rauschen, doch gerade ihre Gleichmäßigkeit verrät sie dem Chi-Quadrat-Test: Nach dem Ersetzen kommen die Werte 2k und 2k+1 gleich oft vor.
- **Die Strandhäuser:** Der Test liegt ohne Botschaft im Himmel weit über 100 (Zeilen 0 bis 40: 163, 40 bis 80: 32), in den Zeilen mit der komprimierten Spezifikation bei 1 bis 4, unterhalb von Zeile 112 wieder wie zuvor. In den Türen (Zeilen 200 bis 240) ist er schon ohne Botschaft nur etwa 5: Feine Struktur im Foto sieht für den Test fast aus wie eine Botschaft, und gerade dort fällt eine auch am wenigsten auf.
- **Gegenmittel** (weniger verstecken, mit Schlüssel verteilen, ±1 statt Ersetzen) stehen auf der Seite; die Farbklassen beantworten das auf ihre Weise.
- **Würfel ungerader Breite** machen Gruppen von 3 oder 5 Werten gleich häufig, die quer zu den Paaren des Tests liegen: Mit der Spezifikation in Würfeln der Breite 3 ergibt der Test für die Zeilen 0 bis 40 der Strandhäuser 213, mehr als ohne Botschaft. Der Test mit Gruppen der Breite 3 ergibt 1,3. Im zweiten Fundstück (die Möwen, Breite 5) ergibt nur die Breite 5 etwa 1 (1,0), die Breiten 2 bis 8 sonst 45 bis 207.
- **Die Lehre** ist das Prinzip von Kerckhoffs: Wer nur darauf baut, dass der Gegner das Verfahren nicht kennt, verliert, sobald er ein paar Varianten ausprobiert.

#### Tipps in Stufen

»Eine Bitebene«, »Verstecken und wiederfinden«, »Der Chi-Quadrat-Test«, »Farbklassen« und »Gruppen statt Paare« haben zwei oder drei Tipps: ein kleines Beispiel von Hand, oft eines aus der Prüfung, den Weg in Worten, ein Gerüst mit Lücken. Die nächste Stufe erscheint eine Minute nach der vorigen, die ganze Lösung steht in keiner. Wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst; hängen viele an derselben Stelle, ist das ein Moment für die ganze Gruppe.

#### Die Aufgaben

- »Eine Bitebene«, »Verstecken und wiederfinden«, »Der Chi-Quadrat-Test«, und »Farbklassen« rufen die Funktionen mit kleinen Bildern auf; »Verstecken und wiederfinden« prüft dabei das genaue Format (Länge zuerst, höchstes Bit zuerst), damit das Fundstück lesbar ist.
- »Gruppen statt Paare« prüft den Test mit `width` an kleinen Bildern.
- »Das Fundstück« fragt das Codewort STRANDKORB ab, »Das zweite Fundstück« LEUCHTTURM, »Wie viel passt?« die 57.596 Bytes.
- »Ab wann sieht man es?« und »Am Test vorbei?« sind Vermutungen, »Stellen mit wechselnder Basis« eine Rechnung von Hand.

#### Eigene Projekte

Nach der Seite können die Lernenden verstecken und entdecken. Daraus wird ein Spiel zwischen beiden Seiten oder ein Verfahren für ein anderes Medium; bewertet wird daran, ob die Botschaft wieder herauskommt und ob der Test sie findet.

- **Verstecken und Suchen** (eine Doppelstunde): Jede Gruppe gibt fünf Bilder ab, in manchen steckt etwas, in manchen nicht. Die anderen suchen mit Bitebenen und dem [Chi-Quadrat-Test](#der-chi-quadrat-test), ohne zu wissen, wo; gewonnen hat, wessen Verfahren am seltensten auffällt, etwa mit einem Schlüssel für die Reihenfolge der Pixel.
- **Eine Botschaft in einem Ton** (eine Doppelstunde): Eine WAV-Datei ist eine Liste von Zahlen wie ein Bild, nur in der Zeit; das unterste Bit hört man so wenig, wie man es in [»Das unterste Bit«](#das-unterste-bit) sieht. Zum Vorführen spielt der Ton vorher und nachher.
- **Eine Botschaft in einem Text** (eine Doppelstunde): Leerzeichen am Zeilenende, unsichtbare Zeichen aus Unicode oder die Wahl zwischen zwei gleichbedeutenden Wörtern. Wer gewinnt, wenn jemand den Text abtippt oder kopiert? Das zeigt, wie zerbrechlich ein Versteck ist.
