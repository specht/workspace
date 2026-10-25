LZW zum Selberbauen, in Ruby oder Python: Lernende komprimieren einen Text von Hand und mit der Zeitlupe auf der Seite, schreiben `compress` und `decompress` und messen an echten Dateien, wie viel LZW spart, und wann gar nichts.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Grundlagen in Ruby oder Python: Methoden oder Funktionen, Schleifen, Arrays oder Listen, Hashes oder <code>dict</code>. Bytes, <code>each_byte</code> und <code>pack('C*')</code> erklärt die Seite, soweit sie gebraucht werden.</td></tr>
<tr><th>Sprache</th><td>Welche Sprache eine Klasse sieht, stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein, ohne Einstellung ist es Ruby. Lernende können auf der Seite selbst umschalten; eine Aufgabe, die sie in einer Sprache gelöst haben, gilt auch in der anderen.</td></tr>
<tr><th>Einordnung</th><td>Das <a href='/gif'>GIF-Labor</a> baut darauf auf: Dort steht dasselbe Verfahren Bit für Bit in einer Datei, mit Farben statt Bytes.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Wiederholungen, die Regeln des Kompressors, <code>ABABABA</code> von Hand, dann <code>compress</code>.</li>
<li><b>2. Doppelstunde:</b> der Sonderfall als Vorhersage, <code>decompress</code>, Messen an der Spezifikation von GIF und dem Foto, die Reflexion.</li>
<li><b>Für Schnelle:</b> das volle Wörterbuch mit Clear Code.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/lzw</code>: <code>lzw.rb</code>, dazu <code>spec-gif89a.txt</code> und <code>44-baseline.jpg</code>, die die Lernenden mit <code>wget</code> laden. Auf der Seite steht nur das Gerüst mit leeren Methoden; die Regeln und Hinweise zu Ruby stehen im Text.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>»Der Kompressor« und »Der Entpacker« laden <code>lzw/lzw.rb</code> im Workspace der Lernenden, als Gast, der nichts verändern kann, und rufen die Methoden mit festen Eingaben auf (8 und 9 Aufrufe, höchstens zehn Sekunden). Der Workspace muss laufen.</li>
<li>Die Rückmeldung nennt den ersten Aufruf, der nicht passt, mit dem, was herauskam.</li>
<li>Geladen werden nur die Definitionen (Methoden, Klassen, Konstanten, <code>require</code>); das Programm unten in der Datei läuft dabei nicht, ein <code>if __FILE__ == $0</code> braucht es nicht.</li>
</ul></td></tr>
<tr><th>Lösung</th><td>Im Repository unter <a href='https://github.com/specht/workspace/blob/main/src/content/lzw/loesung/lzw.rb'><code>src/content/lzw/loesung/lzw.rb</code></a> und <a href='https://github.com/specht/workspace/blob/main/src/content/lzw/loesung/lzw.py'><code>lzw.py</code></a>, mit <code>size_in_bytes</code>; <code>ruby lzw.rb spec-gif89a.txt</code> gibt die Zahlen der Aufgaben aus. Die Lösungen der beiden Programmieraufgaben sehen Lernende nach einer richtigen Antwort auch auf der Karte.</td></tr>
</table>

#### Die Aufgaben

- »Von Hand« (`65 66 256 258`) erkennt zwei typische Antworten: die einzelnen Bytes und ein zweites `256` am Ende, wenn `ABA` als Eintrag übersehen wurde.
- »Ein Code, den es noch nicht gibt« ist eine Vorhersage vor der Erklärung des Sonderfalls; nur der erste Versuch zählt für die Klassenübersicht.
- Die Prüfung von `decompress` enthält `AAAAAAA` (der Sonderfall zweimal hintereinander) und Codes eines fremden Kompressors, damit ein Entpacker nicht nur mit dem eigenen, vielleicht falschen `compress` zusammenpasst.
- »Die Spezifikation von GIF« (28.813 Bytes, ± 2) erkennt die Zahl der Codes (17.415) und 12 Bits pro Code.
- Das Foto wird mit LZW um knapp ein Drittel größer, etwa 185.000 statt 143.206 Bytes; `gzip` spart dort 92 Bytes.

#### Was schiefgeht

- **`compress('')` ergibt `[nil]`.** Am Ende wird der Code des leeren Präfixes ausgegeben. Die Rückmeldung sagt genau das.
- **Der Sonderfall fehlt.** `table[258]` ist dann `nil`, und `decompress` bricht bei `ABABABA` mit `NoMethodError` ab (»undefined method '[]' for nil«). Die Rückmeldung nennt die Zeile.
- **Ein Vergleich mit `p`.** `decompress` gibt einen String mit den Bytes zurück; Ruby zeigt ihn mit `p` als `"ABABABA"` an, Umlaute aber als `"Gr\xC3\xBC\xC3\x9Fe"`. Das ist richtig: Erst `force_encoding('UTF-8')` macht wieder Text daraus.

#### Tipps in Stufen

»Der Kompressor« und »Der Entpacker« haben zwei oder drei Tipps: ein Beispiel von Hand, den Weg in Worten, meist ein Gerüst mit Lücken, nie die ganze Lösung. Die nächste Stufe erscheint eine Minute nach der vorigen, ohne Uhr, und wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst. Hängen viele an derselben Stelle, ist das ein Thema für die ganze Gruppe.

#### Weiter

Das volle Wörterbuch (Clear Code) ist die Brücke zu GIF, wo genau das in der Datei steht. Die Geschichte des LZW-Patents von Unisys und die Entstehung von PNG eignen sich für ein kurzes Referat.
