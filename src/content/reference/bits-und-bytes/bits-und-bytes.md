<div class='meta'>
kind: reference
search: Bit, Byte, binär, Binärzahl, hexadezimal, Hex, 0x, 0b, Hexdump, hd, xxd, ASCII, ASCII-Tabelle, Codepage, Unicode, Codepoint, UTF-8, UTF-16, Emoji, Umlaut, Little Endian, Big Endian, Zweierkomplement, negativ, Überlauf, AND, OR, XOR, Shift, Maske, KiB, MiB
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Bits und Bytes

<p class='abstract'>
Was in jeder Datei und jedem Speicher steckt, zum Nachschlagen: Zahlen binär und hexadezimal, wie viel in ein Byte passt, wie man einen Hexdump liest, Text als Bytes, Zahlen aus mehreren Bytes, negative Zahlen und die Operationen auf einzelnen Bits, in C, Python, Ruby und JavaScript. Gebraucht wird das in <a href='/working-with-files'>Dateien und Verzeichnisse</a>, <a href='/gif'>GIF</a>, <a href='/jpeg'>JPEG</a>, <a href='/steganography'>Geheime Botschaften in Bildern</a>, <a href='/totp'>Einmalcodes</a> und <a href='/sandbox-cpu'>Vom Byte zum 3D-Labyrinth</a>.
</p>

## Binär, dezimal, hexadezimal

<!-- part: zahlensysteme -->
<!-- short: 0b101010 = 42 = 0x2a; eine Hex-Ziffer steht für genau vier Bits, zwei Hex-Ziffern für ein Byte. -->
Eine Zahl ist dieselbe, egal wie man sie aufschreibt. **Binär** hat jede Stelle den doppelten Wert der Stelle rechts von ihr (1, 2, 4, 8, 16 …), **hexadezimal** den 16-fachen, mit den Ziffern `0`–`9` und `a`–`f` für 10 bis 15:

$$101010\_2 = 32 + 8 + 2 = 42$$

$$2a\_{16} = 2 \cdot 16 + 10 = 42$$

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>dezimal</th><th>binär</th><th>hex</th></tr></thead>
<tbody>
<tr><td>0</td><td><code>0000</code></td><td><code>0</code></td></tr>
<tr><td>1</td><td><code>0001</code></td><td><code>1</code></td></tr>
<tr><td>2</td><td><code>0010</code></td><td><code>2</code></td></tr>
<tr><td>3</td><td><code>0011</code></td><td><code>3</code></td></tr>
<tr><td>4</td><td><code>0100</code></td><td><code>4</code></td></tr>
<tr><td>5</td><td><code>0101</code></td><td><code>5</code></td></tr>
<tr><td>6</td><td><code>0110</code></td><td><code>6</code></td></tr>
<tr><td>7</td><td><code>0111</code></td><td><code>7</code></td></tr>
<tr><td>8</td><td><code>1000</code></td><td><code>8</code></td></tr>
<tr><td>9</td><td><code>1001</code></td><td><code>9</code></td></tr>
<tr><td>10</td><td><code>1010</code></td><td><code>a</code></td></tr>
<tr><td>11</td><td><code>1011</code></td><td><code>b</code></td></tr>
<tr><td>12</td><td><code>1100</code></td><td><code>c</code></td></tr>
<tr><td>13</td><td><code>1101</code></td><td><code>d</code></td></tr>
<tr><td>14</td><td><code>1110</code></td><td><code>e</code></td></tr>
<tr><td>15</td><td><code>1111</code></td><td><code>f</code></td></tr>
</tbody>
</table>
</div>

Eine Hex-Ziffer steht für genau vier Bits. Deshalb geht das Umrechnen zwischen binär und hexadezimal ohne Rechnen, Ziffer für Ziffer: `0010 1010` ist `2a`, `1111 0000` ist `f0`. Ein Byte sind immer zwei Hex-Ziffern, von `00` bis `ff`.

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th></th><th>C</th><th>Python</th><th>Ruby</th><th>JavaScript</th></tr></thead>
<tbody>
<tr><th>im Code</th><td><code>0x2a</code>; binär erst ab C23</td><td><code>0x2a</code>, <code>0b101010</code>, mit Unterstrich <code>0b0010_1010</code></td><td><code>0x2a</code>, <code>0b101010</code>, <code>0b0010_1010</code></td><td><code>0x2a</code>, <code>0b101010</code>, <code>0b0010_1010</code></td></tr>
<tr><th>als Text binär</th><td>&ndash;</td><td><code>format(42, '08b')</code> → <code>'00101010'</code></td><td><code>42.to_s(2)</code>, <code>'%08b' % 42</code></td><td><code>(42).toString(2)</code></td></tr>
<tr><th>als Text hex</th><td><code>printf("%02x", 42)</code></td><td><code>format(42, '02x')</code>, <code>hex(42)</code> → <code>'0x2a'</code></td><td><code>42.to_s(16)</code>, <code>'%02x' % 42</code></td><td><code>(42).toString(16)</code></td></tr>
<tr><th>aus Text</th><td><code>strtol("2a", NULL, 16)</code></td><td><code>int('2a', 16)</code>, <code>int('101010', 2)</code></td><td><code>'2a'.to_i(16)</code>, <code>'101010'.to_i(2)</code></td><td><code>parseInt('2a', 16)</code></td></tr>
</tbody>
</table>
</div>

Mit `08b` oder `rjust(8, '0')` füllst du vorn mit Nullen auf, damit ein Byte immer acht Stellen hat.
<!-- /part -->

## Wie viel passt hinein?

<!-- part: groessen -->
<!-- short: n Bits haben 2^n Werte: ein Byte 0 bis 255, 16 Bit bis 65 535; KiB = 1024 Bytes, kB = 1000. -->
Ein **Bit** ist 0 oder 1, ein **Byte** sind 8 Bits. Mit \\(n\\) Bits gibt es \\(2^n\\) verschiedene Werte:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Bits</th><th>Werte</th><th>ohne Vorzeichen</th><th>mit Vorzeichen</th></tr></thead>
<tbody>
<tr><td>4</td><td>16</td><td>0 bis 15</td><td>−8 bis 7</td></tr>
<tr><td>8 (ein Byte)</td><td>256</td><td>0 bis 255</td><td>−128 bis 127</td></tr>
<tr><td>16</td><td>65 536</td><td>0 bis 65 535</td><td>−32 768 bis 32 767</td></tr>
<tr><td>32</td><td>etwa 4,3 Milliarden</td><td>0 bis 4 294 967 295</td><td>etwa ±2,1 Milliarden</td></tr>
</tbody>
</table>
</div>

_person(shannon)

Wie viele Bits braucht man für eine Zahl \\(x\\)? So viele, dass \\(2^n > x\\) ist: für 100 sieben, denn \\(2^7 = 128\\).

Bei Dateigrößen ist **kB** eigentlich 1000 Bytes und **KiB** 1024 (\\(2^{10}\\)), ebenso MB und MiB (\\(2^{20}\\) = 1 048 576). `ls -lh` und `du -h` rechnen mit 1024, schreiben aber nur `K` und `M`.
<!-- /part -->

## Einen Hexdump lesen

<!-- part: hexdump -->
<!-- short: hd datei zeigt links den Offset, in der Mitte 16 Bytes hexadezimal, rechts dieselben als Zeichen; xxd ebenso, in Paaren. -->
`hd` (»hexdump«) und `xxd` zeigen die Bytes einer Datei. Eine Datei mit dem Text `Grüße, Welt!` und einem Zeilenende:

```bash
hd gruss.txt
```

```text
00000000  47 72 c3 bc c3 9f 65 2c  20 57 65 6c 74 21 0a     |Gr....e, Welt!.|
0000000f
```

- Vorne steht der **Offset**: die Position des ersten Bytes dieser Zeile, hexadezimal, ab 0. Die letzte Zeile nennt die Länge, hier `0f` = 15 Bytes.
- In der Mitte stehen bis zu 16 Bytes, mit einer Lücke nach dem achten. Das Byte bei Offset `0x0a` ist also das dritte nach der Lücke, hier `65`.
- Rechts stehen dieselben Bytes als Zeichen. Wo ein Byte kein druckbares ASCII-Zeichen ist, steht ein Punkt; `ü` und `ß` sind je zwei solche Bytes.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Was er zeigt</th></tr></thead>
<tbody>
<tr><td><code>hd datei | less</code></td><td>die ganze Datei, seitenweise</td></tr>
<tr><td><code>hd -n 64 datei</code></td><td>nur die ersten 64 Bytes</td></tr>
<tr><td><code>hd -s 0x0a -n 16 datei</code></td><td>16 Bytes ab Offset 10</td></tr>
<tr><td><code>xxd datei | head</code></td><td>dasselbe in der Form von <code>xxd</code>: <code>00000000: 4772 c3bc …</code>, je zwei Bytes zusammen, die Zeichen ohne Striche</td></tr>
<tr><td><code>xxd -b -l 4 datei</code></td><td>die ersten 4 Bytes binär</td></tr>
</tbody>
</table>
</div>

Ein Hexdump interpretiert nichts: Ob zwei Bytes eine Zahl, zwei Zeichen oder ein Stück Bild sind, weiß nur, wer das Format der Datei kennt.
<!-- /part -->

## Text als Bytes

<!-- part: text-bytes -->
<!-- short: ASCII: 'A' = 0x41, 'a' = 0x61, '0' = 0x30, Leerzeichen 0x20, Zeilenende 0x0a; UTF-8 braucht für Umlaute zwei Bytes. -->
In **ASCII** hat jedes Zeichen eine Nummer von 0 bis 127, ein Byte:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Zeichen</th><th>hex</th><th>dezimal</th><th></th></tr></thead>
<tbody>
<tr><td>Zeilenende (LF, <code>\n</code>)</td><td><code>0a</code></td><td>10</td><td>Tab (<code>\t</code>) ist <code>09</code></td></tr>
<tr><td>Leerzeichen</td><td><code>20</code></td><td>32</td><td></td></tr>
<tr><td><code>0</code> bis <code>9</code></td><td><code>30</code> bis <code>39</code></td><td>48 bis 57</td><td>die Ziffer ist das Byte minus 48</td></tr>
<tr><td><code>A</code> bis <code>Z</code></td><td><code>41</code> bis <code>5a</code></td><td>65 bis 90</td><td></td></tr>
<tr><td><code>a</code> bis <code>z</code></td><td><code>61</code> bis <code>7a</code></td><td>97 bis 122</td><td>32 (<code>0x20</code>) mehr als der Großbuchstabe</td></tr>
</tbody>
</table>
</div>

Alles andere, Umlaute, `€`, Emojis, schreibt **UTF-8** mit zwei bis vier Bytes, die alle größer als 127 sind: `ä` ist `c3 a4`, `ß` ist `c3 9f`, `€` ist `e2 82 ac`. Ein Text aus reinem ASCII ist deshalb auch gültiges UTF-8. Zeichen und Bytes zu zählen, gibt verschiedene Ergebnisse:

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th></th><th>Zeichen</th><th>Bytes</th></tr></thead>
<tbody>
<tr><th>Python</th><td><code>len('Grüße')</code> → 5, <code>ord('A')</code> → 65</td><td><code>'Grüße'.encode()</code> → <code>b'Gr\xc3\xbc\xc3\x9fe'</code>, 7 Bytes; zurück mit <code>.decode()</code></td></tr>
<tr><th>Ruby</th><td><code>'Grüße'.length</code> → 5, <code>'A'.ord</code> → 65</td><td><code>'Grüße'.bytes</code> → <code>[71, 114, 195, 188, 195, 159, 101]</code>, <code>bytesize</code> → 7</td></tr>
<tr><th>Terminal</th><td><code>wc -m datei</code></td><td><code>wc -c datei</code></td></tr>
</tbody>
</table>
</div>

Wer Bytes als Text liest, muss wissen, wie sie kodiert sind. Steht statt `Grüße` plötzlich `GrÃ¼ÃŸe` da, wurden UTF-8-Bytes einzeln als Zeichen gelesen.

_person(thompson)
<!-- /part -->

## Die ASCII-Tabelle

ASCII legt 128 Zeichen fest, die Nummern 0 bis 127. Die Tabelle hier stammt aus einem alten Handbuch für den IBM-PC: links die Nummern 0 bis 127, rechts 128 bis 255, jede dezimal, hexadezimal und mit ihrem Zeichen.

<img class='full invert-dark' src='../../working-with-files/ascii.webp' alt='Eine gedruckte Zeichentabelle in acht Spalten, jede mit Dezimalzahl, Hexzahl und Zeichen. Links 0 bis 127: oben Bildchen wie Smileys, Herzen und Pfeile, dann Satzzeichen, Ziffern und Buchstaben. Rechts 128 bis 255: Buchstaben mit Akzenten und Umlauten, Striche für Rahmen, griechische Buchstaben und mathematische Zeichen.'>

An der Tabelle fällt zweierlei auf. Die Nummern 0 bis 31 sind eigentlich **Steuerzeichen**: 10 ist das Zeilenende, 9 der Tabulator, 13 der Wagenrücklauf. Der IBM-PC zeichnete für sie trotzdem Bildchen, Smileys und Kartensymbole. Und die rechte Hälfte gehört nicht mehr zu ASCII: Sie ist die **Codepage 437** des IBM-PC, mit Umlauten, Akzenten und Strichen für Rahmen. Dort ist `ä` die 132 (`0x84`). In Latin-1, der Codepage, die später unter Windows und im Web üblich war, ist `ä` dagegen die 228 (`0xe4`), und die 132 ist dort gar kein Buchstabe. Welches Zeichen ein Byte über 127 war, hing also davon ab, welche Codepage ein Rechner benutzte, und ein Text kam auf einem anderen Rechner oft mit falschen Zeichen an.

## Unicode

**Unicode** löst das mit einer einzigen Liste für alle Schriften der Welt: Jedes Zeichen hat darin eine eigene Nummer, den **Codepoint**, geschrieben als `U+` und eine Hexzahl. `A` ist U+0041, `ä` ist U+00E4, `€` ist U+20AC, `😀` ist U+1F600. Mehr als 150.000 Zeichen sind vergeben, Platz ist für über eine Million, bis U+10FFFF. Die ersten 128 Codepoints sind genau ASCII, die ersten 256 genau Latin-1.

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th></th><th>Codepoint eines Zeichens</th><th>Zeichen zu einem Codepoint</th></tr></thead>
<tbody>
<tr><th>Python</th><td><code>ord('ä')</code> → 228, <code>hex(ord('€'))</code> → <code>'0x20ac'</code></td><td><code>chr(0x20ac)</code> → <code>'€'</code>, im String <code>'\u20ac'</code></td></tr>
<tr><th>Ruby</th><td><code>'ä'.ord</code> → 228, <code>'€'.ord.to_s(16)</code> → <code>"20ac"</code></td><td><code>0x20ac.chr('UTF-8')</code> → <code>"€"</code>, im String <code>"\u20ac"</code></td></tr>
</tbody>
</table>
</div>

Unicode sagt nur, welche Nummer ein Zeichen hat. Wie diese Nummer als Bytes in einer Datei steht, legt eine **Kodierung** fest, und die wichtigste ist UTF-8.

## UTF-8

UTF-8 schreibt einen Codepoint mit einem bis vier Bytes. An den ersten Bits eines Bytes sieht man, wie viele Bytes zu seinem Zeichen gehören:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Codepoints</th><th>Bytes</th><th>Bits</th></tr></thead>
<tbody>
<tr><td>U+0000 bis U+007F</td><td>1</td><td><code>0xxxxxxx</code></td></tr>
<tr><td>U+0080 bis U+07FF</td><td>2</td><td><code>110xxxxx 10xxxxxx</code></td></tr>
<tr><td>U+0800 bis U+FFFF</td><td>3</td><td><code>1110xxxx 10xxxxxx 10xxxxxx</code></td></tr>
<tr><td>U+10000 bis U+10FFFF</td><td>4</td><td><code>11110xxx 10xxxxxx 10xxxxxx 10xxxxxx</code></td></tr>
</tbody>
</table>
</div>

In die `x` kommen die Bits des Codepoints, von links nach rechts. `ä` ist U+00E4, mit elf Bits `00011100100`. Fünf davon passen ins erste Byte, sechs ins zweite: `110 00011` und `10 100100`, also `c3 a4`. Im Terminal siehst du die Bytes eines Zeichens mit `printf '€' | xxd`: `e2 82 ac`.

Daraus folgt, was UTF-8 so praktisch macht:

- Ein Byte unter 128 ist immer ein ASCII-Zeichen. Ein Programm, das nur ASCII kennt, liest englischen Text in UTF-8 richtig.
- Ein Byte, das mit `10` beginnt, ist immer die Fortsetzung eines Zeichens. Wer mitten in eine Datei springt, findet so den Anfang des nächsten Zeichens.
- Ein Zeichen hat mal ein Byte, mal vier: Zeichen zählen und Bytes zählen gibt verschiedene Zahlen, wie oben bei `Grüße`.

Nicht überall steht Text als UTF-8 im Speicher. JavaScript, Java und Windows benutzen innen **UTF-16**: zwei Bytes für jedes Zeichen bis U+FFFF und zwei mal zwei Bytes für alles darüber, etwa für Emojis. Darum ist in JavaScript `'😀'.length` 2 und erst `[...'😀'].length` 1. In Python ist `len('😀')` 1, in Ruby `'😀'.length` auch.

## Zahlen aus mehreren Bytes

<!-- part: endian -->
<!-- short: Little Endian (GIF, x86): das niedrige Byte zuerst; Big Endian (JPEG, PNG, Netzwerk): das hohe zuerst. -->
Eine Zahl, die nicht in ein Byte passt, steht in mehreren, und jedes Format legt fest, in welcher Reihenfolge. `0x1234` = 4660 in zwei Bytes:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Reihenfolge</th><th>im Hexdump</th><th>wo</th></tr></thead>
<tbody>
<tr><td><strong>Little Endian</strong>: das niedrige Byte zuerst</td><td><code>34 12</code></td><td>GIF, BMP, WAV, der Speicher von PCs (x86, ARM)</td></tr>
<tr><td><strong>Big Endian</strong>: das hohe Byte zuerst</td><td><code>12 34</code></td><td>JPEG, PNG, Zahlen in Netzwerkprotokollen</td></tr>
</tbody>
</table>
</div>

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th></th><th>zwei Bytes lesen</th><th>eine Zahl als Bytes</th></tr></thead>
<tbody>
<tr><th>C</th><td><code>low | (high &lt;&lt; 8)</code>, <code>(high &lt;&lt; 8) | low</code></td><td><code>x &amp; 0xff</code>, <code>(x &gt;&gt; 8) &amp; 0xff</code></td></tr>
<tr><th>Python</th><td><code>int.from_bytes(data[i:i + 2], 'little')</code>, <code>'big'</code></td><td><code>x.to_bytes(2, 'little')</code>, <code>'big'</code></td></tr>
<tr><th>Ruby</th><td><code>data.byteslice(i, 2).unpack1('v')</code>, <code>'n'</code> für Big Endian</td><td><code>[x].pack('v')</code>, <code>[x].pack('n')</code></td></tr>
</tbody>
</table>
</div>

Bei Ruby lesen `v` und `V` (nach dem alten Rechner VAX) Little Endian, `n` und `N` (wie <em>network</em>) Big Endian; die kleinen Buchstaben 16 Bit, die großen 32 Bit. Liest du eine Zahl in der falschen Reihenfolge, wird sie fast immer viel zu groß: `34 12` als Big Endian ist 13 330 statt 4660.
<!-- /part -->

## Negative Zahlen

<!-- part: zweierkomplement -->
<!-- short: Im Zweierkomplement ist das oberste Bit das Vorzeichen: 0xff ist −1, 0x80 ist −128; negieren heißt alle Bits umdrehen und 1 addieren. -->
Im **Zweierkomplement** zählt das oberste Bit negativ: Bei einem Byte ist es −128 statt +128. Dieselben acht Bits bedeuten so je nachdem, ob sie mit oder ohne Vorzeichen gelesen werden, etwas anderes:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Bits</th><th>hex</th><th>ohne Vorzeichen</th><th>mit Vorzeichen</th></tr></thead>
<tbody>
<tr><td><code>0111 1111</code></td><td><code>7f</code></td><td>127</td><td>127</td></tr>
<tr><td><code>1000 0000</code></td><td><code>80</code></td><td>128</td><td>−128</td></tr>
<tr><td><code>1111 1011</code></td><td><code>fb</code></td><td>251</td><td>−5</td></tr>
<tr><td><code>1111 1111</code></td><td><code>ff</code></td><td>255</td><td>−1</td></tr>
</tbody>
</table>
</div>

Eine Zahl negieren: alle Bits umdrehen und 1 addieren. Aus 5 = `0000 0101` wird `1111 1010`, plus 1 `1111 1011`. Dass dabei −5 herauskommt, sieht man an 251 = 256 − 5: Mit 8 Bits ist −x dasselbe Muster wie 256 − x. Das ist der Grund für dieses Verfahren: Addieren und Subtrahieren funktionieren für beide Lesarten mit denselben Schaltungen.

Wird ein Ergebnis zu groß, läuft es **über**: Was nicht mehr in die Bits passt, fällt weg. Ein Byte ohne Vorzeichen geht von 255 plus 1 auf 0, eins mit Vorzeichen von 127 plus 1 auf −128.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th></th><th>Was gilt</th></tr></thead>
<tbody>
<tr><td>C</td><td>feste Größen: <code>uint8_t</code> von 0 bis 255 läuft über (255 + 1 ergibt 0), <code>(int8_t) 200</code> ist −56; bei <code>int</code> ist ein Überlauf nicht erlaubt, das Programm darf dann alles tun</td></tr>
<tr><td>Python, Ruby</td><td>ganze Zahlen werden einfach größer und laufen nie über; ein Byte daraus macht <code>x &amp; 0xff</code> (−5 wird 251), zurück <code>int.from_bytes(b, 'big', signed=True)</code> oder in Ruby <code>[b].pack('C').unpack1('c')</code></td></tr>
<tr><td>JavaScript</td><td>die Bit-Operationen rechnen mit 32 Bit und Vorzeichen: <code>1 &lt;&lt; 31</code> ist −2 147 483 648; <code>x &gt;&gt;&gt; 0</code> liest dieselben Bits ohne Vorzeichen</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

## Mit Bits rechnen

<!-- part: bitoperationen -->
<!-- short: & UND, | ODER, ^ XOR, ~ NICHT, << und >> schieben; Bit k lesen mit (x >> k) & 1, setzen mit x | (1 << k). -->
Die Operationen arbeiten auf jedem Bit für sich und heißen in C, Python, Ruby und JavaScript gleich. Mit `a = 1100` und `b = 1010`:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Operation</th><th>Ergebnis</th><th>ein Bit ist 1, wenn …</th></tr></thead>
<tbody>
<tr><td><code>a &amp; b</code> (UND)</td><td><code>1000</code></td><td>es in beiden 1 ist</td></tr>
<tr><td><code>a | b</code> (ODER)</td><td><code>1110</code></td><td>es in mindestens einem 1 ist</td></tr>
<tr><td><code>a ^ b</code> (XOR, exklusives ODER)</td><td><code>0110</code></td><td>es in genau einem 1 ist; zweimal dasselbe <code>^ b</code> ergibt wieder <code>a</code></td></tr>
<tr><td><code>~a</code> (NICHT)</td><td><code>…0011</code></td><td>es in <code>a</code> 0 ist; alle Bits davor werden auch 1, siehe unten</td></tr>
<tr><td><code>a &lt;&lt; 1</code></td><td><code>11000</code></td><td>schiebt nach links, hinten kommen Nullen dazu: mal 2</td></tr>
<tr><td><code>a &gt;&gt; 2</code></td><td><code>0011</code></td><td>schiebt nach rechts, die untersten Bits fallen weg: durch 4, ohne Rest</td></tr>
</tbody>
</table>
</div>

Die Bits zählt man von rechts, beim untersten mit 0: Bit \\(k\\) hat den Wert \\(2^k\\), und `1 << k` ist eine Zahl, in der nur dieses Bit gesetzt ist. Eine solche Zahl heißt **Maske**:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Was</th><th>Wie</th></tr></thead>
<tbody>
<tr><td>Bit <code>k</code> lesen (0 oder 1)</td><td><code>(x &gt;&gt; k) &amp; 1</code></td></tr>
<tr><td>Bit <code>k</code> setzen, löschen, umdrehen</td><td><code>x | (1 &lt;&lt; k)</code>, <code>x &amp; ~(1 &lt;&lt; k)</code>, <code>x ^ (1 &lt;&lt; k)</code></td></tr>
<tr><td>die untersten <code>n</code> Bits</td><td><code>x &amp; ((1 &lt;&lt; n) - 1)</code>, für vier Bits <code>x &amp; 0x0f</code></td></tr>
<tr><td>die obere Hälfte eines Bytes</td><td><code>x &gt;&gt; 4</code></td></tr>
<tr><td>Rot, Grün, Blau aus <code>0xRRGGBB</code></td><td><code>(c &gt;&gt; 16) &amp; 0xff</code>, <code>(c &gt;&gt; 8) &amp; 0xff</code>, <code>c &amp; 0xff</code></td></tr>
<tr><td>und wieder zusammen</td><td><code>(r &lt;&lt; 16) | (g &lt;&lt; 8) | b</code></td></tr>
</tbody>
</table>
</div>

Zwei Stellen, an denen die Sprachen sich unterscheiden:

- `~` dreht in Python und Ruby unendlich viele Bits um, das Ergebnis ist negativ: `~5` ist −6. Für ein Byte schreib `~x & 0xff` (250).
- In C und JavaScript bindet `&` schwächer als `==`: `x & 1 == 0` heißt dort `x & (1 == 0)` und ist immer 0. In Python und Ruby ist es `(x & 1) == 0`. Schieben bindet überall schwächer als `+`: `1 << n + 1` ist `1 << (n + 1)`. Klammern schaden nie.
<!-- /part -->

## Zum Nachschlagen

- [Python: Bitwise Operations on Integer Types](https://docs.python.org/3/library/stdtypes.html#bitwise-operations-on-integer-types): die Operationen, dazu `int.to_bytes` und `int.from_bytes` weiter unten auf derselben Seite, auf Englisch.
- [Ruby: Packed Data](https://docs.ruby-lang.org/en/master/packed_data_rdoc.html): alle Buchstaben für `pack` und `unpack`, auf Englisch.
- [Zweierkomplement](https://de.wikipedia.org/wiki/Zweierkomplement) und [UTF-8](https://de.wikipedia.org/wiki/UTF-8) in der Wikipedia, mit der Tabelle, wie UTF-8 ein Zeichen auf ein bis vier Bytes verteilt; dazu [Unicode](https://de.wikipedia.org/wiki/Unicode) und die [Codepage 437](https://de.wikipedia.org/wiki/Codepage_437) des IBM-PC.
- [Unicode-Zeichentabellen](https://www.unicode.org/charts/): jeder Codepoint, nach Schriften geordnet, auf Englisch.
