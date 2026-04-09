<div class='meta'>
image: title.webp
</div>

<style>

    :root {
        --color-m0: #ffe617;
        --color-m1: #f4951b;
        --color-m2: #bc2326;
        --color-m3: #f384ae;
        --color-m4: #7b67ae;
        --color-m5: #238acc;
        --color-m6: #12959f;
        --color-m7: #73a946;
        --color-m8: #bf9b6b;
    }
    pre.spec {
        background: none;
        color: #333;
        line-height: 135%;
    }

    pre.hexdump {
        /* background: #f5f5f5; */
        line-height: 135%;

        .m0, .m1, .m2, .m3, .m4, .m5, .m6, .m7, .m8 {
            position: relative;
        }

        .m0::after, .m1::after, .m2::after,
        .m3::after, .m4::after, .m5::after,
        .m6::after, .m7::after, .m8::after {
            content: "";
            position: absolute;
            left: -0.2em;
            right: -0.2em;
            bottom: 0.05em;
            height: 1.2em;
            z-index: -1;
            border-radius: 0.25em;
            opacity: 0.75;
        }

        .m0::after { background-color: var(--color-m0); }
        .m1::after { background-color: var(--color-m1); }
        .m2::after { background-color: var(--color-m2); }
        .m3::after { background-color: var(--color-m3); }
        .m4::after { background-color: var(--color-m4); }
        .m5::after { background-color: var(--color-m5); }
        .m6::after { background-color: var(--color-m6); }
        .m7::after { background-color: var(--color-m7); }
        .m8::after { background-color: var(--color-m8); }
    }

    p, td {
        .m0, .m1, .m2, .m3, .m4, .m5, .m6, .m7, .m8 {
            padding: 0 0.25em;
            border-radius: 0.25em;
        }
        .m0 { background-color: color-mix(in srgb, var(--color-m0) 75%, transparent); }
        .m1 { background-color: color-mix(in srgb, var(--color-m1) 75%, transparent); }
        .m2 { background-color: color-mix(in srgb, var(--color-m2) 75%, transparent); }
        .m3 { background-color: color-mix(in srgb, var(--color-m3) 75%, transparent); }
        .m4 { background-color: color-mix(in srgb, var(--color-m4) 75%, transparent); }
        .m5 { background-color: color-mix(in srgb, var(--color-m5) 75%, transparent); }
        .m6 { background-color: color-mix(in srgb, var(--color-m6) 75%, transparent); }
        .m7 { background-color: color-mix(in srgb, var(--color-m7) 75%, transparent); }
        .m8 { background-color: color-mix(in srgb, var(--color-m8) 75%, transparent); }
    }

    td {
        .m0, .m1, .m2, .m3, .m4, .m5, .m6, .m7, .m8 {
            padding: 0.25em 0.5em;
            code {
                background: transparent;
                color: inherit;
            }
        }
    }

    /* pre.hexdump .hl,
    pre.hexdump .hl1,
    pre.hexdump .hl2,
    pre.hexdump .hl3,
    pre.hexdump .hl4,
    pre.hexdump .hl5,
    pre.hexdump .hl6,
    pre.hexdump .hl7,
    pre.hexdump .hl8 {
        box-shadow: none;
        border: none;
        outline: none;
        padding: 0 0.2em;
        margin: 0 -0.2em;
        border-radius: 0.25em;
        background: linear-gradient(
            transparent 15%,
            #ffd54f 15%,
            #ffd54f 85%,
            transparent 85%
        );
    } */

    pre.hexdump .hl1 {
        /* background-color: #fdb717; */
    }
    pre.hexdump .hl2 {
        /* background-color: #80bc42; */
    }
    pre.hexdump .hl3 {
        /* background-color: #00a8a8; */
    }
    pre.hexdump .hl4 {
        /* background-color: #238acc; */
    }
    pre.hexdump .hl5 {
        /* background-color: #b296c7; */
    }
    pre.hexdump .hl6 {
        /* background-color: #fac6d2; */
    }
    pre.hexdump .hl7 {
        /* background-color: #fdb462; */
    }
    pre.hexdump .hl8 {
        /* background-color: #7fcdbb; */
    }
</style>

# JPEG

<p class='abstract'>
Das JPEG-Format (Joint Photographic Experts Group) ist ein weit verbreitetes Bildformat, das hauptsächlich für die Komprimierung von Fotografien und realistischen Bildern verwendet wird. Es wurde in den frühen 1990er Jahren entwickelt und bietet eine effiziente Methode zur Reduzierung der Dateigröße von Bildern, während gleichzeitig eine akzeptable Bildqualität beibehalten wird. JPEG verwendet eine verlustbehaftete Komprimierungstechnik, bei der bestimmte Bildinformationen entfernt werden, um die Dateigröße zu verringern. Dies führt zu einer gewissen Qualitätsminderung, die jedoch oft nicht sichtbar ist, insbesondere bei höheren Qualitätsstufen.
</p>

## Grundlagen

Dieser Artikel soll dir helfen, einen JPEG-Decoder zu implementieren. Dafür ist es wichtig, zunächst einige Grundlagen zu verstehen:

### Basiswechsel

### Farbräume

### Trennung von Helligkeit und Farbe

### Diskrete Kosinustransformation

### Quantisierung

### Huffman-Codierung

## Grober Ablauf eines JPEG-Decoders

Um eine JPEG-Datei zu decodieren, musst du die folgenden Schritte durchführen:

1. **Marker erkennen**: Suche nach den JPEG-Markern, um die Struktur der Datei zu verstehen.
2. **Segmente interpretieren**: Je nach Marker musst du die entsprechenden Daten interpretieren. Zum Beispiel musst du bei einem DQT-Marker die Quantisierungstabelle lesen, bei einem SOF<sub>0</sub>-Marker die Bildgröße und die Anzahl der Komponenten, und bei einem DHT-Marker einen Huffman-Baum aufbauen.
3. **Huffman-Codes lesen**: Ab dem SOS-Marker (Start of Scan) musst du die komprimierten Bilddaten lesen und die Huffman-Codes dekodieren, um die quantisierten DCT-Koeffizienten zu erhalten.
4. **Dequantisierung**: Verwende die passende Quantisierungstabelle, um die quantisierten DCT-Koeffizienten in ihre ursprünglichen Werte zurückzuverwandeln.
5. **Inverse DCT**: Wende die inverse diskrete Kosinustransformation an, um die Pixelwerte aus den DCT-Koeffizienten zu berechnen.
6. **Farbraumkonvertierung**: Nutze eine Farbraumtransformation, um die Bilddaten von YCbCr zurück in RGB zu konvertieren.
7. **Bild rekonstruieren**: Setze die Pixelwerte zusammen, um das endgültige Bild zu erstellen. Dabei müssen die Chroma-Kanäle entsprechend der Subsampling-Methode interpoliert werden, falls diese verwendet wurde.
8. **Bild anzeigen**: Zeige das rekonstruierte Bild mit dem Pixelflow Canvas an.

## Dokumentation

Der vollständige [JPEG-Standard](https://www.w3.org/Graphics/JPEG/itu-t81.pdf) ist sehr umfangreich und komplex. Wir fassen deshalb hier alle relevanten Informationen zusammen, die du für die Implementierung eines Baseline-JPEG-Decoders benötigst.

### Marker und Segmente

Marker sind spezielle 16-Bit-Sequenzen, die bestimmte Abschnitte eines JPEG-Bildes kennzeichnen. Sie beginnen immer mit 0xFF, gefolgt von einem weiteren Byte, das den Typ des Markers angibt. Einige Marker stehen für sich allein und beinhalten keine Daten, während andere Marker von einer 16-Bit-Länge gefolgt werden, die die Anzahl der nachfolgenden Datenbytes angibt (inkl. der 2 Bytes für die Länge selbst). Hier ist eine Übersicht über die wichtigsten Marker (dabei werden Marker ohne Daten mit einem Sternchen (*) gekennzeichnet):

<table class='table'>
    <thead>
        <tr>
        <th>Marker</th>
        <th colspan='2'>Name</th>
        <th>Beschreibung</th>
        </tr>
    </thead>
    <tbody>
        <tr>
        <td><code>0xFFD8</code></td>
        <td><code>SOI</code>*</td>
        <td>Start of Image</td>
        <td>Markiert den Beginn eines JPEG-Bildes.</td>
        </tr>
        <tr>
        <td><code>0xFFDB</code></td>
        <td><code>DQT</code></td>
        <td>Define Quantization Table</td>
        <td>Definiert eine oder mehrere Quantisierungstabellen.</td>
        </tr>
        <tr>
        <td><code>0xFFC0</code></td>
        <td><code>SOF<sub>0</sub></code></td>
        <td>Start of Frame (Baseline DCT)</td>
        <td>Definiert die Bildgröße, die Anzahl der Komponenten und die Präzision der Bilddaten.</td>
        </tr>
        <tr>
        <td><code>0xFFC4</code></td>
        <td><code>DHT</code></td>
        <td>Define Huffman Table</td>
        <td>Definiert eine oder mehrere Huffman-Tabellen.</td>
        </tr>
        <tr>
        <td><code>0xFFDA</code></td>
        <td><code>SOS</code></td>
        <td>Start of Scan</td>
        <td>Markiert den Beginn der komprimierten Bilddaten.</td>
        </tr>
        <tr>
        <td><code>0xFFD9</code></td>
        <td><code>EOI</code>*</td>
        <td>End of Image</td>
        <td>Markiert das Ende eines JPEG-Bildes.</td>
        </tr>
    </tbody>
</table>

<div class='hint hint-warning'>
Du solltest sicherstellen, dass die Datei mit den Bytes 0xFFD8 (SOI) beginnt. Anschließend kannst du in einer Schleife immer einen Marker lesen, dann je nach Marker eine Länge und die entsprechende Anzahl von Bytes überspringen, bis du auf den SOS-Marker (0xFFDA) stößt. Ab diesem Punkt kannst du später die komprimierten Bilddaten lesen.
</div>

#### Start of Image (SOI)

Der SOI-Marker (0xFFD8) markiert den Beginn eines JPEG-Bildes. Er besteht aus zwei Bytes und enthält keine weiteren Daten. Ein gültiges JPEG-Bild muss mit diesem Marker beginnen.

<pre class='spec'>
     7 6 5 4 3 2 1 0        Field Name                    Type
    +---------------+
 0  |               |       Marker (FFD8)                 Word
    +-             -+
 1  |               |
    +---------------+
</pre>

**Beispiel:**

<pre class='spec hexdump'>
00000000  <span class='m0'>ff d8</span> ff e0 00 10 4a 46  49 46 00 01 01 01 01 2c  |......JFIF.....,|
</pre>

Der <span class='m0'>SOI-Marker</span> steht am Anfang der Datei. Sollte er fehlen, beende dein Programm mit einer Fehlermeldung. Gleich im Anschluss an den SOI-Marker folgt oft ein APP0-Segment (0xFFE0), das Metadaten enthält. Wir überspringen es mit Hilfe der Segmentlänge, da es für uns nicht relevant ist.

<div class='hint'>
Falls du vorher noch nie einen Hex-Dump gesehen hast, hier eine kurze Erklärung zu diesem Format: Es werden immer 16 Bytes in einer Zeile dargestellt. Die erste Spalte zeigt den Offset (die Position) des ersten Bytes in der Zeile an (in hexadezimaler Form). Dann folgen die 16 Bytes in hexadezimaler Form (in der Mitte gibt es eine kleine Lücke nach 8 Bytes, um die Lesbarkeit zu verbessern). Am Ende der Zeile wird die ASCII-Darstellung der Bytes angezeigt, wobei nicht druckbare Zeichen durch einen Punkt (.) ersetzt werden. In diesem Beispiel siehst du, dass die ersten beiden Bytes 0xFF und 0xD8 sind (der SOI-Marker), gefolgt von weiteren Daten, die das APP0-Segment bilden.
</div>

#### Define Quantization Table (DQT)

Im DQT-Segment werden Quantisierungstabellen definiert. Es gibt 4 mögliche Slots für diese Tabellen (0 bis 3), die jeweils 64 Werte enthalten. Die 64 Werte sind in der Zig-Zag-Reihenfolge abgespeichert.

<pre class='spec'>
     7 6 5 4 3 2 1 0        Field Name                    Type
    +---------------+
 0  |               |       Marker (FFDB)                 Word
    +-             -+
 1  |               |
    +---------------+
 2  |               |       Segment Length                Word
    +-             -+
 3  |               |
    +---------------+
 4  |   Pq  |  Tq   |       [Packed Fields]               See below
    +---------------+
 5  |               |       Quantization Table Values     64 Bytes
    +-   . . . .   -+
68  |               |
    +---------------+

    [Packed Fields]  =      Pq: Precision (0 = 8-bit, 1 = 16-bit)     4 Bits
                            Tq: Table Identifier (0–3)                4 Bits
</pre>

<div class='hint hint-warning'>
Für unsere Zwecke kannst du davon ausgehen, dass die Quantisierungstabelle immer 8-Bit-Werte enthält (Pq = 0). Beende dein Programm, falls Pq &ne; 0 ist.

Es kann vorkommen, dass in einem DQT-Segment mehrere Quantisierungstabellen definiert werden. Du erkennst dies daran, dass die Segmentlänge größer als erwartet ist. In diesem Fall wiederholt sich die Struktur ab Offset 4.
</div>

**Beispiel:**

<pre class='spec hexdump'>
00000010  01 2c 00 00 <span class='m0'>ff db</span> <span class='m1'>00 43</span>  <span class='m2'>00</span> <span class='m3'>05 03 04 04 04 03 05</span>  |.,.....C........|
00000020  <span class='m3'>04 04 04 05 05 05 06 07  0c 08 07 07 07 07 0f 0b</span>  |................|
00000030  <span class='m3'>0b 09 0c 11 0f 12 12 11  0f 11 11 13 16 1c 17 13</span>  |................|
00000040  <span class='m3'>14 1a 15 11 11 18 21 18  1a 1d 1d 1f 1f 1f 13 17</span>  |......!.........|
00000050  <span class='m3'>22 24 22 1e 24 1c 1e 1f  1e</span> ff db 00 43 01 05 05  |"$".$.......C...|
</pre>

<table class='table'>
<tr>
<td><span class='m0'><code>FF DB</code></span></td>
<td>DQT-Marker</td>
</tr>
<tr>
<td><span class='m1'><code>00 43</code></span></td>
<td>Segmentlänge (0x43 = 67 Bytes)</td>
</tr>
<tr>
<td><span class='m2'><code>00</code></span></td>
<td>Pq = 0 (8-Bit-Werte), Tq = 0 (Slot 0)</td>
</tr>
<tr>
<td><span class='m3'><code>05 03 04 04</code> ...</span></td>
<td>64 Werte der Quantisierungstabelle</td>
</tr>
</table>

Speichere die Werte in dem entsprechenden Slot (Tq) ab, damit du sie später für die Dequantisierung verwenden kannst.

<div class='hint'>
Du kannst für deinen Decoder davon ausgehen, dass Pq immer 0 ist, wir also 8-Bit-Werte einlesen können. Sollte Pq einen anderen Wert haben, beende dein Programm mit einer Fehlermeldung.
</div>

#### Start of Frame (SOF<sub>0</sub>)

Im SOF<sub>0</sub>-Segment (Start of Frame, Baseline DCT) werden die Bildgröße, die Anzahl der Komponenten und die Präzision der Bilddaten definiert. Es enthält auch Informationen über die Subsampling-Methode, die für die Chroma-Kanäle verwendet wird.

<pre class='spec'>
     7 6 5 4 3 2 1 0        Field Name                    Type
    +---------------+
 0  |               |       Marker (FFC0)                 Word
    +-             -+
 1  |               |
    +---------------+
 2  |               |       Segment Length                Word
    +-             -+
 3  |               |
    +---------------+
 4  |       P       |       Sample Precision              Byte
    +---------------+
 5  |       Y       |       Number of Lines               Word
    +-             -+
 6  |               |
    +---------------+
 7  |       X       |       Number of Samples/Line        Word
    +-             -+
 8  |               |
    +---------------+
 9  |      Nf       |       Number of Image Components    Byte
    +---------------+

(for each image component:)

    +---------------+
10  |      Ci       |       Component Identifier          Byte
    +---------------+
11  |   Hi  |  Vi   |       [Packed Fields]               See below
    +---------------+
12  |      Tqi      |       Quantization Table Selector   Byte
    +---------------+

    [Packed Fields]  =      Hi: Horizontal Sampling Factor     4 Bits
                            Vi: Vertical Sampling Factor       4 Bits
</pre>

**Beispiel:**

<pre class='spec hexdump'>
00000090  1e 1e 1e 1e 1e 1e 1e 1e  1e 1e 1e 1e 1e 1e <span class='m0'>ff c0</span>  |................|
000000a0  <span class='m1'>00 11</span> <span class='m2'>08</span> <span class='m3'>02 9b</span> <span class='m4'>03 e8</span> <span class='m5'>03</span>  <span class='m6'>01</span> <span class='m6'>22</span> <span class='m6'>00</span> <span class='m7'>02</span> <span class='m7'>11</span> <span class='m7'>01</span> <span class='m8'>03</span> <span class='m8'>11</span>  |........."......|
000000b0  <span class='m8'>01</span> ff c4 00 1f 00 00 01  05 01 01 01 01 01 01 00  |................|
</pre>

<table class='table'>
<tr>
<td><span class='m0'><code>FF C0</code></span></td>
<td>SOF<sub>0</sub>-Marker</td>
</tr>
<tr>
<td><span class='m1'><code>00 11</code></span></td>
<td>Segmentlänge (0x11 = 17 Bytes)</td>
</tr>
<tr>
<td><span class='m2'><code>08</code></span></td>
<td>Sample Precision (8 Bit)</td>
</tr>
<tr>
<td><span class='m3'><code>02 9B</code></span></td>
<td>Anzahl der Zeilen (0x29b = 667)</td>
</tr>
<tr>
<td><span class='m4'><code>03 E8</code></span></td>
<td>Anzahl der Samples pro Zeile (0x3e8 = 1000)</td>
</tr>
<tr>
<td><span class='m5'><code>03</code></span></td>
<td>Anzahl der Komponenten (3)</td>
</tr>
<tr>
<td><span class='m6'><code>01</code></span> <span class='m6'><code>22</code></span> <span class='m6'><code>00</code></span></td>
<td>Komponente 1 (Y), 2:2 Subsampling, Quantization Table 0</td>
</tr>
<tr>
<td><span class='m7'><code>02</code></span> <span class='m7'><code>11</code></span> <span class='m7'><code>01</code></span></td>
<td>Komponente 2 (Cb), 1:1 Subsampling, Quantization Table 1</td>
</tr>
<tr>
<td><span class='m8'><code>03</code></span> <span class='m8'><code>11</code></span> <span class='m8'><code>01</code></span></td>
<td>Komponente 3 (Cr), 1:1 Subsampling, Quantization Table 1</td>
</tr>
</table>

<div class='hint'>
Zusätzlich zum SOF<sub>0</sub>-Marker gibt es noch weitere SOF-Marker (SOF<sub>1</sub>, SOF<sub>2</sub>, ...), die für verschiedene JPEG-Varianten verwendet werden. Für unseren Baseline-JPEG-Decoder kannst du davon ausgehen, dass nur der SOF<sub>0</sub>-Marker verwendet wird.
</div>

#### Define Huffman Table (DHT)

Im DHT-Segment werden Huffman-Tabellen definiert. Es gibt 4 mögliche Slots für diese Tabellen (0 bis 3), die jeweils 16 Werte enthalten, die die Anzahl der Codes pro Codewortlänge angeben, gefolgt von den Symbolen, die den Codes zugeordnet sind.

<pre class='spec'>
     7 6 5 4 3 2 1 0        Field Name                    Type
    +---------------+
 0  |               |       Marker (FFC4)                 Word
    +-             -+
 1  |               |
    +---------------+
 2  |               |       Segment Length                Word
    +-             -+
 3  |               |
    +---------------+
 4  |   Tc  |  Th   |       [Packed Fields]               See below
    +---------------+
 5  |      L1       |       Number of Codes of Length 1   Byte
    +-   . . . .   -+
20  |      L16      |       Number of Codes of Length 16  Byte
    +---------------+
21  |      V1       |       First Symbol                  Byte
    +-   . . . .   -+
    |      Vn       |       Last Symbol                   Byte
    +---------------+

    [Packed Fields]  =      Tc: Table Class (0 = DC, 1 = AC)   4 Bits
                            Th: Table Identifier (0–3)         4 Bits
</pre>

<pre class='spec hexdump'>
000000b0  01 <span class='m0'>ff c4</span> <span class='m1'>00 1f</span> <span class='m2'>00</span> <span class='m3'>00 01  05 01 01 01 01 01 01 00</span>  |................|
000000c0  <span class='m3'>00 00 00 00 00 00</span> <span class='m4'>00 01  02 03 04 05 06 07 08 09</span>  |................|
000000d0  <span class='m4'>0a 0b</span> ff c4 00 b5 10 00  02 01 03 03 02 04 03 05  |................|
</pre>

<pre class='spec hexdump'>
000000d0  0a 0b <span class='m0'>ff c4</span> <span class='m1'>00 b5</span> <span class='m2'>10</span> <span class='m3'>00  02 01 03 03 02 04 03 05</span>  |................|
000000e0  <span class='m3'>05 04 04 00 00 01 7d</span> <span class='m4'>01  02 03 00 04 11 05 12 21</span>  |......}........!|
000000f0  <span class='m4'>31 41 06 13 51 61 07 22  71 14 32 81 91 a1 08 23</span>  |1A..Qa."q.2....#|
00000100  <span class='m4'>42 b1 c1 15 52 d1 f0 24  33 62 72 82 09 0a 16 17</span>  |B...R..$3br.....|
00000110  <span class='m4'>18 19 1a 25 26 27 28 29  2a 34 35 36 37 38 39 3a</span>  |...%&'()*456789:|
00000120  <span class='m4'>43 44 45 46 47 48 49 4a  53 54 55 56 57 58 59 5a</span>  |CDEFGHIJSTUVWXYZ|
00000130  <span class='m4'>63 64 65 66 67 68 69 6a  73 74 75 76 77 78 79 7a</span>  |cdefghijstuvwxyz|
00000140  <span class='m4'>83 84 85 86 87 88 89 8a  92 93 94 95 96 97 98 99</span>  |................|
00000150  <span class='m4'>9a a2 a3 a4 a5 a6 a7 a8  a9 aa b2 b3 b4 b5 b6 b7</span>  |................|
00000160  <span class='m4'>b8 b9 ba c2 c3 c4 c5 c6  c7 c8 c9 ca d2 d3 d4 d5</span>  |................|
00000170  <span class='m4'>d6 d7 d8 d9 da e1 e2 e3  e4 e5 e6 e7 e8 e9 ea f1</span>  |................|
00000180  <span class='m4'>f2 f3 f4 f5 f6 f7 f8 f9  fa</span> ff c4 00 1f 01 00 03  |................|
</pre>

#### Start of Scan (SOS)

Der SOS-Marker (Start of Scan) markiert den Beginn der komprimierten Bilddaten. Er enthält Informationen über die Anzahl der Komponenten im Scan, die zu verwendenden Huffman-Tabellen und die Spezifikationen für die Spektralpositionen und die Fortschrittsanzeige.

<pre class='spec'>
     7 6 5 4 3 2 1 0        Field Name                    Type
    +---------------+
 0  |               |       Marker (FFDA)                 Word
    +-             -+
 1  |               |
    +---------------+
 2  |               |       Segment Length                Word
    +-             -+
 3  |               |
    +---------------+
 4  |      Ns       |       Number of Image Components in Scan Byte
    +---------------+

  <table class="hexdump">
    <thead>
      <tr>
        <th>Offset</th>
        <th>00</th><th>01</th><th>02</th><th>03</th>
        <th>04</th><th>05</th><th>06</th><th>07</th>
        <th>08</th><th>09</th><th>0A</th><th>0B</th>
        <th>0C</th><th>0D</th><th>0E</th><th>0F</th>
        <th>ASCII</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td class="offset">00000000</td>
        <td class="hex">48</td><td class="hex">65</td><td class="hex">6c</td><td class="hex">6c</td>
        <td class="hex">6f</td><td class="hex">20</td><td class="hex">77</td><td class="hex">6f</td>
        <td class="hex">72</td><td class="hex">6c</td><td class="hex">64</td><td class="hex">21</td>
        <td class="hex">00</td><td class="hex">0a</td><td class="hex">ff</td><td class="hex">7e</td>
        <td class="ascii">Hello world!...~</td>
      </tr>
    </tbody>
  </table>
