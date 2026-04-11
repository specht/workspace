<div class='meta'>
image: title.webp
</div>

<script src="/include/huffman_decode.js"></script>
<link rel="stylesheet" href="include/katex/katex.min.css">
<script defer src="include/katex/katex.min.js"></script>
<script defer src="include/katex/auto-render.min.js" onload="renderMathInElement(document.body);"></script>

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
        --color-mg: #ddd;
    }
    pre.spec {
        background: none;
        color: #333;
        line-height: 135%;
    }

    pre.hexdump {
        /* background: #f5f5f5; */
        line-height: 135%;

        .m0, .m1, .m2, .m3, .m4, .m5, .m6, .m7, .m8, .mg {
            position: relative;
        }

        .m0::after, .m1::after, .m2::after,
        .m3::after, .m4::after, .m5::after,
        .m6::after, .m7::after, .m8::after, .mg::after {
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
        .mg::after { background-color: var(--color-mg); }
    }

    p, td {
        .m0, .m1, .m2, .m3, .m4, .m5, .m6, .m7, .m8, .mg {
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
        .mg { background-color: color-mix(in srgb, var(--color-mg) 75%, transparent); }
    }

    td {
        .m0, .m1, .m2, .m3, .m4, .m5, .m6, .m7, .m8, .mg {
            padding: 0.25em 0.5em;
            code {
                background: transparent;
                color: inherit;
            }
        }
    }

    p.expl {
        font-style: italic;
        font-size: 0.9em;
        opacity: 0.6;
        margin: 0;
    }

    table.center td, table.center th {
        text-align: center;
    }

    table.dct {
        border-collapse: collapse;
        table-layout: fixed;
        width: unset;
    }

    table.dct td {
        width: 2.5em;
        height: 2.5em;
        font-size: 85%;
        aspect-ratio: 1 / 1;
        padding: 0;
        box-sizing: border-box;
        text-align: center;
        vertical-align: middle;
        line-height: 1;
        overflow: hidden;
        border: 1px solid #ccc;
        font-size: 80%;
    }

    td.m0 { background-color: var(--color-m0); }
    td.m1 { background-color: var(--color-m1); }
    td.m2 { background-color: var(--color-m2); }
    td.m3 { background-color: var(--color-m3); }
    td.m4 { background-color: var(--color-m4); }
    td.m5 { background-color: var(--color-m5); }
    td.m6 { background-color: var(--color-m6); }
    td.m7 { background-color: var(--color-m7); }
    td.m8 { background-color: var(--color-m8); }
    td.mg { background-color: var(--color-mg); }

    .dequant-row {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 2em;
        width: 100%;
    }

    .dequant-row .op {
        font-size: 24px;
    }

</style>

# Eine JPEG-Datei decodieren

<p class='abstract'>
Das JPEG-Format (Joint Photographic Experts Group) ist ein weit verbreitetes Bildformat, das hauptsächlich für die Komprimierung von Fotografien und realistischen Bildern verwendet wird. Es wurde in den frühen 1990er Jahren entwickelt und bietet eine effiziente Methode zur Reduzierung der Dateigröße von Bildern, während gleichzeitig eine akzeptable Bildqualität beibehalten wird. JPEG verwendet eine verlustbehaftete Komprimierungstechnik, bei der bestimmte Bildinformationen entfernt werden, um die Dateigröße zu verringern. Dies führt zu einer gewissen Qualitätsminderung, die jedoch oft nicht sichtbar ist, insbesondere bei höheren Qualitätsstufen.
</p>

## Grundlagen

Dieser Artikel soll dir helfen, einen JPEG-Decoder zu implementieren. Dafür ist es wichtig, zunächst einige Grundlagen zu verstehen:

<div class='row'>
<div class='col-md-4'>
<a href='/rgb'><img src='title.webp' style='width: 100%;'></a>
Farbräume
</div>
<div class='col-md-4'>
<a href='/colorspace'><img src='colorspace.webp' style='width: 100%;'></a>
Trennung von Helligkeit und Farbe
</div>
<div class='col-md-4'>
<a href='/dct'><img src='dct.webp' style='width: 100%;'></a>
Diskrete Kosinustransformation (DCT)
</div>
</div>

<!-- Basiswechsel, Quantisierung, Huffman-Codierung -->

## Grober Ablauf eines JPEG-Decoders

Um eine JPEG-Datei zu decodieren, musst du die folgenden Schritte durchführen:

1. **Marker erkennen**: Suche nach den JPEG-Markern, um die Struktur der Datei zu verstehen.
2. **Segmente interpretieren**: Je nach Marker musst du die entsprechenden Daten interpretieren. Zum Beispiel musst du bei einem DQT-Marker die Quantisierungstabelle lesen, bei einem SOF<sub>0</sub>-Marker die Bildgröße und die Anzahl der Komponenten, und bei einem DHT-Marker einen Huffman-Baum aufbauen.
3. **Huffman-Codes lesen**: Ab dem SOS-Marker (Start of Scan) musst du die komprimierten Bilddaten lesen und die Huffman-Codes decodieren, um die quantisierten DCT-Koeffizienten zu erhalten.
4. **Dequantisierung**: Verwende die passende Quantisierungstabelle, um die quantisierten DCT-Koeffizienten in ihre ursprünglichen Werte zurückzuverwandeln.
5. **Inverse DCT**: Wende die inverse diskrete Kosinustransformation an, um die Pixelwerte aus den DCT-Koeffizienten zu berechnen.
6. **Bild rekonstruieren**: Setze die Pixelwerte zusammen, um das endgültige Bild zu erstellen. Dabei müssen die Chroma-Kanäle entsprechend der Subsampling-Methode interpoliert werden, falls diese verwendet wurde.
7. **Farbraumkonvertierung**: Nutze eine Farbraumtransformation, um die Bilddaten von YCbCr zurück in RGB zu konvertieren.
8. **Bild anzeigen**: Zeige das rekonstruierte Bild mit dem Pixelflow Canvas an.

Der vollständige [JPEG-Standard](https://www.w3.org/Graphics/JPEG/itu-t81.pdf) ist sehr umfangreich und komplex. Wir fassen deshalb hier alle relevanten Informationen zusammen, die du für die Implementierung eines Baseline-JPEG-Decoders benötigst.

Wir beginnen mit den Markern und Segmenten, die die Struktur einer JPEG-Datei definieren, und gehen dann Schritt für Schritt durch die Decodierung der Bilddaten.

Marker sind spezielle 16-Bit-Sequenzen, die bestimmte Abschnitte eines JPEG-Bildes kennzeichnen. Sie beginnen immer mit 0xFF, gefolgt von einem weiteren Byte, das den Typ des Markers angibt. Einige Marker stehen für sich allein und beinhalten keine Daten, während andere Marker von einer 16-Bit-Länge gefolgt werden, die die Anzahl der nachfolgenden Datenbytes angibt (inkl. der 2 Bytes für die Länge selbst). Hier ist eine Übersicht über die wichtigsten Marker (dabei werden Marker ohne Daten mit einem Sternchen (*) gekennzeichnet):

<div style='max-width: 100%; overflow-x: auto;'>
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
</div>

<div class='hint hint-warning'>
Du solltest sicherstellen, dass die Datei mit den Bytes 0xFFD8 (SOI) beginnt. Anschließend kannst du in einer Schleife immer einen Marker lesen, dann je nach Marker eine Länge und die entsprechende Anzahl von Bytes überspringen, bis du auf den SOS-Marker (0xFFDA) stößt. Ab diesem Punkt kannst du später die komprimierten Bilddaten lesen.
</div>

## Start of Image (SOI)

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

Die Beispiele beziehen sich alle auf die Datei <a href='/jpeg/44-baseline.jpg' target='_blank'>44-baseline.jpg</a>, die du zum Testen deiner Implementation verwenden kannst.

## Define Quantization Table (DQT)

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

**Beispiel:**

<pre class='spec hexdump'>
00000010  01 2c 00 00 <span class='m0'>ff db</span> <span class='m1'>00 43</span>  <span class='m2'>00</span> <span class='m3'>05 03 04 04 04 03 05</span>  |.,.....C........|
00000020  <span class='m3'>04 04 04 05 05 05 06 07  0c 08 07 07 07 07 0f 0b</span>  |................|
00000030  <span class='m3'>0b 09 0c 11 0f 12 12 11  0f 11 11 13 16 1c 17 13</span>  |................|
00000040  <span class='m3'>14 1a 15 11 11 18 21 18  1a 1d 1d 1f 1f 1f 13 17</span>  |......!.........|
00000050  <span class='m3'>22 24 22 1e 24 1c 1e 1f  1e</span> ff db 00 43 01 05 05  |"$".$.......C...|
</pre>

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table'>
<tr>
<td><span class='m0'><code>FF DB</code></span></td>
<td>DQT-Marker</td>
</tr>
<tr>
<td><span class='m1'><code>00 43</code></span></td>
<td>Segmentlänge (0x43 = 67 Bytes)
<p class='expl'>
Es kann vorkommen, dass in einem DQT-Segment mehrere Quantisierungstabellen definiert werden. Du erkennst dies daran, dass die Segmentlänge größer als erwartet ist. In diesem Fall wiederholt sich die Struktur ab Offset 4.
</p>
</td>
</tr>
<tr>
<td><span class='m2'><code>00</code></span></td>
<td>Pq = 0 (8-Bit-Werte), Tq = 0 (Slot 0)
<p class='expl'>
Du kannst für deinen Decoder davon ausgehen, dass Pq immer 0 ist, wir also 8-Bit-Werte einlesen können. Sollte Pq einen anderen Wert haben, beende dein Programm mit einer Fehlermeldung.
</p>
</td>
</tr>
<tr>
<td><span class='m3' style='white-space: nowrap;'><code>05 03 04 04</code> ...</span></td>
<td>64 Werte der Quantisierungstabelle
<p class='expl'>
Speichere die Werte in dem entsprechenden Slot (Tq) ab, damit du sie später für die Dequantisierung verwenden kannst.
</p>
</td>
</tr>
</table>
</div>

## Start of Frame (SOF<sub>0</sub>)

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

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table'>
<tr>
<td><span class='m0'><code>FF C0</code></span></td>
<td>SOF<sub>0</sub>-Marker
<p class='expl'>
Zusätzlich zum SOF<sub>0</sub>-Marker gibt es noch weitere SOF-Marker (SOF<sub>1</sub>, SOF<sub>2</sub>, ...), die für verschiedene JPEG-Varianten verwendet werden. Für unseren Baseline-JPEG-Decoder kannst du davon ausgehen, dass nur der SOF<sub>0</sub>-Marker verwendet wird.
</p>
</td>
</tr>
<tr>
<td><span class='m1'><code>00 11</code></span></td>
<td>Segmentlänge (0x11 = 17 Bytes)</td>
</tr>
<tr>
<td><span class='m2'><code>08</code></span></td>
<td>Sample Precision (8 Bit)
<p class='expl'>
Du kannst für deinen Decoder davon ausgehen, dass dieser Wert immer 8 ist. Sollte er einen anderen Wert haben, beende dein Programm mit einer Fehlermeldung.
</p>
</td>
</tr>
<tr>
<td><span class='m3'><code>02 9B</code></span></td>
<td>Anzahl der Zeilen (0x29b = 667)
<p class='expl'>
Dieser Wert beschreibt die Höhe des Bildes in Pixeln.
</p>
</td>
</tr>
<tr>
<td><span class='m4'><code>03 E8</code></span></td>
<td>Anzahl der Samples pro Zeile (0x3e8 = 1000)
<p class='expl'>
Dieser Wert beschreibt die Breite des Bildes in Pixeln.
</p>
</td>
</tr>
<tr>
<td><span class='m5'><code>03</code></span></td>
<td>Anzahl der Komponenten (3)
<p class='expl'>
Du kannst für deinen Decoder davon ausgehen, dass dieser Wert immer 3 ist und für die drei Komponenten Y, Cb und Cr steht. Sollte er einen anderen Wert haben, beende dein Programm mit einer Fehlermeldung.
</p>
</td>
</tr>
<tr>
<td style='white-space: nowrap;'><span class='m6'><code>01</code></span> <span class='m6'><code>22</code></span> <span class='m6'><code>00</code></span></td>
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
</div>

### Chroma Subsampling

In diesem Beispiel siehst du, dass die Y-Komponente (Helligkeit) mit einem 2:2-Subsampling codiert ist, während die Cb- und Cr-Komponenten (Farbinformationen) mit einem 1:1-Subsampling codiert sind. Im JPEG-Jargon spricht man von einer Minimal Coding Unit (MCU), die in diesem Fall aus 4 Y-Blöcken, 1 Cb-Block und 1 Cr-Block besteht. Das bedeutet, dass für jede MCU 4 Blöcke der Y-Komponente und jeweils 1 Block der Cb- und Cr-Komponenten codiert werden: die Auflösung der Chroma-Komponenten ist also halb so hoch wie die der Luma-Komponente in beiden Dimensionen. 

## Define Huffman Table (DHT)

Im DHT-Segment werden Huffman-Tabellen definiert, die wir für die Decodierung der komprimierten Bilddaten benötigen. Dabei wird in DC- und AC-Tabellen unterschieden: DC-Tabellen codieren die Differenzen der DC-Koeffizienten, während AC-Tabellen die AC-Koeffizienten codieren. Es gibt 4 mögliche Slots für DC-Tabellen (0 bis 3) und 4 mögliche Slots für AC-Tabellen (0 bis 3). Jede Tabelle besteht aus 16 Bytes, die die Anzahl der Codes für jede Code-Länge von 1 bis 16 angeben, gefolgt von den Symbolen, die diesen Codes zugeordnet sind.

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

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table'>
<tr>
<td><span class='m0'><code>FF C4</code></span></td>
<td>DHT-Marker
</td>
</tr>
<tr>
<td><span class='m1'><code>00 1F</code></span></td>
<td>Segmentlänge (0x1f = 31 Bytes)
<p class='expl'>
Es kann vorkommen, dass in einem DHT-Segment mehrere Huffman-Tabellen definiert werden. Du erkennst dies daran, dass die Segmentlänge größer als erwartet ist. In diesem Fall wiederholt sich die Struktur ab Offset 4.
</p>
</td>
</tr>
<tr>
<td><span class='m2'><code>00</code></span></td>
<td>Tc = 0 (DC-Tabelle), Th = 0 (Slot 0)
<p class='expl'>
Dieser Huffman-Baum ist für DC-Slot 0 bestimmt und wird später benötigt werden.
</p>
</td>
</tr>
<tr>
<td style='white-space: nowrap;'>
<div style='margin-bottom: 0.5em;'>
<span class='m3'><code>00</code></span> <span class='m3'><code>01</code></span>
<span class='m3'><code>05</code></span> <span class='m3'><code>01</code></span>
</div>
<div style='margin-bottom: 0.5em;'>
<span class='m3'><code>01</code></span> <span class='m3'><code>01</code></span>
<span class='m3'><code>01</code></span> <span class='m3'><code>01</code></span>
</div>
<div style='margin-bottom: 0.5em;'>
<span class='m3'><code>01</code></span> <span class='m3'><code>00</code></span>
<span class='m3'><code>00</code></span> <span class='m3'><code>00</code></span>
</div>
<div style='margin-bottom: 0.5em;'>
<span class='m3'><code>00</code></span> <span class='m3'><code>00</code></span>
<span class='m3'><code>00</code></span> <span class='m3'><code>00</code></span>
</div>
</td>
<td>Anzahl der Codes pro Code-Länge für die DC-Tabelle im Slot 0
<p class='expl'>
Diese Werte geben an, wie viele Codes es mit einer bestimmten Länge gibt. In diesem Beispiel gibt es 0 Codes mit der Länge 1, 1 Code mit der Länge 2, 5 Codes mit der Länge 3 sowie jeweils 1 Code mit den Längen 4, 5, 6, 7, 8 und 9 – insgesamt also 12 Codes, deren Symbole nun folgen.
</p>
</td>
</tr>
<tr>
<td style='white-space: nowrap;'>
<div style='margin-bottom: 0.5em;'>
<span class='m4'><code>00</code></span> <span class='m4'><code>01</code></span>
<span class='m4'><code>02</code></span> <span class='m4'><code>03</code></span>
</div>
<div style='margin-bottom: 0.5em;'>
<span class='m4'><code>04</code></span> <span class='m4'><code>05</code></span>
<span class='m4'><code>06</code></span> <span class='m4'><code>07</code></span>
</div>
<div style='margin-bottom: 0.5em;'>
<span class='m4'><code>08</code></span> <span class='m4'><code>09</code></span>
<span class='m4'><code>0a</code></span> <span class='m4'><code>0b</code></span>
</div>
</td>
<td>Symbole für die DC-Tabelle im Slot 0
<p class='expl'>
Diese Werte sind die Symbole, die den Codes zugeordnet werden. Das erste Symbol (0x00) wird dem einzigen Code mit der Länge 2 zugeordnet, die nächsten 5 Symbole (0x01 bis 0x05) werden den 5 Codes mit der Länge 3 zugeordnet, und so weiter.
</p>
</td>
</tr>
</table>
</div>

<div class='hint'>
Du findest in der JPEG-Datei keinen kompletten Huffman-Baum, sondern nur eine Art Bauanleitung für einen »kanonischen Huffman-Baum«. Anhand der Anzahl der Codes pro Code-Länge kannst du die Struktur des Baumes rekonstruieren und die Symbole den entsprechenden Codes zuordnen.
</div>

**Implementierungshinweis:**

Für einen JPEG-Decoder benötigst du keinen tatsächlichen Huffman-Baum. Es reicht, für jede Codelänge einen Bereich von Codes zu berechnen, der dieser Länge entspricht, und die Symbole in der richtigen Reihenfolge diesen Codes zuzuordnen.

Wir benötigen die folgenden Arrays:

- `min_code`: Ein Array, das für jede Codelänge den kleinsten Code dieser Länge enthält.
- `max_code`: Ein Array, das für jede Codelänge den größten Code dieser Länge enthält.
- `val_ptr`: Ein Array, das für jede Codelänge den Index des ersten Symbols dieser Länge enthält.
- `huffval`: Ein Array, das die Symbole enthält, sortiert nach der Reihenfolge der Codes.

In unserem Beispiel würden sich diese Arrays wie folgt füllen:

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table center'>
<tr>
<th>length</th>
<th>min_code</th>
<th>max_code</th>
<th>(valid codes)</th>
<th>val_ptr</th>
</tr>
<tr>
<td>1</td>
<td>–</td>
<td>–</td>
<td>–</td>
<td>–</td>
</tr>
<tr>
<td>2</td>
<td>0</td>
<td>0</td>
<td>00</td>
<td>0</td>
</tr>
<tr>
<td>3</td>
<td>2</td>
<td>6</td>
<td>010, 011, 100, 101, 110</td>
<td>1</td>
</tr>
<tr>
<td>4</td>
<td>14</td>
<td>14</td>
<td>1110</td>
<td>6</td>
</tr>
<tr>
<td>5</td>
<td>30</td>
<td>30</td>
<td>11110</td>
<td>7</td>
</tr>
<tr>
<td>6</td>
<td>62</td>
<td>62</td>
<td>111110</td>
<td>8</td>
</tr>
<tr>
<td>7</td>
<td>126</td>
<td>126</td>
<td>1111110</td>
<td>9</td>
</tr>
<tr>
<td>8</td>
<td>254</td>
<td>254</td>
<td>11111110</td>
<td>10</td>
</tr>
<tr>
<td>9</td>
<td>510</td>
<td>510</td>
<td>111111110</td>
<td>11</td>
</tr>
<tr>
<td>10</td>
<td>–</td>
<td>–</td>
<td>–</td>
<td>–</td>
</tr>
<tr>
<td colspan='5' style='text-align: center;'>...</td>
</tr>
</table>
</div>

Die Werte für `huffval` können direkt aus den Symbolen im DHT-Segment übernommen werden, da sie bereits in der richtigen Reihenfolge vorliegen. Es ergibt sich also folgende Zuordnung:

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table center'>
<tr>
<th>Code</th>
<td>00</td>
<td>010</td>
<td>011</td>
<td>100</td>
<td>101</td>
<td>110</td>
<td>1110</td>
<td>11110</td>
<td>111110</td>
<td>1111110</td>
<td>11111110</td>
<td>111111110</td>
</tr>
<tr>
<th>Symbol</th>
<td>0</td>
<td>1</td>
<td>2</td>
<td>3</td>
<td>4</td>
<td>5</td>
<td>6</td>
<td>7</td>
<td>8</td>
<td>9</td>
<td>10</td>
<td>11</td>
</tr>
</table>
</div>

Im Decoder müssen wir später »nur noch« die Bits solange einzeln einlesen, bis wir einen gültigen Code erkennen und das entsprechende Symbol ermitteln können.

**Ein weiteres Beispiel:**

In diesem Beispiel wird eine AC-Tabelle im Slot 0 definiert, die deutlich mehr Codes enthält als die vorherige DC-Tabelle. Wir benötigen sie später für die Decodierung der AC-Koeffizienten.

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

## Start of Scan (SOS)

Der SOS-Marker (Start of Scan) markiert den Beginn der komprimierten Bilddaten. Er enthält Informationen über die Anzahl der Komponenten im Scan sowie die zu verwendenden Huffman-Tabellen.

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

(for each image component in scan:)

    +---------------+
 5  |      Cs       |       Scan Component Selector       Byte
    +---------------+
 6  |   Td  |  Ta   |       [Packed Fields]               See below
    +---------------+

    [Packed Fields]  =      Td: DC Huffman Table Selector 4 Bits
                            Ta: AC Huffman Table Selector 4 Bits

(at the end:)

    +---------------+
 7  | Ss            |       Start of Spectral Selection   Byte
    +---------------+
 8  | Se            |       End of Spectral Selection     Byte
    +---------------+
 9  | Ah  | Al      |       Successive Approximation      Byte
    +---------------+

    [Packed Fields]  =  Ah: Successive Approximation High 4 Bits
                        Al: Successive Approximation Low  4 Bits

</pre>

**Beispiel:**

<pre class='spec hexdump'>
00000260  fa <span class='m0'>ff da</span> <span class='m1'>00 0c</span> <span class='m2'>03</span> <span class='m3'>01</span> <span class='m3'>00</span>  <span class='m4'>02</span> <span class='m4'>11</span> <span class='m5'>03</span> <span class='m5'>11</span> <span class='m6'>00</span> <span class='m7'>3f</span> <span class='m8'>00</span> f0  |.............?..|
00000270  7b 9d 26 64 7c c4 d9 52  7e e9 ed 59 b3 44 23 60  |{.&d|..R~..Y.D#`|
00000280  92 2e c6 3d 33 5d f5 c5  96 d2 44 67 38 ec 45 72  |...=3]....Dg8.Er|
</pre>

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table'>
<tr>
<td><span class='m0'><code>FF DA</code></span></td>
<td>SOS-Marker
</td>
</tr>
<tr>
<td><span class='m1'><code>00 0C</code></span></td>
<td>Segmentlänge (0x0c = 12 Bytes)</td>
</tr>
<tr>
<td><span class='m2'><code>03</code></span></td>
<td>Anzahl der Komponenten im Scan (3)
<p class='expl'>
Du kannst für deinen Decoder davon ausgehen, dass dieser Wert immer 3 ist und für die drei Komponenten Y, Cb und Cr steht. Sollte er einen anderen Wert haben, beende dein Programm mit einer Fehlermeldung.
</p>
</td>
</tr>
<tr>
<td style='white-space: nowrap;'>
<div style='margin-bottom: 0.5em;'>
<span class='m3'><code>01</code></span> <span class='m3'><code>00</code></span>
</div>
<div style='margin-bottom: 0.5em;'>
<span class='m4'><code>02</code></span> <span class='m4'><code>11</code></span>
</div>
<div style='margin-bottom: 0.5em;'>
<span class='m5'><code>03</code></span> <span class='m5'><code>11</code></span>
</div>
</td>
<td>Auswahl der Huffman-Tabellen
<p class='expl'>
Für jede der drei Komponenten Y, Cb und Cr wird angegeben, welche DC- und AC-Huffman-Tabelle verwendet werden soll. In diesem Beispiel wird für die Y-Komponente DC-Tabelle 0 und AC-Tabelle 0 verwendet, während für die Cb- und Cr-Komponenten jeweils DC-Tabelle 1 und AC-Tabelle 1 verwendet wird.
</p>
</td>
</tr>
<tr>
<td style='white-space: nowrap;'>
<div style='margin-bottom: 0.5em;'>
<span class='m6'><code>00</code></span>
</div>
<div style='margin-bottom: 0.5em;'>
<span class='m7'><code>3f</code></span>
</div>
<div style='margin-bottom: 0.5em;'>
<span class='m8'><code>00</code></span>
</div>
</td>
<td>Parameter für Progressive JPEGs
<p class='expl'>
In komplexeren JPEG-Varianten können Teile der Bilddaten nach und nach codiert werden. Dies war vor allem früher wichtig, als die Übertragung von Bildern über das Internet noch sehr langsam war. Du kannst für deinen Decoder davon ausgehen, dass diese Werte immer 0x00, 0x3f und 0x00 sind. Sollte dies nicht der Fall sein, beende dein Programm mit einer Fehlermeldung.
</p>
</td>
</tr>
</table>
</div>

## Decodierung der Bilddaten

Direkt hinter dem SOS-Segment beginnen die komprimierten Bilddaten:

<pre class='spec hexdump'>
00000260  fa ff da 00 0c 03 01 00  02 11 03 11 00 3f 00 <span class='m0'>f0</span>  |.............?..|
00000270  <span class='m0'>7b 9d 26 64 7c c4 d9 52  7e e9 ed 59 b3 44 23 60</span>  |{.&d|..R~..Y.D#`|
00000280  <span class='m0'>92 2e c6 3d 33 5d f5 c5  96 d2 44 67 38 ec 45 72</span>  |...=3]....Dg8.Er|
</pre>

Dabei wird das Bild MCU-weise codiert. In unserem Beispiel haben wir eine MCU-Größe von 16x16 Pixeln, bestehend aus 4 Blöcken der Y-Komponente (jeweils 8x8 Pixel), 1 Block der Cb-Komponente (8x8 Pixel) und 1 Block der Cr-Komponente (8x8 Pixel), also folgende Reihenfolge: Y<sub>0</sub>, Y<sub>1</sub>, Y<sub>2</sub>, Y<sub>3</sub>, Cb<sub>0</sub>, Cr<sub>0</sub>. Wir müssen demnach insgesamt sechs 8x8-Pixel-Blöcke decodieren, um eine MCU von 16x16 Pixeln zu erhalten.

### Decodierung eines Blocks: DC-Koeffizient

In jedem Block wird zuerst der DC-Koeffizient decodiert, indem die entsprechenden Bits aus der Datei gelesen werden. Wir verwenden in diesem Beispiel die folgende DC-Tabelle in Slot 0, die wir vorher aus dem ersten DHT-Segment erstellt haben:

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table center'>
<tr>
<th>Code</th>
<td>00</td>
<td>010</td>
<td>011</td>
<td>100</td>
<td>101</td>
<td>110</td>
<td>1110</td>
<td>11110</td>
<td>111110</td>
<td>1111110</td>
<td>11111110</td>
<td>111111110</td>
</tr>
<tr>
<th>Symbol</th>
<td>0</td>
<td>1</td>
<td>2</td>
<td>3</td>
<td>4</td>
<td>5</td>
<td>6</td>
<td>7</td>
<td>8</td>
<td>9</td>
<td>10</td>
<td>11</td>
</tr>
</table>
</div>

Nehmen wir die ersten beiden Bytes `F0 7B` und betrachten sie als Bits:

<pre class='spec hexdump'>
11110000 01111011
</pre>

Den ersten Code, den wir finden ist `11110`:

<pre class='spec hexdump'>
<span class='m0'>11110</span> 000 01111011
</pre>

Der Code `11110` entspricht dem Symbol 7, was bedeutet, dass der DC-Koeffizient in diesem Block mit 7 Bits codiert ist. Wir lesen also die nächsten 7 Bits ein:

<pre class='spec hexdump'>
<span class='m0'>11110</span> <span class='m1'>0000111</span> 1011
</pre>

Wir dürfen nun diese Zahl nicht einfach als 7 interpretieren, sondern müssen sie anhand der JPEG-Spezifikation in eine vorzeichenbehaftete Zahl umwandeln:

- höchstes Bit: 1 → positive Zahl, kann so bleiben
- höchstes Bit: 0 → negative Zahl, wir müssen 2<sup>7</sup> - 1 = 127 subtrahieren (die 7 steht für die Anzahl der Bits)

Also rechnen wir: 7 - 127 = -120. Da der DC-Koeffizient nicht direkt, sondern als Differenz zum vorherigen DC-Koeffizienten codiert ist, müssen wir diesen Wert noch zum vorherigen DC-Koeffizienten addieren. Da dies der erste Block ist, nehmen wir an, dass der vorherige DC-Koeffizient 0 war, und erhalten somit einen DC-Koeffizienten von -120 für diesen Block.

<div class='hint'>
Achtung: Für jede Komponente gibt es einen eigenen vorherigen DC-Koeffizienten.
</div>

Unser DCT-Block sieht bisher wie folgt aus:

<table class='table dct'>
<tr>
<td class='m1'>-120</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td>
</tr>
<tr>
<td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td>
</tr>
<tr>
<td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td>
</tr>
<tr>
<td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td>
</tr>
<tr>
<td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td>
</tr>
<tr>
<td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td>
</tr>
<tr>
<td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td>
</tr>
<tr>
<td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td>
</tr>
</table>

### Decodierung eines Blocks: AC-Koeffizienten

Direkt auf die DC-Differenz folgen die AC-Koeffizienten. Schauen wir uns wieder den Hexdump an:

<pre class='spec hexdump'>
00000260  fa ff da 00 0c 03 01 00  02 11 03 11 00 3f 00 <span class='m0'>f0</span>  |.............?..|
00000270  <span class='m0'>7b 9d 26 64 7c c4 d9 52  7e e9 ed 59</span> b3 44 23 60  |{.&d|..R~..Y.D#`|
00000280  92 2e c6 3d 33 5d f5 c5  96 d2 44 67 38 ec 45 72  |...=3]....Dg8.Er|
</pre>

<!--
  00 ->   1 (0x01)
  01 ->   2 (0x02)
  100 ->   3 (0x03)
  1010 ->   0 (0x00)
  1011 ->   4 (0x04)
  1100 ->  17 (0x11)
  11010 ->   5 (0x05)
  11011 ->  18 (0x12)
  11100 ->  33 (0x21)
  111010 ->  49 (0x31)
  111011 ->  65 (0x41)
  1111000 ->   6 (0x06)
  1111001 ->  19 (0x13)
  1111010 ->  81 (0x51)
  1111011 ->  97 (0x61)
  11111000 ->   7 (0x07)
  11111001 ->  34 (0x22)
  11111010 -> 113 (0x71)
  111110110 ->  20 (0x14)
  111110111 ->  50 (0x32)
  111111000 -> 129 (0x81)
  111111001 -> 145 (0x91)
  111111010 -> 161 (0xA1)
  1111110110 ->   8 (0x08)
  1111110111 ->  35 (0x23)
  1111111000 ->  66 (0x42)
  1111111001 -> 177 (0xB1)
  1111111010 -> 193 (0xC1)
  11111110110 ->  21 (0x15)
  11111110111 ->  82 (0x52)
  11111111000 -> 209 (0xD1)
  11111111001 -> 240 (0xF0)
  111111110100 ->  36 (0x24)
  111111110101 ->  51 (0x33)
  111111110110 ->  98 (0x62)
  111111110111 -> 114 (0x72)
  111111111000000 -> 130 (0x82)
  1111111110000010 ->   9 (0x09)
  1111111110000011 ->  10 (0x0A)
  1111111110000100 ->  22 (0x16)
  1111111110000101 ->  23 (0x17)
  1111111110000110 ->  24 (0x18)
  1111111110000111 ->  25 (0x19)
  1111111110001000 ->  26 (0x1A)
  1111111110001001 ->  37 (0x25)
  1111111110001010 ->  38 (0x26)
  1111111110001011 ->  39 (0x27)
  1111111110001100 ->  40 (0x28)
  1111111110001101 ->  41 (0x29)
  1111111110001110 ->  42 (0x2A)
  1111111110001111 ->  52 (0x34)
  1111111110010000 ->  53 (0x35)
  1111111110010001 ->  54 (0x36)
  1111111110010010 ->  55 (0x37)
  1111111110010011 ->  56 (0x38)
  1111111110010100 ->  57 (0x39)
  1111111110010101 ->  58 (0x3A)
  1111111110010110 ->  67 (0x43)
  1111111110010111 ->  68 (0x44)
  1111111110011000 ->  69 (0x45)
  1111111110011001 ->  70 (0x46)
  1111111110011010 ->  71 (0x47)
  1111111110011011 ->  72 (0x48)
  1111111110011100 ->  73 (0x49)
  1111111110011101 ->  74 (0x4A)
  1111111110011110 ->  83 (0x53)
  1111111110011111 ->  84 (0x54)
  1111111110100000 ->  85 (0x55)
  1111111110100001 ->  86 (0x56)
  1111111110100010 ->  87 (0x57)
  1111111110100011 ->  88 (0x58)
  1111111110100100 ->  89 (0x59)
  1111111110100101 ->  90 (0x5A)
  1111111110100110 ->  99 (0x63)
  1111111110100111 -> 100 (0x64)
  1111111110101000 -> 101 (0x65)
  1111111110101001 -> 102 (0x66)
  1111111110101010 -> 103 (0x67)
  1111111110101011 -> 104 (0x68)
  1111111110101100 -> 105 (0x69)
  1111111110101101 -> 106 (0x6A)
  1111111110101110 -> 115 (0x73)
  1111111110101111 -> 116 (0x74)
  1111111110110000 -> 117 (0x75)
  1111111110110001 -> 118 (0x76)
  1111111110110010 -> 119 (0x77)
  1111111110110011 -> 120 (0x78)
  1111111110110100 -> 121 (0x79)
  1111111110110101 -> 122 (0x7A)
  1111111110110110 -> 131 (0x83)
  1111111110110111 -> 132 (0x84)
  1111111110111000 -> 133 (0x85)
  1111111110111001 -> 134 (0x86)
  1111111110111010 -> 135 (0x87)
  1111111110111011 -> 136 (0x88)
  1111111110111100 -> 137 (0x89)
  1111111110111101 -> 138 (0x8A)
  1111111110111110 -> 146 (0x92)
  1111111110111111 -> 147 (0x93)
  1111111111000000 -> 148 (0x94)
  1111111111000001 -> 149 (0x95)
  1111111111000010 -> 150 (0x96)
  1111111111000011 -> 151 (0x97)
  1111111111000100 -> 152 (0x98)
  1111111111000101 -> 153 (0x99)
  1111111111000110 -> 154 (0x9A)
  1111111111000111 -> 162 (0xA2)
  1111111111001000 -> 163 (0xA3)
  1111111111001001 -> 164 (0xA4)
  1111111111001010 -> 165 (0xA5)
  1111111111001011 -> 166 (0xA6)
  1111111111001100 -> 167 (0xA7)
  1111111111001101 -> 168 (0xA8)
  1111111111001110 -> 169 (0xA9)
  1111111111001111 -> 170 (0xAA)
  1111111111010000 -> 178 (0xB2)
  1111111111010001 -> 179 (0xB3)
  1111111111010010 -> 180 (0xB4)
  1111111111010011 -> 181 (0xB5)
  1111111111010100 -> 182 (0xB6)
  1111111111010101 -> 183 (0xB7)
  1111111111010110 -> 184 (0xB8)
  1111111111010111 -> 185 (0xB9)
  1111111111011000 -> 186 (0xBA)
  1111111111011001 -> 194 (0xC2)
  1111111111011010 -> 195 (0xC3)
  1111111111011011 -> 196 (0xC4)
  1111111111011100 -> 197 (0xC5)
  1111111111011101 -> 198 (0xC6)
  1111111111011110 -> 199 (0xC7)
  1111111111011111 -> 200 (0xC8)
  1111111111100000 -> 201 (0xC9)
  1111111111100001 -> 202 (0xCA)
  1111111111100010 -> 210 (0xD2)
  1111111111100011 -> 211 (0xD3)
  1111111111100100 -> 212 (0xD4)
  1111111111100101 -> 213 (0xD5)
  1111111111100110 -> 214 (0xD6)
  1111111111100111 -> 215 (0xD7)
  1111111111101000 -> 216 (0xD8)
  1111111111101001 -> 217 (0xD9)
  1111111111101010 -> 218 (0xDA)
  1111111111101011 -> 225 (0xE1)
  1111111111101100 -> 226 (0xE2)
  1111111111101101 -> 227 (0xE3)
  1111111111101110 -> 228 (0xE4)
  1111111111101111 -> 229 (0xE5)
  1111111111110000 -> 230 (0xE6)
  1111111111110001 -> 231 (0xE7)
  1111111111110010 -> 232 (0xE8)
  1111111111110011 -> 233 (0xE9)
  1111111111110100 -> 234 (0xEA)
  1111111111110101 -> 241 (0xF1)
  1111111111110110 -> 242 (0xF2)
  1111111111110111 -> 243 (0xF3)
  1111111111111000 -> 244 (0xF4)
  1111111111111001 -> 245 (0xF5)
  1111111111111010 -> 246 (0xF6)
  1111111111111011 -> 247 (0xF7)
  1111111111111100 -> 248 (0xF8)
  1111111111111101 -> 249 (0xF9)
  1111111111111110 -> 250 (0xFA)
-->

<div style='position: relative;'>
<div style='float: right; margin-left: 1em; margin-top: 0.25em;'>
<button id='bu-first' class='btn btn-sm btn-success'><i class='bi bi-chevron-bar-left' style='margin-right: 0;'></i></button>
<button id='bu-prev' class='btn btn-sm btn-success'><i class='bi bi-chevron-left' style='margin-right: 0;'></i></button>
<button id='bu-next' class='btn btn-sm btn-success'><i class='bi bi-chevron-right' style='margin-right: 0;'></i></button>
<button id='bu-last' class='btn btn-sm btn-success'><i class='bi bi-chevron-bar-right' style='margin-right: 0;'></i></button>
</div>
<p>
Hier wird die AC-Tabelle im Slot 0 verwendet, die insgesamt 162 Codes definiert, von denen wir für dieses Beispiel nur einen Auszug benötigen.
Verwende die Buttons, um zu sehen, wie der Bitstream decodiert wird (die Bits des bereits decodierten DC-Koeffizienten sind weiterhin markiert):
</p>
<div style='clear: both;'></div>
<div style='max-width: 100%; overflow-x: auto;'>
<table class='table center' id='ac_table'>
<tr>
<th>Code</th>
<td>00</td>
<td>01</td>
<td>100</td>
<td>1010</td>
<td><span class='m2'>1011</span></td>
<td>1100</td>
<td>11010</td>
<td>11011</td>
<td>11100</td>
<td>111010</td>
<td>111011</td>
<td>1111000</td>
<td>1111001</td>
<td>...</td>
<td>111110111</td>
<td>...</td>
</tr>
<tr>
<th>Symbol</th>
<td>0x01</td>
<td>0x02</td>
<td>0x03</td>
<td>0x00</td>
<td>0x04</td>
<td>0x11</td>
<td>0x05</td>
<td>0x12</td>
<td>0x21</td>
<td>0x31</td>
<td>0x41</td>
<td>0x06</td>
<td>0x13</td>
<td>...</td>
<td>0x32</td>
<td>...</td>
</tr>
</table>
</div>
<div style='float: right; margin: 0.5em;' id='div_block'>

</div>
<div id='div_hexdump'></div>

<ol>
<li class='step0'>Wir finden den nächsten Code <code class='read_code'>1011</code>.</li>
<li class='step1'>Zum Code <code class='read_code'>1011</code> gehört das Symbol <code class='read_symbol'>0x04</code>. Dieses Byte beinhaltet zwei Informationen:
<ul>
<li>die Anzahl der Nullen, die vor diesem Koeffizienten in der DCT-Matrix stehen (0 Nullen)</li>
<li>die Anzahl der Bits, die für diesen Koeffizienten codiert sind (4 Bits)</li>
</ul>
<li class='step2'>Wir lesen also die nächsten 4 Bits ein und erhalten <code>1001</code>.</li>
<li class='step3'></li>
</ol>

<div style='clear: both;'></div>
</div>

Es gibt zwei spezielle Symbole bei der Decodierung der AC-Koeffizienten:

- `0x00`: EOB (End of Block) → Alle restlichen Koeffizienten in diesem Block haben den Wert 0.
- `0xF0`: ZRL (Zero Run Length) → Es werden 16 Nullen in der DCT-Matrix eingetragen, bevor der nächste Koeffizient decodiert wird.

## Dequantisierung

Jeder Block wird anschließend dequantisiert, indem die Koeffizienten komponentenweise mit den entsprechenden Werten aus der Quantisierungstabelle multipliziert werden. In unserem Beispiel verwenden wir die folgende Quantisierungstabelle für die Y-Komponente, die wir vorher aus dem ersten DQT-Segment erstellt haben:

<div class="dequant-row">
    <div id="dequant_0"></div>
    <span class="op">×</span>
    <div id="dequant_1"></div>
    <span class="op">=</span>
    <div id="dequant_2"></div>
</div>

## iDCT

Nachdem alle Koeffizienten eines Blocks decodiert wurden, müssen wir die inverse DCT (iDCT) anwenden, um die Pixelwerte zu erhalten. Die iDCT wird auf die 8x8-DCT-Matrix angewendet und liefert eine 8x8-Matrix mit den Pixelwerten zurück. Diese Werte liegen im Bereich von -128 bis 127 und müssen um 128 verschoben werden, um den Bereich von 0 bis 255 zu erhalten.

Du kannst folgende Formel für die iDCT verwenden:

<div id='idct_here'></div>

## Rekonstruktion der MCU

Wenn wir 6 Blöcke decodiert haben (4&times;Y, 1&times;Cb, 1&times;Cr), können wir die MCU von 16x16 Pixeln rekonstruieren. Dazu müssen wir die Blöcke der Y-Komponente entsprechend ihrer Position in der MCU anordnen und die Blöcke der Cb- und Cr-Komponenten auf die Größe von 16x16 Pixeln hochskalieren (da sie nur 8x8 Pixel groß sind). Anschließend können wir die iDCT auf jeden Block anwenden, um die Pixelwerte zu erhalten.

## Farbraumkonvertierung

Nachdem wir die Pixelwerte für die Y-, Cb- und Cr-Komponenten erhalten haben, müssen wir diese in den RGB-Farbraum konvertieren, um das Bild korrekt darstellen zu können. Die Konvertierung erfolgt mit den folgenden Formeln:

<div>
R = Y + 1.402 * (Cr - 128)<br>
G = Y - 0.344136 * (Cb - 128) - 0.714136 * (Cr - 128)<br>
B = Y + 1.772 * (Cb - 128)
</div>

<script>

const step_min = 3;
const step_max = 65;
let step = step_min;
let steps = [];
const hexBytes = `f0 7b 9d 26 64 7c c4 d9 52 7e e9 ed 59`;
const bytes = parseHexBytes(hexBytes);

function activateStep(step) {
    let div = document.querySelector("#div_hexdump");
    div.innerHTML = stepsToMarkedHexdump(bytes, steps, Math.floor((step + 2) / 2));
    let currentStep = steps[Math.floor(step / 4)];
    let phase = step % 4;
    let value = `0x${(currentStep.huffman.symbol >> 4).toString(16).toUpperCase()}${(currentStep.huffman.symbol & 0x0F).toString(16).toUpperCase()}`;
    for (let x of document.querySelectorAll(".read_code")) {
        x.textContent = currentStep ? currentStep.huffman.bits : '';
    }
    marked_cells = [];
    if (step != step_min) {
        marked_cells.push({ content: currentStep.huffman.bits, className: `m${(Math.floor(step / 4) * 2) % 9}` });
    }
    if (phase > 0) {
        marked_cells.push({ content: value, className: `mg` });
    }
    markTableCellsByContent("#ac_table", marked_cells);
    document.querySelector("#div_block").innerHTML = stepsToDctTable(steps, Math.floor((step - 3) / 2 + 2));
    document.querySelector('#bu-first').disabled = step <= step_min;
    document.querySelector("#bu-prev").disabled = step <= step_min;
    document.querySelector("#bu-next").disabled = step >= step_max;
    document.querySelector('#bu-last').disabled = step >= step_max;
    document.querySelector('.step0').style.display = (step > step_min) ? 'list-item' : 'none';
    document.querySelector('.step1').style.display = (step > step_min && (step) % 4 > 0) ? 'list-item' : 'none';
    document.querySelector('.step2').style.display = (step > step_min && (step) % 4 > 1) ? 'list-item' : 'none';
    document.querySelector('.step3').style.display = (step > step_min && (step) % 4 > 2) ? 'list-item' : 'none';
    // Da das höchste Bit 1 ist, handelt es sich um eine positive Zahl, die so bleiben kann. Der <span id='first_next'>erste</span> AC-Koeffizient in diesem Block hat also den Wert 9.
    if (currentStep.decodedValue < 0) {
        document.querySelector('.step3').innerHTML = `Da das höchste Bit 0 ist, handelt es sich um eine negative Zahl. Wir müssen also 2<sup>${currentStep.huffman.symbol & 0x0F}</sup> - 1 = ${Math.pow(2, currentStep.huffman.symbol & 0x0F) - 1} von der decodierten Zahl subtrahieren, um den tatsächlichen Wert zu erhalten. Der ${step > 8 ? `nächste` : `erste`} AC-Koeffizient in diesem Block hat also den Wert ${currentStep.valueBits.rawValue} - ${Math.pow(2, currentStep.huffman.symbol & 0x0F) - 1} =  ${currentStep.decodedValue}.`;
    } else {
        document.querySelector('.step3').textContent = `Da das höchste Bit 1 ist, handelt es sich um eine positive Zahl, die so bleiben kann. Der ${step > 8 ? `nächste` : `erste`} AC-Koeffizient in diesem Block hat also den Wert ${currentStep.decodedValue}.`;
    }
    if (currentStep.extra.run > 0) {
        document.querySelector('.step3').innerHTML += ` <strong>Achtung:</strong> Da die Anzahl der Nullen hier größer als 0 ist, wurde${currentStep.extra.run > 1 ? 'n' : ''} vorher bereits ${currentStep.extra.run} Null${currentStep.extra.run > 1 ? 'en' : ''} eingetragen.`;
    }
    if (currentStep.kind === 'AC_EOB') {
        document.querySelector('.step1').innerHTML = `Beim Symbol <code>0x00</code> handelt es sich um das EOB-Symbol (End of Block). Alle restlichen Koeffizienten in diesem Block haben den Wert 0 und die Decodierung ist damit abgeschlossen.`;
    } else {
        document.querySelector('.step1').innerHTML =
            `Zum Code <code class='read_code'>${currentStep.huffman.bits}</code> gehört das Symbol <code class='read_symbol'>${value}</code>. Dieses Byte beinhaltet zwei Informationen:
            <ul>
            <li>die Anzahl der Nullen, die vor diesem Koeffizienten in der DCT-Matrix stehen (${currentStep.huffman.symbol >> 4} Null${(currentStep.huffman.symbol >> 4) !== 1 ? 'en' : ''})</li>
            <li>die Anzahl der Bits, die für diesen Koeffizienten codiert sind (${currentStep.huffman.symbol & 0x0F} Bit${(currentStep.huffman.symbol & 0x0F) !== 1 ? 's' : ''})</li>
            </ul>`;
        document.querySelector('.step2').innerHTML = `Wir lesen also ${(currentStep.huffman.symbol & 0x0F) === 1 ? 'das nächste' : 'die nächsten'} ${currentStep.huffman.symbol & 0x0F} Bit${(currentStep.huffman.symbol & 0x0F) !== 1 ? 's' : ''} ein und erhalten <code>${currentStep.valueBits.rawValue.toString(2).padStart(currentStep.huffman.symbol & 0x0F, '0')}</code> = ${currentStep.valueBits.rawValue}.`;
    }
}

document.addEventListener('DOMContentLoaded', function() {
    const dcTable = {
        "00": 0x00,
        "010": 0x01,
        "011": 0x02,
        "100": 0x03,
        "101": 0x04,
        "110": 0x05,
        "1110": 0x06,
        "11110": 0x07,
        "111110": 0x08,
        "1111110": 0x09,
        "11111110": 0x0A,
        "111111110": 0x0B
    };

    const acTable = {
        "00": 0x01,
        "01": 0x02,
        "100": 0x03,
        "1010": 0x00,
        "1011": 0x04,
        "1100": 0x11,
        "11010": 0x05,
        "11011": 0x12,
        "11100": 0x21,
        "111010": 0x31,
        "111011": 0x41,
        "1111000": 0x06,
        "1111001": 0x13,
        "1111010": 0x51,
        "1111011": 0x61,
        "11111000": 0x07,
        "11111001": 0x22,
        "11111010": 0x71,
        "111110110": 0x14,
        "111110111": 0x32,
        "111111000": 0x81,
        "111111001": 0x91,
        "111111010": 0xA1,
        "1111110110": 0x08,
        "1111110111": 0x23,
        "1111111000": 0x42,
        "1111111001": 0xB1,
        "1111111010": 0xC1,
        "11111110110": 0x15,
        "11111110111": 0x52,
        "11111111000": 0xD1,
        "11111111001": 0xF0,
        "111111110100": 0x24,
        "111111110101": 0x33,
        "111111110110": 0x62,
        "111111110111": 0x72,
        "111111111000000": 0x82,
        "1111111110000010": 0x09,
        "1111111110000011": 0x0A,
        "1111111110000100": 0x16,
        "1111111110000101": 0x17,
        "1111111110000110": 0x18,
        "1111111110000111": 0x19,
        "1111111110001000": 0x1A,
        "1111111110001001": 0x25,
        "1111111110001010": 0x26,
        "1111111110001011": 0x27,
        "1111111110001100": 0x28,
        "1111111110001101": 0x29,
        "1111111110001110": 0x2A,
        "1111111110001111": 0x34,
        "1111111110010000": 0x35,
        "1111111110010001": 0x36,
        "1111111110010010": 0x37,
        "1111111110010011": 0x38,
        "1111111110010100": 0x39,
        "1111111110010101": 0x3A,
        "1111111110010110": 0x43,
        "1111111110010111": 0x44,
        "1111111110011000": 0x45,
        "1111111110011001": 0x46,
        "1111111110011010": 0x47,
        "1111111110011011": 0x48,
        "1111111110011100": 0x49,
        "1111111110011101": 0x4A,
        "1111111110011110": 0x53,
        "1111111110011111": 0x54,
        "1111111110100000": 0x55,
        "1111111110100001": 0x56,
        "1111111110100010": 0x57,
        "1111111110100011": 0x58,
        "1111111110100100": 0x59,
        "1111111110100101": 0x5A,
        "1111111110100110": 0x63,
        "1111111110100111": 0x64,
        "1111111110101000": 0x65,
        "1111111110101001": 0x66,
        "1111111110101010": 0x67,
        "1111111110101011": 0x68,
        "1111111110101100": 0x69,
        "1111111110101101": 0x6A,
        "1111111110101110": 0x73,
        "1111111110101111": 0x74,
        "1111111110110000": 0x75,
        "1111111110110001": 0x76,
        "1111111110110010": 0x77,
        "1111111110110011": 0x78,
        "1111111110110100": 0x79,
        "1111111110110101": 0x7A,
        "1111111110110110": 0x83,
        "1111111110110111": 0x84,
        "1111111110111000": 0x85,
        "1111111110111001": 0x86,
        "1111111110111010": 0x87,
        "1111111110111011": 0x88,
        "1111111110111100": 0x89,
        "1111111110111101": 0x8A,
        "1111111110111110": 0x92,
        "1111111110111111": 0x93,
        "1111111111000000": 0x94,
        "1111111111000001": 0x95,
        "1111111111000010": 0x96,
        "1111111111000011": 0x97,
        "1111111111000100": 0x98,
        "1111111111000101": 0x99,
        "1111111111000110": 0x9A,
        "1111111111000111": 0xA2,
        "1111111111001000": 0xA3,
        "1111111111001001": 0xA4,
        "1111111111001010": 0xA5,
        "1111111111001011": 0xA6,
        "1111111111001100": 0xA7,
        "1111111111001101": 0xA8,
        "1111111111001110": 0xA9,
        "1111111111001111": 0xAA,
        "1111111111010000": 0xB2,
        "1111111111010001": 0xB3,
        "1111111111010010": 0xB4,
        "1111111111010011": 0xB5,
        "1111111111010100": 0xB6,
        "1111111111010101": 0xB7,
        "1111111111010110": 0xB8,
        "1111111111010111": 0xB9,
        "1111111111011000": 0xBA,
        "1111111111011001": 0xC2,
        "1111111111011010": 0xC3,
        "1111111111011011": 0xC4,
        "1111111111011100": 0xC5,
        "1111111111011101": 0xC6,
        "1111111111011110": 0xC7,
        "1111111111011111": 0xC8,
        "1111111111100000": 0xC9,
        "1111111111100001": 0xCA,
        "1111111111100010": 0xD2,
        "1111111111100011": 0xD3,
        "1111111111100100": 0xD4,
        "1111111111100101": 0xD5,
        "1111111111100110": 0xD6,
        "1111111111100111": 0xD7,
        "1111111111101000": 0xD8,
        "1111111111101001": 0xD9,
        "1111111111101010": 0xDA,
        "1111111111101011": 0xE1,
        "1111111111101100": 0xE2,
        "1111111111101101": 0xE3,
        "1111111111101110": 0xE4,
        "1111111111101111": 0xE5,
        "1111111111110000": 0xE6,
        "1111111111110001": 0xE7,
        "1111111111110010": 0xE8,
        "1111111111110011": 0xE9,
        "1111111111110100": 0xEA,
        "1111111111110101": 0xF1,
        "1111111111110110": 0xF2,
        "1111111111110111": 0xF3,
        "1111111111111000": 0xF4,
        "1111111111111001": 0xF5,
        "1111111111111010": 0xF6,
        "1111111111111011": 0xF7,
        "1111111111111100": 0xF8,
        "1111111111111101": 0xF9,
        "1111111111111110": 0xFA
    };

    const result = decodeJpegDctStream(bytes, dcTable, acTable, {
        initialDC: 0
    });

    steps = result.blocks[0].steps;

    activateStep(step)

    document.querySelector('#bu-first').addEventListener("click", function() {
        step = step_min;
        activateStep(step);
    });
    document.querySelector("#bu-prev").addEventListener("click", function() {
        if (step > step_min) {
            step--;
            activateStep(step);
        }
    });
    document.querySelector("#bu-next").addEventListener("click", function() {
        if (step < step_max) {
            step++;
            activateStep(step);
        }
    });
    document.querySelector('#bu-last').addEventListener("click", function() {
        step = step_max;
        activateStep(step);
    });
    document.addEventListener("keydown", function(event) {
        if (event.key === "ArrowLeft") {
            if (step > step_min) {
                step--;
                activateStep(step);
            }
        } else if (event.key === "ArrowRight") {
            if (step < step_max) {
                step++;
                activateStep(step);
            }
        }
    });

    document.querySelector("#dequant_0").innerHTML = stepsToDctTable(steps, 65, false);

    const q = [
        5, 3, 4, 4, 4, 3, 5, 4,
        4, 4, 5, 5, 5, 6, 7, 12,
        8, 7, 7, 7, 7, 15, 11, 11,
        9, 12, 17, 15, 18, 18, 17, 15,
        17, 17, 19, 22, 28, 23, 19, 20,
        26, 21, 17, 17, 24, 33, 24, 26,
        29, 29, 31, 31, 31, 19, 23, 34,
        36, 34, 30, 36, 28, 30, 31, 30
    ];

    fillQuantAndDequantTablesFromSteps(
        "#dequant_1",
        "#dequant_2",
        steps,
        q,
        65,
        false
    );

katex.render(
  String.raw`
f(x,y)=\frac{1}{4}
\sum_{u=0}^{7}\sum_{v=0}^{7}
C(u)\,C(v)\,F(u,v)\;
\cos\!\Bigg[\frac{(2x+1)u\pi}{16}\Bigg]\;
\cos\!\Bigg[\frac{(2y+1)v\pi}{16}\Bigg]
`,
  document.getElementById("idct_here"),
  { displayMode: true }
);

});

</script>