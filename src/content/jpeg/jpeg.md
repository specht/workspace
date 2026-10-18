<div class='meta'>
image: teaser.webp
kind: labor
language: C
duration: 4 Doppelstunden
requires: [pixelram]
reference: [pixelram, workspace, farbraeume]
template: https://github.com/specht/pixelram-starter.git
template_name: des PixelRAM-Starters
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3,h4">
</div>

<script src="/include/huffman_decode.js"></script>
<script src="/include/idct_stepper.js"></script>

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

    html[data-bs-theme="dark"] {
        --color-m0: #7a6a00;
        --color-m1: #8a4e00;
        --color-m2: #7a1618;
        --color-m3: #7a2f55;
        --color-m4: #4a3f7a;
        --color-m5: #165a8a;
        --color-m6: #0d646b;
        --color-m7: #4a6f2b;
        --color-m8: #6d5233;
        --color-mg: #555;
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
            /* z-index: 2; */
        }

        .m0::before, .m1::before, .m2::before,
        .m3::before, .m4::before, .m5::before,
        .m6::before, .m7::before, .m8::before, .mg::before {
            content: "";
            position: absolute;
            left: -0.2em;
            right: -0.2em;
            /* centred on the digits, not on the line box: hex digits have no
               descenders, so their middle is at about 0.75em from the top */
            top: 0.15em;
            height: 1.2em;
            z-index: -1;
            border-radius: 0.25em;
            opacity: 0.75;
        }

        .m0::before { background-color: var(--color-m0); }
        .m1::before { background-color: var(--color-m1); }
        .m2::before { background-color: var(--color-m2); }
        .m3::before { background-color: var(--color-m3); }
        .m4::before { background-color: var(--color-m4); }
        .m5::before { background-color: var(--color-m5); }
        .m6::before { background-color: var(--color-m6); }
        .m7::before { background-color: var(--color-m7); }
        .m8::before { background-color: var(--color-m8); }
        .mg::before { background-color: var(--color-mg); }
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
        /* justify-content: center; */
        gap: 2em;
        /* width: 100%; */
        max-width: 100%;
        overflow-x: auto;
    }

    .dequant-row table.dct td {
        min-width: 2.5em;
    }

    .dequant-row .op {
        font-size: 24px;
    }

    #div_hexdump {
        pre {
            white-space: break-spaces;
        }
    }

    html[data-bs-theme="dark"] {
        pre.spec {
            color: #ddd;
        }
        .dark-mode-aware {
            filter: invert(1);
        }
        .m0, .m1, .m2, .m3, .m4, .m5, .m6, .m7, .m8, .mg {
            color: #fff;
        }
        pre, pre .w {
            background-color: transparent !important;
        }
    }

</style>

# Eine JPEG-Datei decodieren

<p class='abstract'>
Das JPEG-Format (Joint Photographic Experts Group) ist ein weit verbreitetes Bildformat, das hauptsächlich für die Komprimierung von Fotografien und realistischen Bildern verwendet wird. Es wurde in den frühen 1990er Jahren entwickelt und bietet eine effiziente Methode zur Reduzierung der Dateigröße von Bildern, während gleichzeitig eine akzeptable Bildqualität beibehalten wird. JPEG verwendet eine verlustbehaftete Komprimierungstechnik, bei der bestimmte Bildinformationen entfernt werden, um die Dateigröße zu verringern. Dies führt zu einer gewissen Qualitätsminderung, die jedoch oft nicht sichtbar ist, insbesondere bei höheren Qualitätsstufen.
</p>

## Grundlagen

Dieser Artikel soll dir helfen, einen JPEG-Decoder zu implementieren. Dafür ist es wichtig, zunächst einige Grundlagen zu verstehen:

<div class='gallery columns-3'>
<!-- tutorial-screenshot
# Die Seite »Farbräume«, wie sie aufgeht
open-site: /rgb
tab: preview
viewport: 1280x720
sleep: 2
-->
<figure><a href='/rgb' target='_blank' rel='noopener'><img src='title.webp' alt='Der RGB-Würfel als Drahtgitter, darin ein Farbpunkt, zerlegt in Pfeile für Y, Cb und Cr.'></a><figcaption>Farbräume</figcaption></figure>
<!-- tutorial-screenshot
# »Trennung von Helligkeit und Farbe«
open-site: /colorspace
tab: preview
viewport: 1280x720
sleep: 2
-->
<figure><a href='/colorspace' target='_blank' rel='noopener'><img src='colorspace.webp' alt='Ein Foto, zerlegt in drei Graubilder für Rot, Grün und Blau, mit Reglern dazwischen und dem wieder zusammengesetzten Foto rechts.'></a><figcaption>Trennung von Helligkeit und Farbe</figcaption></figure>
<!-- tutorial-screenshot
# »DCT« zweidimensional: ein 8 x 8-Block aus der gestreiften Krawatte
# rechts, mit allen 64 Koeffizienten, damit die schrägen Streifen in den
# Koeffizienten und in den gewichteten Kosinusmustern zu sehen sind
open-site: /dct
tab: preview
viewport: 1280x720
sleep: 2
hook: dct-select 476 178 2d 64
move-mouse: selector:#coefficients_chooser
sleep: 1
-->
<figure><a href='/dct' target='_blank' rel='noopener'><img src='dct.webp' alt='Die DCT in 2D: ein 8 × 8-Block aus einer gestreiften Krawatte als Tabelle seiner Helligkeiten, daneben seine 64 DCT-Koeffizienten, die mit ihnen gewichteten Kosinusmuster und rechts der Block, wieder zusammengesetzt.'></a><figcaption>Diskrete Kosinustransformation (DCT)</figcaption></figure>
</div>

<!-- Basiswechsel, Quantisierung, Huffman-Codierung -->

Dein Decoder wird ein Programm in C, das das Bild mit PixelRAM zeigt. Hast du schon [eine GIF-Datei decodiert](/gif), kennst du den Weg: Datei lesen, Block für Block, Bits lesen, Bild zeichnen. JPEG ist schwerer, weil das Bild nicht nur gepackt, sondern umgerechnet ist.

## Vorbereitung

### Die PixelRAM-Vorlage

Hast du [Echtzeitgrafik mit C](/pixelram) oder das GIF-Labor schon bearbeitet, öffne einfach den Ordner `/workspace/pixelram-starter` mit <kbd>Strg</kbd><kbd>K</kbd> <kbd>Strg</kbd><kbd>O</kbd>. Sonst klone die Vorlage:

_include(workspace#repository-klonen)

### Die Beispieldateien

Lade im Terminal zwei Fotos in den Ordner der Vorlage:

```bash_copy
wget ${web_root}/jpeg/44-baseline.jpg
wget ${web_root}/jpeg/91-baseline.jpg
```

Beide haben 1000 × 667 Pixel und sind Baseline-JPEGs, die einfachste Art von JPEG, um die es hier geht. Sie unterscheiden sich, wo es für deinen Decoder darauf ankommt: `44-baseline.jpg` speichert die Farbe nur für jedes zweite Pixel in jeder Richtung und benutzt die Huffman-Tabellen aus dem Standard, `91-baseline.jpg` speichert die Farbe für jedes Pixel und bringt eigene, kürzere Tabellen mit. Alle Beispiele auf dieser Seite stammen aus `44-baseline.jpg`; die Aufgaben fragen oft nach der anderen Datei.

### Das Programm

Lege im Ordner der Vorlage die Datei `jpeg.c` an, mit diesem Anfang:

<!-- page-code: own -->
```c_copy
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "pixelram.h"

// Die ganze Datei im Speicher, und wo wir gerade lesen
uint8_t *file;
long file_size;
long pos = 0;

void fail(const char *message)
{
    printf("Fehler bei Byte %ld: %s\n", pos, message);
    exit(1);
}

uint8_t read_byte(void)
{
    if (pos >= file_size)
        fail("Die Datei ist zu Ende.");
    return file[pos++];
}

// JPEG speichert Zahlen mit zwei Bytes im Big Endian: das hohe Byte zuerst
int read_word(void)
{
    int high = read_byte();
    int low = read_byte();
    return (high << 8) | low;
}

void read_file(const char *path)
{
    FILE *f = fopen(path, "rb");
    if (!f)
    {
        printf("Kann %s nicht öffnen.\n", path);
        exit(1);
    }
    fseek(f, 0, SEEK_END);
    file_size = ftell(f);
    fseek(f, 0, SEEK_SET);
    file = malloc(file_size);
    fread(file, 1, file_size, f);
    fclose(f);
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        printf("Welche Datei? make PROGRAM=jpeg ARGS=44-baseline.jpg\n");
        return 1;
    }
    read_file(argv[1]);
    printf("%s: %ld Bytes\n", argv[1], file_size);
    return 0;
}
```

`read_byte` gibt das Byte an der Stelle `pos` zurück und rückt `pos` weiter, `read_word` setzt zwei Bytes zu einer Zahl zusammen, das hohe zuerst. Übersetze mit dem Namen der Datei, die das Programm lesen soll:

```bash
make PROGRAM=jpeg ARGS=44-baseline.jpg
```

<!-- tutorial-screenshot
# Klonen wie im Handbuch »Workspace« (oben eingebunden); jpeg.c ist hier
# schon der fertige Decoder, oben steht dasselbe wie im Gerüst
show-left-sidebar
clone-start: https://github.com/specht/pixelram-starter.git @ 8f36e382554999421d77687c69f7541ca2c4afb7
press: Enter
wait-for-input-value: /workspace/
press: Enter
wait-for-text: Would you like to open the repository?
click: Open
wait-for-text: main.c
write-file: jpeg.c <- file:loesung/jpeg.c
write-file: 44-baseline.jpg <- file:beispiele/44-baseline.jpg
open-file: jpeg.c
terminal-open
terminal-run: make PROGRAM=jpeg ARGS=44-baseline.jpg
terminal-wait-for-prompt: 180
-->
<img class='full' src='make.webp' alt='Der Workspace mit jpeg.c im Editor und 44-baseline.jpg im Explorer; im Terminal darunter hat make PROGRAM=jpeg ARGS=44-baseline.jpg die Datei jpeg.html erzeugt.'>

Öffne `jpeg.html` mit »Go Live« und dann die Konsole von PixelRAM: Fahre mit der Maus über das Bild und klicke unten rechts auf das Terminal-Symbol oder drücke <kbd>&#96;</kbd>. Dort steht `44-baseline.jpg: 143206 Bytes`. In der Konsole kannst du oben auch die Argumente ändern und mit »Run« eine andere Datei laden, ohne neu zu übersetzen.

Von hier an schreibst du den Decoder selbst. Zu jedem Teil sagt dir ein Kasten <strong>Für deinen Decoder</strong>, welche Funktion du brauchst und was dein Programm ausgeben sollte, wenn sie stimmt.

## Grober Ablauf eines JPEG-Decoders

Um eine JPEG-Datei zu decodieren, musst du die folgenden Schritte durchführen:

1. **Marker erkennen**: Suche nach den JPEG-Markern, um die Struktur der Datei zu verstehen.
2. **Segmente interpretieren**: Je nach Marker musst du die entsprechenden Daten interpretieren. Zum Beispiel musst du bei einem DQT-Marker die Quantisierungstabelle lesen, bei einem SOF<sub>0</sub>-Marker die Bildgröße und die Anzahl der Komponenten, und bei einem DHT-Marker einen Huffman-Baum aufbauen.
3. **Huffman-Codes lesen**: Ab dem SOS-Marker (Start of Scan) musst du die komprimierten Bilddaten lesen und die Huffman-Codes decodieren, um die quantisierten DCT-Koeffizienten zu erhalten.
4. **Dequantisierung**: Verwende die passende Quantisierungstabelle, um die quantisierten DCT-Koeffizienten in ihre ursprünglichen Werte zurückzuverwandeln.
5. **Inverse DCT**: Wende die inverse diskrete Kosinustransformation an, um die Pixelwerte aus den DCT-Koeffizienten zu berechnen.
6. **Bild rekonstruieren**: Setze die Pixelwerte zusammen, um das endgültige Bild zu erstellen. Dabei müssen die Chroma-Kanäle entsprechend der Subsampling-Methode interpoliert werden, falls diese verwendet wurde.
7. **Farbraumkonvertierung**: Nutze eine Farbraumtransformation, um die Bilddaten von YCbCr zurück in RGB zu konvertieren.
8. **Bild anzeigen**: Setze jedes Pixel mit `set_pixel_rgb` auf einen Bildschirm von PixelRAM im Modus `pixel_rgb24`.

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

<div class='hint'>
<strong>Für deinen Decoder:</strong> Schreib in <code>main</code> diese Schleife. Lass sie für jeden Marker ausgeben, welcher es ist und wie lang sein Segment ist, und am SOS-Marker aufhören. Merk dir vor dem Überspringen, wo das nächste Segment beginnt: <code>pos</code> plus Länge minus 2, denn die Länge zählt sich selbst mit. Für <code>44-baseline.jpg</code> sieht die Ausgabe so aus:
<pre class='mt-2 mb-0'>
FFE0   16
FFDB   67
FFDB   67
FFC0   17
FFC4   31
FFC4  181
FFC4   31
FFC4  181
FFDA   12
</pre>
Steht dort Unsinn, liest <code>read_word</code> die Bytes vielleicht in der falschen Reihenfolge: JPEG ist Big Endian.
</div>

## Die Segmente der Datei

Vor den eigentlichen Bilddaten steht eine Folge von Segmenten. Jedes beginnt mit einem Marker und beschreibt einen Teil dessen, was der Decoder später braucht: Tabellen, Bildgröße, Komponenten.

### Start of Image (SOI)

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

### Define Quantization Table (DQT)

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

<div class='hint'>
<strong>Für deinen Decoder:</strong> <code>void read_dqt(int length)</code> liest die Tabellen eines DQT-Segments in ein Array <code>int quant[4][64]</code>, so lange, bis das Segment zu Ende ist. Die Werte stehen in Zig-Zag-Reihenfolge in der Datei; wie du sie an ihren Platz im Block bringst, steht weiter unten bei den AC-Koeffizienten. Gib zur Probe die ersten acht Werte aus, wie sie in der Datei stehen: Für Slot 0 von <code>44-baseline.jpg</code> sind es <code>5 3 4 4 4 3 5 4</code>.
</div>

### Start of Frame (SOF<sub>0</sub>)

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

#### Chroma Subsampling

In diesem Beispiel siehst du, dass die Y-Komponente (Helligkeit) mit einem 2:2-Subsampling codiert ist, während die Cb- und Cr-Komponenten (Farbinformationen) mit einem 1:1-Subsampling codiert sind. Im JPEG-Jargon spricht man von einer Minimal Coding Unit (MCU), die in diesem Fall aus 4 Y-Blöcken, 1 Cb-Block und 1 Cr-Block besteht. Das bedeutet, dass für jede MCU 4 Blöcke der Y-Komponente und jeweils 1 Block der Cb- und Cr-Komponenten codiert werden: die Auflösung der Chroma-Komponenten ist also halb so hoch wie die der Luma-Komponente in beiden Dimensionen. 

<div class='hint'>
<strong>Für deinen Decoder:</strong> Lies Breite, Höhe und die drei Komponenten mit Kennung, Sampling-Faktoren und Quantisierungstabelle. Merk dir außerdem die größten Sampling-Faktoren, hier 2 und 2: Sie bestimmen die Größe einer MCU. Dann öffnest du den Bildschirm mit <code>screen_open(width, height, pixel_rgb24, argv[1])</code>. Bei einem anderen SOF-Marker als 0xFFC0, etwa 0xFFC2 für ein progressives JPEG, beendet <code>fail</code> das Programm: Das kann dein Decoder nicht.
</div>

_task(bloecke)

### Define Huffman Table (DHT)

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

<div class='hint'>
<strong>Für deinen Decoder:</strong> <code>void read_dht(int length)</code> liest die Tabellen eines DHT-Segments und rechnet für jede <code>min_code</code>, <code>max_code</code> und <code>val_ptr</code> aus. Ein DHT-Segment kann mehrere Tabellen enthalten, so wie ein DQT-Segment. Am übersichtlichsten ist ein <code>struct</code> mit den vier Arrays, und davon zwei Arrays mit je vier Slots, eines für DC und eines für AC. Gib für jede Tabelle aus, wie viele Codes sie hat: Bei <code>44-baseline.jpg</code> sind es 12, 162, 12 und 162, die Tabellen aus dem Standard.
</div>

_task(codes)

### Start of Scan (SOS)

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

<div class='hint'>
<strong>Für deinen Decoder:</strong> Merk dir für jede Komponente, welche DC- und welche AC-Tabelle sie benutzt. Direkt nach dem SOS-Segment beginnen die Bilddaten, beim Index, auf dem <code>pos</code> jetzt steht. Die Schleife über die Segmente ist damit fertig.
</div>

## Vom Datenstrom zum Bild

Mit den Tabellen aus den Segmenten lassen sich jetzt die komprimierten Bilddaten lesen, Block für Block, bis am Ende die RGB-Werte jedes Pixels feststehen.

### Decodierung der Bilddaten

Direkt hinter dem SOS-Segment beginnen die komprimierten Bilddaten:

<pre class='spec hexdump'>
00000260  fa ff da 00 0c 03 01 00  02 11 03 11 00 3f 00 <span class='m0'>f0</span>  |.............?..|
00000270  <span class='m0'>7b 9d 26 64 7c c4 d9 52  7e e9 ed 59 b3 44 23 60</span>  |{.&d|..R~..Y.D#`|
00000280  <span class='m0'>92 2e c6 3d 33 5d f5 c5  96 d2 44 67 38 ec 45 72</span>  |...=3]....Dg8.Er|
</pre>

Dabei wird das Bild MCU-weise codiert. In unserem Beispiel haben wir eine MCU-Größe von 16x16 Pixeln, bestehend aus 4 Blöcken der Y-Komponente (jeweils 8x8 Pixel), 1 Block der Cb-Komponente (8x8 Pixel) und 1 Block der Cr-Komponente (8x8 Pixel), also folgende Reihenfolge: Y<sub>0</sub>, Y<sub>1</sub>, Y<sub>2</sub>, Y<sub>3</sub>, Cb<sub>0</sub>, Cr<sub>0</sub>. Wir müssen demnach insgesamt sechs 8x8-Pixel-Blöcke decodieren, um eine MCU von 16x16 Pixeln zu erhalten.

#### Bits lesen und Byte Stuffing

Die Bilddaten sind ein Strom von Bits, und JPEG liest jedes Byte vom höchsten Bit an. In diesem Strom kann jeder Bytewert vorkommen, auch 0xFF, mit dem jeder Marker beginnt. Damit ein Programm die Marker trotzdem findet, schreibt der Encoder hinter jedes 0xFF in den Bilddaten ein 0x00, das nicht dazugehört. Das heißt **Byte Stuffing**. In `44-baseline.jpg` steht das erste schon bei Offset 0x387:

<pre class='spec hexdump'>
00000380  80 12 47 9c 7e 35 ec <span class='m0'>ff</span>  <span class='mg'>00</span> 0e be 21 d9 eb 10 47  |..G.~5.....!...G|
</pre>

Zu den Bilddaten gehört nur das <span class='m0'>0xFF</span>, das <span class='mg'>0x00</span> überspringt dein Decoder. Steht hinter einem 0xFF etwas anderes als 0x00, ist es ein Marker, und die Bilddaten sind zu Ende; bei unseren Dateien ist das 0xFFD9, End of Image. `44-baseline.jpg` hat 391 solcher Stellen. Ohne Byte Stuffing geht das Lesen deshalb nach kurzer Zeit schief.

<div class='hint'>
<strong>Für deinen Decoder:</strong> Ein Bitleser aus zwei Funktionen: <code>int read_bit(void)</code> gibt das nächste Bit zurück. Dafür hält er das aktuelle Byte fest und zählt, wie viele Bits davon noch übrig sind; ist keines mehr übrig, liest er das nächste Byte und überspringt das 0x00 nach einem 0xFF. <code>int read_bits(int n)</code> liest <code>n</code> Bits und setzt sie zu einer Zahl zusammen, das erste als höchstes. Darauf baut <code>int decode_huffman(huffman *table)</code>: Sie liest Bit für Bit, hängt jedes hinten an den Code an und prüft nach jedem Bit, ob der Code für seine Länge gültig ist, also höchstens <code>max_code</code>. Dann gibt sie das Symbol zurück. Nach 16 Bits ohne gültigen Code ist die Datei kaputt oder der Bitleser falsch.
</div>

#### Decodierung eines Blocks: DC-Koeffizient

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

<div class='hint'>
<strong>Für deinen Decoder:</strong> <code>int extend(int value, int bits)</code> macht aus den gelesenen Bits die Zahl mit Vorzeichen, nach der Regel oben; bei 0 Bits ist sie 0. Den vorigen DC-Koeffizienten merkst du dir bei der Komponente, und vor dem ersten Block ist er 0. Gib zur Probe die DC-Koeffizienten der ersten Blöcke von Y aus: Der erste ist −120.
</div>

_task(dc)

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

#### Decodierung eines Blocks: AC-Koeffizienten

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

<div style='position: relative;' data-stepper>
<div style='float: right; margin-left: 1em; margin-top: 0.25em;'>
<button id='bu-first' data-go='first' class='btn btn-sm btn-success'><i class='bi bi-chevron-bar-left' style='margin-right: 0;'></i></button>
<button id='bu-prev' data-go='prev' class='btn btn-sm btn-success'><i class='bi bi-chevron-left' style='margin-right: 0;'></i></button>
<button id='bu-next' data-go='next' class='btn btn-sm btn-success'><i class='bi bi-chevron-right' style='margin-right: 0;'></i></button>
<button id='bu-last' data-go='last' class='btn btn-sm btn-success'><i class='bi bi-chevron-bar-right' style='margin-right: 0;'></i></button>
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

#### Zig-Zag

Die 64 Koeffizienten eines Blocks kommen nicht Zeile für Zeile, sondern im Zick-Zack: oben links beginnend, schräg hin und her, bis unten rechts. So stehen die tiefen Frequenzen vorn und die hohen, die nach der Quantisierung meist 0 sind, hinten, wo ein einziges EOB sie alle erledigt. Dieselbe Reihenfolge haben die Quantisierungstabellen im DQT-Segment.

Diese Tabelle sagt für jede Position <code>k</code> im Zick-Zack, an welchen Platz im Block sie gehört, wenn man den Block Zeile für Zeile von 0 bis 63 durchnummeriert:

```c_copy
// Position k im Zick-Zack -> Platz im 8x8-Block, Zeile für Zeile
const int ZIGZAG[64] = {
     0,  1,  8, 16,  9,  2,  3, 10,
    17, 24, 32, 25, 18, 11,  4,  5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13,  6,  7, 14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63,
};
```

Der dritte Koeffizient (k = 2) gehört also auf Platz 8: Zeile 1, Spalte 0.

<div class='hint'>
<strong>Für deinen Decoder:</strong> <code>void decode_block(…)</code> liest einen Block: erst den DC-Koeffizienten, dann in einer Schleife die AC-Koeffizienten von k = 1 bis 63, mit EOB und ZRL. Jeder Koeffizient kommt an den Platz <code>ZIGZAG[k]</code> eines Arrays <code>int coef[64]</code>, das mit Nullen beginnt. Am einfachsten wird die Dequantisierung, wenn du schon beim Lesen der Quantisierungstabellen jeden Wert mit <code>ZIGZAG</code> an seinen Platz bringst: Dann gehören <code>coef[i]</code> und <code>quant[slot][i]</code> zusammen. Vergisst du die Zick-Zack-Reihenfolge, stimmen die Farben trotzdem ungefähr, aber jeder Block ist ein Muster aus Streifen.
</div>

### Dequantisierung

Jeder Block wird anschließend dequantisiert, indem die Koeffizienten komponentenweise mit den entsprechenden Werten aus der Quantisierungstabelle multipliziert werden. In unserem Beispiel verwenden wir die folgende Quantisierungstabelle für die Y-Komponente, die wir vorher aus dem ersten DQT-Segment erstellt haben:

<div style='display: flex; justify-content: center;'>
<div class="dequant-row">
    <div id="dequant_0"></div>
    <span class="op">×</span>
    <div id="dequant_1"></div>
    <span class="op">=</span>
    <div id="dequant_2"></div>
</div>
</div>

Der DC-Koeffizient −120 wird so zu −120 · 5 = −600, der erste AC-Koeffizient 9 zu 9 · 3 = 27.

_task(dequant)

### iDCT

Nachdem alle Koeffizienten eines Blocks decodiert wurden, müssen wir die inverse DCT (iDCT) anwenden, um die Pixelwerte zu erhalten. Die iDCT wird auf die 8x8-DCT-Matrix angewendet und liefert eine 8x8-Matrix mit den Pixelwerten zurück.

Du kannst folgende Formel für die iDCT verwenden:

<div style="text-align: center; margin: 1em 0; max-width: 100%; overflow-x: auto;">
    <img src="/jpeg/idct_formula.svg" class="dark-mode-aware" alt="Formel der inversen diskreten Kosinustransformation für einen acht mal acht Pixel großen Block." style="width: 720px;" />
</div>

Da die resultierenden Werte nach der iDCT noch im Bereich von -128 bis 127 liegen, müssen wir sie um 128 verschieben, um den Bereich von 0 bis 255 zu erhalten.

Was die Formel tut, siehst du, wenn du sie Koeffizient für Koeffizient ausrechnest. Jeder Koeffizient gehört zu einem **Basisbild**, einem Muster aus Kosinuswellen: (0, 0) ist überall gleich hell, (1, 0) ein Verlauf von links nach rechts, (0, 1) einer von oben nach unten, und je größer u und v, desto feiner das Muster. Der Block ist die Summe aller Basisbilder, jedes mit seinem Koeffizienten malgenommen. Unten wird der Block aus dem Beispiel so zusammengesetzt, in der Zick-Zack-Reihenfolge, mit den Koeffizienten nach der Dequantisierung von oben. Die meisten sind 0 und tragen nichts bei:

<div class='idct-stepper' data-stepper data-coef='-600 27 9 0 7 0 0 0 -88 -12 4 6 0 0 0 0 24 -20 -10 0 0 0 0 0 8 5 -14 -9 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0'></div>

Schon nach wenigen Schritten sieht der Block fast fertig aus: Die großen Koeffizienten stehen vorn, die feinen Muster am Ende tragen wenig bei. Genau das nutzt JPEG aus. Die Quantisierung macht die hinteren Koeffizienten klein oder 0, und das Auge vermisst sie kaum.

<div class='hint'>
<strong>Für deinen Decoder:</strong> Die Formel hat vier Summen über je 8 Werte, für jedes der 64 Pixel. Die Kosinuswerte hängen nur von x und u (und y und v) ab: Rechne sie einmal am Anfang in ein Array <code>double idct_cos[8][8]</code>, dann geht es schnell genug. Noch schneller wird es, wenn du erst jede Zeile und dann jede Spalte transformierst, aber das ist nicht nötig. Runde das Ergebnis, verschiebe es um 128 und begrenze es auf 0 bis 255. <code>M_PI</code> gibt es mit <code>-std=c11</code> nicht; schreib dir π als Konstante hin.
</div>

Ob deine iDCT stimmt, prüfst du am Beispiel: Für den ersten Y-Block von `44-baseline.jpg` muss die erste Zeile `40 41 43 46 46 43 41 42` herauskommen, wie oben in der Zeitlupe am Ende.

_task(idct)

### Rekonstruktion der MCU

Wenn wir 6 Blöcke decodiert haben (4&times;Y, 1&times;Cb, 1&times;Cr), können wir die MCU von 16x16 Pixeln rekonstruieren. Dazu müssen wir die Blöcke der Y-Komponente entsprechend ihrer Position in der MCU anordnen und die Blöcke der Cb- und Cr-Komponenten auf die Größe von 16x16 Pixeln hochskalieren (da sie nur 8x8 Pixel groß sind). Die iDCT ist dann schon auf jeden Block angewendet: Hochskaliert werden Pixelwerte, nicht Koeffizienten.

<div class='hint'>
<strong>Für deinen Decoder:</strong> Eine MCU ist 8 · größter horizontaler Sampling-Faktor Pixel breit und 8 · größter vertikaler Faktor hoch, bei <code>44-baseline.jpg</code> also 16 × 16. Die Blöcke einer Komponente kommen Zeile für Zeile: erst die beiden oberen Y-Blöcke, dann die unteren. 667 ist kein Vielfaches von 16: Die letzte Reihe von MCUs ragt über das Bild hinaus, und was darüber hinausragt, zeichnest du nicht. Am einfachsten schreibst du jede Komponente erst in eine eigene Ebene, ein Array in der Größe aller MCUs, und rechnest danach Pixel für Pixel die Farben aus. Bei Subsampling liest du Cb und Cr dabei an einer kleineren Stelle, bei <code>44-baseline.jpg</code> an <code>x / 2</code>, <code>y / 2</code>, allgemein an <code>x · h / hmax</code>, <code>y · v / vmax</code>.
</div>

_task(mcu-lage)

_task(mcus)

### Farbraumkonvertierung

_uses(ycbcr)

Nachdem wir die Pixelwerte für die Y-, Cb- und Cr-Komponenten erhalten haben, müssen wir diese in den RGB-Farbraum konvertieren, um das Bild korrekt darstellen zu können. Die Konvertierung erfolgt mit den folgenden Formeln:

<div style="text-align: center; margin: 1em 0; max-width: 100%; overflow-x: auto;">
    <img src="/jpeg/rgb_formula.svg" class="dark-mode-aware" alt="Formeln zur Umrechnung von YCbCr-Farbwerten in Rot, Grün und Blau." style="width: 490px;" />
</div>

Anschließend müssen die RGB-Werte noch einmal auf den Bereich von 0 bis 255 beschränkt werden, da sie durch die Berechnung auch Werte außerhalb dieses Bereichs annehmen können.

<div class='hint'>
<strong>Für deinen Decoder:</strong> Rechne mit <code>double</code>, runde und begrenze jeden Wert auf 0 bis 255, dann setzt <code>set_pixel_rgb(x, y, r, g, b)</code> das Pixel. Sind Hauttöne grün und Himmel rot, sind Cb und Cr vertauscht.
</div>

## Prüfen

Ob dein Bild stimmt, siehst du zuerst mit dem Auge: Das Foto sollte aussehen wie im Browser.

<!-- tutorial-screenshot
go-live
tab: preview
viewport: 800x533
hook: open jpeg.html
crop-element: #canvas
-->
<img class='full' src='ergebnis.webp' alt='Eine Frau mit Maske trägt ein Kleinkind in einem Tragetuch an einer Bushaltestelle; daneben eine Tafel mit Busnummern.'>

<p class='expl'>Fotos mit freundlicher Genehmigung von Ian Kobylanski · <a href='https://koby.photography/' target='_blank'>https://koby.photography/</a></p>

Pixel für Pixel vergleichen lassen sich zwei JPEG-Decoder nicht: Die iDCT rechnet mit Kommazahlen, und ob ein Wert 127 oder 128 wird, hängt davon ab, wie genau und in welcher Reihenfolge gerechnet wird. Decoder wie der in deinem Browser rechnen Cb und Cr bei Subsampling außerdem weich hoch, statt einen Wert für vier Pixel zu nehmen. Die Mittelwerte von Rot, Grün und Blau über das ganze Bild ändert das aber kaum. Schreib diese Funktion über `main` und ruf sie am Ende auf:

```c_copy
// Mittelwerte von Rot, Grün und Blau über das ganze Bild
void print_average(void)
{
    double sum[3] = {0, 0, 0};
    for (int y = 0; y < screen_height(); y++)
        for (int x = 0; x < screen_width(); x++)
        {
            uint8_t rgb[3];
            get_pixel_rgb(x, y, &rgb[0], &rgb[1], &rgb[2]);
            for (int i = 0; i < 3; i++)
                sum[i] += rgb[i];
        }
    double n = (double) screen_width() * screen_height();
    printf("Mittelwerte: Rot %.1f, Grün %.1f, Blau %.1f\n", sum[0] / n, sum[1] / n, sum[2] / n);
}
```

_task(blau)

Die Mittelwerte prüfen vor allem die DC-Koeffizienten und die Farben. Ob auch die AC-Koeffizienten stimmen, zeigt ein einzelnes Pixel an einer Stelle mit vielen Details:

_task(pixel)

## Weiter geht's

Dein Decoder liest jetzt Baseline-JPEGs. Was ihm noch fehlt:

- **Progressive JPEGs**: Die übrigen Fotos von <a href='/colorspace' target='_blank' rel='noopener'>Trennung von Helligkeit und Farbe</a> und <a href='/dct' target='_blank' rel='noopener'>DCT</a>, zum Beispiel <a href='/jpeg/30.jpg' target='_blank'>30.jpg</a>, sind progressiv (SOF<sub>2</sub>): Erst kommen von allen Blöcken die DC-Koeffizienten, dann in mehreren Durchgängen die AC-Koeffizienten, erst grob, dann genau. Lädt das Bild langsam, wird es nach und nach schärfer, wie bei GIF mit Interlacing. Probier aus, ob dein Decoder sie sauber ablehnt.
- **Restart Marker**: Manche Dateien haben ein DRI-Segment (0xFFDD) und in den Bilddaten alle paar MCUs einen Marker 0xFFD0 bis 0xFFD7. Danach beginnen die vorigen DC-Koeffizienten wieder bei 0, und der Bitleser beginnt an einem neuen Byte. So zerstört ein Fehler in der Datei nicht das ganze restliche Bild.
- **Weiche Farben**: Rechne Cb und Cr bei Subsampling zwischen den Werten hoch, statt einen für vier Pixel zu nehmen. Vergleich die Ränder roter Flächen vorher und nachher.
- **Schneller**: Die iDCT ist der langsamste Teil. Der Algorithmus von Arai, Agui und Nakajima braucht für 8 Werte nur 5 Multiplikationen; die übrigen schlägt er der Dequantisierung zu.

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
    // ← and → press bu-prev and bu-next (stepper_keys.js, data-stepper).

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
});

</script>

<!-- tutorial-step
# Die Aufgaben mit Werten aus dem Decoder: Er muss ausgeben, was die Aufgabe
# erwartet, in ihrer Toleranz (tasks.yaml). Für »pixel« kommt die Zeile dazu,
# die die Aufgabe verlangt.
write-file: 91-baseline.jpg <- file:beispiele/91-baseline.jpg
hook: pixel-print 800 350
terminal-open
terminal-run: clear
terminal-run: make PROGRAM=jpeg ARGS=44-baseline.jpg
terminal-wait-for-prompt: 180
-->

<!-- tutorial-step
# blau (44-baseline.jpg) und pixel (91-baseline.jpg)
tab: preview
hook: open jpeg.html
hook: expect-value Blau 108.9 1
hook: run 91-baseline.jpg
hook: expect-value 350: 80 2
-->

<!-- tutorial-screenshot
# Das Bild für die Karte: der Weg einer Farbe durch den Farbraum. Auf der
# Seite »Farbräume« der Modus YCbCr und Orange (255, 128, 0), das lange,
# deutlich verschiedene Stücke für Y, Cb und Cr hat; nur der Würfel,
# so nah, wie er geht
open-site: /rgb
tab: preview
viewport: 1280x720
sleep: 2
hook: knob Mode YCbCr
hook: knob R 255
hook: knob G 128
hook: knob B 0
hook: only #cube_view -320
-->
<img class='teaser' hidden src='teaser.webp' alt='Der Weg der Farbe Orange durch den YCbCr-Farbraum: vom schwarzen Punkt ein grauer Pfeil für die Helligkeit Y, dann ein grüner für Cb und ein oranger für Cr bis zum orangefarbenen Punkt, im Hintergrund der RGB-Würfel als Drahtgitter.'>
