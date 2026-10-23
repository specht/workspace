<div class='meta'>
kind: reference
search: int, double, uint8_t, Ganzzahldivision, Cast, struct, typedef, Array, Zeiger, Pointer, malloc, printf, Bits, Shift, Maske, fopen, Fehlermeldung, Segmentation fault
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# C zum Nachschlagen

<p class='abstract'>
Was die Aktivitäten in C immer wieder brauchen: Typen und Rechnen, Arrays und Structs, Funktionen aus <code>math.h</code>, Bits und Bytes, Zeiger, Dateien und was die Fehlermeldungen des Compilers sagen. Die Beispiele sind so geschrieben wie in <a href='/pixelram'>PixelRAM</a>, <a href='/raytracing'>Raytracing</a>, <a href='/gif'>GIF</a> und <a href='/jpeg'>JPEG</a>. Was PixelRAM selbst kann, steht im Handbuch <a href='/reference/pixelram'>PixelRAM</a>, was raylib kann, unter <a href='/reference/raylib'>raylib</a>.
</p>

## Ein Programm

<!-- part: c-programm -->
<!-- short: #include, main, Anweisungen mit Semikolon, Blöcke in geschweiften Klammern, Kommentare mit //. -->
```c
#include <stdio.h>
#include <math.h>

#define W 320

// eine Funktion: Rückgabetyp, Name, Parameter mit Typ
double half(double x)
{
    return x / 2;
}

int main(void)
{
    int n = 7;
    printf("%d %f\n", n, half(n));
    return 0;
}
```

- Jede Anweisung endet mit `;`, Blöcke stehen in `{ … }`. Kommentare: `// bis zum Zeilenende` oder `/* … */`.
- `#include <…>` holt die Erklärungen einer Bibliothek: `stdio.h` für `printf` und Dateien, `stdlib.h` für `malloc`, `free` und `exit`, `math.h` für `sqrt` und Co., `stdint.h` für `uint8_t` und seine Verwandten. `pixelram.h` und `raylib.h` bringen `stdbool.h`, `stdint.h` und ihre eigenen Funktionen schon mit.
- `#define W 320` ersetzt vor dem Übersetzen jedes `W` durch `320`. So stehen feste Größen an einer Stelle.
- Eine Funktion muss oberhalb der Stelle stehen, an der sie aufgerufen wird, oder dort vorher angekündigt sein: `double half(double x);`, mit Semikolon statt Rumpf.
- `main` ist der Anfang. Mit `int main(int argc, char **argv)` bekommt das Programm, was beim Start dahinter steht: `argv[1]` ist das erste Wort, `argc` die Zahl der Wörter einschließlich des Programmnamens. In den PixelRAM-Vorlagen gibst du es mit `make ARGS=diskette.gif` mit.
<!-- /part -->

_person(ritchie)

## Zahlen und Rechnen

<!-- part: c-typen -->
<!-- short: int für ganze Zahlen, double für Kommazahlen, uint8_t für ein Byte, bool für wahr oder falsch. -->
### Typen

<table class='table table-sm reference-table'>
<tr><th>Typ</th><th>Was darin steht</th><th>Beispiel</th></tr>
<tr><td><code>int</code></td><td>ganze Zahl, im Workspace 32 Bit: etwa ±2,1 Milliarden</td><td><code>int x = 160;</code></td></tr>
<tr><td><code>long</code></td><td>ganze Zahl, mindestens 32 Bit; Dateigrößen</td><td><code>long pos = 0;</code></td></tr>
<tr><td><code>double</code></td><td>Kommazahl mit etwa 16 Stellen</td><td><code>double t = 0.5;</code></td></tr>
<tr><td><code>float</code></td><td>Kommazahl mit etwa 7 Stellen; raylib rechnet damit</td><td><code>float speed = 2.5f;</code></td></tr>
<tr><td><code>uint8_t</code></td><td>ein Byte ohne Vorzeichen, 0 bis 255</td><td><code>uint8_t red = 255;</code></td></tr>
<tr><td><code>uint16_t</code>, <code>uint32_t</code></td><td>16 und 32 Bit ohne Vorzeichen; <code>int16_t</code>, <code>int32_t</code> mit</td><td><code>uint32_t color = 0xff8800;</code></td></tr>
<tr><td><code>bool</code></td><td><code>true</code> oder <code>false</code></td><td><code>bool hit = false;</code></td></tr>
<tr><td><code>char</code></td><td>ein Zeichen, eigentlich ein Byte; Text ist ein Array davon</td><td><code>char *name = "Kugel";</code></td></tr>
</table>

Eine Variable, die in einer Funktion steht, gilt nur bis zum Ende ihres Blocks, und ihr Wert ist unbestimmt, bis du ihr einen gibst. Eine Variable außerhalb aller Funktionen gilt überall in der Datei und beginnt bei 0.
<!-- /part -->

<!-- part: c-rechnen -->
<!-- short: Ganze Zahlen teilen ganzzahlig (7 / 2 ist 3); ein Cast wie (double) oder (int) wandelt um; % ist der Rest. -->
### Rechnen

<table class='table table-sm reference-table'>
<tr><th>Ausdruck</th><th>Ergebnis</th><th>Warum</th></tr>
<tr><td><code>7 / 2</code></td><td><code>3</code></td><td>zwei ganze Zahlen: ganzzahlige Division, der Rest fällt weg</td></tr>
<tr><td><code>7 / 2.0</code></td><td><code>3.5</code></td><td>sobald eine Kommazahl beteiligt ist, wird mit Kommazahlen gerechnet</td></tr>
<tr><td><code>(double)x / W</code></td><td>zwischen 0 und 1</td><td>der Cast macht aus <code>x</code> vorher eine Kommazahl</td></tr>
<tr><td><code>(int)(2.9)</code></td><td><code>2</code></td><td>der Cast zu <code>int</code> schneidet ab, er rundet nicht; <code>(int)(-2.9)</code> ist <code>-2</code></td></tr>
<tr><td><code>7 % 3</code></td><td><code>1</code></td><td>der Rest der ganzzahligen Division</td></tr>
<tr><td><code>-7 % 3</code></td><td><code>-1</code></td><td>der Rest hat das Vorzeichen der linken Zahl; für einen Index, der nach vorn umläuft: <code>((i % n) + n) % n</code></td></tr>
<tr><td><code>x += 5</code>, <code>x++</code></td><td></td><td>kurz für <code>x = x + 5</code> und <code>x = x + 1</code></td></tr>
<tr><td><code>a &gt; b ? a : b</code></td><td>die größere</td><td>Bedingung, dann der Wert für wahr, dann der für falsch</td></tr>
</table>

Wird ein Wert in einen kleineren Typ geschrieben, geht der Rest verloren: `uint8_t c = 300;` ergibt 44, denn nur die unteren 8 Bit passen hinein (300 − 256). Wo ein Farbwert über 255 kommen kann, begrenze ihn vorher, etwa mit `v > 255 ? 255 : v`.
<!-- /part -->

<!-- part: c-math -->
<!-- short: math.h: sqrt, fabs, floor, fmod, sin, cos und atan2 im Bogenmaß, pow, fmin, fmax, INFINITY. -->
### `math.h`

<table class='table table-sm reference-table'>
<tr><th>Funktion</th><th>Was sie liefert</th></tr>
<tr><td><code>sqrt(x)</code></td><td>die Wurzel</td></tr>
<tr><td><code>fabs(x)</code></td><td>den Betrag einer Kommazahl; <code>abs(n)</code> aus <code>stdlib.h</code> für ganze Zahlen</td></tr>
<tr><td><code>floor(x)</code>, <code>ceil(x)</code>, <code>round(x)</code></td><td>ab-, auf- und gerundet, als Kommazahl</td></tr>
<tr><td><code>fmod(x, y)</code></td><td>den Rest einer Division von Kommazahlen</td></tr>
<tr><td><code>sin(a)</code>, <code>cos(a)</code>, <code>tan(a)</code></td><td>Winkel im Bogenmaß: ein Vollkreis ist 2π, nicht 360</td></tr>
<tr><td><code>atan2(y, x)</code></td><td>den Winkel des Punkts (x, y), von −π bis π; <code>y</code> kommt zuerst</td></tr>
<tr><td><code>pow(x, y)</code></td><td>x hoch y</td></tr>
<tr><td><code>fmin(a, b)</code>, <code>fmax(a, b)</code></td><td>die kleinere, die größere Kommazahl</td></tr>
<tr><td><code>INFINITY</code></td><td>unendlich: größer als jede Zahl, gut als »noch nichts getroffen«</td></tr>
</table>

π steht nicht in C selbst: `#define PI 3.14159265358979` oben in die Datei; raylib bringt `PI` schon mit.
<!-- /part -->

## Verzweigen und wiederholen

<!-- part: c-kontrolle -->
<!-- short: if/else, for, while, break, continue, switch; Bedingungen mit ==, !=, &&, ||, !. -->
```c
if (x < 0)
    x = 0;
else if (x >= W)
    x = W - 1;

for (int y = 0; y < H; y++)        // y von 0 bis H - 1
    for (int x = 0; x < W; x++)
        set_pixel(x, y, color(x, y));

while (!should_close())
{
    // … ein Bild pro Durchgang …
}
```

- Vergleiche: `==`, `!=`, `<`, `<=`, `>`, `>=`; verknüpft mit `&&` (und), `||` (oder), `!` (nicht). `=` ist eine Zuweisung: `if (x = 0)` setzt x auf 0, und die Bedingung ist nie wahr.
- Ohne geschweifte Klammern gehört nur die nächste Anweisung zu `if`, `for` oder `while`. Sobald es zwei sind, braucht es `{ … }`.
- `break` verlässt die Schleife, `continue` springt zum nächsten Durchgang.
- `switch (key) { case 1: …; break; default: …; }` wählt nach einer ganzen Zahl; ohne `break` läuft es in den nächsten `case` weiter.
<!-- /part -->

## Arrays, Structs und Text

<!-- part: c-arrays -->
<!-- short: int a[10]; Index 0 bis 9, keine Prüfung der Grenzen; zweidimensional map[y][x]; die Länge muss man selbst mitführen. -->
### Arrays

```c
int counts[16] = {0};             // 16 ganze Zahlen, alle 0
uint8_t palette[4][3] = {         // 4 Farben mit je Rot, Grün, Blau
    {0, 0, 0}, {255, 255, 255}, {255, 0, 0}, {0, 0, 255},
};
int map[H][W];                    // zweidimensional: erst die Zeile, dann die Spalte

counts[3] += 1;
palette[2][0] = 200;
map[y][x] = 1;
```

- Die Indizes gehen von 0 bis Länge − 1. C prüft das nicht: `counts[16]` liest oder überschreibt Speicher, der jemand anderem gehört. Das Programm rechnet dann falsch oder stürzt ab, oft weit entfernt von der Stelle des Fehlers.
- Ein Array kennt seine Länge nicht. Gib sie einer Funktion mit: `void draw(int *values, int count)`.
- Ein Array lässt sich nicht als Ganzes zuweisen; kopiert wird mit einer Schleife oder `memcpy` aus `string.h`.
<!-- /part -->

<!-- part: c-structs -->
<!-- short: typedef struct { double x, y, z; } vec; eine Funktion kann ein struct zurückgeben: return (vec){…}. -->
### Structs

```c
typedef struct
{
    double x, y, z;
} vec;

vec add(vec a, vec b)
{
    return (vec){a.x + b.x, a.y + b.y, a.z + b.z};
}

vec eye = {0, 1.2, 5.5};
eye.y += 0.1;
```

Ein `struct` fasst Werte unter einem Namen zusammen, mit `typedef` bekommt er einen kurzen Typnamen. `(vec){…}` schreibt einen Wert dieses Typs direkt hin. Structs werden als Ganzes zugewiesen, übergeben und zurückgegeben, als Kopie: Ändert eine Funktion ihren Parameter `a`, bleibt das Original, wie es war.
<!-- /part -->

<!-- part: c-text -->
<!-- short: printf mit %d, %ld, %f, %.2f, %x, %02x, %s, %c; Text ist ein Array von char mit einer 0 am Ende. -->
### Text ausgeben

<table class='table table-sm reference-table'>
<tr><th>Platzhalter</th><th>für</th><th>Beispiel und Ausgabe</th></tr>
<tr><td><code>%d</code></td><td><code>int</code></td><td><code>printf("%d x %d\n", w, h);</code> → <code>320 x 180</code></td></tr>
<tr><td><code>%ld</code></td><td><code>long</code></td><td><code>printf("%ld Bytes\n", size);</code></td></tr>
<tr><td><code>%f</code>, <code>%.2f</code></td><td><code>double</code>, mit 2 Nachkommastellen</td><td><code>printf("%.2f\n", 3.14159);</code> → <code>3.14</code></td></tr>
<tr><td><code>%x</code>, <code>%02x</code>, <code>%08x</code></td><td>hexadezimal, mit führenden Nullen auf 2 oder 8 Stellen</td><td><code>printf("%02x\n", 10);</code> → <code>0a</code></td></tr>
<tr><td><code>%s</code></td><td>Text</td><td><code>printf("Kann %s nicht öffnen.\n", path);</code></td></tr>
<tr><td><code>%c</code></td><td>ein Zeichen</td><td><code>printf("%c\n", 'G');</code></td></tr>
</table>

`\n` beendet die Zeile. Passt ein Platzhalter nicht zum Typ, druckt `printf` Unsinn; der Compiler warnt davor. In PixelRAM erscheint die Ausgabe in der Konsole der Seite (Handbuch [PixelRAM](/reference/pixelram#uebersetzen)).

Text ist ein Array von `char`, dessen letztes Zeichen eine 0 ist: `"GIF"` belegt vier Bytes. Vergleichen geht nicht mit `==`, sondern mit `strcmp(a, b) == 0` aus `string.h`.
<!-- /part -->

## Bits und Bytes

<!-- part: c-bits -->
<!-- short: & maskiert, | setzt zusammen, << und >> schieben; zwei Bytes im Little Endian: low | (high << 8). -->
<table class='table table-sm reference-table'>
<tr><th>Ausdruck</th><th>Was er tut</th><th>Beispiel</th></tr>
<tr><td><code>a &amp; b</code></td><td>Und, Bit für Bit: lässt nur die Bits stehen, die in beiden gesetzt sind</td><td><code>0xb6 &amp; 0x0f</code> → <code>0x06</code>, die unteren vier Bits</td></tr>
<tr><td><code>a | b</code></td><td>Oder, Bit für Bit: setzt zusammen</td><td><code>low | (high &lt;&lt; 8)</code></td></tr>
<tr><td><code>a ^ b</code></td><td>exklusives Oder: kippt die Bits, die in <code>b</code> gesetzt sind</td><td><code>x ^ 1</code> wechselt zwischen gerade und ungerade</td></tr>
<tr><td><code>~a</code></td><td>kippt alle Bits</td><td><code>~0x0f &amp; 0xff</code> → <code>0xf0</code></td></tr>
<tr><td><code>a &lt;&lt; n</code></td><td>schiebt um <code>n</code> Bits nach links: mal 2<sup>n</sup></td><td><code>1 &lt;&lt; 3</code> → <code>8</code></td></tr>
<tr><td><code>a &gt;&gt; n</code></td><td>schiebt nach rechts: ganzzahlig durch 2<sup>n</sup></td><td><code>0xb6 &gt;&gt; 4</code> → <code>0x0b</code>, die oberen vier Bits</td></tr>
<tr><td><code>(a &gt;&gt; n) &amp; 1</code></td><td>das Bit Nummer <code>n</code>, von rechts ab 0 gezählt</td><td><code>(0xb6 &gt;&gt; 7) &amp; 1</code> → <code>1</code></td></tr>
<tr><td><code>(1 &lt;&lt; n) - 1</code></td><td>eine Maske mit den unteren <code>n</code> Bits</td><td><code>(1 &lt;&lt; 5) - 1</code> → <code>0x1f</code></td></tr>
</table>

`0x` schreibt eine Zahl hexadezimal: `0xff` ist 255, jede Ziffer steht für vier Bits. Eine Zahl aus mehreren Bytes steht in GIF im **Little Endian** (das niedrige Byte zuerst, `low | (high << 8)`), in JPEG im **Big Endian** (das hohe zuerst, `(high << 8) | low`). Klammern lohnen sich: `<<` bindet schwächer als `+`, `&` sogar schwächer als `==`. `1 << n + 1` heißt `1 << (n + 1)`, und `x & 1 == 0` heißt `x & (1 == 0)`.
<!-- /part -->

## Speicher und Dateien

<!-- part: c-zeiger -->
<!-- short: Ein Zeiger ist eine Adresse: int *p = &x; *p liest, was dort steht; malloc holt Speicher, free gibt ihn zurück. -->
### Zeiger

```c
int x = 5;
int *p = &x;          // p ist die Adresse von x
*p = 7;               // schreibt 7 dorthin: x ist jetzt 7

uint8_t *file = malloc(size);   // size Bytes, die das Programm behalten darf
if (file == NULL)
    exit(1);                    // kein Speicher mehr frei
file[0] = 0x47;       // ein Zeiger wird wie ein Array benutzt
free(file);           // zurückgeben, wenn er nicht mehr gebraucht wird
```

- `&x` ist die Adresse von `x`, `*p` der Wert an der Adresse `p`. Ein Array, das einer Funktion übergeben wird, kommt als Zeiger auf sein erstes Element an: `void fill(uint8_t *data)`.
- Soll eine Funktion eine Variable des Aufrufers ändern, bekommt sie deren Adresse: `void swap(int *a, int *b)`, aufgerufen als `swap(&x, &y)`.
- `malloc(n)` aus `stdlib.h` holt `n` Bytes, die bleiben, bis `free` sie zurückgibt. Für ein Array von Werten: `malloc(count * sizeof(double))`. Was nach `free` noch benutzt wird, gehört niemandem mehr.
- `NULL` ist ein Zeiger, der nirgendwohin zeigt. Wer über ihn liest oder schreibt, bringt ein Programm im Terminal zum Absturz; in einer PixelRAM-Seite geht es oft unbemerkt weiter, mit Unsinn.
<!-- /part -->

<!-- part: c-dateien -->
<!-- short: fopen(path, "rb"), fseek und ftell für die Größe, fread in einen Speicher von malloc, fclose. -->
### Eine Datei lesen

So liest der GIF-Decoder eine ganze Datei in den Speicher:

```c
FILE *f = fopen(path, "rb");      // "rb": lesen, Byte für Byte, wie es dasteht
if (!f)
{
    printf("Kann %s nicht öffnen.\n", path);
    exit(1);
}
fseek(f, 0, SEEK_END);            // ans Ende ...
long size = ftell(f);             // ... dort steht die Größe
fseek(f, 0, SEEK_SET);            // ... und zurück an den Anfang
uint8_t *data = malloc(size);
fread(data, 1, size, f);          // size Stücke zu je 1 Byte
fclose(f);
```

`fopen` gibt `NULL` zurück, wenn die Datei fehlt; deshalb die Prüfung. Zum Schreiben öffnest du mit `"wb"` und schreibst mit `fwrite(data, 1, size, f)` oder `fprintf(f, …)` wie mit `printf`.
<!-- /part -->

## Wenn etwas nicht klappt

<!-- part: c-fehler -->
<!-- short: Die erste Fehlermeldung zuerst lesen; die häufigsten Meldungen des Compilers und was sie bedeuten. -->
Der Compiler nennt Datei, Zeile und Spalte: `main.c:12:5: error: …`. Lies die **erste** Meldung zuerst; die folgenden sind oft nur Folgen von ihr. Warnungen (`warning:`) verhindern das Übersetzen nicht, zeigen aber fast immer einen echten Fehler.

<table class='table table-sm reference-table'>
<tr><th>Meldung</th><th>Was meistens los ist</th></tr>
<tr><td><code>expected ';' at end of declaration</code>, <code>expected ';' after expression</code></td><td>an der genannten Stelle fehlt ein Semikolon; zeigt sie auf den Anfang einer Zeile, fehlt es am Ende der Zeile davor</td></tr>
<tr><td><code>use of undeclared identifier 'x'</code></td><td>Tippfehler im Namen, oder <code>x</code> ist in einem anderen Block deklariert</td></tr>
<tr><td><code>call to undeclared library function 'sqrt'</code>, <code>call to undeclared function</code></td><td>das <code>#include</code> fehlt, oder die Funktion steht weiter unten als ihr Aufruf</td></tr>
<tr><td><code>expected '}'</code>, <code>expected expression</code></td><td>eine Klammer fehlt oder ist zu viel; von der genannten Zeile aus nach oben suchen</td></tr>
<tr><td><code>format specifies type 'int' but the argument has type 'double'</code></td><td>der Platzhalter in <code>printf</code> passt nicht: <code>%f</code> statt <code>%d</code></td></tr>
<tr><td><code>unused variable</code></td><td>eine Warnung: die Variable wird nie benutzt, oft ein Tippfehler an der Stelle, an der sie gebraucht wird</td></tr>
<tr><td><code>Segmentation fault</code> im Terminal, <code>memory access out of bounds</code> in der Konsole einer PixelRAM-Seite</td><td>kein Compilerfehler: ein Index außerhalb eines Arrays, ein <code>NULL</code>-Zeiger oder Speicher nach <code>free</code>. In einer Webseite stürzt so ein Fehler oft gar nicht ab, sondern rechnet still mit falschen Werten weiter.</td></tr>
</table>

Rechnet das Programm falsch, ohne abzustürzen, hilft ein `printf` an der verdächtigen Stelle mehr als langes Hinsehen: Gib die Werte aus, die du dort erwartest, und vergleiche.
<!-- /part -->

## Zum Nachschlagen

- [cppreference: C](https://en.cppreference.com/w/c): jede Funktion der Standardbibliothek mit Beispiel, sortiert nach Header (`stdio.h`, `math.h`, `string.h` …), und die Sprache selbst, auf Englisch.
- [Beej's Guide to C Programming](https://beej.us/guide/bgc/): eine Einführung in C von vorn, mit Zeigern, Structs und Speicher ausführlich erklärt, frei zu lesen, auf Englisch.
