<div class='meta'>
image: interference.webp
image_alt: "Bunte, ineinander verschlungene Farbringe aus großen Pixeln"
kind: projekt
language: PixelRAM / C
reference: [pixelram]
template: https://github.com/specht/pixelram-starter.git
template_name: des PixelRAM-Starters
</div>

# Echtzeitgrafik mit C

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

<p class='abstract'>
Mit PixelRAM programmierst du schnelle Computergrafik direkt in C. Es gibt keine Befehle für Linien, Kreise oder Figuren: Dein Programm bestimmt die Farbe jedes einzelnen Bildpunkts selbst. Daraus entstehen in diesem Tutorial bewegte Wellenmuster und ein Spiel für zwei an einer Tastatur.
</p>

<div class='gallery'>
<figure>
<img src='interference.webp' alt='Bunte Wellenringe von zwei Quellen überlagern sich.'>
<figcaption>Zwei Wellen, die sich überlagern: 57.600 Pixel, 60-mal pro Sekunde neu berechnet</figcaption>
</figure>
<figure>
<img src='lichtspur.webp' alt='Eine orange und eine türkise Spur winden sich durch ein umrandetes Spielfeld.'>
<figcaption>»Lichtspur«: ein Spiel für zwei, mit Tastatur, Zusammenstößen und Punkten</figcaption>
</figure>
<figure>
<img src='crystal-caves.webp' alt='Das Intro von Crystal Caves: »Apogee presents« in roter Schrift vor einem Sternenhimmel, unten ein gelber Strahlenkranz.'>
<figcaption>Crystal Caves, ein Spiel von 1991, läuft vollständig auf PixelRAM</figcaption>
</figure>
</div>

Bei [Pixelflow Canvas](/drawing) zeichnest du mit einfachen Befehlen in Ruby. PixelRAM geht eine Ebene tiefer: Der Bildschirm ist ein Stück Speicher, in dem für jeden Bildpunkt eine Zahl steht, und dein Programm schreibt diese Zahlen. Das ist der Bildschirm, wie ihn die ersten PCs hatten, und alles andere baust du dir selbst: Muster, Animationen, Spiele, Fraktale, eigene 3D-Grafik.

Dafür verwenden wir C. Die Sprache ist nah an der Arbeitsweise des Computers und schnell genug, um für jedes Bild zehntausende Pixel einzeln auszurechnen, im Browser, 60-mal pro Sekunde.

## Einrichten

### Repository klonen

Für diese Anleitung brauchst du die PixelRAM-Vorlage:

_include(workspace#repository-klonen)

### Übersetzen und ansehen

Öffne die Datei `main.c` und gib im Terminal ein:

```bash
make
```

Beim ersten Mal lädt `make` die Bibliothek PixelRAM herunter. Dann übersetzt es `main.c` in die Datei `main.html`: Dein C-Programm läuft als WebAssembly im Browser.

<!-- tutorial-screenshot
# Klonen wie im Handbuch »Workspace« (oben eingebunden), dann make im Terminal
show-left-sidebar
clone-start: https://github.com/specht/pixelram-starter.git @ 8f36e382554999421d77687c69f7541ca2c4afb7
press: Enter
wait-for-input-value: /workspace/
press: Enter
wait-for-text: Would you like to open the repository?
click: Open
wait-for-text: main.c
open-file: main.c
terminal-open
terminal-run: make
terminal-wait-for-prompt: 180
terminal-expect-no-text: error:
terminal-expect-no-text: ***
-->
<img class='full' src='make.webp' alt='Der Workspace mit main.c im Editor; im Terminal darunter hat make die Datei main.html erzeugt.'>

<img src='../reference/workspace/go-live.webp' class='r' style='width: 21em;' alt='Unten rechts in der Statusleiste der Knopf »Go Live« mit dem Mauszeiger darauf und dem Hinweis »Click to run live server«.'>

Klicke unten rechts auf »Go Live«. Im neuen Tab siehst du die Dateien des Projekts; klicke dort auf `main.html`. Du solltest einen kleinen grünen Punkt in der Mitte sehen:

<div style='clear: both;'></div>

<!-- tutorial-screenshot
go-live
tab: preview
variants: crt
viewport: 800x450
hook: open main.html
crop-element: #canvas
-->
<img class='full' src='green-dot.webp' alt='Ein kleiner grüner Punkt leuchtet in der Mitte einer schwarzen Fläche.'>

<div class='hint'>
<strong>Röhrenbildschirm:</strong> Fährst du mit der Maus über das Bild, erscheinen unten rechts drei Knöpfe. Der mittlere schaltet den CRT-Filter ein (oder <kbd>F6</kbd>): Scanlines und die gewölbte Scheibe eines alten Röhrenbildschirms, passend zu den großen Pixeln. Die Bilder auf dieser Seite haben dafür oben rechts einen eigenen Knopf mit demselben Zeichen <svg class='icon-crt' viewBox='0 0 24 24' aria-hidden='true'><rect x='3' y='4' width='18' height='15' rx='2'/><path d='M7 8h10M7 11.5h10M7 15h10'/></svg>.
</div>

<div class='hint'>
<strong>So arbeitest du ab jetzt:</strong> Datei ändern und speichern, im Terminal <code>make</code>, dann die Vorschau neu laden, falls sie es nicht von selbst tut. Findet <code>make</code> einen Fehler, nennt es die Zeile in deiner Datei, und <code>main.html</code> bleibt, wie sie war. Lies die erste Fehlermeldung zuerst, die übrigen sind oft nur Folgefehler.
</div>

## Ein Bild aus einer Formel

### Das erste Pixel

So sieht `main.c` aus:

```c
#include "pixelram.h"

int main(void)
{
    if (!screen_open(320, 180, pixel_indexed8, "PixelRAM"))
        return 1;

    set_pixel(160, 90, 10);

    return 0;
}
```

`screen_open(320, 180, pixel_indexed8, "PixelRAM")` öffnet einen Bildschirm mit 320 × 180 Pixeln. `set_pixel(160, 90, 10)` setzt genau einen davon: an der Stelle x = 160, y = 90, also in der Mitte, auf die Farbe 10. Der Punkt (0, 0) liegt links oben, y wächst nach unten.

Im Modus `pixel_indexed8` ist jedes Pixel ein Byte, eine Zahl von 0 bis 255. Welche Farbe zu einer Zahl gehört, legt eine **Palette** fest: In der Standardpalette ist 10 ein helles Grün und 0 Schwarz.

### Alle Pixel

Der Bildschirm hat 320 × 180 = **57.600 Pixel**. Für jedes einen eigenen `set_pixel`-Befehl zu schreiben, wäre unsinnig. Stattdessen laufen zwei verschachtelte Schleifen über alle Zeilen und darin über alle Spalten, und eine Funktion `color` sagt für jedes Pixel, welche Farbe es bekommt. Ersetze den Inhalt von `main.c` durch:

```c
#include "pixelram.h"

#define W 320
#define H 180

int color(int x, int y)
{
    return (x / 20 + y / 20) % 16;
}

int main(void)
{
    screen_open(W, H, pixel_indexed8, "PixelRAM");
    use_palette("sweetie_16");

    for (int y = 0; y < H; y++)
        for (int x = 0; x < W; x++)
            set_pixel(x, y, color(x, y));

    return 0;
}
```

`#define W 320` gibt der Breite einen Namen, damit die Zahl nur an einer Stelle steht. `use_palette("sweetie_16")` wählt eine Palette mit 16 Farben, die Nummern 0 bis 15. Deshalb rechnet `color` am Ende `% 16`: Der Rest beim Teilen durch 16 liegt immer zwischen 0 und 15.

<!-- tutorial-screenshot
write-file: main.c <- previous-code
terminal-open
terminal-run: make
terminal-wait-for-prompt
terminal-expect-no-text: error:
terminal-expect-no-text: ***
tab: preview
variants: crt
viewport: 800x450
hook: open main.html
crop-element: #canvas
-->
<img class='full' src='patches.webp' alt='Diagonale Bänder aus bunten quadratischen Farbfeldern füllen die Fläche.'>

`x / 20` ist eine Ganzzahldivision: Für x = 0 bis 19 kommt 0 heraus, für 20 bis 39 kommt 1 heraus, und so weiter. So entstehen Felder von 20 × 20 Pixeln. Dein Programm zeichnet keine Quadrate. Es beantwortet für jedes Pixel einzeln die Frage: **Welche Farbe hast du?** Darum geht es auf dieser ganzen Seite.

### Formeln ausprobieren

Ändere nur die Zeile mit `return` in `color`. Jede dieser Formeln ergibt ein ganz anderes Bild:

<div class='gallery'>
<figure>
<!-- tutorial-screenshot
write-file: main.c <- code:int color(int x, int y)||return (x / 20 + y / 20) % 16;
terminal-open
terminal-run: sed -i 's|return (x / 20 + y / 20) % 16;|return (x * y / 64) % 16;|' main.c && make
terminal-wait-for-prompt
tab: preview
variants: crt
viewport: 800x450
hook: open main.html
crop-element: #canvas
-->
<img src='formel-mal.webp' alt='Bunte Hyperbelbögen laufen von der linken oberen Ecke aus.'>
<figcaption><code>return (x * y / 64) % 16;</code></figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
write-file: main.c <- code:int color(int x, int y)||return (x / 20 + y / 20) % 16;
terminal-open
terminal-run: sed -i 's|return (x / 20 + y / 20) % 16;|return (x * x + y * y) / 256 % 16;|' main.c && make
terminal-wait-for-prompt
tab: preview
variants: crt
viewport: 800x450
hook: open main.html
crop-element: #canvas
-->
<img src='formel-kreise.webp' alt='Viertelkreise in Regenbogenfarben um die linke obere Ecke, nach außen immer enger.'>
<figcaption><code>return (x * x + y * y) / 256 % 16;</code></figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
write-file: main.c <- code:int color(int x, int y)||return (x / 20 + y / 20) % 16;
terminal-open
terminal-run: sed -i 's|return (x / 20 + y / 20) % 16;|return (x \& y) == 0 ? 12 : 0;|' main.c && make
terminal-wait-for-prompt
tab: preview
variants: crt
viewport: 800x450
hook: open main.html
crop-element: #canvas
-->
<img src='formel-sierpinski.webp' alt='Weiße Dreiecke, die aus immer kleineren Dreiecken bestehen.'>
<figcaption><code>return (x & y) == 0 ? 12 : 0;</code></figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
write-file: main.c <- code:int color(int x, int y)||return (x / 20 + y / 20) % 16;
terminal-open
terminal-run: sed -i 's|return (x / 20 + y / 20) % 16;|return (x ^ y) / 8 % 16;|' main.c && make
terminal-wait-for-prompt
tab: preview
variants: crt
viewport: 800x450
hook: open main.html
crop-element: #canvas
-->
<img src='formel-xor.webp' alt='Ein Muster aus bunten Quadraten in sich wiederholenden, verschachtelten Blöcken.'>
<figcaption><code>return (x ^ y) / 8 % 16;</code></figcaption>
</figure>
</div>

Die dritte Formel zeichnet das **Sierpinski-Dreieck**, ein Fraktal. `x & y` verknüpft die beiden Zahlen Bit für Bit: Ein Bit des Ergebnisses ist 1, wenn es in x **und** in y 1 ist. Das Pixel wird weiß (Farbe 12), wenn x und y keine einzige 1 an derselben Stelle haben. `?` und `:` sind ein Wenn-dann-sonst in einer Zeile. `x ^ y` in der vierten Formel ist das bitweise exklusive Oder.

<div class='hint task'>
<strong>Erfinde eigene Formeln.</strong> Was passiert, wenn du in der zweiten Formel <code>x</code> durch <code>x - 160</code> und <code>y</code> durch <code>y - 90</code> ersetzt? Wie bekommst du senkrechte Streifen, wie ein Schachbrett? Achte darauf, dass das Ergebnis zwischen 0 und 15 liegt.
</div>

### Farbpaletten

Dieselben Zahlen sehen mit einer anderen Palette ganz anders aus. Ersetze `"sweetie_16"` zum Beispiel durch `"pico_8"` (16 Farben), `"endesga_32"` (32 Farben) oder `"aap_64"` (64 Farben). Hat eine Palette mehr Farben, kannst du statt `% 16` auch `% 32` oder `% 64` rechnen.

<div class='hint books'>
Alle eingebauten Paletten siehst du unter <a href='https://specht.github.io/pixelram/palettes.html' target='_blank'>specht.github.io/pixelram/palettes.html</a>. Mit <code>set_palette(nummer, rot, grün, blau)</code> stellst du dir eine einzelne Farbe selbst ein, jeweils mit Werten von 0 bis 255.
</div>

## Wellen

Jetzt wird die Formel für `color` eine Rechnung mit Abständen und Winkelfunktionen, und am Ende bewegt sich alles.

### Abstand vom Mittelpunkt

Wie weit ist ein Pixel von der Mitte des Bildschirms entfernt? Der Satz des Pythagoras sagt es: Mit `dx` und `dy`, dem Abstand nach rechts und nach unten, ist der Abstand `sqrt(dx * dx + dy * dy)`. Für `sqrt` brauchst du die Mathematik-Bibliothek: Schreib `#include <math.h>` unter die erste Zeile und ändere `color` so:

```c
int color(int x, int y)
{
    double dx = x - W / 2.0;
    double dy = y - H / 2.0;
    double distance = sqrt(dx * dx + dy * dy);

    return (int)(distance / 6.0) % 16;
}
```

`double` ist eine Kommazahl. `W / 2.0` statt `W / 2` sorgt dafür, dass C mit Kommazahlen rechnet. `(int)` schneidet die Nachkommastellen ab, denn `%` geht nur mit ganzen Zahlen.

<!-- tutorial-screenshot
write-file: main.c <- code:int color(int x, int y)||return (x / 20 + y / 20) % 16;
terminal-open
terminal-run: sed -i -e 's|#include "pixelram.h"|#include "pixelram.h"\n#include <math.h>|' -e 's|    return (x / 20 + y / 20) % 16;|    double dx = x - W / 2.0;\n    double dy = y - H / 2.0;\n    double distance = sqrt(dx * dx + dy * dy);\n\n    return (int)(distance / 6.0) % 16;|' main.c
terminal-wait-for-prompt
terminal-run: make
terminal-wait-for-prompt
terminal-expect-no-text: error:
terminal-expect-no-text: ***
tab: preview
variants: crt
viewport: 800x450
hook: open main.html
crop-element: #canvas
-->
<img class='full' src='color-circles.webp' alt='Konzentrische Kreise in kräftigen Regenbogenfarben.'>

Pixel mit ungefähr demselben Abstand bekommen dieselbe Farbe. So entstehen Kreise, obwohl das Programm keinen einzigen Kreis zeichnet. Probiere aus, was passiert, wenn du `6.0` größer oder kleiner machst.

### Kreiswellen

Die Ringe haben harte Kanten. Weicher wird es mit der Sinusfunktion: `sin` liefert Werte, die gleichmäßig zwischen −1 und 1 hin- und herpendeln. Ersetze die letzte Zeile von `color`:

```c
double wave = sin(distance * 0.12);
return (int)((wave + 1.0) * 7.5);
```

`wave + 1.0` liegt zwischen 0 und 2, mal 7,5 ergibt das 0 bis 15: genau die 16 Farben der Palette. `distance * 0.20` statt `0.12` rückt die Wellen enger zusammen.

<!-- tutorial-screenshot
terminal-open
terminal-run: sed -i 's|    return (int)(distance / 6.0) % 16;|    double wave = sin(distance * 0.12);\n    return (int)((wave + 1.0) * 7.5);|' main.c
terminal-wait-for-prompt
terminal-run: make
terminal-wait-for-prompt
terminal-expect-no-text: error:
terminal-expect-no-text: ***
tab: preview
variants: crt
viewport: 800x450
hook: open main.html
crop-element: #canvas
-->
<img class='full' src='sinus-rings.webp' alt='Viele konzentrische Ringe mit weichen Farbverläufen.'>

### Bewegung

Bisher rechnet das Programm ein einziges Bild und ist dann fertig. Für eine Animation braucht es eine Schleife, die immer wieder ein neues Bild berechnet, bis jemand die Seite schließt:

```c
while (!should_close())
{
    // ein Bild berechnen
    present();   // und zeigen
}
```

`present()` zeigt das fertige Bild und wartet bis zum nächsten, höchstens 60-mal pro Sekunde. `seconds()` sagt, wie viele Sekunden seit dem Start vergangen sind. Steckt diese Zeit in der Sinusfunktion, wandern die Wellen nach außen. Hier das ganze Programm:

```c
#include "pixelram.h"
#include <math.h>

#define W 320
#define H 180

int main(void)
{
    screen_open(W, H, pixel_indexed8, "PixelRAM");
    use_palette("sweetie_16");

    while (!should_close())
    {
        double t = seconds();

        for (int y = 0; y < H; y++)
            for (int x = 0; x < W; x++)
            {
                double distance = hypot(x - W / 2.0, y - H / 2.0);
                double wave = sin(distance * 0.12 - t * 2.0);
                set_pixel(x, y, (int)((wave + 1.0) * 7.5));
            }

        present();
    }

    screen_close();
    return 0;
}
```

`hypot(dx, dy)` rechnet dasselbe wie `sqrt(dx * dx + dy * dy)`, nur kürzer. Die Funktion `color` ist verschwunden, weil die Farbe jetzt auch von `t` abhängt; die Rechnung steht direkt in der Schleife.

<!-- tutorial-screenshot
write-file: main.c <- previous-code
terminal-open
terminal-run: make
terminal-wait-for-prompt
terminal-expect-no-text: error:
terminal-expect-no-text: ***
tab: preview
variants: crt
viewport: 640x360
hook: open main.html
# sin(distance * 0.12 - t * 2.0) wiederholt sich nach π Sekunden
record: 3.14s
-->
<video src='wellen.mp4' autoplay loop muted playsinline style='width: 100%; max-width: 640px;'></video>

Kein Kreis wird verschoben: Das Programm rechnet jedes Bild komplett neu, und weil `t` wächst, liefert `sin` jedes Mal ein wenig andere Werte.

### Zwei Wellen treffen sich

Die Quelle der Wellen muss nicht in der Mitte bleiben. Mit `cos` und `sin` der Zeit wandert ein Punkt in Schleifen über den Bildschirm. Nimm zwei solche Quellen, rechne für jedes Pixel beide Wellen aus und zähle sie zusammen:

```c
#include "pixelram.h"
#include <math.h>

#define W 320
#define H 180

int main(void)
{
    screen_open(W, H, pixel_indexed8, "Interferenz");
    use_palette("sweetie_16");

    while (!should_close())
    {
        double t = seconds();

        // zwei Wellenquellen, die durchs Bild wandern
        double ax = W / 2.0 + cos(t * 0.8) * 70;
        double ay = H / 2.0 + sin(t * 1.1) * 40;
        double bx = W / 2.0 + cos(t * 1.3) * 90;
        double by = H / 2.0 + sin(t * 0.7) * 50;

        for (int y = 0; y < H; y++)
            for (int x = 0; x < W; x++)
            {
                double d1 = hypot(x - ax, y - ay);
                double d2 = hypot(x - bx, y - by);

                double wave = sin(d1 * 0.12 - t * 2.0) + sin(d2 * 0.12 + t * 2.3);

                set_pixel(x, y, (int)((wave + 2) * 3.75));
            }

        present();
    }

    screen_close();
    return 0;
}
```

Wo beide Wellen gleichzeitig oben sind, verstärken sie sich, wo die eine oben und die andere unten ist, löschen sie sich aus. Das nennt man **Interferenz**. Die Summe liegt zwischen −2 und 2, deshalb rechnet die letzte Zeile `(wave + 2) * 3.75`.

<!-- tutorial-screenshot
write-file: main.c <- previous-code
terminal-open
terminal-run: make
terminal-wait-for-prompt
terminal-expect-no-text: error:
terminal-expect-no-text: ***
tab: preview
variants: crt
viewport: 800x450
hook: open main.html
# nach 3,6 Sekunden: die beiden Quellen nah beieinander, ein ruhiges Muster
hook: at 3.6
crop-element: #canvas
-->
<img class='full' src='interference.webp' alt='Bunte Wellenringe von zwei Quellen überlagern sich zu einem verwickelten Muster.'>

Für jedes Bild berechnet das Programm 57.600 Pixel mit je zwei Abständen und zwei Sinuswerten. Bei 60 Bildern pro Sekunde sind das über **3 Millionen Pixel pro Sekunde**. Für solche Rechnungen ist C gemacht.

<div class='hint task'>
<strong>Experimente.</strong> Mehr Ringe mit <code>d1 * 0.18</code>, langsamere Wellen mit <code>t * 1.0</code>, weitere Wege mit <code>cos(t * 0.8) * 110</code>. Füge eine dritte Quelle hinzu: Wie musst du dann die letzte Zeile ändern, damit die Farben wieder zwischen 0 und 15 liegen? Und wie sieht es mit <code>"aap_64"</code> und 64 Farben aus?
</div>

## Ein Spiel: Lichtspur

Zwei Spuren fahren über das Spielfeld und ziehen eine Linie hinter sich her. Wer in eine Linie oder an die Wand fährt, verliert die Runde. Gespielt wird zu zweit an einer Tastatur, mit <kbd>W</kbd><kbd>A</kbd><kbd>S</kbd><kbd>D</kbd> und mit den Pfeiltasten.

Das Spiel bekommt eine eigene Datei. Lege `lichtspur.c` neben `main.c` an und übersetze sie mit:

```bash
make PROGRAM=lichtspur
```

Das ergibt `lichtspur.html`, die du in der Vorschau öffnest. So kann jedes Programm im Ordner seine eigene Datei haben.

### Eine Spur, die fährt

```c
#include "pixelram.h"

#define W 160
#define H 90

int main(void)
{
    screen_open(W, H, pixel_indexed8, "Lichtspur");
    use_palette("sweetie_16");
    set_target_fps(30);

    int x = 20, y = H / 2;   // Position
    int dx = 1, dy = 0;      // Richtung: nach rechts

    while (!should_close())
    {
        if (key_pressed(pixel_key_up))    { dx = 0;  dy = -1; }
        if (key_pressed(pixel_key_down))  { dx = 0;  dy = 1;  }
        if (key_pressed(pixel_key_left))  { dx = -1; dy = 0;  }
        if (key_pressed(pixel_key_right)) { dx = 1;  dy = 0;  }

        x += dx;
        y += dy;
        set_pixel(x, y, 11);

        present();
    }

    screen_close();
    return 0;
}
```

Klicke in die Vorschau, damit sie die Tasten bekommt, und steuere mit den Pfeiltasten. Drei Dinge sind neu:

- Das Spielfeld hat nur 160 × 90 Pixel, also große Pixel, und `set_target_fps(30)` macht 30 Schritte pro Sekunde.
- `key_pressed` ist in genau dem Bild wahr, in dem eine Taste heruntergedrückt wurde. `dx` und `dy` merken sich die Richtung, und jedes Bild geht die Spur einen Pixel weiter.
- Das Programm löscht den Bildschirm nie. Jedes gesetzte Pixel bleibt stehen, so entsteht die Spur von selbst.

<!-- tutorial-screenshot
write-file: lichtspur.c <- previous-code
terminal-open
terminal-run: make PROGRAM=lichtspur
terminal-wait-for-prompt
terminal-expect-no-text: error:
terminal-expect-no-text: ***
tab: preview
variants: crt
viewport: 800x450
hook: open lichtspur.html
# Die Spur fährt los, sobald die Seite geladen ist: neu laden, dann gleich lenken.
preview-reload
hook: play 1 ArrowUp 0.4 ArrowLeft 0.4 ArrowDown 1
hook: freeze
crop-element: #canvas
-->
<img class='full' src='lichtspur-fahren.webp' alt='Eine türkise Linie fährt nach rechts, macht eine Schleife und kreuzt dabei ihre eigene Spur.'>

Noch kannst du deine eigene Spur kreuzen und aus dem Bild fahren. Das ändert sich jetzt.

### Wände und Zusammenstöße

Was auf dem Bildschirm steht, ist Speicher, und Speicher kann man lesen: `get_pixel(x, y)` gibt die Farbnummer eines Pixels zurück. Ist das Feld, auf das die Spur fahren will, nicht mehr 0 (der Hintergrund), dann steht dort eine Wand oder eine Spur, und es kracht. Du brauchst keine Liste der Linien und keine Rechnung mit Rechtecken: Der Bildschirm selbst ist das Spielfeld.

Zeichne vor der Schleife einen Rahmen in Farbe 14:

```c
for (int x = 0; x < W; x++)
{
    set_pixel(x, 0, 14);
    set_pixel(x, H - 1, 14);
}
for (int y = 0; y < H; y++)
{
    set_pixel(0, y, 14);
    set_pixel(W - 1, y, 14);
}
```

Und prüfe nach jedem Schritt, bevor das neue Pixel gesetzt wird:

```c
x += dx;
y += dy;
if (get_pixel(x, y) != 0)
    crashed = true;

set_pixel(x, y, crashed ? 2 : 11);
```

Dazu gehören eine Variable `bool crashed = false;` vor der Schleife und ein `if (!crashed)` um alles, was in der Schleife lenkt und fährt, denn nach dem Zusammenstoß soll die Spur stehen bleiben. Die Unfallstelle wird rot (Farbe 2).

<div class='hint think'>
<strong>Zum Nachdenken:</strong> Warum musst du zuerst mit <code>get_pixel</code> nachsehen und erst danach <code>set_pixel</code> aufrufen? Und was passiert, wenn du direkt in die Gegenrichtung lenkst?
</div>

### Zu zweit

Für zwei Spuren bräuchtest du alle Variablen doppelt: `x1`, `y1`, `dx1`, `x2`, … Übersichtlicher ist ein `struct`, das alles zusammenfasst, was zu einer Spur gehört. Funktionen wie `drive` bekommen dann einen Zeiger auf eine Spur, `Player *p`, und erreichen ihre Teile mit `p->x`. So arbeitet derselbe Code für beide. Hier das ganze Spiel:

```c
#include "pixelram.h"

#define W 160
#define H 90
#define WALL 14
#define CRASH 12

typedef struct
{
    int x, y;      // Position
    int dx, dy;    // Richtung
    int color;
    bool crashed;
    int wins;
} Player;

void draw_field(Player *a, Player *b)
{
    clear(0);
    for (int x = 0; x < W; x++)
    {
        set_pixel(x, 0, WALL);
        set_pixel(x, H - 1, WALL);
    }
    for (int y = 0; y < H; y++)
    {
        set_pixel(0, y, WALL);
        set_pixel(W - 1, y, WALL);
    }

    // Siege als Punkte in der oberen Wand: links a, rechts b
    for (int i = 0; i < a->wins; i++)
        set_pixel(2 + 2 * i, 0, a->color);
    for (int i = 0; i < b->wins; i++)
        set_pixel(W - 3 - 2 * i, 0, b->color);
}

void start(Player *p, int x, int dx)
{
    p->x = x;
    p->y = H / 2;
    p->dx = dx;
    p->dy = 0;
    p->crashed = false;
}

void steer(Player *p, pixel_key up, pixel_key down, pixel_key left, pixel_key right)
{
    // Umdrehen geht nicht: nur abbiegen
    if (key_pressed(up)    && p->dy == 0) { p->dx = 0;  p->dy = -1; }
    if (key_pressed(down)  && p->dy == 0) { p->dx = 0;  p->dy = 1;  }
    if (key_pressed(left)  && p->dx == 0) { p->dx = -1; p->dy = 0;  }
    if (key_pressed(right) && p->dx == 0) { p->dx = 1;  p->dy = 0;  }
}

void drive(Player *p)
{
    p->x += p->dx;
    p->y += p->dy;
    if (get_pixel(p->x, p->y) != 0)
        p->crashed = true;
}

int main(void)
{
    screen_open(W, H, pixel_indexed8, "Lichtspur");
    use_palette("sweetie_16");
    set_target_fps(30);

    Player a = { .color = 3 };    // Orange, fährt mit WASD
    Player b = { .color = 11 };   // Türkis, fährt mit den Pfeiltasten

    bool running = false;   // Die Leertaste startet jede Runde.
    draw_field(&a, &b);

    while (!should_close())
    {
        if (running)
        {
            steer(&a, pixel_key_w, pixel_key_s, pixel_key_a, pixel_key_d);
            steer(&b, pixel_key_up, pixel_key_down, pixel_key_left, pixel_key_right);

            drive(&a);
            drive(&b);
            if (a.x == b.x && a.y == b.y)   // frontal ineinander
                a.crashed = b.crashed = true;

            set_pixel(a.x, a.y, a.crashed ? CRASH : a.color);
            set_pixel(b.x, b.y, b.crashed ? CRASH : b.color);

            if (a.crashed || b.crashed)
            {
                running = false;
                if (!a.crashed) a.wins++;
                if (!b.crashed) b.wins++;
            }
        }
        else if (key_pressed(pixel_key_space))
        {
            draw_field(&a, &b);
            start(&a, 20, 1);
            start(&b, W - 21, -1);
            running = true;
        }

        present();
    }

    screen_close();
    return 0;
}
```

<!-- tutorial-screenshot
write-file: lichtspur.c <- previous-code
terminal-open
terminal-run: make PROGRAM=lichtspur
terminal-wait-for-prompt
terminal-expect-no-text: error:
terminal-expect-no-text: ***
tab: preview
variants: crt
viewport: 800x450
hook: open lichtspur.html
# Runde 1: Türkis fährt in die Wand. Runde 2: beide kurven, Orange fährt in die türkise Spur.
hook: play Space 0.5 ArrowUp 2.5 Space 0.3 KeyW ArrowDown 0.8 KeyD ArrowLeft 0.9 KeyS ArrowUp 0.6 KeyD ArrowLeft 0.8 KeyS ArrowDown 0.7 KeyA 0.6 ArrowRight 0.5 KeyS ArrowUp 3
crop-element: #canvas
-->
<img class='full' src='lichtspur.webp' alt='Zweite Runde: Eine orange und eine türkise Spur haben sich durchs Spielfeld gewunden, bis eine verunglückt ist; die Unfallstelle leuchtet weiß. Links in der oberen Wand steht ein orangefarbener Siegpunkt.'>

Mit der <kbd>Leertaste</kbd> beginnt eine Runde. Beide Spuren fahren im selben Bild einen Schritt, und erst danach wird gezeichnet. Deshalb ist es gerecht, wenn beide gleichzeitig verunglücken: Dann gewinnt niemand. Die Punkte stehen als Pixel in der oberen Wand.

<div class='hint task'>
<strong>Bau das Spiel aus.</strong> Ein paar Ideen, von leicht bis knifflig:
<ul>
<li><strong>Schneller werden:</strong> Erhöhe mit <code>set_target_fps</code> das Tempo, je länger eine Runde dauert.</li>
<li><strong>Hindernisse:</strong> Setze in <code>draw_field</code> ein paar Blöcke in Wandfarbe ins Feld, mit <code>rand()</code> aus <code>&lt;stdlib.h&gt;</code> jedes Mal woanders.</li>
<li><strong>Lücken:</strong> Lass jede Spur ab und zu ein paar Pixel nicht zeichnen. Durch die Lücken kann man hindurchfahren.</li>
<li><strong>Computergegner:</strong> Spur b lenkt selbst. Sie schaut mit <code>get_pixel(b.x + b.dx, b.y + b.dy)</code> ein Feld voraus und biegt ab, wenn dort etwas ist. Wie kann sie auch weiter vorausschauen?</li>
</ul>
</div>

Die Maus fragst du mit `mouse_position(&x, &y)` und `mouse_button_down(pixel_mouse_left)` ab, alle weiteren Tasten heißen wie `pixel_key_a` oder `pixel_key_enter`. Die ganze Liste steht in der [Dokumentation](https://specht.github.io/pixelram/input.html).

## Mehr PixelRAM

### Beispiele im Starter

Im Ordner `beispiele` der Vorlage liegen fertige Programme. Jedes übersetzt du mit `make PROGRAM=…` und öffnest die passende HTML-Datei:

```bash
make PROGRAM=beispiele/tunnel
```

<div class='gallery'>
<figure>
<!-- tutorial-screenshot
terminal-open
terminal-run: make PROGRAM=beispiele/tunnel
terminal-wait-for-prompt
terminal-expect-no-text: error:
terminal-expect-no-text: ***
tab: preview
variants: crt
viewport: 800x450
hook: open beispiele/tunnel.html
hook: at 2.5
crop-element: #canvas
-->
<img src='tunnel.webp' alt='Strahlenförmige bunte Segmente laufen auf eine dunkle Tunnelöffnung zu.'>
<figcaption><code>beispiele/tunnel</code>: Abstand und Winkel jedes Pixels zur Mitte ergeben einen Flug durch einen Tunnel.</figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
terminal-open
terminal-run: make PROGRAM=beispiele/julia
terminal-wait-for-prompt
terminal-expect-no-text: error:
terminal-expect-no-text: ***
tab: preview
variants: crt
viewport: 800x450
hook: open beispiele/julia.html
hook: at 6
crop-element: #canvas
-->
<img src='julia.webp' alt='Ein Julia-Fraktal mit farbigen Bändern und Spiralformen.'>
<figcaption><code>beispiele/julia</code>: Für jedes Pixel immer wieder dieselbe kleine Rechnung: ein Fraktal, das sich langsam verwandelt.</figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
terminal-open
terminal-run: make PROGRAM=beispiele/magie
terminal-wait-for-prompt
terminal-expect-no-text: error:
terminal-expect-no-text: ***
tab: preview
variants: crt
viewport: 800x450
hook: open beispiele/magie.html
hook: at 1.2
crop-element: #canvas
-->
<img src='magic.webp' alt='Violette und türkise Ringe eines Zauberkreises leuchten auf dunklem Grund.'>
<figcaption><code>beispiele/magie</code>: Der Kreis wird nur einmal gezeichnet. Bewegung bringt allein die Palette, die sich in der Schleife dreht.</figcaption>
</figure>
</div>

Dazu kommen `beispiele/feuer`, ein Feuer, in dem jedes Pixel die Hitze seiner Nachbarn darunter aufnimmt, und `beispiele/sterne`, ein Flug durch 400 Sterne in 3D. Kopiere ein Beispiel neben `main.c` und verändere es: Das ist oft der schnellste Weg zu einer eigenen Idee.

<div class='hint info'>
Der Palettentrick aus <code>beispiele/magie</code> heißt <strong>Color Cycling</strong>. Der Künstler Mark Ferrari hat damit ganze Landschaften mit Regen, Schnee und Wasser zum Leben erweckt, ohne ein einziges Pixel zu verändern. <a href='https://www.effectgames.com/demos/canvascycle/' target='_blank'>Hier kannst du seine Bilder ansehen.</a>
</div>

### Ganze Spiele

PixelRAM ist klein, aber kein Spielzeug. Dieselben wenigen Befehle, die du kennst, tragen auch Spiele aus den Neunzigerjahren, die damals ganz ähnlich programmiert wurden: Ein Programm schreibt Pixel in den Speicher. Sogar DOOM läuft auf PixelRAM. **Crystal Caves** (1991) ist ein Jump-and-Run, dessen erste Episode frei verteilt werden darf. Probier es aus:

```bash
make crystal
```

Das lädt PixelRAM mit seinen Spiele-Anpassungen und den Quelltext von OpenCrystalCaves, einer freien Neuprogrammierung des Spiels, und übersetzt alles. Das dauert ein, zwei Minuten. Dann öffnest du `spiele/crystal.html` in der Vorschau. Du läufst mit den Pfeiltasten, springst mit <kbd>Z</kbd> und schießt mit <kbd>X</kbd>.

<!-- tutorial-screenshot
# Crystal Caves: übersetzen, öffnen, das Intro »Apogee presents« (die Enter
# kommen im Generator noch im Intro an, das Spiel läuft dort mit 15 fps);
# auf 16:9 geschnitten wie die beiden Bilder oben neben ihm
terminal-open
terminal-run: make crystal
terminal-wait-for-prompt: 600
terminal-expect-no-text: error:
terminal-expect-no-text: ***
tab: preview
variants: crt
viewport: 800x450
hook: open spiele/crystal.html
click: selector:#canvas
sleep: 3
press: Enter
sleep: 2
press: Enter
sleep: 4
hook: aspect 16 9
move-mouse: selector:body
crop-element: #canvas
-->
<img class='full' src='crystal-caves.webp' alt='Das Intro von Crystal Caves: »Apogee presents« in roter Schrift vor einem Sternenhimmel, unten ein gelber Strahlenkranz.'>

Die Grafik des Spiels kommt am Ende als Pixel in genau dem Speicher an, den du mit `set_pixel` beschreibst, und die Tasten kommen über `key_down` und `poll_key_event` herein. Wie das Spiel an PixelRAM angeschlossen ist, steht in [`ports/crystal`](https://github.com/specht/pixelram/tree/main/ports/crystal) im Repository von PixelRAM. Wer die Originaldateien besitzt, kann auf dieselbe Weise auch [**Prince of Persia**](https://github.com/specht/pixelram/tree/main/ports/prince) und [**Descent**](https://github.com/specht/pixelram/tree/main/ports/descent) übersetzen.

## Wie geht es weiter?

Auf dieser Seite hast du keine einzige fertige Form gezeichnet. Aus einer Formel wurde ein Bild, aus einer Welle eine Animation und aus einem gelesenen Pixel ein Spiel. Dieselbe Frage trägt noch viel weiter, bis zu eigener 3D-Grafik:
**Welche Farbe soll dieser Pixel haben?**

Die ganze Bibliothek mit allen Befehlen ist unter [specht.github.io/pixelram](https://specht.github.io/pixelram/) beschrieben.
