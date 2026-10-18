<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# PixelRAM

<p class='abstract'>
Alle Befehle von PixelRAM zum Nachschlagen: den Bildschirm öffnen, Pixel setzen und lesen, Paletten, Farben ohne Palette, der Speicher hinter dem Bild, Tastatur, Maus und Zeit. PixelRAM hat mit Absicht keine Befehle für Linien, Kreise oder Figuren: Alles, was du siehst, besteht aus einzelnen Pixeln, die dein Programm setzt.
</p>

## Grundlagen

<!-- part: uebersetzen -->
<!-- short: `make` macht aus `main.c` die Datei `main.html`, `make PROGRAM=spiel` aus `spiel.c` die Datei `spiel.html`. -->
### Übersetzen

In einem Ordner mit der PixelRAM-Vorlage übersetzt `make` die Datei `main.c` in die Webseite `main.html`, die du mit »Go Live« in der Vorschau öffnest. Beim ersten Mal lädt `make` die Bibliothek PixelRAM in den versteckten Ordner `.pixelram`.

```bash
make                   # main.c → main.html
make PROGRAM=spiel     # spiel.c → spiel.html
make PROGRAM=beispiele/feuer
```

- Findet `make` einen Fehler, nennt es Datei und Zeile, und die alte Webseite bleibt, wie sie war. Lies die erste Fehlermeldung zuerst.
- Was dein Programm mit `printf` ausgibt, steht in der Konsole der Seite: Fahre mit der Maus über das Bild und klicke unten rechts auf das Terminal-Symbol oder drücke <kbd>&#96;</kbd>.
- Dort sind auch der CRT-Filter (<kbd>F6</kbd>), der Scanlines wie auf einem alten Röhrenbildschirm zeigt, und das Vollbild (<kbd>F11</kbd>).
<!-- /part -->

<!-- part: bildschirm -->
<!-- short: `screen_open(b, h, pixel_indexed8, "Titel")`, dann `while (!should_close())` mit `present()`, am Ende `screen_close()`. -->
### Bildschirm und Schleife

```c
#include "pixelram.h"

int main(void)
{
    if (!screen_open(320, 180, pixel_indexed8, "Mein Programm"))
        return 1;

    while (!should_close())
    {
        clear(0);
        // … Pixel setzen …
        present();          // das fertige Bild zeigen
    }

    screen_close();
    return 0;
}
```

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>screen_open(breite, hoehe, modus, titel)</code></td><td>Öffnet den Bildschirm mit so vielen Pixeln; <code>false</code>, wenn es nicht klappt</td></tr>
<tr><td><code>screen_width()</code>, <code>screen_height()</code></td><td>Breite und Höhe in Pixeln</td></tr>
<tr><td><code>should_close()</code></td><td>Wahr, wenn das Programm enden soll</td></tr>
<tr><td><code>present()</code></td><td>Zeigt das fertige Bild und wartet auf das nächste; einmal pro Durchlauf der Schleife</td></tr>
<tr><td><code>set_target_fps(n)</code></td><td>Höchstens <code>n</code> Bilder pro Sekunde, ohne Angabe 60; 0 schaltet die Grenze ab</td></tr>
<tr><td><code>set_title(titel)</code></td><td>Ändert den Titel</td></tr>
<tr><td><code>set_pixel_aspect(f)</code></td><td>Pixel breiter (größer als 1) oder höher (kleiner als 1) als quadratisch</td></tr>
<tr><td><code>set_fullscreen(true)</code></td><td>Vollbild, im Browser nach dem nächsten Klick</td></tr>
<tr><td><code>screen_close()</code></td><td>Schließt den Bildschirm am Ende</td></tr>
</tbody>
</table>
</div>

Ein Programm ohne Schleife, das nur ein Bild malt, braucht im Browser kein `present()`: Die Seite zeigt das Bild dann von selbst.
<!-- /part -->

## Pixel und Farben

<!-- part: pixel -->
<!-- short: `set_pixel(x, y, farbe)` und `get_pixel(x, y)`; (0, 0) ist oben links, `clear(farbe)` füllt alles. -->
### Pixel setzen und lesen

Der Pixel (0, 0) ist oben links, x wächst nach rechts, y nach **unten**. Im Modus `pixel_indexed8` ist die Farbe eines Pixels eine Zahl von 0 bis 255, eine Nummer in der Palette.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>set_pixel(x, y, farbe)</code></td><td>Setzt einen Pixel; außerhalb des Bildes passiert nichts</td></tr>
<tr><td><code>get_pixel(x, y)</code></td><td>Die Farbe eines Pixels; außerhalb des Bildes 0</td></tr>
<tr><td><code>clear(farbe)</code></td><td>Füllt das ganze Bild mit einer Farbe</td></tr>
</tbody>
</table>
</div>

Weil man Pixel auch lesen kann, ist das Bild selbst ein Speicher: Ein Spiel kann nachsehen, ob an einer Stelle schon etwas steht, und ein Feuer kann seine Farbe aus den Pixeln darunter ausrechnen.

```c
for (int y = 0; y < screen_height(); y++)
    for (int x = 0; x < screen_width(); x++)
        set_pixel(x, y, (x ^ y) % 16);
```
<!-- /part -->

<!-- part: paletten -->
<!-- short: `use_palette("sweetie_16")` wählt eine fertige Palette, `set_palette(nummer, r, g, b)` ändert eine Farbe. -->
### Paletten

Die Palette sagt, welche Farbe zu jeder Nummer gehört. Ändert man eine Farbe der Palette, ändern sich sofort alle Pixel mit dieser Nummer, ohne dass ein einziger Pixel neu gesetzt wird.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>use_palette("pico_8")</code></td><td>Wählt eine fertige Palette; ohne Angabe gilt <code>vga</code></td></tr>
<tr><td><code>set_palette(nummer, r, g, b)</code></td><td>Gibt einer Nummer eine Farbe, Rot, Grün und Blau von 0 bis 255</td></tr>
<tr><td><code>get_palette(nummer, &amp;r, &amp;g, &amp;b)</code></td><td>Liest eine Farbe der Palette</td></tr>
<tr><td><code>palette_count()</code>, <code>palette_name(i)</code></td><td>Wie viele fertige Paletten es gibt und wie sie heißen</td></tr>
</tbody>
</table>
</div>

PixelRAM kennt dieselben 88 Paletten wie Pixelflow Canvas, zum Beispiel `vga`, `pico_8`, `sweetie_16`, `endesga_32`, `zughy_32` und `aap_64`. Alle stehen in der [Dokumentation von PixelRAM](https://specht.github.io/pixelram/).

Ein Farbverlauf in der Palette macht aus Nummern Helligkeiten, etwa für Feuer: Nummer 0 ist Schwarz, 63 ist Weiß, dazwischen Rot und Gelb.

```c
for (int i = 0; i < 16; i++) {
    set_palette(i,      i * 8,       0,           0);
    set_palette(i + 16, i * 8 + 128, i * 8,       0);
    set_palette(i + 32, 255,         i * 8 + 128, i * 8);
    set_palette(i + 48, 255,         255,         i * 8 + 128);
}
```
<!-- /part -->

<!-- part: farben-direkt -->
<!-- short: Mit `pixel_rgb24` statt Palette: `set_pixel_rgb(x, y, r, g, b)`, `clear_rgb(r, g, b)`. -->
### Farben ohne Palette

In den anderen Modi steht in jedem Pixel gleich die Farbe:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Modus</th><th>Bytes pro Pixel</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>pixel_indexed8</code></td><td>1</td><td>Nummer in der Palette</td></tr>
<tr><td><code>pixel_rgb565</code></td><td>2</td><td>Farbe mit 5 Bit Rot, 6 Bit Grün, 5 Bit Blau</td></tr>
<tr><td><code>pixel_rgb24</code></td><td>3</td><td>Rot, Grün und Blau mit je 8 Bit</td></tr>
<tr><td><code>pixel_rgba32</code></td><td>4</td><td>Dazu 8 Bit Deckkraft</td></tr>
</tbody>
</table>
</div>

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>set_pixel_rgb(x, y, r, g, b)</code></td><td>Setzt einen Pixel in einer Farbe</td></tr>
<tr><td><code>get_pixel_rgb(x, y, &amp;r, &amp;g, &amp;b)</code></td><td>Liest die Farbe; geht in jedem Modus, auch mit Palette</td></tr>
<tr><td><code>clear_rgb(r, g, b)</code></td><td>Füllt das ganze Bild</td></tr>
<tr><td><code>screen_set_mode(breite, hoehe, modus)</code></td><td>Wechselt Größe und Modus, während das Programm läuft</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

<!-- part: speicher -->
<!-- short: `framebuffer()` ist der Speicher hinter dem Bild; Zeile `y` beginnt bei `y * screen_pitch()`. -->
### Der Speicher hinter dem Bild

`set_pixel` schreibt in einen Speicher, den die Grafik am Ende zeigt. Mit `framebuffer()` kommt man direkt an ihn heran, so wie Spiele aus den Neunzigerjahren:

```c
uint8_t *pixel = framebuffer();
pixel[y * screen_pitch() + x] = 12;   // dasselbe wie set_pixel(x, y, 12)
```

`screen_pitch()` ist die Länge einer Zeile in Bytes, `framebuffer_size()` die Größe des ganzen Speichers. Im Modus `pixel_indexed8` ist jedes Byte ein Pixel. Direkt in den Speicher zu schreiben ist schneller, aber nichts passt mehr auf: Wer über den Rand schreibt, zerstört andere Daten.
<!-- /part -->

## Eingabe und Zeit

<!-- part: tastatur -->
<!-- short: `key_down(pixel_key_left)` solange gedrückt, `key_pressed(pixel_key_space)` nur im ersten Bild. -->
### Tastatur

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Wahr, …</th></tr></thead>
<tbody>
<tr><td><code>key_down(pixel_key_w)</code></td><td>solange die Taste gedrückt ist; für Bewegung</td></tr>
<tr><td><code>key_pressed(pixel_key_space)</code></td><td>nur in dem Bild, in dem sie heruntergedrückt wurde</td></tr>
<tr><td><code>key_released(pixel_key_e)</code></td><td>nur in dem Bild, in dem sie losgelassen wurde</td></tr>
<tr><td><code>poll_key_event(&amp;ereignis)</code></td><td>wenn noch ein Tastendruck in der Queue steht; <code>ereignis.key</code> und <code>ereignis.pressed</code> sagen welcher</td></tr>
</tbody>
</table>
</div>

Die Tasten heißen `pixel_key_a` bis `pixel_key_z`, `pixel_key_0` bis `pixel_key_9`, `pixel_key_up`, `pixel_key_down`, `pixel_key_left`, `pixel_key_right`, `pixel_key_space`, `pixel_key_enter`, `pixel_key_escape`, `pixel_key_shift`, `pixel_key_ctrl` und `pixel_key_f1` bis `pixel_key_f12`. Buchstaben und Ziffern haben die Nummer ihres Zeichens: `pixel_key_a` ist `'a'`.

Im Browser bekommt das Programm die Tasten erst, wenn du einmal ins Bild geklickt hast.
<!-- /part -->

<!-- part: maus -->
<!-- short: `mouse_position(&x, &y)` in Pixeln des Bildes, `mouse_button_pressed(pixel_mouse_left)`. -->
### Maus

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>mouse_position(&amp;x, &amp;y)</code></td><td>Wo die Maus steht, in Pixeln des Bildes</td></tr>
<tr><td><code>mouse_delta(&amp;dx, &amp;dy)</code></td><td>Wie weit sie sich seit dem letzten Bild bewegt hat</td></tr>
<tr><td><code>mouse_button_down(pixel_mouse_left)</code></td><td>Wahr, solange die Taste gedrückt ist; auch <code>pixel_mouse_right</code>, <code>pixel_mouse_middle</code></td></tr>
<tr><td><code>mouse_button_pressed(…)</code>, <code>mouse_button_released(…)</code></td><td>Wahr im Bild des Klicks oder des Loslassens</td></tr>
<tr><td><code>set_mouse_relative(true)</code></td><td>Fängt die Maus: Der Zeiger verschwindet, <code>mouse_delta</code> zählt jede Bewegung; im Browser nach dem nächsten Klick</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

<!-- part: zeit -->
<!-- short: `seconds()` seit dem Start; Bewegung = Geschwindigkeit × (jetzt − vorher). -->
### Zeit

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>seconds()</code></td><td>Sekunden seit dem Start, mit Nachkommastellen</td></tr>
<tr><td><code>ticks_ms()</code></td><td>Millisekunden seit dem Start, als ganze Zahl</td></tr>
<tr><td><code>sleep_ms(ms)</code></td><td>Wartet so viele Millisekunden</td></tr>
</tbody>
</table>
</div>

Damit eine Bewegung auf jedem Computer gleich schnell ist, rechnet man mit der Zeit seit dem letzten Bild:

```c
double vorher = seconds();
while (!should_close())
{
    double jetzt = seconds();
    double dt = jetzt - vorher;
    vorher = jetzt;
    x += 40 * dt;          // 40 Pixel pro Sekunde
    // …
    present();
}
```
<!-- /part -->
