<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# raylib

<p class='abstract'>
Die wichtigsten Befehle von raylib zum Nachschlagen: Fenster und Spielschleife, Kamera und Formen in 3D, Farben, Tastatur und Maus, Zeit, Vektoren, Zusammenstöße, eigene Flächen, Texturen und Shader für Licht. Die Beispiele sind in C und passen zur raylib-Vorlage, die im Workspace zu einer Webseite übersetzt wird.
</p>

_person(santamaria)

## Grundlagen

<!-- part: uebersetzen -->
<!-- short: `make` macht aus `main.c` die Datei `main.html`, `make PROGRAM=test` aus `test.c` die Datei `test.html`. -->
### Übersetzen

raylib ist im Workspace schon installiert. In einem Ordner mit der raylib-Vorlage übersetzt `make` die Datei `main.c` in die Webseite `main.html`, die du mit »Go Live« in der Vorschau öffnest:

```bash
make                  # main.c → main.html
make PROGRAM=test     # test.c → test.html
```

- Findet `make` einen Fehler, nennt es Datei und Zeile, und die alte Webseite bleibt, wie sie war. Lies die erste Fehlermeldung zuerst.
- Was dein Programm mit `printf` ausgibt, steht in der Konsole der Seite: Fahre mit der Maus über das Bild und klicke unten rechts auf das Terminal-Symbol.
- Dateien, die das Programm lesen soll, wie die Shader `light.vs` und `light.fs`, bettet das `Makefile` mit `--embed-file` in die Seite ein. Eine weitere Datei schreibst du in die Zeile mit den anderen.

Ohne Vorlage geht es auch mit einem einzigen Befehl:

```bash
emcc main.c -o main.html -lraylib -sUSE_GLFW=3 -sASYNCIFY -sSINGLE_FILE
```
<!-- /part -->

<!-- part: spielschleife -->
<!-- short: `InitWindow`, dann `while (!WindowShouldClose())` mit `BeginDrawing` … `EndDrawing`, am Ende `CloseWindow`. -->
### Fenster und Spielschleife

Ein raylib-Programm öffnet ein Fenster und zeichnet dann in einer Schleife ein Bild nach dem anderen:

```c
#include <raylib.h>

int main(void)
{
    InitWindow(960, 540, "Mein Spiel");   // Breite, Höhe, Titel
    SetTargetFPS(60);                      // höchstens 60 Bilder pro Sekunde

    while (!WindowShouldClose())
    {
        // 1. Eingaben lesen, Spiel weiterrechnen
        BeginDrawing();
        ClearBackground(RAYWHITE);
        // 2. zeichnen
        EndDrawing();
    }

    CloseWindow();
    return 0;
}
```

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>InitWindow(width, height, title)</code></td><td>Öffnet das Fenster, im Browser die Zeichenfläche</td></tr>
<tr><td><code>SetTargetFPS(n)</code></td><td>Bremst die Schleife auf höchstens <code>n</code> Bilder pro Sekunde</td></tr>
<tr><td><code>WindowShouldClose()</code></td><td>Wahr, wenn das Programm enden soll (Fenster zu oder <kbd>Esc</kbd>)</td></tr>
<tr><td><code>SetExitKey(KEY_NULL)</code></td><td><kbd>Esc</kbd> beendet das Programm nicht mehr</td></tr>
<tr><td><code>BeginDrawing()</code>, <code>EndDrawing()</code></td><td>Rahmen um alles, was in einem Bild gezeichnet wird</td></tr>
<tr><td><code>ClearBackground(color)</code></td><td>Füllt das ganze Bild mit einer Farbe</td></tr>
<tr><td><code>GetScreenWidth()</code>, <code>GetScreenHeight()</code></td><td>Größe des Bildes in Pixeln</td></tr>
<tr><td><code>CloseWindow()</code></td><td>Schließt das Fenster am Ende</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

<!-- part: zeichnen-2d -->
<!-- short: `DrawText(text, x, y, fontSize, color)`, `DrawRectangle(x, y, width, height, color)`; (0, 0) ist oben links. -->
### Zeichnen in 2D

In 2D ist (0, 0) die Ecke oben links, x wächst nach rechts und y nach **unten**, gemessen in Pixeln. Was nach `EndMode3D()` gezeichnet wird, liegt über dem 3D-Bild, etwa ein Fadenkreuz oder eine Anzeige.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>DrawText(text, x, y, fontSize, color)</code></td><td>Text, <code>fontSize</code> ist die Höhe in Pixeln</td></tr>
<tr><td><code>TextFormat("%d Punkte", score)</code></td><td>Text mit Werten darin, wie <code>printf</code></td></tr>
<tr><td><code>MeasureText(text, fontSize)</code></td><td>Breite eines Textes in Pixeln, zum Zentrieren</td></tr>
<tr><td><code>DrawRectangle(x, y, width, height, color)</code></td><td>Gefülltes Rechteck</td></tr>
<tr><td><code>DrawRectangleLinesEx((Rectangle){ x, y, width, height }, thickness, color)</code></td><td>Rahmen eines Rechtecks</td></tr>
<tr><td><code>DrawCircle(x, y, radius, color)</code></td><td>Gefüllter Kreis</td></tr>
<tr><td><code>DrawLine(x1, y1, x2, y2, color)</code></td><td>Linie</td></tr>
<tr><td><code>DrawFPS(x, y)</code></td><td>Zeigt die Bilder pro Sekunde</td></tr>
</tbody>
</table>
</div>

```c
const char *text = TextFormat("Zeit: %.1f s", GetTime());
DrawText(text, (GetScreenWidth() - MeasureText(text, 20)) / 2, 10, 20, WHITE);
```
<!-- /part -->

## 3D

<!-- part: koordinaten -->
<!-- short: x nach rechts, y nach oben, z nach vorne; `(Vector3){ x, y, z }`. -->
### Koordinaten

Ein Punkt im Raum hat drei Koordinaten: **x** nach rechts, **y** nach oben, **z** nach vorne, zum Betrachter hin, wenn man von vorne auf die Welt schaut.

<img class='drawing' style='width: 26em' src='axes.webp' alt='Drei Pfeile von einem Punkt aus: x rot nach rechts, y grün nach oben, z blau schräg nach vorne auf den Betrachter zu; darunter ein Gitter, das flach in x und z auf dem Boden liegt.'>

Positionen und Richtungen sind Werte vom Typ `Vector3`:

```c
Vector3 p = { 1, 0.5f, -2 };
p.y += 1;                                  // eins höher
DrawCube((Vector3){ 0, 0.5f, 0 }, 1, 1, 1, RED);
```

`Vector2` hat nur `x` und `y`, etwa für die Maus.
<!-- /part -->

<!-- part: kamera -->
<!-- short: `Camera3D` mit `position`, `target`, `up`, `fovy`; gezeichnet wird zwischen `BeginMode3D(camera)` und `EndMode3D()`. -->
### Kamera

```c
Camera3D camera = { 0 };
camera.position = (Vector3){ 6, 5, 8 };    // wo die Kamera steht
camera.target = (Vector3){ 0, 0, 0 };      // welchen Punkt sie anschaut
camera.up = (Vector3){ 0, 1, 0 };          // welche Richtung oben ist
camera.fovy = 45;                          // Blickwinkel nach oben und unten, in Grad
camera.projection = CAMERA_PERSPECTIVE;

BeginMode3D(camera);
DrawGrid(10, 1);
EndMode3D();
```

- Mit `CAMERA_ORTHOGRAPHIC` statt `CAMERA_PERSPECTIVE` werden entfernte Dinge nicht kleiner; `fovy` ist dann die Höhe des sichtbaren Bereichs in Einheiten.
- Schaut die Kamera genau in Richtung `up` (senkrecht nach unten oder oben), weiß raylib nicht, wie das Bild gedreht sein soll. Ein kleiner Versatz hilft: `position` = `(0, 12, 0.1f)`.

`UpdateCamera(&camera, mode)` bewegt die Kamera selbst, einmal pro Bild aufgerufen:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Modus</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>CAMERA_ORBITAL</code></td><td>Kreist langsam um <code>target</code></td></tr>
<tr><td><code>CAMERA_FREE</code></td><td>Freier Flug mit Maus und Tasten</td></tr>
<tr><td><code>CAMERA_FIRST_PERSON</code></td><td>Laufen in Augenhöhe mit <kbd>W</kbd><kbd>A</kbd><kbd>S</kbd><kbd>D</kbd> und Maus</td></tr>
<tr><td><code>CAMERA_THIRD_PERSON</code></td><td>Wie oben, die Kamera schaut von hinten auf die Figur</td></tr>
</tbody>
</table>
</div>

Für ein eigenes Spiel setzt du `position` und `target` meist selbst, zum Beispiel aus den Augen der Figur und ihrer Blickrichtung (siehe [Vektoren](#vektoren)).
<!-- /part -->

<!-- part: formen-3d -->
<!-- short: `DrawCube(position, width, height, length, color)`, `DrawSphere(center, radius, color)`, `DrawPlane`, `DrawGrid`; `…Wires` für Kanten. -->
### Formen

Alle Formen werden zwischen `BeginMode3D` und `EndMode3D` gezeichnet. Positionen sind die **Mitte** der Form.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>DrawCube(position, width, height, length, color)</code></td><td>Quader</td></tr>
<tr><td><code>DrawCubeV(position, size, color)</code></td><td>Quader, Größe als <code>Vector3</code></td></tr>
<tr><td><code>DrawCubeWires(position, width, height, length, color)</code></td><td>Nur die Kanten</td></tr>
<tr><td><code>DrawSphere(center, radius, color)</code></td><td>Kugel</td></tr>
<tr><td><code>DrawSphereWires(center, radius, rings, slices, color)</code></td><td>Kugel als Gitter</td></tr>
<tr><td><code>DrawCylinder(position, radiusTop, radiusBottom, height, slices, color)</code></td><td>Zylinder, mit Radius 0 oben ein Kegel</td></tr>
<tr><td><code>DrawCapsule(startPos, endPos, radius, slices, rings, color)</code></td><td>Kapsel zwischen zwei Punkten</td></tr>
<tr><td><code>DrawPlane(center, (Vector2){ width, length }, color)</code></td><td>Flache Platte auf dem Boden</td></tr>
<tr><td><code>DrawLine3D(startPos, endPos, color)</code></td><td>Linie</td></tr>
<tr><td><code>DrawGrid(slices, spacing)</code></td><td>Gitter auf dem Boden um (0, 0, 0)</td></tr>
</tbody>
</table>
</div>

Die Formen haben kein Licht: Alle Seiten eines Würfels haben dieselbe Farbe. Für Licht brauchst du [eigene Flächen](#eigene-flaechen) und einen [Shader](#shader).
<!-- /part -->

<!-- part: farben -->
<!-- short: Namen wie `RED` und `SKYBLUE` oder `(Color){ r, g, b, a }` von 0 bis 255; `Fade` macht durchsichtig. -->
### Farben

Eine Farbe ist ein `Color` mit Rot, Grün, Blau und Deckkraft, jeweils von 0 bis 255:

```c
Color grass = { 96, 168, 64, 255 };
DrawCube(p, 1, 1, 1, (Color){ 255, 200, 80, 255 });
```

raylib kennt viele Farben mit Namen, unter anderem `WHITE`, `BLACK`, `GRAY`, `DARKGRAY`, `LIGHTGRAY`, `RED`, `MAROON`, `ORANGE`, `GOLD`, `YELLOW`, `GREEN`, `LIME`, `DARKGREEN`, `SKYBLUE`, `BLUE`, `DARKBLUE`, `PURPLE`, `VIOLET`, `PINK`, `BEIGE`, `BROWN`, `DARKBROWN` und `RAYWHITE`.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>Fade(color, 0.5f)</code></td><td>Dieselbe Farbe, halb durchsichtig</td></tr>
<tr><td><code>ColorBrightness(color, -0.2f)</code></td><td>Dunkler (negativ) oder heller (positiv), von −1 bis 1</td></tr>
<tr><td><code>ColorLerp(a, b, t)</code></td><td>Mischung: bei <code>t</code> = 0 ist es <code>a</code>, bei 1 ist es <code>b</code></td></tr>
<tr><td><code>ColorFromHSV(hue, saturation, value)</code></td><td>Farbe aus Farbton (0 bis 360) und zwei Werten von 0 bis 1</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

## Eingabe und Zeit

<!-- part: tastatur -->
<!-- short: `IsKeyDown(KEY_W)` solange gedrückt, `IsKeyPressed(KEY_SPACE)` nur im ersten Bild. -->
### Tastatur

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Wahr, …</th></tr></thead>
<tbody>
<tr><td><code>IsKeyDown(KEY_W)</code></td><td>solange die Taste gedrückt ist; für Bewegung</td></tr>
<tr><td><code>IsKeyPressed(KEY_SPACE)</code></td><td>nur in dem Bild, in dem sie heruntergedrückt wurde; für einmalige Aktionen</td></tr>
<tr><td><code>IsKeyReleased(KEY_E)</code></td><td>nur in dem Bild, in dem sie losgelassen wurde</td></tr>
</tbody>
</table>
</div>

Tasten heißen `KEY_A` bis `KEY_Z`, `KEY_ZERO` bis `KEY_NINE`, `KEY_SPACE`, `KEY_ENTER`, `KEY_ESCAPE`, `KEY_TAB`, `KEY_LEFT_SHIFT`, `KEY_LEFT_CONTROL`, `KEY_UP`, `KEY_DOWN`, `KEY_LEFT`, `KEY_RIGHT` und `KEY_F1` bis `KEY_F12`. Die Ziffern liegen in einer Reihe: `KEY_ONE + 2` ist `KEY_THREE`.

Im Browser bekommt das Programm die Tasten erst, wenn du einmal ins Bild geklickt hast.
<!-- /part -->

<!-- part: maus -->
<!-- short: `IsMouseButtonPressed(MOUSE_BUTTON_LEFT)`, `GetMouseDelta()`; `DisableCursor()` fängt die Maus. -->
### Maus

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>IsMouseButtonPressed(MOUSE_BUTTON_LEFT)</code></td><td>Wahr im Bild des Klicks; auch <code>MOUSE_BUTTON_RIGHT</code>, <code>MOUSE_BUTTON_MIDDLE</code></td></tr>
<tr><td><code>IsMouseButtonDown(…)</code></td><td>Wahr, solange die Taste gedrückt ist</td></tr>
<tr><td><code>GetMousePosition()</code></td><td>Position als <code>Vector2</code>, in Pixeln</td></tr>
<tr><td><code>GetMouseDelta()</code></td><td>Wie weit sich die Maus seit dem letzten Bild bewegt hat</td></tr>
<tr><td><code>GetMouseWheelMove()</code></td><td>Drehung des Mausrads, positiv nach oben</td></tr>
<tr><td><code>DisableCursor()</code></td><td>Fängt die Maus: Der Zeiger verschwindet, jede Bewegung zählt</td></tr>
<tr><td><code>EnableCursor()</code></td><td>Gibt die Maus wieder frei</td></tr>
<tr><td><code>IsCursorHidden()</code></td><td>Wahr, solange die Maus gefangen ist</td></tr>
</tbody>
</table>
</div>

Im Browser darf eine Seite die Maus nur nach einem Klick fangen, und <kbd>Esc</kbd> gibt sie immer frei. Ruf `DisableCursor()` deshalb beim Klick ins Bild auf:

```c
if (!IsCursorHidden() && IsMouseButtonPressed(MOUSE_BUTTON_LEFT))
    DisableCursor();
```
<!-- /part -->

<!-- part: zeit -->
<!-- short: `GetFrameTime()` ist die Zeit seit dem letzten Bild, Bewegung = Geschwindigkeit × `dt`. -->
### Zeit

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>GetFrameTime()</code></td><td>Sekunden seit dem letzten Bild, meist etwa 1/60</td></tr>
<tr><td><code>GetTime()</code></td><td>Sekunden seit dem Start des Programms</td></tr>
<tr><td><code>GetFPS()</code></td><td>Bilder pro Sekunde</td></tr>
</tbody>
</table>
</div>

Wer sich mit 4,5 Einheiten pro Sekunde bewegt, kommt in einem Bild `4.5f * dt` weit. So ist ein Spiel auf jedem Computer gleich schnell:

```c
float dt = GetFrameTime();
if (dt > 0.05f) dt = 0.05f;   // nach einer Pause keinen Riesensprung machen
x += 4.5f * dt;
```
<!-- /part -->

<!-- part: zufall -->
<!-- short: `GetRandomValue(min, max)` mit beiden Grenzen; `SetRandomSeed(n)` für immer dieselbe Folge. -->
### Zufall

`GetRandomValue(1, 6)` ist eine ganze Zahl von 1 bis 6, beide eingeschlossen. Nach `SetRandomSeed(42)` kommen bei jedem Start dieselben »Zufallszahlen«, zum Beispiel für eine Welt, die immer gleich aussehen soll.
<!-- /part -->

## Rechnen im Raum

<!-- part: vektoren -->
<!-- short: `#include <raymath.h>`: `Vector3Add`, `Vector3Scale`, `Vector3Normalize`, `Vector3Distance`, `Vector3DotProduct`. -->
### Vektoren

Die Rechenbefehle stehen in `raymath.h`:

```c
#include <raymath.h>
```

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Ergebnis</th></tr></thead>
<tbody>
<tr><td><code>Vector3Add(a, b)</code>, <code>Vector3Subtract(a, b)</code></td><td>Summe, Differenz</td></tr>
<tr><td><code>Vector3Scale(v, s)</code></td><td><code>v</code> mal eine Zahl</td></tr>
<tr><td><code>Vector3Length(v)</code></td><td>Länge</td></tr>
<tr><td><code>Vector3Normalize(v)</code></td><td>Dieselbe Richtung mit Länge 1</td></tr>
<tr><td><code>Vector3Distance(a, b)</code></td><td>Abstand zweier Punkte</td></tr>
<tr><td><code>Vector3DotProduct(a, b)</code></td><td>Skalarprodukt: für Länge 1 der Kosinus des Winkels dazwischen</td></tr>
<tr><td><code>Vector3CrossProduct(a, b)</code></td><td>Vektor, der senkrecht auf beiden steht</td></tr>
<tr><td><code>Vector3Lerp(a, b, t)</code></td><td>Punkt zwischen <code>a</code> und <code>b</code></td></tr>
<tr><td><code>Clamp(x, min, max)</code></td><td><code>x</code>, aber nicht kleiner als <code>min</code> und nicht größer als <code>max</code></td></tr>
</tbody>
</table>
</div>

Eine Blickrichtung aus zwei Winkeln im Bogenmaß, `yaw` nach links und rechts und `pitch` nach oben und unten:

```c
Vector3 look = {
    sinf(yaw) * cosf(pitch),
    sinf(pitch),
    cosf(yaw) * cosf(pitch),
};
camera.position = eyes;
camera.target = Vector3Add(eyes, look);
```

`sinf` und `cosf` kommen aus `math.h`. Eine halbe Umdrehung ist `PI`.
<!-- /part -->

<!-- part: kollision -->
<!-- short: `BoundingBox` mit `min` und `max`, `CheckCollisionBoxes`, `CheckCollisionSpheres`, `GetRayCollisionBox`. -->
### Zusammenstöße

Eine `BoundingBox` ist ein Quader mit den Achsen, beschrieben durch die Ecke mit den kleinsten (`min`) und die mit den größten Koordinaten (`max`):

```c
BoundingBox player = { { x - 0.3f, y, z - 0.3f }, { x + 0.3f, y + 1.7f, z + 0.3f } };
BoundingBox crate = { { 2, 0, 2 }, { 3, 1, 3 } };
if (CheckCollisionBoxes(player, crate)) { /* … */ }
```

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Wahr, wenn …</th></tr></thead>
<tbody>
<tr><td><code>CheckCollisionBoxes(a, b)</code></td><td>sich zwei Quader überschneiden</td></tr>
<tr><td><code>CheckCollisionSpheres(center1, radius1, center2, radius2)</code></td><td>sich zwei Kugeln berühren</td></tr>
<tr><td><code>CheckCollisionBoxSphere(box, center, radius)</code></td><td>eine Kugel den Quader berührt</td></tr>
</tbody>
</table>
</div>

Ein **Strahl** (`Ray`) hat einen Anfang und eine Richtung der Länge 1. `GetRayCollisionBox(ray, box)` sagt, ob und wo er einen Quader trifft:

```c
Ray ray = { eyes, look };
RayCollision collision = GetRayCollisionBox(ray, crate);
if (collision.hit && collision.distance < 6) { /* collision.point, collision.normal */ }
```

In einer Welt aus einem Gitter von Blöcken ist es oft einfacher, die Felder selbst zu prüfen: `floorf(x)` ist das Feld, in dem die Koordinate `x` liegt.
<!-- /part -->

## Licht

<!-- part: eigene-flaechen -->
<!-- short: `#include <rlgl.h>`: `rlBegin(RL_QUADS)`, dann je Ecke `rlColor4ub`, `rlNormal3f`, `rlVertex3f`; `rlEnd()`. -->
### Eigene Flächen

Mit `rlgl.h` schickst du der Grafikkarte die Ecken selbst. Zu jeder Ecke gehören eine Farbe und eine **Normale**, ein Vektor der Länge 1, der senkrecht aus der Fläche zeigt. Die Normale braucht ein Shader für das Licht.

```c
#include <rlgl.h>

// Die Oberseite eines Blocks bei (x, y, z): ein Viereck in der Höhe y + 1
rlBegin(RL_QUADS);
rlColor4ub(96, 168, 64, 255);
rlNormal3f(0, 1, 0);
rlVertex3f(x,     y + 1, z);
rlVertex3f(x,     y + 1, z + 1);
rlVertex3f(x + 1, y + 1, z + 1);
rlVertex3f(x + 1, y + 1, z);
rlEnd();
```

- `RL_QUADS` macht aus je vier Ecken ein Viereck, `RL_TRIANGLES` aus je drei ein Dreieck, `RL_LINES` aus je zwei eine Linie.
- Von außen gesehen müssen die Ecken **gegen den Uhrzeigersinn** laufen. Die Grafikkarte zeichnet nur Vorderseiten; in der falschen Reihenfolge ist die Fläche von außen unsichtbar.
- Farbe und Normale gelten für alle folgenden Ecken, bis du sie änderst.
<!-- /part -->

<!-- part: texturen -->
<!-- short: `LoadTexture("bild.png")`, `SetTextureFilter(t, TEXTURE_FILTER_POINT)`; auf eigenen Flächen `rlSetTexture(t.id)` und `rlTexCoord2f(u, v)`. -->
### Texturen

Eine Textur ist ein Bild, das die Grafikkarte auf Flächen klebt. Im Browser muss das Bild in die Webseite eingebettet sein; die raylib-Vorlage bettet `textures.png` ein.

```c
Texture2D texture = LoadTexture("textures.png");
SetTextureFilter(texture, TEXTURE_FILTER_POINT);   // scharfe Pixel statt weich
// …
UnloadTexture(texture);                            // am Ende
```

In 2D zeichnet `DrawTexture(texture, x, y, WHITE)` das ganze Bild, `DrawTextureRec(texture, (Rectangle){ sx, sy, width, height }, (Vector2){ x, y }, WHITE)` einen Ausschnitt. Auf [eigenen Flächen](#eigene-flaechen) bekommt jede Ecke **Texturkoordinaten** u und v von 0 bis 1: u = 0 ist der linke Rand des Bildes, u = 1 der rechte, v = 0 oben, v = 1 unten.

```c
rlSetTexture(texture.id);
rlBegin(RL_QUADS);
rlColor4ub(255, 255, 255, 255);   // Weiß lässt die Farben der Textur, wie sie sind
rlNormal3f(0, 0, 1);
rlTexCoord2f(0, 1); rlVertex3f(0, 0, 1);   // links unten
rlTexCoord2f(1, 1); rlVertex3f(1, 0, 1);   // rechts unten
rlTexCoord2f(1, 0); rlVertex3f(1, 1, 1);   // rechts oben
rlTexCoord2f(0, 0); rlVertex3f(0, 1, 1);   // links oben
rlEnd();
rlSetTexture(0);
```

Liegen mehrere Bilder nebeneinander in einem Streifen (ein **Atlas**), reicht Bild Nummer `k` von acht von u = k / 8 bis u = (k + 1) / 8. Ein eigener Shader liest die Textur mit `uniform sampler2D texture0;` und `texture2D(texture0, uv)`; die Koordinaten kommen im Vertex-Shader als `attribute vec2 vertexTexCoord`.
<!-- /part -->

<!-- part: shader -->
<!-- short: `LoadShader("light.vs", "light.fs")`, `GetShaderLocation`, `SetShaderValue`, zeichnen zwischen `BeginShaderMode` und `EndShaderMode`. -->
### Shader

Ein Shader ist ein kleines Programm für die Grafikkarte, geschrieben in [GLSL](/reference/glsl). Der **Vertex-Shader** läuft für jede Ecke, der **Fragment-Shader** für jeden Pixel einer Fläche. Hier steht, was das C-Programm dazu tut; die Sprache selbst steht bei [GLSL](/reference/glsl). Im C-Programm:

```c
Shader light = LoadShader("light.vs", "light.fs");
int loc_sun = GetShaderLocation(light, "sun");   // vor der Schleife

// in der Schleife
Vector3 sun = Vector3Normalize((Vector3){ 0.5f, 0.8f, 0.3f });
SetShaderValue(light, loc_sun, &sun, SHADER_UNIFORM_VEC3);

BeginMode3D(camera);
BeginShaderMode(light);
// … Flächen zeichnen …
EndShaderMode();
EndMode3D();

UnloadShader(light);   // am Ende
```

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Typ</th><th>Für</th></tr></thead>
<tbody>
<tr><td><code>SHADER_UNIFORM_FLOAT</code></td><td><code>float</code></td></tr>
<tr><td><code>SHADER_UNIFORM_INT</code></td><td><code>int</code></td></tr>
<tr><td><code>SHADER_UNIFORM_VEC3</code></td><td><code>Vector3</code>, im Shader <code>vec3</code></td></tr>
<tr><td><code>SHADER_UNIFORM_VEC4</code></td><td>vier Zahlen, im Shader <code>vec4</code></td></tr>
</tbody>
</table>
</div>

Eine Liste übergibt `SetShaderValueV(shader, locIndex, values, uniformType, count)`. Ist ein Name falsch geschrieben, liefert `GetShaderLocation` −1, und der Wert kommt nie an, ohne Fehlermeldung. Fehler im Shader selbst meldet raylib in der Konsole der Seite.

Im Browser verstehen Shader die Version `#version 100` von GLSL:

```glsl
#version 100
precision mediump float;

varying vec3 normal;        // vom Vertex-Shader, für jeden Pixel gemischt
varying vec4 color;
uniform vec3 sun;          // vom C-Programm, für alle Pixel gleich

void main()
{
    float sunAmount = max(dot(normalize(normal), sun), 0.0);
    gl_FragColor = vec4(color.rgb * (0.4 + 0.6 * sunAmount), 1.0);
}
```

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>GLSL</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>float</code>, <code>int</code>, <code>vec3</code>, <code>vec4</code></td><td>Zahl, ganze Zahl, drei und vier Zahlen</td></tr>
<tr><td><code>dot(a, b)</code></td><td>Skalarprodukt</td></tr>
<tr><td><code>normalize(v)</code>, <code>length(v)</code></td><td>Länge 1, Länge</td></tr>
<tr><td><code>max(a, b)</code>, <code>min(a, b)</code>, <code>clamp(x, a, b)</code></td><td>Grenzen</td></tr>
<tr><td><code>mix(a, b, t)</code></td><td>Mischung zwischen <code>a</code> und <code>b</code></td></tr>
<tr><td><code>gl_FragColor</code></td><td>Die Farbe, die der Pixel am Ende bekommt</td></tr>
</tbody>
</table>
</div>

Eine Schleife braucht in `#version 100` eine feste Obergrenze: `for (int i = 0; i < 16; i++)` und darin `if (i >= count) break;`. Zahlen mit Komma schreibst du immer mit Punkt und Nachkommastelle, `1.0` statt `1`.
<!-- /part -->

## Zum Nachschlagen

- [raylib-Cheatsheet](https://www.raylib.com/cheatsheet/cheatsheet.html): alle Funktionen von raylib auf einer Seite, nach Modulen geordnet, mit ihren Parametern.
- [raymath-Cheatsheet](https://www.raylib.com/cheatsheet/raymath_cheatsheet.html): alle Rechenbefehle für Vektoren und Matrizen aus `raymath.h`.
- [raylib-Beispiele](https://www.raylib.com/examples.html): Hunderte kleine Programme, nach Themen geordnet, im Browser zum Ausprobieren und mit Quelltext.
- [`rlgl.h`](https://github.com/raysan5/raylib/blob/master/src/rlgl.h): die Befehle für eigene Flächen (`rlBegin`, `rlVertex3f` …), mit Kommentaren im Quelltext.
- [raylib-Wiki](https://github.com/raysan5/raylib/wiki): Antworten auf häufige Fragen, etwa zum Übersetzen für den Browser.
- Die Sprache der Shader steht bei [GLSL](/reference/glsl).
