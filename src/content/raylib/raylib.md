<div class='meta'>
image: tag.webp
kind: projekt
language: C / raylib
topic: graphics
duration: 5 Doppelstunden
requires: [pixelram]
reference: [raylib]
template: https://github.com/specht/raylib-starter.git
template_name: der raylib-Vorlage
</div>

# Eine Klötzchenwelt in 3D

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

<p class='abstract'>
Du programmierst in C eine Welt aus Blöcken, durch die du selbst läufst: Mit der Maus siehst du dich um, du springst über Hügel, reißt Blöcke ab und baust neue. Eine Sonne wandert über den Himmel, nachts leuchten deine Fackeln, berechnet von einem Licht-Shader, den du selbst schreibst, und die Blöcke bekommen Texturen, die du selbst zeichnest.
</p>

<div class='screen-gallery'>
<figure>
<img src='tag.webp' alt='Blick über eine hügelige Welt aus Blöcken mit Gras-, Erd- und Holztexturen, ein paar Fackeln stehen im Gras.'>
<figcaption>Unterwegs in der eigenen Welt, mit eigenen Texturen; im dunklen Modus bei Nacht</figcaption>
</figure>
<figure>
<img src='insel.webp' alt='Die ganze Welt von schräg oben: Hügel aus Gras, ein Sandstrand, Bäume, am Rand Erd- und Steinschichten.'>
<figcaption>Die Welt ist ein Array mit 55.296 Feldern</figcaption>
</figure>
<figure>
<img src='bauen.webp' alt='Ein paar gesetzte Steinblöcke auf der Wiese, einer ist schwarz umrandet, unten die Leiste mit sechs Blockarten.'>
<figcaption>Zielen, bauen und abbauen</figcaption>
</figure>
</div>

In der [Anaglyph-Aktivität](/anaglyph) beschreibst du eine Szene, und der Viewer zeigt sie. Bei [PixelRAM](/pixelram) setzt du jedes Pixel selbst. Hier schreibst du ein Programm, das 60-mal in der Sekunde reagiert: auf deine Tasten, auf deine Maus und auf eine Welt, die sich dabei verändert. Dafür verwenden wir **raylib**, eine Bibliothek für Spiele in C. Sie öffnet ein Fenster, liest Tastatur und Maus, zeichnet mit der Grafikkarte in 2D und 3D und spielt Töne ab. Übrigens bringt auch PixelRAM seine Pixel mit raylib auf den Bildschirm.

Du brauchst etwas C: Variablen, Schleifen, Funktionen und `#define`. Wenn du [Echtzeitgrafik mit C](/pixelram) gemacht hast, kennst du alles, was du brauchst. Ein wenig Mathematik kommt dazu, Sinus und Kosinus für die Blickrichtung und Vektoren für Bewegung und Licht; beides wird an der Stelle erklärt, an der du es brauchst. Alle Befehle von raylib, die vorkommen, stehen im [Handbuch](/reference/raylib).

## Einrichten

### Repository klonen

Für diese Anleitung brauchst du die raylib-Vorlage:

_include(workspace#repository-klonen)

### Übersetzen und ansehen

Öffne die Datei `main.c` und gib im Terminal ein:

```bash
make
```

Beim ersten Mal lädt `make` die Webseite herunter, in der dein Programm läuft; sie kommt von PixelRAM. Dann übersetzt es `main.c` in die Datei `main.html`: Dein C-Programm läuft als WebAssembly im Browser.

<!-- tutorial-screenshot
# Die Vorlage so, wie das Klonen sie anlegt (Dateien aus vorlage/, also
# ohne das Repository auf GitHub), den Ordner öffnen, dann make im Terminal
show-left-sidebar
write-file: Makefile <- file:vorlage/Makefile
write-file: main.c <- file:vorlage/main.c
write-file: licht.vs <- file:vorlage/licht.vs
write-file: licht.fs <- file:vorlage/licht.fs
write-file: README.md <- file:vorlage/README.md
write-file: .gitignore <- file:vorlage/.gitignore
write-file: texturen.png <- file:vorlage/texturen.png
press: Control+K
press: Control+O
wait-for-input-value: /workspace/
type: raylib-starter
press: Enter
wait-for-input-value: /workspace/raylib-starter/
press: Enter
wait-for-text: main.c
open-file: main.c
terminal-open
terminal-run: make
terminal-wait-for-prompt: 180
-->
<img class='full' src='make.webp' alt='Der Workspace mit main.c im Editor; im Terminal darunter hat make die Datei main.html erzeugt.'>

<img src='../reference/workspace/go-live.webp' class='r' style='width: 21em;' alt=''>

Klicke unten rechts auf »Go Live«. Im neuen Tab siehst du die Dateien des Projekts; klicke dort auf `main.html`. Du solltest einen orangefarbenen Würfel auf einem Gitter sehen:

<div style='clear: both;'></div>

<!-- tutorial-screenshot
go-live
tab: preview
viewport: 960x540
hook: open main.html
hook: settle
crop-element: #canvas
-->
<img class='full' src='start.webp' alt='Ein orangefarbener Würfel steht auf einem Gitter vor hellblauem Himmel, oben links steht »Hallo, 3D!«.'>

<div class='hint'>
<strong>So arbeitest du ab jetzt:</strong> Datei ändern und speichern, im Terminal <code>make</code>, dann die Vorschau neu laden, falls sie es nicht von selbst tut. Findet <code>make</code> einen Fehler, nennt es die Zeile in deiner Datei, und <code>main.html</code> bleibt, wie sie war. Lies die erste Fehlermeldung zuerst, die übrigen sind oft nur Folgefehler. Was dein Programm mit <code>printf</code> ausgibt, steht in der Konsole der Seite: Fahre mit der Maus über das Bild und klicke unten rechts auf das Terminal-Symbol.
</div>

_task(uebersetzt)

## Eine Welt im Raum

Sieh dir `main.c` an. Das ganze Programm ist eine Schleife, die raylib 60-mal in der Sekunde durchläuft, bis du die Seite schließt:

```c
int main(void)
{
    InitWindow(960, 540, "Klötzchenwelt");
    SetTargetFPS(60);

    // Die Kamera: wo sie steht, wohin sie schaut und wo oben ist
    Camera3D kamera = { 0 };
    kamera.position = (Vector3){ 6, 5, 8 };
    kamera.target = (Vector3){ 0, 0, 0 };
    kamera.up = (Vector3){ 0, 1, 0 };
    kamera.fovy = 45;
    kamera.projection = CAMERA_PERSPECTIVE;

    while (!WindowShouldClose())
    {
        BeginDrawing();
        ClearBackground(SKYBLUE);

        BeginMode3D(kamera);
        DrawGrid(10, 1);
        DrawCube((Vector3){ 0, 0.5f, 0 }, 1, 1, 1, ORANGE);
        DrawCubeWires((Vector3){ 0, 0.5f, 0 }, 1, 1, 1, MAROON);
        EndMode3D();

        DrawText("Hallo, 3D!", 20, 20, 30, DARKBLUE);
        EndDrawing();
    }

    CloseWindow();
    return 0;
}
```

### Drei Achsen

Ein Punkt im Raum hat drei Koordinaten. In raylib zeigt **x** nach rechts, **y** nach oben und **z** nach vorne, auf dich zu, wenn du von vorne auf die Welt schaust. Das Gitter, das `DrawGrid(10, 1)` zeichnet, liegt flach auf dem Boden, also bei y = 0, und reicht von −5 bis 5 in x- und z-Richtung. Eine Einheit ist in diesem Projekt ein Block.

`DrawCube(position, breite, hoehe, tiefe, farbe)` zeichnet einen Quader, dessen **Mitte** an der Position liegt. Der Würfel bei `(0, 0.5f, 0)` ist einen Block hoch und steht deshalb genau auf dem Boden. `DrawCubeWires` zeichnet dieselben Kanten noch einmal als Linien.

`(Vector3){ 0, 0.5f, 0 }` ist ein Wert vom Typ `Vector3`: drei Zahlen `x`, `y` und `z`, zusammengefasst wie in einem `struct`. raylib benutzt `Vector3` für Positionen und für Richtungen. Das `f` hinter `0.5` macht daraus eine `float`-Zahl.

### Die Kamera

Was du siehst, bestimmt die Kamera:

- `position`: wo die Kamera steht,
- `target`: welchen Punkt sie anschaut,
- `up`: welche Richtung oben ist, hier die y-Achse,
- `fovy`: wie weit der Blick nach oben und unten reicht, in Grad. Ein kleiner Wert wirkt wie ein Fernglas, ein großer wie eine Weitwinkellinse.

Alles, was zwischen `BeginMode3D(kamera)` und `EndMode3D()` gezeichnet wird, zeigt raylib durch diese Kamera. Was danach kommt, wie `DrawText`, liegt flach auf dem Bild, wie eine Beschriftung auf einer Glasscheibe.

_task(von-oben)

Stell die Kamera danach wieder zurück. Probiere auch, weitere Würfel mit anderen Farben und Größen dazuzustellen, zum Beispiel einen langen, flachen Balken mit `DrawCube((Vector3){ 2, 0.25f, 0 }, 3, 0.5f, 1, DARKGREEN)`. Die Namen der Farben, die raylib kennt, stehen im [Handbuch](/reference/raylib#farben).

## Eine Welt aus Blöcken

Ein paar Würfel kann man einzeln hinschreiben. Eine ganze Welt braucht eine andere Idee: Wir teilen den Raum in ein Gitter aus Feldern, jedes Feld ist einen Block groß, und für jedes Feld merken wir uns, was dort ist: Luft, Gras, Erde, Stein … Das ist ein dreidimensionales Array.

### Die Welt ist ein Array

Ersetze alles, was in `main.c` über `int main(void)` steht, durch diese Zeilen:

```c
#include <math.h>
#include <stdlib.h>
#include <raylib.h>

// ---------------------------------------------------------------- Die Welt

#define BREITE 48   // x
#define HOEHE 24    // y, nach oben
#define TIEFE 48    // z

enum { LUFT, GRAS, ERDE, STEIN, HOLZ, LAUB, SAND, ANZAHL_ARTEN };

Color farben[ANZAHL_ARTEN] = {
    { 0, 0, 0, 0 },
    { 96, 168, 64, 255 },     // Gras
    { 130, 92, 62, 255 },     // Erde
    { 125, 125, 130, 255 },   // Stein
    { 150, 110, 64, 255 },    // Holz
    { 52, 128, 56, 255 },     // Laub
    { 222, 204, 150, 255 },   // Sand
};

unsigned char welt[BREITE][HOEHE][TIEFE];

// Der Block an einer Stelle; außerhalb der Welt ist Luft
int block(int x, int y, int z)
{
    if (x < 0 || y < 0 || z < 0 || x >= BREITE || y >= HOEHE || z >= TIEFE)
        return LUFT;
    return welt[x][y][z];
}
```

- `BREITE`, `HOEHE` und `TIEFE` sind die Größe der Welt in Blöcken.
- Das `enum` gibt jeder Blockart eine Nummer: `LUFT` ist 0, `GRAS` ist 1 und so weiter. `ANZAHL_ARTEN` ist dann automatisch die Anzahl der Arten.
- `farben` hat für jede Art eine Farbe, in derselben Reihenfolge. `farben[STEIN]` ist das Grau des Steins. Die vier Zahlen sind Rot, Grün, Blau und Deckkraft, jeweils von 0 bis 255.
- `welt[x][y][z]` ist der Block an einer Stelle. `unsigned char` ist eine kleine Zahl von 0 bis 255, mehr brauchen wir für eine Blockart nicht.
- `block(x, y, z)` liest einen Block, sagt aber für Stellen außerhalb der Welt einfach `LUFT`, statt über den Rand des Arrays hinaus zu lesen. Das wird gleich wichtig.

_task(zellen)

### Hügel aus Wellen

Noch ist die Welt leer: Ein globales Array ist in C am Anfang mit Nullen gefüllt, also mit `LUFT`. Für die Landschaft berechnen wir für jede Stelle (x, z) eine Höhe und füllen die Säule darunter mit Blöcken: oben Gras, darunter drei Lagen Erde, ganz unten Stein. Die Höhe entsteht aus drei Sinuswellen mit verschiedenen Längen, die sich überlagern, so wie die Wellen bei [PixelRAM](/pixelram#wellen). Wo die Welt niedrig ist, liegt Sand.

```c
void baum(int x, int y, int z)
{
    for (int i = 0; i < 4; i++)
        welt[x][y + i][z] = HOLZ;
    for (int dx = -2; dx <= 2; dx++)
        for (int dz = -2; dz <= 2; dz++)
            for (int dy = 3; dy <= 5; dy++)
                if (abs(dx) + abs(dz) + (dy - 3) <= 3 && block(x + dx, y + dy, z + dz) == LUFT)
                    welt[x + dx][y + dy][z + dz] = LAUB;
}

void erzeuge_welt(void)
{
    for (int x = 0; x < BREITE; x++) {
        for (int z = 0; z < TIEFE; z++) {
            // Hügel aus drei Wellen, die sich überlagern
            float h = 7.0f + 2.0f * sinf(x * 0.13f) + 1.5f * cosf(z * 0.11f)
                    + 1.5f * sinf((x + z) * 0.07f);
            int hoehe = (int)h;
            for (int y = 0; y < hoehe; y++) {
                if (y == hoehe - 1)
                    welt[x][y][z] = hoehe <= 5 ? SAND : GRAS;
                else if (y >= hoehe - 3)
                    welt[x][y][z] = ERDE;
                else
                    welt[x][y][z] = STEIN;
            }
        }
    }
    SetRandomSeed(7);
    for (int i = 0; i < 14; i++) {
        int x = GetRandomValue(3, BREITE - 4);
        int z = GetRandomValue(3, TIEFE - 4);
        int y = HOEHE - 7;
        while (y > 0 && block(x, y - 1, z) == LUFT)
            y--;
        if (block(x, y - 1, z) == GRAS)
            baum(x, y, z);
    }
}
```

Am Ende stellt `erzeuge_welt` noch 14 Bäume an zufällige Stellen. `SetRandomSeed(7)` sorgt dafür, dass die »zufälligen« Stellen bei jedem Start dieselben sind. Mit einer anderen Zahl wächst ein anderer Wald. Ein Baum ist ein Stamm aus vier Holzblöcken und eine Krone aus Laub, die nach oben schmaler wird: `abs(dx) + abs(dz)` ist der Abstand vom Stamm, gemessen in Schritten entlang der Achsen.

### Was man sieht

Jetzt zeichnen wir die Welt. Für jeden Block einen `DrawCube` aufzurufen, wäre viel Arbeit für den Computer: Die meisten Blöcke stecken im Boden, und niemand sieht sie. Gezeichnet wird deshalb nur ein Block, der an Luft grenzt:

```c
bool sichtbar(int x, int y, int z)
{
    return block(x + 1, y, z) == LUFT || block(x - 1, y, z) == LUFT
        || block(x, y + 1, z) == LUFT || block(x, y - 1, z) == LUFT
        || block(x, y, z + 1) == LUFT || block(x, y, z - 1) == LUFT;
}
```

Hier hilft, dass `block` außerhalb der Welt `LUFT` sagt: Am Rand gilt ein Block als sichtbar, ohne dass wir über das Array hinaus lesen.

Ersetze `main` durch diese Fassung. Die Kamera steht jetzt hoch über der Welt, und `UpdateCamera(&kamera, CAMERA_ORBITAL)` lässt sie langsam um ihr Ziel kreisen, die Mitte der Welt:

```c
int main(void)
{
    InitWindow(960, 540, "Klötzchenwelt");
    SetTargetFPS(60);

    erzeuge_welt();

    Camera3D kamera = { 0 };
    kamera.position = (Vector3){ -20, 30, -20 };
    kamera.target = (Vector3){ BREITE / 2, 4, TIEFE / 2 };
    kamera.up = (Vector3){ 0, 1, 0 };
    kamera.fovy = 45;
    kamera.projection = CAMERA_PERSPECTIVE;

    while (!WindowShouldClose())
    {
        // raylib dreht die Kamera langsam um ihr Ziel
        UpdateCamera(&kamera, CAMERA_ORBITAL);

        BeginDrawing();
        ClearBackground(SKYBLUE);

        BeginMode3D(kamera);
        int bloecke = 0;
        for (int x = 0; x < BREITE; x++)
            for (int y = 0; y < HOEHE; y++)
                for (int z = 0; z < TIEFE; z++)
                    if (welt[x][y][z] != LUFT && sichtbar(x, y, z)) {
                        Vector3 mitte = { x + 0.5f, y + 0.5f, z + 0.5f };
                        DrawCube(mitte, 1, 1, 1, farben[welt[x][y][z]]);
                        DrawCubeWires(mitte, 1, 1, 1, Fade(BLACK, 0.25f));
                        bloecke++;
                    }
        EndMode3D();

        DrawText(TextFormat("%d Blöcke  %d FPS", bloecke, GetFPS()), 10, 10, 20, DARKBLUE);
        EndDrawing();
    }

    CloseWindow();
    return 0;
}
```

<!-- tutorial-screenshot
write-file: main.c <- file:stufen/2.c
terminal-run: make
terminal-wait-for-prompt: 120
tab: preview
viewport: 960x540
hook: open main.html
hook: play 3
hook: settle
crop-element: #canvas
-->
<img class='full' src='insel.webp' alt='Die ganze Welt von schräg oben: Hügel aus Gras, ein Sandstrand, Bäume, am Rand Erd- und Steinschichten.'>

Oben links zählt das Programm, wie viele Blöcke es zeichnet, und wie viele Bilder pro Sekunde (FPS) es schafft. Behalte die Zahl im Auge: Später zeichnen wir die Welt viel geschickter.

<div class='hint think'>
<strong>Zum Nachdenken:</strong> Warum sieht man am Rand der Welt die Schichten aus Erde und Stein? Was würde passieren, wenn <code>block</code> außerhalb der Welt <code>STEIN</code> statt <code>LUFT</code> sagen würde?
</div>

## Mit eigenen Augen

Bis jetzt schwebt die Kamera über der Welt. Jetzt gehst du hinein: Die Kamera wird zu deinen Augen.

### Wohin schaust du?

Eine Figur in der Welt hat eine Position und eine Blickrichtung. Die Position sind ihre Füße. Die Blickrichtung beschreiben wir mit zwei Winkeln: `drehung` dreht nach links und rechts, `neigung` nach oben und unten. Daraus berechnet `blickrichtung()` einen Vektor, der vom Auge aus nach vorne zeigt:

```c
// --------------------------------------------------------------- Die Figur

Vector3 fuesse;          // Mitte der Fußsohlen
float drehung = 0;       // nach links und rechts, im Bogenmaß
float neigung = 0;       // nach oben und unten

#define AUGENHOEHE 1.6f

Vector3 blickrichtung(void)
{
    return (Vector3){
        sinf(drehung) * cosf(neigung),
        sinf(neigung),
        cosf(drehung) * cosf(neigung),
    };
}
```

Winkel misst C im **Bogenmaß**: Eine halbe Umdrehung ist π (etwa 3,14), eine ganze 2π. Ohne Neigung bleiben von `blickrichtung` nur Sinus und Kosinus der Drehung:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th><code>drehung</code></th><th>Blickrichtung (x, y, z)</th><th>du schaust …</th></tr></thead>
<tbody>
<tr><td>0</td><td>(0, 0, 1)</td><td>nach vorne, in z-Richtung</td></tr>
<tr><td>π/2 ≈ 1,57</td><td>(1, 0, 0)</td><td>nach rechts, in x-Richtung</td></tr>
</tbody>
</table>
</div>

Ist die Neigung nicht 0, wird der Blick um `cosf(neigung)` kürzer in der Waagerechten, und `sinf(neigung)` gibt an, wie weit er nach oben zeigt. So hat der Vektor immer die Länge 1.

_task(blick)

### Laufen

`bewege_figur` liest die Tasten und bewegt die Figur. Schreibe sie unter `blickrichtung`:

```c
void bewege_figur(float dt)
{
    // Umsehen: Maus (wenn gefangen) oder Pfeiltasten
    if (IsCursorHidden()) {
        Vector2 maus = GetMouseDelta();
        drehung -= maus.x * 0.003f;
        neigung -= maus.y * 0.003f;
    }
    if (IsKeyDown(KEY_LEFT)) drehung += 2.0f * dt;
    if (IsKeyDown(KEY_RIGHT)) drehung -= 2.0f * dt;
    if (IsKeyDown(KEY_UP)) neigung += 1.5f * dt;
    if (IsKeyDown(KEY_DOWN)) neigung -= 1.5f * dt;
    neigung = Clamp(neigung, -1.5f, 1.5f);

    // Laufen: vorwärts in Blickrichtung, aber ohne nach oben zu fliegen
    Vector3 vorne = { sinf(drehung), 0, cosf(drehung) };
    Vector3 rechts = { -cosf(drehung), 0, sinf(drehung) };
    Vector3 wunsch = { 0 };
    if (IsKeyDown(KEY_W)) wunsch = Vector3Add(wunsch, vorne);
    if (IsKeyDown(KEY_S)) wunsch = Vector3Subtract(wunsch, vorne);
    if (IsKeyDown(KEY_D)) wunsch = Vector3Add(wunsch, rechts);
    if (IsKeyDown(KEY_A)) wunsch = Vector3Subtract(wunsch, rechts);
    wunsch = Vector3Scale(Vector3Normalize(wunsch), 4.5f * dt);
    fuesse = Vector3Add(fuesse, wunsch);
}
```

- **Umsehen:** Mit den Pfeiltasten änderst du die Winkel. Hat das Programm die Maus gefangen, dreht `GetMouseDelta()` den Blick: Das ist, wie weit sich die Maus seit dem letzten Bild bewegt hat. `Clamp` hält die Neigung zwischen −1,5 und 1,5, sonst könntest du dich über Kopf drehen.
- **Laufen:** `vorne` ist die Blickrichtung ohne Neigung, `rechts` steht im rechten Winkel dazu. Jede gedrückte Taste addiert eine dieser Richtungen zu `wunsch`. `Vector3Normalize` macht den Vektor wieder 1 lang, sonst wärst du schräg (<kbd>W</kbd> und <kbd>D</kbd> zusammen) schneller als geradeaus.
- **Zeit:** `dt` ist die Zeit seit dem letzten Bild in Sekunden. Wer 4,5 Blöcke pro Sekunde laufen soll, läuft in einem Bild `4.5f * dt` Blöcke weit. So ist die Figur auf einem schnellen und einem langsamen Computer gleich schnell.

Die Rechenbefehle für Vektoren wie `Vector3Add` und `Vector3Scale` kommen aus `raymath.h`. Schreibe deshalb oben unter die Zeile mit `raylib.h`:

```c
#include <raymath.h>
```

_task(pro-bild)

### Die Kamera folgt

In `main` steht die Kamera jetzt dort, wo die Augen der Figur sind, und schaut in die Blickrichtung. Ersetze `main` noch einmal:

```c
int main(void)
{
    InitWindow(960, 540, "Klötzchenwelt");
    SetTargetFPS(60);
    SetExitKey(KEY_NULL);   // Esc gibt nur die Maus frei

    erzeuge_welt();
    fuesse = (Vector3){ 8.5f, 9, 10.5f };
    drehung = 0.8f;      // schräg über die Welt schauen
    neigung = -0.2f;

    Camera3D kamera = { 0 };
    kamera.up = (Vector3){ 0, 1, 0 };
    kamera.fovy = 70;
    kamera.projection = CAMERA_PERSPECTIVE;

    while (!WindowShouldClose())
    {
        float dt = GetFrameTime();
        if (dt > 0.05f) dt = 0.05f;

        // Ein Klick ins Bild fängt die Maus, Esc gibt sie frei
        if (!IsCursorHidden() && IsMouseButtonPressed(MOUSE_BUTTON_LEFT))
            DisableCursor();
        if (IsKeyPressed(KEY_ESCAPE)) EnableCursor();

        bewege_figur(dt);

        Vector3 augen = { fuesse.x, fuesse.y + AUGENHOEHE, fuesse.z };
        kamera.position = augen;
        kamera.target = Vector3Add(augen, blickrichtung());

        BeginDrawing();
        ClearBackground(SKYBLUE);

        BeginMode3D(kamera);
        int bloecke = 0;
        for (int x = 0; x < BREITE; x++)
            for (int y = 0; y < HOEHE; y++)
                for (int z = 0; z < TIEFE; z++)
                    if (welt[x][y][z] != LUFT && sichtbar(x, y, z)) {
                        Vector3 mitte = { x + 0.5f, y + 0.5f, z + 0.5f };
                        DrawCube(mitte, 1, 1, 1, farben[welt[x][y][z]]);
                        DrawCubeWires(mitte, 1, 1, 1, Fade(BLACK, 0.25f));
                        bloecke++;
                    }
        EndMode3D();

        DrawText(TextFormat("%d Blöcke  %d FPS", bloecke, GetFPS()), 10, 10, 20, DARKBLUE);
        if (!IsCursorHidden() && GetTime() < 8)
            DrawText("Klick ins Bild: Maus fangen. WASD laufen, Pfeiltasten umsehen.", 10, 36, 20, WHITE);
        EndDrawing();
    }

    CloseWindow();
    return 0;
}
```

Neu ist auch die Maus: Ein Klick ins Bild ruft `DisableCursor()` auf. Der Mauszeiger verschwindet, und jede Bewegung der Maus dreht den Blick, wie in einem Spiel. <kbd>Esc</kbd> gibt die Maus wieder frei (`SetExitKey(KEY_NULL)` sorgt dafür, dass <kbd>Esc</kbd> nicht gleich das ganze Programm beendet).

<!-- tutorial-screenshot
write-file: main.c <- file:stufen/3.c
terminal-run: make
terminal-wait-for-prompt: 120
tab: preview
viewport: 960x540
hook: open main.html
hook: play 8 KeyW:1.2 ArrowLeft:0.4
hook: settle
crop-element: #canvas
-->
<img class='full' src='geist.webp' alt='Blick aus Augenhöhe über die Wiese, rechts ein Baum, die Blöcke mit dunklen Kanten.'>

Lauf ein wenig herum. Du merkst schnell: Du bist ein Geist. Du gehst durch Hügel und Bäume hindurch, und wenn das Land abfällt, schwebst du in der Luft.

## Schwerkraft und Wände

### Fest oder nicht?

Damit die Welt dich aufhält, braucht die Figur einen Körper. Wir stellen sie uns als schmale Säule vor: 0,6 Blöcke breit und tief und 1,7 Blöcke hoch. Die Figur steckt in einem Block, wenn ihre Säule in ein festes Feld hineinragt. Dafür muss man nur die wenigen Felder prüfen, die die Säule berührt: `floorf` rundet ab und findet so das Feld, in dem eine Koordinate liegt.

Schreibe `fest` unter `block` und `kollidiert` unter die `#define`s der Figur. Die neuen Variablen `vy` und `am_boden` kommen zu den anderen über `blickrichtung`:

```c
bool fest(int x, int y, int z)
{
    if (x < 0 || z < 0 || x >= BREITE || z >= TIEFE || y < 0)
        return true;
    return block(x, y, z) != LUFT;
}

#define RADIUS 0.3f      // die Figur ist 0,6 Blöcke breit
#define GROESSE 1.7f
#define AUGENHOEHE 1.6f

bool kollidiert(Vector3 f)
{
    for (int x = (int)floorf(f.x - RADIUS); x <= (int)floorf(f.x + RADIUS); x++)
        for (int y = (int)floorf(f.y); y <= (int)floorf(f.y + GROESSE); y++)
            for (int z = (int)floorf(f.z - RADIUS); z <= (int)floorf(f.z + RADIUS); z++)
                if (fest(x, y, z))
                    return true;
    return false;
}

float vy = 0;            // Geschwindigkeit nach oben, in Blöcken pro Sekunde
bool am_boden = false;
```

`fest` hat am Rand und unter der Welt unsichtbare Wände, damit du nicht hinunterfällst.

### Eine Richtung nach der anderen

Am Ende von `bewege_figur` stand bisher `fuesse = Vector3Add(fuesse, wunsch);`. Ersetze diese Zeile durch:

```c
// Jede Richtung einzeln: So gleitet man an einer Wand entlang, statt an
// ihr kleben zu bleiben.
Vector3 neu = fuesse;
neu.x += wunsch.x;
if (!kollidiert(neu)) fuesse.x = neu.x;
neu = fuesse;
neu.z += wunsch.z;
if (!kollidiert(neu)) fuesse.z = neu.z;
```

Das Programm probiert jeden Schritt erst aus: Würde die Figur nach dem Schritt in einem Block stecken, bleibt sie, wo sie ist.

_task(einzeln)

### Fallen und springen

Unter den Schritt in x und z kommt die Schwerkraft:

```c
// Schwerkraft und Springen
vy -= 22.0f * dt;
if (am_boden && IsKeyDown(KEY_SPACE)) vy = 7.5f;
neu = fuesse;
neu.y += vy * dt;
if (kollidiert(neu)) {
    if (vy < 0) {
        am_boden = true;
        fuesse.y = floorf(neu.y) + 1.0f;   // genau auf den Block stellen
    }
    vy = 0;
} else {
    fuesse.y = neu.y;
    am_boden = false;
}
```

`vy` ist die Geschwindigkeit nach oben. Die Schwerkraft verkleinert sie in jeder Sekunde um 22 Blöcke pro Sekunde, also wird die Figur immer schneller, je länger sie fällt. Ein Sprung setzt `vy` auf 7,5, und die Schwerkraft bremst ihn wieder ab. Stößt die Figur beim Fallen auf einen Block, steht sie auf dem Boden: `vy` wird 0, und die Füße kommen genau auf die Oberseite des Blocks.

Zuletzt soll die Figur nicht mehr bei y = 9 in der Luft beginnen, sondern oben über der Welt, und dann auf den Boden fallen. Ändere die Startposition in `main`:

```c
fuesse = (Vector3){ 8.5f, HOEHE - 2, 10.5f };
```

<!-- tutorial-screenshot
write-file: main.c <- file:stufen/4.c
terminal-run: make
terminal-wait-for-prompt: 120
tab: preview
viewport: 960x540
hook: open main.html
hook: play 8 KeyW:2 ArrowRight:0.3
hook: settle
crop-element: #canvas
-->
<img class='full' src='laufen.webp' alt='Blick über die Wiese aus Augenhöhe; die Hügel steigen in Stufen an, hinten ein Baum.'>

_task(sprunghoehe)

_task(figur)

## Bauen

### Wohin zielst du?

Um einen Block abzubauen, musst du wissen, auf welchen Block du schaust. Dafür gehen wir vom Auge aus in kleinen Schritten die Blickrichtung entlang, bis wir in einem Block landen, höchstens sechs Blöcke weit. Das ist ein **Strahl**, wie ein Laserpointer. Das Feld, durch das der Strahl zuletzt noch durch Luft ging, merken wir uns auch: Dort entsteht ein neuer Block, wenn du baust.

Schreibe das über `main`:

```c
// ------------------------------------------------------------------ Zielen

bool getroffen = false;
int zx, zy, zz;          // der Block, auf den du schaust
int lx, ly, lz;          // die Luft davor: Hier entsteht ein neuer Block

void ziele(Vector3 augen, Vector3 blick)
{
    getroffen = false;
    lx = (int)floorf(augen.x); ly = (int)floorf(augen.y); lz = (int)floorf(augen.z);
    // In kleinen Schritten den Blick entlang, bis ein Block im Weg ist
    for (float t = 0; t < 6.0f; t += 0.02f) {
        Vector3 p = Vector3Add(augen, Vector3Scale(blick, t));
        int x = (int)floorf(p.x), y = (int)floorf(p.y), z = (int)floorf(p.z);
        if (block(x, y, z) != LUFT) {
            getroffen = true;
            zx = x; zy = y; zz = z;
            return;
        }
        lx = x; ly = y; lz = z;
    }
}
```

_task(schritte)

### Abbauen und bauen

In `main` braucht das Programm die Blockart, die du gerade baust. Schreibe vor die Schleife:

```c
int auswahl = GRAS;
```

und ersetze in der Schleife den Teil, der die Maus fängt:

```c
if (!IsCursorHidden() && IsMouseButtonPressed(MOUSE_BUTTON_LEFT))
    DisableCursor();
else {
    if (getroffen && IsMouseButtonPressed(MOUSE_BUTTON_LEFT))
        welt[zx][zy][zz] = LUFT;
    if (getroffen && IsMouseButtonPressed(MOUSE_BUTTON_RIGHT) && block(lx, ly, lz) == LUFT) {
        welt[lx][ly][lz] = auswahl;
        if (kollidiert(fuesse))        // nicht in sich selbst bauen
            welt[lx][ly][lz] = LUFT;
    }
}
if (IsKeyPressed(KEY_ESCAPE)) EnableCursor();
for (int i = 1; i < ANZAHL_ARTEN; i++)
    if (IsKeyPressed(KEY_ONE + i - 1)) auswahl = i;
```

Mit der linken Maustaste wird der Block, auf den du schaust, zu Luft. Die rechte Maustaste setzt einen Block der gewählten Art in das Luftfeld davor, aber nicht, wenn du selbst dort stehst. Die Tasten <kbd>1</kbd> bis <kbd>6</kbd> wählen die Art: `KEY_ONE + 1` ist dasselbe wie `KEY_TWO`, weil die Tasten in raylib Nummern in einer Reihe haben.

Damit `ziele` in jedem Bild aufgerufen wird und der Block, auf den du schaust, einen Rahmen bekommt, ändere zwei Stellen. Die Kamera:

```c
Vector3 augen = { fuesse.x, fuesse.y + AUGENHOEHE, fuesse.z };
Vector3 blick = blickrichtung();
kamera.position = augen;
kamera.target = Vector3Add(augen, blick);
ziele(augen, blick);
```

und, direkt vor `EndMode3D()`:

```c
if (getroffen)
    DrawCubeWires((Vector3){ zx + 0.5f, zy + 0.5f, zz + 0.5f }, 1.01f, 1.01f, 1.01f, BLACK);
```

Der Rahmen ist eine Spur größer als der Block, damit er nicht in dessen Kanten verschwindet.

### Fadenkreuz und Leiste

Zuletzt zeichnen wir in 2D über das Bild: ein Fadenkreuz in der Mitte, damit du weißt, wohin du zielst, und unten eine Leiste mit den Blockarten. Für die Namen kommt über `farben` eine Liste dazu:

```c
const char *namen[ANZAHL_ARTEN] = {
    "Luft", "Gras", "Erde", "Stein", "Holz", "Laub", "Sand",
};
```

Und nach `EndMode3D()`, vor dem `DrawText` mit den FPS:

```c
// 2D über dem Bild: Fadenkreuz, Leiste mit den Blöcken, Hinweise
int mx = GetScreenWidth() / 2, my = GetScreenHeight() / 2;
DrawRectangle(mx - 10, my - 1, 20, 2, WHITE);
DrawRectangle(mx - 1, my - 10, 2, 20, WHITE);

int felder = ANZAHL_ARTEN - 1;
int links = mx - felder * 26;
for (int i = 1; i < ANZAHL_ARTEN; i++) {
    int fx = links + (i - 1) * 52, fy = GetScreenHeight() - 62;
    DrawRectangle(fx, fy, 48, 48, Fade(BLACK, 0.4f));
    DrawRectangle(fx + 8, fy + 8, 32, 32, farben[i]);
    DrawText(TextFormat("%d", i), fx + 4, fy + 2, 10, WHITE);
    if (i == auswahl)
        DrawRectangleLinesEx((Rectangle){ fx - 2, fy - 2, 52, 52 }, 3, WHITE);
}
DrawText(namen[auswahl], mx - MeasureText(namen[auswahl], 20) / 2, GetScreenHeight() - 88, 20, WHITE);
```

`MeasureText` sagt, wie breit ein Text wird; so steht der Name genau in der Mitte.

<!-- tutorial-screenshot
write-file: main.c <- file:stufen/5.c
terminal-run: make
terminal-wait-for-prompt: 120
tab: preview
viewport: 960x540
hook: open main.html
hook: play 8 ArrowDown:0.25 Digit3 Rechtsklick ArrowLeft:0.15 Rechtsklick ArrowLeft:0.15 Rechtsklick ArrowRight:0.6 Rechtsklick ArrowUp:0.12
hook: settle
crop-element: #canvas
-->
<img class='full' src='bauen.webp' alt='Ein paar gesetzte Steinblöcke auf der Wiese, einer ist schwarz umrandet, unten die Leiste mit sechs Blockarten.'>

_task(bauen)

## Licht

### Warum alles flach aussieht

Sieh dir einen gebauten Steinblock genau an: Alle Seiten haben genau dieselbe Farbe. Ohne die dunklen Kanten könntest du nicht erkennen, wo eine Seite aufhört und die nächste anfängt. In der Wirklichkeit ist das anders, weil das Licht von einer Seite kommt: Eine Fläche, die zur Sonne zeigt, ist hell, eine Fläche, die schräg zur Sonne steht, ist dunkler, und eine Fläche, die von der Sonne weg zeigt, bekommt nur das Licht, das von überall her kommt.

Um das zu berechnen, muss das Programm für jede Fläche wissen, wohin sie zeigt. Diese Richtung heißt **Normale**: ein Vektor der Länge 1, der senkrecht aus der Fläche herausragt. Die Oberseite eines Blocks hat die Normale (0, 1, 0), die rechte Seite (1, 0, 0). `DrawCube` verrät uns diese Richtungen nicht. Deshalb zeichnen wir die Blöcke ab jetzt selbst, Seite für Seite.

### Seiten statt Würfel

raylib zeichnet alles aus Dreiecken. Mit den Befehlen aus `rlgl.h` können wir der Grafikkarte die Ecken selbst schicken: `rlBegin(RL_QUADS)` sagt, dass immer vier Ecken ein Viereck bilden, `rlColor4ub` und `rlNormal3f` gelten für die folgenden Ecken, `rlVertex3f` ist eine Ecke. Schreibe oben dazu:

```c
#include <rlgl.h>
```

und unter `sichtbar` diese beiden Funktionen:

```c
// Eine Seite eines Blocks: vier Ecken, eine Richtung (die Normale), eine Farbe
void seite(Vector3 a, Vector3 b, Vector3 c, Vector3 d, Vector3 n, Color f)
{
    rlColor4ub(f.r, f.g, f.b, 255);
    rlNormal3f(n.x, n.y, n.z);
    rlVertex3f(a.x, a.y, a.z);
    rlVertex3f(b.x, b.y, b.z);
    rlVertex3f(c.x, c.y, c.z);
    rlVertex3f(d.x, d.y, d.z);
}

// Zeichnet nur die Seiten, an die Luft grenzt: Die anderen sieht man nie.
int zeichne_block(int x, int y, int z)
{
    int art = welt[x][y][z];
    // Jeder Block ist ein wenig heller oder dunkler, dann wirkt die Welt
    // weniger glatt
    float schwankung = 0.92f + 0.08f * (((x * 7 + y * 13 + z * 31) % 5) / 4.0f);
    Color f = ColorBrightness(farben[art], schwankung - 1.0f);

    float x0 = x, x1 = x + 1, y0 = y, y1 = y + 1, z0 = z, z1 = z + 1;
    int seiten = 0;
    rlBegin(RL_QUADS);
    if (!fest(x, y + 1, z)) {   // oben
        seite((Vector3){x0, y1, z0}, (Vector3){x0, y1, z1}, (Vector3){x1, y1, z1}, (Vector3){x1, y1, z0}, (Vector3){0, 1, 0}, f);
        seiten++;
    }
    if (!fest(x, y - 1, z) && y > 0) {   // unten
        seite((Vector3){x0, y0, z0}, (Vector3){x1, y0, z0}, (Vector3){x1, y0, z1}, (Vector3){x0, y0, z1}, (Vector3){0, -1, 0}, f);
        seiten++;
    }
    if (!fest(x + 1, y, z) && x < BREITE - 1) {   // rechts (+x)
        seite((Vector3){x1, y0, z0}, (Vector3){x1, y1, z0}, (Vector3){x1, y1, z1}, (Vector3){x1, y0, z1}, (Vector3){1, 0, 0}, f);
        seiten++;
    }
    if (!fest(x - 1, y, z) && x > 0) {   // links (-x)
        seite((Vector3){x0, y0, z0}, (Vector3){x0, y0, z1}, (Vector3){x0, y1, z1}, (Vector3){x0, y1, z0}, (Vector3){-1, 0, 0}, f);
        seiten++;
    }
    if (!fest(x, y, z + 1) && z < TIEFE - 1) {   // vorne (+z)
        seite((Vector3){x0, y0, z1}, (Vector3){x1, y0, z1}, (Vector3){x1, y1, z1}, (Vector3){x0, y1, z1}, (Vector3){0, 0, 1}, f);
        seiten++;
    }
    if (!fest(x, y, z - 1) && z > 0) {   // hinten (-z)
        seite((Vector3){x0, y0, z0}, (Vector3){x0, y1, z0}, (Vector3){x1, y1, z0}, (Vector3){x1, y0, z0}, (Vector3){0, 0, -1}, f);
        seiten++;
    }
    rlEnd();
    return seiten;
}
```

- Jede der sechs Seiten wird nur gezeichnet, wenn auf ihrer Seite kein fester Block liegt. Zwei Blöcke nebeneinander zeigen sich ihre Seiten nie, die zeichnen wir auch nicht. Am Rand der Welt sagt `fest` »Wand«; damit die Schichten dort trotzdem zu sehen sind, prüft `zeichne_block` den Rand eigens.
- Die Reihenfolge der vier Ecken ist nicht beliebig: Von außen gesehen laufen sie gegen den Uhrzeigersinn. So erkennt die Grafikkarte die Vorderseite eines Vierecks und lässt Rückseiten weg.
- `schwankung` macht jeden Block ein wenig heller oder dunkler, je nach seiner Position. Dann wirkt eine Wiese nicht wie eine einzige grüne Fläche.

_task(wiese)

### Ein Shader rechnet das Licht

Wie hell eine Fläche wird, berechnet die Grafikkarte, und zwar mit einem kleinen Programm, einem **Shader**. Shader sind in einer eigenen Sprache geschrieben, GLSL, die ein wenig wie C aussieht. In der Vorlage liegen schon zwei: `licht.vs` und `licht.fs`. Der **Vertex-Shader** `licht.vs` läuft für jede Ecke eines Dreiecks und rechnet aus, wo die Ecke auf dem Bildschirm landet. Der **Fragment-Shader** `licht.fs` läuft für jeden Pixel einer Fläche und rechnet seine Farbe aus:

```glsl
#version 100
precision mediump float;

// Vom Vertex-Shader, für jeden Pixel einer Fläche
varying vec3 position;
varying vec3 normale;
varying vec4 farbe;

uniform vec3 sonne;         // Richtung, aus der die Sonne scheint
uniform vec3 sonnenlicht;   // Farbe und Stärke des Sonnenlichts
uniform vec3 umgebung;      // Licht, das von überall kommt

void main()
{
    vec3 n = normalize(normale);

    // Wie direkt trifft die Sonne diese Fläche? 1 = senkrecht, 0 = gar nicht
    float sonnig = max(dot(n, sonne), 0.0);
    vec3 licht = umgebung + sonnenlicht * sonnig;

    gl_FragColor = vec4(farbe.rgb * licht, 1.0);
}
```

Die wichtigste Zeile ist `dot(n, sonne)`, das **Skalarprodukt** der Normale und der Richtung zur Sonne. Für zwei Vektoren der Länge 1 ist es 1, wenn sie in dieselbe Richtung zeigen, 0, wenn sie im rechten Winkel stehen, und negativ, wenn sie voneinander weg zeigen; `max(…, 0.0)` macht daraus 0. Dazwischen ist es der Kosinus des Winkels. Genau so viel Sonnenlicht kommt auf einer Fläche an: Je schräger die Sonne auf eine Fläche scheint, desto mehr verteilt sich ihr Licht. Zu diesem Sonnenlicht kommt `umgebung`, das Licht, das vom Himmel kommt und auch die Schattenseiten erreicht.

`uniform` sind Werte, die das C-Programm dem Shader gibt, für alle Pixel gleich. `varying` sind Werte, die der Vertex-Shader für jede Ecke ausrechnet und die Grafikkarte für jeden Pixel dazwischen mischt.

_task(sonnig)

Jetzt muss `main` den Shader laden, ihm die Werte geben und mit ihm zeichnen. `make` packt die beiden Dateien schon mit in die Webseite, so dass `LoadShader` sie findet. Vor die Schleife kommt:

```c
Shader licht = LoadShader("licht.vs", "licht.fs");
int ort_sonne = GetShaderLocation(licht, "sonne");
int ort_sonnenlicht = GetShaderLocation(licht, "sonnenlicht");
int ort_umgebung = GetShaderLocation(licht, "umgebung");
```

`GetShaderLocation` sucht für jeden Namen eines `uniform` die Stelle, an der der Shader ihn erwartet. In der Schleife, nach `bewege_figur(dt);`:

```c
// Die Sonne steht schräg über der Welt
Vector3 sonne = Vector3Normalize((Vector3){ 0.5f, 0.8f, 0.3f });
Vector3 sonnenlicht = { 0.75f, 0.7f, 0.6f };
Vector3 umgebung = { 0.43f, 0.46f, 0.54f };
```

und nach `ziele(augen, blick);`:

```c
SetShaderValue(licht, ort_sonne, &sonne, SHADER_UNIFORM_VEC3);
SetShaderValue(licht, ort_sonnenlicht, &sonnenlicht, SHADER_UNIFORM_VEC3);
SetShaderValue(licht, ort_umgebung, &umgebung, SHADER_UNIFORM_VEC3);
```

Zuletzt ersetzt du die Schleife, die die Welt zeichnet, durch eine, die `zeichne_block` aufruft, eingerahmt von `BeginShaderMode` und `EndShaderMode`:

```c
BeginShaderMode(licht);
int seiten = 0;
for (int x = 0; x < BREITE; x++)
    for (int y = 0; y < HOEHE; y++)
        for (int z = 0; z < TIEFE; z++)
            if (welt[x][y][z] != LUFT)
                seiten += zeichne_block(x, y, z);
EndShaderMode();
```

Die Zeile mit den FPS zählt jetzt Seiten statt Blöcke; ändere sie zu:

```c
DrawText(TextFormat("%d Seiten  %d FPS", seiten, GetFPS()), 10, 10, 10, WHITE);
```

Die Funktion `sichtbar` brauchst du nicht mehr; `zeichne_block` prüft jetzt jede Seite einzeln. Vor `CloseWindow()` gibt `UnloadShader(licht);` den Shader wieder frei.

<!-- tutorial-screenshot
write-file: main.c <- file:stufen/6.c
terminal-run: make
terminal-wait-for-prompt: 120
tab: preview
viewport: 960x540
hook: open main.html
hook: play 10
hook: settle
crop-element: #canvas
-->
<img class='full' src='licht.webp' alt='Dieselbe Welt mit Licht: Oberseiten hell, Seiten dunkler, die Bäume haben deutliche Schattenseiten.'>

Vergleiche die Zahl der Seiten und die FPS mit vorher. Jetzt siehst du auch, warum die Ecken der Vierecke gegen den Uhrzeigersinn laufen müssen: Probiere bei einer Seite die umgekehrte Reihenfolge aus.

_task(licht)

## Tag und Nacht

### Die Sonne wandert

Bisher steht die Sonne still. Mit einer Variable `tageszeit` zwischen 0 und 1 (0 und 1 sind Mitternacht, 0,5 ist Mittag) kann sie wandern. Schreibe über `main`:

```c
float tageszeit = 0.4f;  // 0 und 1: Mitternacht, 0,5: Mittag
```

Und ersetze in der Schleife die Zeilen der festen Sonne durch:

```c
// Die Zeit vergeht: ein Tag dauert zwei Minuten, mit T schneller
tageszeit += dt / 120.0f + (IsKeyDown(KEY_T) ? dt * 0.2f : 0);
tageszeit -= floorf(tageszeit);
if (IsKeyPressed(KEY_N))   // N springt zwischen Tag und Nacht
    tageszeit = (tageszeit > 0.25f && tageszeit < 0.75f) ? 0.9f : 0.4f;
float winkel = (tageszeit - 0.25f) * 2 * PI;   // Sonnenaufgang bei 0,25
Vector3 sonne = Vector3Normalize((Vector3){ cosf(winkel), sinf(winkel), 0.4f });
float tag = Clamp(sinf(winkel) * 4.0f + 0.3f, 0.0f, 1.0f);
Color himmel = ColorLerp((Color){ 12, 16, 36, 255 }, (Color){ 140, 195, 240, 255 }, tag);
Vector3 himmel_v = { himmel.r / 255.0f, himmel.g / 255.0f, himmel.b / 255.0f };
Vector3 sonnenlicht = { 0.75f * tag, 0.7f * tag, 0.6f * tag };
// Nachts bleibt bläuliches Mondlicht
Vector3 umgebung = { 0.2f + 0.23f * tag, 0.22f + 0.24f * tag, 0.34f + 0.2f * tag };
```

- Die Sonne läuft auf einem Kreis um die Welt: `cosf(winkel)` und `sinf(winkel)` sind ihre Position auf dem Kreis, so wie bei der Blickrichtung. Um 0,25 geht sie auf, um 0,75 unter.
- `tag` ist 1, solange die Sonne hoch genug steht, und 0 in der Nacht. Damit mischt `ColorLerp` die Farbe des Himmels zwischen Nachtblau und Tagblau, und das Sonnenlicht wird nachts zu 0. Ein schwaches, bläuliches Mondlicht bleibt als `umgebung`.
- <kbd>T</kbd> lässt die Zeit schneller laufen, <kbd>N</kbd> springt zwischen Tag und Nacht.

Statt `ClearBackground(SKYBLUE)` steht jetzt `ClearBackground(himmel)`.

### Fackeln

Nachts ist es dunkel, Zeit für Licht. Eine Fackel ist eine neue Blockart, die du bauen kannst. Sie ist nicht fest, du kannst durch sie hindurchgehen. Ergänze das `enum`, `namen` und `farben` um eine siebte Art, `FACKEL`:

```c
enum { LUFT, GRAS, ERDE, STEIN, HOLZ, LAUB, SAND, FACKEL, ANZAHL_ARTEN };
```

```c
{ 255, 200, 80, 255 },    // Fackel
```

In `fest` ist eine Fackel jetzt wie Luft:

```c
bool fest(int x, int y, int z)
{
    if (x < 0 || z < 0 || x >= BREITE || z >= TIEFE || y < 0)
        return true;
    int art = block(x, y, z);
    return art != LUFT && art != FACKEL;
}
```

Eine Fackel leuchtet für ihre Umgebung. Dafür sucht das Programm in jedem Bild alle Fackeln der Welt und gibt ihre Positionen dem Shader, als Liste von höchstens 16 Punkten. Das kommt in der Schleife vor die Zeilen mit `SetShaderValue`:

```c
// Fackeln einsammeln: Sie leuchten im Shader
Vector3 fackeln[16];
int anzahl = 0;
for (int x = 0; x < BREITE; x++)
    for (int y = 0; y < HOEHE; y++)
        for (int z = 0; z < TIEFE; z++)
            if (welt[x][y][z] == FACKEL && anzahl < 16)
                fackeln[anzahl++] = (Vector3){ x + 0.5f, y + 0.7f, z + 0.5f };
```

Die Fackeln selbst zeichnen wir nach `EndShaderMode()` ohne den Licht-Shader: Was selbst leuchtet, braucht kein Licht von außen. Ein Stiel aus Holz und oben eine Flamme:

```c
// Fackeln leuchten selbst: ohne Licht-Shader gezeichnet
for (int i = 0; i < anzahl; i++) {
    Vector3 p = fackeln[i];
    DrawCube((Vector3){ p.x, p.y - 0.4f, p.z }, 0.15f, 0.6f, 0.15f, (Color){ 120, 80, 40, 255 });
    DrawCube((Vector3){ p.x, p.y - 0.05f, p.z }, 0.22f, 0.18f, 0.22f, farben[FACKEL]);
}
```

Und in der Schleife, die die Welt zeichnet, werden Fackeln nicht als Block gezeichnet:

```c
if (welt[x][y][z] != LUFT && welt[x][y][z] != FACKEL)
```

### Nebel

Weit entfernte Dinge sehen in der Wirklichkeit blasser aus, weil Luft nie ganz klar ist. Das hilft dem Auge, Entfernungen zu schätzen, und versteckt nebenbei den Rand der Welt. Der Shader mischt dafür die Farbe eines Pixels mit der Farbe des Himmels, je weiter er von den Augen entfernt ist.

Hier ist der ganze neue Fragment-Shader mit Fackeln und Nebel. Ersetze `licht.fs` durch:

```glsl
#version 100
precision mediump float;

varying vec3 position;
varying vec3 normale;
varying vec4 farbe;

uniform vec3 sonne;         // Richtung, aus der die Sonne scheint
uniform vec3 sonnenlicht;   // Farbe und Stärke des Sonnenlichts
uniform vec3 umgebung;      // Licht, das von überall kommt
uniform vec3 himmel;        // Farbe des Himmels, für den Nebel
uniform vec3 augen;         // wo die Kamera ist

#define MAX_FACKELN 16
uniform vec3 fackeln[MAX_FACKELN];
uniform int anzahlFackeln;

void main()
{
    vec3 n = normalize(normale);

    // Wie direkt trifft die Sonne diese Fläche? 1 = senkrecht, 0 = gar nicht
    float sonnig = max(dot(n, sonne), 0.0);
    vec3 licht = umgebung + sonnenlicht * sonnig;

    // Jede Fackel leuchtet warm, je näher, desto heller
    for (int i = 0; i < MAX_FACKELN; i++) {
        if (i >= anzahlFackeln) break;
        vec3 zurFackel = fackeln[i] - position;
        float abstand = length(zurFackel);
        float zugewandt = max(dot(n, zurFackel / abstand), 0.0);
        licht += vec3(1.0, 0.6, 0.3) * (0.3 + 0.7 * zugewandt) / (1.0 + 0.3 * abstand * abstand);
    }

    vec3 ergebnis = farbe.rgb * min(licht, vec3(1.4));

    // Nebel: weit Entferntes verschwimmt mit dem Himmel
    float nebel = clamp((length(position - augen) - 16.0) / 20.0, 0.0, 1.0);
    gl_FragColor = vec4(mix(ergebnis, himmel, nebel), 1.0);
}
```

Jede Fackel gibt warmes, orangefarbenes Licht. Wie bei der Sonne zählt, wie sehr eine Fläche der Fackel zugewandt ist; dazu kommt der Abstand: Das Licht wird mit `1.0 + 0.3 * abstand * abstand` geteilt, also schwächer, je weiter die Fläche entfernt ist. `min(licht, vec3(1.4))` verhindert, dass eine Fläche zwischen vielen Fackeln weiß ausbrennt.

Damit der Shader die neuen Werte bekommt, braucht `main` vier weitere Stellen:

```c
int ort_himmel = GetShaderLocation(licht, "himmel");
int ort_augen = GetShaderLocation(licht, "augen");
int ort_fackeln = GetShaderLocation(licht, "fackeln");
int ort_anzahl = GetShaderLocation(licht, "anzahlFackeln");
```

und in der Schleife nach den anderen `SetShaderValue`:

```c
SetShaderValue(licht, ort_himmel, &himmel_v, SHADER_UNIFORM_VEC3);
SetShaderValue(licht, ort_augen, &augen, SHADER_UNIFORM_VEC3);
SetShaderValueV(licht, ort_fackeln, fackeln, SHADER_UNIFORM_VEC3, anzahl > 0 ? anzahl : 1);
SetShaderValue(licht, ort_anzahl, &anzahl, SHADER_UNIFORM_INT);
```

`SetShaderValueV` gibt dem Shader eine ganze Liste auf einmal. Steht keine Fackel in der Welt, schicken wir trotzdem einen Eintrag, denn eine Liste mit null Einträgen kennt der Shader nicht; `anzahlFackeln` ist dann 0, und die Schleife im Shader hört sofort auf.

<!-- tutorial-screenshot
write-file: main.c <- file:stufen/7.c
write-file: licht.fs <- file:stufen/7.fs
terminal-run: make
terminal-wait-for-prompt: 120
tab: preview
viewport: 960x540
hook: open main.html
hook: play 8 ArrowDown:0.25 Digit7 Rechtsklick ArrowLeft:0.35 Rechtsklick ArrowRight:0.7 Rechtsklick ArrowLeft:0.35 ArrowUp:0.15 KeyS:0.9
hook: settle
variants: dark
crop-element: #canvas
-->
<img class='full' src='fackeln.webp' alt='Blick über die Wiese, drei Fackeln stehen im Gras; im dunklen Modus bei Nacht, die Fackeln leuchten.'>

Drück <kbd>N</kbd>: Es wird Nacht, und deine Fackeln leuchten. Auf dieser Seite zeigt der dunkle Modus die Bilder ebenfalls bei Nacht.

_task(fackel-abstand)

_task(fackeln)

## Texturen

Noch hat jeder Block eine einzige Farbe. In Spielen sehen Flächen nach etwas aus: Gras hat Halme, Stein hat Risse, Holz hat Maserung. Dafür klebt die Grafikkarte ein Bild auf jede Fläche, eine **Textur**.

### Ein Streifen aus Kacheln

In der Vorlage liegt `texturen.png`, ein Bild aus acht Kacheln von 16 × 16 Pixeln nebeneinander:

<img src='vorlage/texturen.png' data-noconvert style='display: block; width: 100%; max-width: 48em; image-rendering: pixelated; margin: 1em 0;' alt='Acht Kacheln nebeneinander: Gras von oben, Gras von der Seite mit grünem Rand, Erde, Stein, Rinde, Jahresringe, Laub, Sand.'>

Gras bekommt oben die erste Kachel, an den Seiten die zweite mit dem grünen Rand und unten Erde. Ein Holzblock zeigt an den Seiten Rinde und oben die Jahresringe. Die Reihenfolge der Kacheln steht als `enum` im Programm, und `kachel` sagt, welche Kachel eine Seite bekommt. Schreibe beides unter `welt`:

```c
// ---------------------------------------------------------------- Texturen

// texturen.png ist ein Streifen aus Kacheln, alle gleich groß, in dieser
// Reihenfolge von links nach rechts
enum { K_GRAS_OBEN, K_GRAS_SEITE, K_ERDE, K_STEIN, K_HOLZ_SEITE, K_HOLZ_OBEN,
       K_LAUB, K_SAND, ANZAHL_KACHELN };

// Welche Kachel bekommt eine Seite? richtung: 0 oben, 1 unten, 2 seitlich
int kachel(int art, int richtung)
{
    switch (art) {
    case GRAS: return richtung == 0 ? K_GRAS_OBEN : richtung == 1 ? K_ERDE : K_GRAS_SEITE;
    case ERDE: return K_ERDE;
    case STEIN: return K_STEIN;
    case HOLZ: return richtung == 2 ? K_HOLZ_SEITE : K_HOLZ_OBEN;
    case LAUB: return K_LAUB;
    default: return K_SAND;
    }
}
```

### Texturkoordinaten

Welcher Teil des Bildes auf welche Ecke kommt, sagen **Texturkoordinaten**: zwei Zahlen u und v von 0 bis 1. u = 0 ist der linke Rand des ganzen Bildes, u = 1 der rechte, v = 0 oben und v = 1 unten. Die Kachel Nummer `k` reicht von u = k / 8 bis u = (k + 1) / 8. Dazwischen mischt die Grafikkarte, für jeden Pixel der Fläche.

Damit eine Kachel nicht auf dem Kopf steht, müssen die Ecken einer Seite immer in derselben Reihenfolge kommen: von außen gesehen links unten, rechts unten, rechts oben, links oben. Das ist auch gegen den Uhrzeigersinn, wie bisher. Ersetze `seite` und `zeichne_block`:

```c
// Eine Seite eines Blocks: vier Ecken, von außen gesehen links unten, rechts
// unten, rechts oben, links oben; dazu die Richtung (Normale), eine
// Helligkeit und die Kachel aus der Textur
void seite(Vector3 lu, Vector3 ru, Vector3 ro, Vector3 lo, Vector3 n, Color f, int k)
{
    float u0 = (float)k / ANZAHL_KACHELN, u1 = (float)(k + 1) / ANZAHL_KACHELN;
    rlColor4ub(f.r, f.g, f.b, 255);
    rlNormal3f(n.x, n.y, n.z);
    rlTexCoord2f(u0, 1); rlVertex3f(lu.x, lu.y, lu.z);
    rlTexCoord2f(u1, 1); rlVertex3f(ru.x, ru.y, ru.z);
    rlTexCoord2f(u1, 0); rlVertex3f(ro.x, ro.y, ro.z);
    rlTexCoord2f(u0, 0); rlVertex3f(lo.x, lo.y, lo.z);
}

// Zeichnet nur die Seiten, an die Luft grenzt: Die anderen sieht man nie.
int zeichne_block(int x, int y, int z)
{
    int art = welt[x][y][z];
    // Jeder Block ist ein wenig heller oder dunkler, dann wirkt die Welt
    // weniger glatt
    float schwankung = 0.92f + 0.08f * (((x * 7 + y * 13 + z * 31) % 5) / 4.0f);
    Color f = ColorBrightness(WHITE, schwankung - 1.0f);
    int oben = kachel(art, 0), unten = kachel(art, 1), seitlich = kachel(art, 2);

    float x0 = x, x1 = x + 1, y0 = y, y1 = y + 1, z0 = z, z1 = z + 1;
    int seiten = 0;
    rlBegin(RL_QUADS);
    if (!fest(x, y + 1, z)) {   // oben
        seite((Vector3){x0, y1, z1}, (Vector3){x1, y1, z1}, (Vector3){x1, y1, z0}, (Vector3){x0, y1, z0}, (Vector3){0, 1, 0}, f, oben);
        seiten++;
    }
    if (!fest(x, y - 1, z) && y > 0) {   // unten
        seite((Vector3){x0, y0, z0}, (Vector3){x1, y0, z0}, (Vector3){x1, y0, z1}, (Vector3){x0, y0, z1}, (Vector3){0, -1, 0}, f, unten);
        seiten++;
    }
    if (!fest(x + 1, y, z) && x < BREITE - 1) {   // rechts (+x)
        seite((Vector3){x1, y0, z1}, (Vector3){x1, y0, z0}, (Vector3){x1, y1, z0}, (Vector3){x1, y1, z1}, (Vector3){1, 0, 0}, f, seitlich);
        seiten++;
    }
    if (!fest(x - 1, y, z) && x > 0) {   // links (-x)
        seite((Vector3){x0, y0, z0}, (Vector3){x0, y0, z1}, (Vector3){x0, y1, z1}, (Vector3){x0, y1, z0}, (Vector3){-1, 0, 0}, f, seitlich);
        seiten++;
    }
    if (!fest(x, y, z + 1) && z < TIEFE - 1) {   // vorne (+z)
        seite((Vector3){x0, y0, z1}, (Vector3){x1, y0, z1}, (Vector3){x1, y1, z1}, (Vector3){x0, y1, z1}, (Vector3){0, 0, 1}, f, seitlich);
        seiten++;
    }
    if (!fest(x, y, z - 1) && z > 0) {   // hinten (-z)
        seite((Vector3){x1, y0, z0}, (Vector3){x0, y0, z0}, (Vector3){x0, y1, z0}, (Vector3){x1, y1, z0}, (Vector3){0, 0, -1}, f, seitlich);
        seiten++;
    }
    rlEnd();
    return seiten;
}
```

Die Farbe einer Ecke ist jetzt Weiß, ein wenig heller oder dunkler: Die Textur bringt die Farbe mit, und Weiß lässt sie, wie sie ist.

### Die Textur laden

Vor die Schleife kommt, gleich nach `LoadShader`:

```c
Texture2D textur = LoadTexture("texturen.png");
SetTextureFilter(textur, TEXTURE_FILTER_POINT);   // scharfe Pixel
```

`TEXTURE_FILTER_POINT` sorgt für scharfe Pixel. Ohne diese Zeile würde die Grafikkarte zwischen den Pixeln weich überblenden, und die Blöcke sähen verschwommen aus. Beim Zeichnen sagt `rlSetTexture`, welche Textur die Flächen bekommen:

```c
BeginShaderMode(licht);
rlSetTexture(textur.id);
int seiten = 0;
for (int x = 0; x < BREITE; x++)
    for (int y = 0; y < HOEHE; y++)
        for (int z = 0; z < TIEFE; z++)
            if (welt[x][y][z] != LUFT && welt[x][y][z] != FACKEL)
                seiten += zeichne_block(x, y, z);
rlSetTexture(0);
EndShaderMode();
```

Vor `UnloadShader(licht);` am Ende kommt `UnloadTexture(textur);`. `make` packt `texturen.png` mit in die Webseite, so wie die Shader.

### Der Shader liest die Textur

Der Vertex-Shader reicht die Texturkoordinaten an jeden Pixel weiter. In `licht.vs` kommen drei Zeilen dazu:

```glsl
attribute vec2 vertexTexCoord;
varying vec2 uv;           // wo in der Textur dieser Punkt liegt
    uv = vertexTexCoord;
```

Die ersten beiden gehören zu den anderen `attribute` und `varying` oben, die dritte in `main`. Im Fragment-Shader `licht.fs` kommen `varying vec2 uv;` und die Textur dazu:

```glsl
uniform sampler2D texture0;  // die Textur mit allen Kacheln
```

`texture2D` liest an der Stelle `uv` die Farbe der Textur. Ersetze die Zeile mit `ergebnis` durch:

```glsl
vec3 pixel = texture2D(texture0, uv).rgb;
vec3 ergebnis = pixel * farbe.rgb * min(licht, vec3(1.4));
```

Licht, Fackeln und Nebel rechnen weiter wie vorher; sie hellen jetzt die Pixel der Textur auf oder dunkeln sie ab.

<!-- tutorial-screenshot
write-file: main.c <- file:stufen/8.c
write-file: licht.vs <- file:stufen/8.vs
write-file: licht.fs <- file:stufen/8.fs
terminal-run: make
terminal-wait-for-prompt: 120
tab: preview
viewport: 960x540
hook: open main.html
hook: play 8 ArrowDown:0.25 Digit7 Rechtsklick ArrowLeft:0.35 Rechtsklick ArrowRight:0.7 Rechtsklick ArrowLeft:0.35 ArrowUp:0.15 KeyS:0.9
hook: settle
variants: dark
crop-element: #canvas
-->
<img class='full' src='tag.webp' alt='Blick über eine hügelige Welt aus Blöcken mit Gras-, Erd- und Holztexturen, ein paar Fackeln stehen im Gras.'>

_task(texturen)

### Eigene Texturen zeichnen

Die Kacheln in `texturen.png` sind nur ein Anfang. Zeichne deine eigenen im [2D Game Studio](https://2d.hackschule.de/):

1. Lege ein neues Sprite an, zum Beispiel »Kacheln«. Unter »Funktionen« › »Sprite« › »Größe ändern« stellst du 16 × 16 Pixel ein; andere Größen gehen auch, solange alle Kacheln quadratisch sind.
2. Zeichne die acht Kacheln als acht Frames desselben Zustands, in der Reihenfolge von oben: Gras von oben, Gras von der Seite, Erde, Stein, Rinde, Jahresringe, Laub, Sand. Unter der Zeichenfläche stehen die Frames nebeneinander; mit dem Plus kommt ein neuer dazu.

<!-- tutorial-screenshot
# Das 2D Game Studio mit den Kacheln der Vorlage als acht Frames (eingefügt
# wie mit »Bilder öffnen …«, nichts wird gespeichert)
open-site: /
tab: preview
viewport: 1400x820
hook: studio raylib/vorlage/texturen.png
-->
<img class='full' src='studio-kacheln.webp' alt='Das 2D Game Studio: ein Sprite »Kacheln« mit 16 × 16 Pixeln, auf der Zeichenfläche die Graskachel, darunter die acht Kacheln als Frames.'>

3. Wähle »Funktionen« › »Sprite« › »Als Bild speichern« › »alle Frames dieses Zustands nebeneinander«. Dein Browser speichert einen Streifen wie `kacheln-zustand-1.png`.

<!-- tutorial-screenshot
tab: preview
viewport: 1400x820
hook: studio-menu
-->
<img class='full' src='studio-speichern.webp' alt='Im 2D Game Studio ist das Menü »Funktionen« › »Sprite« › »Als Bild speichern« offen, ausgewählt ist »alle Frames dieses Zustands nebeneinander«.'>

4. Ziehe die Datei aus deinem Download-Ordner in den Explorer des Workspace, in den Ordner `raylib-starter`, und nenne sie `texturen.png`. Die alte Datei wird dabei ersetzt; die Vorlage hat sie noch, falls du sie zurück willst.
5. `make`, und deine Welt sieht aus wie gezeichnet.

<div class='hint'>
Eine Kachel wird an alle Seiten eines Blocks geklebt, und viele Blöcke liegen nebeneinander. Was am rechten Rand einer Kachel passiert, sollte deshalb zum linken Rand passen, und oben zu unten. Sonst siehst du auf einer Wiese ein Gitter aus Kanten.
</div>

## Deine Welt

Die Welt gehört jetzt dir. Ein paar Ideen, von leicht bis schwer:

- **Eigene Blockarten:** Glas, Schnee, Ziegel, Gold. Eine neue Art braucht einen Namen in `enum` und `namen`, eine Farbe in `farben` und eine Kachel in `texturen.png` und in `kachel`.
- **Eine andere Landschaft:** Ändere die Wellen in `erzeuge_welt`, setze Berge, einen See aus blauen Blöcken oder einen Wald mit mehr Bäumen. Eine andere Zahl bei `SetRandomSeed` verteilt die Bäume anders.
- **Höhlen:** Lass in `erzeuge_welt` Luft in den Stein, wo eine weitere Welle über einem Wert liegt.
- **Farbiges Licht:** Gib den Fackeln eine andere Farbe, oder lass den Himmel bei Sonnenaufgang und -untergang rot leuchten.
- **Fliegen:** Mit einer Taste schaltest du die Schwerkraft aus, und <kbd>Leertaste</kbd> und <kbd>Shift</kbd> fliegen hoch und runter.
- **Ein Spiel daraus machen:** Sammle zehn versteckte Goldblöcke, so schnell du kannst. Die Zeit zeigt `GetTime()`, gezählt wird, wenn du einen Goldblock abbaust.

_task(abgabe)

## Wie geht es weiter?

Du hast eine Welt gebaut, die auf dich reagiert: eine Kamera, die deinen Augen folgt, eine Figur mit Körper und Schwerkraft, einen Strahl zum Zielen und einen Shader für Licht, Tag, Nacht und Nebel. Genau diese Bausteine stecken auch in großen Spielen; dort heißen sie Kamera, Physik, Raycasting und Beleuchtung.

Alle Befehle von raylib, die in diesem Projekt vorkommen, und ein paar mehr findest du im [Handbuch](/reference/raylib). Die vollständige Liste aller Funktionen steht im [raylib-Cheatsheet](https://www.raylib.com/cheatsheet/cheatsheet.html), und unter [raylib.com/examples](https://www.raylib.com/examples.html) gibt es Hunderte Beispiele, die du direkt im Browser ausprobieren kannst.
