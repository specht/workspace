#include <math.h>
#include <stdlib.h>
#include <raylib.h>
#include <raymath.h>

// ---------------------------------------------------------------- Die Welt

#define BREITE 48   // x
#define HOEHE 24    // y, nach oben
#define TIEFE 48    // z

enum { LUFT, GRAS, ERDE, STEIN, HOLZ, LAUB, SAND, ANZAHL_ARTEN };

const char *namen[ANZAHL_ARTEN] = {
    "Luft", "Gras", "Erde", "Stein", "Holz", "Laub", "Sand",
};

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

// Kann man an dieser Stelle nicht hindurch? Am Rand und unter der Welt stehen
// unsichtbare Wände.
bool fest(int x, int y, int z)
{
    if (x < 0 || z < 0 || x >= BREITE || z >= TIEFE || y < 0)
        return true;
    return block(x, y, z) != LUFT;
}

// Grenzt der Block an Luft? Nur dann kann man ihn sehen.
bool sichtbar(int x, int y, int z)
{
    return block(x + 1, y, z) == LUFT || block(x - 1, y, z) == LUFT
        || block(x, y + 1, z) == LUFT || block(x, y - 1, z) == LUFT
        || block(x, y, z + 1) == LUFT || block(x, y, z - 1) == LUFT;
}

// --------------------------------------------------------------- Die Figur

Vector3 fuesse;          // Mitte der Fußsohlen
float vy = 0;            // Geschwindigkeit nach oben, in Blöcken pro Sekunde
bool am_boden = false;
float drehung = 0;       // nach links und rechts, im Bogenmaß
float neigung = 0;       // nach oben und unten

#define RADIUS 0.3f      // die Figur ist 0,6 Blöcke breit
#define GROESSE 1.7f
#define AUGENHOEHE 1.6f

// Steckt eine Figur mit diesen Füßen in einem festen Block?
bool kollidiert(Vector3 f)
{
    for (int x = (int)floorf(f.x - RADIUS); x <= (int)floorf(f.x + RADIUS); x++)
        for (int y = (int)floorf(f.y); y <= (int)floorf(f.y + GROESSE); y++)
            for (int z = (int)floorf(f.z - RADIUS); z <= (int)floorf(f.z + RADIUS); z++)
                if (fest(x, y, z))
                    return true;
    return false;
}

Vector3 blickrichtung(void)
{
    return (Vector3){
        sinf(drehung) * cosf(neigung),
        sinf(neigung),
        cosf(drehung) * cosf(neigung),
    };
}

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

    // Jede Richtung einzeln: So gleitet man an einer Wand entlang, statt an
    // ihr kleben zu bleiben.
    Vector3 neu = fuesse;
    neu.x += wunsch.x;
    if (!kollidiert(neu)) fuesse.x = neu.x;
    neu = fuesse;
    neu.z += wunsch.z;
    if (!kollidiert(neu)) fuesse.z = neu.z;

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
}

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

int main(void)
{
    InitWindow(960, 540, "Klötzchenwelt");
    SetTargetFPS(60);
    SetExitKey(KEY_NULL);   // Esc gibt nur die Maus frei

    erzeuge_welt();
    fuesse = (Vector3){ 8.5f, HOEHE - 2, 10.5f };
    drehung = 0.8f;      // schräg über die Welt schauen
    neigung = -0.2f;

    Camera3D kamera = { 0 };
    kamera.up = (Vector3){ 0, 1, 0 };
    kamera.fovy = 70;
    kamera.projection = CAMERA_PERSPECTIVE;

    int auswahl = GRAS;

    while (!WindowShouldClose())
    {
        float dt = GetFrameTime();
        if (dt > 0.05f) dt = 0.05f;

        // Ein Klick ins Bild fängt die Maus, Esc gibt sie frei
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

        bewege_figur(dt);

        Vector3 augen = { fuesse.x, fuesse.y + AUGENHOEHE, fuesse.z };
        Vector3 blick = blickrichtung();
        kamera.position = augen;
        kamera.target = Vector3Add(augen, blick);
        ziele(augen, blick);

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
        if (getroffen)
            DrawCubeWires((Vector3){ zx + 0.5f, zy + 0.5f, zz + 0.5f }, 1.01f, 1.01f, 1.01f, BLACK);
        EndMode3D();

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
        DrawText(TextFormat("%d Blöcke  %d FPS", bloecke, GetFPS()), 10, 10, 20, DARKBLUE);
        if (!IsCursorHidden() && GetTime() < 8)
            DrawText("Klick ins Bild: Maus fangen. WASD laufen, Pfeiltasten umsehen.", 10, 36, 20, WHITE);
        EndDrawing();
    }

    CloseWindow();
    return 0;
}
