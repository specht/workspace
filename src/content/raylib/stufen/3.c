#include <math.h>
#include <stdlib.h>
#include <raylib.h>
#include <raymath.h>

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

// Grenzt der Block an Luft? Nur dann kann man ihn sehen.
bool sichtbar(int x, int y, int z)
{
    return block(x + 1, y, z) == LUFT || block(x - 1, y, z) == LUFT
        || block(x, y + 1, z) == LUFT || block(x, y - 1, z) == LUFT
        || block(x, y, z + 1) == LUFT || block(x, y, z - 1) == LUFT;
}

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
