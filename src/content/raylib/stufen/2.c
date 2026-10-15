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
