#include <math.h>
#include <stdlib.h>
#include <raylib.h>
#include <raymath.h>

// ---------------------------------------------------------------- Die Welt

#define WIDTH 48    // x
#define HEIGHT 24   // y, nach oben
#define DEPTH 48    // z

enum { AIR, GRASS, DIRT, STONE, WOOD, LEAVES, SAND, BLOCK_TYPES };

Color colors[BLOCK_TYPES] = {
    { 0, 0, 0, 0 },
    { 96, 168, 64, 255 },     // Gras
    { 130, 92, 62, 255 },     // Erde
    { 125, 125, 130, 255 },   // Stein
    { 150, 110, 64, 255 },    // Holz
    { 52, 128, 56, 255 },     // Laub
    { 222, 204, 150, 255 },   // Sand
};

unsigned char world[WIDTH][HEIGHT][DEPTH];

// Der Block an einer Stelle; außerhalb der Welt ist Luft
int block(int x, int y, int z)
{
    if (x < 0 || y < 0 || z < 0 || x >= WIDTH || y >= HEIGHT || z >= DEPTH)
        return AIR;
    return world[x][y][z];
}

void tree(int x, int y, int z)
{
    for (int i = 0; i < 4; i++)
        world[x][y + i][z] = WOOD;
    for (int dx = -2; dx <= 2; dx++)
        for (int dz = -2; dz <= 2; dz++)
            for (int dy = 3; dy <= 5; dy++)
                if (abs(dx) + abs(dz) + (dy - 3) <= 3 && block(x + dx, y + dy, z + dz) == AIR)
                    world[x + dx][y + dy][z + dz] = LEAVES;
}

void generate_world(void)
{
    for (int x = 0; x < WIDTH; x++) {
        for (int z = 0; z < DEPTH; z++) {
            // Hügel aus drei Wellen, die sich überlagern
            float h = 7.0f + 2.0f * sinf(x * 0.13f) + 1.5f * cosf(z * 0.11f)
                    + 1.5f * sinf((x + z) * 0.07f);
            int height = (int)h;
            for (int y = 0; y < height; y++) {
                if (y == height - 1)
                    world[x][y][z] = height <= 5 ? SAND : GRASS;
                else if (y >= height - 3)
                    world[x][y][z] = DIRT;
                else
                    world[x][y][z] = STONE;
            }
        }
    }
    SetRandomSeed(7);
    for (int i = 0; i < 14; i++) {
        int x = GetRandomValue(3, WIDTH - 4);
        int z = GetRandomValue(3, DEPTH - 4);
        int y = HEIGHT - 7;
        while (y > 0 && block(x, y - 1, z) == AIR)
            y--;
        if (block(x, y - 1, z) == GRASS)
            tree(x, y, z);
    }
}

// Grenzt der Block an Luft? Nur dann kann man ihn sehen.
bool visible(int x, int y, int z)
{
    return block(x + 1, y, z) == AIR || block(x - 1, y, z) == AIR
        || block(x, y + 1, z) == AIR || block(x, y - 1, z) == AIR
        || block(x, y, z + 1) == AIR || block(x, y, z - 1) == AIR;
}

// --------------------------------------------------------------- Die Figur

Vector3 feet;          // Mitte der Fußsohlen
float yaw = 0;         // nach links und rechts, im Bogenmaß
float pitch = 0;       // nach oben und unten

#define EYE_HEIGHT 1.6f

Vector3 view_direction(void)
{
    return (Vector3){
        sinf(yaw) * cosf(pitch),
        sinf(pitch),
        cosf(yaw) * cosf(pitch),
    };
}

void move_player(float dt)
{
    // Umsehen: Maus (wenn gefangen) oder Pfeiltasten
    if (IsCursorHidden()) {
        Vector2 mouse = GetMouseDelta();
        yaw -= mouse.x * 0.003f;
        pitch -= mouse.y * 0.003f;
    }
    if (IsKeyDown(KEY_LEFT)) yaw += 2.0f * dt;
    if (IsKeyDown(KEY_RIGHT)) yaw -= 2.0f * dt;
    if (IsKeyDown(KEY_UP)) pitch += 1.5f * dt;
    if (IsKeyDown(KEY_DOWN)) pitch -= 1.5f * dt;
    pitch = Clamp(pitch, -1.5f, 1.5f);

    // Laufen: vorwärts in Blickrichtung, aber ohne nach oben zu fliegen
    Vector3 forward = { sinf(yaw), 0, cosf(yaw) };
    Vector3 right = { -cosf(yaw), 0, sinf(yaw) };
    Vector3 move = { 0 };
    if (IsKeyDown(KEY_W)) move = Vector3Add(move, forward);
    if (IsKeyDown(KEY_S)) move = Vector3Subtract(move, forward);
    if (IsKeyDown(KEY_D)) move = Vector3Add(move, right);
    if (IsKeyDown(KEY_A)) move = Vector3Subtract(move, right);
    move = Vector3Scale(Vector3Normalize(move), 4.5f * dt);
    feet = Vector3Add(feet, move);
}

int main(void)
{
    InitWindow(960, 540, "Klötzchenwelt");
    SetTargetFPS(60);
    SetExitKey(KEY_NULL);   // Esc gibt nur die Maus frei

    generate_world();
    feet = (Vector3){ 8.5f, 9, 10.5f };
    yaw = 0.8f;      // schräg über die Welt schauen
    pitch = -0.2f;

    Camera3D camera = { 0 };
    camera.up = (Vector3){ 0, 1, 0 };
    camera.fovy = 70;
    camera.projection = CAMERA_PERSPECTIVE;

    while (!WindowShouldClose())
    {
        float dt = GetFrameTime();
        if (dt > 0.05f) dt = 0.05f;

        // Ein Klick ins Bild fängt die Maus, Esc gibt sie frei
        if (!IsCursorHidden() && IsMouseButtonPressed(MOUSE_BUTTON_LEFT))
            DisableCursor();
        if (IsKeyPressed(KEY_ESCAPE)) EnableCursor();

        move_player(dt);

        Vector3 eyes = { feet.x, feet.y + EYE_HEIGHT, feet.z };
        camera.position = eyes;
        camera.target = Vector3Add(eyes, view_direction());

        BeginDrawing();
        ClearBackground(SKYBLUE);

        BeginMode3D(camera);
        int blocks = 0;
        for (int x = 0; x < WIDTH; x++)
            for (int y = 0; y < HEIGHT; y++)
                for (int z = 0; z < DEPTH; z++)
                    if (world[x][y][z] != AIR && visible(x, y, z)) {
                        Vector3 center = { x + 0.5f, y + 0.5f, z + 0.5f };
                        DrawCube(center, 1, 1, 1, colors[world[x][y][z]]);
                        DrawCubeWires(center, 1, 1, 1, Fade(BLACK, 0.25f));
                        blocks++;
                    }
        EndMode3D();

        DrawText(TextFormat("%d Blöcke  %d FPS", blocks, GetFPS()), 10, 10, 20, DARKBLUE);
        if (!IsCursorHidden() && GetTime() < 8)
            DrawText("Klick ins Bild: Maus fangen. WASD laufen, Pfeiltasten umsehen.", 10, 36, 20, WHITE);
        EndDrawing();
    }

    CloseWindow();
    return 0;
}
