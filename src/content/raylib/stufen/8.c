#include <math.h>
#include <stdlib.h>
#include <raylib.h>
#include <raymath.h>
#include <rlgl.h>

// ---------------------------------------------------------------- Die Welt

#define WIDTH 48    // x
#define HEIGHT 24   // y, nach oben
#define DEPTH 48    // z

enum { AIR, GRASS, DIRT, STONE, WOOD, LEAVES, SAND, TORCH, BLOCK_TYPES };

const char *names[BLOCK_TYPES] = {
    "Luft", "Gras", "Erde", "Stein", "Holz", "Laub", "Sand", "Fackel",
};

Color colors[BLOCK_TYPES] = {
    { 0, 0, 0, 0 },
    { 96, 168, 64, 255 },     // Gras
    { 130, 92, 62, 255 },     // Erde
    { 125, 125, 130, 255 },   // Stein
    { 150, 110, 64, 255 },    // Holz
    { 52, 128, 56, 255 },     // Laub
    { 222, 204, 150, 255 },   // Sand
    { 255, 200, 80, 255 },    // Fackel
};

unsigned char world[WIDTH][HEIGHT][DEPTH];

// ---------------------------------------------------------------- Texturen

// textures.png ist ein Streifen aus Kacheln, alle gleich groß, in dieser
// Reihenfolge von links nach rechts
enum { T_GRASS_TOP, T_GRASS_SIDE, T_DIRT, T_STONE, T_WOOD_SIDE, T_WOOD_TOP,
       T_LEAVES, T_SAND, TILE_COUNT };

// Welche Kachel bekommt eine Seite? dir: 0 oben, 1 unten, 2 seitlich
int tile_for(int type, int dir)
{
    switch (type) {
    case GRASS: return dir == 0 ? T_GRASS_TOP : dir == 1 ? T_DIRT : T_GRASS_SIDE;
    case DIRT: return T_DIRT;
    case STONE: return T_STONE;
    case WOOD: return dir == 2 ? T_WOOD_SIDE : T_WOOD_TOP;
    case LEAVES: return T_LEAVES;
    default: return T_SAND;
    }
}

// Der Block an einer Stelle; außerhalb der Welt ist Luft
int block(int x, int y, int z)
{
    if (x < 0 || y < 0 || z < 0 || x >= WIDTH || y >= HEIGHT || z >= DEPTH)
        return AIR;
    return world[x][y][z];
}

// Kann man an dieser Stelle nicht hindurch? Fackeln sind nicht fest, und am
// Rand und unter der Welt stehen unsichtbare Wände.
bool solid(int x, int y, int z)
{
    if (x < 0 || z < 0 || x >= WIDTH || z >= DEPTH || y < 0)
        return true;
    int type = block(x, y, z);
    return type != AIR && type != TORCH;
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

// ---------------------------------------------------------- Blöcke zeichnen

// Eine Seite eines Blocks: vier Ecken, von außen gesehen links unten, rechts
// unten, rechts oben, links oben; dazu die Richtung (Normale), eine
// Helligkeit und die Kachel aus der Textur
void face(Vector3 lb, Vector3 rb, Vector3 rt, Vector3 lt, Vector3 n, Color f, int tile)
{
    float u0 = (float)tile / TILE_COUNT, u1 = (float)(tile + 1) / TILE_COUNT;
    rlColor4ub(f.r, f.g, f.b, 255);
    rlNormal3f(n.x, n.y, n.z);
    rlTexCoord2f(u0, 1); rlVertex3f(lb.x, lb.y, lb.z);
    rlTexCoord2f(u1, 1); rlVertex3f(rb.x, rb.y, rb.z);
    rlTexCoord2f(u1, 0); rlVertex3f(rt.x, rt.y, rt.z);
    rlTexCoord2f(u0, 0); rlVertex3f(lt.x, lt.y, lt.z);
}

// Zeichnet nur die Seiten, an die Luft grenzt: Die anderen sieht man nie.
int draw_block(int x, int y, int z)
{
    int type = world[x][y][z];
    // Jeder Block ist ein wenig heller oder dunkler, dann wirkt die Welt
    // weniger glatt
    float shade = 0.92f + 0.08f * (((x * 7 + y * 13 + z * 31) % 5) / 4.0f);
    Color f = ColorBrightness(WHITE, shade - 1.0f);
    int top = tile_for(type, 0), bottom = tile_for(type, 1), side = tile_for(type, 2);

    float x0 = x, x1 = x + 1, y0 = y, y1 = y + 1, z0 = z, z1 = z + 1;
    int faces = 0;
    rlBegin(RL_QUADS);
    if (!solid(x, y + 1, z)) {   // oben
        face((Vector3){x0, y1, z1}, (Vector3){x1, y1, z1}, (Vector3){x1, y1, z0}, (Vector3){x0, y1, z0}, (Vector3){0, 1, 0}, f, top);
        faces++;
    }
    if (!solid(x, y - 1, z) && y > 0) {   // unten
        face((Vector3){x0, y0, z0}, (Vector3){x1, y0, z0}, (Vector3){x1, y0, z1}, (Vector3){x0, y0, z1}, (Vector3){0, -1, 0}, f, bottom);
        faces++;
    }
    if (!solid(x + 1, y, z) && x < WIDTH - 1) {   // rechts (+x)
        face((Vector3){x1, y0, z1}, (Vector3){x1, y0, z0}, (Vector3){x1, y1, z0}, (Vector3){x1, y1, z1}, (Vector3){1, 0, 0}, f, side);
        faces++;
    }
    if (!solid(x - 1, y, z) && x > 0) {   // links (-x)
        face((Vector3){x0, y0, z0}, (Vector3){x0, y0, z1}, (Vector3){x0, y1, z1}, (Vector3){x0, y1, z0}, (Vector3){-1, 0, 0}, f, side);
        faces++;
    }
    if (!solid(x, y, z + 1) && z < DEPTH - 1) {   // vorne (+z)
        face((Vector3){x0, y0, z1}, (Vector3){x1, y0, z1}, (Vector3){x1, y1, z1}, (Vector3){x0, y1, z1}, (Vector3){0, 0, 1}, f, side);
        faces++;
    }
    if (!solid(x, y, z - 1) && z > 0) {   // hinten (-z)
        face((Vector3){x1, y0, z0}, (Vector3){x0, y0, z0}, (Vector3){x0, y1, z0}, (Vector3){x1, y1, z0}, (Vector3){0, 0, -1}, f, side);
        faces++;
    }
    rlEnd();
    return faces;
}

// --------------------------------------------------------------- Die Figur

Vector3 feet;          // Mitte der Fußsohlen
float vy = 0;          // Geschwindigkeit nach oben, in Blöcken pro Sekunde
bool on_ground = false;
float yaw = 0;         // nach links und rechts, im Bogenmaß
float pitch = 0;       // nach oben und unten

#define RADIUS 0.3f      // die Figur ist 0,6 Blöcke breit
#define BODY_HEIGHT 1.7f
#define EYE_HEIGHT 1.6f

// Steckt eine Figur mit diesen Füßen in einem festen Block?
bool collides(Vector3 f)
{
    for (int x = (int)floorf(f.x - RADIUS); x <= (int)floorf(f.x + RADIUS); x++)
        for (int y = (int)floorf(f.y); y <= (int)floorf(f.y + BODY_HEIGHT); y++)
            for (int z = (int)floorf(f.z - RADIUS); z <= (int)floorf(f.z + RADIUS); z++)
                if (solid(x, y, z))
                    return true;
    return false;
}

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

    // Jede Richtung einzeln: So gleitet man an einer Wand entlang, statt an
    // ihr kleben zu bleiben.
    Vector3 next = feet;
    next.x += move.x;
    if (!collides(next)) feet.x = next.x;
    next = feet;
    next.z += move.z;
    if (!collides(next)) feet.z = next.z;

    // Schwerkraft und Springen
    vy -= 22.0f * dt;
    if (on_ground && IsKeyDown(KEY_SPACE)) vy = 7.5f;
    next = feet;
    next.y += vy * dt;
    if (collides(next)) {
        if (vy < 0) {
            on_ground = true;
            feet.y = floorf(next.y) + 1.0f;   // genau auf den Block stellen
        }
        vy = 0;
    } else {
        feet.y = next.y;
        on_ground = false;
    }
}

// ------------------------------------------------------------------ Zielen

bool hit = false;
int hx, hy, hz;          // der Block, auf den du schaust
int ax, ay, az;          // die Luft davor: Hier entsteht ein neuer Block

void aim(Vector3 eyes, Vector3 look)
{
    hit = false;
    ax = (int)floorf(eyes.x); ay = (int)floorf(eyes.y); az = (int)floorf(eyes.z);
    // In kleinen Schritten den Blick entlang, bis ein Block im Weg ist
    for (float t = 0; t < 6.0f; t += 0.02f) {
        Vector3 p = Vector3Add(eyes, Vector3Scale(look, t));
        int x = (int)floorf(p.x), y = (int)floorf(p.y), z = (int)floorf(p.z);
        if (block(x, y, z) != AIR) {
            hit = true;
            hx = x; hy = y; hz = z;
            return;
        }
        ax = x; ay = y; az = z;
    }
}

// --------------------------------------------------------------- Tageszeit

float time_of_day = 0.4f;  // 0 und 1: Mitternacht, 0,5: Mittag

int main(void)
{
    SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_WINDOW_RESIZABLE);   // glatte Kanten, Größe wie die Seite
    InitWindow(960, 540, "Klötzchenwelt");
    SetTargetFPS(60);
    SetExitKey(KEY_NULL);   // Esc gibt nur die Maus frei

    generate_world();
    feet = (Vector3){ 8.5f, HEIGHT - 2, 10.5f };
    yaw = 0.8f;      // schräg über die Welt schauen
    pitch = -0.2f;

    Shader light = LoadShader("light.vs", "light.fs");
    Texture2D texture = LoadTexture("textures.png");
    SetTextureFilter(texture, TEXTURE_FILTER_POINT);   // scharfe Pixel
    int loc_sun = GetShaderLocation(light, "sun");
    int loc_sunlight = GetShaderLocation(light, "sunlight");
    int loc_ambient = GetShaderLocation(light, "ambient");
    int loc_sky = GetShaderLocation(light, "sky");
    int loc_eyes = GetShaderLocation(light, "eyes");
    int loc_torches = GetShaderLocation(light, "torches");
    int loc_torch_count = GetShaderLocation(light, "torchCount");

    Camera3D camera = { 0 };
    camera.up = (Vector3){ 0, 1, 0 };
    camera.fovy = 70;
    camera.projection = CAMERA_PERSPECTIVE;

    int selected = GRASS;

    while (!WindowShouldClose())
    {
        float dt = GetFrameTime();
        if (dt > 0.05f) dt = 0.05f;

        // Ein Klick ins Bild fängt die Maus, Esc gibt sie frei
        if (!IsCursorHidden() && IsMouseButtonPressed(MOUSE_BUTTON_LEFT))
            DisableCursor();
        else {
            if (hit && IsMouseButtonPressed(MOUSE_BUTTON_LEFT))
                world[hx][hy][hz] = AIR;
            if (hit && IsMouseButtonPressed(MOUSE_BUTTON_RIGHT) && block(ax, ay, az) == AIR) {
                world[ax][ay][az] = selected;
                if (collides(feet))        // nicht in sich selbst bauen
                    world[ax][ay][az] = AIR;
            }
        }
        if (IsKeyPressed(KEY_ESCAPE)) EnableCursor();
        for (int i = 1; i < BLOCK_TYPES; i++)
            if (IsKeyPressed(KEY_ONE + i - 1)) selected = i;

        move_player(dt);

        // Die Zeit vergeht: ein Tag dauert zwei Minuten, mit T schneller
        time_of_day += dt / 120.0f + (IsKeyDown(KEY_T) ? dt * 0.2f : 0);
        time_of_day -= floorf(time_of_day);
        if (IsKeyPressed(KEY_N))   // N springt zwischen Tag und Nacht
            time_of_day = (time_of_day > 0.25f && time_of_day < 0.75f) ? 0.9f : 0.4f;
        float angle = (time_of_day - 0.25f) * 2 * PI;   // Sonnenaufgang bei 0,25
        Vector3 sun = Vector3Normalize((Vector3){ cosf(angle), sinf(angle), 0.4f });
        float day = Clamp(sinf(angle) * 4.0f + 0.3f, 0.0f, 1.0f);
        Color sky = ColorLerp((Color){ 12, 16, 36, 255 }, (Color){ 140, 195, 240, 255 }, day);
        Vector3 sky_v = { sky.r / 255.0f, sky.g / 255.0f, sky.b / 255.0f };
        Vector3 sunlight = { 0.75f * day, 0.7f * day, 0.6f * day };
        // Nachts bleibt bläuliches Mondlicht
        Vector3 ambient = { 0.2f + 0.23f * day, 0.22f + 0.24f * day, 0.34f + 0.2f * day };

        Vector3 eyes = { feet.x, feet.y + EYE_HEIGHT, feet.z };
        Vector3 look = view_direction();
        camera.position = eyes;
        camera.target = Vector3Add(eyes, look);
        aim(eyes, look);

        // Fackeln einsammeln: Sie leuchten im Shader
        Vector3 torches[16];
        int torch_count = 0;
        for (int x = 0; x < WIDTH; x++)
            for (int y = 0; y < HEIGHT; y++)
                for (int z = 0; z < DEPTH; z++)
                    if (world[x][y][z] == TORCH && torch_count < 16)
                        torches[torch_count++] = (Vector3){ x + 0.5f, y + 0.7f, z + 0.5f };

        SetShaderValue(light, loc_sun, &sun, SHADER_UNIFORM_VEC3);
        SetShaderValue(light, loc_sunlight, &sunlight, SHADER_UNIFORM_VEC3);
        SetShaderValue(light, loc_ambient, &ambient, SHADER_UNIFORM_VEC3);
        SetShaderValue(light, loc_sky, &sky_v, SHADER_UNIFORM_VEC3);
        SetShaderValue(light, loc_eyes, &eyes, SHADER_UNIFORM_VEC3);
        SetShaderValueV(light, loc_torches, torches, SHADER_UNIFORM_VEC3, torch_count > 0 ? torch_count : 1);
        SetShaderValue(light, loc_torch_count, &torch_count, SHADER_UNIFORM_INT);

        BeginDrawing();
        ClearBackground(sky);

        BeginMode3D(camera);
        BeginShaderMode(light);
        rlSetTexture(texture.id);
        int faces = 0;
        for (int x = 0; x < WIDTH; x++)
            for (int y = 0; y < HEIGHT; y++)
                for (int z = 0; z < DEPTH; z++)
                    if (world[x][y][z] != AIR && world[x][y][z] != TORCH)
                        faces += draw_block(x, y, z);
        rlSetTexture(0);
        EndShaderMode();

        // Fackeln leuchten selbst: ohne Licht-Shader gezeichnet
        for (int i = 0; i < torch_count; i++) {
            Vector3 p = torches[i];
            DrawCube((Vector3){ p.x, p.y - 0.4f, p.z }, 0.15f, 0.6f, 0.15f, (Color){ 120, 80, 40, 255 });
            DrawCube((Vector3){ p.x, p.y - 0.05f, p.z }, 0.22f, 0.18f, 0.22f, colors[TORCH]);
        }

        if (hit)
            DrawCubeWires((Vector3){ hx + 0.5f, hy + 0.5f, hz + 0.5f }, 1.01f, 1.01f, 1.01f, BLACK);
        EndMode3D();

        // 2D über dem Bild: Fadenkreuz, Leiste mit den Blöcken, Hinweise
        int mx = GetScreenWidth() / 2, my = GetScreenHeight() / 2;
        DrawRectangle(mx - 10, my - 1, 20, 2, WHITE);
        DrawRectangle(mx - 1, my - 10, 2, 20, WHITE);

        int slots = BLOCK_TYPES - 1;
        int left = mx - slots * 26;
        for (int i = 1; i < BLOCK_TYPES; i++) {
            int fx = left + (i - 1) * 52, fy = GetScreenHeight() - 62;
            DrawRectangle(fx, fy, 48, 48, Fade(BLACK, 0.4f));
            DrawRectangle(fx + 8, fy + 8, 32, 32, colors[i]);
            DrawText(TextFormat("%d", i), fx + 4, fy + 2, 10, WHITE);
            if (i == selected)
                DrawRectangleLinesEx((Rectangle){ fx - 2, fy - 2, 52, 52 }, 3, WHITE);
        }
        DrawText(names[selected], mx - MeasureText(names[selected], 20) / 2, GetScreenHeight() - 88, 20, WHITE);
        DrawText(TextFormat("%d Seiten  %d FPS", faces, GetFPS()), 10, 10, 10, WHITE);
        if (!IsCursorHidden() && GetTime() < 8)
            DrawText("Klick ins Bild: Maus fangen. WASD laufen, Pfeiltasten umsehen.", 10, 36, 20, WHITE);
        EndDrawing();
    }

    UnloadTexture(texture);
    UnloadShader(light);
    CloseWindow();
    return 0;
}
