#include "pixelram.h"
#include <math.h>
// Palette, Schattentabelle und Texturen, gemacht von palette.rb
#include "palette.h"

#define W 320
#define H 180

// die Karte: '.' ist Boden, jede Ziffer eine Wand
// 1 Ziegel, 2 Stein, 3 Holz, 4 Metall, 5 Moos
#define MAP_W 24
#define MAP_H 16

const char *map[MAP_H] = {
    "111111112222222222222222",
    "1......1...............2",
    "1......4...1...1...1...2",
    "1......................2",
    "1......4...............2",
    "1......1...1...1...1...2",
    "1......1...............2",
    "1113.111222222.222222222",
    "3......3555555.555555555",
    "3......35..............5",
    "3.33...44..5...5...5...5",
    "3......................5",
    "3......44..............5",
    "3..33..35..5...5...5...5",
    "3......35..............5",
    "333333335555555555555555",
};

// so viele Helligkeiten hat die Schattentabelle
#define LEVELS 16

// die Farben für die Karte: die reservierten Farben 0 bis 15 der Palette
enum
{
    BLACK = UI_BLACK,
    MAP_FLOOR = UI_MAP_FLOOR,
    VIEW = UI_VIEW,
    PLAYER = UI_PLAYER,
    RAY = UI_RAY,
    RAY_SIDE = UI_RAY_SIDE,
    CAMERA_PLANE = UI_CAMERA_PLANE
};

// die Schattentabelle: shade[light][color] ist dieselbe Farbe wie color,
// nur mit der Helligkeit light / 15 (wie die COLORMAP in DOOM)
uint8_t shade[LEVELS][256];

// die Spielfigur: Position in Kästchen, Blickrichtung im Bogenmaß
double player_x = 3.5;
double player_y = 3.5;
double player_angle = 0;

// so dick ist die Spielfigur (in Kästchen)
#define PLAYER_RADIUS 0.25

// die Kamera: Blickrichtung (Länge 1) und die Kamera-Ebene senkrecht dazu.
// PLANE ist die halbe Breite der Ebene: 0.89 = W / 2 / H ergibt etwa 83 Grad
// Blickwinkel, und ein Stück Wand ist auf dem Bild so breit wie hoch.
#define PLANE 0.89
double dir_x, dir_y;
double plane_x, plane_y;

void update_camera(void)
{
    dir_x = cos(player_angle);
    dir_y = sin(player_angle);
    // um 90 Grad gedreht, zeigt nach rechts
    plane_x = -dir_y * PLANE;
    plane_y = dir_x * PLANE;
}

// Kästchen pro Sekunde und Bogenmaß pro Sekunde
#define MOVE_SPEED 3.0
#define TURN_SPEED 2.5

// die Farben und die Schattentabelle aus palette.h
void setup_palette(void)
{
    for (int i = 0; i < 256; i++)
        set_palette(i, palette_rgb[i][0], palette_rgb[i][1], palette_rgb[i][2]);

    for (int light = 0; light < LEVELS; light++)
        for (int color = 0; color < 256; color++)
            shade[light][color] = palette_shade[light][color];
}

// Texturen: 64 x 64 Texel, jedes Texel ist eine Farbnummer der Palette
#define TEX 64

// die Texturen in der Reihenfolge der Kacheln in textures.png
enum
{
    TEX_FLOOR,
    TEX_BRICK, // 1 bis 5: die Wandarten
    TEX_STONE,
    TEX_WOOD,
    TEX_METAL,
    TEX_MOSS,
    TEX_CEILING,
    TEXTURES
};

uint8_t texture[TEXTURES][TEX][TEX];

// die Texturen aus palette.h
void make_textures(void)
{
    for (int t = 0; t < TEXTURES; t++)
        for (int y = 0; y < TEX; y++)
            for (int x = 0; x < TEX; x++)
                texture[t][y][x] = palette_texture[t][y][x];
}

// Mipmaps: jede Textur noch einmal in halber Größe, in Viertelgröße ...
// Stufe 0 ist das Original mit 64 Texeln, dann 32, 16, 8 und 4
#define MIP_LEVELS 5
uint8_t mip[TEXTURES][MIP_LEVELS][TEX][TEX];

// die Farbe der Palette, die (r, g, b) am nächsten kommt
int nearest_color(int r, int g, int b)
{
    int best = 0;
    int best_difference = 3 * 256 * 256;
    for (int i = 0; i < 256; i++)
    {
        uint8_t pr, pg, pb;
        get_palette(i, &pr, &pg, &pb);
        int difference = (r - pr) * (r - pr) + (g - pg) * (g - pg) + (b - pb) * (b - pb);
        if (difference < best_difference)
        {
            best_difference = difference;
            best = i;
        }
    }
    return best;
}

void make_mipmaps(void)
{
    for (int t = 0; t < TEXTURES; t++)
    {
        for (int y = 0; y < TEX; y++)
            for (int x = 0; x < TEX; x++)
                mip[t][0][y][x] = texture[t][y][x];

        // jedes Texel einer Stufe mischt vier Texel der Stufe davor
        for (int level = 1; level < MIP_LEVELS; level++)
        {
            int size = TEX >> level;
            for (int y = 0; y < size; y++)
                for (int x = 0; x < size; x++)
                {
                    int r = 0, g = 0, b = 0;
                    for (int dy = 0; dy < 2; dy++)
                        for (int dx = 0; dx < 2; dx++)
                        {
                            uint8_t pr, pg, pb;
                            get_palette(mip[t][level - 1][2 * y + dy][2 * x + dx], &pr, &pg, &pb);
                            r += pr;
                            g += pg;
                            b += pb;
                        }
                    mip[t][level][y][x] = nearest_color(r / 4, g / 4, b / 4);
                }
        }
    }
}

// Mipmaps an (M schaltet um)
bool use_mipmaps = true;

// welche Stufe passt, wenn so viele Texel auf ein Pixel fallen:
// ab 2 Texeln Stufe 1, ab 4 Stufe 2, ab 8 Stufe 3 ...
int mip_level(double texels_per_pixel)
{
    int level = 0;
    while (use_mipmaps && texels_per_pixel >= 2 && level < MIP_LEVELS - 1)
    {
        texels_per_pixel /= 2;
        level++;
    }
    return level;
}

// das Texel beim Punkt (u, v) der Textur t in der Mipmap-Stufe level;
// u und v laufen von 0 bis 1
int texel(int t, int level, double u, double v)
{
    int size = TEX >> level;
    int x = (int)(u * size) & (size - 1);
    int y = (int)(v * size) & (size - 1);
    return mip[t][level][y][x];
}

// ist beim Punkt (x, y) der Karte eine Wand?
int is_wall(double x, double y)
{
    if (x < 0 || y < 0 || x >= MAP_W || y >= MAP_H)
        return 1;
    return map[(int)y][(int)x] != '.';
}

// die Farbe einer Wandart ('1' bis '5')
int wall_color(char cell)
{
    return palette_average[cell - '0'];
}

void fill_rect(int x, int y, int width, int height, int color)
{
    for (int j = y; j < y + height; j++)
        for (int i = x; i < x + width; i++)
            set_pixel(i, j, color);
}

void fill_circle(double cx, double cy, double radius, int color)
{
    for (int y = (int)(cy - radius); y <= (int)(cy + radius); y++)
        for (int x = (int)(cx - radius); x <= (int)(cx + radius); x++)
        {
            double dx = x + 0.5 - cx;
            double dy = y + 0.5 - cy;
            if (dx * dx + dy * dy <= radius * radius)
                set_pixel(x, y, color);
        }
}

// eine Linie: in kleinen Schritten von (x0, y0) nach (x1, y1) gehen
void draw_line(double x0, double y0, double x1, double y1, int color)
{
    int steps = (int)fmax(fabs(x1 - x0), fabs(y1 - y0)) + 1;
    for (int i = 0; i <= steps; i++)
    {
        double t = (double)i / steps;
        set_pixel((int)(x0 + (x1 - x0) * t), (int)(y0 + (y1 - y0) * t), color);
    }
}

// die Karte von oben: jedes Kästchen ist cell Pixel groß,
// die linke obere Ecke liegt bei (left, top)
void draw_map(int left, int top, int cell)
{
    // schwarz darunter: das ergibt die Linien zwischen den Kästchen
    fill_rect(left - 1, top - 1, MAP_W * cell + 1, MAP_H * cell + 1, BLACK);
    for (int y = 0; y < MAP_H; y++)
        for (int x = 0; x < MAP_W; x++)
        {
            char c = map[y][x];
            int color = (c == '.') ? MAP_FLOOR : wall_color(c);
            fill_rect(left + x * cell, top + y * cell, cell - 1, cell - 1, color);
        }
}

// was ein Strahl getroffen hat
typedef struct
{
    double x, y; // der Treffpunkt
    int side;    // 0: eine Wand quer zur x-Achse, 1: quer zur y-Achse
    char cell;   // die Wandart
} Hit;

// Digital Differential Analyzer (DDA): geht von Kästchengrenze zu
// Kästchengrenze, bis der Strahl in einer Wand landet. Ein Punkt auf dem
// Strahl ist (player_x + t * ray_x, player_y + t * ray_y).
Hit cast_ray(double ray_x, double ray_y)
{
    int cell_x = (int)player_x;
    int cell_y = (int)player_y;

    // um so viel wächst t von einer senkrechten (x) bzw. waagrechten (y)
    // Gitterlinie zur nächsten; ein Strahl parallel zur Achse kommt nie an
    double delta_x = (ray_x == 0) ? 1e30 : fabs(1 / ray_x);
    double delta_y = (ray_y == 0) ? 1e30 : fabs(1 / ray_y);

    // in welche Richtung es weitergeht, und t an der ersten Gitterlinie
    int step_x, step_y;
    double next_x, next_y;
    if (ray_x < 0)
    {
        step_x = -1;
        next_x = (player_x - cell_x) * delta_x;
    }
    else
    {
        step_x = 1;
        next_x = (cell_x + 1 - player_x) * delta_x;
    }
    if (ray_y < 0)
    {
        step_y = -1;
        next_y = (player_y - cell_y) * delta_y;
    }
    else
    {
        step_y = 1;
        next_y = (cell_y + 1 - player_y) * delta_y;
    }

    // immer über die Gitterlinie, die näher liegt
    double t = 0;
    int side = 0;
    while (map[cell_y][cell_x] == '.')
    {
        if (next_x < next_y)
        {
            t = next_x;
            next_x += delta_x;
            cell_x += step_x;
            side = 0;
        }
        else
        {
            t = next_y;
            next_y += delta_y;
            cell_y += step_y;
            side = 1;
        }
    }

    Hit hit = {player_x + t * ray_x, player_y + t * ray_y, side, map[cell_y][cell_x]};
    return hit;
}

// die Kamera-Ebene und einige Strahlen, von oben gesehen
void draw_rays(int left, int top, int cell)
{
    double x = left + player_x * cell;
    double y = top + player_y * cell;

    for (int column = 0; column < W; column += 16)
    {
        // camera_x läuft über die Kamera-Ebene von -1 (links) bis +1 (rechts)
        double camera_x = 2.0 * (column + 0.5) / W - 1;
        double ray_x = dir_x + plane_x * camera_x;
        double ray_y = dir_y + plane_y * camera_x;

        Hit hit = cast_ray(ray_x, ray_y);
        int color = (hit.side == 0) ? RAY : RAY_SIDE;
        draw_line(x, y, left + hit.x * cell, top + hit.y * cell, color);
    }

    // die Kamera-Ebene: eine Strecke ein Kästchen vor der Spielfigur
    draw_line(x + (dir_x - plane_x) * cell, y + (dir_y - plane_y) * cell,
              x + (dir_x + plane_x) * cell, y + (dir_y + plane_y) * cell, CAMERA_PLANE);
}

void draw_player(int left, int top, int cell)
{
    double x = left + player_x * cell;
    double y = top + player_y * cell;

    // die Blickrichtung: ein Kästchen lang
    draw_line(x, y, x + cos(player_angle) * cell, y + sin(player_angle) * cell, VIEW);
    fill_circle(x, y, PLAYER_RADIUS * cell, PLAYER);
}

// passt die Spielfigur an den Punkt (x, y)? Alle vier Ecken des Quadrats
// um sie herum müssen auf Boden stehen.
int is_free(double x, double y)
{
    double r = PLAYER_RADIUS;
    return !is_wall(x - r, y - r) && !is_wall(x + r, y - r) &&
           !is_wall(x - r, y + r) && !is_wall(x + r, y + r);
}

// bewegt die Spielfigur, dt ist die Zeit seit dem letzten Bild in Sekunden
void move_player(double dt)
{
    if (key_down(pixel_key_left))
        player_angle -= TURN_SPEED * dt;
    if (key_down(pixel_key_right))
        player_angle += TURN_SPEED * dt;

    double forward = 0;
    double sideways = 0;
    if (key_down(pixel_key_w) || key_down(pixel_key_up))
        forward += 1;
    if (key_down(pixel_key_s) || key_down(pixel_key_down))
        forward -= 1;
    if (key_down(pixel_key_d))
        sideways += 1;
    if (key_down(pixel_key_a))
        sideways -= 1;

    // vorwärts entlang der Blickrichtung, seitwärts um 90 Grad gedreht
    double dir_x = cos(player_angle);
    double dir_y = sin(player_angle);
    double dx = (dir_x * forward - dir_y * sideways) * MOVE_SPEED * dt;
    double dy = (dir_y * forward + dir_x * sideways) * MOVE_SPEED * dt;

    // x und y getrennt: an einer Wand gleitet die Spielfigur entlang
    if (is_free(player_x + dx, player_y))
        player_x += dx;
    if (is_free(player_x, player_y + dy))
        player_y += dy;
}

// wie hell es im Abstand distance ist: 1 ganz nah, gegen 0 in der Ferne
double light_at(double distance)
{
    return 1 / (1 + 0.04 * distance * distance);
}

// eine Farbe mit der Helligkeit light (0 bis 1) aus der Schattentabelle
int shaded(int color, double light)
{
    return shade[(int)(light * (LEVELS - 1) + 0.5)][color];
}

// eine senkrechte Wandscheibe: je weiter weg, desto niedriger;
// im Abstand 1 ist die Wand genau H Pixel hoch;
// u ist die Spalte in der Textur (0 bis 1)
void draw_wall_column(int column, double distance, int tex, double u, double light)
{
    double wall_height = H / fmax(distance, 0.01);
    double wall_top = H / 2.0 - wall_height / 2;
    int level = mip_level(TEX / wall_height);

    for (int y = 0; y < H; y++)
    {
        // wo diese Zeile auf der Wand liegt: 0 oben, 1 unten
        double v = (y + 0.5 - wall_top) / wall_height;
        if (v >= 0 && v < 1)
            set_pixel(column, y, shaded(texel(tex, level, u, v), light));
    }
}

// wie weit der Boden (unter dem Horizont) bzw. die Decke (darüber) in
// Bildzeile y entfernt ist: die Augen sind 0.5 über dem Boden
double row_distance(int y)
{
    return 0.5 * H / fabs(y + 0.5 - H / 2.0);
}

// das Bild aus Sicht der Spielfigur: für jede Spalte ein Strahl
void draw_view(void)
{
    // Decke und Boden: in jeder Zeile derselbe Abstand
    for (int y = 0; y < H; y++)
    {
        double distance = row_distance(y);
        double light = light_at(distance);
        int tex = (y < H / 2) ? TEX_CEILING : TEX_FLOOR;

        // wie viele Texel auf ein Pixel fallen: quer zur Blickrichtung so
        // viele wie bei einer Wand im selben Abstand, in die Tiefe viel
        // mehr, denn von Zeile zu Zeile wächst der Abstand um 2 * distance² / H;
        // wir nehmen das geometrische Mittel der beiden
        double across = distance * 2 * PLANE / W;
        double along = 2 * distance * distance / H;
        int level = mip_level(TEX * sqrt(across * along));

        // der Punkt auf dem Boden am linken Bildrand (Strahl dir - plane)
        // und der Schritt von einem Pixel zum nächsten nach rechts
        double left_x = player_x + distance * (dir_x - plane_x);
        double left_y = player_y + distance * (dir_y - plane_y);
        double step_x = distance * 2 * plane_x / W;
        double step_y = distance * 2 * plane_y / W;

        for (int x = 0; x < W; x++)
        {
            double world_x = left_x + (x + 0.5) * step_x;
            double world_y = left_y + (x + 0.5) * step_y;
            double u = world_x - floor(world_x);
            double v = world_y - floor(world_y);
            set_pixel(x, y, shaded(texel(tex, level, u, v), light));
        }
    }

    for (int column = 0; column < W; column++)
    {
        double camera_x = 2.0 * (column + 0.5) / W - 1;
        double ray_x = dir_x + plane_x * camera_x;
        double ray_y = dir_y + plane_y * camera_x;
        Hit hit = cast_ray(ray_x, ray_y);

        // der Abstand entlang der Blickrichtung: das Skalarprodukt
        // von (Treffpunkt - Spielfigur) mit dir, das die Länge 1 hat
        double distance = (hit.x - player_x) * dir_x + (hit.y - player_y) * dir_y;

        // wo der Strahl die Wand trifft, von 0 bis 1 an der Wand entlang
        double wall_x = (hit.side == 0) ? hit.y : hit.x;
        double u = wall_x - floor(wall_x);
        // von zwei Seiten gesehen wäre die Textur sonst gespiegelt
        if ((hit.side == 0 && ray_x < 0) || (hit.side == 1 && ray_y > 0))
            u = 1 - u;

        // Wände quer zur x-Achse bekommen etwas weniger Licht
        double light = light_at(distance);
        if (hit.side == 0)
            light *= 0.75;
        draw_wall_column(column, distance, hit.cell - '0', u, light);
    }
}

int main(void)
{
    screen_open(W, H, pixel_indexed8, "Raycaster");
    setup_palette();
    make_textures();
    make_mipmaps();
    double last_time = seconds();
    bool show_map = true;

    while (!should_close())
    {
        double now = seconds();
        // nach einem Ruckler nicht mit einem Riesenschritt durch eine Wand
        move_player(fmin(now - last_time, 0.1));
        last_time = now;
        update_camera();

        // M schaltet die Mipmaps ein und aus
        if (key_pressed(pixel_key_m))
            use_mipmaps = !use_mipmaps;

        // Tab blendet die Karte ein und aus
        if (key_pressed(pixel_key_tab))
            show_map = !show_map;

        draw_view();
        if (show_map)
        {
            draw_map(4, 4, 4);
            draw_rays(4, 4, 4);
            draw_player(4, 4, 4);
        }
        present();
    }

    screen_close();
    return 0;
}
