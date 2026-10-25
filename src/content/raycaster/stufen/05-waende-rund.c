#include "pixelram.h"
#include <math.h>

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

// die Farben, die wir benutzen (Nummern in der Palette)
enum
{
    BLACK,
    MAP_FLOOR,
    PLAYER,
    VIEW,
    RAY,
    RAY_SIDE,
    CAMERA_PLANE,
    CEILING,
    FLOOR,
    WALL = 10,     // 10 bis 14: die fünf Wandarten
    WALL_DARK = 20 // 20 bis 24: dieselben etwas dunkler
};

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

void setup_palette(void)
{
    set_palette(BLACK, 0, 0, 0);
    set_palette(MAP_FLOOR, 38, 38, 46);
    set_palette(PLAYER, 255, 214, 64);
    set_palette(VIEW, 255, 255, 255);
    set_palette(RAY, 196, 160, 72);
    set_palette(RAY_SIDE, 72, 176, 210);
    set_palette(CAMERA_PLANE, 255, 92, 92);

    set_palette(WALL + 0, 178, 82, 58);   // Ziegel
    set_palette(WALL + 1, 138, 144, 160); // Stein
    set_palette(WALL + 2, 160, 108, 58);  // Holz
    set_palette(WALL + 3, 84, 132, 164);  // Metall
    set_palette(WALL + 4, 92, 140, 70);   // Moos
    set_palette(CEILING, 52, 52, 60);
    set_palette(FLOOR, 92, 84, 76);

    // die Wände quer zur x-Achse etwas dunkler, damit man Ecken sieht
    for (int i = 0; i < 5; i++)
    {
        uint8_t r, g, b;
        get_palette(WALL + i, &r, &g, &b);
        set_palette(WALL_DARK + i, r * 7 / 10, g * 7 / 10, b * 7 / 10);
    }
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
    return WALL + (cell - '1');
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

    // in welche Richtung es weitergeht
    int step_x = (ray_x < 0) ? -1 : 1;
    int step_y = (ray_y < 0) ? -1 : 1;

    // t an der ersten Gitterlinie in dieser Richtung: nach rechts liegt sie
    // bei cell_x + 1, nach links bei cell_x, und (step_x > 0) ist 1 oder 0;
    // mal step_x wird der Abstand positiv. In y ebenso.
    double next_x = (cell_x + (step_x > 0) - player_x) * step_x * delta_x;
    double next_y = (cell_y + (step_y > 0) - player_y) * step_y * delta_y;

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

    // nicht aus der Karte hinaus; Wände halten die Figur erst später auf
    player_x = fmin(fmax(player_x + dx, 0.5), MAP_W - 0.5);
    player_y = fmin(fmax(player_y + dy, 0.5), MAP_H - 0.5);
}

// eine senkrechte Wandscheibe: je weiter weg, desto niedriger;
// im Abstand 1 ist die Wand genau H Pixel hoch
void draw_wall_column(int column, double distance, int color)
{
    double wall_height = H / fmax(distance, 0.01);
    double wall_top = H / 2.0 - wall_height / 2;

    for (int y = 0; y < H; y++)
    {
        // wo diese Zeile auf der Wand liegt: 0 oben, 1 unten
        double v = (y + 0.5 - wall_top) / wall_height;
        if (v >= 0 && v < 1)
            set_pixel(column, y, color);
    }
}

// das Bild aus Sicht der Spielfigur: für jede Spalte ein Strahl
void draw_view(void)
{
    // oben die Decke, unten der Boden
    fill_rect(0, 0, W, H / 2, CEILING);
    fill_rect(0, H / 2, W, H / 2, FLOOR);

    for (int column = 0; column < W; column++)
    {
        double camera_x = 2.0 * (column + 0.5) / W - 1;
        double ray_x = dir_x + plane_x * camera_x;
        double ray_y = dir_y + plane_y * camera_x;
        Hit hit = cast_ray(ray_x, ray_y);

        // der Abstand: direkt von der Spielfigur zum Treffpunkt
        double distance = hypot(hit.x - player_x, hit.y - player_y);

        int color = wall_color(hit.cell);
        if (hit.side == 0)
            color += WALL_DARK - WALL;
        draw_wall_column(column, distance, color);
    }
}

int main(void)
{
    screen_open(W, H, pixel_indexed8, "Raycaster");
    setup_palette();
    double last_time = seconds();
    bool show_map = true;

    while (!should_close())
    {
        double now = seconds();
        move_player(now - last_time);
        last_time = now;
        update_camera();

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
