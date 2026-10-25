#include "pixelram.h"
#include <math.h>
#include <stdlib.h>

#define W 320
#define H 180
#define GRAVITY 40.0
#define PI 3.14159265358979
#define MAX_PARTICLES 3000
#define HOUSE 64     // Farbe der Häuser
#define WINDOW 65    // 65 bis 67: drei Farben für Fenster

typedef struct
{
    double x, y;     // Position in Pixeln
    double vx, vy;   // Geschwindigkeit in Pixeln pro Sekunde
    double life;     // verbleibende Lebenszeit in Sekunden
    int ramp;        // Farbverlauf 0 bis 3
    bool rocket;     // eine Rakete explodiert, wenn sie umkehrt
} Particle;

Particle particles[MAX_PARTICLES];
int count = 0;
uint8_t city[H][W];   // die Stadt: 0 ist Himmel, sonst eine Farbe

double random_between(double low, double high)
{
    return low + (high - low) * rand() / (double)RAND_MAX;
}

// Ein Schritt der Zeit dt: erst die Schwerkraft, dann die Bewegung
void move_particle(Particle *p, double dt)
{
    p->vy += GRAVITY * dt;   // die Schwerkraft zieht nach unten
    p->x += p->vx * dt;
    p->y += p->vy * dt;
}

void add(double x, double y, double vx, double vy, double life, int ramp, bool rocket)
{
    if (count == MAX_PARTICLES)
        return;
    particles[count] = (Particle){ x, y, vx, vy, life, ramp, rocket };
    count++;
}

void launch(double x, double target_y)
{
    // so schnell, dass die Rakete genau bei target_y umkehrt
    double vy = -sqrt(2 * GRAVITY * (H - target_y));
    add(x, H - 1, random_between(-8, 8), vy, 10, rand() % 4, true);
}

void explode(Particle *p)
{
    // etwa gleich schnell: die Funken bilden eine Kugelschale, keine volle Scheibe
    for (int i = 0; i < 100; i++)
    {
        double angle = random_between(0, 2 * PI);
        double speed = random_between(30, 50);
        add(p->x, p->y, p->vx + cos(angle) * speed, p->vy + sin(angle) * speed,
            random_between(1.5, 2.5), p->ramp, false);
    }
}

// Vier Farbverläufe zu je 16 Farben: 16 * ramp + Helligkeit
void make_palette(void)
{
    const int sky[3] = {10, 12, 35};
    const int colors[4][3] = { {255, 80, 60}, {80, 255, 120}, {90, 150, 255}, {255, 210, 80} };
    for (int ramp = 0; ramp < 4; ramp++)
        for (int b = 0; b < 16; b++)
        {
            double f = b / 15.0;
            // vom Nachthimmel zur Farbe, ganz hell fast weiß
            double white = f > 0.8 ? (f - 0.8) * 5 : 0;
            int rgb[3];
            for (int i = 0; i < 3; i++)
                rgb[i] = (sky[i] + (colors[ramp][i] - sky[i]) * f) * (1 - white) + 255 * white;
            set_palette(16 * ramp + b, rgb[0], rgb[1], rgb[2]);
        }
}

// Häuser mit zufälliger Breite und Höhe, von links nach rechts. Jedes Haus
// hat seinen eigenen Fensterabstand, und in jedem leuchtet ein anderer Teil
// der Fenster. Die Stadt wird einmal ausgedacht und in city gemalt.
void make_skyline(void)
{
    set_palette(HOUSE, 0, 0, 0);
    set_palette(WINDOW, 255, 210, 120);       // Glühbirne
    set_palette(WINDOW + 1, 255, 150, 70);    // warmes Licht
    set_palette(WINDOW + 2, 140, 180, 255);   // Fernseher

    int x = 0;
    while (x < W)
    {
        int width = 10 + rand() % 20;
        int top = H - 12 - rand() % 30;
        int step = 2 + rand() % 2;                 // Fensterabstand
        double lit = random_between(0.05, 0.4);    // Anteil der hellen Fenster

        for (int i = x; i < x + width && i < W; i++)
            for (int y = top; y < H; y++)
                city[y][i] = HOUSE;

        for (int y = top + 2; y < H - 1; y += 3)
            for (int i = x + 2; i < x + width - 1 && i < W; i += step)
                if (random_between(0, 1) < lit)
                    city[y][i] = WINDOW + rand() % 3;

        x += width;
    }
}

// die Stadt vor den Himmel malen
void draw_skyline(void)
{
    for (int y = 0; y < H; y++)
        for (int x = 0; x < W; x++)
            if (city[y][x] != 0)
                set_pixel(x, y, city[y][x]);
}

// Nachleuchten: jedes Pixel wird ein wenig dunkler
void fade(void)
{
    for (int y = 0; y < H; y++)
        for (int x = 0; x < W; x++)
        {
            int c = get_pixel(x, y);
            if (c % 16 > 0)
                set_pixel(x, y, c - 1);
        }
}

int main(void)
{
    screen_open(W, H, pixel_indexed8, "Feuerwerk");
    make_palette();
    make_skyline();

    double last = seconds();
    double next_rocket = 0;
    bool left = false;
    int frame = 0;

    while (!should_close())
    {
        double now = seconds();
        double dt = now - last;
        if (dt > 0.1)   // nach einer Pause keinen Riesensprung
            dt = 0.1;
        last = now;

        if (now > next_rocket)
        {
            // abwechselnd links und rechts, damit sich die Raketen verteilen
            left = !left;
            double x = left ? random_between(30, W / 2) : random_between(W / 2, W - 30);
            launch(x, random_between(20, 70));
            next_rocket = now + random_between(0.3, 1.2);
        }
        if (mouse_button_pressed(pixel_mouse_left))
        {
            int mx, my;
            mouse_position(&mx, &my);
            launch(mx, my);
        }

        // jedes zweite Bild: die Spuren verblassen langsamer
        frame++;
        if (frame % 2 == 0)
            fade();

        for (int i = 0; i < count; i++)
        {
            Particle *p = &particles[i];
            if (!p->rocket)
            {
                // Luftwiderstand bremst die Funken
                p->vx -= p->vx * 1.5 * dt;
                p->vy -= p->vy * 1.5 * dt;
            }
            move_particle(p, dt);
            p->life -= dt;

            if (p->rocket && p->vy >= 0)
            {
                explode(p);
                p->life = 0;
            }

            if (p->life <= 0 || p->y >= H)
            {
                // Lücke mit dem letzten Partikel füllen
                particles[i] = particles[count - 1];
                count--;
                i--;
                continue;
            }

            int brightness = p->rocket ? 15 : (int)(15 * fmin(1, p->life));
            set_pixel((int)p->x, (int)p->y, 16 * p->ramp + brightness);
        }

        // die Stadt vor dem Himmel: nach den Funken gezeichnet
        draw_skyline();
        present();
    }

    screen_close();
    return 0;
}
