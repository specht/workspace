#include "pixelram.h"
#include <math.h>

#define W 320
#define H 180

// Sekunden seit dem Start, für jedes Bild neu
double t;

// zwei Wellenquellen, die durchs Bild wandern
double ax, ay, bx, by;

int color(double x, double y)
{
    double d1 = hypot(x - ax, y - ay);
    double d2 = hypot(x - bx, y - by);

    double wave = sin(d1 * 0.12 - t * 1.0) + sin(d2 * 0.12 + t * 1.15);

    return (int)((wave + 2) * 3.75);
}

// wie oft ein Pixel an einer Kante höchstens geteilt wird
#define LEVELS 1

// die Farben an den Ecken der Pixel: eine Zeile und eine Spalte mehr
int corner[H + 1][W + 1];

// Die Farbe eines Quadrats mit der linken oberen Ecke (x, y) und der
// Seitenlänge size; c00, c10, c01 und c11 sind die Farben seiner Ecken.
void square(double x, double y, double size, int c00, int c10, int c01, int c11, int levels, int rgb[3])
{
    if (levels == 0 || (c00 == c10 && c00 == c01 && c00 == c11))
    {
        // alle Ecken gleich (oder nicht weiter teilen): eine Farbe
        uint8_t r, g, b;
        get_palette(c00, &r, &g, &b);
        rgb[0] = r;
        rgb[1] = g;
        rgb[2] = b;
        return;
    }

    // eine Kante läuft durch: in vier kleinere Quadrate teilen
    double half = size / 2;
    int top    = color(x + half, y);
    int left   = color(x,        y + half);
    int middle = color(x + half, y + half);
    int right  = color(x + size, y + half);
    int bottom = color(x + half, y + size);

    int upper_left[3], upper_right[3], lower_left[3], lower_right[3];
    square(x,        y,        half, c00,    top,    left,   middle, levels - 1, upper_left);
    square(x + half, y,        half, top,    c10,    middle, right,  levels - 1, upper_right);
    square(x,        y + half, half, left,   middle, c01,    bottom, levels - 1, lower_left);
    square(x + half, y + half, half, middle, right,  bottom, c11,    levels - 1, lower_right);

    // und ihre Farben mischen
    for (int i = 0; i < 3; i++)
        rgb[i] = (upper_left[i] + upper_right[i] + lower_left[i] + lower_right[i]) / 4;
}

// zeichnet das ganze Bild mit color, die Kanten geglättet
void draw(void)
{
    for (int y = 0; y <= H; y++)
        for (int x = 0; x <= W; x++)
            corner[y][x] = color(x, y);

    for (int y = 0; y < H; y++)
        for (int x = 0; x < W; x++)
        {
            int rgb[3];
            square(x, y, 1.0, corner[y][x], corner[y][x + 1], corner[y + 1][x], corner[y + 1][x + 1], LEVELS, rgb);
            set_pixel_rgb(x, y, rgb[0], rgb[1], rgb[2]);
        }
}

int main(void)
{
    screen_open(W, H, pixel_rgb24, "Interferenz");
    use_palette("sweetie_16");

    while (!should_close())
    {
        t = seconds();
        ax = W / 2.0 + cos(t * 0.4) * 70;
        ay = H / 2.0 + sin(t * 0.55) * 40;
        bx = W / 2.0 + cos(t * 0.65) * 90;
        by = H / 2.0 + sin(t * 0.35) * 50;

        draw();
        present();
    }

    screen_close();
    return 0;
}
