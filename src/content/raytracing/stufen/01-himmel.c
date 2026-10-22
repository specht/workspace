#include "pixelram.h"
#include <math.h>

#define W 320
#define H 180

// ein Vektor: ein Punkt, eine Richtung oder eine Farbe (x, y, z = rot, grün, blau)
typedef struct
{
    double x, y, z;
} vec;

vec add(vec a, vec b)
{
    return (vec){a.x + b.x, a.y + b.y, a.z + b.z};
}

vec scale(vec a, double s)
{
    return (vec){a.x * s, a.y * s, a.z * s};
}

double dot(vec a, vec b)
{
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

double length(vec a)
{
    return sqrt(dot(a, a));
}

// gleiche Richtung, Länge 1
vec normalize(vec a)
{
    return scale(a, 1 / length(a));
}

// überblenden: t = 0 ergibt a, t = 1 ergibt b
vec mix(vec a, vec b, double t)
{
    return add(scale(a, 1 - t), scale(b, t));
}

// der Himmel: am Horizont hell, nach oben tiefblau
vec sky(vec dir)
{
    double up = dir.y > 0 ? sqrt(dir.y) : 0;
    return mix((vec){0.95, 0.9, 0.85}, (vec){0.15, 0.4, 0.9}, up);
}

// die Richtung vom Auge durch den Pixel (x, y)
vec ray_dir(double x, double y)
{
    double sx = (x - W / 2.0) / H;
    double sy = (H / 2.0 - y) / H;
    return normalize((vec){sx, sy, -1});
}

// Farbwert 0 bis 1 zu 0 bis 255
uint8_t to_byte(double c)
{
    if (c > 1)
        c = 1;
    return (uint8_t)(c * 255 + 0.5);
}

void draw(void)
{
    for (int y = 0; y < H; y++)
    {
        for (int x = 0; x < W; x++)
        {
            vec c = sky(ray_dir(x + 0.5, y + 0.5));
            set_pixel_rgb(x, y, to_byte(c.x), to_byte(c.y), to_byte(c.z));
        }
    }
}

int main(void)
{
    screen_open(W, H, pixel_rgb24, "Raytracer");
    draw();
    while (!should_close())
        present();
    screen_close();
    return 0;
}
