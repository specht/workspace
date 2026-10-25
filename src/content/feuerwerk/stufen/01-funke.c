#include "pixelram.h"
#include <stdlib.h>

#define W 320
#define H 180
#define GRAVITY 40.0

typedef struct
{
    double x, y;     // Position in Pixeln
    double vx, vy;   // Geschwindigkeit in Pixeln pro Sekunde
} Particle;

double random_between(double low, double high)
{
    return low + (high - low) * rand() / (double)RAND_MAX;
}

void throw_spark(Particle *p)
{
    p->x = 20;
    p->y = H - 1;
    p->vx = random_between(20, 60);
    p->vy = random_between(-110, -60);
}

// Ein Schritt der Zeit dt: erst die Schwerkraft, dann die Bewegung
void move_particle(Particle *p, double dt)
{
    p->vy += GRAVITY * dt;   // die Schwerkraft zieht nach unten
    p->x += p->vx * dt;
    p->y += p->vy * dt;
}

int main(void)
{
    screen_open(W, H, pixel_indexed8, "Ein Funke");
    use_palette("sweetie_16");

    Particle spark;
    throw_spark(&spark);

    double last = seconds();
    while (!should_close())
    {
        double now = seconds();
        double dt = now - last;   // Sekunden seit dem letzten Bild
        if (dt > 0.1)   // nach einer Pause keinen Riesensprung
            dt = 0.1;
        last = now;

        move_particle(&spark, dt);

        if (spark.x >= W || spark.y >= H)
            throw_spark(&spark);

        set_pixel((int)spark.x, (int)spark.y, 4);
        present();
    }

    screen_close();
    return 0;
}
