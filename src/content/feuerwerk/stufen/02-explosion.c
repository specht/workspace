#include "pixelram.h"
#include <math.h>
#include <stdlib.h>

#define W 320
#define H 180
#define GRAVITY 40.0
#define PI 3.14159265358979
#define MAX_PARTICLES 2000

typedef struct
{
    double x, y;     // Position in Pixeln
    double vx, vy;   // Geschwindigkeit in Pixeln pro Sekunde
    double life;     // verbleibende Lebenszeit in Sekunden
} Particle;

Particle particles[MAX_PARTICLES];
int count = 0;

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

void add(double x, double y, double vx, double vy, double life)
{
    if (count == MAX_PARTICLES)
        return;
    particles[count] = (Particle){ x, y, vx, vy, life };
    count++;
}

void explode(double x, double y)
{
    for (int i = 0; i < 100; i++)
    {
        double angle = random_between(0, 2 * PI);
        double speed = random_between(30, 50);
        add(x, y, cos(angle) * speed, sin(angle) * speed, random_between(1.5, 2.5));
    }
}

int main(void)
{
    screen_open(W, H, pixel_indexed8, "Explosion");
    use_palette("sweetie_16");

    double last = seconds();
    double next_explosion = 0;

    while (!should_close())
    {
        double now = seconds();
        double dt = now - last;
        if (dt > 0.1)   // nach einer Pause keinen Riesensprung
            dt = 0.1;
        last = now;

        if (now > next_explosion)
        {
            explode(random_between(60, W - 60), random_between(30, 90));
            next_explosion = now + 1.0;
        }

        clear(0);
        for (int i = 0; i < count; i++)
        {
            Particle *p = &particles[i];
            move_particle(p, dt);
            p->life -= dt;

            if (p->life <= 0)
            {
                // verloschen: der letzte Funke rückt an seinen Platz
                particles[i] = particles[count - 1];
                count--;
                i--;
                continue;
            }

            set_pixel((int)p->x, (int)p->y, 12);
        }

        present();
    }

    screen_close();
    return 0;
}
