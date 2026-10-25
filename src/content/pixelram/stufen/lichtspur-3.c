#include "pixelram.h"

#define W 160
#define H 90
#define WALL 14
#define CRASH 12
#define STEP (1.0 / 30)   // ein Schritt alle 1/30 Sekunde

typedef struct
{
    int x, y;      // Position
    int dx, dy;    // Richtung
    int color;
    bool crashed;
    int wins;
} Player;

void draw_field(Player *a, Player *b)
{
    clear(0);
    for (int x = 0; x < W; x++)
    {
        set_pixel(x, 0, WALL);
        set_pixel(x, H - 1, WALL);
    }
    for (int y = 0; y < H; y++)
    {
        set_pixel(0, y, WALL);
        set_pixel(W - 1, y, WALL);
    }

    // Siege als Punkte in der oberen Wand: links a, rechts b
    for (int i = 0; i < a->wins; i++)
        set_pixel(2 + 2 * i, 0, a->color);
    for (int i = 0; i < b->wins; i++)
        set_pixel(W - 3 - 2 * i, 0, b->color);
}

void start(Player *p, int x, int dx)
{
    p->x = x;
    p->y = H / 2;
    p->dx = dx;
    p->dy = 0;
    p->crashed = false;
}

void steer(Player *p, pixel_key up, pixel_key down, pixel_key left, pixel_key right)
{
    // Umdrehen geht nicht: nur abbiegen
    if (key_pressed(up)    && p->dy == 0) { p->dx = 0;  p->dy = -1; }
    if (key_pressed(down)  && p->dy == 0) { p->dx = 0;  p->dy = 1;  }
    if (key_pressed(left)  && p->dx == 0) { p->dx = -1; p->dy = 0;  }
    if (key_pressed(right) && p->dx == 0) { p->dx = 1;  p->dy = 0;  }
}

void drive(Player *p)
{
    p->x += p->dx;
    p->y += p->dy;
    if (get_pixel(p->x, p->y) != 0)
        p->crashed = true;
}

int main(void)
{
    screen_open(W, H, pixel_indexed8, "Lichtspur");
    use_palette("sweetie_16");

    Player a = { .color = 3 };    // Orange, fährt mit WASD
    Player b = { .color = 11 };   // Türkis, fährt mit den Pfeiltasten

    bool running = false;   // Die Leertaste startet jede Runde.
    draw_field(&a, &b);

    double last = seconds();
    double lag = 0;         // Zeit, für die noch Schritte fehlen

    while (!should_close())
    {
        double now = seconds();
        lag += now - last;
        last = now;
        if (lag > 0.25)   // nach einer Pause nicht davonrasen
            lag = 0.25;

        if (running)
        {
            steer(&a, pixel_key_w, pixel_key_s, pixel_key_a, pixel_key_d);
            steer(&b, pixel_key_up, pixel_key_down, pixel_key_left, pixel_key_right);
        }
        else if (key_pressed(pixel_key_space))
        {
            draw_field(&a, &b);
            start(&a, 20, 1);
            start(&b, W - 21, -1);
            running = true;
            lag = 0;   // die Runde beginnt ohne Rückstand
        }

        // beide Spuren fahren im selben Takt: immer beide einen Schritt
        while (running && lag >= STEP)
        {
            drive(&a);
            drive(&b);
            if (a.x == b.x && a.y == b.y)   // frontal ineinander
                a.crashed = b.crashed = true;

            set_pixel(a.x, a.y, a.crashed ? CRASH : a.color);
            set_pixel(b.x, b.y, b.crashed ? CRASH : b.color);

            if (a.crashed || b.crashed)
            {
                running = false;
                if (!a.crashed) a.wins++;
                if (!b.crashed) b.wins++;
            }
            lag -= STEP;
        }

        present();
    }

    screen_close();
    return 0;
}
