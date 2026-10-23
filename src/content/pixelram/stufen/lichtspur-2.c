#include "pixelram.h"

#define W 160
#define H 90
#define STEP (1.0 / 30)   // ein Schritt alle 1/30 Sekunde

int main(void)
{
    screen_open(W, H, pixel_indexed8, "Lichtspur");
    use_palette("sweetie_16");

    for (int x = 0; x < W; x++)
    {
        set_pixel(x, 0, 14);
        set_pixel(x, H - 1, 14);
    }
    for (int y = 0; y < H; y++)
    {
        set_pixel(0, y, 14);
        set_pixel(W - 1, y, 14);
    }

    int x = 20, y = H / 2;   // Position
    int dx = 1, dy = 0;      // Richtung: nach rechts

    double last = seconds();
    double lag = 0;          // Zeit, für die noch Schritte fehlen
    bool crashed = false;

    while (!should_close())
    {
        if (key_pressed(pixel_key_up))    { dx = 0;  dy = -1; }
        if (key_pressed(pixel_key_down))  { dx = 0;  dy = 1;  }
        if (key_pressed(pixel_key_left))  { dx = -1; dy = 0;  }
        if (key_pressed(pixel_key_right)) { dx = 1;  dy = 0;  }

        double now = seconds();
        lag += now - last;
        last = now;
        if (lag > 0.25)   // nach einer Pause nicht davonrasen
            lag = 0.25;

        while (!crashed && lag >= STEP)
        {
            x += dx;
            y += dy;
            if (get_pixel(x, y) != 0)
                crashed = true;

            set_pixel(x, y, crashed ? 2 : 11);
            lag -= STEP;
        }

        present();
    }

    screen_close();
    return 0;
}
