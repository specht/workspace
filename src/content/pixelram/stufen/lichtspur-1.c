#include "pixelram.h"

#define W 160
#define H 90
#define STEP (1.0 / 30)   // ein Schritt alle 1/30 Sekunde

int main(void)
{
    screen_open(W, H, pixel_indexed8, "Lichtspur");
    use_palette("sweetie_16");

    int x = 20, y = H / 2;   // Position
    int dx = 1, dy = 0;      // Richtung: nach rechts

    double last = seconds();
    double lag = 0;          // Zeit, für die noch Schritte fehlen

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

        while (lag >= STEP)
        {
            x += dx;
            y += dy;
            set_pixel(x, y, 11);
            lag -= STEP;
        }

        present();
    }

    screen_close();
    return 0;
}
