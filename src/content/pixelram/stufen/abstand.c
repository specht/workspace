#include "pixelram.h"
#include <math.h>

#define W 320
#define H 180

int color(double x, double y)
{
    double dx = x - W / 2.0;
    double dy = y - H / 2.0;
    double distance = sqrt(dx * dx + dy * dy);

    return (int)(distance / 6.0) % 16;
}

int main(void)
{
    screen_open(W, H, pixel_indexed8, "PixelRAM");
    use_palette("sweetie_16");

    for (int y = 0; y < H; y++)
        for (int x = 0; x < W; x++)
            set_pixel(x, y, color(x, y));

    return 0;
}
