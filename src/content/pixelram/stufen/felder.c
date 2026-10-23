#include "pixelram.h"

#define W 320
#define H 180

int color(int x, int y)
{
    return (x / 20 + y / 20) % 16;
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
