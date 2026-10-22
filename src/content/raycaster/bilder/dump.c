// Macht die Bilder palette.webp und mipmaps.webp für »Raycasting in C« aus den
// Tabellen von Stufe 11 (Palette, Texturen, Mipmaps), mit dem kopflosen
// PixelRAM aus dem Testaufbau (raylib-Stub):
//   gcc -O2 -std=c11 -I PIXELRAM -o dump dump.c PIXELRAM/pixelram.c STUB.c -lm && ./dump
// dann python3 transparent.py (magenta wird durchsichtig, als WebP).
#define main stage_main
#include "../stufen/11-mipmaps.c"
#undef main
#include <stdio.h>
static void rgb(int c, uint8_t *r, uint8_t *g, uint8_t *b) { get_palette(c, r, g, b); }
int main(void)
{
    screen_open(W, H, pixel_indexed8, "dump");
    setup_palette();
    make_textures();
    make_mipmaps();
    // palette: 16 hues (rows) x 16 levels (columns), 24 px each
    FILE *f = fopen("palette.ppm", "wb");
    int cs = 24;
    fprintf(f, "P6\n%d %d\n255\n", 16 * cs, 16 * cs);
    for (int y = 0; y < 16 * cs; y++)
        for (int x = 0; x < 16 * cs; x++)
        {
            uint8_t r, g, b; rgb(COLOR(y / cs, x / cs), &r, &g, &b);
            if (x % cs == 0 || y % cs == 0) { r = 255; g = 0; b = 255; } // gap: transparent later
            fputc(r, f); fputc(g, f); fputc(b, f);
        }
    fclose(f);
    // mip levels of every texture: each level drawn 128 x 128 (nearest)
    for (int t = 0; t < TEXTURES; t++)
    {
        char name[64]; snprintf(name, sizeof name, "mip-%d.ppm", t);
        f = fopen(name, "wb");
        int s = 128, gap = 16;
        int w = MIP_LEVELS * s + (MIP_LEVELS - 1) * gap;
        fprintf(f, "P6\n%d %d\n255\n", w, s);
        for (int y = 0; y < s; y++)
            for (int x = 0; x < w; x++)
            {
                int level = x / (s + gap), lx = x % (s + gap);
                uint8_t r = 255, g = 0, b = 255; // gap: transparent later
                if (lx < s)
                {
                    int size = TEX >> level;
                    rgb(mip[t][level][y * size / s][lx * size / s], &r, &g, &b);
                }
                fputc(r, f); fputc(g, f); fputc(b, f);
            }
        fclose(f);
    }
    screen_close();
    return 0;
}
