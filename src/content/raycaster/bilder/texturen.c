// Macht textures.png für »Eigene Texturen« aus den Texturen des fertigen
// Raycasters: die sieben Kacheln (Boden, fünf Wandarten, Decke) als Streifen
// von 7 x 64 Pixeln, mit dem kopflosen PixelRAM aus dem Testaufbau:
//   gcc -O2 -std=c11 -I PIXELRAM -o texturen texturen.c PIXELRAM/pixelram.c STUB.c -lm && ./texturen
// dann: python3 -c "from PIL import Image; Image.open('textures.ppm').save('textures.png')"
#define main stage_main
#include "../stufen/12-fertig.c"
#undef main
#include <stdio.h>
int main(void)
{
    screen_open(W, H, pixel_indexed8, "dump");
    setup_palette();
    make_textures();
    FILE *f = fopen("textures.ppm", "wb");
    fprintf(f, "P6\n%d %d\n255\n", TEXTURES * TEX, TEX);
    for (int y = 0; y < TEX; y++)
        for (int t = 0; t < TEXTURES; t++)
            for (int x = 0; x < TEX; x++)
            {
                uint8_t r, g, b;
                get_palette(texture[t][y][x], &r, &g, &b);
                fputc(r, f); fputc(g, f); fputc(b, f);
            }
    fclose(f);
    return 0;
}
