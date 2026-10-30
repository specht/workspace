"""The finished raytracer of the page (stufen/17-belichtung.c) at 1920 × 1080
for the picture at the top, raytracer-gross.webp: the students' own program,
unchanged but for its size, compiled natively instead of for the browser.
PixelRAM is replaced by a small stand-in that never opens a window and
stops after PASSES passes; then the program writes the average of its
passes (sum / passes, before exposure) to a file. Several processes with
different seeds run side by side, their averages are averaged, and the
result goes through exposure, tone mapping and gamma exactly as to_byte.

    python3 src/content/raytracing/bilder/render_large.py [passes per process] [processes]

Defaults: 512 passes in each of 6 processes, about a quarter of an hour on
six cores (a pass at 1920 × 1080 takes about 1.5 seconds).
"""
import os
import re
import subprocess
import sys
import tempfile

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.dirname(HERE)
SOURCE = os.path.join(OUT, 'stufen', '17-belichtung.c')
W, H = 1920, 1080

STAND_IN = r'''
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
enum { pixel_rgb24 };
enum { pixel_key_left, pixel_key_right, pixel_key_up, pixel_key_down, pixel_key_h, pixel_key_d };
static int frames_done = 0;
static void screen_open(int w, int h, int format, const char *title) {}
static bool should_close(void) { return frames_done >= PASSES; }
static double seconds(void) { return 0; }
static bool key_down(int key) { return false; }
static bool key_pressed(int key) { return false; }
static void set_title(const char *title) {}
static void set_pixel_rgb(int x, int y, uint8_t r, uint8_t g, uint8_t b) {}
static void present(void) { frames_done++; }
'''

WRITE_SUM = r'''
// the average of all passes, before exposure, as doubles (render_large.py)
void write_sum(int passes)
{
    FILE *f = fopen(OUT_FILE, "wb");
    for (int y = 0; y < H; y++)
        for (int x = 0; x < W; x++)
        {
            double c[3] = {sum[y][x].x / passes, sum[y][x].y / passes, sum[y][x].z / passes};
            fwrite(c, sizeof c, 1, f);
        }
    fclose(f);
}

int main(void)'''


def program(source):
    """The page's program with its changes; each must match once."""
    changes = [
        (r'#include "pixelram.h"\n', STAND_IN),
        (r'#define W 320\n', f'#define W {W}\n'),
        (r'#define H 180\n', f'#define H {H}\n'),
        (r'uint32_t seed = \d+u;', 'uint32_t seed = SEED;'),
        (r'\nint main\(void\)', WRITE_SUM),
        (r'screen_close\(\);', 'write_sum(passes);'),
    ]
    for pattern, replacement in changes:
        source, n = re.subn(pattern, lambda _m: replacement, source)
        if n != 1:
            sys.exit(f'{pattern!r} found {n} times in {SOURCE}')
    return source


def exposed(c, exposure=1.5):
    """to_byte of the program: exposure, tone mapping, gamma."""
    c = 1 - np.exp(-c * exposure)
    return np.clip(np.power(c, 1 / 2.2) * 255 + 0.5, 0, 255).astype(np.uint8)


def main():
    passes = int(sys.argv[1]) if len(sys.argv) > 1 else 512
    processes = int(sys.argv[2]) if len(sys.argv) > 2 else 6
    with tempfile.TemporaryDirectory() as tmp:
        c_file = os.path.join(tmp, 'raytracer.c')
        with open(SOURCE) as f:
            text = program(f.read())
        with open(c_file, 'w') as f:
            f.write(text)
        runs = []
        for i in range(processes):
            binary = os.path.join(tmp, f'raytracer-{i}')
            out = os.path.join(tmp, f'sum-{i}.bin')
            # a different seed for each process (xorshift: never 0)
            seed = 2463534242 + 7919 * (i + 1)
            subprocess.run(['cc', '-O2', '-march=native', f'-DPASSES={passes}', f'-DSEED={seed}u',
                            f'-DOUT_FILE="{out}"', c_file, '-o', binary, '-lm'], check=True)
            runs.append((subprocess.Popen([binary]), out))
        for proc, _ in runs:
            if proc.wait() != 0:
                sys.exit('a render process failed')
        image = sum(np.fromfile(out, dtype=np.float64).reshape(H, W, 3) for _, out in runs) / processes
    Image.fromarray(exposed(image)).save(os.path.join(OUT, 'raytracer-gross.webp'), quality=90, method=6)
    print(f'{processes} × {passes} passes written to raytracer-gross.webp')


if __name__ == '__main__':
    main()
