"""Pictures of the chapter »Filterkernel« in fire.md: a white-hot ring,
treated with a kernel once or several times, in the palette of the page
(64 colours from black over red, orange and yellow to white). Like the
program: a pixel outside the picture is 0, and the sum is divided by the
weights with integer division; unlike it, without chance and from a copy of
the picture, so the picture shows the kernel alone. The edge kernel's weights
add up to 0: its sum is not divided, only made positive.

    python3 src/content/fire/bilder/kernel.py   (writes next to fire.md)
"""
import os

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.dirname(HERE)
SIZE = 64
SCALE = 4


def palette(parts):
    """64 colours from four sections of 16, as set_palette in the program."""
    colours = []
    for section in range(4):
        for i in range(16):
            colours.append(parts[section](i))
    return np.array(colours, dtype=np.uint8)


FIRE = palette([
    lambda i: (i * 8, 0, 0),
    lambda i: (i * 8 + 128, i * 8, 0),
    lambda i: (255, i * 8 + 128, i * 8),
    lambda i: (255, 255, i * 8 + 128),
])
def ring():
    y, x = np.mgrid[0:SIZE, 0:SIZE]
    d = np.hypot(x - 31.5, y - 39.5)
    return np.where((d >= 11) & (d <= 14), 63, 0).astype(np.int64)


def apply(picture, kernel, steps=1, absolute=False, keep=None):
    """keep: the hot pixels drawn again after every step, as the program
    draws its glow or its shape again in every frame."""
    kernel = np.array(kernel)
    weight = kernel.sum() or 1
    for _ in range(steps):
        padded = np.pad(picture, 1)
        total = np.zeros_like(picture)
        for row in range(3):
            for col in range(3):
                total += kernel[row, col] * padded[row:row + SIZE, col:col + SIZE]
        if absolute:
            total = np.abs(total)
        picture = np.clip(total // weight, 0, 63)
        if keep is not None:
            picture = np.maximum(picture, keep)
    return picture


def save(name, picture):
    image = Image.fromarray(FIRE[picture])
    image = image.resize((SIZE * SCALE, SIZE * SCALE), Image.NEAREST)
    image.save(os.path.join(OUT, name), lossless=True)


IDENTITY = [[0, 0, 0], [0, 1, 0], [0, 0, 0]]
BOX = [[1, 1, 1], [1, 1, 1], [1, 1, 1]]
EDGE = [[0, -1, 0], [-1, 4, -1], [0, -1, 0]]
FIRE_KERNEL = [[0, 0, 0], [1, 0, 1], [0, 2, 0]]
GLOW = [[0, 1, 0], [1, 0, 1], [0, 1, 0]]

start = ring()
# once: what a kernel does
save('kernel-identity.webp', apply(start, IDENTITY))
save('kernel-weich.webp', apply(start, BOX))
save('kernel-kanten.webp', apply(start, EDGE, absolute=True))
save('kernel-feuer-1.webp', apply(start, FIRE_KERNEL))
# again and again, the ring kept hot: the heat moves
for steps in (4, 12, 40):
    save(f'kernel-feuer-{steps}.webp', apply(start, FIRE_KERNEL, steps, keep=start))
save('kernel-gluehen-40.webp', apply(start, GLOW, 40, keep=start))
