"""The picture for the card of »Video komprimieren«: a frame of »Spring«
(Andy Goralczyk, © Blender Foundation | cloud.blender.org/spring, CC BY 4.0)
at 395 s, the girl and her dog on a path in the evening sun, cut to 16 : 9
around them, and over it the blocks a codec might choose: a block of 100
pixels is split in four while its brightness varies a lot (the standard
deviation above a threshold, the same share of blocks split at each size),
down to 25 pixels, each edge a light and a dark line. Large blocks where the sky
and the blurred mountain are calm, small ones at rocks, grass and figures.
1600 × 900, a photo, so one WebP for both themes.

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/video-compression/bilder/teaser.py spring.webm

spring.webm is the film from Wikimedia Commons (clips/make.py says how to get it).
"""

import os
import subprocess
import sys

import numpy as np
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, '..', 'teaser.webp')
AT = 395.0
W, H = 1600, 900
SOURCE_W, SOURCE_H = 2048, 858
LEFT = 488                                  # the 16 : 9 cut: 1525 of the 2048 columns
SIZES = [100, 50, 25]
SPLIT = [0.4, 0.2]                          # the share of all blocks of 100, of all blocks of 50 split
LIGHT, DARK = (255, 255, 255, 130), (0, 0, 0, 120)


def frame(path):
    command = ['ffmpeg', '-v', 'error', '-ss', str(AT), '-i', path, '-frames:v', '1',
               '-f', 'rawvideo', '-pix_fmt', 'rgb24', '-']
    data = subprocess.run(command, capture_output=True, check=True).stdout
    picture = Image.fromarray(np.frombuffer(data, np.uint8).reshape(SOURCE_H, SOURCE_W, 3))
    cut = round(SOURCE_H * 16 / 9)
    return picture.crop((LEFT, 0, LEFT + cut, SOURCE_H)).resize((W, H), Image.LANCZOS)


def spread(luma, size):
    """the standard deviation of the brightness in each block of a size"""
    return luma.reshape(H // size, size, W // size, size).std(axis=(1, 3))


def blocks(luma, thresholds, x, y, level=0):
    size = SIZES[level]
    if level < len(SIZES) - 1 and luma[y:y + size, x:x + size].std() > thresholds[level]:
        half = SIZES[level + 1]
        for dy in (0, half):
            for dx in (0, half):
                yield from blocks(luma, thresholds, x + dx, y + dy, level + 1)
    else:
        yield x, y, size


def main(path):
    picture = frame(path)
    luma = np.asarray(picture.convert('L'), dtype=float)
    thresholds = [np.quantile(spread(luma, size), 1 - share) for size, share in zip(SIZES, SPLIT)]
    overlay = Image.new('RGBA', (W, H), (0, 0, 0, 0))
    draw = ImageDraw.Draw(overlay)
    counts = {s: 0 for s in SIZES}
    for y in range(0, H, SIZES[0]):
        for x in range(0, W, SIZES[0]):
            for bx, by, size in blocks(luma, thresholds, x, y):
                counts[size] += 1
                # the top and left edge of each block, a light and a dark
                # line side by side: seen on bright grass and dark rock alike,
                # without washing the picture out
                for k, colour in ((0, LIGHT), (1, DARK)):
                    draw.line([bx, by + k, bx + size, by + k], fill=colour)
                    draw.line([bx + k, by, bx + k, by + size], fill=colour)
    picture = Image.alpha_composite(picture.convert('RGBA'), overlay).convert('RGB')
    picture.save(OUT, quality=86)
    print(OUT, counts)


main(sys.argv[1])
