"""The card picture of »Audioeffekte«: the first second of stimme.wav as a
wave, and behind it its echoes as echo() of the page makes them (0,3 s
apart, each half as high as the one before), in purple, each fainter than
the one before, on a dark ground. 1600 × 900, a picture for both themes.

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/audio-effects/bilder/teaser.py
"""

import os
import wave

import numpy as np
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
SOURCE = os.path.join(HERE, '..', '..', '..', 'static', 'audio', 'stimme.wav')
OUT = os.path.join(HERE, '..', 'teaser.webp')
W, H = 1600, 900
SECONDS = 2.4
DELAY, FEEDBACK = 0.3, 0.5

with wave.open(SOURCE, 'rb') as f:
    rate = f.getframerate()
    x = np.frombuffer(f.readframes(f.getnframes()), '<i2') / 32767
# a word of the voice: the loudest second
start = int(np.argmax(np.convolve(np.abs(x), np.ones(rate), 'valid')))
word = x[start:start + int(0.9 * rate)]

image = Image.new('RGB', (W, H), (24, 22, 38))
draw = ImageDraw.Draw(image, 'RGBA')
mid = H / 2
columns = W
seconds_per_px = SECONDS / columns


def wave_at(offset, height, colour):
    """the word's envelope, column by column, from offset seconds on"""
    for px in range(columns):
        t = px * seconds_per_px - offset
        if t < 0 or t >= len(word) / rate:
            continue
        a = int(t * rate)
        b = max(a + 1, int((t + seconds_per_px) * rate))
        piece = word[a:b]
        top, bottom = piece.max(), piece.min()
        draw.line([(px, mid - top * height * 380), (px, mid - bottom * height * 380)], fill=colour, width=1)


for k in range(4, 0, -1):
    alpha = int(255 * 0.75 ** k)
    wave_at(0.25 + k * DELAY, FEEDBACK ** k, (151, 117, 250, alpha))
wave_at(0.25, 1.0, (233, 236, 239, 255))
image.save(OUT, quality=88)
print(OUT)
