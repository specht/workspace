"""The picture for the card of »Fraktale Bildkompression«: the beach huts,
decoded twice as large from noise, in vertical strips from left to right:
the noise, steps 1, 2 and 3, then the finished picture. 16:9, cut from the
middle of the 960 × 640 picture.

    python3 src/content/fraktale-kompression/bilder/teaser.py
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import drawings_container  # noqa: E402

drawings_container.ensure()     # runs in docker/drawings, the same on every machine

import numpy as np  # noqa: E402
from PIL import Image  # noqa: E402

sys.path.insert(0, os.path.join(HERE, '..', 'loesung'))
import fraktal  # noqa: E402

photo = fraktal.load(os.path.join(HERE, '..', 'beispiele', 'strandhaeuser.png'))
code = fraktal.encode(photo)
scale = 2
image = fraktal.start_image('rauschen', photo.shape[0] * scale, photo.shape[1] * scale)
steps = [image]
for _ in range(8):
    image = fraktal.decode_step(image, code, scale)
    steps.append(image)

h, w = steps[0].shape
order = [0, 1, 2, 3, 8, 8]
band = w // len(order)
teaser = np.empty((h, w))
for i, step in enumerate(order):
    teaser[:, i * band:(i + 1) * band] = steps[step][:, i * band:(i + 1) * band]
height = w * 9 // 16
top = (h - height) // 2
picture = Image.fromarray(np.clip(teaser[top:top + height], 0, 255).round().astype(np.uint8))
picture.save(os.path.join(HERE, '..', 'teaser.webp'), quality=88)
print('teaser.webp', picture.size)
