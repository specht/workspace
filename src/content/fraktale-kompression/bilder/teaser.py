"""The picture for the card of »Fraktale Bildkompression«: the beach huts in
colour, decoded twice as large from coloured noise, in vertical strips from
left to right:
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

path = os.path.join(HERE, '..', 'beispiele', 'strandhaeuser.png')
y, cb, cr = fraktal.load_color(path)
codes = {'y': fraktal.encode(y), 'cb': fraktal.encode(fraktal.shrink(cb)), 'cr': fraktal.encode(fraktal.shrink(cr))}
scale = 2
h, w = fraktal.picture_size(codes['y'])
y0, cb0, cr0 = fraktal.start_image('rauschen', h * scale, w * scale)
images = {'y': y0, 'cb': cb0, 'cr': cr0}
steps = []


def colour(images):
    cbu = np.repeat(np.repeat(images['cb'], 2, axis=0), 2, axis=1)
    cru = np.repeat(np.repeat(images['cr'], 2, axis=0), 2, axis=1)
    ycbcr = np.clip(np.stack([images['y'], cbu, cru], axis=-1), 0, 255).round().astype(np.uint8)
    return np.asarray(Image.fromarray(ycbcr, 'YCbCr').convert('RGB'))


steps.append(colour(images))
for _ in range(8):
    for name in images:
        images[name] = fraktal.decode_step(images[name], codes[name], scale)
    steps.append(colour(images))

height_full, width = steps[0].shape[:2]
order = [0, 1, 2, 3, 8, 8]
band = width // len(order)
teaser = np.empty_like(steps[0])
for i, step in enumerate(order):
    teaser[:, i * band:(i + 1) * band] = steps[step][:, i * band:(i + 1) * band]
height = width * 9 // 16
top = (height_full - height) // 2
picture = Image.fromarray(teaser[top:top + height])
picture.save(os.path.join(HERE, '..', 'teaser.webp'), quality=88)
print('teaser.webp', picture.size)
