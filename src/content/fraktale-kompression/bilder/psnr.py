"""The pictures of »Wie man Fehler in Dezibel misst«: the same corner of the
beach huts (the middle roof and the latch) at about 25, 30, 35 and 40 dB,
each from a JPEG whose quality is chosen so that the whole photo comes
closest to that PSNR; enlarged twice without smoothing, so the errors show.

    python3 src/content/fraktale-kompression/bilder/psnr.py
"""

import io
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import drawings_container  # noqa: E402

drawings_container.ensure()     # runs in docker/drawings, the same on every machine

import numpy as np  # noqa: E402
from PIL import Image  # noqa: E402

photo = Image.open(os.path.join(HERE, '..', 'beispiele', 'strandhaeuser.png')).convert('RGB')
original = np.asarray(photo, dtype=float)
results = []
for quality in range(1, 96):
    data = io.BytesIO()
    photo.save(data, 'JPEG', quality=quality)
    decoded = Image.open(io.BytesIO(data.getvalue())).convert('RGB')
    error = ((np.asarray(decoded, dtype=float) - original) ** 2).mean()
    results.append((10 * np.log10(255 ** 2 / error), quality, decoded))

box = (176, 72, 336, 232)         # the middle roof and the latch, 160 × 160 pixels
for target in (25, 30, 35, 40):
    value, quality, decoded = min(results, key=lambda r: abs(r[0] - target))
    crop = decoded.crop(box).resize((320, 320), Image.NEAREST)
    crop.save(os.path.join(HERE, '..', f'psnr-{target}.webp'), lossless=True)
    print(f'psnr-{target}.webp: quality {quality}, {value:.1f} dB')
