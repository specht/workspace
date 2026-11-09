"""The paper of the Telefonbuch (src/static/telefonbuch.html): a texture
that tiles without seams, with fine grain, a slightly uneven tone and a
few faint stains, as on the thin paper of a real phone book.

    src/scripts/drawings python3 src/scripts/phone-book-paper.py
"""
import numpy as np
from PIL import Image

SIZE = 768
rng = np.random.default_rng(1985)


def periodic_noise(power):
    """noise that tiles: white noise shaped in frequency, 1 / f^power"""
    f = np.fft.fftfreq(SIZE)
    r = np.sqrt(f[:, None] ** 2 + f[None, :] ** 2)
    r[0, 0] = 1
    spectrum = np.fft.fft2(rng.standard_normal((SIZE, SIZE))) / r ** power
    out = np.real(np.fft.ifft2(spectrum))
    return (out - out.mean()) / out.std()


tone = periodic_noise(1.6)              # large, soft unevenness
fibre = periodic_noise(0.6)             # medium texture
grain = rng.standard_normal((SIZE, SIZE))
# splotches: where a low, soft noise rises above a threshold, the paper is a
# little darker and browner, with soft irregular edges (no rings)
blotch = periodic_noise(2.4)
stains = np.clip(blotch - 1.1, 0, None)
stains = stains / stains.max()
base = np.array([248, 242, 222], float)      # the paper
brown = np.array([-10, -16, -30], float)     # what a stain takes away
light = 3.5 * tone + 1.6 * fibre + 2.2 * grain
img = base[None, None, :] + light[..., None] + stains[..., None] * brown[None, None, :] * 0.7
Image.fromarray(np.clip(img, 0, 255).astype(np.uint8)).save('src/static/include/phone-book-paper.webp', quality=82)
print('src/static/include/phone-book-paper.webp')
