import numpy as np
from PIL import Image


def blob(dx, dy, sigma):
    return np.exp(-(dx * dx + dy * dy) / (2 * sigma * sigma))


def save(image, name):
    pixels = np.clip(image, 0, 1) * 255
    Image.fromarray(pixels.astype(np.uint8)).save(name)


if __name__ == '__main__':
    ys, xs = np.mgrid[0:120, 0:160]
    image = blob(xs - 80, ys - 60, 15)
    save(image, 'blob.png')
