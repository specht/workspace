import numpy as np
from PIL import Image


def blob(dx, dy, sigma):
    return np.exp(-(dx * dx + dy * dy) / (2 * sigma * sigma))


def weight(dx, dy, sx, sy, angle):
    c = np.cos(angle)
    s = np.sin(angle)
    u = (c * dx + s * dy) / sx
    v = (-s * dx + c * dy) / sy
    return np.exp(-(u * u + v * v) / 2)


def save(image, name):
    pixels = np.clip(image, 0, 1) * 255
    Image.fromarray(pixels.astype(np.uint8)).save(name)


if __name__ == '__main__':
    ys, xs = np.mgrid[0:120, 0:160]
    image = np.zeros((120, 160, 3))
    image += weight(xs - 50, ys - 60, 30, 8, 0.5)[:, :, None] * np.array([1.0, 0.3, 0.1])
    image += weight(xs - 100, ys - 50, 10, 25, 0.2)[:, :, None] * np.array([0.1, 0.5, 1.0])
    image += weight(xs - 110, ys - 85, 12, 12, 0.0)[:, :, None] * np.array([0.2, 0.9, 0.2])
    save(image, 'blobs.png')
