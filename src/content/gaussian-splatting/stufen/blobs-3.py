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


def error(image, target):
    return np.mean((image - target) ** 2)


def save(image, name):
    pixels = np.clip(image, 0, 1) * 255
    Image.fromarray(pixels.astype(np.uint8)).save(name)


def load(name):
    return np.asarray(Image.open(name).convert('RGB'), dtype=float) / 255


if __name__ == '__main__':
    target = load('pearl-earring.png')
    H, W, _ = target.shape
    rng = np.random.default_rng(1)
    ys, xs = np.mgrid[0:H, 0:W]
    image = np.zeros((H, W, 3))
    for i in range(200):
        x = rng.random() * W
        y = rng.random() * H
        color = target[int(y), int(x)]
        image += weight(xs - x, ys - y, 6, 6, 0)[:, :, None] * color * 0.3
    save(image, 'random.png')
    print('Fehler:', error(image, target))
