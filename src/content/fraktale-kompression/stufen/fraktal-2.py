import sys
import time

import numpy as np
from PIL import Image

R = 8           # so breit und hoch ist ein Block, der ersetzt wird


def load(path):
    return np.asarray(Image.open(path).convert('L'), dtype=float)


def save(image, path):
    Image.fromarray(np.clip(image, 0, 255).round().astype(np.uint8)).save(path)


def shrink(block):
    h, w = block.shape
    return block.reshape(h // 2, 2, w // 2, 2).mean(axis=(1, 3))


def turned(block, k):
    if k >= 4:
        block = np.fliplr(block)
    return np.rot90(block, k % 4)


def fit(domain, target):
    d = domain - domain.mean()
    t = target - target.mean()
    s = (d * t).sum() / max((d * d).sum(), 1e-9)
    s = min(max(s, -1.0), 1.0)
    o = target.mean() - s * domain.mean()
    error = ((s * domain + o - target) ** 2).sum()
    return s, o, error


def mark(image, boxes):
    picture = np.stack([np.clip(image, 0, 255)] * 3, axis=-1).astype(np.uint8)
    for x, y, size, color in boxes:
        picture[y, x:x + size] = color
        picture[y + size - 1, x:x + size] = color
        picture[y:y + size, x] = color
        picture[y:y + size, x + size - 1] = color
    Image.fromarray(picture).resize((picture.shape[1] * 2, picture.shape[0] * 2), Image.NEAREST).save('paar.png')


def main():
    photo = load('strandhaeuser.png')
    x, y = 232, 96
    target = photo[y:y + R, x:x + R]
    best = None
    h, w = photo.shape
    for dy in range(0, h - 2 * R + 1, R):
        for dx in range(0, w - 2 * R + 1, R):
            small = shrink(photo[dy:dy + 2 * R, dx:dx + 2 * R])
            for k in range(8):
                s, o, error = fit(turned(small, k), target)
                if best is None or error < best[0]:
                    best = (error, dx, dy, k, s, o)
    error, dx, dy, k, s, o = best
    print(f'Block ({x}, {y}): am besten ({dx}, {dy}), Lage {k}, s = {s:.2f}, o = {o:.1f}, Fehler {error:.0f}')
    mark(photo, [(x, y, R, (220, 38, 38)), (dx, dy, 2 * R, (43, 95, 180))])


if __name__ == '__main__':
    main()
