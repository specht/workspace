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


def domains(image):
    blocks, places = [], []
    h, w = image.shape
    for y in range(0, h - 2 * R + 1, R):
        for x in range(0, w - 2 * R + 1, R):
            small = shrink(image[y:y + 2 * R, x:x + 2 * R])
            for k in range(8):
                blocks.append(turned(small, k).ravel())
                places.append((x, y, k))
    return np.array(blocks), places


def encode(image):
    blocks, places = domains(image)
    means = blocks.mean(axis=1)
    centred = blocks - means[:, None]
    spread = (centred * centred).sum(axis=1)
    code = []
    h, w = image.shape
    for y in range(0, h, R):
        for x in range(0, w, R):
            target = image[y:y + R, x:x + R].ravel()
            t = target - target.mean()
            dots = centred @ t
            s = np.clip(dots / np.maximum(spread, 1e-9), -1, 1)
            error = t @ t - 2 * s * dots + s * s * spread
            best = int(error.argmin())
            dx, dy, k = places[best]
            o = target.mean() - s[best] * means[best]
            code.append((x, y, dx, dy, k, s[best], o))
    return np.array(code)


def decode_step(image, code):
    new = np.empty_like(image)
    for x, y, dx, dy, k, s, o in code:
        x, y, dx, dy, k = int(x), int(y), int(dx), int(dy), int(k)
        small = shrink(image[dy:dy + 2 * R, dx:dx + 2 * R])
        new[y:y + R, x:x + R] = s * turned(small, k) + o
    return new


def main():
    task = sys.argv[1] if len(sys.argv) > 1 else 'encode'
    if task == 'encode':
        photo = load(sys.argv[2] if len(sys.argv) > 2 else 'strandhaeuser.png')
        begin = time.time()
        code = encode(photo)
        np.save('code.npy', code)
        print(f'{len(code)} Blöcke in {time.time() - begin:.1f} s')
    elif task == 'decode':
        code = np.load('code.npy')
        h = int(code[:, 1].max()) + R
        w = int(code[:, 0].max()) + R
        image = np.full((h, w), 128.0)
        save(image, 'schritt-0.png')
        for step in range(1, 9):
            image = decode_step(image, code)
            save(image, f'schritt-{step}.png')


if __name__ == '__main__':
    main()
