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


def load_color(path):
    image = np.asarray(Image.open(path).convert('YCbCr'), dtype=float)
    return image[:, :, 0], image[:, :, 1], image[:, :, 2]


def save_color(y, cb, cr, path):
    cb = np.repeat(np.repeat(cb, 2, axis=0), 2, axis=1)
    cr = np.repeat(np.repeat(cr, 2, axis=0), 2, axis=1)
    ycbcr = np.clip(np.stack([y, cb, cr], axis=-1), 0, 255).round().astype(np.uint8)
    Image.fromarray(ycbcr, 'YCbCr').convert('RGB').save(path)


def picture_size(code):
    return int(code[:, 1].max()) + R, int(code[:, 0].max()) + R


def start_image(name, h, w):
    if name == 'grau':
        rgb = np.full((h, w, 3), 128.0)
    elif name == 'verlauf':
        rgb = np.zeros((h, w, 3))
        rgb[:, :, 0] = np.linspace(0, 255, w)
        rgb[:, :, 2] = np.linspace(255, 0, w)
    elif name == 'rauschen':
        rgb = np.random.default_rng(1).uniform(0, 255, (h, w, 3))
    else:
        rgb = np.asarray(Image.open(name).convert('RGB').resize((w, h)), dtype=float)
    ycbcr = np.asarray(Image.fromarray(rgb.astype(np.uint8)).convert('YCbCr'), dtype=float)
    return ycbcr[:, :, 0], shrink(ycbcr[:, :, 1]), shrink(ycbcr[:, :, 2])


def main():
    task = sys.argv[1] if len(sys.argv) > 1 else 'encode'
    if task == 'encode':
        y, cb, cr = load_color(sys.argv[2] if len(sys.argv) > 2 else 'strandhaeuser.png')
        begin = time.time()
        np.savez('code.npz', y=encode(y), cb=encode(shrink(cb)), cr=encode(shrink(cr)))
        print(f'fertig in {time.time() - begin:.1f} s')
    elif task == 'decode':
        codes = np.load('code.npz')
        start = sys.argv[2] if len(sys.argv) > 2 else 'grau'
        h, w = picture_size(codes['y'])
        y, cb, cr = start_image(start, h, w)
        images = {'y': y, 'cb': cb, 'cr': cr}
        save_color(images['y'], images['cb'], images['cr'], 'schritt-0.png')
        for step in range(1, 9):
            for name in images:
                images[name] = decode_step(images[name], codes[name])
            save_color(images['y'], images['cb'], images['cr'], f'schritt-{step}.png')


if __name__ == '__main__':
    main()
