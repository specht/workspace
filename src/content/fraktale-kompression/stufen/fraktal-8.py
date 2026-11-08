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


def domains(image, r=R):
    blocks, places = [], []
    h, w = image.shape
    for y in range(0, h - 2 * r + 1, r):
        for x in range(0, w - 2 * r + 1, r):
            small = shrink(image[y:y + 2 * r, x:x + 2 * r])
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


def decode_step(image, code, scale=1):
    new = np.empty_like(image)
    for row in code:
        x, y, dx, dy, k, s, o = row[:7]
        r = (int(row[7]) if len(row) > 7 else R) * scale
        x, y, dx, dy, k = int(x) * scale, int(y) * scale, int(dx) * scale, int(dy) * scale, int(k)
        small = shrink(image[dy:dy + 2 * r, dx:dx + 2 * r])
        new[y:y + r, x:x + r] = s * turned(small, k) + o
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
    sizes = code[:, 7] if code.shape[1] > 7 else R
    return int((code[:, 1] + sizes).max()), int((code[:, 0] + sizes).max())


SIZES = (16, 8, 4)


def encode_quadtree(image, limit):
    pools = {}
    for size in SIZES:
        blocks, places = domains(image, size)
        means = blocks.mean(axis=1)
        centred = blocks - means[:, None]
        pools[size] = (centred, means, (centred * centred).sum(axis=1), places)
    code = []

    def encode_block(x, y, size):
        centred, means, spread, places = pools[size]
        target = image[y:y + size, x:x + size].ravel()
        t = target - target.mean()
        dots = centred @ t
        s = np.clip(dots / np.maximum(spread, 1e-9), -1, 1)
        error = t @ t - 2 * s * dots + s * s * spread
        best = int(error.argmin())
        if error[best] / (size * size) > limit and size > SIZES[-1]:
            half = size // 2
            for dy in (0, half):
                for dx in (0, half):
                    encode_block(x + dx, y + dy, half)
            return
        px, py, k = places[best]
        code.append((x, y, px, py, k, s[best], target.mean() - s[best] * means[best], size))

    h, w = image.shape
    for y in range(0, h, SIZES[0]):
        for x in range(0, w, SIZES[0]):
            encode_block(x, y, SIZES[0])
    return np.array(code)


def show_blocks(image, code):
    picture = np.stack([np.clip(image, 0, 255)] * 3, axis=-1).astype(np.uint8)
    for x, y, size in code[:, [0, 1, 7]].astype(int):
        picture[y, x:x + size] = (220, 38, 38)
        picture[y:y + size, x] = (220, 38, 38)
    Image.fromarray(picture).resize((picture.shape[1] * 2, picture.shape[0] * 2), Image.NEAREST).save('bloecke.png')


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


def psnr(a, b):
    return 10 * np.log10(255 ** 2 / ((a - b) ** 2).mean())


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
        scale = int(sys.argv[3]) if len(sys.argv) > 3 else 1
        h, w = picture_size(codes['y'])
        y, cb, cr = start_image(start, h * scale, w * scale)
        images = {'y': y, 'cb': cb, 'cr': cr}
        save_color(images['y'], images['cb'], images['cr'], 'schritt-0.png')
        for step in range(1, 9):
            for name in images:
                images[name] = decode_step(images[name], codes[name], scale)
            save_color(images['y'], images['cb'], images['cr'], f'schritt-{step}.png')
    elif task == 'quadtree':
        y, cb, cr = load_color(sys.argv[2] if len(sys.argv) > 2 else 'strandhaeuser.png')
        limit = float(sys.argv[3]) if len(sys.argv) > 3 else 60
        code = encode_quadtree(y, limit)
        np.savez('code.npz', y=code, cb=encode(shrink(cb)), cr=encode(shrink(cr)))
        show_blocks(y, code)
        counts = ', '.join(f'{int((code[:, 7] == size).sum())} mit {size}' for size in SIZES)
        print(f'{len(code)} Blöcke: {counts}')
    elif task == 'vergleich':
        photo = np.asarray(Image.open(sys.argv[2] if len(sys.argv) > 2 else 'strandhaeuser.png').convert('RGB'), dtype=float)
        image = np.asarray(Image.open(sys.argv[3] if len(sys.argv) > 3 else 'schritt-8.png').convert('RGB'), dtype=float)
        print(f'{psnr(image, photo):.1f} dB')


if __name__ == '__main__':
    main()
