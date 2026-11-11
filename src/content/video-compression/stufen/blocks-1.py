import numpy as np
from PIL import Image

from chroma import to_ycbcr


def dct_matrix(n):
    k = np.arange(n)[:, None]
    i = np.arange(n)[None, :]
    d = np.cos((2 * i + 1) * k * np.pi / (2 * n)) * np.sqrt(2 / n)
    d[0] /= np.sqrt(2)
    return d


def split(plane, n):
    height, width = plane.shape
    return plane.reshape(height // n, n, width // n, n).swapaxes(1, 2)


def join(blocks):
    rows, columns, n, _ = blocks.shape
    return blocks.swapaxes(1, 2).reshape(rows * n, columns * n)


def dct(blocks):
    d = dct_matrix(blocks.shape[-1])
    return d @ blocks @ d.T


def idct(coefficients):
    d = dct_matrix(coefficients.shape[-1])
    return d.T @ coefficients @ d


def bits(levels):
    n = levels.shape[-1]
    flat = levels.reshape(-1, n * n)
    total = 0
    for k in range(n * n):
        _, counts = np.unique(flat[:, k], return_counts=True)
        p = counts / len(flat)
        total += -(p * np.log2(p)).sum() * len(flat)
    return total


def quantize(blocks, step):
    return np.round(dct(blocks) / step)


def restore(levels, step):
    return idct(levels * step)


def save_detail(path, plane):
    grey = plane.round().clip(0, 255).astype(np.uint8)
    Image.fromarray(grey[130:310, 390:630]).resize((720, 540), Image.NEAREST).save(path)


if __name__ == '__main__':
    luma = to_ycbcr(np.asarray(Image.open('portrait.png').convert('RGB')))[..., 0]
    blocks = split(luma - 128, 8)
    for step in (8, 32, 64):
        levels = quantize(blocks, step)
        share = np.count_nonzero(levels) / levels.size
        print(f'Schritt {step}: {share:.1%} nicht 0, {bits(levels) / luma.size:.2f} Bits je Pixel')
        save_detail(f'step-{step}.png', join(restore(levels, step)) + 128)
