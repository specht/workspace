import numpy as np
from PIL import Image


def to_ycbcr(rgb):
    r, g, b = rgb[..., 0], rgb[..., 1], rgb[..., 2]
    y = 0.299 * r + 0.587 * g + 0.114 * b
    cb = - 0.168736 * r - 0.331264 * g + 0.5 * b
    cr = 0.5 * r - 0.418688 * g - 0.081312 * b
    return np.stack([y, cb, cr], axis=-1)


def from_ycbcr(ycbcr):
    y, cb, cr = ycbcr[..., 0], ycbcr[..., 1] - 128, ycbcr[..., 2] - 128
    r = y + 1.402 * cr
    g = y - 0.344136 * cb - 0.714136 * cr
    b = y + 1.772 * cb
    return np.stack([r, g, b], axis=-1).round().clip(0, 255).astype(np.uint8)


def subsample(plane, n=2):
    height, width = plane.shape
    return plane.reshape(height // n, n, width // n, n).mean(axis=(1, 3))


def upsample(plane, n=2):
    return np.repeat(np.repeat(plane, n, axis=0), n, axis=1)


def save_grey(path, plane):
    Image.fromarray(plane.round().clip(0, 255).astype(np.uint8)).save(path)


def save_detail(path, rgb):
    Image.fromarray(rgb[130:310, 390:630]).resize((720, 540), Image.NEAREST).save(path)


if __name__ == '__main__':
    frame = np.asarray(Image.open('portrait.png').convert('RGB'))
    ycbcr = to_ycbcr(frame)
    save_grey('y.png', ycbcr[..., 0])
    save_grey('cb.png', ycbcr[..., 1])
    save_grey('cr.png', ycbcr[..., 2])

    small_chroma = ycbcr.copy()
    small_chroma[..., 1] = upsample(subsample(ycbcr[..., 1]))
    small_chroma[..., 2] = upsample(subsample(ycbcr[..., 2]))
    small_luma = ycbcr.copy()
    small_luma[..., 0] = upsample(subsample(ycbcr[..., 0]))
    save_detail('detail.png', frame)
    save_detail('detail-chroma.png', from_ycbcr(small_chroma))
    save_detail('detail-luma.png', from_ycbcr(small_luma))
