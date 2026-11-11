import numpy as np
from PIL import Image

from blocks import split, join, quantize, restore, bits, residual
from video import read_frames

CLIPS = ['glance', 'pan', 'slope', 'tilt']


def luma(frames):
    return frames @ np.array([0.299, 0.587, 0.114])


def differences(frames):
    return frames[1:].astype(int) - frames[:-1]


STEP = 32


def code(plane, step):
    return join(restore(quantize(split(plane, 4), step), step))


def encode_naive(planes, step):
    decoded = [code(planes[0] - 128, step) + 128]
    for k in range(1, len(planes)):
        decoded.append(decoded[-1] + code(planes[k] - planes[k - 1], step))
    return np.array(decoded)


def encode(planes, step):
    decoded = [code(planes[0] - 128, step) + 128]
    for plane in planes[1:]:
        decoded.append(decoded[-1] + code(plane - decoded[-1], step))
    return np.array(decoded)


def frame_bits(plane, step):
    return bits(quantize(split(plane, 4), step)) / plane.size


if __name__ == '__main__':
    for name in CLIPS:
        frames = read_frames(f'{name}.mp4')[:24]
        d = differences(frames)
        print(f'{name}: im Mittel {np.abs(d).mean():.2f} Unterschied je Wert')
        grey = (luma(d[11]) + 128).round().clip(0, 255).astype(np.uint8)
        Image.fromarray(grey).save(f'{name}-diff.png')

    for name in CLIPS:
        planes = luma(read_frames(f'{name}.mp4')[:24])
        alone = frame_bits(residual(planes[12], 4), STEP)
        change = frame_bits(planes[12] - planes[11], STEP)
        print(f'{name}: Bild allein {alone:.2f}, Unterschied {change:.2f} Bits je Pixel')

    planes = luma(read_frames('pan.mp4')[:24])
    for coder in (encode_naive, encode):
        decoded = coder(planes, STEP)
        errors = np.abs(decoded - planes).mean(axis=(1, 2))
        print(coder.__name__, ' '.join(f'{e:.1f}' for e in errors[::4]))
