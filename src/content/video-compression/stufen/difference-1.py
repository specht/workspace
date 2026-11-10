import numpy as np
from PIL import Image

from video import read_frames

CLIPS = ['glance', 'pan', 'slope', 'tilt']


def luma(frames):
    return frames @ np.array([0.299, 0.587, 0.114])


def differences(frames):
    return frames[1:].astype(int) - frames[:-1]


if __name__ == '__main__':
    for name in CLIPS:
        frames = read_frames(f'{name}.mp4')[:24]
        d = differences(frames)
        print(f'{name}: im Mittel {np.abs(d).mean():.2f} Unterschied je Wert')
        grey = (luma(d[11]) + 128).round().clip(0, 255).astype(np.uint8)
        Image.fromarray(grey).save(f'{name}-diff.png')
