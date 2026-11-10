import numpy as np
from PIL import Image

from video import read_frames, write_video


def held(frames, step):
    return np.tile(frames[::step], (step,) + (1,) * (frames.ndim - 1))[:len(frames)]


def interlace(frames):
    out = frames[:-1].copy()
    out[:, 1::2] = frames[1:, 1::2]
    return out


if __name__ == '__main__':
    frames = read_frames('schwenk.mp4')
    print(frames.shape, frames.nbytes, 'Bytes')
    write_video('schwenk-6.mp4', held(frames, 4))
    fast = read_frames('aufwaerts.mp4')
    combed = interlace(fast)
    Image.fromarray(combed[48, :200, 300:700]).resize((800, 400), Image.NEAREST).save('combing.png')
    write_video('aufwaerts-halbbilder.mp4', combed)
