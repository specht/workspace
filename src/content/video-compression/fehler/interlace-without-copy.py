import numpy as np
from PIL import Image

from video import read_frames, write_video


def held(frames, step):
    return np.repeat(frames[::step], step, axis=0)[:len(frames)]


def interlace(frames):
    out = frames[:-1]
    out[:, 1::2] = frames[1:, 1::2]
    return out


if __name__ == '__main__':
    frames = read_frames('pan.mp4')
    print(frames.shape, frames.nbytes, 'Bytes')
    write_video('pan-6.mp4', held(frames, 4))
    fast = read_frames('tilt.mp4')
    combed = interlace(fast)
    Image.fromarray(combed[48, :200, 300:700]).resize((800, 400), Image.NEAREST).save('combing.png')
    write_video('tilt-interlaced.mp4', combed)
