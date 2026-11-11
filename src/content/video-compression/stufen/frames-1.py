import numpy as np
from PIL import Image

from video import read_frames, write_video


def held(frames, step):
    return np.repeat(frames[::step], step, axis=0)[:len(frames)]


if __name__ == '__main__':
    frames = read_frames('pan.mp4')
    print(frames.shape, frames.nbytes, 'Bytes')
    write_video('pan-6.mp4', held(frames, 4))
