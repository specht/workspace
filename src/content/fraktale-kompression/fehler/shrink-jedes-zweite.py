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
    return block[::2, ::2]


def main():
    photo = load('strandhaeuser.png')
    save(photo, 'grau.png')
    save(shrink(photo), 'klein.png')


if __name__ == '__main__':
    main()
