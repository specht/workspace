import sys

import numpy as np
from PIL import Image

N = 256


def shrink(image):
    h, w = image.shape[:2]
    return image.reshape(h // 2, 2, w // 2, 2, 3).mean(axis=(1, 3))


def copy_machine(image):
    small = shrink(image)
    h = N // 2
    new = np.full((N, N, 3), 255.0)
    new[0:h, h // 2:h // 2 + h] = small
    new[h:N, 0:h] = small
    new[h:N, h:N] = small
    return new


def main():
    start = sys.argv[1] if len(sys.argv) > 1 else 'strandhaeuser.png'
    if start == 'rauschen':
        image = np.random.default_rng(1).uniform(0, 255, (N, N, 3))
    else:
        image = np.asarray(Image.open(start).convert('RGB').resize((N, N)), dtype=float)
    for step in range(9):
        Image.fromarray(image.astype(np.uint8)).save(f'kopie-{step}.png')
        image = copy_machine(image)


if __name__ == '__main__':
    main()
