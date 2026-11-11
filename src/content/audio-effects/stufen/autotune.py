import numpy as np

from pitch import HOP, SIZE, pitches, save, shift
from wav import read_wav


def nearest_note(f):
    return 440 * 2 ** (round(12 * np.log2(f / 440)) / 12)


def autotune(x, rate):
    found = pitches(x, rate)

    def factor(c):
        k = min(max(0, (c - SIZE // 2) // HOP), len(found) - 1)
        f = found[k]
        return nearest_note(f) / f if 60 < f < 1000 else 1

    return shift(x, rate, factor)


if __name__ == '__main__':
    x, rate = read_wav('stimme.wav')
    save('stimme-autotune.wav', autotune(x, rate), rate)
