import math

import numpy as np

from wav import read_wav, write_wav


def gain_db(level, threshold, ratio):
    if level <= threshold:
        return 0
    return (level - threshold) * (1 - 1 / ratio)


def compress(x, rate, threshold, ratio, attack, release, makeup):
    a = math.exp(-1 / (attack * rate))
    r = math.exp(-1 / (release * rate))
    level = 0
    y = np.zeros_like(x)
    for n, v in enumerate(np.abs(x).tolist()):
        c = a if v > level else r
        level = c * level + (1 - c) * v
        db = 20 * math.log10(max(level, 1e-9))
        y[n] = x[n] * 10 ** ((gain_db(db, threshold, ratio) + makeup) / 20)
    return y


def save(path, y, rate):
    peak = np.abs(y).max()
    write_wav(path, y / max(1, peak), rate)
    print(f'{path}: lauteste Stelle {peak:.2f}')


if __name__ == '__main__':
    x, rate = read_wav('stimme.wav')
    save('stimme-komprimiert.wav', compress(x, rate, -30, 4, 0.005, 0.1, 15), rate)
