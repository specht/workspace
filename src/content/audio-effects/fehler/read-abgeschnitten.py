import numpy as np

from wav import read_wav, write_wav


def read(x, pos):
    return x[np.floor(pos).astype(int)]


def chorus(x, rate, depth, speed, mix):
    n = np.arange(len(x))
    delay = (0.015 + depth * (1 + np.sin(2 * np.pi * speed * n / rate)) / 2) * rate
    pos = n - delay
    wet = np.where(pos >= 0, read(x, np.maximum(pos, 0)), 0)
    return x + mix * wet


def save(path, y, rate):
    peak = np.abs(y).max()
    write_wav(path, y / max(1, peak), rate)
    print(f'{path}: lauteste Stelle {peak:.2f}')


if __name__ == '__main__':
    x, rate = read_wav('stimme.wav')
    save('stimme-chorus.wav', chorus(x, rate, 0.004, 1, 0.7), rate)
