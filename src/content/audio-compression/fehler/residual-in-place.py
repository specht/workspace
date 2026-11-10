import numpy as np

from wav import read_wav


def numbers(path):
    samples, rate = read_wav(path)
    return np.round(samples * 32767).astype(np.int64)


def residual(x):
    x[2:] = x[2:] - (2 * x[1:-1] - x[:-2])
    return x


def restore(r):
    x = r.copy()
    for n in range(2, len(x)):
        x[n] = r[n] + 2 * x[n - 1] - x[n - 2]
    return x


if __name__ == '__main__':
    for path in ['stimme.wav', 'musik.wav']:
        x = numbers(path)
        r = residual(x)
        print(f'{path}: Messungen im Mittel {np.abs(x).mean():.0f}, Reste {np.abs(r).mean():.0f}')
        print('  zurück wie vorher:', np.array_equal(restore(r), x))
