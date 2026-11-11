import numpy as np

from predict import numbers, residual


def zigzag(r):
    return np.where(r >= 0, 2 * r, -2 * r - 1)


def rice(u, k):
    return '0' * (u // 2 ** k) + '1' + format(u % 2 ** k, 'b')


def rice_bits(u, k):
    return int(np.sum((u >> k) + 1 + k))


def best_bits(u):
    return min(rice_bits(u, k) for k in range(16))


if __name__ == '__main__':
    for path in ['stimme.wav', 'musik.wav']:
        x = numbers(path)
        u = zigzag(residual(x))
        print(f'{path}: {best_bits(u) / (16 * len(x)):.0%} mit einem k')
