import time

import numpy as np

from tones import tone


def dft(x):
    N = len(x)
    n = np.arange(N)
    X = np.zeros(N, dtype=complex)
    for k in range(N):
        X[k] = np.sum(x * np.exp(-2j * np.pi * k / N))
    return X


def strongest(X, rate, count):
    N = len(X)
    amplitude = np.abs(X[:N // 2]) * 2 / N
    best = np.argsort(amplitude)[::-1][:count]
    return [(round(k * rate / N), round(float(amplitude[k]), 2)) for k in best]


if __name__ == '__main__':
    rate = 4096
    x = tone(440, 1, rate) + 0.5 * tone(1000, 1, rate)
    start = time.perf_counter()
    X = dft(x)
    print(f'dft: {time.perf_counter() - start:.3f} s', strongest(X, rate, 2))
