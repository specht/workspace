import time

import numpy as np

from tones import tone


def dft(x):
    N = len(x)
    n = np.arange(N)
    X = np.zeros(N, dtype=complex)
    for k in range(N):
        X[k] = np.sum(x * np.exp(-2j * np.pi * k * n / N))
    return X


def fft(x):
    return dft(x)


def strongest(X, rate, count):
    N = len(X)
    amplitude = np.abs(X[:N // 2]) * 2 / N
    best = np.argsort(amplitude)[::-1][:count]
    return [(round(k * rate / N), round(float(amplitude[k]), 2)) for k in best]


if __name__ == '__main__':
    rate = 8192
    x = tone(440, 1, rate) + 0.5 * tone(1000, 1, rate)
    for name, transform in [('dft', dft), ('fft', fft), ('numpy', np.fft.fft)]:
        start = time.perf_counter()
        X = transform(x)
        print(f'{name:5}: {time.perf_counter() - start:.4f} s', strongest(X, rate, 2))
