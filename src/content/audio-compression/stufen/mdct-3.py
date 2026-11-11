import functools

import numpy as np

from wav import read_wav, write_wav


@functools.cache
def tones(N):
    n = np.arange(2 * N)
    k = np.arange(N)
    return np.cos(np.pi / N * np.outer(k + 0.5, n + 0.5 + N / 2))


def mdct(x):
    return tones(len(x) // 2) @ x


def imdct(X):
    return tones(len(X)).T @ X * 2 / len(X)


def window(N):
    return np.sin(np.pi * (np.arange(2 * N) + 0.5) / (2 * N))


def encode(x, N):
    w = window(N)
    x = np.concatenate([np.zeros(N), x, np.zeros(2 * N)])
    return [mdct(w * x[s:s + 2 * N]) for s in range(0, len(x) - 2 * N + 1, N)]


def decode(blocks, N, length):
    w = window(N)
    y = np.zeros((len(blocks) + 1) * N)
    for i, X in enumerate(blocks):
        y[i * N:i * N + 2 * N] += w * imdct(X)
    return y[N:N + length]


if __name__ == '__main__':
    print(np.round(imdct(mdct(np.arange(16.0))), 2))
    x, rate = read_wav('musik.wav')
    x = x[:2 * rate]
    y = decode(encode(x, 1024), 1024, len(x))
    print('größter Fehler:', np.abs(x - y).max())
